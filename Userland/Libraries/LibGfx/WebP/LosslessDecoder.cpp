/*
 * Copyright (c) 2022, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/BinarySearch.h>
#include <AK/Debug.h>
#include <AK/QuickSort.h>
#include <AK/Vector.h>
#include <LibGfx/Bitmap.h>
#include <LibGfx/WebP/Common.h>
#include <LibGfx/WebP/LosslessData.h>
#include <LibGfx/WebP/LosslessDecoder.h>

namespace WebP::Lossless {

class BitStream {
public:
    BitStream(ReadonlyBytes data)
        : m_data(data)
    {
    }

    [[nodiscard]] bool read_bit()
    {
        return static_cast<bool>(read_bits<1>());
    }

    template<size_t bits>
    [[nodiscard]] u32 read_bits()
    {
        static_assert(bits <= 32);
        while (m_bit_count < bits) {
            if (m_data.is_empty()) {
                dbgln_if(WEBP_DEBUG, "Ran out of bits");
                return 0;
            }
            m_value |= m_data[0] << m_bit_count;
            m_bit_count += 8;
            m_data = m_data.slice(1);
        }
        return get_low_bits(bits);
    }

    [[nodiscard]] u32 read_bits(size_t bits)
    {
        VERIFY(bits <= 32);
        while (m_bit_count < bits) {
            m_value |= m_data[0] << m_bit_count;
            m_bit_count += 8;
            m_data = m_data.slice(1);
        }
        return get_low_bits(bits);
    }

private:
    u32 get_low_bits(u8 bits)
    {
        u32 result = m_value & ((1 << bits) - 1);
        m_value >>= bits;
        m_bit_count -= bits;
        //dbgln_if(WEBP_DEBUG, "Read {} bits 0x{:x}", bits, result);
        return result;
    }
    ReadonlyBytes m_data;
    u64 m_value = 0;
    u8 m_bit_count = 0;
};

enum class LosslessTransformType : u8 {
    Predictor = 0,
    Color = 1,
    SubtractGreen = 2,
    ColorIndexing = 3,
};

struct Transform {
    LosslessTransformType type;
    u32 argument { 0 };
    RefPtr<Gfx::Bitmap> image {};
};

struct LosslessContext {
    BitStream stream = { {} };
    Gfx::IntSize image_size;
};

LosslessDecoder::LosslessDecoder(ReadonlyBytes data)
{
    m_context = make<LosslessContext>();
    m_context->stream = BitStream(data);
}

LosslessDecoder::~LosslessDecoder() = default;

ErrorOr<Gfx::IntSize> LosslessDecoder::try_decode_frame_header()
{
    auto& stream = m_context->stream;
    if (stream.read_bits<8>() != 0x2f)
        return Error::from_string_view("Invalid lossless signature"sv);
    m_context->image_size = { stream.read_bits<14>() + 1, stream.read_bits<14>() + 1 };
    return m_context->image_size;
}

class ColorCache {
public:
    ColorCache(size_t color_cache_code_bits)
        : m_code_bits(color_cache_code_bits)
    {
        VERIFY(m_code_bits > 0);
        m_cache.resize(1 << m_code_bits);
        dbgln_if(WEBP_DEBUG, "Color cache size: {}", m_cache.size());
    }

    ALWAYS_INLINE Color& operator[](u32 color) {
        size_t index = (COLOR_CACHE_MULTIPLIER * color) >> (32 - m_code_bits);
        return m_cache.at(index);
    }

    ALWAYS_INLINE void add(Color color) {
        this[color.value()] = color.value();
    }

    size_t size() const { return m_cache.size(); }
private:
    u32 m_code_bits;
    Vector<Color> m_cache;
};

static ErrorOr<Optional<ColorCache>> read_color_cache_info(BitStream& stream)
{
    if (stream.read_bit()) {
        size_t color_cache_code_bits = stream.read_bits<4>();
        if (color_cache_code_bits > MAX_COLOR_CACHE_BITS || color_cache_code_bits == 0)
            return Error::from_string_view("Invalid color cache size"sv);

        return ColorCache { color_cache_code_bits };
    };
    return Optional<ColorCache> {};
}

struct Symbol {
    size_t length;
    size_t value;
};

class CanonicalCode {
public:
    CanonicalCode() = default;

    ErrorOr<size_t> read_symbol(BitStream& stream)
    {
        size_t code_bits = 1;

        while (code_bits < (1 << 16)) {
            // FIXME: This is very inefficient and could greatly be improved by implementing this
            //        algorithm: https://www.hanshq.net/zip.html#huffdec
            size_t index;
            if (binary_search(m_symbol_codes.span(), code_bits, &index) != nullptr) {
                //dbgln("read_symbol: {}", m_symbol_values[index]);
                return m_symbol_values[index];
            }

            code_bits = (code_bits << 1) | stream.read_bits<1>();
        }

        return Error::from_string_literal("no matching code found");
    }

    ErrorOr<void> init(Vector<size_t> const& lengths, Vector<size_t> values = {})
    {
        VERIFY(lengths.size() == values.size());
        m_symbol_codes.clear();
        m_symbol_values.clear();

        size_t max_length = 0;
        size_t num_non_zero = 0;
        for (size_t length : lengths) {
            max_length = max(max_length, length);
            if (length > 0)
                num_non_zero++;
        }

        m_symbol_codes.ensure_capacity(num_non_zero);
        m_symbol_values.ensure_capacity(num_non_zero);

        auto next_code = 0;
        for (size_t length = 1; length <= max_length; length++) {
            next_code <<= 1;
            auto start_bit = 1 << length;
            for (size_t symbol = 0; symbol < lengths.size(); symbol++) {
                if (lengths[symbol] != length)
                    continue;

                if (next_code >= start_bit)
                    return Error::from_string_view("Invalid prefix code, too many codes used"sv);

                StringBuilder sb;
                sb.appendff("{:b}            "sv, start_bit | next_code);
                dbgln("{}: {}", sb.string_view().substring_view(1, max_length + 1), values[symbol]);
                m_symbol_codes.append(start_bit | next_code);
                m_symbol_values.append(values[symbol]);
                next_code++;
            }
        }

        if (next_code != (1 << max_length))
            return Error::from_string_view("Invalid prefix code, not all codes are used"sv);

        return {};
    }

    ErrorOr<void> init(Vector<Symbol>& symbols)
    {
        quick_sort(symbols, [](auto a, auto b) { return a.value < b.value; });

        auto lengths = Vector<size_t> {};
        TRY(lengths.try_ensure_capacity(symbols.size()));

        auto values = Vector<size_t> {};
        TRY(values.try_ensure_capacity(symbols.size()));

        for (auto& symbol : symbols) {
            lengths.append(symbol.length);
            values.append(symbol.value);
        }

        return init(lengths, values);
    }

private:
    Vector<size_t> m_symbol_codes;
    Vector<size_t> m_symbol_values;
};

static ErrorOr<void> read_simple_huffman_code(BitStream& stream, CanonicalCode& code)
{
    auto symbols = Vector<Symbol> {};
    auto num_symbols = stream.read_bits<1>() + 1;
    auto is_first_8_bits = stream.read_bit();
    auto symbol_0 = is_first_8_bits ? stream.read_bits<8>() : stream.read_bits<1>();
    symbols.append({ 1, symbol_0 });
    if (num_symbols > 1) {
        auto symbol_1 = num_symbols > 1 ? stream.read_bits<8>() : 0u;
        symbols.append({ 1, symbol_1 });
    } else {
        symbols.append({ 1, 0 });
    }
    return code.init(symbols);
}

static ErrorOr<void> read_full_huffman_code(BitStream& stream, CanonicalCode& code, size_t alphabet_size)
{
    auto symbols = Vector<Symbol> {};
    auto num_code_lengths = 4 + stream.read_bits<4>();
    dbgln("num_code_lengths={}", num_code_lengths);
    if (num_code_lengths >= CODE_LENGTH_CODES)
        return Error::from_string_view("Too many code lengths"sv);

    auto code_length_codes = Vector<Symbol> {};
    TRY(code_length_codes.try_ensure_capacity(num_code_lengths));
    for (size_t i = 0; i < num_code_lengths; i++)
        code_length_codes.append({ stream.read_bits<3>(), CODE_LENGTH_CODE_ORDER[i] });

    auto code_length_code = CanonicalCode {};
    TRY(code_length_code.init(code_length_codes));

    auto max_symbol = stream.read_bit() ? (2 + stream.read_bits(2 + 2 * stream.read_bits<3>())) : alphabet_size;
    size_t last_non_zero = 8;
    for (size_t i = 0; i < max_symbol; i++) {
        auto code_length_symbol = TRY(code_length_code.read_symbol(stream));
        dbgln("code_length_symbol={}", code_length_symbol);
        if (code_length_symbol == 0)
            break;

        if (code_length_symbol <= 15) {
            symbols.append({ code_length_symbol, i });
            last_non_zero = code_length_symbol;
        } else if (code_length_symbol == 16) {
            auto repetitions = 3 + stream.read_bits<2>();
            dbgln("{}*{}", repetitions, last_non_zero);
            auto end = i + repetitions;
            while (i < end) {
                symbols.append({ last_non_zero, i });
                i++;
            }
        } else if (code_length_symbol == 17) {
            auto zeros = 3 + stream.read_bits<3>();
            dbgln("{}*0", zeros);
            i += zeros;
        } else if (code_length_symbol == 18) {
            auto zeros = 11 + stream.read_bits<7>();
            dbgln("{}*0", zeros);
            i += zeros;
        }
    }
    if (symbols.size() > max_symbol)
        return Error::from_string_view("More symbols than expected"sv);
    return code.init(symbols);
}

static ErrorOr<void> read_huffman_code(BitStream& stream, CanonicalCode& code, size_t alphabet_size)
{
    auto simple_code = stream.read_bit();
    dbgln("simple_code={}", simple_code);
    if (simple_code)
        return read_simple_huffman_code(stream, code);
    return read_full_huffman_code(stream, code, alphabet_size);
}

struct PrefixCodeGroup {
    CanonicalCode green {};
    CanonicalCode red {};
    CanonicalCode blue {};
    CanonicalCode alpha {};
    CanonicalCode distance {};
};

static ErrorOr<PrefixCodeGroup> read_prefix_code_group(BitStream& stream, size_t color_cache_size)
{
    PrefixCodeGroup group {};
    dbgln("green:");
    TRY(read_huffman_code(stream, group.green, 256 + 24 + color_cache_size));
    dbgln("red:");
    TRY(read_huffman_code(stream, group.red, 256));
    dbgln("blue:");
    TRY(read_huffman_code(stream, group.blue, 256));
    dbgln("alpha:");
    TRY(read_huffman_code(stream, group.alpha, 256));
    dbgln("distance:");
    TRY(read_huffman_code(stream, group.distance, 40));
    return group;
}

static ALWAYS_INLINE size_t decode_size_symbol(BitStream& stream, size_t size_symbol)
{
    if (size_symbol < 4)
        return size_symbol + 1;
    u8 extra_bits = (size_symbol - 2) >> 1;
    u8 offset = (2 + (size_symbol & 1)) << extra_bits;
    return (offset | stream.read_bits(extra_bits)) + 1;
}

static ALWAYS_INLINE Gfx::IntPoint distance_to_offset(size_t distance)
{
    VERIFY(distance > 0);
    if (distance > DISTANCE_MAP.size())
        return { static_cast<int>(distance - DISTANCE_MAP.size()), 0 };
    auto offset_vector = DISTANCE_MAP[distance - 1];
    auto x = 8 - (offset_vector & 0xf);
    auto y = offset_vector >> 4;
    return { x, y };
}

static ALWAYS_INLINE int prefix_group_from_pixel(Gfx::Color pixel)
{
    return (static_cast<int>(pixel.value()) >> 8) & 0xffff;
}

static ErrorOr<NonnullRefPtr<Gfx::Bitmap>> read_entropy_coded_image(BitStream& stream, Gfx::IntSize size, bool allow_meta_huffman)
{
    dbgln_if(WEBP_DEBUG, "Reading entropy image: {}x{}", size.width(), size.height());

    auto color_cache = TRY(read_color_cache_info(stream));

    auto num_prefix_groups = 1;
    size_t prefix_bits = 0;
    RefPtr<Gfx::Bitmap> prefix_data {};

    if (allow_meta_huffman && stream.read_bit()) {
        prefix_bits = stream.read_bits<3>() + 2;
        Gfx::IntSize prefix_size {
            ceil_div(size.width(), 1 << prefix_bits),
            ceil_div(size.height(), 1 << prefix_bits)
        };
        dbgln("Reading prefix map:");
        prefix_data = TRY(read_entropy_coded_image(stream, size, false));
        if (false) {
            for (int y = 0; y < prefix_data->height(); y++) {
                auto* scanline = prefix_data->scanline(y);
                for (int x = 0; x < prefix_data->height(); x++) {
                    scanline[x] = (scanline[x] << 2) | 0xff000000u;
                }
            }
            return prefix_data.release_nonnull();
        }
        auto max_prefix_group = 0;
        for (int y = 0; y < prefix_size.height(); y++)
            for (int x = 0; x < prefix_size.width(); x++)
                max_prefix_group = max(max_prefix_group, prefix_group_from_pixel(prefix_data->get_pixel(x, y)));
        num_prefix_groups = max_prefix_group + 1;
        dbgln("Prefix groups: {}", num_prefix_groups);
    }

    auto prefix_groups = Vector<PrefixCodeGroup> {};
    TRY(prefix_groups.try_ensure_capacity(num_prefix_groups));

    for (int i = 0; i < num_prefix_groups; i++) {
        dbgln("Prefix group {}", i);
        prefix_groups.append(TRY(read_prefix_code_group(stream, color_cache.has_value() ? color_cache->size() : 0)));
    }

    auto bitmap = TRY(Gfx::Bitmap::try_create(Gfx::BitmapFormat::BGRA8888, size));

    size_t backref_length = 0;
    Gfx::IntPoint backref_offset = {};
    for (int y = 0; y < size.height(); y++) {
        for (int x = 0; x < size.width(); x++) {
            if (!backref_length) {
                auto meta_prefix_code = prefix_data ? prefix_group_from_pixel(prefix_data->get_pixel(x >> prefix_bits, y >> prefix_bits)) : 0;
                auto& prefix_group = prefix_groups[meta_prefix_code];
                auto green_symbol = TRY(prefix_group.green.read_symbol(stream));
                if (green_symbol < 256) {
                    auto red = static_cast<u8>(TRY(prefix_group.red.read_symbol(stream)));
                    auto green = static_cast<u8>(green_symbol);
                    auto blue = static_cast<u8>(TRY(prefix_group.blue.read_symbol(stream)));
                    auto alpha = static_cast<u8>(TRY(prefix_group.alpha.read_symbol(stream)));
                    dbgln("pos=({},{}) color=#{:02x}{:02x}{:02x}{:02x}", x, y, alpha, red, green, blue);
                    bitmap->set_pixel(x, y, { red, green, blue, alpha });
                } else if (green_symbol < 256 + 24) {
                    auto length = decode_size_symbol(stream, green_symbol - 256);
                    auto distance = decode_size_symbol(stream, TRY(prefix_group.distance.read_symbol(stream)));
                    backref_offset = distance_to_offset(distance);
                    backref_length = length;
                    dbgln("pos=({},{}) code={} backref=({},{}) length={}", x, y, distance, backref_offset.x(), backref_offset.y(), length);
                } else {
                    VERIFY(color_cache.has_value());
                    auto index = green_symbol - 256 - 24;
                    dbgln("pos=({},{}) color_cache[{}]={}", x, y, index, (*color_cache)[index].to_string());
                    bitmap->set_pixel(x, y, (*color_cache)[index]);
                }
            }
            if (backref_length) {
                backref_length--;
                auto ref_pos = Gfx::IntPoint { x, y } - backref_offset;
                if (ref_pos.x() < 0)
                    ref_pos += { size.width(), -1 };
                if (ref_pos.y() < 0)
                    return Error::from_string_view("Invalid backref"sv);
                dbgln("pos: ({},{}) ref_pos: ({},{})", x, y, ref_pos.x(), ref_pos.y());
                bitmap->set_pixel(x, y, bitmap->get_pixel(ref_pos.x(), ref_pos.y()));
            }
            if (color_cache.has_value())
                color_cache->add(bitmap->get_pixel(x, y));
        }
    }
    return bitmap;
}

static ErrorOr<Vector<Transform, 4>> try_read_transforms(BitStream& stream, Gfx::IntSize size)
{
    u32 transforms_seen = 0;
    Vector<Transform, 4> transforms;

    while (stream.read_bit()) {
        transforms.empend();
        auto transform = transforms.last();
        transform.type = static_cast<LosslessTransformType>(stream.read_bits<2>());

        if ((transforms_seen & (1 << to_underlying(transform.type))) != 0)
            return Error::from_string_view("Duplicate transform"sv);
        transforms_seen |= 1 << to_underlying(transform.type);

        switch (transform.type) {
        case LosslessTransformType::Predictor:
        case LosslessTransformType::Color: {
            transform.argument = stream.read_bits<3>() + 2;
            u32 block_size = 1 << transform.argument;
            Gfx::IntSize sub_size = { ceil_div(size.width(), block_size), ceil_div(size.height(), block_size) };
            transform.image = TRY(read_entropy_coded_image(stream, sub_size, false));
            break;
        }
        case LosslessTransformType::SubtractGreen:
            break;
        case LosslessTransformType::ColorIndexing:
            transform.argument = stream.read_bits<8>() + 1;
            transform.image = TRY(read_entropy_coded_image(stream, { static_cast<int>(transform.argument), 1 }, false));
            break;
        }
        dbgln_if(WEBP_DEBUG, "Transform: type={}, argument={}, image={:p}", to_underlying(transform.type), transform.argument, transform.image.ptr());
    }

    return transforms;
}

static ErrorOr<NonnullRefPtr<Gfx::Bitmap>> read_lossless_argb(BitStream& stream, Gfx::IntSize size)
{
    (void)stream.read_bit();

    auto version_number = stream.read_bits<3>();
    if (version_number > 0)
        return Error::from_string_view("Invalid lossless version number"sv);

    auto transforms = TRY(try_read_transforms(stream, size));

    auto bitmap = TRY(read_entropy_coded_image(stream, size, true));

    // TODO: apply transforms

    return bitmap;
}

ErrorOr<NonnullRefPtr<Gfx::Bitmap>> LosslessDecoder::try_decode_image()
{
    return read_lossless_argb(m_context->stream, m_context->image_size);
}

}
