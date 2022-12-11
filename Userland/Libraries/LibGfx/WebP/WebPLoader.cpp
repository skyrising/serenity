/*
 * Copyright (c) 2022, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Debug.h>
#include <AK/String.h>
#include <LibGfx/WebP/Common.h>
#include <LibGfx/WebP/LosslessDecoder.h>
#include <LibGfx/WebP/VP8Decoder.h>
#include <LibGfx/WebP/WebPLoader.h>

namespace WebP {

template<size_t N>
static constexpr u32 fourcc(char const (&code)[N])
{
    static_assert(N == 5);
    return code[0] | code[1] << 8 | code[2] << 16 | code[3] << 24;
}

enum class Format {
    Lossy,
    Lossless,
};

enum class AlphaFormat {
    None = 0,
    Uncompressed,
    Compressed,
};

struct WebPLoadingContext {
    enum State {
        NotDecoded = 0,
        Error,
        HeaderDecoded,
        FrameHeaderDecoded,
        BitmapDecoded,
    };
    State state { NotDecoded };
    Optional<AK::Error> error;
    ReadonlyBytes file_data;
    ReadonlyBytes main_data;
    Format format;
    struct {
        AlphaFormat format;
        ReadonlyBytes data;
    } alpha;
    Gfx::IntSize image_size { 0, 0 };

    OwnPtr<VP8::VP8Decoder> vp8_decoder;
    OwnPtr<Lossless::LosslessDecoder> lossless_decoder;

    RefPtr<Gfx::Bitmap> bitmap;
};

struct RiffChunk {
    u32 fourcc;
    ReadonlyBytes data;
};

WebPImageDecoderPlugin::WebPImageDecoderPlugin(u8 const* data, size_t size)
{
    m_context = make<WebPLoadingContext>();
    m_context->file_data = ReadonlyBytes(data, size);
}

WebPImageDecoderPlugin::~WebPImageDecoderPlugin() = default;

static Optional<RiffChunk> read_chunk(ReadonlyBytes const& buffer)
{
    if (buffer.is_null() || buffer.size() < 8)
        return {};
    u32 fourcc = read_u32(buffer, 0).release_value();
    size_t size = static_cast<size_t>(read_u32(buffer, 4).release_value());
    if (buffer.size() < 8 + size) {
        return {};
    }
    return RiffChunk { fourcc, buffer.slice(8, size) };
}

static ErrorOr<void> try_decode_webp_header(WebPLoadingContext& context)
{
    auto main_chunk = read_chunk(context.file_data);
    if (!main_chunk.has_value() || main_chunk->fourcc != fourcc("RIFF"))
        return Error::from_string_view("Not a RIFF container"sv);

    context.main_data = main_chunk->data;
    if (context.main_data.size() < 8 || read_u32(context.main_data, 0).release_value() != fourcc("WEBP"))
        return Error::from_string_view("Not a WebP RIFF container"sv);

    size_t offset = 4;
    bool has_image_data = false;

    while (offset < context.main_data.size()) {
        auto chunk = read_chunk(context.main_data.slice(offset));
        if (!chunk.has_value()) {
            dbgln_if(WEBP_DEBUG, "Can't read next chunk @{}/{}", offset, context.main_data.size());
            return Error::from_string_view("Invalid chunk"sv);
        }
        if constexpr (WEBP_DEBUG) {
            auto fourcc_string = StringView { reinterpret_cast<char const*>(&chunk->fourcc), 4 };
            dbgln("Got {} chunk ({} bytes)", fourcc_string, chunk->data.size());
        }
        switch (chunk->fourcc) {
        case fourcc("VP8 "):
            context.format = Format::Lossy;
            context.vp8_decoder = make<VP8::VP8Decoder>(chunk->data);
            has_image_data = true;
            break;
        case fourcc("VP8L"):
            context.format = Format::Lossless;
            context.lossless_decoder = make<Lossless::LosslessDecoder>(chunk->data);
            has_image_data = true;
            break;
        case fourcc("VP8X"):
            if (chunk->data.size() < 10)
                return Error::from_string_view("Invalid VP8X chunk"sv);
            break;
        case fourcc("ALPH"):
            if (chunk->data.size() < 1)
                continue;
            switch (chunk->data[0] & 3) {
            case 0:
                context.alpha.format = AlphaFormat::Uncompressed;
                break;
            case 1:
                context.alpha.format = AlphaFormat::Compressed;
                break;
            default:
                return Error::from_string_view("Invalid alpha compression method"sv);
            }
            context.alpha.data = chunk->data.slice(1);
            break;
        }

        offset += 8 + chunk->data.size() + (chunk->data.size() & 1);
    }

    if (!has_image_data)
        return Error::from_string_view("No image data chunk found"sv);

    return {};
}

static ErrorOr<void> try_decode_frame_header(WebPLoadingContext& context)
{
    switch (context.format) {
    case Format::Lossy:
        context.image_size = TRY(context.vp8_decoder->try_decode_frame_header());
        dbgln_if(WEBP_DEBUG, "VP8: image_size={}", context.image_size);
        return {};
    case Format::Lossless:
        context.image_size = TRY(context.lossless_decoder->try_decode_frame_header());
        dbgln_if(WEBP_DEBUG, "VP8L: image_size={}", context.image_size);
        return {};
    }
    VERIFY_NOT_REACHED();
}

static ALWAYS_INLINE void set_alpha_data(Gfx::Bitmap& target, Function<u8(int,int)> alpha)
{
    for (int y = 0; y < target.height(); y++) {
        auto* scanline = reinterpret_cast<Color*>(target.scanline(y));
        for (int x = 0; x < target.width(); x++)
            scanline[x].set_alpha(alpha(x, y));
    }
}

static ErrorOr<void> try_decode_image(WebPLoadingContext& context)
{
    switch (context.format) {
    case Format::Lossy:
        context.bitmap = TRY(context.vp8_decoder->try_decode_image(context.alpha.format != AlphaFormat::None ? Gfx::BitmapFormat::BGRA8888 : Gfx::BitmapFormat::BGRx8888));
        break;
    case Format::Lossless:
        context.bitmap = TRY(context.lossless_decoder->try_decode_image());
        break;
    }

    auto width = context.image_size.width();
    auto height = context.image_size.height();

    if (context.alpha.format == AlphaFormat::Uncompressed) {
        if (context.alpha.data.size() != static_cast<size_t>(width) * static_cast<size_t>(height))
            return Error::from_string_view("Alpha layer size does not match image"sv);

        set_alpha_data(*context.bitmap, [width, data = context.alpha.data](int x, int y) {
            return data[y * width + x];
        });
    } else if (context.alpha.format == AlphaFormat::Compressed) {
        Lossless::LosslessDecoder decoder { context.alpha.data };
        auto alpha_image = TRY(decoder.try_decode_image());

        if (alpha_image->size() != context.image_size)
            return Error::from_string_view("Alpha layer size does not match image"sv);

        set_alpha_data(*context.bitmap, [alpha_image](int x, int y) {
            return alpha_image->get_pixel(x, y).green();
        });
    }

    return {};
}

template<auto try_stage, WebPLoadingContext::State stage>
static bool decode_stage(WebPLoadingContext& context)
{
    if (context.state >= stage)
        return true;
    if (context.state == WebPLoadingContext::Error)
        return false;
    auto error = try_stage(context);
    if (error.is_error()) {
        context.error = error.release_error();
        context.state = WebPLoadingContext::Error;
        return false;
    }
    context.state = stage;
    return true;
}

static bool decode_webp_header(WebPLoadingContext& context)
{
    return decode_stage<try_decode_webp_header, WebPLoadingContext::HeaderDecoded>(context);
}

static bool decode_frame_header(WebPLoadingContext& context)
{
    return decode_webp_header(context) && decode_stage<try_decode_frame_header, WebPLoadingContext::FrameHeaderDecoded>(context);
}

static bool decode_image(WebPLoadingContext& context)
{
    return decode_frame_header(context) && decode_stage<try_decode_image, WebPLoadingContext::BitmapDecoded>(context);
}

bool WebPImageDecoderPlugin::sniff()
{
    return decode_webp_header(*m_context);
}

Gfx::IntSize WebPImageDecoderPlugin::size()
{
    if (!decode_frame_header(*m_context))
        return {};
    return m_context->image_size;
}

size_t WebPImageDecoderPlugin::frame_count()
{
    return 1;
}

bool WebPImageDecoderPlugin::is_animated()
{
    return false;
}

size_t WebPImageDecoderPlugin::loop_count()
{
    return 0;
}

void WebPImageDecoderPlugin::set_volatile()
{
    if (m_context->bitmap)
        m_context->bitmap->set_volatile();
}

bool WebPImageDecoderPlugin::set_nonvolatile(bool& was_purged)
{
    if (!m_context->bitmap)
        return false;
    return m_context->bitmap->set_nonvolatile(was_purged);
}

ErrorOr<Gfx::ImageFrameDescriptor> WebPImageDecoderPlugin::frame(size_t index)
{
    if (index > 0)
        return Error::from_string_view("WebPImageDecoderPlugin: Invalid frame index"sv);

    if (!decode_image(*m_context)) {
        if (m_context->error.has_value()) {
            dbgln_if(WEBP_DEBUG, "Error: {}", m_context->error.value());
            return m_context->error.value();
        }
        return Error::from_string_view("WebPImageDecoderPlugin: Decoding failed"sv);
    }

    return Gfx::ImageFrameDescriptor { m_context->bitmap, 0 };
}

}
