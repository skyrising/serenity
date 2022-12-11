/*
 * Copyright (c) 2022, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Bitmap.h>
#include <AK/Debug.h>
#include <AK/FixedArray.h>
#include <AK/Function.h>
#include <AK/String.h>
#include <LibGfx/Bitmap.h>
#include <LibGfx/WebP/Common.h>
#include <LibGfx/WebP/VP8Data.h>
#include <LibGfx/WebP/VP8Decoder.h>

namespace VP8 {

constexpr bool DEBUG_BITSTREAM = false;
constexpr bool DEBUG_PREDICT = true;
constexpr bool DEBUG_RESIDUALS = false;

class BitStream {
public:
    BitStream(ReadonlyBytes data)
    {
        if (data.size() >= 2) {
            m_value = data[0] << 8 | data[1];
            m_data = data.slice(2);
            dbgln_if(WEBP_DEBUG && DEBUG_BITSTREAM, "BitStream init: {:04x}", m_value);
        } else {
            m_data = data;
        }
    }

    [[nodiscard]] inline bool has_data()
    {
        return !m_data.is_empty();
    }

    [[nodiscard]] inline bool flag()
    {
        return get(128);
    }

    template<size_t bits>
    [[nodiscard]] inline u32 literal()
    {
        u32 value = 0;
        for (int bit = bits - 1; bit >= 0; bit--) {
            value |= flag() << bit;
        }
        return value;
    }

    template<size_t bits>
    [[nodiscard]] inline i32 signed_literal()
    {
        i32 abs = literal<bits>();
        return flag() ? -abs : abs;
    }

    template<size_t bits>
    [[nodiscard]] inline i32 maybe_signed_literal()
    {
        return flag() ? signed_literal<bits>() : 0;
    }

    [[nodiscard]] bool get(u8 probability)
    {
        u32 split = 1 + (((m_range - 1) * probability) >> 8);
        u32 split_shifted = split << 8;
        bool result = m_value >= split_shifted;
        m_range = result ? m_range - split : split;
        m_value = result ? m_value - split_shifted : m_value;

        while (m_range < 128) {
            m_value <<= 1;
            m_range <<= 1;

            m_index++;
            if (++m_bit_count == 8)
                read_byte();
        }

        return result;
    }

    template<typename LeafType>
    LeafType read_tree(TreeIndex<LeafType> const* tree, u8 const* probabilities, int start_index = 0)
    {
        int index = start_index;

        while (true) {
            u8 prob = probabilities[index >> 1];
            auto lookup_index = index + (get(prob) ? 1 : 0);
            auto next = tree[lookup_index];
            if (next.type() == TreeNodeType::LEAF) {
                return next.leaf_value();
            }
            index = next.index();
        }
    }

    ScopeGuard<Function<void()>> track(StringView name)
    {
        if constexpr (!WEBP_DEBUG || !DEBUG_BITSTREAM)
            return ScopeGuard { Function<void()> { [] {} } };
        size_t before = m_index;
        dbgln("{}: start {}", name, before);
        Function<void()> callback = [this, name, before]() {
            size_t after = m_index;
            dbgln("{}: end {} -> {}", name, after, after - before);
        };
        return ScopeGuard { move(callback) };
    }

    size_t bits_remaining()
    {
        return m_data.size() * 8 + 8 - m_bit_count;
    }

private:
    void read_byte()
    {
        m_bit_count = 0;
        if (!m_data.is_empty()) {
            m_value |= m_data[0];
            m_data = m_data.slice(1);
        } else {
            warnln_if(WEBP_DEBUG, "Ran out of bits");
        }
    }
    ReadonlyBytes m_data;
    u32 m_range = 255;
    u32 m_value;
    u8 m_bit_count = 0;
    size_t m_index = 0;
};

enum class ReconstructionFilter {
    None = 0,
    Bilinear,
    Bicubic,
};

ReconstructionFilter reconstruction_filters[] {
    ReconstructionFilter::Bicubic,
    ReconstructionFilter::Bilinear,
    ReconstructionFilter::Bilinear,
    ReconstructionFilter::None,
};

enum class LoopFilter {
    None,
    Simple,
    Normal,
};

LoopFilter loop_filters[] {
    LoopFilter::Normal,
    LoopFilter::Simple,
    LoopFilter::None,
    LoopFilter::None,
};

static constexpr size_t known_versions = min(array_size(reconstruction_filters), array_size(loop_filters));

struct SegmentHeader {
    bool enabled;
    bool update_data;
    bool update_map;
    bool absolute;
    u8 tree_probabilities[MB_FEATURE_TREE_PROBS];
    u8 loop_filter_level[MAX_MB_SEGMENTS];
    u8 quant_idx[MAX_MB_SEGMENTS];
};

struct LoopFilterHeader {
    bool use_simple;
    u8 level;
    u8 sharpness;
    bool delta_enabled;
    u8 ref_delta[BLOCK_CONTEXTS];
    u8 mode_delta[BLOCK_CONTEXTS];
};

struct PartitionHeader {
    u8 partitions;
    u32 partition_size[MAX_PARTITIONS];
};

struct QuantizationFactors {
    int dc;
    int ac;
};

struct QuantizationMatrix {
    QuantizationFactors y;
    QuantizationFactors y2;
    QuantizationFactors uv;
};

struct EntropyHeader {
    u8 coeff_probs[BLOCK_TYPES][COEF_BANDS][PREV_COEF_CONTEXTS][ENTROPY_NODES];
    bool coeff_skip_enabled;
    u8 coeff_skip_prob;
};

using DCTCoefficients = Array<i16, 16>;

struct MacroBlockCoefficients {
    Array<DCTCoefficients, 4ul * 4ul> y;
    Array<DCTCoefficients, 2ul * 2ul> u;
    Array<DCTCoefficients, 2ul * 2ul> v;
    Array<DCTCoefficients, 1> y2;
};

struct MacroBlock {
    bool skip;
    u8 segment;

    PredictionMode y_mode;
    Array<PredictionBMode, 4ul * 4ul> b_modes;
    PredictionMode uv_mode;

    Array<u8, 16ul * 16ul> y;
    Array<u8, 8ul * 8ul> u;
    Array<u8, 8ul * 8ul> v;

    Bytes slice_y(size_t row, size_t column, Optional<size_t> size = {})
    {
        VERIFY(row < 16ul && column < 16ul);
        size_t used_size = size.has_value() ? size.value() : 16ul * 16ul - row * 16ul - column;
        VERIFY(row * 16ul + column + used_size <= 16ul * 16ul);
        return { &y[row * 16 + column], used_size };
    }

    bool has_y2() const
    {
        return y_mode != PredictionMode::B_PRED;
    }
};

struct VP8Context {
    ReadonlyBytes image_data;
    Gfx::IntSize image_size { 0, 0 };
    struct {
        bool is_keyframe;
        u8 version;
        bool is_shown;
        u32 first_partition_size;
        ReconstructionFilter reconstruction_filter;
        LoopFilter loop_filter;
    } frame_header;
    ReadonlyBytes first_partition;
    struct {
        BitStream stream = { {} };
    } partitions[MAX_PARTITIONS];
};

VP8Decoder::VP8Decoder(ReadonlyBytes data)
{
    m_context = make<VP8Context>();
    m_context->image_data = data;
}

VP8Decoder::~VP8Decoder() = default;

// 9. Frame Header https://www.rfc-editor.org/rfc/rfc6386.html#section-9

template<size_t index, size_t length, typename T = u32>
static ALWAYS_INLINE constexpr T bits(T value)
{
    return (value >> index) & ((1 << length) - 1);
}

template<size_t index, typename T = u32>
static ALWAYS_INLINE constexpr bool bit(T value)
{
    return (value >> index) & 1;
}

ErrorOr<Gfx::IntSize> VP8Decoder::try_decode_frame_header()
{
    auto& context = *m_context;

    // 9.1. Uncompressed Data Chunk https://www.rfc-editor.org/rfc/rfc6386.html#section-9.1
    // The uncompressed data chunk comprises a common (for key frames and interframes) 3-byte frame tag that contains four fields, as follows:
    u32 frame_header = TRY(read_u24(context.image_data, 0));
    // 1. A 1-bit frame type (0 for key frames, 1 for interframes).
    context.frame_header.is_keyframe = !bit<0>(frame_header);
    // 2. A 3-bit version number (0 - 3 are defined as four different profiles with different decoding complexity;
    //    other values may be defined for future variants of the VP8 data format).
    context.frame_header.version = bits<1, 3>(frame_header);
    // 3. A 1-bit show_frame flag (0 when current frame is not for display, 1 when current frame is for display).
    context.frame_header.is_shown = bit<4>(frame_header);
    // 4. A 19-bit field containing the size of the first data partition in bytes.
    context.frame_header.first_partition_size = bits<5, 19>(frame_header);

    if (!context.frame_header.is_keyframe || !context.frame_header.is_shown)
        return Error::from_string_view("Expected visible keyframe"sv);

    if (context.frame_header.version >= known_versions)
        return Error::from_string_view("Unknown version"sv);

    context.frame_header.reconstruction_filter = reconstruction_filters[context.frame_header.version];
    context.frame_header.loop_filter = loop_filters[context.frame_header.version];

    size_t offset = 3;

    // For key frames, the frame tag is followed by a further 7 bytes of uncompressed data, as follows:
    //   Start code byte 0: 0x9d
    //   Start code byte 1: 0x01
    //   Start code byte 2: 0x2a
    u32 keyframe_magic = TRY(read_u24(context.image_data, offset));
    offset += 3;

    if (keyframe_magic != KEYFRAME_MAGIC)
        return Error::from_string_view("Invalid keyframe"sv);

    // 16 bits : (2 bits Horizontal Scale << 14) | Width (14 bits)
    // 16 bits : (2 bits Vertical Scale << 14) | Height (14 bits)
    u32 keyframe_header = TRY(read_u32(context.image_data, offset));
    offset += 4;

    if (bits<14, 2>(keyframe_header) != 0 || bits<30, 2>(keyframe_header) != 0)
        dbgln_if(WEBP_DEBUG, "VP8 frame header has scaling information, this is ignored in WebP");

    context.image_size = { bits<0, 14>(keyframe_header), bits<16, 14>(keyframe_header) };

    if (context.image_size.width() == 0 || context.image_size.height() == 0)
        return Error::from_string_view("Zero sized image"sv);

    Gfx::IntSize canvas_size = {
        (context.image_size.width() + 15) & ~0xf,
        (context.image_size.height() + 15) & ~0xf,
    };

    if (((u64)canvas_size.width()) * ((u64)canvas_size.height()) > (1ULL << 32))
        return Error::from_string_view("Image too large"sv);

    if (context.frame_header.first_partition_size + offset > context.image_data.size())
        return Error::from_string_view("Insufficient data for first partition"sv);

    context.first_partition = context.image_data.slice(offset, context.frame_header.first_partition_size);
    context.image_data = context.image_data.slice(offset + context.frame_header.first_partition_size);
    return context.image_size;
}

// 9.3. Segment-Based Adjustments https://www.rfc-editor.org/rfc/rfc6386.html#section-9.3

static void read_segment_header(BitStream& stream, SegmentHeader& header)
{
    // The context for decoding this feature at the macroblock level is provided by a subsection in the frame header, which contains:
    // 1. A segmentation_enabled flag that enables the feature for this frame if set to 1, and disables it if set to 0.
    header.enabled = stream.flag();
    //    The following fields occur if the feature is enabled.
    if (header.enabled) {
        // 2. L(1) indicates if the segment map is updated for the current frame (update_mb_segmentation_map).
        header.update_map = stream.flag();
        // 3.  L(1) indicates if the segment feature data items are updated for the current frame (update_segment_feature_data).
        header.update_data = stream.flag();
        // 4. If Item 3 above (update_segment_feature_data) is 1, the following fields occur:
        if (header.update_data) {
            // a. L(1), the mode of segment feature data (segment_feature_mode), can be absolute-value mode (0) or delta value mode (1).
            // FIXME: is this inverted?
            header.absolute = !stream.flag();
            // b. Segment feature data items are decoded segment by segment for each segment feature.
            // For every data item, a one-bit flag indicates whether the item is 0, or a non-zero value to be decoded.
            // If the value is non-zero, then the value is decoded as a magnitude L(n), followed by a one-bit sign (L(1) -- 0 for positive and 1 for negative).
            // The length n can be looked up from a pre-defined length table for all feature data.
            // NOTE: quantizer_update_value L(7) and lf_update_value L(6) found in https://www.rfc-editor.org/rfc/rfc6386.html#section-19.2
            for (int i = 0; i < MAX_MB_SEGMENTS; i++)
                header.quant_idx[i] = stream.maybe_signed_literal<7>();
            for (int i = 0; i < MAX_MB_SEGMENTS; i++)
                header.loop_filter_level[i] = stream.maybe_signed_literal<6>();
        }
        // 5.  If the L(1) flag as noted in Item 2 above is set to 1, the probabilities of the decoding tree for the segment map are decoded from the bitstream.
        if (header.update_map) {
            for (int i = 0; i < MB_FEATURE_TREE_PROBS; i++) {
                // Each probability is decoded with a one-bit flag
                // indicating whether the probability is the default value of 255 (flag is set to 0),
                // or an 8-bit value, L(8), from the bitstream.
                header.tree_probabilities[i] = stream.flag() ? stream.literal<8>() : 255;
            }
        }
    } else {
        header.update_map = false;
        header.update_data = false;
    }
    dbgln_if(WEBP_DEBUG, "segment_header: enabled={}, update_map={}, update_data={}", header.enabled, header.update_map, header.update_data);
    if (header.update_data)
        dbgln_if(WEBP_DEBUG, "segment_header: absolute={}, quant_ids=[{}], loop_filter_level=[{}]", header.absolute, String::join(", "sv, header.quant_idx), String::join(", "sv, header.loop_filter_level));
    if (header.update_map)
        dbgln_if(WEBP_DEBUG, "segment_header: tree_probabilities=[{}]", String::join(", "sv, header.tree_probabilities));
}

// 9.4. Loop Filter Type and Levels https://www.rfc-editor.org/rfc/rfc6386.html#section-9.4

static void read_loop_filter_header(BitStream& stream, LoopFilterHeader& header)
{
    header.use_simple = stream.flag();
    header.level = stream.literal<6>();
    header.sharpness = stream.literal<3>();
    header.delta_enabled = stream.flag();

    dbgln_if(WEBP_DEBUG, "loop_filter_header: use_simple={}, level={}, sharpness={}, delta_enabled={}", header.use_simple, header.level, header.sharpness, header.delta_enabled);
    if (header.delta_enabled && stream.flag()) {
        for (int i = 0; i < BLOCK_CONTEXTS; i++)
            header.ref_delta[i] = stream.signed_literal<6>();
        for (int i = 0; i < BLOCK_CONTEXTS; i++)
            header.mode_delta[i] = stream.signed_literal<6>();
        dbgln_if(WEBP_DEBUG, "loop_filter_header: ref_delta=[{}], mode_delta=[{}]", String::join(", "sv, header.ref_delta), String::join(", "sv, header.mode_delta));
    }
}

// 9.5. Token Partition and Partition Data Offsets https://www.rfc-editor.org/rfc/rfc6386.html#section-9.5

ErrorOr<void> VP8Decoder::read_partition_header(BitStream& stream, PartitionHeader& header)
{
    auto& context = *m_context;

    // A two-bit L(2) is used to indicate the number of coefficient data partitions within a compressed frame.
    // The two bits are defined in the following table:
    //   00 -> 1
    //   01 -> 2
    //   10 -> 4
    //   11 -> 8
    header.partitions = 1 << stream.literal<2>();
    dbgln_if(WEBP_DEBUG, "{} partitions:", header.partitions);

    auto last = header.partitions - 1;
    if (context.image_data.size() < 3 * (size_t)last)
        return Error::from_string_view("Insufficient data for partition sizes"sv);

    size_t offset = 0;
    size_t used_size = 0;

    // If the number of data partitions is greater than 1, the size of each partition (except the last) is written in 3 bytes (24 bits).
    for (int i = 0; i < last; i++) {
        auto size = header.partition_size[i] = read_u24(context.image_data, offset).release_value();
        offset += 3;
        used_size += size;
    }
    used_size += offset;

    if (used_size > context.image_data.size())
        return Error::from_string_view("Truncated partition data"sv);

    // The size of the last partition is the remainder of the data not used by any of the previous partitions.
    header.partition_size[last] = context.image_data.size() - used_size;

    for (int i = 0; i < header.partitions; i++) {
        auto size = header.partition_size[i];
        dbgln_if(WEBP_DEBUG, "- 0x{:06x} bytes", size);
        context.partitions[i].stream = BitStream(context.image_data.slice(offset, size));
        offset += size;
    }

    return {};
}

// 9.6. Dequantization Indices https://www.rfc-editor.org/rfc/rfc6386.html#section-9.6

static void read_quantization_matrix(BitStream& stream, SegmentHeader const& segment, Array<QuantizationMatrix, BLOCK_CONTEXTS>& matrixes)
{
    // The first 7-bit index gives the dequantization table index for Y-plane AC coefficients, called yac_qi.
    // It is always coded and acts as a baseline for the other 5 quantization indices, each of which is represented by a delta from this baseline index.
    auto y_ac_qi = stream.literal<7>();
    auto y_dc_delta = stream.maybe_signed_literal<4>();
    auto y2_dc_delta = stream.maybe_signed_literal<4>();
    auto y2_ac_delta = stream.maybe_signed_literal<4>();
    auto uv_dc_delta = stream.maybe_signed_literal<4>();
    auto uv_ac_delta = stream.maybe_signed_literal<4>();

    for (int i = 0; i < BLOCK_CONTEXTS; i++) {
        u32 q;
        if (segment.enabled) {
            q = segment.quant_idx[i];
            if (!segment.absolute)
                q += y_ac_qi;
        } else if (i > 0) {
            matrixes[i] = matrixes[0];
            continue;
        } else {
            q = y_ac_qi;
        }

        dbgln_if(WEBP_DEBUG, "{} q={}, y_dc_delta={}, y2_dc_delta={}, y2_ac_delta={}, uv_dc_delta={}, uv_ac_delta={}", i, q, y_dc_delta, y2_dc_delta, y2_ac_delta, uv_dc_delta, uv_ac_delta);

        auto& matrix = matrixes[i];
        matrix.y.dc = dcTable[clamp(q + y_dc_delta, 0, 127)];
        matrix.y.ac = acTable[clamp(q, 0, 127)];

        matrix.y2.dc = dcTable[clamp(q + y2_dc_delta, 0, 127)] * 2;
        matrix.y2.ac = acTable[clamp(q + y2_ac_delta, 0, 127)] * 155 / 100;
        if (matrix.y2.ac < 8)
            matrix.y2.ac = 8;

        matrix.uv.dc = dcTable[clamp(q + uv_dc_delta, 0, 117)];
        matrix.uv.ac = acTable[clamp(q + uv_ac_delta, 0, 127)];
    }
}

// 9.9. DCT Coefficient Probability Update https://www.rfc-editor.org/rfc/rfc6386.html#section-9.9

static void read_entropy_header(BitStream& stream, EntropyHeader& header)
{
    // For each of the probabilities in the tables, there is an L(1) flag indicating if the probability is updated for the current frame,
    // and if the L(1) flag is set to 1, there follows an additional 8-bit value representing the new probability value.
    for (int i = 0; i < BLOCK_TYPES; i++)
        for (int j = 0; j < COEF_BANDS; j++)
            for (int k = 0; k < PREV_COEF_CONTEXTS; k++)
                for (int l = 0; l < ENTROPY_NODES; l++)
                    if (stream.get(coeff_entropy_update_probs[i][j][k][l])) {
                        header.coeff_probs[i][j][k][l] = stream.literal<8>();
                        dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "coeff_probs[{}][{}][{}][{}] = {}", i, j, k, l, header.coeff_probs[i][j][k][l]);
                    }

    // 9.11. Remaining Frame Header Data (Key Frame) https://www.rfc-editor.org/rfc/rfc6386.html#section-9.11
    header.coeff_skip_enabled = stream.flag();
    header.coeff_skip_prob = header.coeff_skip_enabled ? stream.literal<8>() : 0;
}

static void read_macro_block_header(BitStream& stream, EntropyHeader const& entropy_header, SegmentHeader const& segment, MacroBlock& block, Span<PredictionBMode const> above, Span<PredictionBMode> left)
{
    // 10. Segment-Based Feature Adjustments https://www.rfc-editor.org/rfc/rfc6386.html#section-10
    // If both the segmentation_enabled and update_mb_segmentation_map flags in subsection B of the frame header take a value of 1,
    // NOTE: segment.update_map implies segment.enabled
    if (segment.update_map) {
        // the prediction data for each (intra- or inter-coded) macroblock begins with a specification of segment_id for the current macroblock.
        block.segment = stream.read_tree(mb_segment_tree, segment.tree_probabilities);
    } else {
        block.segment = 0;
    }

    // 11. Key Frame Macroblock Prediction Records https://www.rfc-editor.org/rfc/rfc6386.html#section-11

    // 11.1. mb_skip_coeff
    // The single bool flag is decoded using prob_skip_false if and only if mb_no_skip_coeff is set to 1 (see Sections 9.10 and 9.11).
    // If mb_no_skip_coeff is set to 0, then this value defaults to 0.
    block.skip = entropy_header.coeff_skip_enabled && stream.get(entropy_header.coeff_skip_prob);

    // 11.2. Luma Modes
    // First comes the luma specification of type intra_mbmode, coded using the kf_ymode_tree
    // For key frames, the Y mode is decoded using a fixed probability array: kf_ymode_prob
    block.y_mode = stream.read_tree(kf_ymode_tree, kf_ymode_prob);

    // If the Ymode is B_PRED, it is followed by a (tree-coded) mode for each of the 16 Y subblocks.
    if (block.y_mode == PredictionMode::B_PRED) {
        auto above_temp = above;
        for (size_t y = 0; y < 4; y++) {
            for (size_t x = 0; x < 4; x++) {
                // The dependence of subblock mode probability on the nearby subblock mode context is most easily handled using a three-dimensional constant array: kf_bmode_prob
                // The outer two dimensions of this array are indexed by the already-coded subblock modes above and to the left of the current block, respectively.
                auto mode = stream.read_tree(bmode_tree, kf_bmode_prob[to_underlying(above_temp[x])][to_underlying(left[y])]);
                dbgln_if(WEBP_DEBUG && DEBUG_PREDICT, "b_mode[{}][{}]: above={} left={} -> {}", y, x, prediction_bmode_names[to_underlying(above_temp[x])], prediction_bmode_names[to_underlying(left[y])], prediction_bmode_names[to_underlying(mode)]);
                left[y] = mode;
                block.b_modes[y * 4 + x] = mode;
            }
            above_temp = Span<PredictionBMode> { block.b_modes }.slice(y * 4, 4);
        }
    } else {
        // 11.3.4
        // Many macroblocks will of course be coded using a 16x16 luma prediction mode.
        // For the purpose of predicting ensuing subblock modes (only), such macroblocks derive a subblock mode,
        // constant throughout the macroblock, from the 16x16 luma mode as follows:
        PredictionBMode b_mode;
        switch (block.y_mode) {
        // DC_PRED uses B_DC_PRED,
        case PredictionMode::DC_PRED:
            b_mode = PredictionBMode::DC_PRED;
            break;
        // V_PRED uses B_VE_PRED,
        case PredictionMode::V_PRED:
            b_mode = PredictionBMode::VE_PRED;
            break;
        // H_PRED uses B_HE_PRED,
        case PredictionMode::H_PRED:
            b_mode = PredictionBMode::HE_PRED;
            break;
        // and TM_PRED uses B_TM_PRED.
        case PredictionMode::TM_PRED:
            b_mode = PredictionBMode::TM_PRED;
            break;
        case PredictionMode::B_PRED:
            VERIFY_NOT_REACHED();
        }
        block.b_modes.fill(b_mode);
        left.fill(b_mode);
    }

    // 11.4. Chroma Modes
    // After the Y mode (and optional subblock mode) specification comes the chroma mode.
    // The chroma modes are a subset of the Y modes and are coded using the uv_mode_tree
    // As for the Y modes (in a key frame), the chroma modes are coded using a fixed, contextless probability table: kf_uv_mode_prob
    block.uv_mode = stream.read_tree(uv_mode_tree, kf_uv_mode_prob);
}

// 12. Intraframe Prediction https://www.rfc-editor.org/rfc/rfc6386.html#section-12
// 12.2. Chroma Prediction https://www.rfc-editor.org/rfc/rfc6386.html#section-12.2

template<size_t Size>
static void predict_simple(PredictionMode mode, Bytes data, ReadonlyBytes above, ReadonlyBytes left, u8 left_above, size_t mb_x, size_t mb_y)
{
    // NOTE: predict_simple is also used for "12.3. Luma Prediction" using Size = 16.
    //       The spec comments refer to chroma prediction with Size = 8.
    constexpr int Log2Size = count_trailing_zeroes(Size);
    static_assert(1 << Log2Size == Size);

    switch (mode) {
    case PredictionMode::V_PRED:
        // Vertical prediction (chroma mode V_PRED) simply fills each 8-pixel row of the 8x8 chroma block with a copy of the "above" row (A).
        for (size_t i = 0; i < Size; i++)
            above.copy_trimmed_to(data.slice(Size * i, Size));
        break;
    case PredictionMode::H_PRED:
        // Similarly, horizontal prediction (H_PRED) fills each 8-pixel column of the 8x8 chroma block with a copy of the "left" column (L).
        for (size_t i = 0; i < Size; i++)
            data.slice(Size * i, Size).fill(left[i]);
        break;
    case PredictionMode::DC_PRED: {
        int dc_value;
        if (mb_x || mb_y) {
            // For DC_PRED, apart from the exceptional case of the top-left macroblock,
            // we are averaging either 16 or 8 pixel values to get a single prediction value that fills the 8x8 block
            int sum = 0;
            u32 shift = mb_x && mb_y ? Log2Size + 1 : Log2Size;
            if (mb_y) {
                for (size_t i = 0; i < Size; i++)
                    sum += above[i];
            }
            if (mb_x) {
                for (size_t i = 0; i < Size; i++)
                    sum += left[i];
            }
            dc_value = (sum + (1 << (shift - 1))) >> shift;
        } else {
            // In the case of the leftmost macroblock on the top row of the frame, the 8x8 block is simply filled with the constant value 128.
            dc_value = 128;
        }
        data.fill(dc_value);
        break;
    case PredictionMode::TM_PRED:
        // TM_PRED uses the following equation to calculate X_ij:
        //   X_ij = L_i + A_j - P
        for (size_t i = 0; i < Size; i++)
            for (size_t j = 0; j < Size; j++)
                data[i * Size + j] = clamp(left[i] + above[j] - left_above, 0, 255);
        break;
    }
    default:
        VERIFY_NOT_REACHED();
    }
}

// 12.3. Luma Prediction https://www.rfc-editor.org/rfc/rfc6386.html#section-12.3

static ALWAYS_INLINE u8 average_pixels(u8 x, u8 y)
{
    return static_cast<u8>((x + y + 1) >> 1);
}

static ALWAYS_INLINE u8 average_pixels(u8 x, u8 y, u8 z)
{
    return static_cast<u8>((x + y + y + z + 2) >> 2);
}

static ALWAYS_INLINE void fill_y_subblock(Bytes data, Function<int(size_t, size_t)> function)
{
    for (size_t i = 0; i < 4; i++) {
        for (size_t j = 0; j < 4; j++)
            data[i * 16 + j] = function(i, j);
    }
}

static void predict_bmode_sub_block(PredictionBMode mode, Bytes data, ReadonlyBytes above, ReadonlyBytes left, u8 left_above)
{
    switch (mode) {
    case PredictionBMode::DC_PRED: {
        // Average 8 pixels and fill prediction buffer with constant DC value
        int average = 4;
        for (size_t i = 0; i < 4; i++)
            average += above[i] + left[i];
        average >>= 3;
        fill_y_subblock(data, [average](auto, auto) {
            return average;
        });
        break;
    }
    case PredictionBMode::TM_PRED: {
        // Just like 16x16 TM_PRED
        fill_y_subblock(data, [left, above, left_above](auto i, auto j) {
            return clamp(left[i] + above[j] - left_above, 0, 255);
        });
        break;
    }
    case PredictionBMode::VE_PRED: {
        // Like 16x16 V_PRED except using averages
        // 0 1 2 3
        // 0 1 2 3
        // 0 1 2 3
        // 0 1 2 3
        Array<u8, 4> smoothed {
            average_pixels(left_above, above[0], above[1]),
            average_pixels(above[0], above[1], above[2]),
            average_pixels(above[1], above[2], above[3]),
            average_pixels(above[2], above[3], above[4]),
        };
        fill_y_subblock(data, [smoothed](auto, auto j) {
            return smoothed[j];
        });
        break;
    }
    case PredictionBMode::HE_PRED: {
        // Like 16x16 H_PRED except using averages
        // 0 0 0 0
        // 1 1 1 1
        // 2 2 2 2
        // 3 3 3 3
        Array<u8, 4> smoothed {
            average_pixels(left_above, left[0], left[1]),
            average_pixels(left[0], left[1], left[2]),
            average_pixels(left[1], left[2], left[3]),
            average_pixels(left[2], left[3], left[3]),
        };
        fill_y_subblock(data, [smoothed](auto i, auto) {
            return smoothed[i];
        });
        break;
    }
    case PredictionBMode::LD_PRED: {
        // southwest (left and down) step = (-1, 1)
        // 0 1 2 3
        // 1 2 3 4
        // 2 3 4 5
        // 3 4 5 6
        Array<u8, 7> smoothed = {
            average_pixels(above[0], above[1], above[2]), // 0,0
            average_pixels(above[1], above[2], above[3]), // 0,1 & 1,0
            average_pixels(above[2], above[3], above[4]), // 0,2 & 1,1 & 2,0
            average_pixels(above[3], above[4], above[5]), // 0,3 & 1,2 & 2,1 & 3,0
            average_pixels(above[4], above[5], above[6]), // 1,3 & 2,2 & 3,1
            average_pixels(above[5], above[6], above[7]), // 2,3 & 3,2
            average_pixels(above[6], above[7], above[7]), // 3,3
        };
        fill_y_subblock(data, [smoothed](auto i, auto j) {
            return smoothed[i + j];
        });
        break;
    }
    case PredictionBMode::RD_PRED: {
        // southeast (right and down) step = (1,1)
        // 3 4 5 6
        // 2 3 4 5
        // 1 2 3 4
        // 0 1 2 3
        Array<u8, 7> smoothed {
            average_pixels(left[3], left[2], left[1]),      // 3,0
            average_pixels(left[2], left[1], left[0]),      // 3,1 & 2,0
            average_pixels(left[1], left[0], left_above),   // 3,2 & 2,1 & 1,0
            average_pixels(left[0], left_above, above[0]),  // 3,3 & 2,2 & 1,1 & 0,0
            average_pixels(left_above, above[0], above[1]), // 2,3 & 1,2 & 0,1
            average_pixels(above[0], above[1], above[2]),   // 1,3 & 0,2
            average_pixels(above[1], above[2], above[3]),   // 0,3
        };
        fill_y_subblock(data, [smoothed](auto i, auto j) {
            return smoothed[3 - i + j];
        });
        break;
    }
    case PredictionBMode::VR_PRED: {
        // SSE (vertical right) step = (2,1)
        // 3 5 7 9
        // 2 4 6 8
        // 1 3 5 7
        // 0 2 4 6
        Array<u8, 10> smoothed {
            average_pixels(left[2], left[1], left[0]),      // 3,0
            average_pixels(left[1], left[0], left_above),   // 2,0
            average_pixels(left[0], left_above, above[0]),  // 3,1 & 1,0
            average_pixels(left_above, above[0]),           // 2,1 & 0,0
            average_pixels(left_above, above[0], above[1]), // 3,2 & 1,1
            average_pixels(above[0], above[1]),             // 2,2 & 0,1
            average_pixels(above[0], above[1], above[2]),   // 3,3 & 1,2
            average_pixels(above[1], above[2]),             // 2,3 & 0,2
            average_pixels(above[1], above[2], above[3]),   // 1,3
            average_pixels(above[2], above[3]),             // 0,3
        };
        fill_y_subblock(data, [smoothed](auto i, auto j) {
            return smoothed[3 - i + j * 2];
        });
        break;
    }
    case PredictionBMode::VL_PRED: {
        // SSW (vertical left) step = (2,-1)
        // 0 2 4 6
        // 1 3 5 7
        // 2 4 6 8
        // 3 5 7 9
        Array<u8, 10> smoothed {
            average_pixels(above[0], above[1]),           // 0,0
            average_pixels(above[0], above[1], above[2]), // 1,0
            average_pixels(above[1], above[2]),           // 2,0 & 0,1
            average_pixels(above[1], above[2], above[3]), // 1,1 & 3,0
            average_pixels(above[2], above[3]),           // 2,1 & 0,2
            average_pixels(above[2], above[3], above[4]), // 3,1 & 1,2
            average_pixels(above[3], above[4]),           // 2,2 & 0,3
            average_pixels(above[3], above[4], above[5]), // 3,2 & 1,3
            average_pixels(above[4], above[5], above[6]), // 2,3
            average_pixels(above[5], above[6], above[7]), // 3,3
        };
        fill_y_subblock(data, [smoothed](auto i, auto j) {
            return smoothed[i + j * 2];
        });
        break;
    }
    case PredictionBMode::HD_PRED: {
        // ESE (horizontal down) step = (1,2)
        // 6 7 8 9
        // 4 5 6 7
        // 2 3 4 5
        // 0 1 2 3
        Array<u8, 10> smoothed {
            average_pixels(left[3], left[2]),               // 3,0
            average_pixels(left[3], left[2], left[1]),      // 3,1
            average_pixels(left[2], left[1]),               // 2,0 & 3,2
            average_pixels(left[2], left[1], left[0]),      // 2,1 & 3,3
            average_pixels(left[1], left[0]),               // 2,2 & 1,0
            average_pixels(left[1], left[0], left_above),   // 2,3 & 1,1
            average_pixels(left[0], left_above),            // 1,2 & 0,0
            average_pixels(left[0], left_above, above[0]),  // 1,3 & 0,1
            average_pixels(left_above, above[0], above[1]), // 0,2
            average_pixels(above[0], above[1], above[2]),   // 0,3
        };
        fill_y_subblock(data, [smoothed](auto i, auto j) {
            return smoothed[(3 - i) * 2 + j];
        });
        break;
    }
    case PredictionBMode::HU_PRED: {
        // ENE (horizontal up) step = (1,-2)
        // 0 1 2 3
        // 2 3 4 5
        // 4 5 6 7
        // 6 7 8 9
        Array<u8, 10> smoothed {
            average_pixels(left[0], left[1]),          // 0,0
            average_pixels(left[0], left[1], left[2]), // 0,1
            average_pixels(left[1], left[2]),          // 0,2 & 1,0
            average_pixels(left[1], left[2], left[3]), // 0,3 & 1,1
            average_pixels(left[2], left[3]),          // 1,2 & 2,0
            average_pixels(left[2], left[3], left[3]), // 1,3 & 2,1
            left[3],                                   // 2,2 & 3,0
            left[3],                                   // 2,3 & 3,1
            left[3],                                   // 3,2
            left[3],                                   // 3,3
        };
        fill_y_subblock(data, [smoothed](auto i, auto j) {
            return smoothed[i * 2 + j];
        });
        break;
    }
    }
    dbgln_if(WEBP_DEBUG && DEBUG_PREDICT, "mode: {}, P: {:02x}, A: {}, L: {}", prediction_bmode_names[to_underlying(mode)], left_above, above, left);
    for (size_t i = 0; i < 4; i++)
        dbgln_if(WEBP_DEBUG && DEBUG_PREDICT, "  {}", data.slice(i * 16, 4));
}

template<size_t i, size_t j>
static ALWAYS_INLINE void predict_bmode_sub_block(MacroBlock& block, ReadonlyBytes above, Bytes left, u8& left_above)
{
    auto sub_block_data = block.slice_y(i * 4, j * 4);
    predict_bmode_sub_block(block.b_modes[i * 4 + j], sub_block_data, above.slice(j * 4, 8), left.slice(i * 4, 4), left_above);
    for (size_t k = 0; k < 4; k++)
        left[i * 4 + k] = sub_block_data[k * 16 + 3];
    left_above = above[j * 4 + 3];
}

static void predict_bmode(MacroBlock& block, ReadonlyBytes above, ReadonlyBytes left, u8 left_above)
{
    VERIFY(above.size() == 20);
    Array<u8, 20> above_temp;
    above.copy_to(above_temp);

    VERIFY(left.size() == 16);
    Array<u8, 16> left_temp;
    left.copy_to(left_temp);

    u8 left_above_temp = left_above;

    predict_bmode_sub_block<0, 0>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<0, 1>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<0, 2>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<0, 3>(block, above_temp, left_temp, left_above_temp);

    block.slice_y(3, 0, 16).copy_to(above_temp);
    left_above_temp = left[3];

    predict_bmode_sub_block<1, 0>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<1, 1>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<1, 2>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<1, 3>(block, above_temp, left_temp, left_above_temp);

    block.slice_y(7, 0, 16).copy_to(above_temp);
    left_above_temp = left[7];

    predict_bmode_sub_block<2, 0>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<2, 1>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<2, 2>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<2, 3>(block, above_temp, left_temp, left_above_temp);

    block.slice_y(11, 0, 16).copy_to(above_temp);
    left_above_temp = left[11];

    predict_bmode_sub_block<3, 0>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<3, 1>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<3, 2>(block, above_temp, left_temp, left_above_temp);
    predict_bmode_sub_block<3, 3>(block, above_temp, left_temp, left_above_temp);
}

// 13.3. https://www.rfc-editor.org/rfc/rfc6386.html#page-66
static bool read_coefficients_for_sub_block(BitStream& stream, EntropyHeader const& entropy_header, DCTCoefficients& coefficients, PlaneType plane, bool non_zero_above, bool non_zero_left, QuantizationFactors const& factors)
{
    auto non_zero = false;
    dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "plane={}, above={}, left={}", to_underlying(plane), non_zero_above, non_zero_left);
    coefficients.fill(0);
    DCTToken token = DCTToken::EOB;
    int first_coeff = plane == PlaneType::YAfterY2 ? 1 : 0;

    int ctx3 = 0;
    if (non_zero_above)
        ctx3++;
    if (non_zero_left)
        ctx3++;

    for (int i = first_coeff; i < 16; i++) {
        // auto track = stream.track(String::formatted("coefficient {}", i));
        int ctx2 = coeff_bands[i];
        token = stream.read_tree(dct_token_tree, entropy_header.coeff_probs[to_underlying(plane)][ctx2][ctx3], token == DCTToken::VALUE_0 ? 2 : 0);
        if (token == DCTToken::EOB)
            break;

        // All tokens (except end-of-block) specify either a single unsigned value
        int value = dct_token_base[to_underlying(token)];

        u8 const* extra_bits_prob = dct_token_extra_bits_prob[to_underlying(token)];
        if (extra_bits_prob) {
            // or a range of unsigned values (immediately) followed by a simple probabilistic encoding of the offset of the value from the base of that range.
            int extra = 0;
            do {
                extra = extra * 2 + (stream.get(*extra_bits_prob) ? 1 : 0);
                ++extra_bits_prob;
            } while (*extra_bits_prob != 0);
            dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "extra bits[{}]: {} + {} = {}", strlen(reinterpret_cast<char const*>(dct_token_extra_bits_prob[to_underlying(token)])), value, extra, value + extra);
            value += extra;
        }

        if (value == 0)
            ctx3 = 0;
        else if (value == 1)
            ctx3 = 1;
        else
            ctx3 = 2;

        if (value != 0) {
            non_zero = true;

            // Non-zero values (of either type) are then followed by a flag indicating the sign of the coded value (negative if 1, positive if 0).
            if (stream.flag())
                value = -value;
        }

        // 14.1. Dequantization
        // After decoding the DCTs/WHTs as described above, each (quantized) coefficient in each subblock is multiplied by one of six dequantization factors,
        // the choice of factor depending on the plane (Y2, Y, or chroma) and position (DC = coefficient zero, AC = any other coefficient).
        value *= i == 0 ? factors.dc : factors.ac;

        coefficients[zig_zag[i]] = static_cast<i16>(value);
        dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "{} = {}", i, value);
    }

    return non_zero;
}

template<size_t Size>
static ALWAYS_INLINE size_t non_zero_index(size_t macro_block_x, size_t sub_block_x)
{
    return macro_block_x * Size + sub_block_x;
}

template<size_t Size>
static bool read_coefficients(BitStream& stream, Array<DCTCoefficients, Size * Size>& coefficients, EntropyHeader const& entropy_header, AK::Bitmap& non_zero, size_t mb_x, PlaneType plane, QuantizationFactors const& factors)
{
    auto any_non_zero = false;
    for (size_t sb_y = 0; sb_y < Size; sb_y++) {
        for (size_t sb_x = 0; sb_x < Size; sb_x++) {
            dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "{},{} {}", sb_y, sb_x, "{");
            auto above_non_zero = non_zero.get(non_zero_index<Size>(mb_x, sb_x));

            auto left_non_zero = false;
            if (sb_x > 0) {
                dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "left index: {}", non_zero_index<Size>(mb_x, sb_x - 1));
                left_non_zero = non_zero.get(non_zero_index<Size>(mb_x, sb_x - 1));
            } else if (mb_x > 0) {
                dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "left index: {}", non_zero_index<Size>(mb_x - 1, Size - 1));
                left_non_zero = non_zero.get(non_zero_index<Size>(mb_x - 1, Size - 1));
            }

            auto& coeffs = coefficients[sb_x + sb_y * Size];
            auto non_zero_sub_block = read_coefficients_for_sub_block(stream, entropy_header, coeffs, plane, above_non_zero, left_non_zero, factors);
            non_zero.set(non_zero_index<Size>(mb_x, sb_x), non_zero_sub_block);
            any_non_zero |= non_zero_sub_block;
            dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "{}+({},{}) -> {}/{} {}, [{}]", mb_x, sb_y, sb_x, above_non_zero, non_zero.size(), non_zero_sub_block, String::join(", "sv, coeffs));
            dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "{}", "}");
        }
    }

    return any_non_zero;
}

// 14.3. Implementation of the WHT Inversion https://www.rfc-editor.org/rfc/rfc6386.html#section-14.3

static void invert_wht(Span<i16> data)
{
    for (size_t i = 0; i < 4; i++) {
        auto column = data.slice(i);

        int a = column[0] + column[12];
        int b = column[4] + column[8];
        int c = column[4] - column[8];
        int d = column[0] - column[12];

        column[0] = static_cast<i16>(a + b);
        column[4] = static_cast<i16>(c + d);
        column[8] = static_cast<i16>(a - b);
        column[12] = static_cast<i16>(d - c);
    }

    for (size_t i = 0; i < 4; i++) {
        auto row = data.slice(i * 4);

        int a = row[0] + row[3];
        int b = row[1] + row[2];
        int c = row[1] - row[2];
        int d = row[0] - row[3];

        row[0] = static_cast<i16>((a + b + 3) >> 3);
        row[1] = static_cast<i16>((c + d + 3) >> 3);
        row[2] = static_cast<i16>((a - b + 3) >> 3);
        row[3] = static_cast<i16>((d - c + 3) >> 3);
    }
}

// 14.4. Implementation of the DCT Inversion https://www.rfc-editor.org/rfc/rfc6386.html#section-14.4

static ALWAYS_INLINE int fixed_16_mul(i16 a, i16 b)
{
    return (a * b) >> 16;
}

static ALWAYS_INLINE int idct_sin_term(i16 x)
{
    return fixed_16_mul(x, sin_pi_over_8_times_sqrt_2);
}

static ALWAYS_INLINE int idct_cos_term(i16 x)
{
    return x + fixed_16_mul(x, cos_pi_over_8_times_sqrt_2_minus_1);
}

static void invert_dct(Span<i16 const> coefficients, Span<i16> output)
{
    constexpr size_t stride = 4;

    dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "coefficients:");
    for (size_t i = 0; i < 4; i++)
        dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, String::join(", "sv, coefficients.slice(i * 4, 4)));

    for (size_t i = 0; i < 4; i++) {
        auto in = coefficients.slice(i);
        auto out = output.slice(i);

        int a = in[0] + in[8];
        int b = in[0] - in[8];
        int c = idct_sin_term(in[4]) - idct_cos_term(in[12]);
        int d = idct_cos_term(in[4]) + idct_sin_term(in[12]);

        out[0] = static_cast<i16>(a + d);
        out[4] = static_cast<i16>(b - c);
        out[8] = static_cast<i16>(b + c);
        out[12] = static_cast<i16>(a - d);
    }

    dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "after columns:");

    for (size_t i = 0; i < 4; i++) {
        auto row = output.slice(i * stride, 4);
        dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, String::join(", "sv, row));

        int a = row[0] + row[2];
        int b = row[0] - row[2];
        int c = idct_sin_term(row[1]) - idct_cos_term(row[3]);
        int d = idct_cos_term(row[1]) + idct_sin_term(row[3]);

        row[0] = static_cast<i16>((a + d + 4) >> 3);
        row[1] = static_cast<i16>((b - c + 4) >> 3);
        row[2] = static_cast<i16>((b + c + 4) >> 3);
        row[3] = static_cast<i16>((a - d + 4) >> 3);
    }
}

template<size_t SubBlockCount, size_t Size = 4 * SubBlockCount>
static void add_residuals(Array<DCTCoefficients, SubBlockCount * SubBlockCount> const& coefficients, Array<u8, Size * Size>& pixels)
{
    Array<i16, 16> residual_buffer;
    for (size_t sb_y = 0; sb_y < SubBlockCount; sb_y++) {
        for (size_t sb_x = 0; sb_x < SubBlockCount; sb_x++) {
            invert_dct(coefficients[sb_y * SubBlockCount + sb_x], residual_buffer);
            dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "({}, {}):", sb_y, sb_x, String::join(", "sv, residual_buffer));
            for (size_t i = 0; i < 4; i++) {
                dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, String::join(", "sv, residual_buffer.span().slice(i * 4, 4)));
                for (size_t j = 0; j < 4; j++) {
                    u8& pixel = pixels[(4 * sb_y + i) * Size + 4 * sb_x + j];
                    i16 residual = residual_buffer[4 * i + j];
                    pixel = clamp(static_cast<int>(pixel) + residual, 0, 255);
                }
            }
        }
    }
}

template<size_t Size>
static void copy_to_plane(size_t mb_x, size_t mb_y, ReadonlyBytes macro_block_data, Bytes plane, size_t macro_blocks_per_row)
{
    size_t stride = macro_blocks_per_row * Size;
    for (size_t y = 0; y < Size; y++) {
        size_t offset = (mb_y * Size + y) * stride + mb_x * Size;
        macro_block_data.slice(y * Size, Size).copy_to(plane.slice(offset, Size));
    }
}

static constexpr Color yuv_to_rgb(u8 y, u8 u, u8 v)
{
    auto yf = static_cast<float>(y - 16);
    auto uf = static_cast<float>(u - 128);
    auto vf = static_cast<float>(v - 128);
    auto r = 1.164f * yf + 1.596f * vf;
    auto g = 1.164f * yf - 0.813f * vf - 0.391f * uf;
    auto b = 1.164f * yf + 2.018f * uf;
    return {
        static_cast<u8>(clamp(r, 0, 255)),
        static_cast<u8>(clamp(g, 0, 255)),
        static_cast<u8>(clamp(b, 0, 255)),
    };
}

static void copy_to_bitmap(Gfx::Bitmap& bitmap, ReadonlyBytes y_plane, ReadonlyBytes u_plane, ReadonlyBytes v_plane, size_t y_stride)
{
    size_t width = bitmap.width();
    size_t height = bitmap.height();
    for (size_t y = 0; y < height; y++) {
        for (size_t x = 0; x < width; x++) {
            auto c_y = y_plane[y * y_stride + x];
            auto c_u = u_plane[(y / 2) * (y_stride / 2) + (x / 2)];
            auto c_v = v_plane[(y / 2) * (y_stride / 2) + (x / 2)];
            auto color = yuv_to_rgb(c_y, c_u, c_v);
            bitmap.set_pixel(static_cast<int>(x), static_cast<int>(y), color);
        }
    }
}

[[maybe_unused]] static void debug_macro_block(MacroBlock const& macro_block, MacroBlockCoefficients const& coefficients)
{
    auto print_coeffs = [](StringView plane, int sb_x, int sb_y, DCTCoefficients const& coefficients) {
        StringBuilder builder;
        builder.append(plane);
        builder.appendff(" ({},{})", sb_x, sb_y);
        for (int j = 0; j < 16; j++) {
            builder.append((j & 3) != 0 ? ' ' : '\n');
            builder.appendff("{}", coefficients[j]);
        }
        dbgln_if(DEBUG_RESIDUALS, "{}", builder.string_view());
    };
    dbgln_if(DEBUG_PREDICT, "y_mode: {}", prediction_mode_names[to_underlying(macro_block.y_mode)]);
    if (macro_block.y_mode == PredictionMode::B_PRED) {
        for (int y = 0; y < 4; y++)
            dbgln_if(DEBUG_PREDICT, "  {} {} {} {}",
                prediction_bmode_names[to_underlying(macro_block.b_modes[y * 4 + 0])],
                prediction_bmode_names[to_underlying(macro_block.b_modes[y * 4 + 1])],
                prediction_bmode_names[to_underlying(macro_block.b_modes[y * 4 + 2])],
                prediction_bmode_names[to_underlying(macro_block.b_modes[y * 4 + 3])]);
    }
    dbgln_if(DEBUG_PREDICT, "uv_mode: {}", prediction_mode_names[to_underlying(macro_block.uv_mode)]);
    dbgln_if(DEBUG_RESIDUALS, "coefficients:");
    for (int sb_y = 0; sb_y < 4; sb_y++)
        for (int sb_x = 0; sb_x < 4; sb_x++)
            print_coeffs("Y "sv, sb_x, sb_y, coefficients.y[sb_y * 4 + sb_x]);
    for (int sb_y = 0; sb_y < 2; sb_y++)
        for (int sb_x = 0; sb_x < 2; sb_x++)
            print_coeffs("U "sv, sb_x, sb_y, coefficients.u[sb_y * 2 + sb_x]);
    for (int sb_y = 0; sb_y < 2; sb_y++)
        for (int sb_x = 0; sb_x < 2; sb_x++)
            print_coeffs("V "sv, sb_x, sb_y, coefficients.v[sb_y * 2 + sb_x]);
    print_coeffs("Y2"sv, 0, 0, coefficients.y2[0]);
    dbgln_if(WEBP_DEBUG, "y: {:p}", &macro_block.y);
    for (size_t row = 0; row < 16; row++)
        dbgln_if(WEBP_DEBUG, "{}", ReadonlyBytes { macro_block.y }.slice(row * 16, 16));
    dbgln_if(WEBP_DEBUG, "u: {:p}", &macro_block.u);
    for (size_t row = 0; row < 8; row++)
        dbgln_if(WEBP_DEBUG, "{}", ReadonlyBytes { macro_block.u }.slice(row * 8, 8));
    dbgln_if(WEBP_DEBUG, "v: {:p}", &macro_block.v);
    for (size_t row = 0; row < 8; row++)
        dbgln_if(WEBP_DEBUG, "{}", ReadonlyBytes { macro_block.v }.slice(row * 8, 8));
    dbgln_if(WEBP_DEBUG, "{}", ReadonlyBytes { &macro_block, sizeof(macro_block) });
}

static ErrorOr<void> try_decode_image_data(size_t macro_blocks_x, size_t macro_blocks_y, BitStream& stream, BitStream& image_data_stream, EntropyHeader const& entropy_header, SegmentHeader const& segment_header, Array<QuantizationMatrix, BLOCK_CONTEXTS> const& quantization_matrixes, Bytes y_plane, Bytes u_plane, Bytes v_plane)
{
    auto macro_block_row_a = TRY(FixedArray<MacroBlock>::try_create(macro_blocks_x));
    auto macro_block_row_b = TRY(FixedArray<MacroBlock>::try_create(macro_blocks_x));

    auto non_zero_y_coeffs = TRY(AK::Bitmap::try_create(macro_blocks_x * 4, false));
    auto non_zero_u_coeffs = TRY(AK::Bitmap::try_create(macro_blocks_x * 2, false));
    auto non_zero_v_coeffs = TRY(AK::Bitmap::try_create(macro_blocks_x * 2, false));
    auto non_zero_y2_coeffs = TRY(AK::Bitmap::try_create(macro_blocks_x * 1, false));

    auto* macro_block_row = &macro_block_row_a;
    auto* previous_macro_block_row = &macro_block_row_b;

    Span<PredictionBMode const> pred_above = bmode_default;
    Array<u8, 20> pixels_y_above;
    Array<u8, 16> pixels_u_above;
    Array<u8, 16> pixels_v_above;
    Array<PredictionBMode, 4> pred_left;
    Array<u8, 16> pixels_y_left;
    Array<u8, 8> pixels_u_left;
    Array<u8, 8> pixels_v_left;
    MacroBlockCoefficients coefficients;

    pixels_y_above.fill(PIXEL_ABOVE_DEFAULT);
    pixels_u_above.fill(PIXEL_ABOVE_DEFAULT);
    pixels_v_above.fill(PIXEL_ABOVE_DEFAULT);

    for (size_t mb_y = 0; mb_y < macro_blocks_y; mb_y++) {
        pred_left.fill(PredictionBMode::DC_PRED);
        pixels_y_left.fill(PIXEL_LEFT_DEFAULT);
        pixels_u_left.fill(PIXEL_LEFT_DEFAULT);
        pixels_v_left.fill(PIXEL_LEFT_DEFAULT);
        swap(macro_block_row, previous_macro_block_row);

        bool left_y2_exists = false;
        bool left_y2_non_zero = false;

        for (size_t mb_x = 0; mb_x < macro_blocks_x; mb_x++) {
            if (!image_data_stream.has_data())
                break;
            Optional<MacroBlock> block_left_above = {};
            if (mb_y > 0) {
                auto& block_above = previous_macro_block_row->at(mb_x);
                pred_above = Span<PredictionBMode const> { block_above.b_modes }.slice_from_end(4);
                if (mb_x + 1 < macro_blocks_x) {
                    auto& block_right_above = previous_macro_block_row->at(mb_x + 1);
                    block_right_above.y.span().slice_from_end(16).copy_trimmed_to(pixels_y_above.span().slice(16));
                }
                block_above.y.span().slice_from_end(16).copy_to(pixels_y_above);
                block_above.u.span().slice_from_end(8).copy_to(pixels_u_above);
                block_above.v.span().slice_from_end(8).copy_to(pixels_v_above);
                if (mb_x > 0) {
                    block_left_above = previous_macro_block_row->at(mb_x - 1);
                }
            }
            auto pixel_y_left_above = block_left_above.has_value() ? block_left_above->y.last() : PIXEL_ABOVE_DEFAULT;
            auto pixel_u_left_above = block_left_above.has_value() ? block_left_above->u.last() : PIXEL_ABOVE_DEFAULT;
            auto pixel_v_left_above = block_left_above.has_value() ? block_left_above->v.last() : PIXEL_ABOVE_DEFAULT;
            auto& macro_block = macro_block_row->at(mb_x);
            {
                auto track = stream.track("macro block header"sv);
                read_macro_block_header(stream, entropy_header, segment_header, macro_block, pred_above, pred_left);
            }
            if (macro_block.y_mode != PredictionMode::B_PRED) {
                predict_simple<16>(macro_block.y_mode, macro_block.y, pixels_y_above, pixels_y_left, pixel_y_left_above, mb_x, mb_y);
            } else {
                predict_bmode(macro_block, pixels_y_above, pixels_y_left, pixel_y_left_above);
            }
            predict_simple<8>(macro_block.uv_mode, macro_block.u, pixels_u_above, pixels_u_left, pixel_u_left_above, mb_x, mb_y);
            predict_simple<8>(macro_block.uv_mode, macro_block.v, pixels_v_above, pixels_v_left, pixel_v_left_above, mb_x, mb_y);

            // 13. DCT Coefficient Decoding https://www.rfc-editor.org/rfc/rfc6386.html#section-13

            // 13.1. Macroblock without Non-Zero Coefficient Values
            // If the flag within macroblock (MB) MODE_INFO indicates that a macroblock does not have any non-zero coefficients,
            // the decoding process of DCT coefficients is skipped for the macroblock.
            if (!macro_block.skip) {
                auto const& quantization_matrix = quantization_matrixes[macro_block.segment];
                PlaneType y_plane_type = PlaneType::YWithoutY2;

                // (13.) For all intra- and inter-prediction modes apart from B_PRED (intra: whose Y subblocks are independently predicted)
                // and SPLITMV (inter), each macroblock's residue record begins with the Y2 component of the residue, coded using a WHT.
                // B_PRED and SPLITMV coded macroblocks omit this WHT and specify the 0th DCT coefficient in each of the 16 Y subblocks.
                if (macro_block.has_y2()) {
                    if (left_y2_exists)
                        non_zero_y2_coeffs.set(mb_x - 1, left_y2_non_zero);
                    auto track2 = image_data_stream.track("Y2 residuals"sv);
                    left_y2_non_zero = read_coefficients<1>(image_data_stream, coefficients.y2, entropy_header, non_zero_y2_coeffs, mb_x, PlaneType::Y2, quantization_matrix.y2);
                    left_y2_exists = true;
                    y_plane_type = PlaneType::YAfterY2;
                }
                // After the optional Y2 block, the residue record continues with 16 DCTs for the Y subblocks,
                {
                    auto track2 = image_data_stream.track("Y residuals"sv);
                    read_coefficients<4>(image_data_stream, coefficients.y, entropy_header, non_zero_y_coeffs, mb_x, y_plane_type, quantization_matrix.y);

                    // 14.2. Inverse Transforms https://www.rfc-editor.org/rfc/rfc6386.html#section-14.2
                    // If the Y2 residue block exists (i.e., the macroblock luma mode is not SPLITMV or B_PRED)
                    if (macro_block.has_y2()) {
                        auto& y2_coefficients = coefficients.y2[0];
                        // it is inverted first (using the inverse WHT)
                        invert_wht(y2_coefficients);
                        dbgln_if(WEBP_DEBUG, "Inverted WHT: {}", String::join(", "sv, y2_coefficients));
                        // and the element of the result at row i, column j is used as the 0th coefficient of the Y subblock at position (i, j),
                        // that is, the Y subblock whose index is (i * 4) + j.
                        for (int i = 0; i < 16; i++)
                            coefficients.y[i][0] = y2_coefficients[i];
                    }
                    add_residuals<4>(coefficients.y, macro_block.y);
                }
                // followed by 4 DCTs for the U subblocks,
                {
                    auto track2 = image_data_stream.track("U residuals"sv);
                    read_coefficients<2>(image_data_stream, coefficients.u, entropy_header, non_zero_u_coeffs, mb_x, PlaneType::Chroma, quantization_matrix.uv);
                    add_residuals<2>(coefficients.u, macro_block.u);
                }
                // ending with 4 DCTs for the V subblocks.
                {
                    auto track2 = image_data_stream.track("V residuals"sv);
                    read_coefficients<2>(image_data_stream, coefficients.v, entropy_header, non_zero_v_coeffs, mb_x, PlaneType::Chroma, quantization_matrix.uv);
                    add_residuals<2>(coefficients.v, macro_block.v);
                }
                if constexpr (WEBP_DEBUG) {
                    dbgln("({},{})", mb_y, mb_x);
                    debug_macro_block(macro_block, coefficients);
                }
            }

            copy_to_plane<16>(mb_x, mb_y, macro_block.y, y_plane, macro_blocks_x);
            copy_to_plane<8>(mb_x, mb_y, macro_block.u, u_plane, macro_blocks_x);
            copy_to_plane<8>(mb_x, mb_y, macro_block.v, v_plane, macro_blocks_x);
        }
    }

    dbgln_if(WEBP_DEBUG, "memory: planes:{} non_zero_bitmaps:{} macro_block_rows:{}",
        y_plane.size() + u_plane.size() + v_plane.size(),
        non_zero_y_coeffs.size_in_bytes() + non_zero_u_coeffs.size_in_bytes() + non_zero_v_coeffs.size_in_bytes() + non_zero_y2_coeffs.size_in_bytes(),
        (macro_block_row_a.size() + macro_block_row_b.size()) * sizeof(MacroBlock));

    return {};
}

ErrorOr<NonnullRefPtr<Gfx::Bitmap>> VP8Decoder::try_decode_image(Gfx::BitmapFormat format)
{
    auto& context = *m_context;

    dbgln_if(WEBP_DEBUG, "Decoding bitstream:");
    dbgln_if(WEBP_DEBUG, "first_partition[{}] {}...", context.first_partition.size(), context.first_partition.slice(0, min(20, context.first_partition.size())));
    dbgln_if(WEBP_DEBUG, "image_data[{}] {}...", context.image_data.size(), context.image_data.slice(0, min(20, context.image_data.size())));
    auto stream = BitStream(context.first_partition);

    // 9.2. Color Space and Pixel Type (Key Frames Only) https://www.rfc-editor.org/rfc/rfc6386.html#section-9.2
    // The color space type bit is encoded as follows:
    // - 0: YUV color space similar to the YCrCb color space defined in ITU-R BT.601
    // - 1: Reserved for future use
    if (stream.literal<1>() != 0)
        return Error::from_string_literal("Unexpected color space");

    // The pixel value clamping type bit is encoded as follows:
    // - 0: Decoders are required to clamp the reconstructed pixel values to between 0 and 255 (inclusive).
    // - 1: Reconstructed pixel values are guaranteed to be between 0 and 255; no clamping is necessary.
    // NOTE: We always clamp, ignore the flag
    (void)stream.literal<1>();

    SegmentHeader segment_header;
    {
        auto track = stream.track("SegmentHeader"sv);
        read_segment_header(stream, segment_header);
    }

    LoopFilterHeader loop_filter_header;
    {
        auto track = stream.track("LoopFilterHeader"sv);
        read_loop_filter_header(stream, loop_filter_header);
    }

    PartitionHeader partition_header;
    {
        auto track = stream.track("PartitionHeader"sv);
        TRY(read_partition_header(stream, partition_header));
    }

    Array<QuantizationMatrix, BLOCK_CONTEXTS> quantization_matrixes = {};
    {
        auto track = stream.track("QuantizationMatrix"sv);
        read_quantization_matrix(stream, segment_header, quantization_matrixes);
    }

    for (auto& m : quantization_matrixes) {
        dbgln_if(WEBP_DEBUG && DEBUG_RESIDUALS, "y:{},{}, y2:{},{}, uv:{},{}", m.y.dc, m.y.ac, m.y2.dc, m.y2.ac, m.uv.dc, m.uv.ac);
    }

    // Ignore entropy update flag
    (void)stream.literal<1>();

    EntropyHeader entropy_header;
    memcpy(entropy_header.coeff_probs, default_coeff_probs, sizeof(default_coeff_probs));
    {
        auto track = stream.track("EntropyHeader"sv);
        read_entropy_header(stream, entropy_header);
    }

    size_t macro_blocks_x = (context.image_size.width() + 15) >> 4;
    size_t macro_blocks_y = (context.image_size.height() + 15) >> 4;
    dbgln_if(WEBP_DEBUG, "{} x {} macro blocks", macro_blocks_x, macro_blocks_y);

    auto image_data_stream = BitStream(context.image_data);

    auto y_plane = TRY(FixedArray<u8>::try_create(macro_blocks_x * macro_blocks_y * 16 * 16));
    auto u_plane = TRY(FixedArray<u8>::try_create(macro_blocks_x * macro_blocks_y * 8 * 8));
    auto v_plane = TRY(FixedArray<u8>::try_create(macro_blocks_x * macro_blocks_y * 8 * 8));

    TRY(try_decode_image_data(macro_blocks_x, macro_blocks_y, stream, image_data_stream, entropy_header, segment_header, quantization_matrixes, y_plane.span(), u_plane.span(), v_plane.span()));

    dbgln_if(WEBP_DEBUG, "remaining: first_partition:{}, image_data:{}", stream.bits_remaining(), image_data_stream.bits_remaining());

    auto bitmap = TRY(Gfx::Bitmap::try_create(format, context.image_size));
    copy_to_bitmap(bitmap, y_plane.span(), u_plane.span(), v_plane.span(), macro_blocks_x * 16);
    return bitmap;
}

}
