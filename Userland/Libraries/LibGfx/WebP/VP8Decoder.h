/*
 * Copyright (c) 2022, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/OwnPtr.h>
#include <LibGfx/Forward.h>
#include <LibGfx/WebP/VP8Data.h>

namespace VP8 {

class BitStream;
struct VP8Context;
struct PartitionHeader;

class VP8Decoder {
public:
    VP8Decoder(ReadonlyBytes data);
    ~VP8Decoder();
    ErrorOr<Gfx::IntSize> try_decode_frame_header();
    ErrorOr<NonnullRefPtr<Gfx::Bitmap>> try_decode_image(Gfx::BitmapFormat format);

private:
    ErrorOr<void> read_partition_header(BitStream& stream, PartitionHeader& header);

    OwnPtr<VP8Context> m_context;
};

}
