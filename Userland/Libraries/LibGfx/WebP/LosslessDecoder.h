/*
 * Copyright (c) 2022, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/OwnPtr.h>
#include <LibGfx/Forward.h>

namespace WebP::Lossless {

struct LosslessContext;

class LosslessDecoder {
public:
    LosslessDecoder(ReadonlyBytes data);
    ~LosslessDecoder();
    ErrorOr<Gfx::IntSize> try_decode_frame_header();
    ErrorOr<NonnullRefPtr<Gfx::Bitmap>> try_decode_image();

private:
    OwnPtr<LosslessContext> m_context;
};

}
