/*
 * Copyright (c) 2022, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibGfx/ImageDecoder.h>

namespace WebP {

struct WebPLoadingContext;

class WebPImageDecoderPlugin final : public Gfx::ImageDecoderPlugin {
public:
    WebPImageDecoderPlugin(u8 const*, size_t);
    virtual ~WebPImageDecoderPlugin() override;

    virtual Gfx::IntSize size() override;
    virtual bool sniff() override;
    virtual bool is_animated() override;
    virtual void set_volatile() override;
    virtual bool set_nonvolatile(bool& was_purged) override;
    virtual size_t loop_count() override;
    virtual size_t frame_count() override;
    virtual ErrorOr<Gfx::ImageFrameDescriptor> frame(size_t index) override;

private:
    OwnPtr<WebPLoadingContext> m_context;
};

}
