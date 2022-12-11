/*
 * Copyright (c) 2022, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Error.h>
#include <AK/Types.h>

namespace WebP {

ALWAYS_INLINE ErrorOr<u32> read_u24(ReadonlyBytes const& buffer, size_t offset)
{
    if (buffer.size() < offset + 3)
        return Error::from_string_literal("More data expected");
    return buffer[offset] | buffer[offset + 1] << 8 | buffer[offset + 2] << 16;
}

ALWAYS_INLINE ErrorOr<u32> read_u32(ReadonlyBytes const& buffer, size_t offset)
{
    if (buffer.size() < offset + 4)
        return Error::from_string_literal("More data expected");
    return buffer[offset] | buffer[offset + 1] << 8 | buffer[offset + 2] << 16 | buffer[offset + 3] << 24;
}

}

using WebP::read_u24;
using WebP::read_u32;
