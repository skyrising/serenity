/*
 * Copyright (c) 2022, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

namespace WebP::Lossless {
static constexpr size_t MAX_COLOR_CACHE_BITS = 11;
static constexpr u32 COLOR_CACHE_MULTIPLIER = 0x1e35a7bd;

static constexpr u8 compress_offset(int x, int y)
{
    return (y << 4) | ((8 - x) & 0xf);
}
#define O compress_offset
static constexpr Array<u8, 120> DISTANCE_MAP = {
    O(+0, 1), O(+1, 0), O(+1, 1), O(-1, 1), O(+0, 2), O(+2, 0), O(+1, 2), O(-1, 2),
    O(+2, 1), O(-2, 1), O(+2, 2), O(-2, 2), O(+0, 3), O(+3, 0), O(+1, 3), O(-1, 3),
    O(+3, 1), O(-3, 1), O(+2, 3), O(-2, 3), O(+3, 2), O(-3, 2), O(+0, 4), O(+4, 0),
    O(+1, 4), O(-1, 4), O(+4, 1), O(-4, 1), O(+3, 3), O(-3, 3), O(+2, 4), O(-2, 4),
    O(+4, 2), O(-4, 2), O(+0, 5), O(+3, 4), O(-3, 4), O(+4, 3), O(-4, 3), O(+5, 0),
    O(+1, 5), O(-1, 5), O(+5, 1), O(-5, 1), O(+2, 5), O(-2, 5), O(+5, 2), O(-5, 2),
    O(+4, 4), O(-4, 4), O(+3, 5), O(-3, 5), O(+5, 3), O(-5, 3), O(+0, 6), O(+6, 0),
    O(+1, 6), O(-1, 6), O(+6, 1), O(-6, 1), O(+2, 6), O(-2, 6), O(+6, 2), O(-6, 2),
    O(+4, 5), O(-4, 5), O(+5, 4), O(-5, 4), O(+3, 6), O(-3, 6), O(+6, 3), O(-6, 3),
    O(+0, 7), O(+7, 0), O(+1, 7), O(-1, 7), O(+5, 5), O(-5, 5), O(+7, 1), O(-7, 1),
    O(+4, 6), O(-4, 6), O(+6, 4), O(-6, 4), O(+2, 7), O(-2, 7), O(+7, 2), O(-7, 2),
    O(+3, 7), O(-3, 7), O(+7, 3), O(-7, 3), O(+5, 6), O(-5, 6), O(+6, 5), O(-6, 5),
    O(+8, 0), O(+4, 7), O(-4, 7), O(+7, 4), O(-7, 4), O(+8, 1), O(+8, 2), O(+6, 6),
    O(-6, 6), O(+8, 3), O(+5, 7), O(-5, 7), O(+7, 5), O(-7, 5), O(+8, 4), O(+6, 7),
    O(-6, 7), O(+7, 6), O(-7, 6), O(+8, 5), O(+7, 7), O(-7, 7), O(+8, 6), O(+8, 7)
};
#undef O
static constexpr size_t CODE_LENGTH_CODES = 19;
static constexpr Array<size_t, CODE_LENGTH_CODES> CODE_LENGTH_CODE_ORDER = {
    17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
};
}
