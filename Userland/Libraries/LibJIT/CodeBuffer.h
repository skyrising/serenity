/*
 * Copyright (c) 2023, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Format.h>
#include <AK/Noncopyable.h>
#include <AK/Span.h>
#include <AK/Types.h>

class CodeBuffer {
public:
    CodeBuffer();

    void reserve(size_t bytes)
    {
        VERIFY(bytes <= 8);
        if (m_size + bytes > m_capacity) [[unlikely]]
            grow();
    }

    void append(u8 byte)
    {
        reserve(1);
        m_buffer[m_size++] = byte;
    }

    void append16(u16 value)
    {
        reserve(2);
        m_buffer[m_size++] = (value >> 0) & 0xff;
        m_buffer[m_size++] = (value >> 8) & 0xff;
    }

    void append32(u32 value)
    {
        reserve(4);
        m_buffer[m_size++] = (value >> 0) & 0xff;
        m_buffer[m_size++] = (value >> 8) & 0xff;
        m_buffer[m_size++] = (value >> 16) & 0xff;
        m_buffer[m_size++] = (value >> 24) & 0xff;
    }

    void append64(u64 value)
    {
        reserve(8);
        m_buffer[m_size++] = (value >> 0) & 0xff;
        m_buffer[m_size++] = (value >> 8) & 0xff;
        m_buffer[m_size++] = (value >> 16) & 0xff;
        m_buffer[m_size++] = (value >> 24) & 0xff;
        m_buffer[m_size++] = (value >> 32) & 0xff;
        m_buffer[m_size++] = (value >> 40) & 0xff;
        m_buffer[m_size++] = (value >> 48) & 0xff;
        m_buffer[m_size++] = (value >> 56) & 0xff;
    }

    u8& operator[](size_t index)
    {
        VERIFY(index < m_size);
        return m_buffer[index];
    }

    u8 operator[](size_t index) const
    {
        VERIFY(index < m_size);
        return m_buffer[index];
    }

    ReadonlyBytes bytes() const { return { m_buffer, m_size }; }
    Bytes bytes() { return { m_buffer, m_size }; }

    void* data() { return m_buffer; }
    size_t size() const { return m_size; }

    ErrorOr<void> mark_executable();

private:
    void grow();

    u8* m_buffer;
    size_t m_page_size;
    size_t m_capacity;
    size_t m_size { 0 };
};
