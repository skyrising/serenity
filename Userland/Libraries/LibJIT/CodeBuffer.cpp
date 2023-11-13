/*
 * Copyright (c) 2023, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Error.h>
#include <LibJIT/CodeBuffer.h>
#include <sys/mman.h>
#include <unistd.h>

CodeBuffer::CodeBuffer()
{
    m_page_size = sysconf(_SC_PAGESIZE);
    m_capacity = m_page_size;
    m_buffer = bit_cast<u8*>(mmap(nullptr, m_capacity, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0, 0));
    VERIFY(m_buffer != MAP_FAILED);
}

void CodeBuffer::grow()
{
    auto new_capacity = m_capacity * 2;
#ifdef MREMAP_MAYMOVE
    u8* new_buffer = bit_cast<u8*>(mremap(m_buffer, m_capacity, new_capacity, MREMAP_MAYMOVE));
    if (new_buffer == MAP_FAILED) {
        warnln("grow to {} failed {}", new_capacity, strerror(errno));
    }
    VERIFY(new_buffer != MAP_FAILED);
#else
    u8* new_buffer = bit_cast<u8*>(mremap(m_buffer, m_capacity, new_capacity, 0));
    if (new_buffer == MAP_FAILED) {
        new_buffer = bit_cast<u8*>(mmap(nullptr, new_capacity, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0, 0));
        VERIFY(new_buffer != MAP_FAILED);
        memcpy(new_buffer, m_buffer, m_size);
        munmap(m_buffer, m_capacity);
    }
#endif
    m_capacity = new_capacity;
    m_buffer = new_buffer;
}

ErrorOr<void> CodeBuffer::mark_executable()
{
    if (mprotect(m_buffer, m_size, PROT_READ | PROT_EXEC) < 0)
        return Error::from_errno(errno);
    return {};
}
