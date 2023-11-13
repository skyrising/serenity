/*
 * Copyright (c) 2023, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/ByteBuffer.h>
#include <AK/Time.h>
#include <LibELF/ELFABI.h>
#include <LibJIT/JITDumpFile.h>
#include <sys/mman.h>
#include <unistd.h>

namespace JIT {

struct [[gnu::packed]] Header {
    u32 magic { MAGIC };
    u32 version { VERSION };
    u32 total_size;
    u32 elf_mach;
    u32 pad1;
    u32 pid;
    u64 timestamp;
    u64 flags;

    static constexpr u32 MAGIC = 0x4A695444;
    static constexpr u32 VERSION = 1;
};
static_assert(AssertSize<Header, 40>());

enum class RecordType : u32 {
    Load,
    Move,
    DebugInfo,
    Close,
    UnwindingInfo,
};

struct [[gnu::packed]] CodeLoadRecord {
    RecordType id { RecordType::Load };
    u32 total_size;
    u64 timestamp;
    u32 pid;
    u32 tid;
    u64 vma;
    u64 code_addr;
    u64 code_size;
    u64 code_index;
};
static_assert(AssertSize<CodeLoadRecord, 56>());

struct [[gnu::packed]] DebugInfoRecord {
    RecordType id { RecordType::DebugInfo };
    u32 total_size;
    u64 timestamp;
    u64 code_addr;
    u64 entry_count;

    struct [[gnu::packed]] Entry {
        u64 address;
        u32 line;
        u32 column;
    };
    static_assert(AssertSize<Entry, 16>());
};
static_assert(AssertSize<DebugInfoRecord, 32>());

JITDumpFile::JITDumpFile(NonnullOwnPtr<Core::File> file)
    : m_file(move(file))
{
}

ErrorOr<OwnPtr<JITDumpFile>> JITDumpFile::open(StringView filename)
{
    auto file = TRY(Core::File::open(filename, Core::File::OpenMode::ReadWrite));

    // Map one page of the file, so perf can detect it
    auto marker = mmap(nullptr, PAGE_SIZE, PROT_READ | PROT_EXEC, MAP_PRIVATE, file->fd(), 0);
    if (marker == MAP_FAILED)
        return Error::from_errno(errno);

    auto dump_file = make<JITDumpFile>(move(file));
    TRY(dump_file->emit_file_header());

    return dump_file;
}

constexpr static u32 get_elf_machine_type()
{
#if ARCH(X86_64)
    return EM_X86_64;
#elif ARCH(AARCH64)
    return EM_AARCH64;
#elif ARCH(RISCV64)
    return EM_RISCV;
#else
    TODO("Unknown architecture");
#endif
}

ErrorOr<void> JITDumpFile::emit_file_header()
{
    Header const header {
        .total_size = sizeof(Header),
        .elf_mach = get_elf_machine_type(),
        .pid = static_cast<u32>(getpid()),
        .timestamp = static_cast<u64>(MonotonicTime::now().nanoseconds()),
        .flags = 0,
    };
    TRY(m_file->write_until_depleted({ &header, sizeof(header) }));
    return {};
}

ErrorOr<void> JITDumpFile::emit_code_load(ReadonlyBytes code, StringView function_name, Vector<DebugEntry> const& debug_entries)
{

    auto code_load_size = sizeof(CodeLoadRecord) + function_name.length() + 1 + code.size();
    auto debug_info_size = debug_entries.is_empty() ? 0 : sizeof(DebugInfoRecord);
    for (auto entry : debug_entries)
        debug_info_size += sizeof(DebugInfoRecord::Entry) + entry.name.length() + 1;

    auto total_size = code_load_size + debug_info_size;

    ByteBuffer buffer;
    TRY(buffer.try_ensure_capacity(total_size));

    auto timestamp = static_cast<u64>(MonotonicTime::now().nanoseconds());

    if (!debug_entries.is_empty()) {
        DebugInfoRecord const debug_record = {
            .total_size = static_cast<u32>(debug_info_size),
            .timestamp = timestamp,
            .code_addr = bit_cast<u64>(code.data()),
            .entry_count = debug_entries.size(),
        };
        buffer.append(&debug_record, sizeof(debug_record));

        dbgln("-------------");
        for (auto entry : debug_entries) {
            DebugInfoRecord::Entry const debug_entry = {
                .address = entry.code_address + 0x40,
                .line = entry.line,
                .column = entry.column,
            };
            buffer.append(&debug_entry, sizeof(debug_entry));
            buffer.append(entry.name.bytes());
            buffer.append('\0');
            dbgln("{}:{}:{} {:p}", entry.name, entry.line, entry.column, entry.code_address);
        }
    }

    CodeLoadRecord const record = {
        .total_size = static_cast<u32>(code_load_size),
        .timestamp = timestamp,
        .pid = static_cast<u32>(getpid()),
        .tid = static_cast<u32>(gettid()),
        .vma = bit_cast<u64>(code.data()),
        .code_addr = bit_cast<u64>(code.data()),
        .code_size = code.size(),
        .code_index = m_code_id++,
    };
    buffer.append(&record, sizeof(record));

    buffer.append(function_name.bytes());
    buffer.append('\0');
    buffer.append(code);

    TRY(m_file->write_until_depleted(buffer.bytes()));
    return {};
}

}
