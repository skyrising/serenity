/*
 * Copyright (c) 2023, Simon Wanner <simon@skyrising.xyz>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibCore/File.h>

#pragma once

namespace JIT {

class JITDumpFile {
public:
    struct DebugEntry {
        FlatPtr code_address;
        u32 line;
        u32 column;
        StringView name;
    };

    explicit JITDumpFile(NonnullOwnPtr<Core::File>);
    static ErrorOr<OwnPtr<JITDumpFile>> open(StringView filename);

    ErrorOr<void> emit_code_load(ReadonlyBytes code, StringView function_name, Vector<DebugEntry> const&);

private:
    ErrorOr<void> emit_file_header();

    NonnullOwnPtr<Core::File> m_file;
    u32 m_code_id { 0 };
};

}
