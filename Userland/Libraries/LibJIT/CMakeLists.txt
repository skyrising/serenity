set(SOURCES
    Assembler.cpp
        JITDumpFile.cpp
        CodeBuffer.cpp
)

serenity_lib(LibJIT jit)
target_link_libraries(LibJIT PRIVATE LibCore)
