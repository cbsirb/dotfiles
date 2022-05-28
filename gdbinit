set history save
set confirm off
set verbose off

set print pretty on
set print array off
set print array-indexes on
set print object on
set print static-members on
set print vtbl on
set print demangle on
set demangle-style gnu-v3

set python print-stack full
set disassembly-flavor intel

add-auto-load-safe-path /usr/lib/go/src/runtime/runtime-gdb.py

set auto-load python-scripts
set auto-load gdb-scripts
