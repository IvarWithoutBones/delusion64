set disassembly-flavor intel

set print asm-demangle on
set print pretty on
set print array on

set history save on
set max-value-size unlimited
set pagination off
set confirm off
set disassemble-next-line on
set step-mode on
set tui active-border-mode normal
set tui compact-source on

tui new-layout asm {-horizontal asm 1 regs 1} 2 status 0 cmd 1

define lempty
    tui disable
end

define lasm
    layout asm
end

set remotetimeout 9999
echo connecting to localhost:9001\n
target remote localhost:9001
