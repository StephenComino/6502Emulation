# 6502Emulation
Compile:
vasm6502_oldstyle -Fbin -dotdir <blink.s>
hexdump -C a.out

Memory Layout:
-- 0 -> 0x3FFF ram
-- 0x6000 -> 0x8000 VIA
-- 8000 -> 0xFFFF rom

This is a simple Emulator for 65c02 processor. https://www.westerndesigncenter.com/wdc/documentation/w65c02s.pdf


http://www.6502.org/
