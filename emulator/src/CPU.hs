module CPU
    ( someFunc
    ) where

-- registers
-- IR register -- Instruction is loaded into this register from the data bus and latched during the 
-- OpCode Fetch Cycle
-- OpCode is then decoded, along with timing and interrup signals, to generate various control signals
someFunc :: IO ()
someFunc = undefined

-- capable of addressing 65,536 bytes of memory

data OpCodes = ADC | AND | ASL | BBR | BBS | BBC | BCS | BEQ | BIT | BMI | BME | BNE | BPL | BRA |
               BRK | BVC | BVS | CLC | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR |
               INC | INX | INY | JMP | JSR | LDA | LDX | LDY | LSR | NOP | ORA | PHA | PHP | PHX |
               PHY | PLA | PLP | PLX | PLY | RMB | ROL | ROR | RTI | RTS | SBC | SEC | SED | SEI |
               SMB | STA | STA | STP | STX | STY | STZ | TAX | TAY | TRB | TSB | TSX | TXS | TYA |
               WAI


-- 1. LDA STA

Registers 