{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module CPU
    ( someFunc
    ) where
import Data.Int
import Control.Monad.State (State, execState, get)
import Control.Lens

data Processor = Processor { 
  _ir :: Int16, 
  _tcu :: Int, -- Timing Control Unit
  _alu :: Int, -- Arithmetic and Logic Unit
  _acc :: Int, -- Accumulator
  _idx :: Int, -- Index Register X
  _idy :: Int, -- Index Register Y
  _pr :: Int, -- Processor Status Register
  _pc :: Int, -- Process Counter
  _sp :: Int -- Stack Pointer
} deriving (Show)

$(makeLenses ''Processor)

type MyProcessor = State Processor Processor
instance Show (State Processor Processor) where
  show a   = 


initial :: Processor
initial = Processor 0 0 0 0 0 0 0 0 0

setup :: State Processor Processor
setup = do
    p <- get
    --p <- get
    --return p
    sp .= 1
    return p
-- registers
-- IR register -- Instruction is loaded into this register from the data bus and latched during the 
-- OpCode Fetch Cycle
-- OpCode is then decoded, along with timing and interrup signals, to generate various control signals
someFunc :: MyProcessor
someFunc = undefined


-- capable of addressing 65,536 bytes of memory

data OpCodes = ADC | AND | ASL | BBR | BBS | BBC | BCS | BEQ | BIT | BMI | BME | BNE | BPL | BRA |
               BRK | BVC | BVS | CLC | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR |
               INC | INX | INY | JMP | JSR | LDA | LDX | LDY | LSR | NOP | ORA | PHA | PHP | PHX |
               PHY | PLA | PLP | PLX | PLY | RMB | ROL | ROR | RTI | RTS | SBC | SEC | SED | SEI |
               SMB | STA | STP | STX | STY | STZ | TAX | TAY | TRB | TSB | TSX | TXS | TYA | WAI


-- 1. LDA STA
