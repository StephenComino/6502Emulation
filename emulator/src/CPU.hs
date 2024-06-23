{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}
module CPU
    ( 
      Processor,
      run
    ) where

import Data.Int
import Data.Bits
import System.IO.Unsafe
import Control.Monad.State (State, execState, get, evalState)
import Parser (is16bit, parseInstruction, Command(..), Parser, parse, loadCommands, parseInstructions, OpCodes(..))
import Rom (Rom(..))
import Ram (Ram(..))
import Control.Monad.State.Lazy
import Control.Lens
import Control.Concurrent
import Debug.Trace
import Data.Char
import Data.List
import GHC.Word
import Data.Map
import Data.Bits ((.|.))
import Control.Exception
import GHC.IO (catchException)
import Control.Monad.Accum (MonadAccum(accum))

data Pins = Pins {
  _a0 :: Word8,
  _a1 :: Word8,
  _a2 :: Word8,
  _a3 :: Word8,
  _a4 :: Word8,
  _a5 :: Word8,
  _a6 :: Word8,
  _a7 :: Word8,
  _a8 :: Word8,
  _a9 :: Word8,
  _a10 :: Word8,
  _a11 :: Word8,
  _a12 :: Word8,
  _a13 :: Word8,
  _a14 :: Word8,
  _a15 :: Word8,
  _d0 :: Word8,
  _d1 :: Word8,
  _d2 :: Word8,
  _d3 :: Word8,
  _d4 :: Word8,
  _d5 :: Word8,
  _d6 :: Word8,
  _d7 :: Word8
} deriving (Show)

data Processor = Processor { 
  _ir :: Int16, 
  _tcu :: Int, -- Timing Control Unit
  _alu :: Int, -- Arithmetic and Logic Unit
  _acc :: Word8, -- Accumulator
  _idx :: Word8, -- Index Register X
  _idy :: Word8, -- Index Register Y
  _pr :: Word8, -- Processor Status Register
  _pc :: Word16, -- Program Counter
  _sp :: Word8, -- Stack Pointer from 0100 to 01FF
  _carryFlag :: Bool,
  _zeroFlag :: Bool,
  _irqbFlag :: Bool,
  _decimalModeFlag :: Bool,
  _brkFlag :: Bool, -- Sets IRQB to 0
  _overflowFlag :: Bool,
  _negativeFlag :: Bool,
  _cycles :: Int,
  _pins :: Pins,
  _rom :: Map Word16 Command,
  _ram :: Map Word16 Word8,
  _stack :: Map Word16 Word16
} deriving (Show)

instance Show (State Rom Rom) where
  show a = show (execState a (Rom $ fromList []))


$(makeLenses ''Pins)
$(makeLenses ''Processor)
$(makeLenses ''Rom)
$(makeLenses ''Ram)
-- Data is loaded into the ROM.
-- The PC resets and points to ROM first instruction
-- 

-- RESET (RESB)
-- The program 
-- counter is loaded with the reset vector from locations FFFC (low byte) and FFFD (high byte).
-- 0 -> 0x3FFF ram
-- 0x6000 -> 0x8000 VIA
-- 8000 -> 0xFFFF rom
reset :: State Processor Processor
reset = do
  -- Lasting 7 clock cyces
  -- Reads fffc and fffd from the databus
  -- v <- readCpuMemory16 0xFFFC
  p <- get
  pc .= 8000
  return p

initial :: Processor
initial = Processor 0 0 0 0 0 0 0 0 0 0 0 (Pins 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (fromList []) (fromList []) (fromList [])

instance Show (State Processor Processor) where
  show a = show $ _rom (evalState a initial)

-- execState setup initial
-- Increment the State... Stack pointer by one
setup :: State Processor Processor
setup = do
    p <- get
    res <- use sp
    sp .= res + 1
    return p

--getNextCommand :: Command
--getNextCommand = getParsedValue $ parse parseInstruction

performInstruction :: Command -> State Processor Processor
performInstruction (Command instruction addr ) = case instruction of
                      ADC -> adc ((fromIntegral $ addr)::Word8)
                      ORA -> ora ((fromIntegral $ addr)::Word8)
                      EOR -> eor ((fromIntegral $ addr)::Word8)
                      LDX -> ldx ((fromIntegral $ addr)::Word8)
                      LDY -> ldy ((fromIntegral $ addr)::Word8)
                      TXS -> txs
                      LDA -> lda ((fromIntegral $ addr)::Word8)
                      STA -> sta ((fromIntegral $ addr)::Word16)
                      JSR -> jsr ((fromIntegral $ addr)::Word16)
                      INX -> inx
                      RTS -> rts
                      NOP -> nop
                      _ -> other

other :: State Processor Processor
other = do
          p <- get
          return p

------- [[[[[ Instructions ]]]]] -------
-- Implement only # addressing first
-- LDX A2
-- execState (performInstruction (Command { instruction=LDX, address=256 } )) initial
lda :: Word8 -> State Processor Processor
lda addr = do
        p <- get
        counter <- use pc
        res <- use acc
        acc .= addr
        --pc .= counter + 1
        return p


ldx :: Word8 -> State Processor Processor
ldx addr = do
        p <- get
        res <- use idx
        idx .= addr
        return p

ldy :: Word8 -> State Processor Processor
ldy addr = do
        p <- get
        res <- use idy
        idy .= addr
        return p

txs :: State Processor Processor
txs = do
        p <- get
        res <- use idx
        sp .= res
        return p

-- Store Word16 at Address
sta :: Word16 -> State Processor Processor
sta addr = do
        p <- get
        counter <- use pc
        res <- use acc
        r <- use ram
        ram .= Data.Map.insert addr res r
        --pc .= counter + 1
        --traceShow addr $ pure ()
        return p

-- Pushes address - 1 of the return point onto the tack and then sets the program counter to the target memory address
jsr :: Word16 -> State Processor Processor
jsr addr = do
        p <- get
        counter <- use pc
        s <- use stack
        stack .= Data.Map.insert 0 (counter - 2) s
        --traceShow addr $ pure ()
        pc .= (addr - 1)
        return p

-- Gets program counter from Stack
rts :: State Processor Processor
rts = do
        p <- get
        counter <- use pc
        s <- use stack
        let newPc = s ! (0 :: Word16)
        pc .= newPc - 1
        --traceShow stack $ pure ()
        return p

-- Add memory to accumulator with carry
adc :: Word8 -> State Processor Processor
adc value = do
        p <- get
        cary <- use carryFlag
        acc .= value + cary
        return p

-- Does this instruction need Word16?
ora :: Word8 -> State Processor Processor
ora value = do
        p <- get
        ac <- use acc
        acc .= (.|.) ac value
        return p

-- Does this instruction need Word16?
eor :: Word8 -> State Processor Processor
eor value = do
        p <- get
        ac <- use acc
        acc .= xor ac value
        return p

-- Compare memory with accumulator
cmp :: Word8 -> State Processor Processor
cmp value = do
        p <- get
        acc <- use acc

        return p
inx :: State Processor Processor
inx = do
        p <- get
        x <- use idx
        idx .= x + 1
        return p

nop :: State Processor Processor
nop = do
        p <- get
        counter <- use pc
        --pc .= counter + 1
        return p

getParsedValue :: [Either a ([Command], [Char])] -> [Command]
getParsedValue (a:as)= case a of
                      Left err -> [Command { instruction=NOP,address=0 }]
                      Right (command, rest) -> command ++ getParsedValue as
getParsedValue ([]) = [Command { instruction=NOP,address=0 }]

loadRom2 :: Word16 -> [Command] -> State Processor Processor
loadRom2 addr (x:xs) = do
  r <- get 
  --Load each instruction into ROM
  rd <- use rom
  
  rom .= Data.Map.insert addr x rd 
    --_ <- use romData
  --romData .= info
  let addressPlus = sortCommands x
  loadRom2 (addr + addressPlus) xs
loadRom2 addr [] = do
  r <- get
  return r

incrementPCToSortCommands :: State Processor Processor
incrementPCToSortCommands = do
        p <- get
        r <- use rom
        reset
        counter <- use pc
        let commands = unsafePerformIO loadCommands
        
        let indiv = intercalate " " $ lines $ Prelude.map toUpper commands
        let info = getParsedValue $ (fmap (parse parseInstructions) [indiv])

        loadRom2 counter info--execState loadRom (Rom $ fromList [])
        --traceShow r $ pure ()
        reset
        return p

-- we want to start at 0x8000
sortCommands :: Command -> Word16
sortCommands command = case is16bit (instruction command) of
        True -> 3
        False -> 2

performActions :: State Processor Processor
performActions = do
        p <- get
        counter <- use pc
        rd <- use rom
        --traceShow rd $ pure ()
        let command =  rd ! counter
        pc .= counter + (sortCommands command)
        --traceShow command $ pure ()
        res <- (performInstruction command)
        return res

-- 1. LDA STA
run :: IO ()
run = do
    --print $ indiv
    let result = execState incrementPCToSortCommands initial
    --print result
    procLoop result where
        procLoop r = do
                let m = execState performActions r
                print $ m
                threadDelay 2000000
                procLoop m
    --run
