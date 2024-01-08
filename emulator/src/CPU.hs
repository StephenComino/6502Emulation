{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, RecordWildCards #-}
module CPU
    ( 
      Processor
    ) where

import Data.Int
import Data.Bits
import System.IO.Unsafe
import Control.Monad.State (State, execState, get, evalState)
import Parser (parseInstruction, Command(..), Parser, parse, loadCommands, OpCodes(..))
import Rom (Rom(..))
import Ram (Ram(..))
import Control.Monad.State.Lazy
import Control.Lens
import Control.Concurrent
import Debug.Trace
import GHC.Word
import Data.Map

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
  _sp :: Word8, -- Stack Pointer
  _cycles :: Int,
  _pins :: Pins,
  _rom :: State Rom Rom,
  _ram :: Map Word16 Word8
} deriving (Show)

instance Show (State Rom Rom) where
  show a = show (execState a (Rom []))


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
  pc .= 0x8000
  return p

initial :: Processor
initial = Processor 0 0 0 0 0 0 0 0 0 0 (Pins 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) loadRom (fromList [])

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
                      LDX -> ldx ((fromIntegral $ addr)::Word8)
                      LDY -> ldy ((fromIntegral $ addr)::Word8)
                      TXS -> txs
                      LDA -> lda ((fromIntegral $ addr)::Word8)
                      STA -> sta ((fromIntegral $ addr)::Word16)
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
        pc .= counter + 1
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
        ram .= insert addr res r
        traceShow addr $ pure ()
        return p

getParsedValue :: Either a (Command, [Char]) -> Command
getParsedValue a = case a of
                      Left err -> Command { instruction=NOP,address=0 }
                      Right (command, rest) -> command

loadRom :: State Rom Rom
loadRom = do
  r <- get
  let commands = unsafePerformIO loadCommands
  let indiv = lines commands
  let info = fmap getParsedValue $ (fmap (parse parseInstruction) indiv)
  _ <- use romData
  romData .= info
  return r

-- 1. LDA STA
run :: IO ()
run = do
    let resetProcess = execState reset initial
    commands <- loadCommands
    let indiv = lines commands
    let result = execState loadRom (Rom [])
    let state = performActions (_romData result) resetProcess where
                  performActions (x:xs) a = performActions xs (execState (performInstruction x) a)
                  performActions [] a = a
    print $ state
    threadDelay 2000000
    run
