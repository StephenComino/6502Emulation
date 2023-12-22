{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, RecordWildCards #-}
module CPU
    ( 
      Processor
    ) where

import Data.Int
import Data.Bits
import Control.Monad.State (State, execState, get, evalState)
import Parser (parseInstruction, Command(..), Parser, parse, loadCommands, OpCodes(..))
import Control.Monad.State.Lazy
import Control.Lens
import GHC.Word

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
  _a15 :: Word8
} deriving (Show)

data Processor = Processor { 
  _ir :: Int16, 
  _tcu :: Int, -- Timing Control Unit
  _alu :: Int, -- Arithmetic and Logic Unit
  _acc :: Word8, -- Accumulator
  _idx :: Word8, -- Index Register X
  _idy :: Word8, -- Index Register Y
  _pr :: Word8, -- Processor Status Register
  _pc :: Word16, -- Process Counter
  _sp :: Word8, -- Stack Pointer
  _pins :: Pins
} deriving (Show)

$(makeLenses ''Pins)
$(makeLenses ''Processor)

--- How can I represent Memory and Program jump instructions?
-- Parse function Names... Keep Reference and jump to them

initial :: Processor
initial = Processor 0 0 0 0 0 0 0 0 0 $ Pins 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

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
                      _ -> other

other :: State Processor Processor
other = do
          p <- get
          return p

------- [[[[[ Instructions ]]]]] -------
-- Implement only # addressing first
-- LDX A2
-- execState (performInstruction (Command { instruction=LDX, address=256 } )) initial
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

getParsedValue :: Either a (Command, [Char]) -> Command
getParsedValue a = case a of
                      Left err -> Command { instruction=NOP,address=0 }
                      Right (command, rest) -> command
-- 1. LDA STA
run :: IO ()
run = do
    let init = initial
    commands <- loadCommands
    let indiv = lines commands
    print indiv
    let result =  fmap getParsedValue $ (fmap (parse parseInstruction) indiv)
    let state = performActions result initial where
                  performActions (x:xs) a = performActions xs (execState (performInstruction x) a)
                  performActions (x:[]) a = performActions [] (execState (performInstruction x) a)
                  performActions [] a = a
    print $ state