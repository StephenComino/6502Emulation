{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module CPU
    ( 
      Processor
    ) where

import Data.Int
import Control.Monad.State (State, execState, get, evalState)
import Parser (parseInstruction, Command(..), Parser, parse, loadCommands, OpCodes(..))
import Control.Monad.State.Lazy
import Control.Lens
import GHC.Word

data Processor = Processor { 
  _ir :: Int16, 
  _tcu :: Int, -- Timing Control Unit
  _alu :: Int, -- Arithmetic and Logic Unit
  _acc :: Int, -- Accumulator
  _idx :: Word8, -- Index Register X
  _idy :: Word8, -- Index Register Y
  _pr :: Int, -- Processor Status Register
  _pc :: Int, -- Process Counter
  _sp :: Int -- Stack Pointer
} deriving (Show)

$(makeLenses ''Processor)

--- How can I represent Memory and Program jump instructions?
-- Parse function Names... Keep Reference and jump to them

initial :: Processor
initial = Processor 0 0 0 0 0 0 0 0 0

-- execState setup initial
-- Increment the State... Stack pointer by one
setup :: State Processor Processor
setup = do
    p <- get
    res <- use sp
    sp .= res + 1
    return p

getNextCommand :: Command
getNextCommand = getParsedValue $ parse parseInstruction (loadCommands !! 0)

performInstruction :: Command -> State Processor Processor
performInstruction (Command instruction addr ) = case instruction of
                      LDX -> ldx ((fromIntegral $ addr)::Word8)
                      LDY -> ldy ((fromIntegral $ addr)::Word8)
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

getParsedValue :: Either a (Command, [Char]) -> Command
getParsedValue a = case a of
                      Left err -> Command { address=0 }
                      Right (command, rest) -> command
-- 1. LDA STA
