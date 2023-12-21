{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module CPU
    ( readInstructions
    ) where

import Data.Int
import Control.Monad.State (State, execState, get, evalState)
import Parser (parseInstruction, Command(..), Parser, parse, loadCommands, OpCodes(..))
import Control.Monad.State.Lazy
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

-- Implement only # addressing first
-- LDX A2
readInstructions :: State Processor Processor
readInstructions = do
              p <- get
              let m = getParsedValue $ parse parseInstruction loadCommands
              res <- use sp
              sp .= address m 
              return p

getParsedValue :: Either a (Command, [Char]) -> Command
getParsedValue a = case a of
                      Left err -> Command { address=0}
                      Right (com, rest) -> com
-- 1. LDA STA
