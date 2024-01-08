{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, RecordWildCards #-}
module Ram
    ( 
      Ram(..)
    ) where

-- 0 -> 0x3FFF ram
import Parser (parseInstruction, Command(..), Parser, parse, loadCommands, OpCodes(..))
import Control.Lens
import Control.Concurrent
import Data.Int
import Data.Bits
import Control.Monad.State.Lazy
import GHC.Word
import Data.Map

data Ram = Ram {
      _ramData :: Map Word16 Word8
} deriving (Show)

