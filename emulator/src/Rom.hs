{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, RecordWildCards #-}
module Rom
    ( 
      Rom(..)
    ) where

import Parser (parseInstruction, Command(..), Parser, parse, loadCommands, OpCodes(..))
import Control.Lens
import Control.Concurrent
import Data.Int
import Data.Bits
import Control.Monad.State.Lazy
import Data.Map
import Data.Word


data Rom = Rom {
      _romData :: Map Word16 Command
} deriving (Show)


-- 0x8000 -> 0xFFFF -- Word16
-- 32767 size List
--
