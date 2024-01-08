{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, RecordWildCards #-}
module Rom
    ( 
      Rom(..)
    ) where

import Parser (parseInstruction, Command(..), Parser, parse, loadCommands, OpCodes(..))
import Control.Lens
import Control.Concurrent

data Rom = Rom {
      _romData :: [Command]
} deriving (Show)


-- 0x8000 -> 0xFFFF
-- 32767 size List
