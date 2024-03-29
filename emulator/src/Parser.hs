{-# LANGUAGE TemplateHaskell, FlexibleInstances, LambdaCase, OverloadedStrings #-}
module Parser (is16bit,parseInstruction, Command(..), Parser, parse, loadCommands, parseInstructions, OpCodes(..)) where

import Control.Applicative
import Data.Char
import Data.Int
import System.IO.Unsafe
import Control.Monad.State.Lazy
import Control.Lens
import System.Directory
import Debug.Trace
import Data.ByteString as BS
import GHC.Enum
import Data.Word
import Text.Printf
import Data.Bits
import Data.Text.Internal.Unsafe.Char
import GHC.Base

data Error = Error deriving (Show)


data OpCodes = ADC | AND | ASL | BBR | BBS | BBC | BCS | BEQ | BIT | BMI | BME | BNE | BPL | BRA |
               BRK | BVC | BVS | CLC | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR |
               INC | INX | INY | JMP | JSR | LDA | LDX | LDY | LSR | NOP | ORA | PHA | PHP | PHX |
               PHY | PLA | PLP | PLX | PLY | RMB | ROL | ROR | RTI | RTS | SBC | SEC | SED | SEI |
               SMB | STA | STP | STX | STY | STZ | TAX | TAY | TRB | TSB | TSX | TXS | TYA | WAI |
               LDAa deriving (Show)

data Command = Command { instruction :: OpCodes, address :: Int} deriving (Show)

$(makeLenses ''Command)

newtype Parser a i = Parser {
    parse :: [a] -> Either Error (i, [a])
}

$(makeLenses ''Parser)

instance Functor (Parser a) where
    fmap f (Parser a) = Parser $ \input -> 
        case a input of
            Left err -> Left err
            Right (x, rest) -> Right (f x, rest)

instance Applicative (Parser a) where
    pure a = Parser $ \input -> Right (a, input)
    Parser a <*> Parser b = Parser $ \input ->
        case a input of
            Left err -> Left err
            Right (x, rest) -> 
                case b rest of
                    Left err -> Left err
                    Right (y, rest') -> Right (x y, rest')

instance Alternative (Parser a) where
    empty = Parser $ \input -> Left Error
    Parser a <|> Parser b = Parser $ \input ->
        case a input of
            Left err -> 
                case b input of
                    Left err -> Left Error
                    Right (x,xs) -> Right (x,xs)
            Right (x,xs) -> Right (x,xs)

instance Monad (Parser i) where
  return = pure
  Parser p >>= k = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) ->
        let
          Parser p' = k output
        in
        p' rest

satisfy :: (s -> Bool) -> Parser s s
satisfy f = Parser $ \case
        [] -> Left Error
        x:rest
            | f x -> Right (x, rest)
            | otherwise -> Left Error

-- *> -- (*>) readChar 'l' readChar 'l'
readChar :: (Eq s) => s -> Parser s s 
readChar x = Parser f 
    where
        f (y:ys)
            | y == x    = Right (x,ys)
            | otherwise = Left Error
        f [] = Left Error
-- | Parser for null json

-- How does this work?
readString :: [Char] -> Parser Char [Char]
readString str = Parser $ \input ->
    case parse (traverse readChar str) input of
        Left err -> Left err
        Right (x, rest) -> Right (x, rest)

-- Define LAnguage Things
parseInstruction :: Parser Char Command
parseInstruction = do
    _ <- many $ parseSpace
    instruction <- loadX <|> loadY <|> loadTxs <|> loadLda <|> loadLdaAbsolute <|> loadSta <|> jumpSubRoutine <|> returnFromSubRoutine <|> inx <|> nop
    _ <- many $ parseSpace
    if (is16bit instruction) then do
            addressHigh <- many $ getAddress8
            _ <- many $ parseSpace
            addressLow <- many $ getAddress8
            traceShow (addressLow ++ addressHigh) $ pure ()
            return $ Command { instruction=instruction, address= read (addressLow ++ addressHigh) }
        else do
            address <- many $ getAddress8
            return $ Command { instruction=instruction, address= hexToDecimal address }

parseInstructions :: Parser Char [Command]
parseInstructions = do
  l <- many $ parseInstruction
  return l

hexToDecimal :: String -> Int
hexToDecimal = sum . Prelude.zipWith (*) (iterate (*16) 1) . Prelude.reverse . Prelude.map digitToInt . Prelude.map toUpper

getAddress8 :: Parser Char Char
getAddress8 = satisfy isAlpha <|> satisfy isDigit

-- Program Start
parseSpace :: Parser Char [Char]
parseSpace = readString " "

loadX :: Parser Char OpCodes
loadX = LDX <$ readString "A2"

loadY :: Parser Char OpCodes
loadY = LDY <$ readString "A0"

loadTxs :: Parser Char OpCodes
loadTxs = TXS <$ readString "9A"

loadLda :: Parser Char OpCodes
loadLda = LDA <$ readString "A9"

loadLdaAbsolute :: Parser Char OpCodes
loadLdaAbsolute = LDAa <$ readString "AD"

loadSta :: Parser Char OpCodes
loadSta = STA <$ readString "8D"

jumpSubRoutine :: Parser Char OpCodes
jumpSubRoutine = JSR <$ readString "20"

returnFromSubRoutine :: Parser Char OpCodes
returnFromSubRoutine = RTS <$ readString "60"

inx :: Parser Char OpCodes
inx = INX <$ readString "E8"

nop :: Parser Char OpCodes
nop = NOP <$ readString "EA"

is16bit :: OpCodes -> Bool
is16bit STA = True
is16bit JSR = True
is16bit LDAa = True
is16bit _   = False

loadCommands :: IO String
loadCommands = do
    directory <- getCurrentDirectory
    contents <- (BS.readFile (directory ++ "/src/examples/instructions2.bin"))
    return $ Prelude.concatMap ((printf "%02x\n")) $ unpack contents
    --startToken putStrLn "someFunc"

