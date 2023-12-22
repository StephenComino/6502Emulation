{-# LANGUAGE TemplateHaskell, FlexibleInstances, LambdaCase, OverloadedStrings #-}
module Parser (parseInstruction, Command(..), Parser, parse, loadCommands, OpCodes(..)) where

import Control.Applicative
import Data.Char
import Data.Int
import System.IO.Unsafe
import Control.Monad.State.Lazy
import Control.Lens
import System.Directory

data Error = Error deriving (Show)


data OpCodes = ADC | AND | ASL | BBR | BBS | BBC | BCS | BEQ | BIT | BMI | BME | BNE | BPL | BRA |
               BRK | BVC | BVS | CLC | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR |
               INC | INX | INY | JMP | JSR | LDA | LDX | LDY | LSR | NOP | ORA | PHA | PHP | PHX |
               PHY | PLA | PLP | PLX | PLY | RMB | ROL | ROR | RTI | RTS | SBC | SEC | SED | SEI |
               SMB | STA | STP | STX | STY | STZ | TAX | TAY | TRB | TSB | TSX | TXS | TYA | WAI deriving (Show)

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
    instruction <- loadX <|> loadY <|> loadTxs <|> loadLda
    _ <- many $ parseSpace
    address <- many $ getAddress
    return $ Command { instruction=instruction, address= hexToDecimal address }

hexToDecimal :: String -> Int
hexToDecimal = sum . zipWith (*) (iterate (*16) 1) . reverse . map digitToInt . map toUpper

getAddress :: Parser Char Char
getAddress = satisfy isAlpha <|> satisfy isDigit

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

loadCommands :: IO String
loadCommands = do
    directory <- getCurrentDirectory
    contents <- (readFile (directory ++ "\\src\\examples\\instructions.bin"))
    return $ contents
    --startToken putStrLn "someFunc"

