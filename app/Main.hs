{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Lib
import Text.Parsec (parse, digit, anyChar, many, char, manyTill, optional, ParseError, manyTill)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, space, upper, lower)
import System.Environment (getArgs)

data Foo = Foo String Char
  deriving Show

data DatiApp = DatiApp
  {
    occ :: String
  , scala :: Int
  , piano :: Int
  }
  deriving (Show, Eq)

--datiApp :: Parser DatiApp
--datiApp = 

word :: Parser String
word = pure (:) <*> upper <*> many lower <* (optional space)

f :: Parser Char
f = char 'f' :: Parser Char

n :: Parser String
n = many f

l :: Parser Foo
l = do
  name <- n
  d <- digit
  return (Foo name d)

parseContents :: String -> String -> Either ParseError Foo
parseContents sourceName contents = parse l sourceName contents

data DatiAppartamento = DatiAppartamento
  {
    occupante       :: String
  , scala           :: Int
  , piano           :: Int
  , riscaldIdronico :: Riscaldamento
  , acquaCaldaSanit :: Riscaldamento
  , acquaPot        :: Acqua
  , acquaDuale      :: Acqua
  , totaleAlloggio  :: Float
  }
  deriving (Eq, Show)

data Riscaldamento = Riscaldamento
  {
    unitaRip   :: Float
  , consVolont :: Float 
  , millServ   :: Float
  , consInvol  :: Float
  , consObbl   :: Float
  , consTot    :: Float
  , costUnitEnerTermUtile :: Float
  , spesaConsVol :: Float
  , spesaConsInvol :: Float
  , spesaConsObblig :: Float
  , spesaEnerg :: Float
  , spesaGest :: Float
  , quotaCons :: Float
  , quotaFissa :: Float
  , spesaLocali :: Float
  , spesaTotServ :: Float
  }
  deriving (Eq, Show)

data Acqua = Acqua
  {
    millesimiServ :: Float
  , quotaCons :: Float
  , quotaFissa :: Float
  , spesaLocali :: Float
  , spesaTotServ :: Float
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  let file = args !! 0
  contents <- readFile file
  let p = parseContents file contents
  putStrLn (show p)
