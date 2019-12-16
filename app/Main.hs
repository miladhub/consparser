module Main where

import Lib
import Text.Parsec
import System.Environment

type Parser = Parsec String ()

data Foo = Foo String Char deriving Show

f = char 'f' :: Parser Char
n = many f
l = do
  name <- n
  d <- digit
  return (Foo name d)

parseContents :: String -> String -> Either ParseError Foo
parseContents = parse l

main :: IO ()
main = do
  args <- getArgs
  let file = args !! 0
  contents <- readFile file
  let p = parseContents file contents
  putStrLn (show p)
