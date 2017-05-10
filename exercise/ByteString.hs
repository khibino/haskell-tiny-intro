{-# LANGUAGE OverloadedStrings #-}

module ByteString where

import Data.Char (digitToInt)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import MonoidError (Parser', errorP)
import Calc (Expr (..), eval)


type Parser = Parser' ByteString

runParser :: Parser a -> ByteString -> Except (Last String) (a, ByteString)
runParser = runStateT

token :: Parser Char
token =  undefined

-- runParser token "abc"
-- runParser token ""

-- hint. fromIntegral
chunk :: Int -> Parser ByteString
chunk n = undefined

-- runParser (chunk 4) "abc"
-- runParser (chunk 4) "abcd"

eof :: Parser ()
eof = undefined

-- runParser eof "abc"
-- runParser eof ""

satisfy :: (Char -> Bool) -> String -> Parser Char
satisfy p em = undefined

-- 以下の文法の Expr型の結果を持つ parser を実装してください
-- Implement following syntax parser which result is Expr type.
-- 演算子は右結合でよい
-- right associative operator implementation may be more simple.

--  expr :=
--       | expr + expr
--       | expr * expr
--       | ( expr )
--       | <decimal number>

char :: Char -> Parser Char
char c = undefined

readNum :: String -> Expr
readNum = undefined

-- decimal number parser
numExpr :: Parser Expr
numExpr = undefined

-- unit term parser
unitExpr :: Parser Expr
unitExpr = undefined

-- multiply formula parser
multExpr :: Parser Expr
multExpr = undefined

-- plus formula parser
plusExpr :: Parser Expr
plusExpr = undefined

-- top-level parser
expr :: Parser Expr
expr = undefined

-- runParser expr "1"
-- runParser expr "(1)"
-- runParser expr "1+2+3"
-- runParser expr "2*3*4"
-- runParser expr "(1+2)*(3+4)+5"

-- eval . fst <$> runParser expr "(1+2)*(3+4)+5"
