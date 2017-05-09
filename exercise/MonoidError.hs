module MonoidError where

import Data.Monoid
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Calc (Expr (..), eval)


-- :i Last
-- Last Nothing <> Last (Just "foo")
-- Last (Just "foo") <> Last Nothing
-- Last (Just "foo") <> Last (Just "bar")

--  Monoid e                =>  MonadPlus (ExceptT e m)

type Parser' in' = StateT in' (Except (Last String))
type Parser = Parser' String

runParser :: Parser a -> String -> Except (Last String) (a, String)
runParser = runStateT

-- error message 付きの Parser を実装する
-- implementing Parser with error message

-- error message 付きの失敗
-- failure with error message
-- hint. except, throwE
errorP :: String -> Parser a
errorP s = undefined

-- 入力を一文字消費し、結果とする parser
-- parser which consume one char input and that char is parser's result
token :: Parser Char
token = undefined

-- 入力の終わりなら成功し、そうでなければ失敗する
-- success on end of input, otherwise failure
eof :: Parser ()
eof = undefined


-- 以下の文法の Expr型の結果を持つ parser を実装してください
-- Implement following syntax parser which result is Expr type.
-- 演算子は右結合でよい
-- right associative operator implementation may be more simple.

--  expr :=
--       | expr + expr
--       | expr * expr
--       | ( expr )
--       | <decimal number>

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
