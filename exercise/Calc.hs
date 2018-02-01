module Calc (Expr (..), expr, eval) where

import Data.Char
import Control.Applicative

import Parser
import Monadic


data Expr
  = Num Int
  | Plus Expr Expr
  | Mult Expr Expr
  deriving (Eq, Show)

-- 以下の文法の Expr型の結果を持つ parser を実装してください
-- Implement following syntax parser which result is Expr type.
-- 演算子は右結合でよい
-- right associative operator implementation may be more simple.

--  expr :=
--       | expr + expr
--       | expr * expr
--       | ( expr )
--       | <decimal number>

-- hint.
-- plusExpr := plusExpr + multExpr |  -- 左再帰 は都合が悪い  -- left recursion is difficult
--             multExpr
--
-- expr := plusExpr
-- plusExpr := multExpr '+' plusExpr |
--             multExpr
-- multExpr := unitExpr '*' multExpr |
--             unitExpr


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
-- runParser expr "2*3+4*5"

-- evaluator
eval :: Expr -> Int
eval = undefined

-- eval . fst <$> runParser expr "(1+2)*(3+4)+5"
-- eval . fst <$> runParser expr "2*3+4*5"
