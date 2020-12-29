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
readNum = Num . foldl (\a c -> a * 10 + digitToInt c) 0

-- decimal number parser
numExpr :: Parser Expr
numExpr = readNum <$> some (satisfy (`elem` ['0' .. '9']))

-- unit term parser
unitExpr :: Parser Expr
unitExpr =
  char '(' *> plusExpr <* char ')' <|>
  numExpr

-- multiply formula parser
multExpr :: Parser Expr
multExpr = do
  -- parse unitExpr only once for `unitExpr '*' multExpr | unitExpr`
  u <- unitExpr
  (pure (Mult u) <* char '*' <*> multExpr <|>
   pure u)

{- naive implementation
multExpr =
  Mult <$> unitExpr <* char '*' <*> multExpr <|>
  unitExpr
 -}

-- plus formula parser
plusExpr :: Parser Expr
plusExpr = do
  -- parse multExpr only once for `unitExpr '*' multExpr | unitExpr`
  m <- multExpr
  (pure (Plus m) <* char '+' <*> plusExpr <|>
   pure m)

{- naive implementation
plusExpr =
  Plus <$> multExpr <* char '+' <*> plusExpr <|>
  multExpr
-}

-- top-level parser
expr :: Parser Expr
expr = plusExpr <* eof {- add eof to parse whole input -}

-- runParser expr "1"
-- runParser expr "(1)"
-- runParser expr "1+2+3"
-- runParser expr "2*3*4"
-- runParser expr "(1+2)*(3+4)+5"
-- runParser expr "2*3+4*5"

-- evaluator
eval :: Expr -> Int
eval = f  where
  f (Num n) = n
  f (Plus x y) = eval x + eval y
  f (Mult x y) = eval x * eval y

-- eval . fst <$> runParser expr "(1+2)*(3+4)+5"
-- eval . fst <$> runParser expr "2*3+4*5"
