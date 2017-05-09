module State where

import MonoidError (Parser)
import Control.Monad.Trans.State

data Loc =
  Loc
  { line :: !Int
  , column :: !Int
  } deriving (Eq, Show)

type LParser = StateT Loc Parser


-- 位置情報入りの error message 付きの Parser を実装する
-- implementing Parser with error message of location information

-- error message 付きの失敗
-- failure with error message
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
