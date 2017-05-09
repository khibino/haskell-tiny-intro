module MonoidError where

import Data.Monoid
import Control.Monad.Trans.State
import Control.Monad.Trans.Except


--  Monoid e                =>  MonadPlus (ExceptT e m)

type Parser' in' = StateT in' (Except (Last String))
type Parser = Parser' String


-- error message 付きの Parser を実装する
-- implementing Parser with error message

-- 入力を一文字消費し、結果とする parser
-- parser which consume one char input and that char is parser's result
token :: Parser Char
token = undefined

-- 入力の終わりなら成功し、そうでなければ失敗する
-- success on end of input, otherwise failure
eof :: Parser ()
eof = undefined
