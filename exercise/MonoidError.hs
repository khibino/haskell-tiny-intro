module MonoidError where

import Data.Monoid
import Control.Monad.Trans.State
import Control.Monad.Trans.Except


-- :i Last
-- Last Nothing <> Last (Just "foo")
-- Last (Just "foo") <> Last Nothing
-- Last (Just "foo") <> Last (Just "bar")

--  Monoid e                =>  MonadPlus (ExceptT e m)

type Parser' in' = StateT in' (Except (Last String))
type Parser = Parser' String


-- error message 付きの Parser を実装する
-- implementing Parser with error message

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
