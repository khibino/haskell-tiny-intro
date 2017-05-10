module State where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Data.Monoid (Last (..))

import MonoidError (Parser)

data Loc =
  Loc
  { line :: !Int
  , column :: !Int
  } deriving (Eq, Show)

type LParser = StateT Loc Parser

--- StateT s m a  ===  s -> m (a, s)
--- StateT Loc Parser a
--- StateT Loc (Parser' String) a
--- StateT Loc (StateT String (Except (Last String))) a
---             StateT String (Except (Last String)) a
---               ===  String -> Except (Last String) (a, String)
--- StateT Loc (StateT String (Except (Last String))) a
---               ===  Loc -> StateT String (Except (Last String)) (a, Loc)
---               ===  Loc -> String -> (Except (Last String)) ((a, Loc), String)

initialLoc :: Loc
initialLoc = undefined

runParser :: LParser a -> Loc -> String -> Except (Last String) ((a, Loc), String)
runParser lp = undefined

-- 位置情報入りの error message 付きの Parser を実装する
-- implementing Parser with error message of location information

-- error message 付きの失敗
-- failure with error message
errorP :: String -> LParser a
errorP s = undefined

-- 入力を一文字消費し、結果とする parser
-- parser which consume one char input and that char is parser's result
token :: LParser Char
token = undefined

-- runParser initialLoc (replicateM 6 token) "ab\nc\nd"
-- runParser initialLoc (replicateM 7 token) "ab\nc\nd"

-- 入力の終わりなら成功し、そうでなければ失敗する
-- success on end of input, otherwise failure
eof :: LParser ()
eof = undefined

-- runParser initialLoc (replicateM 6 token <* eof) "ab\nc\nd"
-- runParser initialLoc (replicateM 5 token <* eof) "ab\nc\nd"
