module Parser
       ( Parser (..)
       , success, combine
       , failure, orElse
       , token, eof
       ) where


-- type Parser a = String ->      Maybe                 (a,                     String)
--                 ^              ^                      ^                      ^
--                 入力の文字列   失敗するかもしれない   parser の結果の型      残りの入力
--                 input string   may fail               parser's result type   rest input

--  入力の文字列を解釈し、成功した場合は、結果と残りの入力のペアを返す関数
--  function which parse input string and returns parse result and rest input when succeed

-- newtype Parser a = Parser (String -> Maybe (a, String))

-- type と newtype
-- type and newtype

-- newtype は、元の型と同じ能力を持たせたいが、型を区別したいときに使う
-- newtype preserve propositions of source type, but another type.

newtype Parser a =
  Parser { runParser :: String -> Maybe (a, String) }

-- :t Parser
-- :t runParser

-- Parser と runParser は同型になっている
-- Parser and runParser is isomorphic.


-- 以下の関数の定義を完成させてください
-- Fill following function definitions.

-- success の簡単版
-- easier version of success
_success :: a -> (String -> Maybe (a, String))
_success x = \s -> Just (x, s)

-- 入力を消費せずに、必ず成功し、指定された値を結果とする parser
-- not consume input, always success and return result which is specified
success :: a -> Parser a
success x = Parser $ \s -> Just (x, s)

-- runParser (success 1) ""
-- runParser (success 0) "a"

-- 必ず失敗する parser
-- always fail parser
failure :: Parser a
failure = Parser $ const Nothing

-- runParser failure "a"

-- 入力を一文字消費し、結果とする parser
-- parser which consume one char input and that char is parser's result
token :: Parser Char
token = Parser $ \s -> case s of
  []    ->  Nothing
  x:xs  ->  Just (x, xs)

-- runParser token "abc"
-- runParser token ""

-- 入力の終わりなら成功し、そうでなければ失敗する
-- success on end of input, otherwise failure
eof :: Parser ()
eof =  Parser $ \s -> case s of
  []  -> Just ((), "")
  _:_ -> Nothing

-- runParser eof ""
-- runParser eof "a"

-- Parser の連接
-- Parser sequence

-- a を結果とする parser と a を使って b を結果とする parser を作る関数を合成して、b を結果とする parser を返す
-- Parser a : parser's result is `a'
-- a -> Parser b : function using `a' which result is parser which result is `b'
-- combine above two and make parser which result is `b'
combine :: Parser a -> (a -> Parser b) -> Parser b
combine pa f = Parser $ \s -> case runParser pa s of
  Nothing       ->  Nothing
  Just (a, s')  ->  runParser (f a) s'

-- runParser (token `combine` \c -> success c) "a"

-- combine を使って pair の parser を作る
-- make pair parser using `combine'
pair :: Parser a -> Parser b -> Parser (a, b)
pair pa pb =
  pa `combine`
  \a -> pb `combine`
        \b -> success (a, b)

-- 一文字入力し、条件判定が真なら成功し入力した文字を結果とする parser
-- input one char and success if predicate is true, otherwise failure
satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  token `combine`
  \c -> if p c then success c else failure

-- '1' `elem` ['0' .. '9']
-- runParser (satisfy (`elem` ['0' .. '9'])) "a"
-- runParser (satisfy (`elem` ['0' .. '9'])) "3"


-- Parser のエラーハンドリング
-- Parser error handling

-- a を結果とする parser を 2つ受けとり、1つ目が失敗したら 2つ目を実行する parser を返す
-- combine two parser. if first one is failed, run second parser.
orElse :: Parser a -> Parser a -> Parser a
orElse px py =
  Parser
  $ \s -> case runParser px s of
    r@(Just (_, _))  ->  r
    Nothing          ->  runParser py s

-- runParser (satisfy (`elem` ['0' .. '9']) `orElse` token) "a"
