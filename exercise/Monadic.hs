module Monadic (satisfy, char) where

import Data.Char (digitToInt)
import Control.Applicative
import Control.Monad

import Basic (readHex)
import Parser


-- 定義した Parser を環境に登録する (型クラスの instance にする)
-- Registering defined Parser type to environments (make type class instances)


-- 連接の抽象化
-- sequence abstraction

-- Monad
-- return :: a -> Parser a
-- return :: Monad m => a -> m a
-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

instance Monad Parser where
  return = success
  (>>=)  = combine


-- combine の代わりに (>>=) を使った pair を定義してください
-- define `pair' using (>>=) instead of combine
pairM' :: Parser a -> Parser b -> Parser (a, b)
pairM' pa pb = undefined

-- Monad にしたことによって、Haskell の do 糖衣構文が使えるようになる
-- `do' syntax sugar is available by applying Monad
pairM :: Parser a -> Parser b -> Parser (a, b)
pairM pa pb = do
  a <- pa
  b <- pb
  return $ (,) a b


-- Monad よりも力の弱い型クラスの instance にもする
-- make instances of weaker proposition type classes

-- Functor
-- fmap  :: Functor f => (a -> b) -> f a -> f b
--       :: Functor f => (a -> b) -> (f a -> f b)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<$>) :: (a -> b) -> Parser a -> Parser b
-- liftM :: Monad m => (a -> b) -> m a -> m b

instance Functor Parser where
  fmap = liftM

-- Applicative
-- pure :: Applicative f => a -> f a
-- return :: Monad m => a -> m a
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
-- ap :: Monad m => m (a -> b) -> m a -> m b

instance Applicative Parser where
  pure  = return
  (<*>) = ap

-- (,) :: a -> b -> (a, b)
--
-- (,) <$> (pa :: Parser a) :: Parser (b -> (a, b))
-- (,) <$> pa <*> pb :: Parser (a, b)

-- (,,) <$> pa <*> pb <*> pc :: Parser (a, b, c)
-- (,,,) <$> pa <*> pb <*> pc <*> pd :: Parser (a, b, c, d)


-- Applicative を使った pair の定義
-- pair definition using Applicative
pairA :: Parser a -> Parser b -> Parser (a, b)
pairA pa pb = (,) <$> pa <*> pb

swappedM :: Parser a -> Parser b -> Parser (a, b)
swappedM pa pb = do
  b <- pb
  a <- pa
  return $ (,) a b

-- エラーハンドリングの抽象化
-- error handling abstraction

-- empty :: Alternative f => f a
-- empty :: Parser a
-- (<|>) :: Parser a -> Parser a -> Parser a
-- (<|>) :: Alternative f => f a -> f a -> f a

-- a <|> empty  ==  a
-- empty <|> a  ==  a
-- (a <|> b) <|> c == a <|> (b <|> c)

-- a `mplus` mzero  ==  a
-- mzero `mplus` a  ==  a
-- (a `mplus` b) `mplus` c == a `mplus` (b `mplus` c)

instance Alternative Parser where
  empty  =  failure
  (<|>)  =  orElse

instance MonadPlus Parser where
  mzero  =  failure
  mplus  =  orElse


-- (*>) :: Applicative f => f a -> f b -> f b
-- (*>) :: Parser a -> Parser b -> Parser b
-- (<*) :: Applicative f => f a -> f b -> f a
-- (<*) :: Parser a -> Parser b -> Parser a
-- guard :: MonadPlus m => Bool -> m ()
-- guard :: Bool -> Parser ()

-- runParser (guard True) ""
-- runParser (guard False) ""


-- Monad あるいは Applicative を利用して以下を実装してください
-- Implement followings using Monad and Applicative

-- satisfy 再び。 guard を使って実装してみましょう。
-- satisfy again. Please use guard function.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = undefined

-- 与えられた文字を入力したら成功し、その文字を返す
-- specified character input succeed parsing and that character is result.
-- hint. satisfy
char :: Char -> Parser Char
char c = undefined

-- runParser (char 'a') "b"
-- runParser (char 'a') "a"

-- 16進数の文字だったら成功し、その文字を返す
-- hexadecimal character parser
-- hint. satisfy
--       '1' `elem` ['0' .. '9']
--       'e' `elem` ['a' .. 'f']
hex :: Parser Char
hex = undefined

-- runParser hex "1"
-- runParser hex "f"
-- runParser hex "x"

-- parser a が失敗するまで繰り返し実行し、[a] を parser の結果とする
-- repeat until run `parser a'. parser result is list
-- hint. -- 次の repeat1 との関係は? -- may be related to next problem repeat1
repeat0 :: Parser a -> Parser [a]
repeat0 pa = undefined

-- runParser (repeat0 hex) ""
-- runParser (repeat0 hex) "a1"

-- repeat0 とは違い、一つ以上の結果を成功とする
-- one or more length result only succeeds, different from repeat0
repeat1 :: Parser a -> Parser [a]
repeat1 pa = undefined

-- runParser (repeat1 hex) ""
-- runParser (repeat1 hex) "a1"

-- 0x で始まる16進数を読む parser, 0 桁はエラー
-- parser of hexadecimal integer begin with `0x', 0 digit number is error
hexInt :: Parser Int
hexInt = undefined

-- runParser hexInt "0x10A"


-- repeat0, repeat1 は定義しなくても標準で機能が提供されている
-- standard library functions are provided like `repeat0' and `repeat1'

-- :t many
-- :t some

-- many :: Alternative f => f a -> f [a]
-- some :: Alternative f => f a -> f [a]
