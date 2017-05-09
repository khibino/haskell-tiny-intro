module MonadTrans where

import Data.Time
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

import Calc (Expr (..), eval)

-- Monad Transformer

-- Monad m があったとき Monad (t m) となるような t
-- t which are Monad (t m) where Monad m
{-
class MonadTrans t where
  lift :: Monad m => m a -> t m a
  --                        型の式もカリー化されている  (t m) a
  --                        type expression is also curried  (t m) a

-- Monad (t m) の instance は MonadTrans とは別に定義する
 -}

-- 何もしない monad
{-
newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return = Identity
  Identity x >>= f  =  f x

-- MonadTrans t と組みあわせると普通の monad になる
-}

-- 代表的な monad transformer
--               IdentityT m a   ===  m a
--               ReaderT r m a   ===  r -> m a
--   Monoid w => WriterT w m a   ===  m (a, w)
--               StateT s m a    ===  s -> m (a, s)
--               MaybeT m a      ===  m (Maybe a)
--               ExceptT e m a   ===  m (Either e a)

-- :i IdentityT
-- :i ReaderT
-- :i WriterT
-- :i StateT
-- :i MaybeT
-- :i ExceptT


type Parser = StateT String Maybe


-- MonadPlus と MonadTrans
-- MonadPlus and MonadTrans
-- 内側の性質を引き継ぐ
-- inherit inner monad structure
--   MonadPlus m             =>  MonadPlus (IdentityT m)
--   MonadPlus m             =>  MonadPlus (ReaderT r m)
--  (Monoid w, MonadPlus m)  =>  MonadPlus (WriterT w m)
--   MonadPlus m             =>  MonadPlus (StateT s m)

-- 代わりの結果
--                               MonadPlus (MaybeT m)
--   Monoid e                =>  MonadPlus (ExceptT e m)


-- エラーハンドリング
--                              MonadPlus (MaybeT m)
--  MonadPlus m             =>  MonadPlus (StateT s m)

--                              MonadPlus (StateT String Maybe)

-- 入力を一文字消費し、結果とする parser
-- parser which consume one char input and that char is parser's result
token :: Parser Char
token = undefined

-- 入力の終わりなら成功し、そうでなければ失敗する
-- success on end of input, otherwise failure
eof :: Parser ()
eof = undefined


-- parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2017-05-09 12:34:56" :: Maybe LocalTime

-- 時刻を解釈する parser を実装してください
-- implement parser to parse timestamp string
timestamp :: Parser LocalTime
timestamp = undefined
