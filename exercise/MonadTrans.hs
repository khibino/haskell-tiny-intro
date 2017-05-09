module MonadTrans where

import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

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

-- 入力を一文字消費し、結果とする parser
-- parser which consume one char input and that char is parser's result
token :: Parser Char
token = undefined

-- 入力の終わりなら成功し、そうでなければ失敗する
-- success on end of input, otherwise failure
eof :: Parser ()
eof = undefined
