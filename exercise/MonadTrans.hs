module MonadTrans
       ( Parser'
       )where

import Control.Monad.Trans.State

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

type Parser' in' = StateT in' Maybe

type Parser = Parser' String


token :: Parser Char
token = undefined

eof :: Parser ()
eof = undefined
