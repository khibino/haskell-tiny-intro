module Intro where

-- Haskell
-- 純粋関数プログラミング言語
-- Purely Functional Programming Language


-- 簡単な定義の例
-- simple definition examples

foo :: Int -> Int -> Int -> Int     -- 型シグネチャ  -- type signature
foo x y z = x + y + z               -- 定義          -- definition body

bar :: Bool -> Int
bar b =
  if b
  then 1
  else 0


-- :type foo
-- :t foo
-- :t bar

-- foo 1 2 3
-- bar True
-- bar2 True


-- データ型の定義
-- data type definition

-- 直積型
-- Product type

data P0  =                P0 Int String               deriving Show
--   ^                    ^                           ^
--   Type constructor     Data constructor            code generation for printing
--   型コンストラクタ     データ(値)コンストラクタ    表示用コード生成

-- :i P0
-- :t P0
-- :t P0 0

exampleP0 :: P0
exampleP0 = P0 0 "Hello"

-- :t exampleP0      -- タブで補完できる -- completion is available using TAB
-- :t P0 1 "World!"


-- 複数の値を同時に持つ型を直積型(デカルト積型)という。C でいう構造体のような型
-- data type which has multiple value, called direct product(cartesian product) type.
-- like structure type in C.

data P1  =
  P1
  { rid  :: Int
  , name :: String
  } deriving Show

-- :t P1
-- :t rid
-- :t name

data P2 a  =  P2 a String    deriving Show
--      ^
--      型の引数
--      arity type variable

-- :i P2
-- :t P2
-- :t P2 'x'

-- パラメータを持った型を定義することができる。
-- parameterized type definition
-- like C++ template class, Java generic class


{-
-- あらかじめ定義済みの型の例
-- predefined type examples

data (,) a b  =  (,) a b
--       ^ ^
--       型の引数
--       arity type variables

-- (,) a b === (a, b)

data (a, b)  =  (a, b)
 -}

-- :i (,)
-- :t (,)


-- 直和型
-- Sum type

data S
  = X Char Int
  | Y String
  deriving Show

-- :i S
-- :t X
-- :t Y

exampleX0 :: S
exampleX0 = X 'z' 10

exampleY0 :: S
exampleY0 = Y "wai"

-- 複数の値のいづれかを持つ型を直和型という。 たとえば C では判別共用体で実現する
-- data type which representation is one of multiple value, called sum(direct sum) type.

{-
-- あらかじめ定義済みの型の例
-- predefined type examples

data Bool
  = False
  | True
 -}

-- :i Bool
-- :t False

{-
data Maybe a
  = Just a
  | Nothing
 -}

-- :i Maybe
-- :t Just
-- :t Just "Hello"

{-
data [] a
  = []
  | (:) a ([] a)

-- [] a === [a]
-- (:) x y === x : y

data [a]
  = []
  | a : [a]
 -}

-- :i :
-- :t 'a' : "bc"


-- 値、関数 の定義
-- definitions of values and functions

one :: Integer
one = 1

bar2 :: Bool -> Int
bar2 b = bar b + 2


-- パターンマッチ(パターン照合)
-- pattern match

bar3 :: Bool -> Int
bar3 True   =  1
bar3 False  =  0

baz :: a -> Maybe a -> a  -- 型変数入りの型シグネチャ  -- type signature using type variable
baz d m = case m of
  Just x   -> x
  Nothing  -> d

baz2 :: a -> Maybe a -> a
baz2 _ (Just x)  =  x
baz2 d Nothing   =  d

-- baz2 "Hello" Nothing
-- baz2 "Hello" (Just "World")

-- baz2 "Hello" (Just 1)  -- error


factorial :: Integer -> Integer
factorial n0 = case n0 of
  0 -> 1
  n -> n * factorial (n - 1)  -- 再帰  -- recursion

-- factorial 10


-- List Pattern match
sumInt :: [Integer] -> Integer
sumInt xxs = case xxs of
  []       ->  0
  x : xs   ->  x + sumInt xs  -- 再帰  -- recursion

-- sumInt [1 .. 10]


-- 型エイリアスの定義
-- type alias definition

type MaybeI = Maybe Int

-- 新しい型名は右辺の型式と同じ型になる
-- new named type is the same type of right type formula


{-
type String = [Char]
 -}


-- 2項演算子
-- infix binary operators

-- 関数としての演算子
-- operator as function

plus :: Int -> Int -> Int
plus = (+)


-- セクション  -- 演算子の部分適用
-- section     -- partial application of operator function

-- :t ('x' :)            -- 2項演算子の左辺に適用  -- apply left argument of infix binary operator
-- ('x' :) "abc"
-- :t (: "abc")          -- 2項演算子の右辺に適用  -- apply right argument of infix binary operator
-- (: "abc") 'x'
-- :t (== "Hello")
-- :t (`plus` 1)         -- ` ` でくくると関数を 2項演算子として利用できる
                         -- backquoted function is available as infix binary operator
-- :t (+ (1 :: Int))
-- :t (+ 1)


-- ラムダ式
-- lambda formula

-- :t \s -> 'x' : s
-- :t \x y -> Just (x, y)
-- :t map (\x -> plus x 1)
-- :t map $ \x -> plus x 1   -- 2項演算子 $ は括弧の代わりに使えることが多い
                             -- binary operator `$' is useful instead of parens
-- :t ($)


-- ガード
-- Guard

bar4 :: Bool -> Int
bar4 b
  | b          =  1
  | otherwise  =  0

factorial2 :: Integer -> Integer
factorial2 n
  | n <= 0    =  1
  | otherwise =  n * factorial2 (n - 1)
