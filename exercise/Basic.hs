module Basic where

import Data.Bool
import Data.Char (digitToInt)
import Data.Maybe (isJust, fromJust)

-- Haskell
-- 純粋関数プログラミング言語
-- Purely Functional Programming Language


-- 簡単な定義の例 - 関数、値 の定義
-- simple definition examples - definitions of values and functions

--     Int -> (Int -> (Int -> Int))
foo :: Int ->  Int ->  Int -> Int   -- 型シグネチャ  -- type signature
foo x y z = x + y + z               -- 定義          -- definition body

bar :: Bool -> Int
bar b =
  if b
  then 1
  else 0

one :: Integer
one = 1

bar2 :: Bool -> Int
bar2 b = bar b + 2


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


-- 複数の値を同時に持つ型を直積(デカルト積)型という。C でいう構造体のような型
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

exampleP1 :: P1
exampleP1 = P1 1 "World"

-- :t exampleP1

data P2 a  =  P2 a String    deriving Show
--      ^
--      型の引数
--      arity type variable

-- :i P2
-- :t P2
-- :t P2 'x'
-- :t P2 foo

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

-- :i (,)
-- :t (,)

data () = ()  -- ユニット, unit

属する値がただ一つ () しか無いような型 ()。
Type () contains the only one value ().

直積の単位元と考えることができる。
assume unit of product type.

-- :i ()
-- :t ()
-}


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

-- 複数の値のいずれかを持つ型を直和型という。 C ではたとえば判別共用体で実現する
-- data type which representation is one of multiple value, called sum(disjoint union) type.
-- Implementing this as tagged union in C.

-- 複数の可能性があるが、必ずどれかになる型という有用な概念。静的型付けの関数型言語以外ではあまり無い機能。
-- It is useful idea that sum type may be a number of possibility, and must be one in them.
-- It is rare feature other than statically typed functional programming language.

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
  = Nothing
  | Just a
 -}

-- :i Maybe
-- :t Just
-- :t Just "Hello"

{-
data Either e a
  = Left e
  | Right a
 -}

-- :i Either
-- :t Right
-- :t Left

{-
data [] a
  = []
--  ^ data constructor
  | (:) a ([] a)
--  ^ data constructor

-- [] a === [a]
-- (:) x y === x : y

data [a]
  = []
  | a : [a]
 -}

-- :i :
-- :t []
-- :t (:)
-- :t 'a' : "bc"


-- パターンマッチ(パターン照合)
-- pattern match

bar3 :: Bool -> Int
bar3 True   =  1
bar3 False  =  0

bar3' :: Bool -> Int
bar3' b = case b of
  True  ->  1
  False ->  0

--     型変数入りの型シグネチャ
--     type signature using type variable
baz :: a -> Maybe a -> a
baz d m = case m of
  Just x   -> x
  Nothing  -> d

baz4 :: Int -> Maybe Int -> Int
baz4 d m = case m of
  Just x   ->  x + 1
  Nothing  ->  d

baz4' :: Int -> Maybe Int -> Int
baz4' d m =
  if isJust m           --  m != null
  then fromJust m + 1   --  m + 1
  else d

wrongBaz4 :: Int -> Maybe Int -> Int
wrongBaz4 d m =
  if isJust m           --  m != null
  then d
  else fromJust m + 1   --  m + 1  -- error
       -- このelse節はコンパイルエラーにならず、ランタイムエラーになる
       -- This else clause is not compile error but runtime error

       -- 場合を尽していない場合の警告を見ることができない
       -- Can't find non-exhaustive

baz2 :: a -> Maybe a -> a
baz2 _ (Just x)  =  x     -- 使用しない引数を受けるのに (_) ワイルドカードパターンを使っている
                          -- using (_) wild-card pattern against not using arity
baz2 d Nothing   =  d

-- baz2 "Hello" Nothing
-- baz2 "Hello" (Just "World")

-- baz2 "Hello" (Just 1)  -- error

xfoo :: a -> Maybe (Maybe (Int, a)) -> a
xfoo d m = case m of
  Just (Just (_, x)) ->  x
  Just  Nothing      ->  d
  Nothing            ->  d

  -- ネストしていてもパターンマッチできる
  -- nesting pattern matching

-- xfoo "Hello" (Just (Just (10, "World")))

xfoo2 :: Maybe (Maybe (Int, a)) -> Int
xfoo2 m = case m of
  Just _  ->  1
  Nothing ->  0

  -- 型を充足するように必要なところまでパターンマッチすればよい
  -- may do pattern match as needed

factorial :: Integer -> Integer
factorial n0 = case n0 of
  0 -> 1
  n -> n * factorial (n - 1)  -- 再帰  -- recursion
  -- 通常のデータコンストラクタではないのでちょっと例が良くないけれど ...
  -- Not good example because it is not normal data constructor ...

-- factorial 10


-- List Pattern match
sumInt :: [Integer] -> Integer
sumInt xxs = case xxs of
  []       ->  0
  x : xs   ->  x + sumInt xs  -- 再帰  -- recursion

-- sumInt [1 .. 10]


-- フィボナッチ数列の n 項目を返す関数を書いてください
-- Write a function which result is nth result of Fibonacci number

-- f(n+2) = f(n+1) + f(n)
--      1 =     1  +  0
--      2 =     1  +  1
--      3 =     2  +  1
--      5 =     3  +  2
--      ...

-- hint
fib :: Integer -> Integer
fib 0 = 0
fib 1 = undefined
fib n = undefined

-- map fib [0 .. 10]

-- 型エイリアスの定義
-- type alias definition

type MaybeI = Maybe Int
type PResult a = Maybe (a, String)

-- 新しい型名は右辺の型式と同じ型になる
-- new named type is the same type of right type formula

-- (Just 1 :: Maybe Int) :: MaybeI
-- (Just 1 :: MaybeI) :: Maybe Int

-- type String = [Char]

-- :i String

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

-- exp $ 2 + 3
-- :info $
-- :info +

-- :t (.)
-- (2^) . (* 3) . (+ 2) $ 3


-- ガード
-- Guard

bar4 :: Bool -> Int
bar4 b
  | b          =  1
  | otherwise  =  0

bar4' :: Bool -> Int
bar4' b
  | b          =  1
  | False      =  2
  | otherwise  =  0

bar5 :: Int -> Int
bar5 n
  | n <= 0            =  0
  | 1 <= n && n <= 5  =  1
  | otherwise         =  2

factorial2 :: Integer -> Integer
factorial2 n
  | n <= 0    =  1
  | otherwise =  n * factorial2 (n - 1)

factorial3 :: Integer -> Integer
factorial3 n =  case n of
  0              ->  1
  k | k <  0     ->  1
    | otherwise  ->  k * factorial3 (k - 1)


-- リストの先頭から最大 n 個を取り出す関数を書いてください
-- Write a function which takes most n elements from list head.
take' :: Int -> [a] -> [a]
take' = undefined

-- take' 2 []          ===  []
-- take' 0 [1,2,3]     ===  []
-- take' 2 [1,2,3]     ===  [1,2]
-- take' (-1) [1,2]    ===  []
-- take' 3 [1,2,3,4]   ===  [1,2,3]
-- take' 3 [1,2]       ===  [1,2]

-- 局所定義
-- local scope definition


factorial4 :: Integer -> Integer
factorial4 = go 1
  where
    go :: Integer -> Integer -> Integer  -- 局所定義では省略してもよい
                                         -- type signature is optional in local scope
    go a n
      | n <= 0     =  a
      | otherwise  =  go (n * a) (n - 1)  --  tail recursion
--                       ^
--                       蓄積引数
--                       accumulator arity

-- :t digitToInt
-- map digitToInt ['0' .. '9']
-- map digitToInt ['a' .. 'f']

-- 16進数文字列を数値に直す関数を書いてください
-- Write a function which convert hexadecimal string into integer
readHex' :: String -> Int
readHex' = undefined

-- 畳み込み
-- folding

-- S型に対する畳み込み
-- folding for type S
runS :: (Char -> Int -> a) -> (String -> a) -> S -> a
runS f g s = case s of
  X c i -> f c i
  Y str -> g str

-- :t runS
-- runS (\c i -> replicate i c) id exampleX0
-- runS (\c i -> replicate i c) id exampleY0

-- Bool型に対する畳み込み
-- folding for type Bool
-- GHC 8 以降なら bool という名前で定義されている
-- pre-defined as `bool' in GHC 8 or newer
bool' :: a -> a -> Bool -> a
bool' t e b = if b then t else e

-- :t bool'
-- :t bool
exampleBoolT :: String
exampleBoolT = bool' "foo" "bar" True
exampleBoolF :: String
exampleBoolF = bool' "foo" "bar" False

-- Maybe型に対する畳み込み

-- :t maybe
exampleMaybeJ :: Integer
exampleMaybeJ = maybe 0 (\x -> 2 ^ x) (Just 10 :: Maybe Integer)
exampleMaybeJ' :: Integer
exampleMaybeJ' = case Just 10 :: Maybe Integer of
  Nothing  -> 0
  Just x   -> 2 ^ x

exampleMaybeN :: Integer
exampleMaybeN = maybe 0 (\x -> 2 ^ x) (Nothing :: Maybe Integer)
exampleMaybeN' :: Integer
exampleMaybeN' = case Nothing :: Maybe Integer of
  Nothing  ->  0
  Just x   ->  2 ^ x

-- Either型に対する畳み込み
-- :t either
exampleEitherR :: String
exampleEitherR = either (\e -> "failed: " ++ e) (\x -> "result is " ++ show x) (Right 10 :: Either String Int)
exampleEitherR' :: String
exampleEitherR' = case Right 10 :: Either String Int of
  Left e  -> "failed: " ++ e
  Right x -> "result is " ++ show x

exampleEitherL :: String
exampleEitherL = either (\e -> "failed: " ++ e) (\x -> "result is " ++ show x) (Left "no result" :: Either String Int)
exampleEitherL' :: String
exampleEitherL' = case Left "no result" :: Either String Int of
  Left e  -> "failed: " ++ e
  Right x -> "result is " ++ show x

-- 次のデータ型に対する畳み込みの関数を書いてください。
-- Write a folding function for the next data types.

-- data P0 -- 定義済みの P0

runP0 :: (Int -> String -> r) -> P0 -> r
runP0 = undefined

data Pair a b = Pair a b

runPair = undefined

data S2 a
  = Foo Int
  | Bar a Int
  | Baz String

runS2 = undefined

-- List型 ( [] ) に対する畳み込み
-- folding for List ( [] ) type
-- List型に対する畳み込みは 2通り考えられる
-- There is two way folding for list type.

-- :t foldr
{-
foldr f i xxs = case xxs of
  x:xs  ->  f x (foldr f i xs)
  []    ->  i
 -}
-- 余再帰
exampleListLRN :: Integer
exampleListLRN = foldr (+) 0 []
exampleListLRC :: Integer
exampleListLRC = foldr (+) 0 [1 .. 20]

-- :t foldl
{-
foldl f i xxs = case xxs of
  x:xs  ->  foldl f (f i x) xs
  []    ->  i
 -}
-- 末尾再帰
exampleListLLN :: Integer
exampleListLLN = foldl (+) 0 []
exampleListLLC :: Integer
exampleListLLC = foldl (+) 0 [1 .. 20]


factorial5 :: Integer -> Integer
factorial5 n = foldl (*) 1 [n, n - 1 .. 1]

-- 畳み込みを使った readHex' を書いてください
-- Write folding version of readHex'
-- hint. factorial5 n = foldl (\a n -> n * a) 1 [n, n - 1 .. 1]
readHex :: String -> Int
readHex = undefined
