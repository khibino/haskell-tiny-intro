import Control.Applicative
import System.Environment

import Parser
import Calc

-- length :: [a] -> Int

-- 行に区切る
-- lines :: String -> [String]

-- getArgs :: IO [String]

-- readFile :: FilePath -> IO String
-- putStrLn :: String -> IO ()
-- print :: Show a => a -> IO ()

-- map :: (a -> b) -> [a] -> [b]
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- mapM_ :: Monad m => (a -> m b) -> [a] -> IO ()

-- コマンドライン引数の最初のファイルの内容の全ての行について、
-- その文字列を expr として解釈し、
-- それぞれの評価結果を出力する
-- 解釈が失敗した場合には Error と出力する
-- コマンドライン引数が無いときにはなにもしない

-- 注意:
--   runParser expr           ではなく
--   runParser (expr <* eof)  を使った方がよい
main :: IO ()
main = do
  args <- getArgs
  case args of
    []    ->  undefined
    x:_xs  ->  do
      in' <- readFile x
      undefined

-- 文字列を expr として解釈し、
-- 評価結果を出力する
-- 解釈が失敗した場合には Error と出力する
parseAndPrint :: String -> IO ()
parseAndPrint s = case undefined of
  Nothing -> undefined
  Just e  -> undefined
