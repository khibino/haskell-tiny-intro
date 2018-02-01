import System.Environment

import Parser
import Calc

-- length :: [a] -> Int

-- 行に区切る
-- lines :: String -> [String]

-- getArgs :: IO [String]

-- readFile :: FilePath -> IO String
-- print :: Show a => a -> IO ()

-- map :: (a -> b) -> [a] -> [b]
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- mapM_ :: Monad m => (a -> m b) -> [a] -> IO ()

-- コマンドライン引数の最初のファイルの内容の全ての行について、
-- その文字列を expr として解釈し、
-- それぞれの評価結果を出力する
-- コマンドライン引数が無いときにはなにもしない
main :: IO ()
main = do
  args <- getArgs
  case args of
    []    ->  undefined
    x:xs  ->  undefined
