import System.Environment

-- length :: [a] -> Int

-- 単語に区切る
-- words :: String -> [String]

-- 行に区切る
-- lines :: String -> [String]

-- リストを逆順に
-- reverse :: [a] -> [a]

-- getArgs :: IO [String]

-- readFile :: FilePath -> IO String
-- print :: Show a => a -> IO ()

-- コマンドライン引数の最初のファイルの内容の全ての行について、
-- 単語の順序を逆順にし、
-- 結果の文字列を出力する
-- コマンドライン引数が無いときにはなにもしない
main :: IO ()
main = do
  args <- getArgs
  case args of
    []    ->  undefined
    x:xs  ->  undefined
