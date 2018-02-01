import System.Environment

-- length :: [a] -> Int

-- 単語に区切る
-- words :: String -> [String]

-- getArgs :: IO [String]

-- readFile :: FilePath -> IO String
-- print :: Show a => a -> IO ()

-- コマンドライン引数の最初のファイルの内容を全て読み込み、
-- 単語に区切ってその数をカウントし、
-- 結果の数値を出力する
-- コマンドライン引数が無いときにはなにもしない
main :: IO ()
main = do
  args <- getArgs
  case args of
    []    ->  undefined
    x:xs  ->  undefined
