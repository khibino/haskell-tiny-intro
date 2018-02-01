import System.Environment

-- length :: [a] -> Int

-- 単語に区切る
-- words :: String -> [String]

-- 単語列を一つの文字列に
-- unwords :: [String] -> String

-- 行に区切る
-- lines :: String -> [String]

-- 複数行を一つの文字列に
-- unlines :: [String] -> String

-- リストを逆順に
-- reverse :: [a] -> [a]

-- getArgs :: IO [String]

-- readFile :: FilePath -> IO String
-- putStrLn :: String -> IO ()
-- putStr :: String -> IO ()

-- map :: (a -> b) -> [a] -> [b]
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- mapM_ :: Monad m => (a -> m b) -> [a] -> IO ()

-- do の中で let を使って変数定義できる
--   do x <- readLn
--      let y = 2 * x
--      ...


-- コマンドライン引数の最初のファイルの内容の全ての行について、
-- 単語の順序を逆順にし、
-- 結果の文字列を出力する
-- コマンドライン引数が無いときにはなにもしない
main :: IO ()
main = do
  args <- getArgs
  case args of
    []    ->  undefined
    x:xs  ->  do
      undefined
