module IO where

--
-- IO モナド
--

-- 外界に影響を与えるプログラムを書くには
-- IO という Monad を使う
--
-- IO a
--    ^ 結果
-- 「IO を行なった結果として a が得られる」
-- というように読める

-- Monad なので do 文を利用することができる
-- 結合法則も成り立っている
--
-- do A
--    do B
--       C
-- ===
-- do do A
--       B
--    C

-- putStrLn :: String -> IO ()
-- putStrLn "Hello world!"

-- getLine :: IO String
-- getLine

-- readLn :: Read a => IO a
-- readLn

-----

--
-- 遅延 IO
--

-- IO の結果が遅延する特別な IO

-- getContents :: IO String

-- -- type FilePath = String
-- readFile :: FilePath -> IO String
