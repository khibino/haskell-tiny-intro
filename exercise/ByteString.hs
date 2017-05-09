module ByteString where

import MonoidError (Parser', errorP)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8


type Parser = Parser' ByteString

token :: Parser Char
token = undefined

chunk :: Int -> Parser ByteString
chunk = undefined

eof :: Parser ()
eof = undefined
