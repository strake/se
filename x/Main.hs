module Main (main) where

import Prelude hiding (getContents, putStr)

import Data.ByteString.Lazy.Char8 (getContents, putStr)
import System.Environment

import Lib

main :: IO ()
main = do
    progString:_ <- getArgs
    prog <- doProgram (Program progString)
    getContents >>= prog >>= putStr
