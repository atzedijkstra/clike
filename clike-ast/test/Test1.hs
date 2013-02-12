module Main where

-- import System.Console.GetOpt
import System.Environment
import System.IO
import Control.Monad

import Language.CLike.Parser

main = do
  args <- getArgs
  unless (null args) (do
      h <- openFile (head args) ReadMode
      s <- hGetContents h
      run s
    )
