module Main where

-- import System.Console.GetOpt
import System.Environment
import System.IO
import Control.Monad

import Language.CLike.Parser
import Language.CLike.PreProcess

main = do
  args <- getArgs
  unless (null args) (do
      h <- openFile (head args) ReadMode
      s <- hGetContents h
      m <- runPrePr s
      let ts = preprocessModule m
      putStrLn "=========== preprocessed ==========="
      forM_ ts (putStrLn . show)
    )
