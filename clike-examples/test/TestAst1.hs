module Main where

-- import System.Console.GetOpt
import           System.Environment
import           System.IO
import           Control.Monad
import qualified UHC.Util.FastSeq as Seq

import           Language.CLike.Parser
import           Language.CLike.PreProcess

main = do
  args <- getArgs
  unless (null args) (do
      m <- parsePrePrFileAndReport (head args)
      (ts,errs) <- runPrePr m
      putStrLn "=========== preprocessed ==========="
      forM_ (Seq.toList ts) (putStrLn . show)
      putStrLn "=========== preprocessed errs ==========="
      forM_ (Seq.toList errs) (putStrLn)
    )
