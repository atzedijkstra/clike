module Main where

-- import System.Console.GetOpt
import           System.Environment
import           System.IO
import           Control.Monad
import qualified UHC.Util.FastSeq as Seq
import           Options.Applicative

import           Language.CLike.Parser
import           Language.CLike.PreProcess

main = do
    args <- getArgs
    unless (null args) (do
        (file,mkopts) <- execParser opts
        m <- parsePrePrFileAndReport file
        (ts,errs) <- runPrePr (mkopts emptyPrePrOpts) m
        putStrLn "=========== preprocessed ==========="
        forM_ (Seq.toList ts) (putStrLn . show)
        putStrLn "=========== preprocessed errs ==========="
        forM_ (Seq.toList errs) (putStrLn)
      )
  where 
    opts =
      info (   helper
           <*> ((,)
               <$> argument str (metavar "FILE")
               <*> parsePrePrOpts
               )
           ) 
           (  fullDesc
           <> progDesc "Test for CLike preprocessing"
           <> header "test-ast1"
           )

{-
main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
-}
