module Main where

-- import System.Console.GetOpt
import System.Environment
import System.IO
import Control.Monad
import Language.CLike.Lexer

main = do
  args <- getArgs
  unless (null args) (do
      h <- openFile (head args) ReadMode
      s <- hGetContents h
      case scanner ( scfgAddOpts [ScOpt_CommentAs1Token] $
                     scannerConfigCXX
                   ) s of
        Left m -> putStrLn m
        Right t -> forM_ t (putStrLn . show)
    )
