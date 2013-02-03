module Main where

-- import System.Console.GetOpt
import System.Environment
import System.IO
import Control.Monad
import Language.CLike.Lexer

{-
alexLoop s = do
  r <- runAlex s alexMonadScan
  case r of
    Left msg -> putStrLn msg
    Right t -> do
      rest <- 
-}

main = do
  args <- getArgs
  unless (null args) (do
      h <- openFile (head args) ReadMode
      s <- hGetContents h
      case scanner s of
        Left m -> putStrLn m
        Right t -> forM_ t (putStrLn . show)
    )
