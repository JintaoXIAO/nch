module Main (main) where

import System.Environment
import Lib (process)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src, dest] -> do
      putStrLn $ "src: " ++ src ++ ", dest: " ++ dest
      process src dest
    _ -> putStrLn "print usage"
