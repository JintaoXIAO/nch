module Main (main) where

import System.Environment
import Lib (process, Config(..))
import RIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src, dest] -> do
      putStrLn $ "src: " ++ src ++ ", dest: " ++ dest
      let config = Config src dest
      runRIO config $ process
    _ -> putStrLn "print usage"
