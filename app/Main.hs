module Main (main) where

import System.Environment
import Lib (run)
import RIO.Directory (getHomeDirectory)
import RIO.FilePath ((</>))

-- only for MacOS
netEaseMusicCachePath :: String
netEaseMusicCachePath = "Library/Containers/com.netease.163music/Data/Caches/online_play_cache"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--version"] -> putStrLn "0.0.1"
    [dest] -> do
      home <- getHomeDirectory
      let src = home </> netEaseMusicCachePath
      run src dest
    [src, dest] -> run src dest
    _ -> putStrLn "print usage"



