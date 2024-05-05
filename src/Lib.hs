module Lib (process) where

import System.Directory ( listDirectory )
import System.FilePath ((</>), takeBaseName, takeDirectory)
import Text.Regex.Posix ( (=~) )
import Control.Monad (forM_)
import NetEase (MusicCacheInfo, musicName)
import Data.ByteString.Lazy.Internal (unpackBytes, packBytes)
import Data.Aeson (decode)
import Data.Bits (xor)
import Data.Word (Word8)
import Data.ByteString.Lazy (readFile, writeFile)
import Prelude hiding (readFile, writeFile)
import Debug.Trace ( traceIO )

process :: FilePath -> FilePath -> IO ()
process src dest = do
    ucFiles <- getUcFiles src
    forM_ ucFiles $ \file -> convert file dest
    putStrLn "finish process"

-- (xxx.uc!, xxx.info) -> (xxx.mp3)
convert :: FilePath -> FilePath -> IO ()
convert file dest = do
    contents <- traceIO ("convert: " ++ file) >> readFile file
    let bytes = packBytes $ unmask <$> unpackBytes contents
    info <- loadInfo . infoName $ file
    let songName = case info of
                Nothing -> takeBaseName file
                Just mi -> musicName mi
    writeFile (dest </> songName) bytes

unmask :: Word8 -> Word8
unmask = xor 0xa3

getUcFiles :: FilePath -> IO [FilePath]
getUcFiles path = do
    files <- listDirectory path
    return $ (path </>) <$> filter isUc files

isUc :: FilePath -> Bool
isUc path = path =~ ".uc!"

infoName :: FilePath -> FilePath
infoName ucFile = takeDirectory ucFile </> takeBaseName ucFile ++ ".info"

loadInfo :: FilePath -> IO (Maybe MusicCacheInfo)
loadInfo file = do
  contents <- traceIO ("findInfo: " ++ file) >> readFile file
  return . decode $ contents


