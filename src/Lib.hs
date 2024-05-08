{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Lib (process, Config(..)) where

import Text.Regex.Posix ((=~))
-- import Text.Regex.Posix.ByteString.Lazy
import Control.Monad (forM_)
import NetEase (MusicCacheInfo, musicName)
import Data.Aeson (decode)
import Data.Bits (xor)
import Data.Word (Word8)
import RIO
import RIO.FilePath
import RIO.Directory (listDirectory)
import qualified RIO.ByteString.Lazy as L

data Config = Config
    { musicCachePath :: FilePath
    , targetDir :: FilePath }

process :: RIO Config ()
process  = do
    Config src dest <- ask
    ucFiles <- getUcFiles src
    forM_ ucFiles $ \file -> convert file dest
    --logInfo "finish process"

-- (xxx.uc!, xxx.info) -> (xxx.mp3)
convert :: FilePath -> FilePath -> RIO Config ()
convert file dest = do
    contents <- {- traceIO ("convert: " ++ file) >> -} liftIO $ L.readFile file
    let bytes = L.pack $ unmask <$> L.unpack contents
    info <- loadInfo . infoName $ file
    let songName = case info of
                Nothing -> takeBaseName file
                Just mi -> musicName mi
    liftIO $ L.writeFile (dest </> songName) bytes

unmask :: Word8 -> Word8
unmask = xor 0xa3

getUcFiles :: FilePath -> RIO Config [FilePath]
getUcFiles path = do
    files <- liftIO $ listDirectory path
    return $ (path </>) <$> filter isUc files

isUc :: FilePath -> Bool
isUc path = path =~ (".uc!" :: String)

infoName :: FilePath -> FilePath
infoName ucFile = takeDirectory ucFile </> takeBaseName ucFile ++ ".info"

loadInfo :: FilePath -> RIO Config (Maybe MusicCacheInfo)
loadInfo file = do
  contents <- {- traceIO ("findInfo: " ++ file) >> -} liftIO $ L.readFile file
  return . decode $ contents
