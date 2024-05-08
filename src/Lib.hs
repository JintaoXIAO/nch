{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Lib (run) where

import Text.Regex.Posix ((=~))
import NetEase (MusicCacheInfo, musicName)
import Data.Aeson (decode)
import Data.Bits (xor)
import RIO
    ( (++),
      stdout,
      lens,
      filter,
      traceIO,
      ($),
      Monad(return),
      Bool(..),
      String,
      Maybe(..),
      IO,
      Word8,
      (>>),
      (<$>),
      (.),
      error,
      FilePath,
      MonadIO(..),
      forM_,
      logInfo,
      runRIO,
      MonadReader(ask),
      RIO,
      logOptionsHandle,
      SimpleApp, HasLogFunc, LogFunc, setLogUseTime, withLogFunc, HasLogFunc(..), displayShow)
import RIO.FilePath ( (</>), takeBaseName, takeDirectory )
import RIO.Directory (listDirectory, getHomeDirectory)
import qualified RIO.ByteString.Lazy as L
import qualified RIO.Text as LT

data Config = Config
    { logFunc :: !LogFunc
    , musicCachePath :: FilePath
    , targetDir :: FilePath }

instance HasLogFunc Config where
  logFuncL = lens logFunc (\x y -> x { logFunc = y })

run :: String -> String -> IO ()
run src dest = do
  rawlogOptions <- logOptionsHandle stdout True
  let logOptions = setLogUseTime True rawlogOptions
  withLogFunc logOptions $ \lf -> do
    let conf = Config lf src dest
    runRIO conf process

process :: RIO Config ()
process  = do
    Config _ src dest <- ask
    ucFiles <- getUcFiles src
    forM_ ucFiles $ \file -> convert file dest
    logInfo "finish process"

-- (xxx.uc!, xxx.info) -> (xxx.mp3)
convert :: FilePath -> FilePath -> RIO Config ()
convert file dest = do
    contents <- {- logInfo (displayShow $ "convert: " ++ file) >> -} liftIO $ L.readFile file
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

loadInfo ::  FilePath -> RIO Config (Maybe MusicCacheInfo)
loadInfo file = do
  logInfo (displayShow $ "findInfo: " ++ file)
  contents <- liftIO $ L.readFile file
  return . decode $ contents
