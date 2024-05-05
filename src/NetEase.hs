{-# LANGUAGE DeriveGeneric #-}

module NetEase (MusicCacheInfo(..)
               ,musicName) where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON )

data MusicCacheInfo = MusicCacheInfo
  { bitrate :: String
  , volumeGain :: String
  , et :: String
  , songId :: String
  , md5 :: String
  , format :: String
  } deriving Generic

musicName :: MusicCacheInfo -> String
musicName mi = songId mi ++ "." ++ format mi

instance FromJSON MusicCacheInfo