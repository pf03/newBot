{-# LANGUAGE DeriveGeneric #-}

module Interface.Cache.Config.Types where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics (Generic)
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Log.Exports as Log



data Config = Config
  {
    forks :: Bool,
    name :: String,
    defaultRepeatNumber :: Int,
    apps :: [Cache.ConfigApp],
    text :: Cache.ConfigText,
    log :: Log.Config
  }
  deriving (Show, Generic)

instance ToJSON Config
instance FromJSON Config