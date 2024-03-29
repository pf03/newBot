module Interface.Cache.Config.Functions where

import Control.Exception (IOException)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as L
import Interface.Cache.Config.Internal
  ( checkExistAndSingleName,
    checkMinLogLevel,
    checkRepeatNumber,
    checkUniqueNames,
  )
import Interface.Cache.Config.Types (Config)
import qualified Interface.Error.Exports as Error
import qualified Parse.Functions as Parse (eDecode)
import System.IO.Error (isDoesNotExistError)

readConfig :: (MonadIO m) => m Config
readConfig = do
  bs <- L.readFile pathConfig `Error.catchEIO` handler
  config <- Parse.eDecode bs
  checkMinLogLevel config
  checkRepeatNumber config
  checkUniqueNames config
  checkExistAndSingleName config
  return config
  where
    handler :: IOException -> Error.Error
    handler err
      | isDoesNotExistError err = Error.ConfigError "Configuration file not found!"
      | otherwise = Error.ConfigError "Error reading configuration file"

pathConfig :: FilePath
pathConfig = "config.json"
