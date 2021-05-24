module Logic.Config.Functions where

import Logic.Config.Types ( Config )
import Logic.Config.Internal
import Control.Exception (IOException)
import qualified Data.ByteString.Lazy as L
import Interface.Class ( MIOError )
import qualified Interface.MError.Exports as Error
import qualified Logic.Parse.Functions as Parse (eDecode)
import System.IO.Error (isDoesNotExistError)

readConfig :: MIOError m => m Config
readConfig = do
  bs <- L.readFile pathConfig `Error.catchEIO` handler
  c <- Parse.eDecode bs
  checkMinLogLevel c
  checkRepeatNumber c
  checkUniqueNames c
  checkExistAndSingleName c
  return c
  where
    handler :: IOException -> Error.E
    handler e
      | isDoesNotExistError e = Error.ConfigError "Configuration file not found!"
      | otherwise = Error.ConfigError "Error reading configuration file"

pathConfig :: FilePath
pathConfig = "config.json"
