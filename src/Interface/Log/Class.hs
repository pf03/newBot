module Interface.Log.Class where

import Interface.Log.Types
  ( ColorScheme,
    Enable,
    FuncName,
    Config,
    Level,
    Settings,
  )

class Monad m => MLog m where
  getSettings :: m Settings
  setSettings :: ColorScheme -> Enable -> FuncName -> m ()
  getConfig :: m Config
  message :: Config -> Settings -> Level -> String -> m ()