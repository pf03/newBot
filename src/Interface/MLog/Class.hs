module Interface.MLog.Class where

import Interface.MLog.Types
  ( ColorScheme,
    Enable,
    FuncName,
    LogConfig,
    LogLevel,
    LogSettings,
  )

class Monad m => MLog m where
  getSettings :: m LogSettings
  setSettings :: ColorScheme -> Enable -> FuncName -> m ()
  getConfig :: m LogConfig
  message :: LogConfig -> LogSettings -> LogLevel -> String -> m ()