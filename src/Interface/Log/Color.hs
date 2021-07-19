module Interface.Log.Color where

import Common.Functions (ifJust)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromJust)
import System.Console.ANSI
  ( Color (..),
    ColorIntensity (..),
    ConsoleIntensity (BoldIntensity, NormalIntensity),
    ConsoleLayer (Background, Foreground),
    SGR (Reset, SetColor, SetConsoleIntensity),
    setSGR,
  )

setSchemeT :: MonadIO m => Color -> m ()
setSchemeT color = case color of
  Blue -> setColorSchemeT Dull BoldIntensity (Just Yellow) (Just Blue)
  Cyan -> setColorSchemeT Vivid NormalIntensity (Just Black) (Just Cyan)
  Green -> setColorSchemeT Dull BoldIntensity (Just Blue) (Just Green)
  Yellow -> setColorSchemeT Vivid NormalIntensity (Just Blue) (Just Yellow)
  _ -> resetColorSchemeT

setColorT :: MonadIO m => Color -> m ()
setColorT color = setColorSchemeT Vivid NormalIntensity (Just color) Nothing

setColorSchemeT :: (MonadIO m) => ColorIntensity -> ConsoleIntensity -> Maybe Color -> Maybe Color -> m ()
setColorSchemeT colorIntensity consoleIntensity mColor1 mColor2 = liftIO $ do
  ifJust mColor1 $ setSGR [SetColor Foreground colorIntensity (fromJust mColor1)]
  ifJust mColor2 $ setSGR [SetColor Background colorIntensity (fromJust mColor2)]
  setSGR [SetConsoleIntensity consoleIntensity]

resetColorSchemeT :: MonadIO m => m ()
resetColorSchemeT = liftIO $ setSGR [Reset]