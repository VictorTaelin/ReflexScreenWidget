{-# LANGUAGE ScopedTypeVariables #-}

import Data.Time.Clock
import Reflex.Dom (mainWidget, constant, MonadWidget)
import Reflex.Dom.Widget.Screen (screenWidget, ByteImageRgba(ByteImageRgba))
import Reflex.Dom.Widget.Screen.Test (screenWidgetTestApp)
import qualified Data.ByteString as BS

-- The screen widget renders a ByteImageRgba dynamically.
-- To create a ByteImageRgba, we need a width, a height,
-- and a ByteString buffer with its color information, like
-- this: `B.pack [R,G,B,A, R,G,B,A, ...]`. That is, the
-- length of the buffer is `width * height * 4`.

-- This is a 128x128 screen widget that is always blue:
blueScreen :: forall t m . MonadWidget t m => m ()
blueScreen = do
    let width  = 128
    let height = 128
    let blue   = [0, 0, 255, 255] -- [Red, Green, Blue, Alpha]
    let buffer = BS.pack $ concat $ replicate (width*height) blue
    let image  = ByteImageRgba width height buffer
    _ <- screenWidget $ constant image
    return ()

-- Starts an widget with 3 screens.
main :: IO ()
main = do
    startTime <- getCurrentTime
    mainWidget $ do
        blueScreen
        screenWidgetTestApp startTime True  128 128
        screenWidgetTestApp startTime False 128 128