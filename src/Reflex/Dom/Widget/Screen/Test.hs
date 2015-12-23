{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

module Reflex.Dom.Widget.Screen.Test where

import Control.Monad (void)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Reflex.Dom (MonadWidget, Event, performEventAsync, getPostBuild, TickInfo, tickLossy, hold, mainWidget, _tickInfo_n)
import Reflex.Dom.Widget.Screen (screenWidget, ByteImageRgba, ByteImageRgba(ByteImageRgba))
import qualified Data.ByteString as B

-- | A test application that will use screenWidget.
screenWidgetTestApp :: forall t m . (MonadWidget t m) => UTCTime -> Bool -> Int -> Int -> m ()
screenWidgetTestApp startTime renderWaves width height = void $ do

    -- The initial image is just white.
    let initImage :: ByteImageRgba
        initImage = ByteImageRgba width height $ B.replicate (width*height*4) 0xFF

    -- Renders a trippy sine wave image's pixel
    let sineWave :: Double -> Int -> Word8
        sineWave t byteIdx = col where
            col  :: Word8
            col  = case mod byteIdx 4 of
                        0 -> floor $ 255*(0.5+sin (fi dx/(7 + cos t * 3)+t*5)/2)
                        1 -> floor $ 255*(0.5+sin (fi dy/5+sin t * 7)/2)
                        2 -> floor $ 255*(0.75+sin (dist/2+(9+cos t * 3))/4)
                        otherwise -> 255
            fi   = fromIntegral
            cx   = div width 2  :: Int
            cy   = div height 2 :: Int
            dist = sqrt . fi $ dx*dx + dy*dy :: Double
            dx   = x-cx
            dy   = y-cy
            x    = mod idx width
            y    = div idx height
            idx  = div byteIdx 4

    -- Renders a varying brightness image's pixel
    let brightness :: Double -> Int -> Word8
        brightness t _ = mod (floor (t*60)) 255

    -- Renders a sine-wave image that varies in function of time.
    let genImage :: (Double -> Int -> Word8) -> Double -> ByteImageRgba
        genImage genPixel t 
            = ByteImageRgba width height 
            $ fst
            $ B.unfoldrN (width*height*4) (\ i -> Just (genPixel t i, i+1)) (0::Int) 

    -- Choose the renderer to use. The `brightness is much faster than `sineWave`.
    let renderer :: Double -> Int -> Word8
        renderer = if renderWaves then sineWave else brightness

    -- Creates an image signal that will be re-rendered 60 times per second.
    ticker        <- tickLossy (1/60) startTime
    imageBehavior <- hold initImage $ (genImage renderer.(/60).fromIntegral._tickInfo_n) <$> ticker

    -- Starts the screen widget with that image
    screenWidget imageBehavior


