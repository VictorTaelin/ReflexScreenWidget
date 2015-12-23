{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, JavaScriptFFI, CPP #-}

module Reflex.Dom.Widget.Screen where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import GHCJS.DOM.Types (unElement, toElement)
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.Types (JSVal)
import Reflex.Dom
import Reflex.Dom.AnimationFrame (animationFrame)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Unsafe as B (unsafeUseAsCString)
import qualified Data.Map as M (Map, empty)

#ifdef __GHCJS__
foreign import javascript unsafe 
    -- Arguments
    --    canvas : JSHtmlElementCanvas
    --    width  : JSNumber
    --    height : JSNumber
    --    pixels : Ptr a -- Pointer to a ByteString in the format below
    "(function(){                                                     \
        var cvs    = $1;                                              \
        var width  = $2;                                              \
        var height = $3;                                              \
        var pixels = new Uint8ClampedArray($4.u8);                    \
        cvs.width  = width;                                           \
        cvs.height = height;                                          \
        var ctx    = cvs.getContext('2d');                            \
        ctx.putImageData(new ImageData(pixels, width, height), 0, 0); \
    })()" 
    blit :: forall a . JSVal -> JSVal -> JSVal -> Ptr a -> IO ()
#endif

-- | The image format used to render to the canvas. Each byte of the buffer
-- represents a color channel from 0~255, in the following format:
-- [0xRR,0xGG,0xBB,0xAA, 0xRR,0xGG,0xBB,0xAA...]. The length of the ByteString
-- must, thus, be equal to `width * height * 4`. This unsafely casts the
-- ByteString to a C Ptr that will be used directly on the JS blitting
-- function. I don't know how safe this is.
data ByteImageRgba = ByteImageRgba { 
    _width  :: Int,
    _height :: Int,
    _buffer :: B.ByteString}

-- | Renders a dynamic ByteImageData using a Canvas. The canvas is refreshed
--   at every animation frame of the browser. Returns the canvas.
screenWidgetAttr :: forall t m . MonadWidget t m => M.Map String String -> Behavior t ByteImageRgba -> m (El t)
screenWidgetAttr attrs imageBehavior = do

    -- Creates the canvas element on which we will render
    (canvasEl,event) <- elAttr' "canvas" attrs (text "")

    -- Gets the proper GHCJS's JSVal of the canvas
    let canvasJS = unElement.toElement._el_element $ canvasEl

    -- IO action that will draw our pixels to the canvas 
    let draw :: ByteImageRgba -> IO ()
        draw (ByteImageRgba width height pixelByteString) = do
            B.unsafeUseAsCString pixelByteString $ \ ptr -> do
                blit canvasJS (pToJSVal width) (pToJSVal height) ptr

    -- Redraws the canvas whenever the window is ready to render a frame.
    animator <- animationFrame
    performEvent_ $ liftIO . draw <$> tag imageBehavior animator

    return canvasEl

-- | Same as above, without the Attr argument.
screenWidget :: forall t m . MonadWidget t m => Behavior t ByteImageRgba -> m (El t)
screenWidget = screenWidgetAttr M.empty
