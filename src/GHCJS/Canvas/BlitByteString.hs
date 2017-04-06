{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, JavaScriptFFI, CPP #-}

module GHCJS.Canvas.BlitByteString (blitByteString) where
--
-- Code recovered from old version of Screen.hs (before commit daac065 )
--
import Foreign.Ptr (Ptr)
import GHCJS.Types (JSVal)

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
    -- | Draw a Haskell ByteString to a JavaScript Canvas
    blitByteString :: forall a . JSVal -> JSVal -> JSVal -> Ptr a -> IO ()
#endif