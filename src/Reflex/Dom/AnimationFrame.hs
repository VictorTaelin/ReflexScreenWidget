module Reflex.Dom.AnimationFrame where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import GHCJS.Concurrent (OnBlocked(ContinueAsync))
import JavaScript.Web.AnimationFrame (inAnimationFrame)
import Reflex.Dom (MonadWidget, Event, performEventAsync, getPostBuild)

-- | Event that is fired continuously using onRequestAnimateFrame,
--   starting after a specified event.
animationFrameFrom :: MonadWidget t m => Event t a -> m (Event t Double)
animationFrameFrom = performEventAsync . fmap (const (void . liftIO . loop 0)) where 
    loop d cb = inAnimationFrame ContinueAsync (`loop` cb) >> cb d

-- | Event that is fired continuously using onRequestAnimationFrame,
--   starting after all widgets are built.
animationFrame :: MonadWidget t m => m (Event t Double)
animationFrame = animationFrameFrom =<< getPostBuild
