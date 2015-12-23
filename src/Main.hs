import Data.Time.Clock
import Reflex.Dom.Widget.Screen.Test (screenWidgetTestApp)
import Reflex.Dom (mainWidget)

-- This will continuously render some images on JavaScript, using the CPU.
main = do
    startTime <- getCurrentTime
    mainWidget $ do
        screenWidgetTestApp startTime True  128 128
        screenWidgetTestApp startTime False 128 128
