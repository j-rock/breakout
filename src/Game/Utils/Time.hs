module Game.Utils.Time (doWithFPS) where

import Control.Concurrent (threadDelay)
import Data.Time.Units (getCPUTimeWithUnit, toMicroseconds, fromMicroseconds, Microsecond)

fps2Micros = fromMicroseconds . floor . (* 1000000) . (1 /) . fromIntegral

getCurrentTime = getCPUTimeWithUnit :: IO Microsecond

sleep = threadDelay . fromIntegral . toMicroseconds

doWithFPS action fps = do
    (r, timeTaken) <- time action
    sleep $ max 0 $ fps2Micros fps - timeTaken
    return r

time action = do
    start <- getCurrentTime
    r <- action
    end <- getCurrentTime
    return (r, end - start)

