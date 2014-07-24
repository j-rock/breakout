module Main (main) where

import System.Environment (getArgs)
import Control.Monad (liftM2)
import Game.Utils.Time (doWithFPS)
--import Game.Breakout.Types.World (World(..), updateWorld, renderGraphics)

main :: IO ()
main = do
    gameloop "joey rulz"

gameloop world = do
    world' <- singleUpdate world `doWithFPS` 60
    gameloop world'

--singleUpdate = liftM2 (>>) renderGraphics updateWorld
singleUpdate = liftM2 (>>) putStrLn return
