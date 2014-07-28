{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)

import Control.Monad.SFML (runSFML)
import qualified Control.Monad.SFML.System as S
import qualified Control.Monad.SFML.Graphics as G
import SFML.Graphics (RenderWindow)
import SFML.System.Time (Time, microseconds, asSeconds)
import qualified SFML.Window as W

import Game.Breakout.Behavior (updateWorld)
import Game.Breakout.Types (defaultWorld)
import Game.Breakout.Graphics (renderWorld)

main :: IO ()
main = runSFML $ do
    context <- defaultContext
    gameloop context defaultWorld

data GameContext = GameContext
    { window :: RenderWindow
    , clock  :: W.Clock
    , delay  :: Time
    }

defaultContext = do
    let settings = Just $ W.ContextSettings 24 8 0 1 2
        delay    = fps2Micro 60
    window <- G.createRenderWindow
        (W.VideoMode 640 480 24)
        "Breakout"
        [W.SFDefaultStyle]
        settings
    clock <- S.createClock
    return $ GameContext {window, clock, delay}
  where fps2Micro = microseconds . floor . (* 1000000) . (1 /) . fromIntegral

gameloop context world = unlessClose (window context) $ do
    sleepMinDelay (clock context) (delay context)
    renderWorld world $ window context
    let w' = updateWorld world
    gameloop context w'

unlessClose window action = do
    event <- G.pollEvent window
    case event of
        Just W.SFEvtClosed -> return ()
        _                  -> action

sleepMinDelay clock delay = do
    elapsed <- S.restartClock clock
    S.sfSleep $ max 0 $ delay - elapsed
    S.restartClock clock
    return ()
