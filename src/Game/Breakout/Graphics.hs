{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Breakout.Graphics
    (
      renderWorld
    ) where

import Game.Breakout.Types

import GHC.Float (double2Float)
import Control.Monad.SFML
import qualified Control.Monad.SFML.Graphics as G
import SFML.Graphics (RenderWindow, renderStates)
import SFML.Graphics.Color
import SFML.System.Vector2 (Vec2u(..), Vec2f(..))

renderWorld :: World -> RenderWindow -> SFML ()
renderWorld world window = do
    G.clearRenderWindow window $ Color 0 0 0 255
    drawBall window (ball world) (brect world)
    G.display window

drawBall window Ball{pos', radius, ..} brect = do
    pxCoords <- gameCoords2PixelCoords window brect pos'
    circle <- G.createCircleShape
    G.setRadius circle $ double2Float radius
    G.setOrigin circle pxCoords
    G.setPosition circle pxCoords
    G.setFillColor circle $ Color 0 255 0 255
    G.drawCircle window circle Nothing

gameCoords2PixelCoords window (bWidth, bHeight) (x, y) = do
    Vec2u w h <- G.getWindowSize window
    let (width, height) = (fromIntegral w, fromIntegral h)
        x' = double2Float $ width * bWidth / x
        y' = double2Float $ height * (1.0 - bHeight / y)
    return $ Vec2f x' y'
