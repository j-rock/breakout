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
    drawPaddle window (paddle world) (brect world)
    G.display window

drawBall window Ball{pos', radius, ..} brect = do
    circle <- G.createCircleShape
    let fRad = double2Float radius
    G.setRadius circle fRad
    G.setOrigin circle $ Vec2f fRad fRad
    G.setFillColor circle $ Color 0 255 0 255
    pxCoords <- gameCoords2PixelCoords window brect pos'
    G.setPosition circle pxCoords
    G.drawCircle window circle Nothing

drawPaddle window Paddle{pos, width, height, ..} brect = do
    rect <- G.createRectangleShape
    let (fWidth, fHeight) = (double2Float width, double2Float height)
    G.setSize rect $ Vec2f fWidth fHeight
    G.setOrigin rect $ Vec2f (fWidth / 2) (fHeight / 2)
    G.setFillColor rect $ Color 0 125 255 255
    pxCoords <- gameCoords2PixelCoords window brect pos
    G.setPosition rect pxCoords
    G.drawRectangle window rect Nothing


gameCoords2PixelCoords window (bWidth, bHeight) (x, y) = do
    Vec2u w h <- G.getWindowSize window
    let (width, height) = (fromIntegral w, fromIntegral h)
        x'              = double2Float $ width * x / bWidth
        y'              = double2Float $ height * (1.0 - y / bHeight)
    return $ Vec2f x' y'
