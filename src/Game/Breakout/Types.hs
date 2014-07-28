{-# LANGUAGE NamedFieldPuns #-}

module Game.Breakout.Types where

import Prelude hiding (length)

data World = World
    { paddle :: Paddle
    , ball   :: Ball
    , brect  :: BoundingRectangle
    } deriving (Show)

type Position = (Double, Double)
type Velocity = (Double, Double)
type Size     = Double
type BoundingRectangle = (Size, Size)

data Paddle = Paddle
    { pos    :: Position
    , width  :: Size
    , height :: Size
    } deriving (Show)

data Ball = Ball
    { pos'   :: Position
    , vel    :: Velocity
    , radius :: Size
    } deriving (Show)

defaultWorld = World {paddle, ball, brect}
  where ball = defaultBall
        paddle = defaultPaddle
        brect  = defaultBounds

defaultPaddle = Paddle {pos, width, height}
  where pos    = (320.0, 5.1)
        width  = 55.0
        height = 3.1

defaultBall = Ball {pos', vel, radius}
  where pos'   = (320.0, 120.0)
        vel    = (-5.0, 5.0)
        radius = 5.5


defaultBounds = (640.0, 480.0)
