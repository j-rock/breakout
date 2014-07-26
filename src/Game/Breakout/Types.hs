{-# LANGUAGE NamedFieldPuns #-}

module Game.Breakout.Types where

import Prelude (Double, Show)

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
    , length :: Size
    } deriving (Show)

data Ball = Ball
    { pos'   :: Position
    , vel    :: Velocity
    , radius :: Size
    } deriving (Show)

defaultWorld = World {paddle, ball, brect}
  where paddle = defaultPaddle
        ball   = defaultBall
        brect  = defaultBounds

defaultPaddle = Paddle {pos, length}
  where pos    = (50.0, 1.0)
        length = 15.0

defaultBall = Ball {pos', vel, radius}
  where pos'   = (50.0, 20.0)
        vel    = (0.0, 0.0)
        radius = 60.0

defaultBounds = (640.0, 480.0)
