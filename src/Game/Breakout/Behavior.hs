{-# LANGUAGE NamedFieldPuns #-}

module Game.Breakout.Behavior
    (
      updateWorld
    ) where

import Game.Breakout.Types

updateWorld :: World -> World
updateWorld World{paddle, ball, brect} = World {paddle = p, ball = b, brect}
  where p = updatePaddle paddle
        b = updateBall ball brect

updatePaddle = id

updateBall Ball{pos', vel, radius} brect = Ball{pos' = (x', y'), vel = (vx', vy'), radius}
  where (tx, ty) = updatePos pos' vel
        (x', y') = enforceBounds radius (tx, ty) brect
        vx'      = if x' == tx then fst vel else negate $ fst vel
        vy'      = if y' == ty then snd vel else negate $ snd vel

updatePos (x, y) (vx, vy) =  (x + vx, y + vy)

enforceBounds radius (x, y) (width, height) = (x', y')
  where x'  = max radius $ min x $ width - radius
        y'  = max radius $ min y $ height - radius
