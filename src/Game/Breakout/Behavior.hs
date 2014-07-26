module Game.Breakout.Behavior
    (
      updateWorld
    ) where

import Game.Breakout.Types

updateWorld :: World -> World
updateWorld world = World {paddle = p, ball = b}
  where
    p = updatePaddle $ paddle world
    b = updateBall $ ball world

updatePaddle = undefined
updateBall = undefined
