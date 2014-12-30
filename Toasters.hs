module Toasters where

import Data.Ratio (Rational)
import Action
import Space
import Pretty
import GraphicsBoilerplate
import Graphics

moveToChange :: Move -> Direction -> Change
moveToChange move dir =
  case move of
    Move1     -> advance   1  dir
    Move2     -> advance   2  dir
    Move3     -> advance   3  dir
    BackUp    -> advance (-1) dir
    TurnLeft  -> turn 1
    UTurn     -> turn 2
    TurnRight -> turn 3
  where
    advance :: Int -> Direction -> Change
    advance s (D a) = Translation (T x y)
      where (x, y) = vec a
            vec 0 = ( 1,  0)
            vec 1 = ( 0,  1)
            vec 2 = (-1,  0)
            vec 3 = ( 0, -1)

    turn :: Int -> Change
    turn a = Rotation (R a)

moveToChange' :: Move -> Pose -> Change
moveToChange' move pose = moveToChange move (dir pose)

fps = 50

main = graphicsLoop fps draw 0

draw tick = do
  let x = (tick `div` fps) `mod` 7
  grid 9 7 $ do
    pose (Pose (P 3 3) west)  $ robot red
    pose (Pose (P 4 x) north) $ robot green
    pose (Pose (P 5 3) east)  $ robot blue
  return (tick + 1)
