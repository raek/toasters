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
            vec 0 = ( s,  0)
            vec 1 = ( 0,  s)
            vec 2 = (-s,  0)
            vec 3 = ( 0, -s)

    turn :: Int -> Change
    turn a = Rotation (R a)

moveToChange' :: Move -> Pose -> Change
moveToChange' move pose = moveToChange move (dir pose)

type GameState = (Int, Pose, [Move])

fps = 50

main = graphicsLoop fps gameStep (1, startPose, moves)

startPose = Pose (P 4 3) north
moves = cycle [Move1, Move1, UTurn, Move2, TurnRight, Move1, BackUp, UTurn, TurnLeft]

gameStep :: GameState -> IO GameState
gameStep (tick, pose, moves) = do
  let (cycle, phase) = tick `divMod` fps
      (pose', moves') = if phase == 0
                        then perform pose moves
                        else (pose, moves)
      ongoingChange = moveToChange' (head moves') pose'
      phase' = (fromIntegral phase) / (fromIntegral fps)
      angle = negate $ phase' * 360
  grid 9 7 $ do
    object $ do
      inPose (Pose (P 0 0) north)
      Graphics.rotate angle
      robot black
    object $ do
      withPoseAndChange phase' pose' ongoingChange
      robot green
  return (tick + 1, pose', moves')

perform :: Pose -> [Move] -> (Pose, [Move])
perform pose []             = (pose, [])
perform pose (move : moves) = (pose', moves)
  where
    pose'  = apply change pose
    change = moveToChange' move pose
