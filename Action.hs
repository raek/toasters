module Action where

import Data.List (sort)

data Action = Action Priority Move
            deriving (Eq, Ord)

type Priority = Int

data Move = Move1 | Move2 | Move3 | BackUp | TurnLeft | UTurn | TurnRight
            deriving (Eq, Ord)

actions = sort (concatMap expand specs)
    where
      expand (move, start, count, step) =
          let prios = take count (iterate (+step) start)
              wrap prio = Action prio move
          in map wrap prios

      specs = [ (UTurn,     10,   6, 10)
              , (TurnLeft,  70,  18, 20)
              , (TurnRight, 80,  18, 20)
              , (BackUp,    430,  6, 10)
              , (Move1,     490, 18, 10)
              , (Move2,     670, 12, 10)
              , (Move3,     790,  6, 10)
              ]
