module Pretty where

import Action
import Space

instance Show Action where
    show (Action p m) = show p ++ ": " ++ show m

instance Show Move where
    show Move1     = "Move 1"
    show Move2     = "Move 2"
    show Move3     = "Move 3"
    show BackUp    = "Back-Up"
    show TurnLeft  = "Turn Left"
    show UTurn     = "U-Turn"
    show TurnRight = "Turn Right"

instance Show Pose where
    show (Pose p d) = "facing " ++ show d ++ " at " ++ show p

instance Show Position where
    show (P x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Show Direction where
    show (D 0) = "E"
    show (D 1) = "N"
    show (D 2) = "W"
    show (D 3) = "S"
    show (D a) = "(D " ++ show a ++ ")"

instance Show Change where
    show (Translation t) = show t
    show (Rotation    r) = show r

instance Show Translation where
    show (T dx dy) = "translate by (" ++ show dx ++ ", " ++ show dy ++ ")"

instance Show Rotation where
    show (R da) =
        case da of
          0 -> deg 0
          1 -> deg 90
          2 -> deg 180
          3 -> deg 270
          n -> "(R " ++ show n ++ ")"
        where
          deg x = "rotate " ++ show x ++ " dregrees ccw"
