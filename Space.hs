module Space where

data Pose      = Pose { pos :: Position, dir :: Direction }
data Position  = P Int Int
data Direction = D Int

data Change      = Translation Translation | Rotation Rotation
data Translation = T Int Int
data Rotation    = R Int

apply :: Change -> Pose -> Pose
apply (Translation t) (Pose p d) = Pose (translate t p) d
apply (Rotation    r) (Pose p d) = Pose p (rotate r d)

translate :: Translation -> Position -> Position
translate (T dx dy) (P x y) = P (x + dx) (y + dy)

rotate :: Rotation -> Direction -> Direction
rotate (R da) (D a) = D $ (a + da) `mod` 4

east  = D 0
north = D 1
west  = D 2
south = D 3
