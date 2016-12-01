data Direction = TurnLeft | TurnRight | Straight deriving (Show, Eq)
data Point = Point{
    px :: Int,
    py :: Int
    } deriving (Show)

calcTurn :: Point -> Point -> Point -> Direction
calcTurn a b c = 
    if area == 0 then Straight
    else if area > 0 then TurnLeft
         else TurnRight
    where
        area = (px b-px a)*(py c-py a)-(py b-py a)*(px c-px a)