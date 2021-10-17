import System.Environment
import Data.List

data Action = N | S | E | W | L | R | F deriving (Show, Eq)
type Move = (Action, Int)
data Direction = North | South | East | West deriving (Show, Eq)
data Position = Position {posEast :: Int, posNorth :: Int, posFacing :: Direction} deriving (Show, Eq)

startPosition = Position {posEast=0, posNorth=0, posFacing=East}
wayPoint = Position {posEast=10, posNorth=1, posFacing=North}

parseAction :: Char -> Action
parseAction 'N' = N
parseAction 'S' = S
parseAction 'E' = E
parseAction 'W' = W
parseAction 'L' = L
parseAction 'R' = R
parseAction 'F' = F

parseMove :: String -> Move
parseMove (x:xs) = (parseAction x, read xs)

rotate face  R angle = rotate face L (360 - angle)
rotate North L 90  = West
rotate North L 180 = South
rotate North L 270 = East
rotate South L 90  = East
rotate South L 180 = North
rotate South L 270 = West
rotate West  L 90  = South
rotate West  L 180 = East
rotate West  L 270 = North
rotate East  L 90  = North
rotate East  L 180 = West
rotate East  L 270 = South

sine 0   =  0
sine 90  =  1
sine 180 =  0
sine 270 = -1
sine angle = (-1) * sine ((-1)*angle) -- negative angles

cosine 0   =  1
cosine 90  =  0
cosine 180 = -1
cosine 270 =  0
cosine angle = cosine ((-1)*angle) -- negative angles

applyCmd (position@Position{posEast=east, posNorth=north, posFacing=facing}) (action, parameter) =
  case action of
    N -> position { posNorth = north + parameter }
    S -> position { posNorth = north - parameter }
    E -> position { posEast = east + parameter }
    W -> position { posEast = east - parameter }
    L -> position { posFacing = rotate facing action parameter }
    R -> position { posFacing = rotate facing action parameter }
    F -> case facing of
            North -> position { posNorth = north + parameter }
            South -> position { posNorth = north - parameter }
            East  -> position { posEast = east + parameter }
            West  -> position { posEast = east - parameter }

rotateWaypoint :: Position -> Action -> Int -> Position
rotateWaypoint position turn angle = position {posEast = newEast, posNorth=newNorth} where
  (east, north) = (posEast position, posNorth position)
  theta = if turn == L then angle else (-1)*angle
  newEast  = east * (cosine theta) - north * (sine theta)
  newNorth = east * (sine theta)   + north * (cosine theta)

applyCmd2 (waypoint, ship) (action, parameter) =
  let (weast, wnorth) = (posEast waypoint, posNorth waypoint)
      (seast, snorth) = (posEast ship    , posNorth ship)
      newShipEast  = seast  + parameter * weast
      newShipNorth = snorth + parameter * wnorth
  in case action of
       N -> (waypoint { posNorth = wnorth + parameter }, ship)
       S -> (waypoint { posNorth = wnorth - parameter }, ship)
       E -> (waypoint { posEast = weast + parameter }, ship)
       W -> (waypoint { posEast = weast - parameter }, ship)
       L -> (rotateWaypoint waypoint action parameter, ship)
       R -> (rotateWaypoint waypoint action parameter, ship)
       F -> (waypoint, (ship { posEast = newShipEast, posNorth = newShipNorth }))

manhattan Position{posEast=east, posNorth=north} = abs(east) + abs(north)

main = do
  inputFilePath <- head <$> getArgs
  inputLines <- lines <$> readFile inputFilePath
  let moves = map parseMove inputLines
  --mapM print moves
  let res1 = foldl' applyCmd startPosition moves
  putStrLn. ("Part 1: " ++) . show . manhattan $ res1
  let res2 = snd $ foldl' applyCmd2 (wayPoint, startPosition) moves
  putStrLn. ("Part 2: " ++) . show . manhattan $ res2
  return ()
