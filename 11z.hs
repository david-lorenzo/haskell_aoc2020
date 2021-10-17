import System.Environment
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad

rows = 91
cols = 99

maxY = rows - 1
maxX = cols - 1

data Seat = E | B | F deriving (Show, Eq)

charToSeat 'L' = E
charToSeat '#' = B
charToSeat '.' = F

seatToChar E = 'L'
seatToChar B = '#'
seatToChar F = '.'

index y x = y * cols + x
cell mtx y x = mtx V.! (index y x)

rule1 :: Seat -> Int -> Seat
rule1 value nbs = case value of
                    F -> F
                    E -> if nbs == 0 then B else E
                    B -> if nbs >= 4 then E else B

rule2 :: Seat -> Int -> Seat
rule2 value nbs = case value of
                    F -> F
                    E -> if nbs == 0 then B else E
                    B -> if nbs >= 5 then E else B

conwayCell mtx i j = rule1 (cell mtx i j) bns
  where bns = length . filter (== B) $ ns
        ns = [cell mtx y x | y <- [i-1, i, i+1]
                           , x <- if y == i then [j-1, j+1] else [j-1, j, j+1]
                           , y >= 0, y < rows
                           , x >= 0, x < cols]


conwayCell2 mtx i j = rule2 (cell mtx i j) bns
  where lineOfSight []     = []
        lineOfSight (F:xs) = lineOfSight xs
        lineOfSight (x:_)  = [x]
        n  = lineOfSight [cell mtx y j | y <- [(i-1),(i-2)..0]]
        s  = lineOfSight [cell mtx y j | y <- [(i+1),(i+2)..maxY]]
        w  = lineOfSight [cell mtx i x | x <- [(j-1),(j-2)..0]]
        e  = lineOfSight [cell mtx i x | x <- [(j+1),(j+2)..maxX]]
        nw = lineOfSight [cell mtx y x | (y, x) <- zip [(i-1),(i-2)..0]    [(j-1),(j-2)..0] ]
        ne = lineOfSight [cell mtx y x | (y, x) <- zip [(i-1),(i-2)..0]    [(j+1),(j+2)..maxX] ]
        sw = lineOfSight [cell mtx y x | (y, x) <- zip [(i+1),(i+2)..maxY] [(j-1),(j-2)..0] ]
        se = lineOfSight [cell mtx y x | (y, x) <- zip [(i+1),(i+2)..maxY] [(j+1),(j+2)..maxX] ]
        bns = length . filter (==B) $ concat [n, s, w, e, nw, ne, sw, se]

--conwayMatrix conway mtx = createMatrix [[conway mtx y x | x <- [0..maxX]] | y <- [0..maxY]]

conwayMatrix conway mtx = runST $ do
  nmtx <- MV.new $ V.length mtx
  forM_ [0..maxY] (\y -> 
    forM_ [0..maxX] (\x -> do
      MV.write nmtx (index y x) (conway mtx y x)))
  V.freeze nmtx

printValue grid y x = do putChar $ seatToChar $ cell grid y x
printRow grid y = mapM_ (\x -> printValue grid y x) [0..maxX]
printGrid grid  = mapM_ (\y -> printRow grid y >> putChar '\n') [0..maxY]

main = do
  inputFilePath <- head <$> getArgs
  let inputFilePath = "../input11.txt"
  --let inputFilePath = "input11.txt"
  inputLines <- lines <$> readFile inputFilePath
  let m0 = V.fromList . map charToSeat . concat $ inputLines
  --printGrid (conwayMatrix conwayCell m0)
  -- Part 1
  let states = iterate (conwayMatrix conwayCell) m0
  let state1 = fst . head . filter (uncurry (==)) $ zip states (tail states)
  let res1 = V.foldl' (\acc b -> if b == B then acc+1 else acc) 0 state1
  putStrLn . ("Part 1: " ++) . show $ res1
  -- Part 2
  let states2 = iterate (conwayMatrix conwayCell2) m0
  let state2 = fst . head . filter (uncurry (==)) $ zip states2 (tail states2)
  let res2 = V.foldl' (\acc b -> if b == B then acc+1 else acc) 0 state2
  putStrLn . ("Part 2: " ++) . show $ res2
--   let m1 = conwayMatrix conwayCell2 m0
--   let l1 = map (map seatToChar . V.toList) $ V.toList m1
--   let convert = map (map seatToChar . V.toList) . V.toList 
--   mapM_ (\x -> do mapM_ putStrLn x >> putStrLn "") $ take 3 $ map convert states2

