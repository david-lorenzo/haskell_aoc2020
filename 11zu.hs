import System.Environment
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad

rows = 91
cols = 99

maxY = rows - 1
maxX = cols - 1

vE = 'L'
vB = '#'
vF = '.'

index y x = y * cols + x
cell mtx y x = mtx V.! (index y x)

rule1 value nbs = if value == vE && nbs == 0
                    then vB
                    else if value == vB && nbs >= 4
                            then vE
                            else value

rule2 value nbs = if value == vE && nbs == 0
                    then vB
                    else if value == vB && nbs >= 5
                            then vE
                            else value

conwayCell mtx i j = rule1 (cell mtx i j) bns
  where bns = length . filter (== vB) $ ns
        ns = [cell mtx y x | y <- [i-1, i, i+1]
                           , x <- if y == i then [j-1, j+1] else [j-1, j, j+1]
                           , y >= 0, y < rows
                           , x >= 0, x < cols]


conwayCell2 mtx i j = rule2 (cell mtx i j) bns
  where lineOfSight []     = []
        lineOfSight (x:xs) = if x == vF then lineOfSight xs else [x]
        n  = lineOfSight [cell mtx y j | y <- [(i-1),(i-2)..0]]
        s  = lineOfSight [cell mtx y j | y <- [(i+1),(i+2)..maxY]]
        w  = lineOfSight [cell mtx i x | x <- [(j-1),(j-2)..0]]
        e  = lineOfSight [cell mtx i x | x <- [(j+1),(j+2)..maxX]]
        nw = lineOfSight [cell mtx y x | (y, x) <- zip [(i-1),(i-2)..0]    [(j-1),(j-2)..0] ]
        ne = lineOfSight [cell mtx y x | (y, x) <- zip [(i-1),(i-2)..0]    [(j+1),(j+2)..maxX] ]
        sw = lineOfSight [cell mtx y x | (y, x) <- zip [(i+1),(i+2)..maxY] [(j-1),(j-2)..0] ]
        se = lineOfSight [cell mtx y x | (y, x) <- zip [(i+1),(i+2)..maxY] [(j+1),(j+2)..maxX] ]
        bns = length . filter (==vB) $ concat [n, s, w, e, nw, ne, sw, se]

conwayMatrix conway mtx = runST $ do
  nmtx <- MV.replicate (V.length mtx) vF
  forM_ [0..maxY] (\y -> 
    forM_ [0..maxX] (\x -> do
      MV.write nmtx (index y x) (conway mtx y x)))
  V.freeze nmtx

printValue grid y x = do putChar $ cell grid y x
printRow grid y = mapM_ (\x -> printValue grid y x) [0..maxX]
printGrid grid  = mapM_ (\y -> printRow grid y >> putChar '\n') [0..maxY]

main = do
  inputFilePath <- head <$> getArgs
  let inputFilePath = "../input11.txt"
  --let inputFilePath = "input11.txt"
  inputLines <- lines <$> readFile inputFilePath
  let m0 = V.fromList . concat $ inputLines :: V.Vector Char
  -- Part 1
  let states = iterate (conwayMatrix conwayCell) m0
  let state1 = fst . head . filter (uncurry (==)) $ zip states (tail states)
  let res1 = V.foldl' (\acc b -> if b == vB then acc+1 else acc) 0 state1
  putStrLn . ("Part 1: " ++) . show $ res1
  -- Part 2
  let states2 = iterate (conwayMatrix conwayCell2) m0
  let state2 = fst . head . filter (uncurry (==)) $ zip states2 (tail states2)
  let res2 = V.foldl' (\acc b -> if b == vB then acc+1 else acc) 0 state2
  putStrLn . ("Part 2: " ++) . show $ res2
--   let m1 = conwayMatrix conwayCell2 m0
--   let l1 = map (map seatToChar . V.toList) $ V.toList m1
--   let convert = map (map seatToChar . V.toList) . V.toList 
--   mapM_ (\x -> do mapM_ putStrLn x >> putStrLn "") $ take 3 $ map convert states2

