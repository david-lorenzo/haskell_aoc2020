import System.Environment
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as M


main = do
  inputFilePath <- head <$> getArgs
  --let inputFilePath = "../input11.txt"
  let inputFilePath = "input11.txt"
  inputLines <- lines <$> readFile inputFilePath
  --let m0 = createMatrix $ map (map charToSeat) inputLines
  let ml0 = map (map charToSeat) inputLines
  m0 <- sequenceA $ initGrid ml0
  let m1 = V.fromList m0
  --c <- M.unsafeRead (m1 V.! 0) 0
  writeCell m1 0 0 B
  c <- readCell m1 0 0
  print $ c
  let r = m1 V.! 0
  c0 <- MV.unsafeRead r 0
  print c0
  let c1 = MV.read r 0
  print c1
  return ()
  
  -- Part 1
--   let states = iterate (conwayMatrix conwayCell) m0
--   let state1 = fst . head . filter (uncurry (==)) $ zip states (tail states)
--   let res1 = length . filter (==B) . concatMap V.toList . V.toList $ state1
--   putStrLn . ("Part 1: " ++) . show $ res1
  -- Part 2
--   let states2 = iterate (conwayMatrix conwayCell2) m0
--   let state2 = fst . head . filter (uncurry (==)) $ zip states2 (tail states2)
--   let res2 = length . filter (==B) . concatMap V.toList . V.toList $ state2
--   putStrLn . ("Part 2: " ++) . show $ res2
--   let m1 = conwayMatrix conwayCell2 m0
--   let l1 = map (map seatToChar . V.toList) $ V.toList m1
--   let convert = map (map seatToChar . V.toList) . V.toList 
--   mapM_ (\x -> do mapM_ putStrLn x >> putStrLn "") $ take 3 $ map convert states2


data Seat = E | B | F deriving (Show, Eq)

charToSeat 'L' = E
charToSeat '#' = B
charToSeat '.' = F

seatToChar E = 'L'
seatToChar B = '#'
seatToChar F = '.'

--mSize m = (V.length m, (V.length . V.head) m)

--createMatrix = V.fromList . map V.fromList

--cell mtx y x = (mtx V.! y) V.! x

--mSize m = (V.length m, rowApply basicLength (V.head m))

-- lengthHead g = do
--   r <- g V.! 0
--   length r

--readCell mtx y x   = basicUnsafeRead  (mtx V.! y) x
--mutCell  mtx y x v = basicUnsafeWrite (mtx V.! y) x v

readCell mtx y x = MV.unsafeRead (mtx V.! y) x
writeCell mtx y x v = MV.unsafeWrite (mtx V.! y) x v

initRow rs = do
  r <- MV.new (Prelude.length rs)
  mapM (\(i, v) -> do M.unsafeWrite r i v) (zip [0,1..] rs)
  return r

initGrid = map initRow 
   
-- conwayCell mtx i j = case cell mtx i j of
--                     F -> F
--                     E -> if bns == 0 then B else E
--                     B -> if bns >= 4 then E else B
--   where (height, width) = mSize mtx
--         bns = length . filter (== B) $ ns
--         ns = [cell mtx y x | y <- [i-1, i, i+1]
--                            , x <- if y == i then [j-1, j+1] else [j-1, j, j+1]
--                            , y >= 0, y < height
--                            , x >= 0, x < width]
-- 
-- 
-- conwayCell2 mtx i j = case cell mtx i j of
--                         F -> F
--                         E -> if bns == 0 then B else E
--                         B -> if bns >= 5 then E else B
--   where (height, width) = mSize mtx
--         (lastRowIdx, lastColIdx) = (height-1, width-1)
--         lineOfSight []     = []
--         lineOfSight (F:xs) = lineOfSight xs
--         lineOfSight (x:_)  = [x]
--         n  = lineOfSight [cell mtx y j | y <- [(i-1),(i-2)..0]]
--         s  = lineOfSight [cell mtx y j | y <- [(i+1),(i+2)..lastRowIdx]]
--         w  = lineOfSight [cell mtx i x | x <- [(j-1),(j-2)..0]]
--         e  = lineOfSight [cell mtx i x | x <- [(j+1),(j+2)..lastColIdx]]
--         nw = lineOfSight [cell mtx y x | (y, x) <- zip [(i-1),(i-2)..0]          [(j-1),(j-2)..0] ]
--         ne = lineOfSight [cell mtx y x | (y, x) <- zip [(i-1),(i-2)..0]          [(j+1),(j+2)..lastColIdx] ]
--         sw = lineOfSight [cell mtx y x | (y, x) <- zip [(i+1),(i+2)..lastRowIdx] [(j-1),(j-2)..0] ]
--         se = lineOfSight [cell mtx y x | (y, x) <- zip [(i+1),(i+2)..lastRowIdx] [(j+1),(j+2)..lastColIdx] ]
--         bns = length . filter (==B) $ concat [n, s, w, e, nw, ne, sw, se]
-- 
-- conwayMatrix conway mtx = createMatrix [[conway mtx y x | x <- [0..lastColIx]] | y <- [0..lastRowIx]]
--   where lastRowIx = V.length mtx - 1
--         lastColIx = (V.length $ V.head mtx) - 1
-- 
-- 
