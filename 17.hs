import System.Environment
import qualified Data.Map.Strict as M
import Data.Hashable
import Data.Bool
import Control.Monad

data Range = Range Int Int deriving (Eq, Show)

range (Range mn mx) = [mn..mx]

expand (Range mn mx) = Range (mn-1) (mx+1)

conway 1 2 = 1
conway _ 3 = 1
conway _ _ = 0

get :: (Ord k) => k -> M.Map k Int -> Int
get = M.findWithDefault 0

neighbors (x, y, z) cube =
  sum [ get (ix, iy, iz) cube | ix <- [(x-1), x, (x+1)]
                              , iy <- [(y-1), y, (y+1)]
                              , iz <- [(z-1), z, (z+1)]
                              , (x, y, z) /= (ix, iy, iz) ]

run1 0 cube _ _ _ = cube
run1 n cube rx ry rz = run1 (n-1) cube' nrx nry nrz where
  nrx = expand rx
  nry = expand ry
  nrz = expand rz
  cube' = M.fromList [(k, 1) | x <- range rx
                             , y <- range ry
                             , z <- range rz
                             , let k = (x, y, z)
                             , 1 == conway (get k cube) (neighbors k cube)]

neighbors2 (x, y, w, z) cube =
  sum [ get (ix, iy, iw, iz) cube | ix <- [(x-1), x, (x+1)]
                                  , iy <- [(y-1), y, (y+1)]
                                  , iw <- [(w-1), w, (w+1)]
                                  , iz <- [(z-1), z, (z+1)]
                                  , (x, y, w, z) /= (ix, iy, iw, iz) ]


run2 0 cube _ _ _ _ = cube
run2 n cube rx ry rw rz = run2 (n-1) cube' nrx nry nrw nrz where
  nrx = expand rx
  nry = expand ry
  nrw = expand rw
  nrz = expand rz
  cube' = M.fromList [(k, 1) | x <- range rx
                             , y <- range ry
                             , w <- range rw
                             , z <- range rz
                             , let k = (x, y, w, z)
                             , 1 == conway (get k cube) (neighbors2 k cube)]

{-
printCube cube = do
  let ks = M.keys cube
  let rx0 = minimum . map (\(x, y, z) -> x) $ ks
  let rx1 = maximum . map (\(x, y, z) -> x) $ ks
  let ry0 = minimum . map (\(x, y, z) -> y) $ ks
  let ry1 = maximum . map (\(x, y, z) -> y) $ ks
  let rz0 = minimum . map (\(x, y, z) -> z) $ ks
  let rz1 = maximum . map (\(x, y, z) -> z) $ ks
  forM_ [rz0..rz1] (\iz -> do
      putStrLn $ "z = " ++ (show iz)
      mapM_ putStrLn [[bool '.' '#' ((get (ix, iy, iz) cube) == 1) | ix <- [rx0..rx1] ] | iy <- [ry0..ry1]]
    )
  return ()
  -}

mkRange n acs = expand (Range mnr mxr) where
  mnr = minimum $ map f acs
  mxr = maximum $ map f acs
  f = case n of
        0 -> ((\(a, _, _) -> a) . fst)
        1 -> ((\(_, b, _) -> b) . fst)


main = do
  inputFilePath <- head <$> getArgs
  inputLines <- lines <$> readFile inputFilePath
  let acs = [((x, y, 0), 1) | (y, l) <- zip [0..] inputLines
                            , (x, c) <- zip [0..] l
                            , c == '#']

  let acs2 = [((x, y, 0, 0), 1) | (y, l) <- zip [0..] inputLines
                                , (x, c) <- zip [0..] l
                                , c == '#']

  let cube  = M.fromList acs
  let cube2 = M.fromList acs2

  let rx    = mkRange 0 acs
  let ry    = mkRange 1 acs
  let rw    = Range (-1) 1
  let rz    = Range (-1) 1

  let res15 = run1 6 cube rx ry rz
  let res1 = M.size res15
  putStrLn . ("Part 1: " ++) . show $ res1

  let res25 = run2 6 cube2 rx ry rw rz
  let res2 = M.size res25
  putStrLn . ("Part 2: " ++) . show $ res2

