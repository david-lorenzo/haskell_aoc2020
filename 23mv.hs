{-# LANGUAGE NumericUnderscores #-}
import System.Environment
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

pairSeq xs = go xs
  where go (x:[])   = [(x, head xs)]
        go (x:y:zs) = (x,y) : go (y:zs)

{-# INLINE decr #-}
decr n 1 = n
decr n i = (i-1)

{-# INLINE getPred #-}
getPred n x x1 x2 x3 =
  let p1x = decr n x
      p2x = decr n p1x
      p3x = decr n p2x
      p4x = decr n p3x
  in if p1x /= x1 && p1x /= x2 && p1x /= x3
       then p1x
       else if p2x /= x1 && p2x /= x2 && p2x /= x3
              then p2x
              else if p3x /= x1 && p3x /= x2 && p3x /= x3
                     then p3x
                     else p4x

vec2list' :: Int -> Int -> V.Vector Int -> [Int]
vec2list' 1 _ _ = []
vec2list' n i v = let next = v V.! i
                  in next:(vec2list' (n-1) next v)

vec2list :: Int -> V.Vector Int -> [Int]
vec2list i v = vec2list' (V.length v) i v

run :: Int -> Int -> [Int] -> V.Vector Int
run n len xs = runST (do
    v <- MV.new (len + 1)
    MV.write v 0 (head xs) -- element at 0 is the current element
    forM_ (pairSeq xs)  (\(i,x) -> do MV.write v i x)
    forM_ [1..n] (\_ -> do
          c  <- MV.read v 0
          c1 <- MV.read v c
          c2 <- MV.read v c1
          c3 <- MV.read v c2
          c4 <- MV.read v c3
          let dest = getPred len c c1 c2 c3
          dest1 <- MV.read v dest
          MV.write v c3 dest1
          MV.write v dest c1
          MV.write v c c4
          MV.write v 0 c4)
    V.freeze v)


main = do
  let cups = [5, 9, 8, 1, 6, 2, 7, 3, 4]
  --let cups = [3, 8, 9, 1, 2, 5, 4, 6, 7] -- demo input
  let len = length cups
  let res1 = run 100 len cups
  let res1l = vec2list 1 res1
  putStrLn . ("Part 1:   " ++) . concatMap show . init $ res1l
  putStrLn ""

  -- Part 2
  let len2 = 1_000_000
  let cups2 = cups ++ [10..len2]
  let res2 = run 10_000_000 len2 cups2
  let m1 = res2 V.! 1
  let m2 = res2 V.! m1
  putStrLn . ("Part 2:   " ++) . show $ m1 * m2

