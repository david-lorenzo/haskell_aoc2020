{-# LANGUAGE BangPatterns #-}

import System.Environment
import Data.Bool
import Data.List
import Data.List.Split
import qualified Data.Set as S

calc_result len acc idx val = acc + val*(len - idx)

play1 :: [Int] -> [Int] -> [Int]
play1 p1 [] = p1
play1 [] p2 = p2
play1 (x:xs) (y:ys) = if x > y
                        then play1 (xs ++ [x,y]) ys
                        else play1 xs (ys ++ [y,x])


rplay !mem p1 p2 | S.member (p1, p2) mem = (p1, [])
                | (null p1 || null p2)  = (p1, p2)
                | otherwise             = rplay newmem newp1 newp2
  where (h1:t1) = p1
        (h2:t2) = p2
        r1          = h1 <= length t1
        r2          = h2 <= length t2
        recurse     = r1 && r2
        (q1, q2)    = rplay S.empty (take h1 t1) (take h2 t2)
        p1wr        = not . null $ q1
        p1wnr       = h1 > h2
        p1wins      = bool p1wnr p1wr recurse
        newp1       = bool t1 (t1 ++ [h1, h2]) p1wins
        newp2       = bool t2 (t2 ++ [h2, h1]) (not p1wins)
        newmem      = S.insert (p1, p2) mem

main = do
  inputFilePath <- head <$> getArgs
  [inp1, inp2] <- splitOn "\n\n" <$> readFile inputFilePath
  let p1 = map read . tail . lines $ inp1
  let p2 = map read . tail . lines $ inp2

  -- Part 1
  let lres1 = play1 p1 p2
  let len1  = length lres1
  let res1  = foldl' (\a (n, v) -> a + n*v) 0 (zip [len1, len1-1..] lres1)
  putStrLn . ("Part 1: " ++) . show $ res1

  -- Part 2
  let (l1, l2) = rplay S.empty p1 p2
  let lres2 = bool l1 l2 (null l1)
  let len2  = length lres2
  let res2  = foldl' (\a (n, v) -> a + n*v) 0 (zip [len2, len2-1..] lres2)
  putStrLn . ("Part 1: " ++) . show $ res2
