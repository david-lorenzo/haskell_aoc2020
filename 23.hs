{-# LANGUAGE NumericUnderscores #-}
import System.Environment
import Data.Bool
import Data.List
import Data.Maybe
import Prelude hiding (pred)
import qualified Prelude as P

data Clock = Clock (Int, Int) Int deriving (Eq, Ord, Show)

succ :: Clock -> Clock
succ (Clock (mn, mx) i) = let ni = P.succ i
                   in if ni > mx
                      then (Clock (mn, mx) mn)
                      else (Clock (mn, mx) ni)


pred :: Clock -> Clock
pred (Clock (mn, mx) i) = let ni = P.pred i
                   in if ni < mn
                      then (Clock (mn, mx) mx)
                      else (Clock (mn, mx) ni)


run1 (x:xs) = zs ++ z:ys ++ zss ++ [x]
  where (ys, yss) = splitAt 3 xs
        a = head . dropWhile (flip elem ys) $ iterate pred (pred x)
        (zs, z:zss) = break (==a) yss

get (Clock (_,_) a) = a

main = do
  let cups = [5, 9, 8, 1, 6, 2, 7, 3, 4]
  --let cups = [3, 8, 9, 1, 2, 5, 4, 6, 7] -- demo input
  let n    = length cups
  let mx   = maximum cups
  let mn   = minimum cups
  let ccups  = map (Clock (mn, mx)) cups
  let it        = iterate run1 ccups
  let sol       = it !! 100 
  let splitting = break (==(Clock (mn, mx) 1)) sol
  let res1      = (\(xs, y:ys) -> ys ++ xs) splitting
  putStrLn . ("Part 1: " ++) . concatMap (show . get) $ res1

  -- Part 2 !!! This approach doesn't scale
--  let p2size = 1_000_000
--  let moves = 10_000_000
--  let cups2 = cups ++ [mx+1..p2size]
--  let ccups2 = map (Clock (mn, p2size)) cups2
--  let res2 = (\(xs, y:ys) -> ys ++ xs) $ break (==(Clock (mn, mx) 1)) $ (!! moves) $ iterate run1 ccups2
--  let [n1, n2] = take 2 res2
--  putStrLn . ("Part 2: " ++) . show $ get n1 * get n2

