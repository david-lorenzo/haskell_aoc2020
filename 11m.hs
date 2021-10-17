--{-# LANGUAGE FlexibleContexts #-}
import System.Environment
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.ST


data Seat = E | B | F deriving (Show, Eq)

charToSeat 'L' = E
charToSeat '#' = B
charToSeat '.' = F

seatToChar E = 'L'
seatToChar B = '#'
seatToChar F = '.'

main = do
  inputFilePath <- head <$> getArgs
  inputLines <- lines <$> readFile inputFilePath
  let ml0 = map (map charToSeat) inputLines
  let row = head ml0
  c <- makeRow row
  d <- sequence $ map makeRow ml0
  writeCell d 0 0 F
  e <- MV.unsafeRead (d !! 0) 0
  print e
  return ()

readCell  grid y x   = MV.unsafeRead  (grid !! y) x
writeCell grid y x v = MV.unsafeWrite (grid !! y) x v

makeRow r = let n = length r
  in runST $ do
    v <- MV.new n
    fill v 0 r
  where fill v i [] = do return v
        fill v i (r:rs) = do { GM.unsafeWrite v i r; fill v (i+1) rs }

