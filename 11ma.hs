import System.Environment
import Data.Array.ST
import Data.Array
import Data.STRef


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
  let a = runSTArray $ do
      arr <- newListArray (0, length row - 1) row
      return arr
  print a
  let b = runSTArray $ do
      let limits = (length ml0 - 1, length (head ml0) - 1)
      arrb <- newArray ((0,0), limits) F
      writeArray arrb (1, 0) B
      return arrb
  print $ elems b




