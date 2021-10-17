import System.Environment
import Data.List (sort, foldl')
import qualified Data.Vector as V

main = do
  inputFilePath <- head <$> getArgs
  numbers <- sort . (\xs -> [0, maximum(xs)+3] ++ xs) . map read . lines <$> readFile inputFilePath
  let res1 = (uncurry (*)) . foldl' onesAndThrees (0, 0) $ zipWith (-) (tail numbers) numbers
  putStrLn . ("Part 1: " ++) . show $ res1
  let numVec = V.fromList numbers
  let lenNumbers = V.length numVec
  let accVec = (V.replicate lenNumbers 0) V.// [(0, 1)]
  let res2 = V.last $ run numVec accVec lenNumbers 1
  putStrLn . ("Part 2: " ++) . show $ res2

onesAndThrees :: (Int, Int) -> Int -> (Int, Int)
onesAndThrees p@(ones, threes) x = case x of
    1 -> (succ ones, threes)
    3 -> (ones, succ threes)
    _ -> p

run numVec accVec end i =
  let accVec' = accVec V.// [(i, sum ns)]
      ns = [accVec V.! x | x <- [i-3, i-2, i-1]
                         , x >= 0
                         , (numVec V.! i) - (numVec V.! x) <= 3]
  in if i >= end
        then accVec
        else run numVec accVec' end (succ i)

