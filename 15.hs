import System.Environment
import Data.List.Split
import Data.List
import qualified Data.IntMap.Strict as M

type State = (M.IntMap Int, Int)

run1 :: State -> Int -> State
run1 (allseq, lastn) i = case allseq M.!? lastn of
                           Nothing -> (M.insert lastn (i-1) allseq, 0)
                           Just n  -> (M.insert lastn (i-1) allseq, (i-n-1))

main = do
  inputFilePath <- head <$> getArgs
  let inputFilePath = "../input15.txt"
  inputLines <- map (read :: String -> Int) . concatMap (splitOn ",")  . lines <$> readFile inputFilePath
  let s0 = (M.fromList $ zip (init inputLines) [1..], last inputLines)
  let iter1 = [(length inputLines + 1)..2020]
  let res1 = snd $ foldl' run1 s0 iter1
  putStrLn . ("Part 1: " ++) . show $ res1
  let iter2 = [(length inputLines + 1)..30000000]
  let res2 = snd $ foldl' run1 s0 iter2
  putStrLn . ("Part 2: " ++) . show $ res2

