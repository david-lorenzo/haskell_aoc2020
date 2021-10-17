import System.Environment
import qualified Data.Map as M
import Data.List.Split

main = do
  inputFilePath <- head <$> getArgs
  inputData <- splitOn "\n\n" <$> readFile inputFilePath
  --let groupAnswers = map processGroups inputData
  let groupAnswers = zip (map (counter . (filter (/= '\n'))) inputData) (map (length . lines) inputData)
  putStrLn . ("Part 1: " ++) . show . sum . map (M.size . fst) $ groupAnswers
  putStrLn . ("Part 2: " ++) . show . sum . (map g) $ groupAnswers

counter = M.fromListWith (+) . (map (\x -> (x, 1)))

processGroups x = (xmap, groupSize)
  where xmap      = counter $ filter (/= '\n') x
        groupSize = length  $ lines x

g (xmap, xsize) = length . filter ((== xsize) . snd) $ M.toList xmap
