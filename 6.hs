import System.Environment
import qualified Data.Map as M
import Data.List.Split

main = do
  inputFilePath <- head <$> getArgs
  groupAnswers <- map processGroups . splitOn "\n\n" <$> readFile inputFilePath
  putStrLn . ("Part 1: " ++) . show . sum . map (M.size . fst) $ groupAnswers
  putStrLn . ("Part 2: " ++) . show . sum . map g $ groupAnswers

counter = M.fromListWith (+) . (map (\x -> (x, 1)))

processGroups x = (xmap, xsize)
  where xmap      = counter $ filter (/= '\n') x
        xsize     = length  $ lines x

g (xmap, xsize) = length . filter ((== xsize) . snd) $ M.toList xmap
