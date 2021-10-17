import System.Environment
import qualified Data.Vector as V

main = do
  inputFilePath <- head <$> getArgs
  numbers <- V.fromList . map (read :: String -> Int) . lines <$> readFile inputFilePath
  let res1 = case filter (not . checkNumber numbers) [25..(length numbers - 1)] of
               [] -> Nothing
               (x:_) -> numbers V.!? x
  mapM_ (putStrLn . ("Part 1: " ++) . show) res1
  mapM_ (putStrLn . ("Part 2: " ++) . show . tryStart numbers 0) res1


checkNumber :: V.Vector Int -> Int -> Bool
checkNumber vector i = case xs of
                         [] -> False
                         (x:_) -> True
  where xs = [(x, y) | x <- [(i-25)..(i-1)]
                     , y <- [(x+1)..(i-1)]
                     , vector V.! i == (vector V.! x) + (vector V.! y)]


scanV :: V.Vector Int -> Int -> Int -> [(Int, Int)]
scanV vector target i = dropWhile ((< target) . snd) $ takeWhile ((<= target) . snd) listOfSums
  where range = [i..(V.length vector - 1)]
        listOfSums = zip range $ (tail . scanl (\z i -> z + vector V.! i) 0) range


tryStart :: V.Vector Int -> Int -> Int -> Int
tryStart vector start target =
  let res = scanV vector target start
      sumStartEnd xs = minimum xs + maximum xs
  in if start >= V.length vector
        then 0
        else case res of
              []           -> tryStart vector (succ start) target
              ((end, _):_) -> sumStartEnd $ map (vector V.!) [start..end]

