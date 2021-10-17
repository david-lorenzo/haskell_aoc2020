import System.Environment
import qualified Data.Vector as V

main = do
  inputFilePath <- head <$> getArgs
  numbers <- V.fromList . map (read :: String -> Int) . lines <$> readFile inputFilePath
  let res1 = case filter (not . checkNumber numbers) [25..(length numbers - 1)] of
               [] -> Nothing
               (x:_) -> numbers V.!? x
  mapM_ (putStrLn . ("Part 1: " ++) . show) res1


checkNumber :: V.Vector Int -> Int -> Bool
checkNumber vector i = case xs of
                         [] -> False
                         (x:_) -> True
  where xs = [(x, y) | x <- [(i-25)..(i-1)]
                     , y <- [(x+1)..(i-1)]
                     , vector V.! i == (vector V.! x) + (vector V.! y)]


