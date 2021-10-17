import System.Environment
import Data.List.Split
import Data.List

nextDeparture departure n = let x = (departure `div` n) + 1 in ((x * n) `rem` departure, n)

chinesseRemTheorem (n0, a0) (n1, a1) = (n2, x0 + n2)
  where x0 = head $ dropWhile ((/=a1) . (`mod` n1)) [a0,(a0 + n0)..]
        n2 = n0*n1

main = do
  --inputFilePath <- head <$> getArgs
  let inputFilePath = "../input13.txt"
  inputLines <- lines <$> readFile inputFilePath
  --mapM_ print inputLines
  let departure = read . head $ inputLines :: Int
  let buses = map (read :: String -> Int) . filter (/= "x") . splitOn "," . head . tail $ inputLines
  --let res1 = (uncurry (*)) . head . sort . map (nextDeparture departure) $ buses
  let res1 = (uncurry (*)) . minimum . map (nextDeparture departure) $ buses
  putStrLn . ("Part 1: " ++) . show $ res1

  -- Part 2
  let buses2 = map (\(x, y) -> (read x :: Int, y))
                   . filter ((/= "x") . fst)
                   . flip zip [0..] $ (splitOn "," . head . tail) inputLines
  let mods2 = sortBy (flip compare) . map (\(x, y) -> (x, (-y) `mod` x)) $ buses2
  let res2 = uncurry (flip (-)) $ foldl1' chinesseRemTheorem mods2
  putStrLn . ("Part 2: " ++) . show $ res2

