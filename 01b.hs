import Control.Monad
import System.Environment
import qualified Data.Set as S

main = do
    inputFilePath <- liftM head $ getArgs
    inputContent <- liftM lines $ readFile inputFilePath
    let result = findTriplet 2020 $ map (read :: String -> Int) inputContent
    case result of
      (x, y, z):_ -> putStrLn $ show (x * y * z)
      []          -> putStrLn "Could not find the target from the input"


findTriplet :: Int -> [Int] -> [(Int, Int, Int)]
findTriplet target numbers = [(x, y, z) | x <- numbers,
                                          y <- numbers,
                                          let z = target - x - y,
                                          S.member z numberSet]
                             where numberSet = S.fromList numbers
