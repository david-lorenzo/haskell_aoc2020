import Control.Monad
import qualified Data.Set as S
import System.Environment

main = do
    inputFilePath <- liftM head $ getArgs
    inputContent <- liftM lines $ readFile inputFilePath
    let result = findDupla 2020 $ toIntList inputContent
    case result of
      Just (x, y) -> putStrLn $ show (x * y)
      Nothing     -> putStrLn "Could not find the target from the input"
  where
    toIntList :: [String] -> [Int]
    toIntList = map read

findDupla :: Int -> [Int] -> Maybe (Int, Int)
findDupla target candidates =
  let
    candidatesSet = S.fromList candidates
    safeHead [] = Nothing
    safeHead (x:xs) = Just x
  in
    safeHead [(x, y) | x <- candidates, let y = target - x, S.member y candidatesSet]
