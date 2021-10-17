import System.Environment
import Data.List (foldl')
import qualified Data.IntSet as S

main = do
    inputFilePath <- head <$> getArgs
    inputData <- map code2id . lines <$> readFile inputFilePath
    let maxId = maximum inputData
    putStrLn . ("Part 1: " ++) . show $ maxId
    let seats = S.fromList inputData
    putStrLn . ("Part 2: " ++) . show $ findEmptySeat seats maxId


code2id = foldl' (\acc b -> acc*2 + bit2num b) 0
  where bit2num 'B' = 1
        bit2num 'R' = 1
        bit2num  _  = 0


findEmptySeat seats maxId = filter testId [0..maxId]
  where testId x = and [ S.notMember x seats
                       , S.member (x-1) seats
                       , S.member (x+1) seats
                       ]
