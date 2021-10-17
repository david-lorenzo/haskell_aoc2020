{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import Data.List hiding (lines, words, concat)
import Data.Text.IO (readFile)
import Data.Text (Text, lines, pack, unpack, splitOn, words, concat)
import Prelude hiding (readFile, lines, words, concat)
import Data.Hashable (hash)
import Text.Parsec

main = do
  inputFilePath <- head <$> getArgs
  inputData <- lines <$> readFile inputFilePath
  let records = map process inputData
  let p1 = (containShinyGold . M.fromListWith (++) . concatMap invertEdges) records
  (putStrLn . ("Part 1: " ++) . show) p1
  let p2 = (containedInShinyGold . M.fromListWith (++)) records
  (putStrLn . ("Part 2: " ++) . show) p2
  return ()

process :: Text -> (Int, [(Int, Int)])
process xs = (hash $ parseColor bag, parseBags bags)
  where [bag, bags]= splitOn " contain " xs

parseColor bag = concat [intensity, " ", color]
  where (intensity:color:_) = words bag

parseBags :: Text -> [(Int, Int)]
parseBags bags = case bags of
                  "no other bags." -> []
                  _ -> map parseNumBags $ splitOn ", " bags

parseNumBags bag = (read (unpack number) :: Int, hash $ concat [intensity, pack " ", color])
  where (number:intensity:color:_) = words bag

invertEdges :: (Int, [(Int, Int)]) -> [(Int, [Int])]
invertEdges (x, ys) = map (\(n, y) -> (y, [x])) ys


containShinyGold :: M.IntMap [Int] -> Int
containShinyGold bagsMap = S.size $ go $ (hash ("shiny gold" :: Text) :: Int)
  where go :: Int -> S.IntSet
        go color = case M.lookup color bagsMap of
                     Just xs -> foldl' S.union (S.fromList xs) $ map go xs
                     Nothing -> S.empty


containedInShinyGold :: M.IntMap [(Int, Int)] -> Int
containedInShinyGold bagsMap = go $ (hash ("shiny gold" :: Text) :: Int)
  where go :: Int -> Int
        go color = case M.lookup color bagsMap of
                     Just xs -> sum $ map (\(n, x) -> n + n * go x) xs
                     Nothing -> 0


