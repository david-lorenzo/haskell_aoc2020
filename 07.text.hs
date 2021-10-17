{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import Data.List hiding (lines)
import Data.Text.IO (readFile)
import Data.Text (lines, pack, unpack)
import Prelude hiding (readFile, lines)
import Data.Hashable (hash)
import Text.Parsec

main = do
  inputFilePath <- head <$> getArgs
  inputData <- lines <$> readFile inputFilePath
  let records = sequenceA $ map ((parse p "") . unpack) inputData
  let p1 = (containShinyGold . M.fromListWith (++) . concatMap invertEdges) <$> records
  mapM_ (putStrLn . ("Part 1: " ++) . show) p1
  let p2 = (containedInShinyGold . M.fromListWith (++)) <$> records
  mapM_ (putStrLn . ("Part 2: " ++) . show) p2
  return ()


invertEdges :: (Int, [(Int, Int)]) -> [(Int, [Int])]
invertEdges (x, ys) = map (\(n, y) -> (y, [x])) ys


containShinyGold :: M.IntMap [Int] -> Int
containShinyGold bagsMap = S.size $ go $ (hash ("shiny gold" :: String) :: Int)
  where go :: Int -> S.IntSet
        go color = case M.lookup color bagsMap of
                     Just xs -> foldl' S.union (S.fromList xs) $ map go xs
                     Nothing -> S.empty


containedInShinyGold :: M.IntMap [(Int, Int)] -> Int
containedInShinyGold bagsMap = go $ (hash ("shiny gold" :: String) :: Int)
  where go :: Int -> Int
        go color = case M.lookup color bagsMap of
                     Just xs -> sum $ map (\(n, x) -> n + n * go x) xs
                     Nothing -> 0


parseBagColor :: Parsec String () Int
parseBagColor = do
  intensity <- many1 letter
  spaces
  color <- many1 letter
  spaces
  try (string "bags") <|> try (string "bag")
  return (hash $ intensity ++ ' ':color)


parseEmptyBag :: Parsec String () [(Int, Int)]
parseEmptyBag = do
  spaces
  string "no other bags"
  return []


parseFullBag :: Parsec String () (Int, Int)
parseFullBag = do
  spaces
  num <- many1 digit
  spaces
  color <- parseBagColor
  return (read num, color)


parseContent :: Parsec String () [(Int, Int)]
parseContent = do
  content <- try parseEmptyBag <|> try (parseFullBag `sepBy1` char ',')
  return content


p :: Parsec String () (Int, [(Int, Int)])
p = do
  bagColor <- parseBagColor
  spaces
  string "contain"
  content <- parseContent
  char '.'
  return (bagColor, content)

