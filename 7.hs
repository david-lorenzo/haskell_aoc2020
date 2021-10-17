import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Text.Parsec

main = do
  inputFilePath <- head <$> getArgs
  inputData <- lines <$> readFile inputFilePath
  let records = sequenceA $ map (parse p "") inputData
  let p1 = (containShinyGold . M.fromListWith (++) . concatMap invertEdges) <$> records
  mapM_ (putStrLn . ("Part 1: " ++) . show) p1
  let p2 = (containedInShinyGold . M.fromListWith (++)) <$> records
  mapM_ (putStrLn . ("Part 2: " ++) . show) p2
  return ()


invertEdges :: (String, [(Int, String)]) -> [(String, [String])]
invertEdges (x, ys) = map (\(n, y) -> (y, [x])) ys


containShinyGold :: M.Map String [String] -> Int
containShinyGold bagsMap = S.size $ go "shiny gold"
  where go :: String -> S.Set String
        go color = case M.lookup color bagsMap of
                     Just xs -> foldl' S.union (S.fromList xs) $ map go xs
                     Nothing -> S.empty


containedInShinyGold :: M.Map String [(Int, String)] -> Int
containedInShinyGold bagsMap = go "shiny gold"
  where go :: String -> Int
        go color = case M.lookup color bagsMap of
                     Just xs -> sum $ map (\(n, x) -> n + n * go x) xs
                     Nothing -> 0


parseBagColor :: Parsec String () String
parseBagColor = do
  intensity <- many1 letter
  spaces
  color <- many1 letter
  spaces
  try (string "bags") <|> try (string "bag")
  return (unwords [intensity, color])


parseEmptyBag :: Parsec String () [(Int, String)]
parseEmptyBag = do
  spaces
  string "no other bags"
  return []


parseFullBag :: Parsec String () (Int, String)
parseFullBag = do
  spaces
  num <- many1 digit
  spaces
  color <- parseBagColor
  return (read num :: Int, color)
  

parseContent :: Parsec String () [(Int, String)]
parseContent = do
  content <- try parseEmptyBag <|> try (parseFullBag `sepBy1` char ',')
  return content
 

p :: Parsec String () (String, [(Int, String)])
p = do
  bagColor <- parseBagColor
  spaces
  string "contain"
  content <- parseContent
  char '.'
  return (bagColor, content)

