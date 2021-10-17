import System.Environment
import Text.Parsec
import Data.List.Split
import Data.List
import Data.Bool
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S

type RName = String
type RRange = (Int, Int)
type Ticket = [Int]

data Restriction = Restriction RName RRange RRange deriving (Eq, Show)
data Input = Input [Restriction] Ticket [Ticket]   deriving (Eq, Show)

integer :: Parsec String () Int
integer = do
  strNum <- many1 digit
  return $ read strNum

parseRestriction :: Parsec String () Restriction
parseRestriction = do
  field <- many1 $ satisfy (/= ':')
  char ':'
  spaces
  r00 <- integer
  char '-'
  r01 <- integer
  spaces
  string "or"
  spaces
  r10 <- integer
  char '-'
  r11 <- integer
  return $ Restriction field (r00, r01) (r10, r11)

parseTicket :: Parsec String () [Int]
parseTicket = do
  ticket <- integer `sepBy1` char ','
  return ticket

parseTicket2 :: Parsec String () [Int]
parseTicket2 = do
  ticket <- parseTicket
  char '\n'
  return ticket


parseYourTicket :: Parsec String () [Int]
parseYourTicket = do
  string "your ticket:\n"
  yt <- parseTicket
  return yt

parseOtherTickets :: Parsec String () [[Int]]
parseOtherTickets = do
  string "nearby tickets:\n"
  otherTickets <- many1 parseTicket2
  return otherTickets

parseRestrictions :: Parsec String () [Restriction]
parseRestrictions = do
  rest <- parseRestriction `sepBy1` char '\n'
  return $ rest

parseInput :: String -> Either ParseError Input
parseInput idata = do
  let [a, b, c] = splitOn "\n\n" idata
  rest <- parse parseRestrictions "" a
  yt <- parse parseYourTicket "" b
  ots <- parse parseOtherTickets "" c
  return $ Input rest yt ots


check :: Restriction -> Int -> Bool
check (Restriction _ (r00, r01) (r10, r11)) n = (r00 <= n && n <= r01) || (r10 <= n && n <= r11)

checkAll :: [Restriction] -> Int -> Bool
checkAll restrictions n = or ((map check restrictions) <*> pure n)

run1 :: Input -> Int
run1 (Input rs _ ots) = sum $ filter (not . (checkAll rs)) $ concat ots

fieldOpts :: [Restriction] -> [[Int]] -> [(Int, Int)]
fieldOpts rs fs = [(m, n) | (n, r) <- zip [0..] rs
                          , (m, f) <- zip [0..] fs
                          , and (map (check r) f)]

okTicket rs = and . (map (checkAll rs))

okTickets rs ts = filter (okTicket rs) ts

makeSets xs = (fst $ head xs, S.fromList $ map snd xs)



reduceOptions :: M.IntMap S.IntSet -> M.IntMap Int -> Int -> M.IntMap Int
reduceOptions ops im n
    | n == 0          = M.empty
    | M.size ops == 0 = im
    | otherwise       = reduceOptions newOps newIm (n-1)
  where
    solo = M.mapWithKey (\k a -> S.findMin a)  $  M.filter (\x -> S.size x == 1) ops
    newIm = M.union im solo
    ops1 = M.foldlWithKey (\x k _ -> M.delete k x) ops solo
    values = M.foldl (\a v -> S.insert v a) S.empty solo
    newOps = M.map (\x -> x S.\\ values) ops1


run2 (Input rs yt ots) = product ytValues
  where
    oktk = transpose . okTickets rs $ ots
    fops = sort . fieldOpts rs $ oktk
    options = M.fromList . map makeSets . groupBy (\x y -> fst x == fst y) $ fops
    c2rMap = reduceOptions options M.empty 21
    r2cMap = M.fromList [(y, x) | (x, y) <- M.toList c2rMap]
    names = map (\(Restriction name _ _) -> name) rs
    fdPos = map fst $ filter (and . zipWith (==) "departure" . snd) $ zip [0..] names
    csPos = map (r2cMap M.!) fdPos
    ytValues = map (yt !!) csPos


main = do
  inputFilePath <- head <$> getArgs
  inputData <- parseInput <$> readFile inputFilePath
  let res1 = run1 <$> inputData
  mapM_ (putStrLn . ("Part 1: " ++) . show)  res1
  -- Part 2
  let (Right indata) = inputData
  let res2 = run2 indata
  putStrLn . ("Part 2: " ++) . show $ res2
  return ()

