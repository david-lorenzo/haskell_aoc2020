import System.Environment
import Data.List
import Data.List.Split
import Text.Parsec

data Expr = Err
          | Num Int
          | Sum Int
          | Mul Int
          | LExpr [Expr] deriving (Show, Eq)

parseNum = do
  spaces
  n <- many1 digit
  return $ Num (read n)

parseSum = do
  spaces
  char '+'
  return (Sum 0)

parseMul = do
  spaces
  char '*'
  return (Mul 1)

parseSubexpr = do
  spaces
  char '('
  subexp <- parseExpr
  spaces
  char ')'
  return subexp


parseExpr = do
  lexp <- many (try parseSubexpr
                 <|> try parseNum
                 <|> try parseSum
                 <|> try parseMul)
  return (LExpr lexp)


eval1 :: Expr -> Expr -> Expr
eval1 (Num n) (Sum 0) = Sum n
eval1 (Num n) (Mul 1) = Mul n
eval1 (Sum n) (Num m) = Num (n+m)
eval1 (Mul n) (Num m) = Num (n*m)
eval1 (LExpr as) b    = eval1 (foldl1' eval1 as) b
eval1 b (LExpr as)    = eval1 b (foldl1' eval1 as)
eval1 _ _             = Err

runeval1 :: Expr -> Maybe Int
runeval1 (LExpr es) = let resrun = foldl1' eval1 es
  in case resrun of
       Num n -> Just n
       _     -> Nothing

runeval2 :: Expr -> Int
runeval2 (Num n) = n
runeval2 (Sum n) = n
runeval2 (LExpr es) = product . map (sum . map runeval2) $ splitOn [Mul 1] es

main = do
  inputFilePath <- head <$> getArgs
  inputLines <- lines <$> readFile inputFilePath
  let parsedLines = sequenceA . map (parse parseExpr "") $ inputLines

  -- Part 1
  let doeval1 = sequenceA . map runeval1
  let res1 = ((sum <$>) <$>) . (doeval1 <$>) $ parsedLines
  mapM_ (mapM_ (putStrLn . ("Part 1: " ++) . show)) res1

  -- Part 2
  let res2 = sum . map runeval2 <$> parsedLines
  mapM_ (putStrLn . ("Part 2: " ++) . show) res2
  return ()

