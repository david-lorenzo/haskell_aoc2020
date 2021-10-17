import System.Environment
import Data.List
import Text.Parsec

data Expr = Const Int
          | OpSum
          | OpMul
          | SubExp [Expr]
          | ESum Int
          | EMul Int
          | EErr deriving (Show, Eq)

parseConst = do
  spaces
  n <- many1 digit
  return (Const (read n))

parseSum = do
  spaces
  char '+'
  return OpSum

parseMul = do
  spaces
  char '*'
  return OpMul

parseSub = do
  spaces
  char '('
  t <- parseExpr
  char ')'
  return t

parseExpr = do
  t <- many (try parseSum
               <|> try parseMul
               <|> try parseConst
               <|> try parseSub)
  return (SubExp t)

run1 :: Expr -> Either String Int
run1 EErr       = Left "Error de evaluacion"
run1 (Const n)  = Right n
run1 (SubExp e) = run1 $ eval1 (ESum 0) (SubExp e)
  where
    eval1 (Const n) OpSum    = ESum n
    eval1 (Const n) OpMul    = EMul n
    eval1 (ESum n) (Const m) = Const (n+m)
    eval1 (EMul n) (Const m) = Const (n*m)
    eval1 a (SubExp b) = eval1 a (foldl' eval1 (ESum 0) b)
    eval1 _ _ = EErr

main = do
  inputFilePath <- head <$> getArgs
  inputLines <- lines <$> readFile inputFilePath

  -- Part 1
  let res1 = fmap (fmap sum) . fmap (sequenceA . map run1) . (sequenceA . map (parse parseExpr "")) $ inputLines
  mapM_ (mapM_ (putStrLn . ("Part 1: " ++) . show)) res1

  -- Part 2
  let res2 = 0
  putStrLn . ("Part 2: " ++) . show $ res2

