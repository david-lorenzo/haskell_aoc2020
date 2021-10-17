import System.Environment
import Control.Monad
import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse)
import Data.Either
import qualified Data.IntMap.Strict as M
import Data.List.Split

data Rule = SRule String
          | CRule11 Int
          | CRule12 Int Int
          | CRule21 Int Int
          | CRule22 (Int, Int) (Int, Int)
          deriving (Show, Eq)

type Parser = Parsec String ()

integer :: Parser Int
integer = read <$> many1 digit

parseBody11 :: Parser Rule
parseBody11 = do
  l0 <- integer
  return $ CRule11 l0

parseBody12 :: Parser Rule
parseBody12 = do
  l0 <- integer
  spaces
  l1 <- integer
  return $ CRule12 l0 l1

parseBody21 :: Parser Rule
parseBody21 = do
  l0 <- integer
  spaces
  char '|'
  spaces
  r0 <- integer
  return $ CRule21 l0 r0

parseBody22 :: Parser Rule
parseBody22 = do
  l0 <- integer
  spaces
  l1 <- integer
  spaces
  char '|'
  spaces
  r0 <- integer
  spaces
  r1 <- integer
  return $ CRule22 (l0, l1) (r0, r1)


parseBase :: Parser Rule
parseBase = do
  s <- many1 anyChar
  let ss = filter (/= '"') s
  return $ SRule ss

parseRule :: Parser (M.IntMap Rule)
parseRule = do
  ruleid <- integer
  char ':'
  spaces
  rbody <-     try parseBody22
           <|> try parseBody21
           <|> try parseBody12
           <|> try parseBody11
           <|> try parseBase
  return $ M.singleton ruleid rbody


parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

parselist :: Parser a -> [String] -> [a]
parselist p = either (error . show) id . mapM (parse p)

createTop :: M.IntMap Rule -> Int -> Parser ()
createTop rules n = ptop
  where p42 = createp rules 42
        p31 = createp rules 31
        ptop = do
          r42 <- many1 $ try p42
          r31 <- many1 p31
          guard $ length r42 > length r31
          eof
          return ()

createp :: M.IntMap Rule -> Int -> Parser ()
createp rules n = go rule
  where
    rule = rules M.! n
    go (SRule s) = ps
      where ps = do
              _s <- string s
              return ()
    go (CRule11 a) = p11
      where pa = createp rules a
            p11 = do
              a <- pa
              return ()
    go (CRule12 a b) = p12
      where pa = createp rules a
            pb = createp rules b
            p12 = do
              a <- pa
              b <- pb
              return ()
    go (CRule21 a b) = p21
      where pa = createp rules a
            pb = createp rules b
            p21 = do
              c <- try pa <|> try pb
              return ()
    go (CRule22 (a, b) (c, d)) = p22
      where pa = createp rules a
            pb = createp rules b
            pc = createp rules c
            pd = createp rules d
            p22 = do
              c <- try (pa >> pb) <|> try (pc >> pd)
              return ()

main = do
  inputFilePath <- head <$> getArgs
  inputLines <- splitOn "\n\n" <$> readFile inputFilePath
  let [rules, messages] = inputLines
  let rulesmap = M.unions $ parselist parseRule $ lines rules
  let topparser = createTop rulesmap 0
  let res2 = length . rights . map (parse topparser) $ lines messages
  putStrLn . ("Part 2: " ++) . show $ res2
