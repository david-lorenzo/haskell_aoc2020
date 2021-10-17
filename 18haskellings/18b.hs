
import System.Environment
import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Data.Either
import Data.Char

type Parser = Parsec String ()

integer :: Parser Int
integer = read <$> many1 digit

paren = char '(' *> expr <* char ')'
term = paren <|> integer
table = [[Infix (char '+' >> return (+)) AssocLeft], [Infix (char '*' >> return (*)) AssocLeft]]
expr = buildExpressionParser table term


parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""


parselist :: Parser a -> [String] -> [a]
parselist p = either (error . show) id . mapM (parse p)

main = do
  inputFile <- head <$> getArgs
  res <- sum . parselist expr . lines . filter (/=' ') <$> readFile inputFile
  print res
  
