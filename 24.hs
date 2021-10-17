import           System.Environment
import           Data.Either
import           Data.List
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import           Control.Monad
import           Control.Monad.ST

data Dir = E | SE | SW | W | NW | NE deriving (Show, Eq, Read, Bounded, Enum)

type Parser = Parsec String ()

dirp :: Parser Dir
dirp = do
  d <-    try (string "nw" >> return NW)
      <|> try (string "ne" >> return NE)
      <|> try (string "sw" >> return SW)
      <|> try (string "se" >> return SE)
      <|> try (string "w"  >> return W)
      <|> try (string "e"  >> return E)
  return d

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

parselist :: Parser a -> [String] -> [a]
parselist p = either (error . show) id . mapM (parse p)

type Pos = (Int, Int)

runpath :: (Int, Int) -> Dir -> (Int, Int)
runpath (y, x) dir = let offset = y `rem` 2
  in case dir of
        NW -> (y-1, x - offset)
        NE -> (y-1, x + 1 - offset)
        SW -> (y+1, x - offset)
        SE -> (y+1, x + 1 - offset)
        W  -> (y, x - 1)
        E  -> (y, x + 1)


pos2idx n (y, x) = y*n + x


grid :: Int -> [[Dir]] -> V.Vector Bool
grid n paths = runST (do
  let center = (n `div` 2, n `div` 2)
  v <- MV.new (n*n)
  forM_ [0..(n*n-1)] (\i -> do MV.write v i False)
  forM_ paths (\path -> do
          let pos = foldl' runpath center path
          let idx = pos2idx n pos
          MV.modify v (\x -> not x) idx) 
  V.freeze v)


neighbors :: Int -> Pos -> [Pos]
neighbors n (y, x) = filter (\(y, x) -> and [0 <= y, y < n, 0 <= x, x < n]) ns
  where offset = y `rem` 2
        ns = [ (y - 1, x     - offset)
             , (y - 1, x + 1 - offset)
             , (y    , x - 1         )
             , (y    , x + 1         )
             , (y + 1, x     - offset)
             , (y + 1, x + 1 - offset) ]


countTrueVector :: V.Vector Bool -> Int
countTrueVector = V.foldl' (\acc x -> if x then acc + 1 else acc) 0 


newValue :: Int -> Pos -> V.Vector Bool -> Bool
newValue n pos v = case (value, nblacks) of
                      (True , 1) -> True
                      (True , 2) -> True
                      (True , _) -> False
                      (False, 2) -> True
                      (False, _) -> False
  where value   = v V.! (pos2idx n pos)
        ns      = map (\p -> v V.! (pos2idx n p)) . neighbors n $ pos
        nblacks = foldl' (\acc x -> if x then acc + 1 else acc) 0 ns
        

flips :: Int -> V.Vector Bool -> V.Vector Bool
flips n v = runST (do v' <- MV.new (n*n)
                      forM_ [(y, x) | y <- [0..(n-1)], x <- [0..(n-1)]]
                            (\pos -> do MV.write v' (pos2idx n pos) (newValue n pos v))
                      V.freeze v')

main = do
  inputFilePath <- head <$> getArgs
  let inputFilePath = "../input24.txt"
  inputLines <- lines <$> readFile inputFilePath
  let paths = rights . map (parse (many1 dirp)) $ inputLines
  let gridSize = 201
  let res1 = grid gridSize paths
  putStrLn . ("Part 1: " ++) . show . countTrueVector $ res1

  -- Part 2
  let res2 = head . drop 100 . iterate (flips gridSize) $ res1
  putStrLn . ("Part 2: " ++) . show . countTrueVector $ res2

