import System.Environment
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Ingredient = String
type Allergen = String
type Product = ([Ingredient], [Allergen])


readprod :: String -> Product
readprod strproduct = (ingrs, allergs)
  where [a, b] = splitOn " (contains" strproduct
        ingrs = splitOn " " $ a
        allergs = splitOn "," . filter (/= ' ') . init $ b

prodLineToAlSetIngr :: Product -> [(Allergen, S.Set Ingredient)]
prodLineToAlSetIngr (ingrs, allrgs) = [(a, S.fromList ingrs) | a <- allrgs]


discover :: M.Map Allergen (S.Set Ingredient) -> M.Map Allergen (S.Set Ingredient) -> M.Map Allergen Ingredient
discover acc aim 
  | M.null aim = M.map (head . S.toList) acc
  | otherwise = discover newAcc newAim
  where singles = filter ((==1) . S.size . snd) $ M.toList aim
        toRemove = S.unions . map snd $ singles
        newAcc = M.union acc (M.fromList singles)
        newAim0 = foldl' (\m k -> M.delete k m) aim (map fst singles)
        newAim = M.map (\v -> S.difference v toRemove) newAim0


main = do
  inputFilePath <- head <$> getArgs
  let inputFilePath = "../input21.txt"
  inputLines <- lines <$> readFile inputFilePath
  let products = map readprod inputLines

  -- Part 1
  let allergensIngredientsMap = M.fromListWith S.intersection $ concatMap prodLineToAlSetIngr products
  let allIngredients = S.fromList . concatMap fst $ products
  let allIngredientsWithAllergens = M.foldl S.union S.empty allergensIngredientsMap
  let ingrWithoutAllergens = S.difference allIngredients allIngredientsWithAllergens
  let res1 = length . filter (flip S.member ingrWithoutAllergens) $ concatMap fst products
  putStrLn . ("Part 1: " ++) . show $ res1

  -- Part 2
  let res20 = discover M.empty allergensIngredientsMap 
  let res21 = intercalate "," . map snd . sort . M.toList $ res20
  putStrLn . ("Part 2: " ++) $ res21
  return ()

