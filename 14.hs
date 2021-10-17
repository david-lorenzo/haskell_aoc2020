import System.Environment
import Data.Bits
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.IntMap.Strict as M

intToString :: Int -> Int -> String
intToString len n = go len "" n
  where go 0 acc _ = acc
        go l acc m = go (l-1) (a:acc) (shiftR m 1)
          where a = case (m .&. 1) of
                      0 -> '0'
                      1 -> '1'

type Mask = (Int, Int)

data Instruction = IAssignment { address :: Int, value :: Int }
                 | IMask { mask :: Mask }
                 | IAssignment2 { address2 :: String, value2 :: Int }
                 | IMask2 { mask2 :: String } deriving (Eq, Show)


data Operation = OpAnd | OpOr deriving (Show, Eq)

data CompState = CompState { csMemory :: M.IntMap Int, csMask :: Mask } deriving (Eq, Show)
resetCompState = CompState { csMemory = M.empty, csMask = (1, 0) }

data CompState2 = CompState2 { csMemory2 :: M.IntMap Int, csMask2 :: String } deriving (Eq, Show)
resetCompState2 = CompState2 { csMemory2 = M.empty, csMask2 = intToString 36 0 }


parseMaskInstruction :: String -> Mask
parseMaskInstruction instr = (andMask, orMask) where
  toInt acc b = 2*acc + b
  parseMask _ '1' = 1
  parseMask _ '0' = 0
  parseMask OpAnd 'X' = 1
  parseMask OpOr  'X' = 0
  andMask = (foldl1' toInt . map (parseMask OpAnd)) instr
  orMask  = (foldl1' toInt . map (parseMask OpOr)) instr


parseInstruction = mkInstruction . splitOn " = " where
  mkInstruction [x, y] = let getAddress = read . takeWhile isDigit . dropWhile (not . isDigit)
    in if "mask" == x
         then IMask $ parseMaskInstruction y
         else IAssignment {address = getAddress x, value = read y}


runInstructions :: CompState -> Instruction -> CompState
runInstructions state (IMask{mask=imask}) = state { csMask = imask }
runInstructions state (IAssignment{address=addr, value=val}) =
    state {csMemory = M.insert addr (applyMask msk val) mem}
  where mem = csMemory state
        msk = csMask state
        applyMask :: Mask -> Int -> Int
        applyMask mask = ((snd mask) .|.) . ((fst mask) .&.)
        

parseInstruction2 = mkInstruction . splitOn " = " where
  mkInstruction [x, y] = let getAddress = intToString 36 . read . takeWhile isDigit . dropWhile (not . isDigit)
    in if "mask" == x
         then IMask2 {mask2 = y}
         else IAssignment2 {address2 = getAddress x, value2 = read y}


maskAddress = zipWith go where
  go mask address = case mask of
                      '0' -> address
                      '1' -> '1'
                      'X' -> 'X'

genAddresses :: Int -> String -> [Int]
genAddresses acc [] = [acc]
genAddresses acc (x:xs) =
  case x of
    '0' -> genAddresses (2*acc) xs
    '1' -> genAddresses (2*acc + 1) xs
    'X' -> (genAddresses (2*acc) xs) ++ (genAddresses (2*acc + 1) xs)
    
runInstructions2 :: CompState2 -> Instruction -> CompState2
runInstructions2 state (IMask2{mask2=imask}) = state { csMask2 = imask }
runInstructions2 state (IAssignment2{address2=addr, value2=val}) =
    state {csMemory2 = M.union newMemState mem}
  where mem = csMemory2 state
        msk = csMask2 state
        addresses = genAddresses 0 $ maskAddress msk addr
        newMemState = M.fromList $ zip addresses (repeat val)

main = do
  inputFilePath <- head <$> getArgs
  let inputFilePath = "../input14.txt"
  inputLines <- lines <$> readFile inputFilePath
  --mapM_ print inputLines
  let instructions = map parseInstruction inputLines
  let res1 = M.foldl (+) 0 . csMemory . foldl' runInstructions resetCompState $ instructions
  putStrLn . ("Part 1: " ++) . show $ res1

  -- Part 2
  let instructions2 = map parseInstruction2 inputLines
  let res2 = M.foldl (+) 0 . csMemory2 . foldl' runInstructions2 resetCompState2 $ instructions2
  putStrLn . ("Part 2: " ++) . show $ res2

