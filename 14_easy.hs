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

type Address = Int
type Value = Int
type Mask = (Int, Int)
type Memory = M.IntMap Int

data Instruction = IAssignment  Address Value
                 | IMask Mask
                 deriving (Eq, Show)


type CompState = (Memory, Mask)
resetCompState = (M.empty,(1, 0))

type Address2 = String
type Mask2 = String
type CompState2 = (Memory, Mask2)
resetCompState2 = (M.empty, intToString 36 0)

data Instruction2 = IAssignment2 Address2 Value
                  | IMask2 Mask2
                  deriving (Eq, Show)

parseMaskInstruction :: String -> Mask
parseMaskInstruction instr = (andMask, orMask) where
  toInt acc b = 2*acc + b
  parseMask _ '1' = 1
  parseMask _ '0' = 0
  parseMask b 'X' = b
  andMask = (foldl1' toInt . map (parseMask 1)) instr
  orMask  = (foldl1' toInt . map (parseMask 0)) instr


parseInstruction :: String -> Instruction
parseInstruction = mkInstruction . splitOn " = " where
  mkInstruction [x, y] = let getAddress = read . takeWhile isDigit . dropWhile (not . isDigit)
    in if "mask" == x
         then IMask (parseMaskInstruction y)
         else IAssignment (getAddress x) (read y)


run1 :: CompState -> Instruction -> CompState
run1 (memory, mask) (IMask imask) = (memory, imask)
run1 (memory, mask) (IAssignment addr val) = (newMemory, mask)
  where applyMask :: Int -> Int
        applyMask = ((snd mask) .|.) . ((fst mask) .&.)
        newMemory = M.insert addr (applyMask val) memory
        

parseInstruction2 :: String -> Instruction2
parseInstruction2 = mkInstruction . splitOn " = " where
  mkInstruction [x, y] = let getAddress = intToString 36 . read . takeWhile isDigit . dropWhile (not . isDigit)
    in if "mask" == x
         then IMask2 y
         else IAssignment2 (getAddress x) (read y)


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
    
run2 :: CompState2 -> Instruction2 -> CompState2
run2 (memory, mask) (IMask2 imask) = (memory, imask)
run2 (memory, mask) (IAssignment2 addr val) = (newMemState, mask)
  where addresses = genAddresses 0 $ maskAddress mask addr
        newUpdates  = M.fromList $ zip addresses (repeat val)
        newMemState = M.union newUpdates memory

main = do
  inputFilePath <- head <$> getArgs
  let inputFilePath = "../input14.txt"
  inputLines <- lines <$> readFile inputFilePath
  --mapM_ print inputLines
  let instructions = map parseInstruction inputLines
  let res1 = M.foldl (+) 0 . fst . foldl' run1 resetCompState $ instructions
  putStrLn . ("Part 1: " ++) . show $ res1

  -- Part 2
  let instructions2 = map parseInstruction2 inputLines
  let res2 = M.foldl (+) 0 . fst . foldl' run2 resetCompState2 $ instructions2
  putStrLn . ("Part 2: " ++) . show $ res2

