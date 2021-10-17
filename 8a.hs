import System.Environment
import qualified Data.Map as M
import qualified Data.IntSet as S
import Data.List.Split
import Data.Array

main = do
  inputFilePath <- head <$> getArgs
  inputData <- lines <$> readFile inputFilePath
  let instructions = map readInstruction inputData
  let aInstructions = listArray (0, length instructions - 1) instructions
  (putStrLn . ("Part 1: " ++) . show . run) aInstructions 
  return ()

data Instruction = Nop Int | Acc Int | Jmp Int deriving (Show)


readInstruction :: String -> Instruction
readInstruction x = case inst of
                      "jmp" -> Jmp p
                      "acc" -> Acc p
                      "nop" -> Nop p
  where [inst, param] = words x
        p = case param of
              ('+':ps) -> read ps
              _        -> read param

data Computer = Computer { address :: Int, accumulator :: Int } deriving (Show)

runInstruction :: Array Int Instruction -> S.IntSet -> Computer -> (Bool, S.IntSet, Computer)
runInstruction prog addressSet (Computer { address = address, accumulator = accumulator }) = 
    case prog ! address of
      Nop _ -> (infLoop, newAddressSet, Computer {address = address + 1, accumulator = accumulator})
      Jmp x -> (infLoop, newAddressSet, Computer {address = address + x, accumulator = accumulator})
      Acc x -> (infLoop, newAddressSet, Computer {address = address + 1, accumulator = accumulator + x})
  where infLoop = S.member address addressSet
        newAddressSet = S.insert address addressSet


run :: Array Int Instruction -> Int
run prog = go S.empty Computer { address = 0, accumulator = 0 }
  where go addressSet computer =
          case runInstruction prog addressSet computer of
            (True, _, _) -> accumulator computer
            (False, newAddressSet, newComputer) -> go newAddressSet newComputer

