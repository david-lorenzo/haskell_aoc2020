import System.Environment

cryptoCore :: Int -> Int -> Int -> Int
cryptoCore divisor x y = (x * y) `rem` divisor

crypto :: Int -> Int -> [Int]
crypto divisor subjectNumber = scanl (cryptoCore divisor) 1 . repeat $ subjectNumber

secretKey :: Int -> Int -> Int -> Int
secretKey divisor loopSize publicKey = head . drop loopSize . scanl (cryptoCore divisor) 1 . repeat $ publicKey

main = do
  let divisor = 20201227
  let cardSubjectNumber = 7
  let cardPublicKey = 12232269
  let cardLoopSize = length . takeWhile (/= cardPublicKey) . crypto divisor $ cardSubjectNumber
  let doorSubjetNumber = 7
  --let doorLoopSize = ?? -- it doesn't matter
  let doorPublicKey = 19452773
  let res1 = secretKey divisor cardLoopSize doorPublicKey
  putStrLn . ("Part 1: " ++) . show $ res1

