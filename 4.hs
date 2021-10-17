import Control.Monad
import System.Environment
import Data.List.Split
import qualified Data.Map.Strict as SM
import qualified Data.Set as Set
import Data.Char

main = do
    inputFilePath <- head <$> getArgs
    inputData <- (map makeMap . splitOn "\n\n") <$> readFile inputFilePath
    let validPassports = filter testPassport inputData
    let checkedPassports = filter testPassportFields validPassports
    putStrLn $ "Part 1: " ++ ((show . length) validPassports)
    putStrLn $ "Part 2: " ++ ((show . length) checkedPassports)


listToTuple [x, y] = (x, y)

type Passport = SM.Map String String

makeMap :: String -> Passport
makeMap = SM.fromList . map (listToTuple . splitOn ":") . concatMap words . lines

testPassport :: Passport -> Bool
testPassport passport = and $ (fields <*> pure passport)
  where fields = map SM.member ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


testYear :: Int -> Int -> String -> Bool
testYear min max v = y == "" && min <= z && z <= max
  where (x, y) = span isDigit v
        z = read x

testByr = testYear 1920 2002
testIyr = testYear 2010 2020
testEyr = testYear 2020 2030

testHgt v = num /= "" && (cms || inches)
  where (num, unit) = span isDigit v
        hgt    = read num
        cms    = unit == "cm" && 150 <= hgt && hgt <= 193
        inches = unit == "in" &&  59 <= hgt && hgt <= 76

testHcl (h:rest) = and [h == '#', length rest == 6, snd (span isHexDigit rest) == ""]
testHcl _ = False

testEcl = flip Set.member okColors
  where okColors = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

testPid value = length value == 9 && rest == ""
  where (num, rest) = span isDigit value

testField :: (String -> Bool) -> String -> Passport -> Bool
testField predicate field passport =
  case (passport SM.!? field) of
    Just value -> predicate value 
    Nothing    -> False

testPassportFields :: Passport -> Bool
testPassportFields passport = and $ (fieldTests <*> pure passport)
  where fieldTests = [ testField testByr "byr"
                     , testField testIyr "iyr"
                     , testField testEyr "eyr"
                     , testField testHgt "hgt"
                     , testField testHcl "hcl"
                     , testField testEcl "ecl"
                     , testField testPid "pid"
                     ]

