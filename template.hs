import System.Environment

main = do
  inputFilePath <- head <$> getArgs
  --let inputFilePath = "../inputXX.txt"
  inputLines <- lines <$> readFile inputFilePath
  mapM_ print inputLines
  let res1 = 0
  putStrLn . ("Part 1: " ++) . show $ res1
  let res2 = 0
  putStrLn . ("Part 2: " ++) . show $ res2

