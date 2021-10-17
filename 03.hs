import System.Environment

main = do
    inputFilePath <- head <$> getArgs
    inputData <- lines <$> readFile inputFilePath
    let a = f 1 1 inputData
    let b = f 3 1 inputData
    let c = f 5 1 inputData
    let d = f 7 1 inputData
    let e = f 1 2 inputData
    putStrLn $ "First part solution: " ++ show b
    putStrLn $ "(1, 1): " ++ show a
    putStrLn $ "(3, 1): " ++ show b
    putStrLn $ "(5, 1): " ++ show c
    putStrLn $ "(7, 1): " ++ show d
    putStrLn $ "(1, 2): " ++ show e
    putStrLn $ "Second part solution: " ++ (show $ foldr (*) 1 [a, b, c, d, e])
  where
    f dx dy = length . filter (== '#') . tail . moves dx dy 

moves :: Int -> Int -> [String] -> String
moves dx dy inp = map (\ (x,y) -> (inp !! y) !! (x `rem` width)) $ zip horizontal vertical
  where height = length inp
        width = length $ head inp
        horizontal = [0,dx..]
        vertical = [0,dy..(height-1)]
