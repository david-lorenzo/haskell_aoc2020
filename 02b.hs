import Data.List.Split
import Text.Read

data Policy = Policy {
    p1 :: Int,
    p2 :: Int,
    char :: Char,
    password :: String
} deriving (Show) 

-- main = interact ((++ "\n") . show . length . (filter (validate test1)) . (map parsePolicy) . lines)
main = interact ((++ "\n") . show . length . (filter (validate test2)) . (map parsePolicy) . lines)

parsePolicy :: String -> Maybe Policy
parsePolicy str = do
  let [params, char:_, password] = words str
  let [p1, p2] = splitOn "-" params
  Policy <$> readMaybe p1
         <*> readMaybe p2
         <*> pure char
         <*> pure password
  
validate :: (Policy -> Bool) -> Maybe Policy -> Bool
validate _ Nothing = False
validate test (Just p) = test p

test1 :: Policy -> Bool
test1 Policy {p1=x, p2=y, char=c, password=p} = x <= n && n <= y
  where n = length . (filter (== c)) $ p

test2 :: Policy -> Bool
test2 Policy {p1=x, p2=y, char=c, password=p} = 
  let c1 = p !! (x-1) == c
      c2 = p !! (y-1) == c
  in case (c1, c2) of
        (True, False) -> True
        (False, True) -> True
        (_    , _   ) -> False

