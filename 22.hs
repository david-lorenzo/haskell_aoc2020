import System.Environment
import Data.Bool
import Data.List.Split
import Data.Sequence
import qualified Data.Set as S
import qualified Data.Sequence as Q

play1 p1 p2 | Q.null p1 = p2
            | Q.null p2 = p1
            | otherwise = play1 newp1 newp2
 where (h1 :<| t1) = p1
       (h2 :<| t2) = p2
       p1wins      = h1 > h2
       newp1       = bool t1 (t1 |> h1 |> h2) p1wins
       newp2       = bool t2 (t2 |> h2 |> h1) (not p1wins)


rplay mem p1 p2 | S.member (p1, p2) mem    = (p1, Q.empty)
                | (Q.null p1 || Q.null p2) = (p1, p2)
                | otherwise                = rplay newmem newp1 newp2
  where (h1 :<| t1) = p1
        (h2 :<| t2) = p2
        r1          = h1 <= Q.length t1
        r2          = h2 <= Q.length t2
        recurse     = r1 && r2
        (q1, q2)    = rplay S.empty (Q.take h1 t1) (Q.take h2 t2)
        p1wr        = not . Q.null $ q1
        p1wnr       = h1 > h2
        p1wins      = bool p1wnr p1wr recurse
        newp1       = bool t1 (t1 |> h1 |> h2) p1wins
        newp2       = bool t2 (t2 |> h2 |> h1) (not p1wins)
        newmem      = S.insert (p1, p2) mem


calc_result len acc idx val = acc + val*(len - idx)


main = do
  inputFilePath <- head <$> getArgs
  [inp1, inp2] <- splitOn "\n\n" <$> readFile inputFilePath
  let p1 = Q.fromList . map read . tail . lines $ inp1 :: Q.Seq Int
  let p2 = Q.fromList . map read . tail . lines $ inp2 :: Q.Seq Int

  -- Part 1
  let qres1 = play1 p1 p2
  let lres1 = Q.length qres1
  let res1  = Q.foldlWithIndex (calc_result lres1) 0 qres1
  putStrLn . ("Part 1: " ++) . show $ res1

  -- Part 2
  let (q1, q2) = rplay S.empty p1 p2
  let qres2 = bool q1 q2 (Q.null q1)
  let lres2 = Q.length qres2
  let res2  = Q.foldlWithIndex (calc_result lres2) 0 qres2
  putStrLn . ("Part 2: " ++) . show $ res2

