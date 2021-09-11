module Main where
import Data.List

main :: IO ()
main = do
    print $ detLeibniz [[1,2],[3,4]]
    print $ piLeibniz 1000

detLeibniz :: (Num a) => [[a]] -> a
detLeibniz a = sum [sgn sigma * product [a !! (sigma !! i) !! i | i <- [0..n-1]] | sigma <- sym n]
  where
    n = length a
    sgn sigma = (-1) ^ inv sigma
    inv [] = 0
    inv sigma = length (filter (< head sigma) (tail sigma)) + inv (tail sigma)
    sym n = permutations [0..n-1]

piLeibniz :: Int -> Double
piLeibniz terms = (*4) $ sum $ take terms series
  where
    series = map (\x -> numer x / denom x) [0..]
    numer x = fromIntegral $ if even x then 1 else -1
    denom x = fromIntegral $ 2*x + 1
