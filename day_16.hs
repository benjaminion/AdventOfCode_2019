import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Function (on)
import Debug.Trace

main :: IO ()
main = do
  input <- readFile "day_16_input.dat"
  putStrLn . solve0 $ input
  putStrLn . solve1 $ input

---- First part

-- This is slow, but it is elegant!
solve0 :: [Char] -> [Char]
solve0 = concat. map show . take 8 . head . drop 100 . iterate fft . map read . chunksOf 1

fft :: [Int] -> [Int]
fft xs = zipWith getDigit mask . replicate (length xs) $ xs

getDigit :: [Int] -> [Int] -> Int
getDigit ms = (\n -> mod (abs n) 10) . sum . zipWith (*) ms

-- An infinite list of infinite lists...
mask :: [[Int]]
mask = [tail . cycle $ (replicate n 0 ++ replicate n 1 ++ replicate n 0 ++ replicate n (-1)) | n <- [1..]]

---- Second part

{-
We're going to rely on three things.
  (1) a digit only depends on the values of digits that come after it
  (2) digits in the second half are easy to compute directly
  (3) our input indicates that the digits we want are in the second half
-}

solve1 :: [Char] -> [Char]
solve1 input = concat. map show . take 8 . reverse . iterateFft 100 . reverse $ digits
  where
    start = (read . take 7 $ input)::Int
    digits = map read . chunksOf 1 . drop start . concat . replicate 10000 $ input

-- Using scanl' on the reversed list is faster than using scanr, but it's still slow
-- All numbers are +ve so skip the `abs` - this doubles the speed.
iterateFft :: Int -> [Int] -> [Int]
iterateFft 0 xs = xs
iterateFft n xs = iterateFft (n - 1) . map (`mod` 10) . tail . scanl' (+) 0 $ xs
