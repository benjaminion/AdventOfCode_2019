import System.Environment

main :: IO ()
main = do
  input <- readFile "day_01_input.dat"
  putStr . show . solve0 . toInts $ input
  putStr "\n"
  putStr . show . solve1 . toInts $ input
  putStr "\n"

toInts :: String -> [Integer]
toInts = map read . words

solve0 :: [Integer] -> Integer
solve0 [] = 0
solve0 (x:xs)  = (div x 3) - 2 + (solve0 xs)

solve1 :: [Integer] -> Integer
solve1 = sum . map fuel

-- Love those infinite lists!
fuel :: Integer -> Integer
fuel = sum . takeWhile (> 0) . tail . iterate (\x -> div x 3 - 2)
