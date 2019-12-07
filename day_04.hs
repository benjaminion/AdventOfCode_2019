import Data.List

main :: IO ()
main = do
  putStrLn . show $ solve0
  putStrLn . show $ solve1

solve0 :: Int
solve0 =  length . filter hasDouble . filter isMonotonic $ map show [lo..hi]

solve1 :: Int
solve1 =  length . filter noGroups . filter isMonotonic $ map show [lo..hi]

-- Digits are non-decreasing
isMonotonic :: [Char] -> Bool
isMonotonic xs = and $ zipWith (<=) xs (tail xs)

-- There are double digits
hasDouble :: [Char] -> Bool
hasDouble xs = any (>=2) . map length $ group xs

-- The double digits are not part of a larger group
noGroups :: [Char] -> Bool
noGroups xs = any (==2) . map length $ group xs

lo :: Int
lo = 171309

hi :: Int
hi = 643603
