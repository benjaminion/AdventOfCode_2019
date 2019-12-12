import Data.List
import Data.List.Split (chunksOf)
import Control.Monad (ap)

main :: IO ()
main = do
  input <- readFile "day_12_input.dat"
  putStrLn . show . solve0 . transpose . map readInputLine $ lines input
  putStrLn . show . solve1 . transpose . map readInputLine $ lines input

solve0 :: [[Int]] -> Int
solve0 input = energy . map (head . drop 1000 . coords) $ zip input (zeroVelocity input)

solve1 :: [[Int]] -> Int
solve1 input = foldr lcm 1 $ map (firstRepeat . coords) $ zip input (zeroVelocity input)

-- An infinite list of the future positions and velocities on one axis.
coords :: ([Int], [Int]) -> [([Int], [Int])]
coords (xs, vs) = (xs, vs) : coords (xs', vs')
  where
    vs' = zipWith (+) vs $ velocityDeltas xs
    xs' = zipWith (+) xs vs'

-- The change in velocity give the positions
velocityDeltas :: [Int] -> [Int]
velocityDeltas x = map (sum . (\ b -> [signum (a - b) | a <- x])) x

-- Given all the coords and the velocities, return the total "energy"
energy :: [([Int], [Int])] -> Int
energy zs = sum $ zipWith (*) (sumabs . fst . unzip $ zs) (sumabs . snd . unzip $ zs)
  where
    sumabs = map (sum . map abs) . transpose

-- Assume that the repeat is of the first position. I don't know if that can be proved.
-- Experimenting with "point free" here!
firstRepeat :: (Eq a) => [a] -> Int
firstRepeat = (1 +) . maybe undefined id . ap (findIndex . (==) . head) tail

-- The initial velocities (zero)
zeroVelocity :: [[Int]] -> [[Int]]
zeroVelocity = map (map (\x -> 0))

-- Quick and dirty input parsing
readInputLine :: String -> [Int]
readInputLine line = read $ "[" ++ (filter (`notElem` "<>xyz= ") line) ++ "]"
