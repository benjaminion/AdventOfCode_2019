{-
  This is the mess that got me the correct answers. I might come back and
  make it prettier later. This took far too long.
  Note that x and y are transposed!
-}

import Data.List
import Data.Ord (comparing)
import Data.Ratio
import Data.Function

main :: IO ()
main = do
  input <- readFile "day_10_input.dat"
  putStrLn . show . solve0 . words $ input
  putStrLn . show . solve1 . words $ input

solve0 :: [String] -> Int
solve0 input = fst . findMaxVisible $ input

solve1 :: [String] -> Int
solve1 input = (\(x, y) -> x + 100 * y) . head . drop 199 . eliminate . order $ input

-- Order the asteroids, taking each quadrant in turn
order :: [String] -> [[(Int, Int)]]
order input = map (map (\(x, y) -> (x + fst base, y + snd base)))
  . map (sortBy (comparing (\(x,y) -> (+) (abs x) (abs y))))
  . concat . fmap ($ relCoords) $
  [
    groupBy ((==) `on` f1) . sortBy (comparing f1) . filter (\(x,y) -> y >= 0 && x < 0),
    groupBy ((==) `on` f2) . sortBy (comparing f2) . filter (\(x,y) -> y > 0 && x >= 0),
    groupBy ((==) `on` f3) . sortBy (comparing f3) . filter (\(x,y) -> y <= 0 && x > 0),
    groupBy ((==) `on` f4) . sortBy (comparing f4) . filter (\(x,y) -> y < 0 && x <= 0)
  ]
  where
    relCoords = map (\(x, y) -> (x - (fst base), y - (snd base))) $ listCoords  input
    base = (snd . findMaxVisible $ input)
    f1 (x, y) = y % (-x)
    f2 (x, y) = x % y
    f3 (x, y) = (-y) % x
    f4 (x, y) = (-x) % (-y)

-- Zap asteroids in turn
eliminate :: [[(Int, Int)]] -> [(Int, Int)]
eliminate [] = []
eliminate list = head next : (eliminate $ if tail next == [] then (tail list) else (tail list) ++ [(tail next)])
  where next = head list

-- The asteroid with the greatest visibility
findMaxVisible :: [[Char]] -> (Int, (Int, Int))
findMaxVisible input = (maxvis, snd . head . dropWhile (\(n,_) -> n /= maxvis) $ visibility input)
  where
    maxvis = maximum . map fst . visibility $ input

visibility :: [[Char]] -> [(Int, (Int, Int))]
visibility input = zip (map (countVisible input coords) $ coords) coords
  where
    coords = listCoords input

-- List of coords of the asteroids
listCoords :: [[Char]] -> [(Int, Int)]
listCoords input = map (\x -> divMod x width) . elemIndices '#' . concat $ input
  where
    width = length . head $ input

-- Subtract 1 so I don't see myself
countVisible :: [[Char]] -> [(Int, Int)] -> (Int, Int) -> Int
countVisible grid coords pos = subtract 1 . length . filter (id) . map (visibleFrom grid pos) $ coords

visibleFrom :: [[Char]] -> (Int, Int) -> (Int, Int) -> Bool
visibleFrom grid p1 p2 = null . filter (\(x, y) -> grid!!x!!y == '#') $ inbetweenPoints p1 p2

inbetweenPoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
inbetweenPoints (x0, y0) (x1, y1) = [(x0 + n * ddx, y0 + n * ddy) | n <- [1..(g-1)]]
  where
    (dx, dy) = (x1 - x0, y1 - y0)
    g = gcd dx dy
    (ddx, ddy) = (div dx g, div dy g)
