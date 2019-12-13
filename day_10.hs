import Data.List

main :: IO ()
main = do
  input <- readFile "day_10_input.dat"
  putStrLn . show . solve0 . words $ input
  putStrLn . show . solve1 . words $ input

solve0 :: [String] -> Int
solve0 input = fst . findMaxVisible $ input

solve1 :: [String] -> Int
solve1 input = (\(x, y) -> x + 100 * y) . head . drop 199 . eliminate . order $ input

-- Order the asteroids by angle, taking each quadrant in turn
order :: [String] -> [[(Int, Int)]]
order input = map (fromRel . (sortBy ordDist)) . concat . fmap ($ (toRel . listCoords) input) $
  [
    groupBy eqAngle . map r2 . sortBy ordAngle . filter quadrant . map r0,
    groupBy eqAngle . map r1 . sortBy ordAngle . filter quadrant . map r1,
    groupBy eqAngle . map r0 . sortBy ordAngle . filter quadrant . map r2,
    groupBy eqAngle . map r3 . sortBy ordAngle . filter quadrant . map r3
  ]
  where
    base = (snd . findMaxVisible $ input)
    toRel = map (\(x, y) -> (x - fst base, y - snd base))
    fromRel = map (\(x, y) -> (x + (fst base), y + (snd base)))
    r0 (x, y) = (y, -x)  -- rotate -90 degrees
    r1 (x, y) = (x, y)   -- rotate 0 degrees
    r2 (x, y) = (-y, x)  -- rotate +90 degrees
    r3 (x, y) = (-x, -y) -- rotate 180 degrees
    quadrant (x, y) = x >= 0 && y > 0
    ordAngle (a, b) (c, d) = compare (a * d) (b * c)
    eqAngle (a, b) (c, d) = a * d == b * c
    ordDist (a, b) (c, d) = compare (abs a + abs b) (abs c + abs d)

-- Zap asteroids in turn based on the ordering above
eliminate :: [[(Int, Int)]] -> [(Int, Int)]
eliminate [] = []
eliminate list = head next : (eliminate $ tail list ++ tail' next)
  where
    next = head list
    tail' [] = []
    tail' x = [tail x]

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
