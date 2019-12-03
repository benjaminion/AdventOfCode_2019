{-
  The imperative for this one was to *not* use a grid structure (as I
  would have done naturally in C). Instead we use paths and intersect them.
  An important optimisation is to only intersect lists of points visited
  at the same Manhattan distance. Otherwise the intersection complexity is
  O(n*m) in terms of points visited by each path. This approach paid off in
  the second part of the puzzle, which became quite straightforward.
-}

import System.Environment
import Data.List
import Data.List.Split
import Data.Ord (comparing)

-- Specify the file name on the command line
main :: IO ()
main = do
  args <- getArgs
  input <- readFile (head args)
  putStr . show . solve0 $ input
  putStr "\n"
  putStr . show . solve1 $ input
  putStr "\n"

solve0 :: String -> Int
solve0 =  manhattan . head . points

solve1 :: String -> Int
solve1 = minimum . pathLengths

---- High-level structures

-- The (two) paths
paths :: String -> [[Point]]
paths = map makePath . map getMoves . getLists

-- The list of intersection points
points :: String -> [Point]
points = getCommonPoints . paths

-- The combined lengths of paths to each intersection
pathLengths :: String -> [Int]
pathLengths input = map sum . map (\p -> map (lenPath p) $ paths input) $ points input

---- Process Input

getLists :: String -> [String]
getLists = words

getMoves :: String -> [String]
getMoves = splitOn ","

---- Convert input to lists of points visited (paths)

makePath :: [String] -> [Point]
makePath moves = appendPath (0,0) moves

appendPath :: Point -> [String] -> [Point]
appendPath _     []     = []
appendPath start (x:xs) = (moves start x) ++ (appendPath (last newMoves) xs)
  where
    newMoves = moves start x

-- The list of squares visited in this move, starting from the given square
moves :: Point -> String -> [Point]
moves (x, y) move
  | d == 'R' = [(x + i, y) | i <- [1..n]]
  | d == 'L' = [(x - i, y) | i <- [1..n]]
  | d == 'U' = [(x, y + i) | i <- [1..n]]
  | d == 'D' = [(x, y - i) | i <- [1..n]]
  where
    d = head move
    n = read $ tail move

---- Intersect the paths to find the nearest intersection point

-- Find the common points in the sublists (at each Manhattan distance)
getCommonPoints :: [[Point]] -> [Point]
getCommonPoints = concat . filter (/= []) . commonPoints . map (bin)

commonPoints :: [[[Point]]] -> [[Point]]
commonPoints lists
  | length lists == 2 = zipWith (intersect) (lists!!0) (lists!!1)
  | otherwise = undefined

-- Split the list into sublists of the same Manhattan distance
-- Increasing by 1 each time
bin :: [Point] -> [[Point]]
bin = bin' 1 . sortManhattan

-- The input list must be sorted
bin' :: Int -> [Point] -> [[Point]]
bin' _ [] = []
bin' n x  = takeWhile distN x : (bin' (n + 1) $ dropWhile distN x)
  where
    distN z = n == manhattan z

---- Utilities

manhattan :: Point -> Int
manhattan (a, b) = (abs a) + (abs b)

sortManhattan :: [Point] -> [Point]
sortManhattan = sortBy (comparing manhattan)

-- Distance to get to intersection point p (including the point itself)
lenPath :: Point -> [Point] -> Int
lenPath p = (+ 1) . length . takeWhile (/= p)

---- Types

type Point = (Int, Int)
