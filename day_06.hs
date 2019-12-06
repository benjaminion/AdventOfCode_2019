import System.Environment
import Data.List
import Data.List.Split

main :: IO ()
main = do
  input <- readFile "day_06_input.dat"
  putStr . show . solve0 $ input
  putStr "\n"
  putStr . show . solve1 $ input
  putStr "\n"

solve0 :: String -> Int
solve0 input = sum . map (subtract 1 . length . orbitList i . (\(a, b) -> b)) $ i
  where
    i = dataFromInput input

solve1 :: String -> Int
solve1 input = subtract 2 . length  $ (you \\ san) ++ (san \\ you)
  where
    you = orbitList i "YOU"
    san = orbitList i "SAN"
    i = dataFromInput input
  
-- This is **slow**. Memoisation would make it super-fast
orbitList :: [(String, String)] -> String -> [String]
orbitList list name
  | name == "COM" = [name]
  | otherwise = name : (orbitList list $ fst . head $ (filter (\(x, y) -> y == name)) list)

dataFromInput :: String -> [(String, String)]
dataFromInput = map (\x -> (x!!0, x!!1)) . map (splitOn ")") . words
