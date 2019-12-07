import Data.List
import Data.List.Split

main :: IO ()
main = do
  input <- readFile "day_06_input.dat"
  putStrLn . show . solve0 $ input
  putStrLn . show . solve1 $ input

solve0 :: String -> Int
solve0 = sum . map (subtract 1 . length) . orbitLists . dataFromInput

solve1 :: String -> Int
solve1 input = subtract 2 . length  $ (you \\ san) ++ (san \\ you)
  where
    you = head . filter (\x -> head x == "YOU") $ orbitLists i
    san = head . filter (\x -> head x == "SAN") $ orbitLists i
    i = dataFromInput input

-- Build the orbits from the root up as an optimisation
orbitLists :: [(String, String)] -> [[String]]
orbitLists input = orbitLists' input "COM"

orbitLists' :: [(String, String)] -> String -> [[String]]
orbitLists' input name = [name] : (map (++ [name]) . concat . map (orbitLists' input) $ orbiters)
  where
    orbiters = map snd . filter (\(x, y) -> x == name) $ input

dataFromInput :: String -> [(String, String)]
dataFromInput = map ((\x -> (x!!0, x!!1)) . splitOn ")") . words
