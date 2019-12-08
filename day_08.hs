import Data.List
import Data.List.Split

main :: IO ()
main = do
  input <- readFile "day_08_input.dat"
  putStrLn . show . solve0 $ input
  putStr . solve1 $ input

width  = 25
height = 6
size   = (*) width height
unJust = maybe undefined id

solve0 :: String -> Int
solve0 input = (*) (count '1' minLayer) (count '2' minLayer)
  where
    layers = chunksOf size input
    count c = length . filter (==c)
    numZeros = map (count '0') layers
    minLayer = layers !! (unJust . elemIndex (minimum $ numZeros) $ numZeros)

solve1 :: String -> String
solve1 = unlines . chunksOf width . map (display . head . dropWhile (=='2')) . transpose . chunksOf size
  where
    display = \c -> if c == '1' then '*' else ' '
