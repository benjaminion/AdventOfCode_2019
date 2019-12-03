import System.Environment
import Data.List.Split
import qualified Data.Vector as V
import Data.Vector ((//))
import Data.Vector ((!))

-- Specify the file name on the command line
main :: IO ()
main = do
  args <- getArgs
  input <- readFile (head args)
  putStr . show . solve0 . toVector $ input
  putStr "\n"
  putStr . show . solve1 . toVector $ input
  putStr "\n"

solve0 :: V.Vector Int -> Int
solve0 = V.head . runCode 0 . modifyInput (12, 02)

-- Lazy evaluation FTW!
solve1 :: V.Vector Int -> Int
solve1 v = parseResult . head . filter (/= Nothing) . map (\x -> trial x v) $ nounVerbs

parseResult :: Maybe (Int, Int) -> Int
parseResult (Just (a, b)) = 100 * a + b
parseResult _ = undefined

trial :: (Int, Int) -> V.Vector Int -> Maybe (Int, Int)
trial x v
  | result == target = Just x
  | otherwise        = Nothing
  where
    result = V.head . runCode 0 $ modifyInput x v

modifyInput :: (Int, Int) -> V.Vector Int -> V.Vector Int
modifyInput (a,b) v = v // [(1,a), (2,b)]

runCode:: Int -> V.Vector Int -> V.Vector Int
runCode i v
  | v!i == 1  = runCode (i + 4) $ op1 i v
  | v!i == 2  = runCode (i + 4) $ op2 i v
  | v!i == 99 = v
  | otherwise = undefined
  where
    op1 i v = v // [(v!(i+3), (v!(v!(i+1)) + v!(v!(i+2))))]
    op2 i v = v // [(v!(i+3), (v!(v!(i+1)) * v!(v!(i+2))))]

-- The list of all combinations of noun and verb to try
nounVerbs :: [(Int, Int)]
nounVerbs = [(x, y) | x <- [0..99], y <- [0..99]]

-- Our target value to solve for
target :: Int
target = 19690720

---- Utilities

toInts :: String -> [Int]
toInts = map read . splitOn ","

toVector :: String -> V.Vector Int
toVector = V.fromList . toInts
