import System.Environment
import Data.List.Split
import qualified Data.Vector as V
import Data.Vector ((//))
import Data.Vector ((!))
import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  input <- readFile "day_05_input.dat"
  putStr . show . solve 1 $ toVector input
  putStr "\n"
  putStr . show . solve 5 $ toVector input
  putStr "\n"

solve :: Int -> V.Vector Int -> [Int]
solve system v = (\(a, _ , _) -> a) . runCode system $ ([], 0, v)

runCode:: Int -> ([Int], Int, V.Vector Int) -> ([Int], Int, V.Vector Int)
runCode system (output, i, v)
  | code == 1  = runCode system op1
  | code == 2  = runCode system op2
  | code == 3  = runCode system op3
  | code == 4  = runCode system op4
  | code == 5  = runCode system op5
  | code == 6  = runCode system op6
  | code == 7  = runCode system op7
  | code == 8  = runCode system op8
  | code == 99 = (output, i, v)
  | otherwise = trace ((show i) ++ ":" ++ (show (v!i))) undefined
  where
    code = mod (v!i) 100
    flags = div (v!i) 100
    param1 = if (mod flags 10 == 0) then v!(v!(i+1)) else v!(i+1)
    param2 = if (div flags 10 == 0) then v!(v!(i+2)) else v!(i+2)
    op1 = (output, i + 4, v // [(v!(i+3), param1 + param2)])
    op2 = (output, i + 4, v // [(v!(i+3), param1 * param2)])
    op3 = (output, i + 2, v // [(v!(i+1), system)])
    op4 = (output ++ [param1], i + 2, v)
    op5 = (output, if (param1 /= 0) then param2 else (i + 3), v)
    op6 = (output, if (param1 == 0) then param2 else (i + 3), v)
    op7 = (output, i + 4, v // [(v!(i+3), if (param1 < param2) then 1 else 0)])
    op8 = (output, i + 4, v // [(v!(i+3), if (param1 == param2) then 1 else 0)])

toInts :: String -> [Int]
toInts = map read . splitOn ","

toVector :: String -> V.Vector Int
toVector = V.fromList . toInts
