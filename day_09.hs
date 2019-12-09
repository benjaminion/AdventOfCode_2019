import Data.List.Split
import qualified Data.Vector as V
import Data.Vector ((!?))
import Debug.Trace

main :: IO ()
main = do
  input <- readFile "day_09_input.dat"
  putStrLn . show . solve0 . toVector $ input
  putStrLn . show . solve1 . toVector $ input

solve0 :: V.Vector Integer -> [Integer]
solve0 v = _output . runCode $ blankState {_code = v, _input = [1]}

solve1 :: V.Vector Integer -> [Integer]
solve1 v = _output . runCode $ blankState {_code = v, _input = [2]}

blankState = Comp {
  _output = [],
  _pc = 0,
  _code = V.empty,
  _input = [],
  _stop = False,
  _rbo = 0
  }

---- The Intcode computer

runCode :: Comp -> Comp
runCode comp
  | code == 1  = runCode op1 -- add
  | code == 2  = runCode op2 -- mul
  | code == 3  = runCode op3 -- input
  | code == 4  = runCode op4 -- output
  | code == 5  = runCode op5 -- jump if nonzero
  | code == 6  = runCode op6 -- jump if zero
  | code == 7  = runCode op7 -- less than
  | code == 8  = runCode op8 -- equal to
  | code == 9  = runCode op9 -- adjust relative base offset
  | code == 99 = comp {_stop = True} -- stop
  | otherwise = trace (show comp) error "Error: undefined opcode"
  where
    o = _output comp
    p = _pc comp
    v = _code comp
    i = _input comp
    r = _rbo comp
    code = mod (v!p) 100
    flags = (\n -> mod (div (v!p) n) 10) <$> [100, 1000, 10000]
    param n
      | flags!!(n-1) == 0 = v!(p + fromIntegral n)
      | flags!!(n-1) == 1 = p + fromIntegral n
      | flags!!(n-1) == 2 = r + v!(p + fromIntegral n)
      | otherwise = trace (show comp) error "Error: undefined flag"
    op1 = comp {_pc = p + 4, _code = v // (param 3, v!(param 1) + v!(param 2))}
    op2 = comp {_pc = p + 4, _code = v // (param 3, v!(param 1) * v!(param 2))}
    op3 = comp {_pc = p + 2, _code = v // (param 1, head i), _input = tail i}
    op4 = comp {_pc = p + 2, _output = o ++ [v!(param 1)]}
    op5 = comp {_pc = if (v!(param 1) /= 0) then (v!(param 2)) else (p + 3)}
    op6 = comp {_pc = if (v!(param 1) == 0) then (v!(param 2)) else (p + 3)}
    op7 = comp {_pc = p + 4, _code = v // (param 3, if (v!(param 1) < v!(param 2)) then 1 else 0)}
    op8 = comp {_pc = p + 4, _code = v // (param 3, if (v!(param 1) == v!(param 2)) then 1 else 0)}
    op9 = comp {_pc = p + 2, _rbo = r + (v!(param 1))}

-- Safely retrieve values from the vector. Values beyond the end are 0.
(!) :: Integral a => V.Vector Integer -> a -> Integer
(!) v n = maybe 0 id $ v!?(fromIntegral n)

-- Return updated version of the vector, extending if required
(//) :: Integral a => V.Vector Integer -> (a, Integer) -> V.Vector Integer
(//) v (n, x)
  | nn < 0     = error "Error: Accessing negative memory."
  | nn >= lenv = V.concat [v, V.snoc (V.replicate (nn - lenv) 0) x]
  | otherwise  = V.unsafeUpd v [(nn, x)]
  where
    nn   = fromIntegral n
    lenv = V.length v

data Comp = Comp {
    _output::[Integer],
    _pc::Integer,
    _code::V.Vector Integer,
    _input::[Integer],
    _stop::Bool,
    _rbo::Integer
    } deriving (Show)

---- Process input

toVector :: String -> V.Vector Integer
toVector = V.fromList . map read . splitOn ","
