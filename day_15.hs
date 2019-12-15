import Data.List.Split
import qualified Data.Vector as V
import Data.Vector ((!?))
import Debug.Trace
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  input <- readFile "day_15_input.dat"
  putStrLn . show . solve0 . toVector $ input
  putStrLn . show . solve1 . toVector $ input

solve0 :: Code -> Int
solve0 = snd . oxygen

solve1 :: Code -> Int
solve1 = flood1 M.empty . return . fst . oxygen

oxygen :: Code -> ((Comp, Coord), Int)
oxygen v = flood0 M.empty [(blankState {_code = v}, (0, 0))]

---- First part

-- The shortest path from (0, 0) to the oxygen (uses flood fill/BFS)
-- Return the computer's state as input for the second part
flood0 :: Visited -> [(Comp, Coord)] -> ((Comp, Coord), Int)
flood0 m xs
  | any (==[2]) . map (_output . fst) $ xs = (head . dropWhile (\(c, p) -> _output c /= [2]) $ xs, 0)
  | otherwise = (+1) <$> flood0 (markVisited m (map snd xs)) (visitNext m xs)

---- Second part

-- Find the minimum steps to flood fill the cavern
flood1 :: Visited -> [(Comp, Coord)] -> Int
flood1 m xs
  | null xs = -1    -- We've gone a step too far, so return one less
  | otherwise = 1 + flood1 (markVisited m (map snd xs)) (visitNext m xs)

---- Common

type Coord = (Int, Int)
type Visited = M.Map Coord Bool

markVisited :: Visited -> [Coord] -> Visited
markVisited = foldr (\p -> M.insert p True)

visitNext :: Visited -> [(Comp, Coord)] -> [(Comp, Coord)]
visitNext m = filter (\(c, p) -> notWall c && notVisited p) . concat . map (\(c, p) -> map (update (c, p)) [1..4])
  where
    notWall c = _output c /= [0]
    notVisited p = M.lookup p m == Nothing

-- Run the code for the given direction, and update our coordinates
update :: (Comp, Coord) -> Int -> (Comp, Coord)
update (c, p) dir = (runCode $ c {_input = [fromIntegral dir], _output = []}, newPos p dir)

newPos :: Coord -> Int -> Coord
newPos (x, y) 1 = (x, y + 1) -- North
newPos (x, y) 2 = (x, y - 1) -- South
newPos (x, y) 3 = (x - 1, y) -- West
newPos (x, y) 4 = (x + 1, y) -- East

---- The Intcode computer

runCode :: Comp -> Comp
runCode comp
  | code == 1  = runCode op1 -- add
  | code == 2  = runCode op2 -- mul
  | code == 3  = if null i                -- input buffer is empty
                 then comp {_wait = True} -- request input
                 else runCode op3         -- process input
  | code == 4  = op4         -- output
  | code == 5  = runCode op5 -- jump if nonzero
  | code == 6  = runCode op6 -- jump if zero
  | code == 7  = runCode op7 -- less than
  | code == 8  = runCode op8 -- equal to
  | code == 9  = runCode op9 -- adjust relative base offset
  | code == 99 = comp {_stop = True} -- stop
  | otherwise = trace (show comp) error "Error: undefined opcode"
  where
    (o, p, v, i, r) = (_output comp, _pc comp, _code comp, _input comp, _rbo comp)
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
(!) :: Integral a => Code -> a -> Integer
(!) v n = maybe 0 id $ v!?(fromIntegral n)

-- Return updated version of the vector, extending if required
(//) :: Integral a => Code -> (a, Integer) -> Code
(//) v (n, x)
  | nn < 0     = error "Error: Accessing negative memory."
  | nn >= lenv = V.concat [v, V.snoc (V.replicate (nn - lenv) 0) x]
  | otherwise  = V.unsafeUpd v [(nn, x)]
  where
    nn   = fromIntegral n
    lenv = V.length v

type Code = V.Vector Integer

-- The internal state of the computer
data Comp = Comp {
    _output::[Integer], -- Output buffer
    _pc::Integer,       -- Program counter
    _code::Code,        -- The program code (mutable)
    _input::[Integer],  -- Input buffer
    _wait::Bool,        -- Waiting for input flag
    _stop::Bool,        -- Stopped flag
    _rbo::Integer       -- Relative base offset value
    } deriving (Show)

-- Default blank state of the computer
blankState = Comp {
  _output = [],
  _pc = 0,
  _code = V.empty,
  _input = [],
  _wait = False,
  _stop = False,
  _rbo = 0
  }

---- Process input

toVector :: String -> Code
toVector = V.fromList . map read . splitOn ","
