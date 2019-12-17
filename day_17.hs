import Data.List
import Data.List.Split
import qualified Data.Vector as V
import Data.Vector ((!?))
import Debug.Trace
import qualified Data.Map.Strict as M
import Data.Char (chr, ord)
import Data.Tuple (swap)

main :: IO ()
main = do
  input <- readFile "day_17_input.dat"
  putStrLn . show . solve0 . toVector $ input
  putStrLn . cheat . toVector $ input
  putStrLn . show . solve1 . toVector $ input

solve0 :: Code -> Int
solve0 input = intersections . scaffold $ input

solve1 :: Code -> Integer
solve1 input = doIt $ input // (0, 2)

cheat :: Code -> String
cheat input = concat . getRoute . scaffold $ input

---- Common

type Point = (Int, Int)
type View = M.Map Point Char

-- A map of just the scaffold points indexed by (x, y), zero-based
scaffold :: Code -> View
scaffold input = M.fromList $ [(swap (divMod n width), c) | (c, n) <- filter (not . (`elem` ".\n") . fst) $ zip rawView [0..]]
  where
    rawView = map (chr . fromIntegral) . _output . runToStop $ blankState { _code = input }
    width = (+1) . length . head . lines $ rawView

---- First part

intersections :: View -> Int
intersections v = sum . map (\(x, y) -> x * y) . filter (isIntersection v) . M.keys $ v

isIntersection :: View -> Point -> Bool
isIntersection v (x, y) = all (\p -> (M.lookup p v) /= Nothing) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

---- Second part

-- Yep, I cheated! This is hand-constructed from the output of "cheat" above.
-- Real solution is a WIP to come later
instructions = map (fromIntegral. ord) . concat $
  [
    "A,B,A,C,A,B,C,B,C,B\n",
    "L,10,R,8,L,6,R,6\n",
    "L,8,L,8,R,8\n",
    "R,8,L,6,L,10,L,10\n",
    "n\n"
  ]

doIt :: Code -> Integer
doIt input = head . reverse . _output . runToStop $ blankState {_code = input, _input = instructions}

---- The rest is used by cheat, but will be used to get the proper solution

getRoute :: View -> [String]
getRoute v = map format . group . map (_move) . tail . takeWhile (not . _end) . iterate (doMove . nextMove v) $ Robot { _pos = pos, _dir = dir, _move = 'X', _end = False}
  where
    (pos, dir) = robotStart v
    format xs
      | length xs > 1 = show $ length xs
      | otherwise = xs

robotStart :: View -> (Point, Dir)
robotStart v =  (pos, dir)
  where
    pos = fst . head . filter ((/= '#') . snd) $ M.toList v
    dir = getDir . maybe undefined id $ M.lookup pos v
    getDir '^' = U
    getDir '<' = L
    getDir '>' = R
    getDir 'v' = D

turnR :: Dir -> Dir
turnR L = U
turnR d = succ d

turnL :: Dir -> Dir
turnL U = L
turnL d = pred d

doMove :: Robot -> Robot
doMove r
  | _move r == 'F' = r {_pos = nextPos (_pos r) (_dir r)}
  | _move r == 'L' = r {_dir = turnL (_dir r)}
  | _move r == 'R' = r {_dir = turnR (_dir r)}
  | _move r == 'E' = r {_end = True}

-- Fails when initially pointing in the opposite direction ¯\_(ツ)_/¯
-- And we are assuming a gap of at least one between paths
nextMove :: View -> Robot -> Robot
nextMove v r
  -- Point ahead is scaffold, so move forward
  | M.lookup (nextPos p d) v == Just '#' = r {_move = 'F'}
  -- Point to the left is scaffold, so turn
  | M.lookup (nextPos p (turnL d)) v == Just '#' = r {_move = 'L'}
  -- Point to the left is scaffold, so turn
  | M.lookup (nextPos p (turnR d)) v == Just '#' = r {_move = 'R'}
  -- The end of the road
  | otherwise = r {_move = 'E'}
  where
    (p, d) = (_pos r, _dir r)

nextPos :: Point -> Dir -> Point
nextPos (x, y) U = (x, y - 1)
nextPos (x, y) R = (x + 1, y)
nextPos (x, y) D = (x, y + 1)
nextPos (x, y) L = (x - 1, y)

data Dir = U | R | D | L deriving (Enum, Show)

data Robot = Robot {
  _pos :: Point,
  _dir :: Dir,
  _move :: Char,
  _end :: Bool
  }

---- The Intcode computer

runToStop :: Comp -> Comp
runToStop = head . dropWhile (not . _stop) . iterate runCode

runToWait :: Comp -> Comp
runToWait = head . dropWhile (not . _wait) . iterate runCode

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
