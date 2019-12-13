import Data.List.Split
import qualified Data.Vector as V
import Data.Vector ((!?))
import Debug.Trace
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  input <- readFile "day_13_input.dat"
  putStrLn . show . solve0 . toVector $ input
  putStrLn . show . solve1 . toVector $ input

solve0 :: V.Vector Integer -> Int
solve0 v = M.size . M.filter (==2) . _tiles . fst. run $ (startGame, blankState {_code = v})

solve1 :: V.Vector Integer -> Integer
solve1 v = _score . fst . run $ (startGame, blankState {_code = v // (0, 2)})

run :: (Game, Comp) -> (Game, Comp)
run (g, c)
  -- The computer has stopped, so we are done
  | _stop c = (g, c)
  -- The computer is waiting for input, so provide some
  | _wait c = run (g, c {_input = [joyStick g], _wait = False})
  -- Not enough output, so go back for more
  | length (_output c) < 3 = run (g, runCode c)
  -- We have output, so advance the game and the computer
  | otherwise = run (nextGame g $ _output c, runCode $ c {_output = []})

nextGame :: Game -> [Integer] -> Game
nextGame g o
  -- Update score
  | (o!!0) == (-1) && (o!!1) == 0 = g {_score = o!!2}
  -- Update bat position
  | (o!!2) == 3 = g {_bat = o!!0}
  -- Update ball position
  | (o!!2) == 4 = g {_ball = o!!0}
  -- Update grid (not required for second part, but it's easier just to leave it here)
  | otherwise = g {_tiles = M.insert (o!!0, o!!1) (o!!2) (_tiles g)}

-- Move bat towards the ball
joyStick :: Game -> Integer
joyStick g = signum (_ball g - _bat g)

-- The internal state of the game
data Game = Game {
  _tiles::M.Map (Integer, Integer) Integer,
  _ball::Integer,
  _bat::Integer,
  _score::Integer
  }

-- Starting state of the robot
startGame = Game {
  _tiles = M.empty,
  _ball = 0,
  _bat  = 0,
  _score = 0
  }

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

-- The internal state of the computer
data Comp = Comp {
    _output::[Integer],      -- Output buffer
    _pc::Integer,            -- Program counter
    _code::V.Vector Integer, -- The program code (mutable)
    _input::[Integer],       -- Input buffer
    _wait::Bool,             -- Waiting for input flag
    _stop::Bool,             -- Stopped flag
    _rbo::Integer            -- Relative base offset value
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

toVector :: String -> V.Vector Integer
toVector = V.fromList . map read . splitOn ","
