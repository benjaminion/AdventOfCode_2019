import Data.List.Split
import qualified Data.Vector as V
import Data.Vector ((!?))
import Debug.Trace
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  input <- readFile "day_11_input.dat"
  putStrLn . show . solve0 . toVector $ input
  putStr . solve1 . toVector $ input

solve0 :: V.Vector Integer -> Int
solve0 v = M.size . _panels . fst . run $ (startRobot,  blankState {_code = v, _input = [0]})

solve1 :: V.Vector Integer -> String
solve1 v = printOut . _panels . fst . run $ (startRobot,  blankState {_code = v, _input = [1]})

---- For part 2

-- "heuristically determined" dimensions of the output image
width = 40
size = 240

printOut :: M.Map (Integer, Integer) Integer -> String
printOut m = unlines . chunksOf width $ makeOutput (take size . repeat $ '.') white
  where
    -- All the panels painted white. We flip the y-coords for printing
    white = map (negate <$>) . M.keys . M.filter (== 1) $ m

makeOutput :: String -> [(Integer, Integer)] -> String
makeOutput s [] = s
makeOutput s l = makeOutput ((take idx s) ++ "#" ++ drop (idx + 1) s) $ tail l
  where
    idx = (\(x, y) -> (fromIntegral x) + width * (fromIntegral y)) . head $ l

---- For parts 1 and 2

run :: (Robot, Comp) -> (Robot, Comp)
run (r, c)
  -- The computer has stopped, so we are done
  | _stop c = (r, c)
  -- Not enough output, so go back for more
  | length (_output c) < 2 = run (r, runCode c)
  -- Advance the robot and the computer
  | otherwise = run (nextRobot, nextCode)
  where
    nextCode = runCode $ c {_input = maybe [0] (:[]) $ M.lookup (_pos nextRobot) (_panels r), _output = []}
    nextRobot = runRobot r $ _output c

runRobot :: Robot -> [Integer] -> Robot
runRobot r input
  | length input /= 2 = error $ "Robot input incorrect: " ++ show input
  | otherwise = r {_panels = newPanels, _dir = newDir, _pos = newPos}
  where
    -- Paint or repaint panel with the colour in the first place
    newPanels = M.insert (_pos r) (input!!0) (_panels r)
    -- 0 is up, 1 is right, 2 is down, 3 is left
    newDir = mod (_dir r + fromIntegral (2 * (input!!1) - 1)) 4
    --  +x is right, +y is up
    newPos
      | newDir == 0 = (\(x, y) -> (x, y + 1)) $ _pos r
      | newDir == 1 = (\(x, y) -> (x + 1, y)) $ _pos r
      | newDir == 2 = (\(x, y) -> (x, y - 1)) $ _pos r
      | newDir == 3 = (\(x, y) -> (x - 1, y)) $ _pos r

-- The internal state of the robot
data Robot = Robot {
  _panels::M.Map (Integer, Integer) Integer,
  _dir::Int,
  _pos::(Integer, Integer)
  } deriving Show

-- Starting state of the robot
startRobot = Robot {
  _panels = M.empty,
  _dir = 0,
  _pos = (0, 0)
  }

---- The Intcode computer

runCode :: Comp -> Comp
runCode comp
  | code == 1  = runCode op1 -- add
  | code == 2  = runCode op2 -- mul
  | code == 3  = runCode op3 -- input
  | code == 4  = op4         -- return output
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
    _output::[Integer],
    _pc::Integer,
    _code::V.Vector Integer,
    _input::[Integer],
    _stop::Bool,
    _rbo::Integer
    } deriving (Show)

-- Default blank state of the computer
blankState = Comp {
  _output = [],
  _pc = 0,
  _code = V.empty,
  _input = [],
  _stop = False,
  _rbo = 0
  }

---- Process input

toVector :: String -> V.Vector Integer
toVector = V.fromList . map read . splitOn ","
