{-
  I'm sure there's a sweet and elegant Haskelly way to do this, but I struggled
  mightily to find anything that worked, never mind good looking.
  Might revisit this one day with more experience.
-}

import Data.List
import Data.List.Split
import qualified Data.Vector as V
import Data.Vector ((//))
import Data.Vector ((!))
import Debug.Trace

main :: IO ()
main = do
  input <- readFile "day_07_input.dat"
  putStr . show . solve0 . toVector $ input
  putStr "\n"
  putStr . show . solve1 . repeat . toVector $ input
  putStr "\n"

---- First part

solve0 :: V.Vector Int -> Int
solve0 v = maximum . map (trial0 v 0) . permutations $ ([0..4]::[Int])

trial0 :: V.Vector Int -> Int -> [Int] -> Int
trial0 v input phases = foldl (run v) 0 phases
  where
    run v input phase = _output . runCode $ Amp 0 0 v [phase, input] False

---- Second part

solve1 :: [V.Vector Int] -> Int
solve1 vs = maximum . map (trial1 0 . zipWith id (map makeAmp vs)) . permutations $ ([5..9]::[Int])
  where
    makeAmp v input = Amp 0 0 v [input] False

trial1 :: Int -> [Amp] -> Int
trial1 input amps
  | _stop amp == True = input
  | otherwise = trial1 (_output updatedAmp) ((tail amps) ++ [updatedAmp])
  where
    amp = head amps
    updatedAmp = runCode $ addInput input amp 
    addInput input amp = amp {_input = _input amp ++ [input]}

---- The Intcode computer

runCode:: Amp -> Amp
runCode amp
  | code == 1  = runCode op1
  | code == 2  = runCode op2
  | code == 3  = runCode op3
  | code == 4  = op4          -- return when we have ouput
  | code == 5  = runCode op5
  | code == 6  = runCode op6
  | code == 7  = runCode op7
  | code == 8  = runCode op8
  | code == 99 = Amp o p v i True
  | otherwise = trace (show amp) undefined
  where
    o = _output amp
    p = _pc amp
    v = _code amp
    i = _input amp
    s = _stop amp
    code = mod (v!p) 100
    flags = div (v!p) 100
    param1 = if (mod flags 10 == 0) then v!(v!(p+1)) else v!(p+1)
    param2 = if (div flags 10 == 0) then v!(v!(p+2)) else v!(p+2)
    op1 = Amp o (p + 4) (v // [(v!(p+3), param1 + param2)]) i s
    op2 = Amp o (p + 4) (v // [(v!(p+3), param1 * param2)]) i s
    op3 = Amp o (p + 2) (v // [(v!(p+1), head i)]) (tail i) s
    op4 = Amp param1 (p + 2) v i s
    op5 = Amp o (if (param1 /= 0) then param2 else (p + 3)) v i s
    op6 = Amp o (if (param1 == 0) then param2 else (p + 3)) v i s
    op7 = Amp o (p + 4) (v // [(v!(p+3), if (param1 < param2) then 1 else 0)]) i s
    op8 = Amp o (p + 4) (v // [(v!(p+3), if (param1 == param2) then 1 else 0)]) i s

---- Misc

toVector :: String -> V.Vector Int
toVector = V.fromList . map read . splitOn ","

data Amp = Amp {
    _output::Int,
    _pc::Int,
    _code::V.Vector Int,
    _input:: [Int],
    _stop:: Bool
    } deriving (Show)
