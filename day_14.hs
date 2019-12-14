import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Function (on)

main :: IO ()
main = do
  input <- readFile "day_14_input.dat"
  putStrLn . show . solve0 $ input
  putStrLn . show . solve1 $ input

trillion = 1000000000000::Int

-- Ore required to make one unit of fuel
solve0 :: String -> Int
solve0 input = calcOre (toMap input) 1

-- Binary search an interval for the solution
solve1 :: String -> Int
solve1 input = fst $ binSearch reactions (start, calcOre reactions start) (2 * start, 2 * (calcOre reactions start))
  where
    reactions = toMap input
    start = div trillion (calcOre reactions 1)

---- For first and second parts

-- The minimum ore needed to make n units of fuel
calcOre :: Reactions -> Int -> Int
calcOre reactions n = snd . head . filter ((=="ORE") . fst) . doIt reactions . doReaction reactions $ ("FUEL", n)

-- Iterate through the reactions until we need only ore
doIt :: Reactions -> [Reactant] -> [Reactant]
doIt reactions reactants
  | (length . filter ((>0) . snd) $ reactants) == 1 = reactants
  | otherwise = doIt reactions . deDupe . concat $ map (doReaction reactions) reactants

-- Find out how to make reactant r. Track any excess made with a negative amount.
doReaction :: Reactions -> Reactant -> [Reactant]
doReaction reactions (r, n) = (r, mm) : (map ((*nn) <$>) reactants)
  where
    lookup = maybe undefined id . M.lookup r $ reactions
    reactants = snd lookup
    m  = fst lookup        -- The multiplier on the RHS of the reaction
    nn = 1 + div (n - 1) m -- The number of times we need this reaction
    mm = n - nn * m        -- The surplus of the reactant (negative)

-- Sum up multiple quantities of the same reactant
deDupe :: [Reactant] -> [Reactant]
deDupe reactants =  map (\xs -> (fst . head $ xs, sum . map snd $ xs)) . groupBy ((==) `on` fst) . sortOn fst . filter ((/=0) . snd) $ reactants

---- For second part

binSearch :: Reactions -> (Int, Int) -> (Int, Int) -> (Int, Int)
binSearch reactions (a, fa) (b, fb)
  | fa > trillion || fb < trillion = error $ "Search interval does not contain solution" ++ (show (a, fa)) ++ (show (b, fb))
  | b == a + 1 = (a, fa)
  | fn <= trillion = binSearch reactions (n, fn) (b, fb)
  | fn > trillion = binSearch reactions (a, fa) (n, fn)
  where
    n = div (a + b) 2
    fn = calcOre reactions n

---- Process input

-- (name, quantity)
type Reactant = (String, Int)
-- M.Map name (multiplier, [reactants])
type Reactions = M.Map String (Int, [Reactant])

toMap :: String -> Reactions
toMap = foldr insertLine (M.insert "ORE" (1, [("ORE", 1)]) M.empty)  . lines

insertLine :: String -> Reactions -> Reactions
insertLine line reactions = M.insert (fst output) (snd output, inputs) reactions
  where
    sides = splitOn "=>" line
    output = getReactant . head . tail $ sides
    inputs = map getReactant . splitOn "," . head $ sides
    getReactant x = (head . tail . words $ x, read . head . words $ x)
