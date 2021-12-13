module Main where

import Control.Arrow ((&&&), first, second)
import Control.Monad (ap)
import Data.Bifunctor (bimap)
import Data.Foldable (foldl', foldl1, for_)
import Data.Functor ((<&>), void)
import Data.List ((\\), elemIndex, group, intersect, nub, sort, sortOn, transpose, union)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Tuple.Extra (both)
import System.IO (withFile, IOMode(..))
import Util (count)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Parse (PaperFold(..), PilotCommand(..))
import qualified Parse

fromBase :: (Eq a, Show a) => [a] -> [a] -> Int
fromBase digits = foldl' (\n d -> n * length digits + theIndexIn digits d) 0

mid :: [a] -> a
mid xs = xs !! (length xs `div` 2)

the :: (Eq a, Show a) => [a] -> a
the (x:xs) | all (==x) xs = x
the xs = error $ "ambiguous: " <> show xs

theIndexIn :: (Eq a, Show a) => [a] -> a -> Int
theIndexIn xs x = fromMaybe (error $ show x <> " not found in " <> show xs) $ elemIndex x xs

toSnd :: (a -> b) -> a -> (a, b)
toSnd f x = (x, f x)

toFst :: (a -> b) -> a -> (b, a)
toFst f x = (f x, x)

without :: Eq a => [a] -> [a] -> [a]
without = (\\)

triangle :: Int -> Int
triangle n = n * (n + 1) `div` 2

day1 :: Text -> (Int, Int)
day1 = (increasing 1 &&& increasing 3) . Parse.day1
  where
    increasing :: Int -> [Int] -> Int
    increasing dist = count id . ap (zipWith (<)) (drop dist)

day2 :: Text -> (Int, Int)
day2 = (a &&& b) . Parse.day2
  where
    a = uncurry (*) . foldl' runCommand (0, 0)
    b = (\(x, y, _) -> x * y) . foldl' runCommand2 (0, 0, 0)

    runCommand :: (Int, Int) -> PilotCommand -> (Int, Int)
    runCommand (distance, depth) command = case command of
      Forward n -> (distance + n, depth)
      Down n    -> (distance, depth + n)
      Up n      -> (distance, depth - n)

    runCommand2 :: (Int, Int, Int) -> PilotCommand -> (Int, Int, Int)
    runCommand2 (distance, depth, aim) command = case command of
      Forward n -> (distance + n, depth + aim * n, aim)
      Down n    -> (distance, depth, aim + n)
      Up n      -> (distance, depth, aim - n)

day3 :: Text -> (Int, Int)
day3 = both (uncurry (*) . both (fromBase [False, True])) . (a &&& b) . Parse.day3
  where
    a = (map not &&& id) . map leastCommon . transpose
    b = (o2 &&& co2) . sort

    leastCommon :: Ord a => [a] -> a
    leastCommon = snd . minimum . map (length &&& head) . group . sort

    o2  = rating id
    co2 = rating not

    rating :: (Bool -> Bool) -> [[Bool]] -> [Bool]
    rating f [num] = num
    rating f nums = b : rating f nums'
      where
        b = f $ head $ mid nums
        nums' = map tail $ filter ((==b) . head) nums

day4 :: Text -> (Int, Int)
day4 = both snd . (minimum &&& maximum) . uncurry (map . scoreBoard) . Parse.day4
  where
    scoreBoard :: [Int] -> [[Int]] -> (Int, Int)
    scoreBoard draws board = (winTurn, score)
      where
        drawTurn :: Int -> Int
        drawTurn = theIndexIn draws

        turnBoard :: [[(Int, Int)]]
        turnBoard = (fmap . fmap) (toFst drawTurn) board

        (winTurn, winDraw) = minimum $ map maximum $ turnBoard ++ transpose turnBoard

        unmarked :: [Int]
        unmarked = map snd $ filter ((>winTurn) . fst) $ concat turnBoard

        score :: Int
        score = winDraw * sum unmarked

day5 :: Text -> (Int, Int)
day5 = both (count ((> 1) . length) . group . sort . concatMap linePoints)
  . (toFst $ filter $ not . isDiagonal) . Parse.day5
  where
    range :: Int -> Int -> [Int]
    range x x'
      | x <  x' = [x .. x']
      | x >  x' = reverse [x' .. x]
      | otherwise = repeat x

    isDiagonal :: ((Int, Int), (Int, Int)) -> Bool
    isDiagonal ((x, y), (x', y')) = x /= x' && y /= y'

    linePoints :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
    linePoints ((x, y), (x', y'))
      | x == x' && y == y' = [(x, y)]
      | otherwise = zip (range x x') (range y y')

day6 :: Text -> (Int, Int)
day6 = both sum . ((!! 80) &&& (!! 256)) . iterate step . groupInput . Parse.day6
  where
    groupInput :: [Int] -> [Int]
    groupInput = map (subtract 1 . length) . group . sort . ([0..8]++)

    step :: [Int] -> [Int]
    step [x0,x1,x2,x3,x4,x5,x6,x7,x8] = [x1,x2,x3,x4,x5,x6,x7+x0,x8,x0]

day7 :: Text -> (Int, Int)
day7 = (linearCost &&& costBy triangle) . sort . Parse.day7
  where
    linearCost crabs = sum $ map (abs . subtract (mid crabs)) crabs
    costBy costFunction crabs = minimum $ map rateSolution solutionRange
      where
        solutionRange = [minimum crabs .. maximum crabs]
        rateSolution n = sum $ map (costFunction . abs . subtract n) crabs

day8 :: Text -> (Int, Int)
day8 = (countEasy &&& sum . map (uncurry decode)) . Parse.day8
  where
    countEasy = count (`elem` [2, 3, 4, 7]) . map length . concat . map snd

    decode :: [String] -> [String] -> Int
    decode digits = fromBase [0..9] . map (digitMap digits)

    digitMap :: [String] -> String -> Int
    digitMap digits = theIndexIn [zero, one, two, three, four, five, six, seven, eight, nine]
      where
        withLength n = filter ((==n) . length) digits
        one   = sort $ the $ withLength 2
        seven = sort $ the $ withLength 3
        four  = sort $ the $ withLength 4
        adg   = sort $ foldl1 intersect $ withLength 5
        abfg  = sort $ foldl1 intersect $ withLength 6
        eight = sort $ the $ withLength 7
        five  = sort $ adg `union` abfg
        three = sort $ adg `union` one
        nine  = sort $ five `union` one
        zero  = sort $ eight `without` (adg `intersect` four)
        two   = sort $ (eight `without` five) `union` adg
        six   = sort $ five `union` (two `without` three)

day9 :: Text -> (Int, Int)
day9 = const (0, 0)

data MatchResult
  = Correct
  | ExtraCloser
  | Corrupt Int
  | Incomplete Int

day10 :: Text -> (Int, Int)
day10 = (sum . catCorrupts &&& mid . sort . catIncompletes) . map (matchParens []) . Parse.day10
  where
    matchParens xs ('(':ys) = matchParens (')':xs) ys
    matchParens xs ('[':ys) = matchParens (']':xs) ys
    matchParens xs ('{':ys) = matchParens ('}':xs) ys
    matchParens xs ('<':ys) = matchParens ('>':xs) ys
    matchParens (x:xs) (y:ys) | x == y = matchParens xs ys
    matchParens [] [] = Correct
    matchParens [] _ = ExtraCloser
    matchParens xs [] = Incomplete $ fromBase " )]}>" xs
    matchParens _ (')':_) = Corrupt 3
    matchParens _ (']':_) = Corrupt 57
    matchParens _ ('}':_) = Corrupt 1197
    matchParens _ ('>':_) = Corrupt 25137

    catCorrupts (Corrupt x : xs) = x : catCorrupts xs
    catCorrupts (_ : xs) = catCorrupts xs
    catCorrupts [] = []

    catIncompletes (Incomplete x : xs) = x : catIncompletes xs
    catIncompletes (_ : xs) = catIncompletes xs
    catIncompletes [] = []

day11 :: Text -> (Int, Int)
day11 = const (0, 0)

day12 :: Text -> (Int, Int)
day12 = const (0, 0)

data Coords = Coords [(Int, Int)]

instance Show Coords where
  show (Coords coords) = '\n' : unlines renderBlock
    where
      renderPoint y x = if (x, y) `elem` coords then '#' else ' '
      renderLine y = [0..fst size] <&> renderPoint y
      renderBlock = [0..snd size] <&> renderLine
      size = both maximum $ unzip coords

day13 :: Text -> (Int, Coords)
day13 = bimap length Coords . both (nub . sort . uncurry (flip foldAll))
  . (second (take 1) &&& id) . Parse.day13
  where
    foldAll :: [PaperFold] -> [(Int, Int)] -> [(Int, Int)]
    foldAll folds = map $ foldr (flip (.)) id $ map foldOver folds

    foldOver :: PaperFold -> (Int, Int) -> (Int, Int)
    foldOver (FoldAlongX fx) = first $ ap min (2*fx-)
    foldOver (FoldAlongY fy) = second $ ap min (2*fy-)

printDay :: (Show a, Show b) => Int -> (Text -> (a, b)) -> IO ()
printDay n solve = do
  (a, b) <- solve <$> withFile ("input/day" <> show n) ReadMode Text.hGetContents
  putStrLn $ "Day " <> show n <> ": " <> show a <> "  " <> show b

main :: IO ()
main = do
  printDay 1 day1
  printDay 2 day2
  printDay 3 day3
  printDay 4 day4
  printDay 5 day5
  printDay 6 day6
  printDay 7 day7
  printDay 8 day8
  printDay 9 day9
  printDay 10 day10
  printDay 11 day11
  printDay 12 day12
  printDay 13 day13
