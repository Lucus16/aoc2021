module Main where

import Control.Applicative ((<|>), liftA2, many, optional, some)
import Control.Arrow ((&&&))
import Control.Exception (Exception(..), throw)
import Control.Monad (ap, replicateM)
import Data.Bifunctor (bimap)
import Data.Char (isSpace)
import Data.Foldable (foldl', for_)
import Data.Functor ((<&>), void)
import Data.List (elemIndex, group, sort, sortOn, transpose)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Tuple.Extra (both)
import Data.Void (Void)
import System.IO (withFile, IOMode(..))
import Text.Megaparsec (eof, errorBundlePretty, sepBy, single, takeWhileP)
import Util (count)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Text.Megaparsec.Parsec Void Text

tshow :: Show a => a -> Text
tshow = Text.pack . show

parse :: Parser a -> Text -> a
parse p = either (error . errorBundlePretty) id . Text.Megaparsec.parse (p <* space <* eof) "input"

space :: Parser ()
space = void $ takeWhileP (Just "space") isSpace

spaces :: Parser ()
spaces = void $ takeWhileP (Just "spaces") (==' ')

newline :: Parser ()
newline = void $ symbol "\n"

emptyLine :: Parser ()
emptyLine = newline >> newline

comma :: Parser ()
comma = void $ symbol ","

arrow :: Parser ()
arrow = void $ symbol "->"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaces

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaces

int :: Parser Int
int = lexeme Lexer.decimal

commaInts :: Parser [Int]
commaInts = (int `sepBy` comma) <* newline

bit :: Parser Bool
bit = command "0" False <|> command "1" True

linesOf :: Parser a -> Parser [a]
linesOf p = some $ p <* newline

command :: Text -> a -> Parser a
command name cons = cons <$ symbol name

binaryToInt :: [Bool] -> Int
binaryToInt = foldl' (\n b -> n * 2 + fromEnum b) 0

mid :: [a] -> a
mid xs = xs !! (length xs `div` 2)

toSnd :: (a -> b) -> a -> (a, b)
toSnd f x = (x, f x)

toFst :: (a -> b) -> a -> (b, a)
toFst f x = (f x, x)

triangle :: Int -> Int
triangle n = n * (n + 1) `div` 2

day1 :: Text -> (Int, Int)
day1 = (increasing 1 &&& increasing 3) . parse (linesOf int)
  where
    increasing :: Int -> [Int] -> Int
    increasing dist = count id . ap (zipWith (<)) (drop dist)

data PilotCommand
  = Forward Int
  | Down Int
  | Up Int

day2 :: Text -> (Int, Int)
day2 = (a &&& b) . parse (linesOf pilotCommand)
  where
    a = uncurry (*) . foldl' runCommand (0, 0)
    b = (\(x, y, _) -> x * y) . foldl' runCommand2 (0, 0, 0)

    pilotCommand :: Parser PilotCommand
    pilotCommand = command "forward" Forward <*> int
      <|> command "down" Down <*> int
      <|> command "up" Up <*> int

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
day3 = both (uncurry (*) . both binaryToInt) . (a &&& b) . parse (linesOf (some bit))
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
day4 = both snd . (minimum &&& maximum) . uncurry (map . scoreBoard)
  . parse (liftA2 (,) commaInts (some parseBoard))
  where
    parseBoard :: Parser [[Int]]
    parseBoard = newline *> replicateM 5 (replicateM 5 int <* newline)

    scoreBoard :: [Int] -> [[Int]] -> (Int, Int)
    scoreBoard draws board = (winTurn, score)
      where
        drawTurn :: Int -> Int
        drawTurn = fromMaybe (error "not drawn") . (`elemIndex` draws)

        turnBoard :: [[(Int, Int)]]
        turnBoard = (fmap . fmap) (toFst drawTurn) board

        (winTurn, winDraw) = minimum $ map maximum $ turnBoard ++ transpose turnBoard

        unmarked :: [Int]
        unmarked = map snd $ filter ((>winTurn) . fst) $ concat turnBoard

        score :: Int
        score = winDraw * sum unmarked

data Line = Line Int Int Int Int

day5 :: Text -> (Int, Int)
day5 = both (count ((> 1) . length) . group . sort . concatMap linePoints)
  . (toFst $ filter $ not . isDiagonal) . parse (linesOf parseLine)
  where
    parseLine :: Parser Line
    parseLine = Line <$> int <* comma <*> int <* arrow <*> int <* comma <*> int

    range :: Int -> Int -> [Int]
    range x x'
      | x <  x' = [x .. x']
      | x >  x' = reverse [x' .. x]
      | otherwise = repeat x

    isDiagonal :: Line -> Bool
    isDiagonal (Line x y x' y') = x /= x' && y /= y'

    linePoints :: Line -> [(Int, Int)]
    linePoints (Line x y x' y')
      | x == x' && y == y' = [(x, y)]
      | otherwise = zip (range x x') (range y y')

day6 :: Text -> (Int, Int)
day6 = both sum . ((!! 80) &&& (!! 256)) . iterate step . groupInput . parse commaInts
  where
    groupInput :: [Int] -> [Int]
    groupInput = map (subtract 1 . length) . group . sort . ([0..8]++)

    step :: [Int] -> [Int]
    step [x0,x1,x2,x3,x4,x5,x6,x7,x8] = [x1,x2,x3,x4,x5,x6,x7+x0,x8,x0]

day7 :: Text -> (Int, Int)
day7 = (linearCost &&& costBy triangle) . sort . parse commaInts
  where
    linearCost crabs = sum $ map (abs . subtract (mid crabs)) crabs
    costBy costFunction crabs = minimum $ map rateSolution solutionRange
      where
        solutionRange = [minimum crabs .. maximum crabs]
        rateSolution n = sum $ map (costFunction . abs . subtract n) crabs

days :: [Text -> (Int, Int)]
days = [ day1, day2, day3, day4, day5, day6, day7 ]

main :: IO ()
main = for_ (zip [1..] days) \(n, solve) -> do
  (a, b) <- solve <$> withFile ("input/day" <> show n) ReadMode Text.hGetContents
  putStrLn $ "Day " <> show n <> ": " <> show a <> "  " <> show b
