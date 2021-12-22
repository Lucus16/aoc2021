module Parse where

import Control.Applicative ((<|>), liftA2, some)
import Control.Monad (replicateM)
import Data.Char (digitToInt, isAlpha, isSpace)
import Data.Functor (void)
import Data.List (sort)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (eof, errorBundlePretty, satisfy, sepBy, takeWhile1P, takeWhileP)
import Text.Megaparsec.Char (digitChar, hspace, space)
import qualified Data.Text as Text
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Text.Megaparsec.Parsec Void Text

parse :: Parser a -> Text -> a
parse p = either (error . errorBundlePretty) id . Text.Megaparsec.parse (p <* space <* eof) "input"

symbol :: Text -> Parser Text
symbol = Lexer.symbol hspace

comma :: Parser ()
comma = void $ symbol ","

newline :: Parser ()
newline = void $ symbol "\n"

word :: Parser String
word = Text.unpack <$> takeWhile1P (Just "word") isAlpha <* hspace

letter :: Parser Char
letter = satisfy isAlpha

int :: Parser Int
int = Lexer.decimal <* hspace

commaInts :: Parser [Int]
commaInts = (int `sepBy` comma) <* newline

bit :: Parser Bool
bit = False <$ symbol "0" <|> True  <$ symbol "1"

digit :: Parser Int
digit = digitToInt <$> digitChar

linesOf :: Parser a -> Parser [a]
linesOf p = some $ p <* newline

coordinate :: Parser (Int, Int)
coordinate = (,) <$> int <* comma <*> int

day1 :: Parser [Int]
day1 = linesOf int

data PilotCommand
  = Forward Int
  | Down Int
  | Up Int

day2 :: Parser [PilotCommand]
day2 = linesOf pilotCommand
  where
    pilotCommand :: Parser PilotCommand
    pilotCommand = Forward <$ symbol "forward" <*> int
               <|> Down    <$ symbol "down"    <*> int
               <|> Up      <$ symbol "up"      <*> int

day3 :: Parser [[Bool]]
day3 = linesOf $ some bit

day4 :: Parser ([Int], [[[Int]]])
day4 = liftA2 (,) commaInts (some bingoBoard)
  where
    bingoBoard :: Parser [[Int]]
    bingoBoard = newline *> replicateM 5 (replicateM 5 int <* newline)

day5 :: Parser [((Int, Int), (Int, Int))]
day5 = linesOf $ (,) <$> coordinate <* symbol "->" <*> coordinate

day6 :: Parser [Int]
day6 = commaInts

day7 :: Parser [Int]
day7 = commaInts

day8 :: Parser [([String], [String])]
day8 = linesOf $ (,) <$> some segments <* symbol "|" <*> some segments
  where
    segments :: Parser String
    segments = sort <$> word

day9 :: Parser [[Int]]
day9 = linesOf $ some digit

day10 :: Parser [String]
day10 = linesOf $ some $ satisfy $ not . isSpace

data PaperFold
  = FoldAlongX Int
  | FoldAlongY Int

day13 :: Parser ([(Int, Int)], [PaperFold])
day13 = (,) <$> linesOf coordinate <* newline <*> linesOf paperFold
  where
    paperFold :: Parser PaperFold
    paperFold = symbol "fold along x=" *> fmap FoldAlongX int
      <|> symbol "fold along y=" *> fmap FoldAlongY int

day14 :: Parser (String, [((Char, Char), Char)])
day14 = (,) <$> word <* space <*> linesOf pairInsertion
  where
    charPair :: Parser (Char, Char)
    charPair = (,) <$> letter <*> letter <* hspace

    pairInsertion :: Parser ((Char, Char), Char)
    pairInsertion = (,) <$> charPair <* symbol "->" <*> letter
