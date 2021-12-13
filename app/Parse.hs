module Parse where

import Control.Applicative ((<|>), liftA2, some)
import Control.Monad (replicateM)
import Data.Char (isAlpha, isSpace)
import Data.Functor (void)
import Data.List (sort)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (eof, errorBundlePretty, satisfy, sepBy, takeWhile1P, takeWhileP)
import Text.Megaparsec.Char (hspace, space)
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

word :: Parser Text
word = takeWhile1P (Just "word") isAlpha <* hspace

int :: Parser Int
int = Lexer.decimal <* hspace

commaInts :: Parser [Int]
commaInts = (int `sepBy` comma) <* newline

bit :: Parser Bool
bit = False <$ symbol "0" <|> True  <$ symbol "1"

linesOf :: Parser a -> Parser [a]
linesOf p = some $ p <* newline

coordinate :: Parser (Int, Int)
coordinate = (,) <$> int <* comma <*> int

day1 :: Text -> [Int]
day1 = parse $ linesOf int

data PilotCommand
  = Forward Int
  | Down Int
  | Up Int

day2 :: Text -> [PilotCommand]
day2 = parse $ linesOf pilotCommand
  where
    pilotCommand :: Parser PilotCommand
    pilotCommand = Forward <$ symbol "forward" <*> int
               <|> Down    <$ symbol "down"    <*> int
               <|> Up      <$ symbol "up"      <*> int

day3 :: Text -> [[Bool]]
day3 = parse $ linesOf $ some bit

day4 :: Text -> ([Int], [[[Int]]])
day4 = parse $ liftA2 (,) commaInts (some bingoBoard)
  where
    bingoBoard :: Parser [[Int]]
    bingoBoard = newline *> replicateM 5 (replicateM 5 int <* newline)

day5 :: Text -> [((Int, Int), (Int, Int))]
day5 = parse $ linesOf $ (,) <$> coordinate <* symbol "->" <*> coordinate

day6 :: Text -> [Int]
day6 = parse commaInts

day7 :: Text -> [Int]
day7 = parse commaInts

day8 :: Text -> [([String], [String])]
day8 = parse $ linesOf $ (,) <$> some segments <* symbol "|" <*> some segments
  where
    segments :: Parser String
    segments = sort . Text.unpack <$> word

day10 :: Text -> [String]
day10 = parse $ linesOf $ some $ satisfy $ not . isSpace

data PaperFold
  = FoldAlongX Int
  | FoldAlongY Int

day13 :: Text -> ([(Int, Int)], [PaperFold])
day13 = parse $ (,) <$> linesOf coordinate <* newline <*> linesOf paperFold
  where
    paperFold :: Parser PaperFold
    paperFold = symbol "fold along x=" *> fmap FoldAlongX int
      <|> symbol "fold along y=" *> fmap FoldAlongY int
