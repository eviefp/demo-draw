module Draw
  ( interpret,
    parseInput,
  )
where

import Color (Color, blue, green, red)
import Control.Monad (void)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Image (Image (Image, getImage), translate)
import Text.Megaparsec (Parsec, many, (<|>))
import Text.Megaparsec.Char (alphaNumChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Prelude

newtype VarName = VarName String
  deriving (Show, Eq)

data SquareData = SquareData
  { sdColor :: !Color,
    sdSize :: !Int,
    sdPosition :: !(Int, Int)
  }
  deriving (Show)

data TranslateData = TranslateData
  { tdVarName :: !VarName,
    tdX :: !Int,
    tdY :: !Int
  }
  deriving (Show)

data DrawCommand
  = MkSquare VarName SquareData
  | Compose VarName VarName VarName
  | Translate VarName TranslateData
  | Draw VarName
  deriving (Show)

--------------------------------------------------------------------------------
-- Parser
type Parser a = Parsec Void Text a

parseInput :: Parser [DrawCommand]
parseInput = many (parseDrawCommand <* newline)

parseDrawCommand :: Parser DrawCommand
parseDrawCommand = do
  vn <- parseVarName
  (string " = " *> bindings vn) <|> (string " draw" $> Draw vn)
  where
    bindings vn = parseMkSquare vn <|> parseTranslate vn <|> parseCompose vn

parseMkSquare :: VarName -> Parser DrawCommand
parseMkSquare vn = do
  sdColor <- parseColor
  void $ string " square "
  sdSize <- decimal
  _ <- string " at "
  x <- signed space decimal
  space
  y <- signed space decimal
  let sdPosition = (x, y)
  pure $ MkSquare vn $ SquareData {..}

parseCompose :: VarName -> Parser DrawCommand
parseCompose var = do
  left <- parseVarName
  void $ string " + "
  Compose var left <$> parseVarName

parseTranslate :: VarName -> Parser DrawCommand
parseTranslate var = do
  void $ string "translate "
  tdVarName <- parseVarName
  space
  tdX <- signed space decimal
  space
  tdY <- signed space decimal
  pure $ Translate var TranslateData {..}

parseColor :: Parser Color
parseColor = parseGreen <|> parseRed <|> parseBlue
  where
    parseGreen = string "green" $> green
    parseRed = string "red" $> red
    parseBlue = string "blue" $> blue

parseVarName :: Parser VarName
parseVarName = VarName <$> many alphaNumChar

--------------------------------------------------------------------------------
-- Interpret
interpret :: [DrawCommand] -> Image Int Color
interpret = foldMap (\(_, i, _) -> i) . filter (\(_, _, draw) -> draw) . foldl go []
  where
    go :: [(VarName, Image Int Color, Bool)] -> DrawCommand -> [(VarName, Image Int Color, Bool)]
    go xs =
      \case
        (MkSquare vn sd) -> (vn, mkSquare sd, False) : xs
        (Compose vRes va vb) ->
          case (<>) <$> lookup' va <*> lookup' vb of
            Nothing -> xs
            Just res -> (vRes, res, False) : xs
        (Translate vn TranslateData {..}) ->
          case lookup' tdVarName of
            Nothing -> xs
            Just res -> (vn, translate tdX tdY res, False) : xs
        (Draw vn) ->
          case lookup' vn of
            Just res -> (vn, res, True) : xs
            Nothing -> xs
      where
        xs' = fmap (\(vn, img, _) -> (vn, img)) xs
        lookup' var = lookup var xs'

    mkSquare :: SquareData -> Image Int Color
    mkSquare SquareData {sdColor, sdSize, sdPosition = (tx, ty)} =
      translate tx ty Image {getImage = mkImg}
      where
        mkImg x y
          | 0 > x || x > sdSize = mempty
          | 0 > y || y > sdSize = mempty
          | otherwise = sdColor
