module One
  ( go,
    square,
    main,
  )
where

import Color
import Data.Foldable (fold)
import Data.Text.IO qualified as T
import Data.Word (Word8)
import Draw (interpret, parseInput)
import Image
import Text.Megaparsec (parse)
import Prelude

square :: Image Int Color
square = Image app
  where
    app x y
      | 0 > x || x > 10 = mempty
      | 0 > y || y > 10 = mempty
      | otherwise = red

twoSquare :: Image Int Color
twoSquare = it
  where
    s1 = square
    s2 = translate 50 50 . fmap mkGreen $ square

    two = s1 <> s2
    it = fold $ zipWith3 translate [10, 30 .. 300] [10, 30 .. 300] (repeat two)

    mkGreen :: Color -> Color
    mkGreen c
      | c == mempty = mempty
      | otherwise = green

go :: IO ()
go = save "test.png" toRGB 500 500 twoSquare

toRGB :: Color -> [Word8]
toRGB Color {getRed, getGreen, getBlue} =
  fromIntegral . min 255
    <$> [ getRed,
          getGreen,
          getBlue
        ]

main :: IO ()
main = do
  contents <- T.readFile "commands.draw"
  let input = parse parseInput "" contents
  case input of
    Left err -> print err
    Right commands -> save "test.png" toRGB 500 500 $ interpret commands
