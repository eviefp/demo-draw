module Color
  ( Color (..),
    red,
    green,
    blue,
  )
where

import Prelude

data Color = Color
  { getRed :: !Int,
    getGreen :: !Int,
    getBlue :: !Int
  }
  deriving (Eq, Show)

instance Semigroup Color where
  Color {getRed = r1, getGreen = g1, getBlue = b1} <> Color {getRed = r2, getGreen = g2, getBlue = b2} =
    Color {getRed = min 255 (r1 + r2), getBlue = min 255 (b1 + b2), getGreen = min 255 (g1 + g2)}

instance Monoid Color where
  mempty = Color 0 0 0

red :: Color
red = mempty {getRed = 255}

green :: Color
green = mempty {getGreen = 255}

blue :: Color
blue = mempty {getBlue = 255}
