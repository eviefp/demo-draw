module Image
  ( Image (..),
    translate,
    save,
  )
where

import Codec.Picture qualified as Codec
import Data.Profunctor (Profunctor (dimap))
import Data.Vector.Storable qualified as V
import Data.Word (Word8)
import Prelude

newtype Image n k = Image
  { getImage :: n -> n -> k
  }
  deriving (Functor)

instance Semigroup k => Semigroup (Image n k) where
  Image {getImage = i1} <> Image {getImage = i2} = Image {getImage = app}
    where
      app :: n -> n -> k
      app x y = i1 x y <> i2 x y

instance Monoid k => Monoid (Image n k) where
  mempty = Image {getImage = const (const mempty)}

instance Profunctor Image where
  dimap :: forall n x k l. (x -> n) -> (k -> l) -> Image n k -> Image x l
  dimap f g Image {getImage} = Image {getImage = app}
    where
      app :: x -> x -> l
      app x y = g $ getImage (f x) (f y)

translate :: forall n k. Num n => n -> n -> Image n k -> Image n k
translate x' y' Image {getImage} = Image {getImage = app}
  where
    app :: n -> n -> k
    app x y = getImage (x - x') (y - y')

save ::
  -- | filename
  FilePath ->
  -- | packing colors into RGB8
  (k -> [Word8]) ->
  -- | width
  Int ->
  -- | height
  Int ->
  -- | image
  Image Int k ->
  IO ()
save file f width height Image {getImage} =
  Codec.savePngImage file (Codec.ImageRGB8 img)
  where
    img :: Codec.Image Codec.PixelRGB8
    img =
      Codec.Image
        { Codec.imageWidth = width,
          Codec.imageHeight = height,
          Codec.imageData = imageData
        }
    imageData :: V.Vector Word8
    imageData =
      V.fromList $
        concatMap
          (f . uncurry getImage)
          [(x, y) | y <- [0 .. height -1], x <- [0 .. width -1]]
