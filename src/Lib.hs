{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Builder
import Data.Word
import Data.Complex

import Data.Monoid
-- sample image
class Encodeable a where
  enc :: a -> Builder

instance Encodeable Int where
  enc n = lazyByteString $ C.pack (show n)

instance Encodeable Word8 where
  enc = word8

instance Encodeable Pixel where
  enc p = enc (r p) <> enc (g p) <> enc (b p)


space :: Builder
space = byteString "\n"

data Pixel = Pixel { r :: Word8
                   , g :: Word8
                   , b :: Word8
                   }
             deriving (Show)

black :: Pixel
black =  Pixel 0 0 0

defaultWidth = 500
defaultHeight = 500

data Image = Image { width :: Int
                   , height :: Int
                   , gen    :: Int -> Int -> Pixel
                   }


defaultImage :: Image
defaultImage = Image defaultWidth defaultHeight (\_ _ -> black)

newtype Generated = MkGenerated L.ByteString

write :: FilePath -> Generated -> IO ()
write p (MkGenerated i) = L.writeFile p i

generate :: Image -> Generated
generate (Image h w g) = MkGenerated $ toLazyByteString $ header h w <> generate' h w g

quick :: FilePath -> (Int -> Int -> Pixel) -> IO ()
quick p g = write p $ generate (defaultImage {gen = g})

header :: Int -> Int -> Builder
header w h = byteString "P6" <> space
              <> enc w <> space
              <> enc h <> space
              <> enc (255 :: Int) <> space


generate' :: Int -> Int -> (Int -> Int -> Pixel) -> Builder
generate' w h f | w <= 0 || h <= 0 = error "wrong dimensions"
               | otherwise = mconcat [enc (f x y)|y <- [0..(h - 1)], x <- [0..(w - 1)]]

-- Define a dsl working on images

grayscale :: Int -> Pixel
grayscale i = Pixel a a a
  where a = fromIntegral i

-- lets define that funny image
add :: Pixel -> Pixel -> Pixel
add (Pixel a b c) (Pixel x y z) = Pixel (a+x) (b+y) (c+z)

-- Resample from x from a..b to c..d

resample c d a b x = (offset * stretch) + c
  where
    offset :: Float
    offset = x - a
    stretch = (d - c) / (b - a)

toColor = resample 0 255
toWidth = toColor 0 $ fromIntegral (defaultWidth - 1)
toHeight = toColor 0 $ fromIntegral(defaultHeight - 1)

example x y = add redChannel blueChannel
  where
    redChannel = Pixel x' 0 0
    blueChannel = Pixel 0 0 y'
    x' = (round . toWidth . fromIntegral) x
    y' = (round . toWidth . fromIntegral) y
    
toIntensity = toColor 0 32

mandelX = resample (-2) 1 0 $ fromIntegral (defaultWidth - 1)
mandelY = resample (-1.5) 1.5 0 $ fromIntegral (defaultHeight - 1)

mandelbrot :: Int -> Int -> Pixel
mandelbrot x y = grayscale $ 255 * mand x' y' `div` 32
  where
    x' = mandelX $ fromIntegral x
    y' = mandelY $ fromIntegral y


mand :: Float -> Float -> Int
mand x y = go z0  0
 where
   z0 = 0 :+ 0
   c  = x :+ y
   go z n | n > 32 = 32
          | magnitude z > 2 = n
          | otherwise = go ((z^2) + c) (n + 1)

