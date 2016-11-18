{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Builder
import Data.Word
import Data.Monoid
-- sample image

import Prelude hiding (const)

data Pixel = Pixel { _r :: Word8
                   , _g :: Word8
                   , _b :: Word8
                   }
             deriving (Show)

black = Pixel 0 0 0
red   = Pixel 255 0 0
-- Bitmap

type Bitmap = Builder

space = byteString "\n"
const = byteString

int :: Int -> Bitmap
int = lazyByteString . C.pack . show

infixr 6 <|>
(<|>) :: Bitmap -> Bitmap -> Bitmap
a <|> b = a <> space <> b

-- convert a pixel to bitmap
rastPixel (Pixel r g b) = word8 r <> word8 g <> word8 b
-- convert a list of pixels to bitmap
rastPixels l = foldr (<>) (lazyByteString L.empty) (map rastPixel l)
-- header
header x y = const "P6" <|>
             int x <|>
             int y <|>
             int 255 <> space

-- A Simple render function
renderBlack :: Int -> Int -> Pixel
renderBlack _ _ = black

renderRed :: Int -> Int -> Pixel
renderRed _ _ = red

renderPixels :: Int -> Int -> (Int -> Int -> Pixel) -> [Pixel]
renderPixels width height renderFun = [renderFun x y | y <- [0..(height-1)], x <- [0..(width-1)]]

render :: Int -> Int -> (Int -> Int -> Pixel) -> Bitmap
render width height renderFun = imageHeader <> imageBitmap
  where
    imageHeader = header width height
    imageBitmap = rastPixels (renderPixels width height renderFun)

writeBitmap :: FilePath -> Bitmap -> IO ()
writeBitmap file bitmap = L.writeFile file (toLazyByteString bitmap)

data Sphere = Sphere {  sphereX :: Float
                      , sphereY :: Float
                      , sphereZ :: Float
                      , sphereR :: Float
                      , sphereC :: Pixel
                      }

baseSphere = Sphere 320 240 20 50 black

data Vector = Vector { vectorX :: Float
                     , vectorY :: Float
                     , vectorZ :: Float
                     }

infixr <+>
infixr <.>

-- Vector addition
(<+>) :: Vector -> Vector -> Vector
a <+> b = Vector x y z
  where
    x = vectorX a + vectorX b
    y = vectorY a + vectorY b
    z = vectorZ a + vectorZ b

-- Dot product
(<.>) :: Vector -> Vector -> Float
a <.> b = axbx + ayby + azbz
  where
    axbx = vectorX a * vectorX b
    ayby = vectorY a * vectorY b
    azbz = vectorZ a * vectorZ b

len :: Vector -> Float
len a = sqrt (a <.> a)

dumb :: (Float -> Float -> a) -> Int -> Int -> a
dumb f a b = f (fromIntegral a) (fromIntegral b)

rayGen :: Float -> Float -> Vector
rayGen x y = undefined

renderSphere :: Sphere -> Int -> Int -> Pixel
renderSphere s = dumb sphere
  where
    c = sphereC s
    x = sphereX s
    y = sphereY s
    r = sphereR s
    sphere :: Float -> Float -> Pixel
    sphere x' y' | (x - x') ** 2 + (y - y') ** 2 < r ** 2 = c
                 | otherwise = black    

main :: IO ()
main = do
  let s = baseSphere {sphereC = red}
  let renderFun = renderSphere s
  let bitmap = render 640 480 renderFun
  writeBitmap "output/foo.ppm" bitmap
