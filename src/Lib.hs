{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Builder
import Data.Word
import Data.Complex

import Data.Monoid
-- sample image

space :: Builder
space = byteString "\n"

data Pixel = Pixel { r :: Word8
                   , g :: Word8
                   , b :: Word8
                   }
             deriving (Show)

blank :: Pixel
blank = Pixel 0 0 0

class Encodeable a where
  enc :: a -> Builder

instance Encodeable Int where
  enc n = lazyByteString $ C.pack (show n)

instance Encodeable Word8 where
  enc = word8

instance Encodeable Pixel where
  enc p = enc (r p) <> enc (g p) <> enc (b p)

header :: Int -> Int -> Builder
header w h = byteString "P6" <> space
              <> enc w <> space
              <> enc h <> space
              <> enc (255 :: Int) <> space

fill :: Int -> Int -> Pixel -> Builder
fill w h p = mconcat (replicate (w * h) (enc p))

generate :: Int -> Int -> (Int -> Int -> Pixel) -> Builder
generate w h f | w <= 0 || h <= 0 = error "wrong dimensions"
               | otherwise = mconcat [enc (f x y)|y <- [0..(h - 1)], x <- [0..(w - 1)]]

red = blank { r = 255}

redGen :: Int -> Int -> Pixel
redGen _ _ = red

genImage :: Int -> Int -> (Int -> Int -> Pixel) -> L.ByteString
genImage w h f = toLazyByteString (header w h <> generate w h f)
  
sampleBImage :: L.ByteString
sampleBImage = genImage 200 100 redGen

main :: IO ()
main = magic 200 100 mandelbrot

example :: Int -> Int -> Pixel
example x y = blank {r = r, b = b}
  where
    r = fromIntegral $ ((x + 1) * 255 `div` 200)
    b = fromIntegral $ ((y + 1) * 255 `div` 100)

genPixel :: Int -> Pixel
genPixel n = Pixel a a a
  where a = fromIntegral $ n * 256 `div` 32

mandelbrot' :: Int -> Int -> Int -> (Complex Float) -> Pixel
mandelbrot' x y n z | n > 32 = genPixel 32
                    | (realPart (abs z)) > 2 = genPixel n
                    | otherwise = mandelbrot' x y (n + 1) (z * z + (x' :+ y'))
                    where
                      x' = fromIntegral x
                      y' = fromIntegral y

mandelbrot :: Int -> Int -> Pixel
mandelbrot x y = mandelbrot' x y 0 (0 :+ 0)

magic :: Int -> Int -> (Int -> Int -> Pixel) -> IO ()
magic w h f = do
  let image = genImage w h f
  L.writeFile "output/mandelbrot.ppm" image
