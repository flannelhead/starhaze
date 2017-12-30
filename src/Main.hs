module Main where

import           Graphics.Image           as I
import           Graphics.Image.Interface as I
import           Linear
import           Prelude                  as P

import           StarMap
import           Util

backgroundColor = PixelRGB 0 0 0
imageSize = 1600
saturation = 0.6
intensity = 1.0

stereographicInverse (x, y) = let
    denom = 1 / (1 + x*x + y*y)
    x' = 2 * x * denom
    y' = 2 * y * denom
    z' = (x*x + y*y - 1) * denom
  in V3 x' y' z'

makePixel :: StarTree -> Int -> (Int, Int) -> Pixel RGB Double
makePixel startree size (i, j) = let
    radius = fromIntegral size / (2 :: Double)
    i' = fromIntegral i / radius - 1
    j' = fromIntegral j / radius - 1
    vec = stereographicInverse (j', i')
  in if i'*i' + j'*j' <= 1 then starLookup startree intensity saturation vec
                           else backgroundColor

main :: IO ()
main = do
  etree <- readTreeFromFile "stars.kdt"
  case etree of
    Right tree -> do
      let img = makeImage (imageSize, imageSize)
                (makePixel tree imageSize) :: Image RPU RGB Double
      writeImage "test.png" . compute $ img
    _          -> putStrLn "Failed reading star tree"
