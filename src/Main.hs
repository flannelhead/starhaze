module Main where

import           Graphics.Image           as I
import           Graphics.Image.Interface as I
import           Linear
import           Prelude                  as P

import           StarMap
import           Util

backgroundColor = PixelRGB 0 0 0
imageSize = 3200
saturation = 0.5
intensity = 1.0

stereographicInverse :: V2 Double -> V3 Double
stereographicInverse v@(V2 x y) = let
    norm2 = quadrance v
    x' = 2 * x
    y' = 2 * y
    z' = (norm2 - 1)
  in V3 x' y' z' ^/ (1 + norm2)

makePixel :: StarTree -> Int -> (Int, Int) -> Pixel RGB Double
makePixel startree size (i, j) = let
    radius = fromIntegral size / (2 :: Double)
    vec2 = V2 (fromIntegral j) (fromIntegral i) ^/ radius ^-^ V2 1 1
    vec3 = stereographicInverse vec2
  in if quadrance vec2 <= 1 then starLookup startree intensity saturation vec3
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
