{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module StarMap
    ( Star, StarTree, StoredStarTree
    , readMapFromFile, treeToByteString, readTreeFromFile
    , buildStarTree, sqrnorm, starLookup ) where

import           Control.Monad
import qualified Data.ByteString   as B
import           Data.Char
import           Data.KdMap.Static
import           Data.Serialize    as S
import           Data.Word
import           Graphics.Image    as I
import           Linear            as L
import           Prelude           as P

import           Util

type Star = (V3 Double, (Int, Double, Double))
type StarTree = KdMap Double (V3 Double) (Int, Double, Double)
type StoredStar = (V3 Double, (Int, Char))
type StoredStarTree = KdMap Double (V3 Double) (Int, Char)

instance Serialize StoredStarTree
instance Serialize (TreeNode Double (V3 Double) (Int, Char))

-- We can't serialize functions but let's hack around it so that we can
-- serialize the KdMap anyway
instance Serialize (SquaredDistanceFn Double (V3 Double)) where
    put _ = put (0 :: Word8)
    get = skip 1 >> return (defaultSqrDist v3AsList)

instance Serialize (PointAsListFn Double (V3 Double)) where
    put _ = put (0 :: Word8)
    get = skip 1 >> return v3AsList

-- Parse the star list in the binary format specified at
-- http://tdc-www.harvard.edu/software/catalogs/ppm.entry.html
readMap :: Get [StoredStar]
readMap = do
    -- Skip the header
    skip 28
    nBytes <- remaining
    replicateM (nBytes `div` 28) $ do
        ra <- getFloat64be
        dec <- getFloat64be
        spectral <- getWord8
        skip 1
        mag <- getInt16be
        skip 8
        return ( raDecToCartesian ra dec
               , (fromIntegral mag, chr $ fromIntegral spectral) )

starColor' :: (Int, Char) -> (Int, Double, Double)
starColor' (mag, ch) = let (h, s) = starColor ch in (mag, h, s)

-- Some nice colour values for different spectral types
starColor :: Char -> (Double, Double)
starColor 'O' = (227, 0.39)
starColor 'B' = (226, 0.33)
starColor 'A' = (224, 0.21)
starColor 'F' = (234, 0.03)
starColor 'G' = (32, 0.09)
starColor 'K' = (34, 0.29)
starColor 'M' = (34, 0.56)
starColor _   = (0, 0)

raDecToCartesian :: Double -> Double -> V3 Double
raDecToCartesian ra dec = V3 (cos dec*cos ra) (cos dec*sin ra) (sin dec)

readMapFromFile :: FilePath -> IO (Either String [StoredStar])
readMapFromFile path = do
    ebs <- readSafe path
    return $ ebs >>= runGet readMap

readTreeFromFile :: FilePath -> IO (Either String StarTree)
readTreeFromFile path = do
    ebs <- readSafe path
    return $ fmap starColor' <$> (S.decode =<< ebs)

treeToByteString :: StoredStarTree -> B.ByteString
treeToByteString = S.encode

buildStarTree :: [StoredStar] -> StoredStarTree
buildStarTree = build v3AsList

v3AsList :: V3 Double -> [Double]
v3AsList (V3 x y z) = [x, y, z]

sqrnorm :: V3 Double -> Double
{-# INLINE sqrnorm #-}
sqrnorm (V3 x y z) = x*x + y*y + z*z

starLookup :: StarTree -> Double -> Double -> V3 Double -> Pixel RGB Double
{-# INLINE starLookup #-}
starLookup starmap intensity saturation vel = let
        -- The magnitude value tells about the intensity of the star. The
        -- brighter the star, the smaller the magnitude. These constants are
        -- used for adjusting the dynamics of the rendered celestial sphere.
        -- We need three fixed points to determine the dynamics:
        -- the points m0 and m1 fix the logarithmic scale. m0 is the reference
        -- "minimum" magnitude. When the magnitude reaches m1, the brightness
        -- will be doubled. m2 is required for normalization and corresponds to
        -- the maximal brightness value that will be represented on the screen.
        max_brightness = 400 :: Double   -- the "maximum brightness" magnitude
        dynamic = 60 :: Double
        w = 0.005                       -- width parameter of the gaussian function

        nvel = L.normalize vel
        d2 = sqrnorm $ pos ^-^ nvel  -- the distance from the star on the
                                     -- celestial sphere surface
        (pos, (mag, hue, sat)) = nearest starmap nvel
        -- Conversion from the log magnitude scale to linear brightness
        -- and a Gaussian intensity function. This determines the apparent size
        -- and brightness of the star.
        a = log 2 / dynamic
        val = min 1 . (* intensity)
              . exp $ a*(max_brightness - fromIntegral mag) - d2/(2*w^(2 :: Int))
    in toPixelRGB $ PixelHSI (hue / 360) (saturation * sat) val
