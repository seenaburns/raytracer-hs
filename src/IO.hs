module IO (bufToPPM) where

import Data.List -- intersperse
import Vec3 (Vec3)
import qualified Vec3

-- Rendering PPM
bufToPPM :: Int -> Int -> [Vec3] -> String
bufToPPM width height buf =
  (ppmHeader width height) ++ (renderBuf buf)

-- Apply scaling to float and convert to int, then to string
-- 1.0 => "255"
renderValue :: Float -> String
renderValue x = show $ truncate (255.99 * x)

-- Render triplet with spaces
-- Vec3 0 1 1 => "0 255 255"
renderVec3 :: Vec3 -> String
renderVec3 v = concat (intersperse " " (map (\f -> renderValue f) (Vec3.toList v)))

-- Render list of Vec3 with newlines
-- [Vec3 0 0 0, Vec3 1 1 1] => "0 0 0\n255 255 255"
renderBuf :: [Vec3] -> String
renderBuf l = concat (intersperse "\n" (map (renderVec3) l))

ppmHeader :: Int -> Int -> String
ppmHeader x y = "P3\n" ++ (show x) ++ " " ++ (show y) ++ "\n255\n"
