import Vec3
import Vec3 (Vec3(Vec3))
import qualified IO
import Ray
import Ray (Ray(Ray))

nx = 200
ny = 100

main :: IO ()
main = putStr $ IO.bufToPPM nx ny (testScene nx ny)

-- Test Scene
testScene :: Int -> Int -> [Vec3]
testScene width height =
  do
    j <- (reverse [0..height-1])
    i <- ([0..width-1])
    let u = (fromIntegral i) / (fromIntegral width)
    let v = (fromIntegral j) / (fromIntegral height)
    let r = Ray origin (lowerleftcorner `addV` (horizontal `mul` u) `addV` (vertical `mul` v))
    return $ color r
  where
    -- Define origin and picture plane from corner, width and height
    lowerleftcorner = Vec3 (-2) (-1) (-1)
    horizontal      = Vec3 4 0 0
    vertical        = Vec3 0 2 0
    origin          = Vec3 0 0 0

-- Blend between two colors with the y component of the given ray
color :: Ray -> Vec3
color r =
  lerp (Vec3 0.5 0.7 1.0) (Vec3 1 1 1) t
  where
    unitDir = (normalized (dir r))
    t = 0.5 * ((y unitDir) + 1.0)
