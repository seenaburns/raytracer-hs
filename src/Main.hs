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
  let
    -- Define origin and picture plane from corner, width and height
    lowerleftcorner = Vec3 (-2) (-1) (-1)
    horizontal      = Vec3 4 0 0
    vertical        = Vec3 0 2 0
    origin          = Vec3 0 0 0
    in
    do
      j <- (reverse [0..height-1])
      i <- ([0..width-1])
      let
        u = (fromIntegral i) / (fromIntegral width)
        v = (fromIntegral j) / (fromIntegral height)
        r = Ray origin (lowerleftcorner `addV` (horizontal `mul` u) `addV` (vertical `mul` v))
        in return $ color r

color :: Ray -> Vec3
color r =
  let
    unitDir = (normalized (dir r))
    t = 0.5 * ((y unitDir) + 1.0)
  in ((Vec3 1 1 1) `mul` (1.0-t)) `addV` ((Vec3 0.5 0.7  1.0) `mul` t)
