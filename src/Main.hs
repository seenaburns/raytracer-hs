import Vec3 (Vec3)
import qualified Vec3
import qualified IO

nx = 200
ny = 100

main :: IO ()
main = putStr $ IO.bufToPPM nx ny (testScene nx ny)

-- Test Scene
testScene :: Int -> Int -> [Vec3]
testScene x y = testScene' x y 0 y

testScene' :: Int -> Int -> Int -> Int -> [Vec3]
testScene' x y i j
  | j < 0     = []
  | i >= x    = testScene' x y 0 (j-1)
  | otherwise =
    let r = (fromIntegral i) / (fromIntegral x)
        g = (fromIntegral j) / (fromIntegral y)
        b = 0.2
    in [Vec3.Vec3 r g b] ++ (testScene' x y (i+1) j)
