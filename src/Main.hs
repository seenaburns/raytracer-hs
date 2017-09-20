import Data.List
import Vec3 (Vec3)
import qualified Vec3

nx = 200
ny = 100

main :: IO ()
main = renderPPM nx ny (testScene nx ny)

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

-- Rendering PPM
renderPPM :: Int -> Int -> [Vec3] -> IO ()
renderPPM width height buf = do
  let header = ppmHeader width height
  putStrLn $ header ++ (renderBuf buf)

renderValue :: Float -> String
renderValue x = show $ truncate (255.99 * x)

mkString :: String -> [String] -> String
mkString s l = foldr (\x acc -> x ++ s ++ acc) "" l

renderBuf :: [Vec3] -> String
renderBuf l =
  foldr (\x acc -> (renderVec3 x) ++ "\n" ++ acc) "" l
  where
    renderVec3 v = mkString "" $ (intersperse " " (map (\f -> renderValue f) (Vec3.toList v)))

ppmHeader :: Int -> Int -> String
ppmHeader x y = "P3\n" ++ (show nx) ++ " " ++ (show ny) ++ "\n255\n"
