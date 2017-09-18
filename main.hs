import Data.List

nx = 200
ny = 100

main :: IO ()
main = renderPPM nx ny (testScene nx ny)

-- Test Scene
testScene :: Int -> Int -> [Float]
testScene x y = testScene' x y 0 y

testScene' :: Int -> Int -> Int -> Int -> [Float]
testScene' x y i j
  | j < 0     = []
  | i >= x    = testScene' x y 0 (j-1)
  | otherwise =
    let r = (fromIntegral i) / (fromIntegral x)
        g = (fromIntegral j) / (fromIntegral y)
        b = 0.2
    in [r,g,b] ++ (testScene' x y (i+1) j)

-- Rendering PPM
renderPPM :: Int -> Int -> [Float] -> IO ()
renderPPM width height buf = do
  let header = ppmHeader width height
  let renderedBuf = renderBuf "" buf
  putStrLn $ header ++ renderedBuf

renderValue :: Float -> String
renderValue x = show $ truncate (255.99 * x)

renderBuf :: String -> [Float] -> String
renderBuf s [] = s
renderBuf s l  =
  let (triple, remaining) = splitAt 3 l
      renderedTriple      = map renderValue triple
      joinedTriple        = (foldr (\x acc -> x ++ acc) "" (intersperse " " renderedTriple)) ++ "\n"
  in
    s ++ (renderBuf joinedTriple remaining)

ppmHeader :: Int -> Int -> String
ppmHeader x y = "P3\n" ++ (show nx) ++ " " ++ (show ny) ++ "\n255\n"
