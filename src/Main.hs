import qualified IO
import Camera
import Model
import Material
import Render
import Renderable
import Vec3
import Util

import System.Random
import Control.Monad.State (runState)

nx = 200
ny = 100

main :: IO ()
main =
  do
    randGen <- getStdGen
    let objects = fst $ runState (randomScene 11) randGen
    let buf = fst $ runState (render (scene objects)) randGen
    putStr $ IO.bufToPPM nx ny buf
  where
    cam = makeCamera (vec3 16 2 4) (vec3 (-3) 0.5 (-1)) (vec3 0 1 0) 15 (aspect nx ny)
    scene o = Scene {
      objects = o,
      camera  = cam,
      imgX    = nx,
      imgY    = ny
    }

threeSphere :: Renderable
threeSphere =
  fromList $ [s1,s2,s3,floor]
  where
    s1 = withMaterial (lambertian (vec3 0.8 0.3 0.3)) $ sphere (vec3 0 0 (-1)) 0.5
    s2 = withMaterial (metal (vec3 0.8 0.6 0.2) 0.3) $ sphere (vec3 1 0 (-1)) 0.5
    s3 = withMaterial (metal (vec3 0.8 0.8 0.8) 1.0) $ sphere (vec3 (-1) 0 (-1)) 0.5
    floor = withMaterial (lambertian (vec3 0.8 0.8 0)) $ sphere (vec3 0 (-100.5) (-1)) 100

randomScene :: Float -> RandomState Renderable
randomScene n =
  fromList <$> ([big1,big2,floor] ++ ) <$> randomSpheres
  where
    matL = do
      v1 <- randomVec3
      v2 <- randomVec3
      return $ lambertian (v1 * v2)
    matM = do
      v1 <- randomVec3
      fuzz <- randomFloat
      return $ metal ((v1 +: 1) *: 0.5) (fuzz*0.5)
    randomMat = do
      r <- randomFloat
      if (r < 0.5)
        then matL
        else matM
    randomSphere a b = do
      ra <- randomFloat
      rb <- randomFloat
      let center = vec3 (a + 0.9 * ra) 0.2 (b + 0.9 * rb)
      return $ sphere center 0.2
    randomSpheres =
      sequence $ do
        a <- [-n..n]
        b <- [-n..n]
        return $ do
          s <- (randomSphere a b) :: RandomState Hitable
          m <- randomMat
          return $ withMaterial m s
    big1 = withMaterial (lambertian (vec3 0.4 0.2 0.1)) $ sphere (vec3 (-2) 1 0) 1
    big2 = withMaterial (metal (vec3 0.7 0.6 0.5) 0.0) $ sphere (vec3 2 1 0) 1
    floor = withMaterial (lambertian (vec3 0.5 0.5 0.5)) $ sphere (vec3 0 (-1000) (0)) 1000

aspect :: Int -> Int -> Float
aspect x y = (fromIntegral x)/(fromIntegral y)
