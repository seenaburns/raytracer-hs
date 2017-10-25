import qualified IO
import Model
import Material
import Render
import Renderable
import Vec3

import System.Random
import Control.Monad.State (runState)

nx = 200
ny = 100

main :: IO ()
main =
  do
    randGen <- getStdGen
    let buf = fst $ runState (render scene) randGen
    putStr $ IO.bufToPPM nx ny buf
  where
    s1 = withMaterial (lambertian (vec3 0.8 0.3 0.3)) $ sphere (vec3 0 0 (-1)) 0.5
    s2 = withMaterial (metal (vec3 0.8 0.6 0.2) 0.3) $ sphere (vec3 1 0 (-1)) 0.5
    s3 = withMaterial (metal (vec3 0.8 0.8 0.8) 1.0) $ sphere (vec3 (-1) 0 (-1)) 0.5
    floor = withMaterial (lambertian (vec3 0.8 0.8 0)) $ sphere (vec3 0 (-100.5) (-1)) 100
    scene = Scene {
      objects = fromList $ [s1,s2,s3,floor],
      imgX    = nx,
      imgY    = ny
    }

