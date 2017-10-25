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
    mat = lambertian (vec3 0.5 0.5 0.5)
    s1 = sphere (vec3   0.25  0 (-1.2)) 0.5
    s2 = sphere (vec3 (-0.25) 0 (-1)) 0.5
    floor = sphere (vec3 0 (-100.5) (-1)) 100
    scene = Scene {
      objects = fromList $ fmap (withMaterial mat) [s1,s2,floor],
      imgX    = nx,
      imgY    = ny
    }

