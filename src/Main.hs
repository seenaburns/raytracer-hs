import qualified IO
import Model
import Render
import Vec3

nx = 200
ny = 100

main :: IO ()
main =
  do
    buf <- render scene
    putStr $ IO.bufToPPM nx ny buf
  where
    s1 = sphere (vec3   0.25  0 (-1.2)) 0.5
    s2 = sphere (vec3 (-0.25) 0 (-1)) 0.5
    floor = sphere (vec3 0 (-100.5) (-1)) 100
    scene = Scene {
      objects = hitableList [s1,s2,floor],
      imgX    = nx,
      imgY    = ny
    }

