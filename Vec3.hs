module Vec3 where

data Vec3 = Vec3 Float Float Float deriving (Show)

x :: Vec3 -> Float
x (Vec3 x _ _ ) = x

y :: Vec3 -> Float
y (Vec3 _ y _) = y

z :: Vec3 -> Float
z (Vec3 _ _ z) = z

map :: (Float -> Float) -> Vec3 -> Vec3
map f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

map2 :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
map2 f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (f x1 x2) (f y1 y2) (f z1 z2)

toList :: Vec3 -> [Float]
toList (Vec3 x y z) = [x, y, z]

-- Reduce applies function f like ((x f y) f z)
reduce :: (Float -> Float -> Float) -> Vec3 -> Float
reduce f (Vec3 x y z) = ((x `f` y) `f` z)

-- Vec Math
dot :: Vec3 -> Vec3 -> Float
dot a b = Vec3.reduce (+) (Vec3.mulV a b)

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 ax ay az) (Vec3 bx by bz) =
  let x = ay*bz - az*by
      y = - (ax*bz - az*bx)
      z = ax*by - ay*bx
  in Vec3 x y z

length :: Vec3 -> Float
length v = sqrt (squaredLength v)

-- Squared length = x*x + y*y + z*z
squaredLength :: Vec3 -> Float
squaredLength v = Vec3.reduce (+) (Vec3.map (\x -> x * x) v)

normalized :: Vec3 -> Vec3
normalized v = let len = (Vec3.length v)
               in Vec3.map (\x -> x / len) v

-- Vec and Float math
add :: Vec3 -> Float -> Vec3
add v f = Vec3.map (\x -> x + f) v

sub :: Vec3 -> Float -> Vec3
sub v f = Vec3.map (\x -> x - f) v

mul :: Vec3 -> Float -> Vec3
mul v f = Vec3.map (\x -> x * f) v

div :: Vec3 -> Float -> Vec3
div v f = Vec3.map (\x -> x / f) v

-- Vec and Vec Math
addV :: Vec3 -> Vec3 -> Vec3
addV a b = Vec3.map2 (\aa bb -> aa + bb) a b

subV :: Vec3 -> Vec3 -> Vec3
subV a b = Vec3.map2 (\aa bb -> aa - bb) a b

mulV :: Vec3 -> Vec3 -> Vec3
mulV a b = Vec3.map2 (\aa bb -> aa * bb) a b

divV :: Vec3 -> Vec3 -> Vec3
divV a b = Vec3.map2 (\aa bb -> aa / bb) a b
