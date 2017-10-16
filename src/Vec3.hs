module Vec3 where

import Data.Functor
import Control.Applicative

data Vec3Generic a = Vec3Generic {
  x :: !a,
  y :: !a,
  z :: !a
} deriving (Show, Eq)

type Vec3 = Vec3Generic Float
vec3 :: Float -> Float -> Float -> Vec3
vec3 x y z = Vec3Generic x y z

instance Functor Vec3Generic where
  fmap f (Vec3Generic x y z) = Vec3Generic (f x) (f y) (f z)

instance Applicative Vec3Generic where
  pure a = Vec3Generic a a a
  (Vec3Generic fx fy fz) <*> (Vec3Generic x y z) = Vec3Generic (fx x) (fy y) (fz z)

toList :: Vec3Generic a -> [a]
toList (Vec3Generic x y z) = [x, y, z]

-- Reduce applies function f like ((x f y) f z)
reduce :: (a -> a -> a) -> Vec3Generic a -> a
reduce f (Vec3Generic x y z) = ((x `f` y) `f` z)

-- Vec Math
dot :: (Num a) => Vec3Generic a -> Vec3Generic a -> a
dot a b = reduce (+) (a * b)

cross :: (Num a) => Vec3Generic a -> Vec3Generic a -> Vec3Generic a
cross (Vec3Generic ax ay az) (Vec3Generic bx by bz) =
  let x = ay*bz - az*by
      y = - (ax*bz - az*bx)
      z = ax*by - ay*bx
  in Vec3Generic x y z

length :: (Floating a) => Vec3Generic a -> a
length v = sqrt (squaredLength v)

-- Squared length = x*x + y*y + z*z
squaredLength :: (Num a) => Vec3Generic a -> a
squaredLength v = reduce (+) (fmap (\x -> x * x) v)

normalized :: (Floating a) => Vec3Generic a -> Vec3Generic a
normalized v = let len = Vec3.length v
               in fmap (\x -> x / len) v

lerp :: (Fractional a) => Vec3Generic a -> Vec3Generic a -> a -> Vec3Generic a
lerp v1 v2 t = (v1 *: t) + (v2 *: (1.0-t))

-- Vec and Vec Math
instance (Num a) => Num (Vec3Generic a) where
  negate = fmap negate
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Vec3Generic a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

-- Vec and scalar math, _: to indicate scalar
(+:) :: (Num a) => Vec3Generic a -> a -> Vec3Generic a
(+:) v f = fmap (\x -> x + f) v

(-:) :: (Num a) => Vec3Generic a -> a -> Vec3Generic a
(-:) v f = fmap (\x -> x - f) v

(*:) :: (Num a) => Vec3Generic a -> a -> Vec3Generic a
(*:) v f = fmap (\x -> x * f) v

(/:) :: (Fractional a) => Vec3Generic a -> a -> Vec3Generic a
(/:) v f = fmap (\x -> x / f) v
