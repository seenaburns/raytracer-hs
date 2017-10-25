module Renderable where

import Model
import Material
import Ray

import Data.Semigroup

-- Renderable exists to remove the cyclical dependency of Material from HitRecord
-- Essentially a Hitable interface but also returns Material

type Renderable = (Ray -> Float -> Float -> Maybe RenderableHit)

-- Alias, also lets us define the Semigroup
-- Possibly excessive without purpose
newtype RenderableHit = RenderableHit { inner :: (HitRecord, Material) }

-- Only a semigroup because no sensible identity
-- Semigroup lets us take advantage of the Maybe Semigroup for fromList
-- Originally HitRecord's semigroup, combine chooses the closer hit
-- the implementation (comparing t) assumes that hits are only compared for the same ray
instance Semigroup RenderableHit where
  a <> b = if (t $ fst $ inner a) < (t $ fst $ inner b) then a else b

-- Construct Renderable from hitable and matieral
withMaterial :: Material -> Hitable -> Renderable
withMaterial material hitable ray tmin tmax =
  fmap (\hr -> RenderableHit (hr, material)) (hit hitable ray tmin tmax)

fromList :: [Renderable] -> Renderable
fromList rs ray tmin tmax =
  foldr join' Nothing rs
  where
    join' x closest = (x ray tmin tmax) <> closest

-- Dummy apply function to make usaage more explicit
-- e.g. hit (sphere ray ...) instead of sphere ray ...
hit :: a -> a
hit r = r
