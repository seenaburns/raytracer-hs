import Vec3
import Ray
import Model
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function
import Control.Applicative

instance (Arbitrary a) => Arbitrary (Vec3Generic a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Vec3Generic x y z

--
-- Utility functions
--
approx_equal :: Float -> Float -> Bool
approx_equal a b = abs (a-b) < 0.000001

assert_approx_equal :: Float -> Float -> Assertion
assert_approx_equal a b =
  assertBool msg (approx_equal a b)
  where msg = "expected: " ++ (show b) ++ " but got: " ++ (show a)

--
-- Vec3 quickcheck tests
--
propSquaredLength :: Vec3 -> Bool
propSquaredLength (Vec3Generic x y z) = squaredLength (vec3 x y z) == (x*x + y*y + z*z)
propLength :: Vec3 -> Bool
propLength v = Vec3.length v == sqrt (squaredLength v)
propNormalized :: Vec3 -> Property
propNormalized v =
  (Vec3.length v) /= 0 ==> reduce (&&) (liftA2 (approx_equal) a b)
  where
    a = normalized v
    b = v /: (Vec3.length v)
propVec3ApplicativeIdentity :: Vec3 -> Bool
propVec3ApplicativeIdentity v = (pure id <*> v) == v
propVec3ApplicativeHomomorphism :: Int -> Fun Int Int -> Bool
propVec3ApplicativeHomomorphism x f = a == b
  where
    ff = apply f
    a = pure ff <*> pure x :: Vec3Generic Int
    b = pure (ff x) :: Vec3Generic Int
propVec3ApplicativeComposition :: Fun Int Int -> Fun Int Int -> Vec3Generic Int -> Bool
propVec3ApplicativeComposition u v x = a == b
  where
    uu = pure (apply u)
    vv = pure (apply v)
    a = pure (.) <*> uu <*> vv <*> x
    b = uu <*> (vv <*> x)
propVec3ApplicativeInterchange :: Fun Int Int -> Int -> Bool
propVec3ApplicativeInterchange f x = a == b
  where
    ff = pure (apply f)
    a = ff <*> pure x :: Vec3Generic Int
    b = pure ($ x) <*> ff :: Vec3Generic Int

suite :: TestTree
suite = testGroup "Test Suite" [
    testGroup "Units"
      [
        -- Vec3
        testCase "Vec3 toList" $ toList (vec3 1 2 3) @=? [1,2,3],
        testCase "Vec3 dot"    $ dot (vec3 1 2 3) (vec3 4 5 6) @=? 32,
        testCase "Vec3 dot"    $ dot (vec3 1 0 0) (vec3 0 1 0) @=? 0,
        testCase "Vec3 cross"  $ cross (vec3 1 2 3) (vec3 4 5 6) @=? (vec3 (-3) 6 (-3)),
        -- Ray
        testCase "Ray pointAtParameter" $ pointAtParameter (Ray (vec3 1 1 1) (vec3 1 2 3)) 2 @=? (vec3 3 5 7),
        -- Hitables
        -- Sphere
        testCase "Hitable Sphere" $
          hit (sphere (vec3 0 0 0) 1) (rayFromTo (vec3 0 0 (-2)) (vec3 0 0 (-1))) 0.0001 1000 @=?
          Just (HitRecord 1 (vec3 0 0 (-1)) (vec3 0 0 (-1))),
        testCase "Hitable Sphere 2" $
          hit (sphere (vec3 0 0 0) 1) (rayFromTo (vec3 0 0 (-1)) (vec3 1 0 0)) 0.0001 1000 @=?
          Just (HitRecord 1 (vec3 1 0 0) (vec3 1 0 0)),
        -- Hitable List
        testCase "Hitable List" $
          let
            s1 = sphere (vec3 0 0 5) 1
            s2 = sphere (vec3 0 0 0) 1
            hl = hitableList [s1,s2]
            ray = rayFromTo (vec3 0 0 (-2)) (vec3 0 0 (-1))
          in
            hit hl ray 0.0001 1000 @=? Just (HitRecord 1 (vec3 0 0 (-1)) (vec3 0 0 (-1))),
        testCase "Hitable List With Miss" $
          let
            s1 = sphere (vec3 0 0 5) 1
            s2 = sphere (vec3 10 0 0) 1
            hl = hitableList [s1,s2]
            ray = rayFromTo (vec3 0 0 (-2)) (vec3 0 0 (-1))
          in
            hit hl ray 0.0001 1000 @=? Just (HitRecord 6 (vec3 0 0 4) (vec3 0 0 (-1)))
      ],

    testGroup "QuickCheck tests"
      [
        testProperty "Quickcheck Vec3 squaredLength" propSquaredLength,
        testProperty "Quickcheck Vec3 length" propLength,
        testProperty "Quickcheck Vec3 normalized" propNormalized,
        testProperty "Quickcheck Vec3 applicative identity" propVec3ApplicativeIdentity,
        testProperty "Quickcheck Vec3 applicative composition" propVec3ApplicativeComposition,
        testProperty "Quickcheck Vec3 applicative homomorphism" propVec3ApplicativeHomomorphism,
        testProperty "Quickcheck Vec3 applicative interchange" propVec3ApplicativeInterchange
      ]
  ]

main :: IO ()
main = defaultMain suite
