import Vec3
import Ray
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
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
        testCase "Ray pointAtParameter" $ pointAtParameter (Ray (vec3 1 1 1) (vec3 1 2 3)) 2 @=? (vec3 3 5 7)
      ],

    testGroup "QuickCheck tests"
      [
        testProperty "Quickcheck Vec3 squaredLength" propSquaredLength,
        testProperty "Quickcheck Vec3 length" propLength,
        testProperty "Quickcheck Vec3 normalized" propNormalized
      ]
  ]

main :: IO ()
main = defaultMain suite
