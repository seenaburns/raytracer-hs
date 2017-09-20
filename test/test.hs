-- module Main where
--
-- import System.Exit (exitFailure)
--
-- main = do
--     putStrLn "This test always fails!"
--     exitFailure
--

import Vec3
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

approx_equal :: Float -> Float -> Bool
approx_equal a b = abs (a-b) < 0.000001

assert_approx_equal :: Float -> Float -> Assertion
assert_approx_equal a b =
  assertBool msg (approx_equal a b)
  where msg = "expected: " ++ (show b) ++ " but got: " ++ (show a)

checkSquaredLength :: Float -> Float -> Float -> Bool
checkSquaredLength x y z = Vec3.squaredLength (Vec3 x y z) == (x*x + y*y + z*z)

suite :: TestTree
suite = testGroup "Test Suite" [
    testGroup "Units"
      [
        testCase "toList" $ Vec3.toList (Vec3 1 2 3) @=? [1,2,3],
        testCase "dot"    $ Vec3.dot (Vec3 1 2 3) (Vec3 4 5 6) @=? 32,
        testCase "dot"    $ Vec3.dot (Vec3 1 0 0) (Vec3 0 1 0) @=? 0,
        testCase "cross"  $ Vec3.cross (Vec3 1 2 3) (Vec3 4 5 6) @=? (Vec3 (-3) 6 (-3))
      ],

    testGroup "QuickCheck tests"
      [
        testProperty "Quickcheck squaredLength"
          (\x y z -> Vec3.squaredLength (Vec3 x y z) == (x*x + y*y + z*z)),
        testProperty "Quickcheck length"
          (\x y z ->
            let v = Vec3 x y z
            in Vec3.length v == sqrt (Vec3.squaredLength v)
          ),
        testProperty "Quickcheck normalized"
          (\x y z ->
            let v = Vec3 x y z
                a = Vec3.normalized v
                b = Vec3.div v (Vec3.length v)
            in ((Vec3.length v) == 0) || (
               (approx_equal (Vec3.x a) (Vec3.x b)) &&
               (approx_equal (Vec3.y a) (Vec3.y b)) &&
               (approx_equal (Vec3.z a) (Vec3.z b)))
          )
      ]
  ]

main :: IO ()
main = defaultMain suite
