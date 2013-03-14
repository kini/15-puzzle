import Test.HUnit
import Data.List
import Puzzle

-- Here are a few tests. Until the AI is completed, there's not a whole
-- lot to test, but here are some tests of the puzzle generation
-- functions, for what it's worth.

evenQ :: Ord a => [a] -> Bool
evenQ = (==0) . flip mod 2 . inversions
  where
    inversions :: Ord a => [a] -> Integer
    inversions [] = 0
    inversions (x:ys) = sum [1 | y <- ys, x > y] + inversions ys

unitTests = [
  -- Test whether nthPermutation actually produces all the permutations
  TestCase (assertEqual "nthPermutation produces all permutations"
            (sort (map (flip nthPermutation [0..7]) [0..40319]))
            (sort (permutations [0..7]))),

  -- Test whether nthPermutation alternately provides even and odd
  -- permutations
  TestCase (assertBool
            "nthPermutation produces even permutations for even input" $
            and $ zipWith (==) (cycle [True, False]) $ map evenQ $
            map (flip nthPermutation [0..7]) [0..40319]) ]

main = do
  runTestTT $ TestList unitTests
