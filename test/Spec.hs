module Main (main) where

import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Unit tests" unitTests
  , testGroup "Property tests" propertyTests
  ]

unitTests :: [TestTree]
unitTests = []

propertyTests :: [TestTree]
propertyTests = []

