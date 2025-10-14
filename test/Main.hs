module Main (main) where

import Rogui.Components.Layout
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [layoutTests]