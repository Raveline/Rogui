module Main (main) where

import Rogui.Components.Layout
import Rogui.Components.MessageLogTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [layoutTests, messageLogTests]