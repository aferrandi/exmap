module OperationsTest (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Map.Strict as M

import OperationsTestUtils
import Operations
import TestTypes
import XMapTypes
import XFunction

testMapDoubleA :: XMap
testMapDoubleA = makeDoubleXMap [("a", 1.2), ("b", 2.3), ("c", 3.5)]

testMapDoubleB :: XMap
testMapDoubleB = makeDoubleXMap [("a", 1.4), ("c", 2.4), ("d", 3.1)]

add_standard = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Add) Union [testMapDoubleA, testMapDoubleB]
          expected = makeDoubleMap [("a", 2.6), ("b", 2.3), ("c", 5.9), ("d", 3.1)]

subtract_standard = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Subtract) Union [testMapDoubleA, testMapDoubleB]
          expected = makeDoubleMap [("a", -0.2), ("b", 2.3), ("c", 1.1), ("d", -3.1)]


tests = [add_standard, subtract_standard]


