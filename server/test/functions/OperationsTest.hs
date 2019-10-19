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
testMapDoubleB = makeDoubleXMap [("a", 1.4), ("c", -2.4), ("d", 3.1)]

add_standard = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Add) Union [testMapDoubleA, testMapDoubleB]
          expected = makeDoubleMap [("a", 2.6), ("b", 2.3), ("c", 1.1), ("d", 3.1)]

subtract_standard = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Subtract) Union [testMapDoubleA, testMapDoubleB]
          expected = makeDoubleMap [("a", -0.2), ("b", 2.3), ("c", 5.9), ("d", -3.1)]

times_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Times) Union [testMapDoubleA, testMapDoubleB]
          expected = makeDoubleMap [("a", 1.68), ("b", 0), ("c", -8.4), ("d", 0)]

negate_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Negate) Union [testMapDoubleA]
          expected = makeDoubleMap [("a", -1.2), ("b", -2.3), ("c", -3.5)]

sin_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Sin) Union [testMapDoubleA]
          expected = makeDoubleMap [("a", 0.93203908596), ("b", 0.74570521217), ("c", -0.35078322769)]

cos_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Cos) Union [testMapDoubleA]
          expected = makeDoubleMap [("a", 0.362357754), ("b", -0.666276021), ("c", -0.936456687)]

tan_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Tan) Union [testMapDoubleA]
          expected = makeDoubleMap [("a", 2.572151622), ("b", -1.119213642), ("c", 0.37458564)]

exp_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Exp) Union [testMapDoubleA]
          expected = makeDoubleMap [("a", 3.320116923), ("b", 9.974182455), ("c", 33.115451959)]

log_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Log) Union [testMapDoubleA]
          expected = makeDoubleMap [("a", 0.1823215567939546), ("b", 0.8329091229351039), ("c", 1.252762968495368)]

log_minus  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Log) Union [testMapDoubleB]
          expected = makeDoubleMap [("a", 0.3364722366212129), ("d", 1.1314021114911006)]

keysTo_union  = TestCase $ assertXMapDoubleEqual expected actual
     where keyMap = makeStringXMap [("a", "e"), ("b", "f"), ("d", "g")]
           actual = (operationRepository KeysTo) Union [keyMap, testMapDoubleA]
           expected = makeDoubleMap [("c", 3.5), ("e", 1.2), ("f", 2.3)]

keysTo_interesection  = TestCase $ assertXMapDoubleEqual expected actual
     where keyMap = makeStringXMap [("a", "e"), ("b", "f"), ("d", "g")]
           actual = (operationRepository KeysTo) Intersection [keyMap, testMapDoubleA]
           expected = makeDoubleMap [("e", 1.2), ("f", 2.3)]

merge_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Merge) Union [testMapDoubleA, testMapDoubleB]
          expected = makeDoubleMap [("a", 1.2), ("b", 2.3), ("c", 3.5), ("d", 3.1)]

tests = [
          add_standard,
          subtract_standard,
          times_standard,
          negate_standard,
          sin_standard,
          cos_standard,
          tan_standard,
          exp_standard,
          log_standard,
          log_minus,
          keysTo_union,
          keysTo_interesection,
          merge_standard
        ]


