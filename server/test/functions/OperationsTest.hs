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
testMapDoubleA = makeXMap arr
  where arr :: [(String, Double)]
        arr = [("a", 1.2), ("b", 2.3), ("c", 3.5)]

testMapDoubleB :: XMap
testMapDoubleB = makeXMap arr
  where arr :: [(String, Double)]
        arr = [("a", 1.4), ("c", -2.4), ("d", 3.1)]

testMapDoubleC :: XMap
testMapDoubleC = makeXMap arr
  where arr :: [(String, Double)]
        arr = [("a", 1.4), ("b", -2.4), ("c", 0)]

testMapStringA :: XMap
testMapStringA = makeStringXMap  arr
  where arr :: [(String, String)]
        arr = [("a", "morning"), ("b", "day"), ("c", "night"), ("d", "")]

testMapIntA :: XMap
testMapIntA = makeXMap arr
  where arr :: [(String, Int)]
        arr = [("a", 1), ("b", -2), ("c", 3)]

testMapBoolA :: XMap
testMapBoolA = makeXMap arr
  where arr :: [(String, Bool)]
        arr = [("a", True), ("b", True), ("c", False), ("d", False)]

testMapBoolB :: XMap
testMapBoolB = makeXMap arr
  where arr :: [(String, Bool)]
        arr = [("a", True), ("b", False), ("c", True), ("d", False), ("e", False)]



add_standard = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Add) Union [testMapDoubleA, testMapDoubleB]
          expected = makeMap [("a", 2.6), ("b", 2.3), ("c", 1.1), ("d", 3.1)]

subtract_standard = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Subtract) Union [testMapDoubleA, testMapDoubleB]
          expected = makeMap [("a", -0.2), ("b", 2.3), ("c", 5.9), ("d", -3.1)]

times_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Times) Union [testMapDoubleA, testMapDoubleB]
          expected = makeMap [("a", 1.68), ("b", 0), ("c", -8.4), ("d", 0)]

divide_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Divide) Union [testMapDoubleA, testMapDoubleB]
          expected = makeMap [("a", 0.85714285714), ("c", -1.45833333333), ("d", 0)]

negate_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Negate) Union [testMapDoubleA]
          expected = makeMap [("a", -1.2), ("b", -2.3), ("c", -3.5)]

abs_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Abs) Union [testMapDoubleC]
          expected = makeMap [("a", 1.4), ("b", 2.4), ("c", 0)]

sin_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Sin) Union [testMapDoubleA]
          expected = makeMap [("a", 0.93203908596), ("b", 0.74570521217), ("c", -0.35078322769)]

cos_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Cos) Union [testMapDoubleA]
          expected = makeMap [("a", 0.362357754), ("b", -0.666276021), ("c", -0.936456687)]

tan_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Tan) Union [testMapDoubleA]
          expected = makeMap [("a", 2.572151622), ("b", -1.119213642), ("c", 0.37458564)]

exp_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Exp) Union [testMapDoubleA]
          expected = makeMap [("a", 3.320116923), ("b", 9.974182455), ("c", 33.115451959)]

log_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Log) Union [testMapDoubleA]
          expected = makeMap [("a", 0.1823215567939546), ("b", 0.8329091229351039), ("c", 1.252762968495368)]

log_minus  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Log) Union [testMapDoubleB]
          expected = makeMap [("a", 0.3364722366212129), ("d", 1.1314021114911006)]

keysTo_union  = TestCase $ assertXMapDoubleEqual expected actual
     where keyMap = makeStringXMap [("a", "e"), ("b", "f"), ("d", "g")]
           actual = (operationRepository KeysTo) Union [keyMap, testMapDoubleA]
           expected = makeMap [("c", 3.5), ("e", 1.2), ("f", 2.3)]

keysTo_interesection  = TestCase $ assertXMapDoubleEqual expected actual
     where keyMap = makeStringXMap [("a", "e"), ("b", "f"), ("d", "g")]
           actual = (operationRepository KeysTo) Intersection [keyMap, testMapDoubleA]
           expected = makeMap [("e", 1.2), ("f", 2.3)]

merge_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Merge) Union [testMapDoubleA, testMapDoubleB]
          expected = makeMap [("a", 1.2), ("b", 2.3), ("c", 3.5), ("d", 3.1)]

sum_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Sum) Union [testMapDoubleA]
          expected = makeMap [("sum", 7.0)]

average_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository Avg) Union [testMapDoubleA]
          expected = makeMap [("avg", 2.33333333)]

equals_double  = TestCase $ assertXMapBoolEqual expected actual
    where actual = (operationRepository Equals) Union [testMapDoubleA, makeDoubleXMap [("a", 1.2), ("b", 2.4), ("c", 3.5)]]
          expected = makeMap [("a", True), ("b", False), ("c", True)]

equals_string  = TestCase $ assertXMapBoolEqual expected actual
    where actual = (operationRepository Equals) Union [testMapStringA, makeStringXMap [("a", "Morning"), ("b", "dy"), ("c", "night")]]
          expected = makeMap [("a", False), ("b", False), ("c", True), ("d", True)]

len_standard  = TestCase $ assertXMapIntEqual expected actual
    where actual = (operationRepository Len) Union [testMapStringA]
          expected = makeMap [("a", 7), ("b", 3), ("c", 5), ("d", 0)]

toDecimal_standard  = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository ToDecimal) Union [testMapIntA]
          expected = makeMap [("a", 1.0), ("b", -2.0), ("c", 3.0)]

and_standard = TestCase $ assertXMapBoolEqual expected actual
    where actual = (operationRepository And) Union [testMapBoolA, testMapBoolB]
          expected = makeMap [("a", True), ("b", False), ("c", False), ("d", False), ("e", False)]

or_standard = TestCase $ assertXMapBoolEqual expected actual
    where actual = (operationRepository Or) Union [testMapBoolA, testMapBoolB]
          expected = makeMap [("a", True), ("b", True), ("c", True), ("d", False), ("e", False)]

ifThen_standard = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository IfThen) Union [testMapBoolA, testMapDoubleA]
          expected = makeMap [("a", 1.2), ("b", 2.3)]

ifThenElse_standard = TestCase $ assertXMapDoubleEqual expected actual
    where actual = (operationRepository IfThenElse) Union [testMapBoolA, testMapDoubleA, testMapDoubleB]
          expected = makeMap [("a", 1.2), ("b", 2.3), ("c", -2.4), ("d", 3.1)]

tests = [
          add_standard,
          subtract_standard,
          times_standard,
          divide_standard,
          negate_standard,
          abs_standard,
          sin_standard,
          cos_standard,
          tan_standard,
          exp_standard,
          log_standard,
          log_minus,
          keysTo_union,
          keysTo_interesection,
          merge_standard,
          sum_standard,
          average_standard,
          equals_double,
          equals_string,
          len_standard,
          toDecimal_standard,
          and_standard,
          or_standard,
          ifThen_standard,
          ifThenElse_standard
        ]


