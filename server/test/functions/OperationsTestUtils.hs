module OperationsTestUtils (assertXMapDoubleEqual, assertXMapBoolEqual) where

import Test.HUnit

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Time.Clock as DT
import Control.Monad (unless)

import XMapTypes
import XFunction
import XValues

assertEqualDouble :: String -> Double -> Double -> Double -> Assertion
assertEqualDouble preface delta expected actual =
  unless (abs (expected - actual) < delta) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
                    "expected: " ++ show expected ++ "\n but got: " ++ show actual

assertXMapDoubleEqual :: MapValue Double -> XMapErr -> Assertion
assertXMapDoubleEqual ms xs = case xs of
                          (Right (XMapDouble ds)) -> assertMapDoubleEqual ms ds
                          err -> assertFailure $ "The result is not a matrix but " ++ show err

assertMapDoubleEqual :: MapValue Double -> MapValue Double -> Assertion
assertMapDoubleEqual as bs = do
    assertEqual "the map keys are not the same" ( M.keys as) (M.keys bs)
    mapM_ (\(a, b) ->  assertEqualsValues a b) (L.zip (M.elems as) (M.elems bs))
    where assertEqualsValues a b = assertEqualDouble "the map values are not the same" 0.0001 a b

assertMapEqual :: (XValue a, Eq a, Show a) => MapValue a -> MapValue a -> Assertion
assertMapEqual as bs = do
    assertEqual "the map keys are not the same" ( M.keys as) (M.keys bs)
    mapM_ (\(a, b) ->  assertEqual "the map values are not the same" a b) (L.zip (M.elems as) (M.elems bs))

assertXMapBoolEqual :: MapValue Bool -> XMapErr -> Assertion
assertXMapBoolEqual ms xs = case xs of
                          (Right (XMapBool ds)) -> assertMapEqual ms ds
                          err -> assertFailure $ "The result is not a matrix but " ++ show err
