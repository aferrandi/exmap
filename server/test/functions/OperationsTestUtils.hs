module OperationsTestUtils (assertXMapDoubleEqual, assertXMapBoolEqual, assertXMapIntEqual, assertXMapStringEqual) where

import Test.HUnit

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Time.Clock as DT
import qualified Data.Text as T
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
                          err -> assertFailure $ "The result is not a map but " ++ show err

assertMapDoubleEqual :: MapValue Double -> MapValue Double -> Assertion
assertMapDoubleEqual as bs = do
    assertEqual "the map keys are not the same" ( M.keys as) (M.keys bs)
    mapM_ (\(a, b, k) ->  assertEqualsValues a b k) (L.zip3 (M.elems as) (M.elems bs) (M.keys as))
    where assertEqualsValues a b k = assertEqualDouble ("the map values are not the same for " ++ (show k)) 0.0001 a b

assertMapEqual :: (XValue a, Eq a, Show a) => MapValue a -> MapValue a -> Assertion
assertMapEqual as bs = do
    assertEqual "the map keys are not the same" ( M.keys as) (M.keys bs)
    mapM_ (\(a, b, k) ->  assertEqual ("the map values are not the same for " ++ (show k)) a b) (L.zip3 (M.elems as) (M.elems bs) (M.keys as))

assertXMapBoolEqual :: MapValue Bool -> XMapErr -> Assertion
assertXMapBoolEqual ms xs = case xs of
                          (Right (XMapBool ds)) -> assertMapEqual ms ds
                          err -> assertFailure $ "The result is not a map but " ++ show err

assertXMapIntEqual :: MapValue Int -> XMapErr -> Assertion
assertXMapIntEqual ms xs = case xs of
                          (Right (XMapInt ds)) -> assertMapEqual ms ds
                          err -> assertFailure $ "The result is not a map but " ++ show err

assertXMapStringEqual :: MapValue T.Text -> XMapErr -> Assertion
assertXMapStringEqual ms xs = case xs of
                          (Right (XMapString ds)) -> assertMapEqual ms ds
                          err -> assertFailure $ "The result is not a map but " ++ show err