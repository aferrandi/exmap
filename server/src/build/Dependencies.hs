module Dependencies where

import Data.List
import Data.Maybe (mapMaybe)

import Formula
import View
import XMapTypes
import Calculation

formulaDependencies :: XFormula -> [XMapName]
formulaDependencies (XFMap n) = [n]
formulaDependencies (XFOperation _ fs) = nub (concatMap formulaDependencies fs)

viewDependencies :: View -> [XMapName]
viewDependencies (View _ rs)= concatMap rowDependencies rs
    where rowDependencies (ViewRow items)  = mapMaybe itemDependencies items
          itemDependencies (MapItem m) = Just m
          itemDependencies (LabelItem _) = Nothing

calculationDependencies :: Calculation -> [XMapName]
calculationDependencies c = formulaDependencies $ formula c

