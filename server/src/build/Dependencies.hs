module Dependencies where

import Data.List
import Data.Maybe (mapMaybe)

import Formula
import View
import XMapTypes
import Calculation

formulaDependenciesMaps :: XFormula -> [XMapName]
formulaDependenciesMaps (XFMap n) = [n]
formulaDependenciesMaps (XFOperation _ fs) = nub (concatMap formulaDependenciesMaps fs)

viewDependenciesMaps :: View -> [XMapName]
viewDependenciesMaps (View _ rs)= concatMap rowDependencies rs
    where rowDependencies (ViewRow items _)  = mapMaybe itemDependencies items
          itemDependencies (MapItem m) = Just m
          itemDependencies (LabelItem _) = Nothing

calculationDependenciesMaps :: Calculation -> [XMapName]
calculationDependenciesMaps c = formulaDependenciesMaps $ formula c

