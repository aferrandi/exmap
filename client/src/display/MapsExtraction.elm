module MapsExtraction exposing (..)

import Set as Set
import Dict as Dict

import Views exposing (..)
import XMapTypes exposing (..)
import ProjectModel exposing (..)

mapValues : XMap -> Dict.Dict String String
mapValues m =
    let keyValueToString _ v =  toString v
    in case m of
     XMapInt (MapValue mapInt) -> Dict.map keyValueToString mapInt
     XMapString (MapValue mapString) -> Dict.map keyValueToString mapString
     XMapBool (MapValue mapBool) -> Dict.map keyValueToString mapBool
     XMapDouble (MapValue mapDouble) -> Dict.map keyValueToString mapDouble


itemToTable : XMapByName -> Set.Set XMapKey -> ViewItem -> List String
itemToTable ms ids item =
    let idsMap = Set.foldr (\id dict -> Dict.insert id  "" dict) Dict.empty ids
        mapValuesInDict xmapName = case Dict.get xmapName ms of
            Just m -> mapValues m
            Nothing -> idsMap
        mapValuesForEachId xmapName = Dict.values (Dict.union (mapValuesInDict xmapName) idsMap)
      in case item of
         MapItem xmapName -> mapValuesForEachId xmapName
         LabelItem _ -> Dict.values idsMap

mapKeys : XMap -> List XMapKey
mapKeys m = case m of
               XMapInt (MapValue mapInt) -> Dict.keys  mapInt
               XMapString (MapValue mapString) -> Dict.keys  mapString
               XMapBool (MapValue mapBool) -> Dict.keys  mapBool
               XMapDouble (MapValue mapDouble) -> Dict.keys  mapDouble
