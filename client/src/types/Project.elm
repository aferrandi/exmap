module Project exposing (..)

import XMapTypes exposing (..)
import Views exposing (..)

type alias Error =  String

type OperationMode = Union | Intersection

type OperationName = Add
    | Subtract

type ApplicationName = Negate

type alias CalculationName = String

type alias CalculationFormulaText = String

type alias ProjectName = String

type alias OdbcSourceType = { connectionString : String, sqlQuery : String}
type alias HttpSourceType = { url : String }

type SourceType = FileSource
      | OdbcSource OdbcSourceType
      | HttpSource HttpSourceType

type alias Source = {
    sourceType : SourceType,
    sourceOfMaps : List XMapName}

type alias Project = {
    projectName : ProjectName,
    calculations : List CalculationName,
    viewNames : List ViewName,
    sources : List Source} -- maps could come from different sources

type alias AllProjects = List ProjectName
