module ProjectModel exposing (..)

import Material
import Dict as Dict

import Project exposing (..)
import Views exposing (..)
import XMapTypes exposing (..)
import WebMessages exposing (WebRequest)
import List.Extra as ListX

type alias Mdl = Material.Model

type alias XMapByName = Dict.Dict XMapName XMap

type alias ViewModel = {
    view : View
    , maps : XMapByName
    }

type alias ProjectModel = {
    project : Project
    , openViews : List ViewModel
    }

type alias Model = {
    openProjects : List ProjectModel
    , allProjects : List ProjectName
    , mdl : Material.Model
    , projectTab : Int
    , viewTab : Int
    , xmapName : Maybe XMapName
    , xmapType : Maybe XMapType
    , xmapToEdit : Maybe XMap
    , xmapEditing : Maybe String
    , newXmapName : String
    , messages : List Error
    }

type InternalMsg =
  SelectProjectTab Int
  | SelectViewTab Int
  | NewProject
  | MapToTextArea
  | MapToTable
  | TextToTextArea String
  | NewMapName String
  | ShowMessage String

type Msg
  = Receive String
  | Send WebRequest
  | Mdl (Material.Msg Msg)
  | Internal InternalMsg

mdlIdxProjects = 0
mdlIdxViews = 1

emptyModel : Model
emptyModel = { openProjects = []
               , allProjects = []
               , messages = ["Client started"]
               , mdl =Material.model
               , projectTab = 0
               , viewTab = 0
               , xmapName = Nothing
               , xmapType = Nothing
               , xmapToEdit = Nothing
               , xmapEditing = Nothing
               , newXmapName = ""
               }

currentOpenProject : Model -> Maybe ProjectModel
currentOpenProject model = ListX.getAt model.projectTab model.openProjects