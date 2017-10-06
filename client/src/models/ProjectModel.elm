module ProjectModel exposing (..)

import Material
import Dict as Dict

import Project exposing (..)
import Views exposing (..)
import XMapTypes exposing (..)
import WebMessages exposing (WebRequest)

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
    , messages : List Error
    }

type Msg
  = Receive String
  | Send WebRequest
  | Mdl (Material.Msg Msg)
  | SelectProjectTab Int
  | SelectViewTab Int

emptyModel : Model
emptyModel = { openProjects = []
               , allProjects = []
               , messages = ["testMessage"]
               , mdl =Material.model
               , projectTab = 0
               , viewTab = 0
               }


