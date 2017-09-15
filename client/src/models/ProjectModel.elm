module ProjectModel exposing (..)

import Material
import Dict exposing (..)

import Project exposing (..)
import Views exposing (..)
import WebMessages exposing (WebRequest)

type alias Mdl = Material.Model

type alias ViewModel = {
    view : View}

type alias ProjectModel = {
    project : Project
    , openViews : List ViewModel }

type alias Model = {
    openProjects : Dict ProjectName ProjectModel
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
emptyModel = { openProjects = Dict.empty
               , allProjects = []
               , messages = []
               , mdl =Material.model
               , projectTab = 0
               , viewTab = 0
               }


