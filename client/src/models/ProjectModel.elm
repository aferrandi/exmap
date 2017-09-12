module ProjectModel exposing (..)

import Material

import Project exposing (..)
import Views exposing (..)

type alias Mdl = Material.Model

type alias ViewModel = {
    view : View}

type alias ProjectModel = {
    project : Project
    , openViews : List ViewModel }

type alias Model = {
    openProjects : List ProjectModel
    , mdl : Material.Model
    , tab : Int
    }
type Msg
  = Receive String
  | Send String
  | Mdl (Material.Msg Msg)
  | SelectTab Int