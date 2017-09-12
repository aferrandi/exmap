module Main exposing (main)
import Html        exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import WebSocket exposing (..)
import Json.Decode exposing (decodeString)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css)

import XMapTypes exposing (..)
import Project exposing (..)
import Views exposing (..)
import DecodeWebEvent exposing (..)
import EncodeWebRequest exposing (..)
import WebMessages exposing (..)
import ProjectModel exposing (..)
import ProjectsUI exposing (..)


main = Html.program
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

type Msg
  = Receive String
  | Send String
  | Mdl (Material.Msg Msg)

init : (Model, Cmd Msg)
init = ({ openProjects = []
       , mdl =Material.model
       }, Cmd.none)

type alias Mdl = Material.Model

view : Model -> Html Msg
view model = viewProjects model
        |> Material.Scheme.top

wsUrl : String
wsUrl = "ws://localhost:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Receive json -> case decodeString webEventDecoder json of
                      Ok evt -> updateEvent evt model
                      Err err -> updateEvent (WEError err) model
    Send req -> model ! [ WebSocket.send wsUrl req ]
    Mdl msg_ -> Material.update Mdl msg_ model

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl Receive

updateEvent : WebEvent -> Model -> (Model, Cmd Msg)
updateEvent evt model = (model , Cmd.none)

