module Main exposing (main)

import Html        exposing (..)
import WebSocket exposing (..)
import Json.Encode exposing (encode)
import Material
import Material.Scheme
import Material.Color as Color
import Material.Layout as Layout

import InternalMessageUpdate exposing (..)
import XMapTypes exposing (..)
import Project exposing (..)
import Views exposing (..)
import DecodeWebEvent exposing (..)
import EncodeWebRequest exposing (..)
import WebMessages exposing (..)
import ProjectModel exposing (..)
import ProjectsUI exposing (..)
import WebMessageUpdate exposing (..)


main = Html.program
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

init : (Model, Cmd Msg)
init = ( emptyModel ! [  sendToServer WRAllProjects, Layout.sub0 Mdl ] )

view : Model -> Html Msg
view model = viewProjects model |> Material.Scheme.topWithScheme Color.Teal Color.Red
        -- |> Material.Scheme.top

wsUrl : String
wsUrl = "ws://localhost:3000"

sendToServer : WebRequest -> Cmd Msg
sendToServer req = WebSocket.send wsUrl (encode 0 (encodeWebRequest req))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Receive json -> updateWithWebEvent json model
    Send req -> model ! [ sendToServer req ]
    Mdl msg_ -> Material.update Mdl msg_ model
    Internal msg -> updateInternal (Debug.log "Internal received:" msg) model

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl Receive


