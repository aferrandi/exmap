module Main exposing (main)
import Html        exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import WebSocket exposing (..)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
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

init : (Model, Cmd Msg)
init = emptyModel ! [  sendToServer WRAllProjects ]

view : Model -> Html Msg
view model = viewProjects model
        |> Material.Scheme.top

wsUrl : String
wsUrl = "ws://localhost:3000"

sendToServer : WebRequest -> Cmd Msg
sendToServer req = WebSocket.send wsUrl (encode 0 (encodeWebRequest req))


updateWithWebEvent : String -> Model -> (Model, Cmd Msg)
updateWithWebEvent json model = let _ = Debug.log ("Event " ++ json)
                                in case decodeString webEventDecoder json of
                                      Ok evt -> updateEvent evt model
                                      Err err -> updateEvent (WEError err) model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Receive json -> updateWithWebEvent json model
    Send req -> model ! [ WebSocket.send wsUrl req ]
    Mdl msg_ -> Material.update Mdl msg_ model
    SelectTab idx -> ({ model | tab = idx }, Cmd.none)
    OpenProject pn -> (model , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl Receive

updateEvent : WebEvent -> Model -> (Model, Cmd Msg)
updateEvent evt model = case evt of
                            WEAllProjects ap -> ({ model | allProjects = ap }, Cmd.none)
                            otherwise -> (model , Cmd.none)

