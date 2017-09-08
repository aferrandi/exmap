module Main exposing (main)

{-

client for exmap

-}

import Html        exposing (..)
import Html.Events exposing (..)
import WebSocket exposing (..)
import Json.Decode exposing (decodeString)

import XMapTypes exposing (..)
import Project exposing (..)
import Views exposing (..)
import DecodeWebEvent exposing (..)
import EncodeWebRequest exposing (..)
import WebMessages exposing (..)

main = Html.program
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

type alias Model
  = Int

type Msg
  = Receive String
  | Send String

init : (Model, Cmd Msg)
init =
  (0, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ p [] [ text <| "Pokes: " ++ toString model ]
    , button [ onClick (Send "pippo") ] [ text "Project" ]
    ]

wsUrl : String
wsUrl = "ws://localhost:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Receive json -> case decodeString webEventDecoder json of
                      Ok evt -> updateEvent evt model
                      Err err -> updateEvent (WEError err) model
    Send req -> model ! [ WebSocket.send wsUrl req ]

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl Receive

updateEvent : WebEvent -> Model -> (Model, Cmd Msg)
updateEvent evt model = (model , Cmd.none)

