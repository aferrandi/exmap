module Main exposing (main)

import Html        exposing (..)
import Html.Events exposing (..)
import WebSocket

main = Html.program
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

type alias Model
  = Int

type Msg
  = data WebRequest =
        WRLoadProject ProjectName
        | WRNewProject Project
        | WRUpdateProject Project
        | WRLoadMap ProjectName XMapName
        | WRStoreMap ProjectName XNamedMap
        | WRSubscribeToView ProjectName ViewName
        | WRUnsubscribeFromView ProjectName ViewName
        deriving (Show, Eq)

    data WebEvent =
        WEViewChanged ProjectName ViewName XNamedMap
        | WEProjectContent Project
        | WEProjectStored ProjectName
        | WEMapLoaded ProjectName XNamedMap
        | WEMapStored ProjectName XMapName
        | WEUnsubscribedFromView ProjectName ViewName
        | WEViewStatus ProjectName View [XNamedMap]
        | WEError Error
        deriving (Show, Eq)


  | Send

init : (Model, Cmd Msg)
init =
  (0, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ p [] [ text <| "Pokes: " ++ toString model ]
    , button [ onClick Send ] [ text "Poke others" ]
    ]

wsUrl : String
wsUrl = "ws://localhost:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Receive "poke" ->
      (model + 1) ! []
    Receive _ ->
      model ! []
    Send ->
      model ! [ WebSocket.send wsUrl "poke" ]

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl Receive

