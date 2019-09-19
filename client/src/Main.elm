module Main exposing (main)

import Browser
import Html        exposing (..)
import Material

import Handler.InternalMessageUpdate exposing (..)
import Models.EmptyModel exposing (emptyModel)
import Handler.ModelUpdate exposing (showMessage)
import Platform.Cmd exposing (batch)
import Models.WebMessages exposing (..)
import Models.ProjectModel exposing (..)
import Display.ProjectsUI exposing (..)
import Handler.WebMessageUpdate exposing (..)
import Server.ServerMessaging exposing (..)

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
         }

init : (Model, Cmd Msg)
init = (emptyModel, Cmd.batch [Material.init Mdc, connect 3000] )

view : Model -> Html Msg
view model = viewProjects model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    Receive json -> updateWithWebEvent (Debug.log "Web received:" json) model
    WebSocketConnected isConnected -> if isConnected
                                        then (showMessage model "Connected", sendToServer WRAllProjects)
                                        else (showMessage model "Disconnected", Cmd.none)
    Send req -> (model, batch [ sendToServer req ])
    SendMany reqs -> (model, batch (List.map sendToServer reqs))
    Mdc msg_ -> Material.update Mdc msg_ model
    Internal imsg -> updateInternal (Debug.log "Internal received:" imsg ) (Debug.log "Model: " model)
    None -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [
        onConnected model,
        subscriptionsToServer model
    ]


