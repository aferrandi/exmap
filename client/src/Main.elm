module Main exposing (main)

import Html        exposing (..)
import Material
import Material.Scheme
import Material.Color as Color
import Material.Layout as Layout

import InternalMessageUpdate exposing (..)
import EmptyModel exposing (emptyModel)
import XMapTypes exposing (..)
import Project exposing (..)
import Views exposing (..)
import DecodeWebEvent exposing (..)
import WebMessages exposing (..)
import ProjectModel exposing (..)
import ProjectsUI exposing (..)
import WebMessageUpdate exposing (..)
import ServerMessaging exposing (..)


main = Html.program
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

init : (Model, Cmd Msg)
init = ( emptyModel ! [  sendToServer WRAllProjects, Layout.sub0 Mdl ] )


view : Model -> Html Msg
view model = viewProjects model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Receive json -> updateWithWebEvent (Debug.log "Web received:" json) model
    Send req -> model ! [ sendToServer req ]
    SendMany reqs -> model ! (List.map sendToServer reqs)
    Mdl msg_ -> Material.update Mdl msg_ model
    Internal msg -> updateInternal (Debug.log "Internal received:" msg ) (Debug.log "Model: " model)

subscriptions : Model -> Sub Msg
subscriptions model = subscriptionsToServer model


