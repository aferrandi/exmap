module MapEditor exposing (mapEditorView)

import Html exposing (..)
import Html.Events
import InternalMessages exposing (..)
import List.Extra as ListX
import MapsExtraction exposing (..)

import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Material.Options as Options
import Material.DataTable as DataTable
import Material.TextField as TextField
import MdlIndexes exposing (..)
import Project exposing (..)
import ProjectModel exposing (..)
import UIWrapper exposing (..)
import WebMessages exposing (WebRequest(..))
import XMapText exposing (..)
import XMapTypes exposing (..)


mapEditorView : Model -> ProjectModel -> Html Msg
mapEditorView model pm =
    let
        xmapEditorModel =
            model.xmapEditorModel

        title =
            case xmapEditorModel.xmapName of
                Just xmapName ->
                    "Editing map: " ++ xmapNameToString xmapName

                Nothing ->
                    "Map Editor"
    in
    div []
        [ titleWithIcon title "layers" "DarkOrange"
        , LayoutGrid.view [ heightInView 70 ]
            [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span2Desktop, LayoutGrid.span1Phone] [ mapEditorMapList model pm.project, newMapButton model ]
            , LayoutGrid.cell [LayoutGrid.span6Tablet, LayoutGrid.span10Desktop, LayoutGrid.span3Phone] [ mapEditorViewForMap model pm ]
            ]
        ]
-- Grid.noSpacing

mapEditorViewForMap : Model -> ProjectModel -> Html Msg
mapEditorViewForMap model pm =
    let
        xmapEditorModel =
            model.xmapEditorModel
    in
    case model.xmapEditorModel.xmapName of
        Just mn ->
            div []
                   -- Grid.noSpacing,
                [ LayoutGrid.view [ heightInView 60 ]
                    [ LayoutGrid.cell [LayoutGrid.span3Tablet, LayoutGrid.span5Desktop, LayoutGrid.span1Phone] [ mapEditorTextArea model pm ]
                    , LayoutGrid.cell [LayoutGrid.span5Tablet, LayoutGrid.span7Desktop, LayoutGrid.span3Phone] [ mapEditorTable xmapEditorModel.xmapToEdit ]
                    ]
                    --Grid.noSpacing
                , LayoutGrid.view [  ]
                    [ LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span4Desktop, LayoutGrid.span3Phone] [ buttonClick model (makeIndex mapEditorIdx 1) "To Table >" (Internal MapToTable) ]
                    , LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span4Desktop, LayoutGrid.span3Phone] [ buttonClick model (makeIndex mapEditorIdx 2) "< To Text" (Internal MapToTextArea) ]
                    , LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span4Desktop, LayoutGrid.span2Phone] [ buttonMaybe model (makeIndex mapEditorIdx 3) "Store" (Maybe.map2 (storeMap pm) xmapEditorModel.xmapName xmapEditorModel.xmapToEdit) ]
                    ]
                ]

        Nothing ->
            div [] []


xmapTypeChoice : Model -> Html Msg
xmapTypeChoice model =
    let
        hasType t =
            model.xmapEditorModel.xmapType == t
    in
    div []
        [ toggle model (makeIndex mapEditorIdx 4) "Double" "mapType" (hasType TypeDouble) (Internal (ChangeMapType TypeDouble))
        , toggle model (makeIndex mapEditorIdx 5) "Int" "mapType" (hasType TypeInt) (Internal (ChangeMapType TypeInt))
        , toggle model (makeIndex mapEditorIdx 6) "String" "mapType" (hasType TypeString) (Internal (ChangeMapType TypeString))
        , toggle model (makeIndex mapEditorIdx 7) "Bool" "mapType" (hasType TypeBool) (Internal (ChangeMapType TypeBool))
        ]


newMapButton : Model -> Html Msg
newMapButton model =
    let
        xmapEditorModel =
            model.xmapEditorModel

        storeNewMap =
            case xmapNameFromString xmapEditorModel.newXmapName of
                Ok mn ->
                    Internal (NewMapWithName mn xmapEditorModel.xmapType)

                Err e ->
                    Internal (ShowMessage e)
    in
    div []
        [ xmapTypeChoice model
        , TextField.view Mdc
            (makeIndex mapEditorIdx  8)
            model.mdc
            [ TextField.label "New map name"
            --, TextField.floatingLabel
            -- , TextField.text_
            , Options.onInput (\s -> Internal (UpdateMapName s))
            , TextField.value xmapEditorModel.newXmapName
            ]
            []
        , buttonClick model (makeIndex mapEditorIdx 9) "New map" storeNewMap
        ]


mapEditorMapList : Model -> Project -> Html Msg
mapEditorMapList model p =
    let
        sendShowMap index = sendListMsg (\mn -> Internal (ShowMapInEditor mn)) (fileSourcesOfProject p) index
        listItem mn =
            Lists.li []
                [
                    Lists.graphicIcon [] "list",
                    text (xmapNameToString mn)
                ]
    in
    Lists.ul Mdc (makeIndex mapEditorIdx 11) model.mdc
        [ Lists.onSelectListItem sendShowMap ]  -- (scrollableListStyle 50)
        (List.map listItem (fileSourcesOfProject p))


storeMap : ProjectModel -> XMapName -> XMap -> Msg
storeMap pm n m =
    WRStoreMap pm.project.projectName { xmapName = n, xmap = m } |> Send


fileSourcesOfProject : Project -> List XMapName
fileSourcesOfProject p =
    let
        maybeMaps : Maybe (List XMapName)
        maybeMaps =
            ListX.find (\s -> s.sourceType == FileSource) p.sources |> Maybe.map (\s -> s.sourceOfMaps)
    in
    Maybe.withDefault [] maybeMaps


mapEditorTextArea : Model -> ProjectModel -> Html Msg
mapEditorTextArea model pm =
    TextField.view Mdc
        (makeIndex mapEditorIdx 10)
        model.mdc
        [ TextField.label "Enter the map data"
        --, TextField.floatingLabel
        , TextField.textarea
        , TextField.rows 20
        , TextField.value (Maybe.withDefault "" model.xmapEditorModel.xmapEditing)
        , Options.onInput (\s -> Internal (TextToMapTextArea s))
        ]
        []


mapEditorTableFull : XMap -> Html Msg
mapEditorTableFull m =
    DataTable.table (scrollableTableStyle 50)
        [ mapHeader
        , mapRows m
        ]


mapEditorTableEmpty : Html Msg
mapEditorTableEmpty =
    DataTable.table []
        [ mapHeader
        , DataTable.tbody [] []
        ]


mapEditorTable : Maybe XMap -> Html Msg
mapEditorTable mm =
    case mm of
        Just m ->
            mapEditorTableFull m

        Nothing ->
            mapEditorTableEmpty


mapHeader : Html Msg
mapHeader =
    DataTable.thead []
        [ DataTable.tr []
            [ DataTable.th bold [ text "Ids" ]
            , DataTable.th bold [ text "Values" ]
            ]
        ]


mapRows : XMap -> Html Msg
mapRows m =
    let
        rows =
            List.map lineToTableRow (mapToTransposedMatrix m)
    in
    DataTable.tbody [] rows


lineToTableRow : List String -> Html Msg
lineToTableRow line =
    DataTable.tr [] (List.map (\v -> DataTable.td [] [ text v ]) line)
