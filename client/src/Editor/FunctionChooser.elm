module Editor.FunctionChooser exposing (viewFunctions)

import Material.TabBar as TabBar
import Types.Calculation exposing (..)
import Models.EmptyModel exposing (emptyFunctionModel)
import Html exposing (Html, div, text)
import Models.InternalMessages exposing (..)
import Material.List as Lists
import Material.Options as Options
import Display.MdcIndexes exposing (..)
import Models.ProjectModel exposing (..)
import Display.UIWrapper exposing (..)
import Dict as Dict
import List.Extra as ListX

viewFunctions : Model -> Html Msg
viewFunctions model  =
    div []
        [ functionsCategoriesTabs model
        , functionsNamesList model
        ]

functions : Model -> FunctionsModel
functions model = Maybe.withDefault emptyFunctionModel model.functions

categories : Model -> List OperationCategory
categories model = Dict.keys (functions model).idsByCategory

functionsCategoriesTabs : Model -> Html Msg
functionsCategoriesTabs model =
    let
        tab category =
            TabBar.tab
                [ Options.css "width" "2px"
                , Options.onClick (Internal (SwitchCategoryTo category))
                ]
                [text category]
        indexOfCategory ct = ListX.elemIndex ct (categories model)
        categoryToTab ct = Maybe.withDefault 0 (Maybe.andThen indexOfCategory ct)
    in
        TabBar.view Mdc
            (makeIndex calcEditorIdx "tbsCat")
            model.mdc [ TabBar.activeTab (categoryToTab model.currentCategory) ]
            (List.map tab (categories model))


functionsNamesList : Model -> Html Msg
functionsNamesList model  =
    let
        currentOrFirstCategory = case model.currentCategory of
                Nothing -> List.head (categories model)
                _ ->  model.currentCategory
        operationIdsForCategoryMaybe = Maybe.andThen (\c -> Dict.get c (functions model).idsByCategory) currentOrFirstCategory
        operationIdsForCategory = Maybe.withDefault [] operationIdsForCategoryMaybe
        sendAddOperation index = sendListMsg (\on -> (Internal (AddOperationToCalculation on))) operationIdsForCategory index
        operationListItem on =
            Lists.li []
                [
                  Lists.graphicIcon  [] "play_arrow",
                  text on.name
                ]
        operationList = List.map operationListItem operationIdsForCategory
    in
        Lists.ul Mdc
            (makeIndex calcEditorIdx "lstFnc")
            model.mdc
            (( Lists.onSelectListItem sendAddOperation) :: (scrollableListStyle 32))
            operationList

