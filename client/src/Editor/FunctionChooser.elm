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

functionsCategoriesTabs : Model -> Html Msg
functionsCategoriesTabs model =
    let
        categories = Dict.keys (functions model).idsByCategory
        tab category =
            TabBar.tab
                [ Options.css "width" "4px"
                , Options.onClick (Internal (SwitchCategoryTo category))
                ]
                [text category]
        categoryToTab ct = Maybe.withDefault 0 (Maybe.andThen (\c -> ListX.elemIndex c categories) ct)
    in
        TabBar.view Mdc
            (makeIndex calcEditorIdx "tbsCat")
            model.mdc [ TabBar.activeTab (categoryToTab model.currentCategory) ]
            (List.map (\c -> tab c) categories)


functionsNamesList : Model -> Html Msg
functionsNamesList model  =
    let
        operationIdsForCategoryMaybe = Maybe.andThen (\c -> Dict.get c (functions model).idsByCategory) model.currentCategory
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
            (( Lists.onSelectListItem sendAddOperation) :: (scrollableListStyle 40))
            operationList

