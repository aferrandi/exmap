module MapEditor exposing (..)

import Html        exposing (..)
import Material
import Material.Button as Button exposing (render)
import Material.Dialog as Dialog exposing (openOn)
import Material.Textfield as Textfield

import ProjectModel exposing (..)

viewModel : Model -> Html Msg
viewModel model =
                Dialog.view
                    []
                    [ Dialog.title [] [ text "Map Editor" ]
                    , Dialog.content []
                        [
                            Textfield.render Mdl [9] model.mdl
                          [ Textfield.label "Enter the map data"
                          , Textfield.floatingLabel
                          , Textfield.textarea
                          , Textfield.rows 20
                          ]
                          []
                          ]
                    , Dialog.actions []
                        [ Button.render Mdl
                            [ 0 ]
                            model.mdl
                            [ Dialog.closeOn "click" ]
                            [ text "Close" ]
                        , Button.render Mdl
                            [ 1 ]
                            model.mdl
                            [ Button.disabled ]
                            [ text "GTNW" ]
                        ]
                    ]

