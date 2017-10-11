module DialogStarter exposing (..)

import Html exposing (Html,text, div)
import Material.Button as Button exposing (render)
import Material.Options as Options exposing (css)
import Material.Dialog as Dialog exposing (openOn)


import ProjectModel exposing (..)
import MapEditor exposing (..)


startDialog : Model -> ProjectModel -> Html Msg
startDialog model pm =
                Dialog.view
                    [ Options.css "width" "50%", Options.css "height" "50%" ]
                    [ Dialog.title [] [ text "" ]
                    , Dialog.content [] (mapEditorContent model pm)
                    , Dialog.actions []
                        [ Button.render Mdl
                            [ 0 ]
                            model.mdl
                            [ Dialog.closeOn "click" ]
                            [ text "Close" ]
                        ]
                    ]
