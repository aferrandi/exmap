module EncodeView exposing (..)

import Json.Encode exposing (..)

import EncodeXMap exposing (..)
import Views exposing (..)


encodeViewItem : ViewItem -> Value
encodeViewItem i = case i of
                    MapItem mn -> object
                          [ ("type", string "mapItem")
                          , ("mapItem", encodeXmapName mn)
                          ]
                    LabelItem l -> object
                         [ ("type", string "label")
                         , ("label", string l)
                         ]

encodeViewRow : ViewRow -> Value
encodeViewRow r =
    let items (ViewRow is) = is
    in object
         [ ("items", List.map encodeViewItem (items r) |> list)
          ]


encodeView : View -> Value
encodeView v = object
                 [ ("viewName", string v.viewName)
                 , ("rows", List.map encodeViewRow v.rows |> list)
                  ]
