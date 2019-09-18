module Display.MdlIndexes exposing (..)


projectsUIIdx = "prjs"
projectUIIdx = "prj"
viewsUIIdx = "vws"
viewUIIdx = "vw"
calcEditorIdx = "clce"
mapEditorIdx = "mape"
viewEditorIdx = "vwe"

makeIndex: String -> Int -> String
makeIndex idx sub = idx ++ String.fromInt(sub)
