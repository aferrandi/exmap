module Display.MdcIndexes exposing (..)

projectsUIIdx = "prjs"
projectUIIdx = "prj"
viewsUIIdx = "vws"
viewUIIdx = "vw"
calcEditorIdx = "clce"
mapEditorIdx = "mape"
viewEditorIdx = "vwe"

makeIndex: String -> String -> String
makeIndex viewIdx compIdx = viewIdx ++ "." ++ compIdx
