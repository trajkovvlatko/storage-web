module Domain.Drawer exposing (Drawer, Drawers, drawerDecoder, drawersDecoder)

import Json.Decode exposing (Decoder, field, int, list, map4, string)


type alias Drawer =
    { id : Int, storage_unit_id : Int, note : String, level : Int }


type alias Drawers =
    List Drawer


drawersDecoder : Decoder Drawers
drawersDecoder =
    list drawerDecoder


drawerDecoder : Decoder Drawer
drawerDecoder =
    map4 Drawer
        (field "id" int)
        (field "storage_unit_id" int)
        (field "note" string)
        (field "level" int)
