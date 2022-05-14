module Domain.Item exposing (Item, Items, itemDecoder, itemsDecoder)

import Json.Decode exposing (Decoder, field, int, list, map7, string)


type alias Item =
    { id : Int
    , drawer_id : Int
    , name : String
    , color_id : Int
    , item_type_id : Int
    , color_label : String
    , item_type_label : String
    }


type alias Items =
    List Item


itemsDecoder : Decoder Items
itemsDecoder =
    list itemDecoder


itemDecoder : Decoder Item
itemDecoder =
    map7 Item
        (field "id" int)
        (field "drawer_id" int)
        (field "name" string)
        (field "color_id" int)
        (field "item_type_id" int)
        (field "color_label" string)
        (field "item_type_label" string)
