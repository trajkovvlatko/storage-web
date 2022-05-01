module Domain.Item exposing (Item, Items, itemDecoder, itemsDecoder)

import Json.Decode exposing (Decoder, field, int, list, map4, map5, string)


type alias Item =
    { id : Int, drawer_id : Int, name : String, color_id : Int, item_type_id : Int }


type alias Items =
    List Item


itemsDecoder : Decoder Items
itemsDecoder =
    list itemDecoder


itemDecoder : Decoder Item
itemDecoder =
    map5 Item
        (field "id" int)
        (field "drawer_id" int)
        (field "name" string)
        (field "color_id" int)
        (field "item_type_id" int)
