module Domain.Room exposing (Room, Rooms, roomDecoder, roomsDecoder)

import Json.Decode exposing (Decoder, field, int, list, map2, string)


type alias Room =
    { id : Int, name : String }


type alias Rooms =
    List Room


roomsDecoder : Decoder Rooms
roomsDecoder =
    list roomDecoder


roomDecoder : Decoder Room
roomDecoder =
    map2 Room
        (field "id" int)
        (field "name" string)
