module Domain.StorageUnit exposing (StorageUnit, StorageUnits, storageUnitDecoder, storageUnitsDecoder)

import Json.Decode exposing (Decoder, field, int, list, map3, string)


type alias StorageUnit =
    { id : Int, room_id : Int, name : String }


type alias StorageUnits =
    List StorageUnit


storageUnitsDecoder : Decoder StorageUnits
storageUnitsDecoder =
    list storageUnitDecoder


storageUnitDecoder : Decoder StorageUnit
storageUnitDecoder =
    map3 StorageUnit
        (field "id" int)
        (field "room_id" int)
        (field "name" string)
