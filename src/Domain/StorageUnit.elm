module Domain.StorageUnit exposing (StorageUnit, StorageUnits, storageUnitDecoder, storageUnitsDecoder)

import Json.Decode exposing (Decoder, field, int, list, map2, string)


type alias StorageUnit =
    { id : Int, name : String }


type alias StorageUnits =
    List StorageUnit


storageUnitsDecoder : Decoder StorageUnits
storageUnitsDecoder =
    list storageUnitDecoder


storageUnitDecoder : Decoder StorageUnit
storageUnitDecoder =
    map2 StorageUnit
        (field "id" int)
        (field "name" string)
