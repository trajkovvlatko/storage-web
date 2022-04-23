module Domain.User exposing (User, decoder, encode)

import Json.Decode as Json
import Json.Encode as Encode


type alias User =
    { token : String
    }


decoder : Json.Decoder User
decoder =
    Json.map User
        (Json.field "token" Json.string)


encode : User -> Json.Value
encode user =
    Encode.object
        [ ( "token", Encode.string user.token )
        ]
