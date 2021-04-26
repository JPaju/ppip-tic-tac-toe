module TicTacToe.Mark exposing (Mark, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import TicTacToe.Coordinate as Coordinate exposing (Coordinate)
import TicTacToe.Sign as Sign exposing (Sign)


type alias Mark =
    { sign : Sign
    , location : Coordinate
    }


encoder : Mark -> Encode.Value
encoder { sign, location } =
    Encode.object
        [ ( "coordinate", Coordinate.encoder location )
        , ( "sign", Sign.encoder sign )
        ]


decoder : Decoder Mark
decoder =
    Decode.succeed Mark
        |> required "sign" Sign.decoder
        |> required "coordinate" Coordinate.decoder
