module TicTacToe.Coordinate exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Coordinate =
    ( Int, Int )


getX : Coordinate -> Int
getX =
    Tuple.first


getY : Coordinate -> Int
getY =
    Tuple.second



---- ENCODE ----


encoder : Coordinate -> Encode.Value
encoder ( x, y ) =
    Encode.object
        [ ( "x", Encode.int x )
        , ( "y", Encode.int y )
        ]



---- DECODE ----


decoder : Decoder Coordinate
decoder =
    Decode.succeed Tuple.pair
        |> required "x" Decode.int
        |> required "y" Decode.int
