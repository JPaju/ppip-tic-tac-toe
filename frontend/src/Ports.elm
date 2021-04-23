port module Ports exposing (receiveCoordinate, sendCoordinate)

import Json.Decode as Json
import Matrix exposing (Coordinate)


port sendCoordinate : Coordinate -> Cmd msg


port receiveCoordinate : (Json.Value -> msg) -> Sub msg
