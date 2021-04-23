port module Ports exposing (receiveCoordinate, sendCoordinate)

import Json.Decode as Json
import Matrix exposing (Coordinate)



-- TODO Send mark and not coordinate


port sendCoordinate : Coordinate -> Cmd msg


port receiveCoordinate : (Json.Value -> msg) -> Sub msg
