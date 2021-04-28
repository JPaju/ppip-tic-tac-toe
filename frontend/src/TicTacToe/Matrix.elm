module TicTacToe.Matrix exposing
    ( Dimensions
    , Matrix
    , create
    , filter
    , fromList
    , get
    , getCapacity
    , getDimensions
    , getElements
    , getHeight
    , getRowsWithCoordinates
    , getWidth
    , hasNDiagonally
    , hasNHorizontally
    , hasNVertically
    , set
    , square
    )

import Dict exposing (Dict)
import TicTacToe.Coordinate as Coordinate exposing (Coordinate)
import Util


type alias Dimensions =
    { height : Int
    , width : Int
    }


type Matrix a
    = Matrix Dimensions (Dict Coordinate a)



---- CREATE ----


create : Dimensions -> Matrix a
create dimensions =
    Matrix dimensions Dict.empty


square : Int -> Matrix a
square size =
    create { height = size, width = size }


fromList : Dimensions -> List ( Coordinate, a ) -> Matrix a
fromList dimensions values =
    values
        |> List.filter (Tuple.first >> isIncluded dimensions)
        |> Dict.fromList
        |> Matrix dimensions



---- DIMENSIONS ----


getDimensions : Matrix a -> Dimensions
getDimensions (Matrix dimensions _) =
    dimensions


getHeight : Matrix a -> Int
getHeight =
    getDimensions >> .height


getWidth : Matrix a -> Int
getWidth =
    getDimensions >> .width


getCapacity : Matrix a -> Int
getCapacity matrix =
    getHeight matrix * getWidth matrix


isIncluded : Dimensions -> Coordinate -> Bool
isIncluded { width, height } ( x, y ) =
    (x < width) && (y < height)



---- ACCESS ----


get : Coordinate -> Matrix a -> Maybe a
get coordinate matrix =
    matrix
        |> getDict
        |> Dict.get coordinate


getElements : Matrix a -> List a
getElements matrix =
    matrix
        |> getDict
        |> Dict.values


getRowsWithCoordinates : Matrix a -> List (List ( Coordinate, Maybe a ))
getRowsWithCoordinates (Matrix dimensions dict) =
    dimensions
        |> getAllCoordinates
        |> Util.groupBy Coordinate.getY
        |> Dict.values
        |> List.map (List.sortBy Coordinate.getX)
        |> List.map (List.map (\coordinate -> ( coordinate, Dict.get coordinate dict )))


filter : (a -> Bool) -> Matrix a -> Matrix a
filter pred (Matrix dimensions dict) =
    dict
        |> Dict.filter (always pred)
        |> Matrix dimensions



---- MODIFY ----


set : Coordinate -> a -> Matrix a -> Matrix a
set coordinate value (Matrix dimensions dict) =
    if isIncluded dimensions coordinate then
        dict
            |> Dict.insert coordinate value
            |> Matrix dimensions

    else
        Matrix dimensions dict



---- CALCULATE ----


hasNHorizontally : Int -> Matrix a -> Bool
hasNHorizontally count =
    hasNInRow count (\( x, y ) -> ( x, y + 1 ))


hasNVertically : Int -> Matrix a -> Bool
hasNVertically count =
    hasNInRow count (\( x, y ) -> ( x + 1, y ))


hasNDiagonally : Int -> Matrix a -> Bool
hasNDiagonally count matrix =
    hasNInRow count (\( x, y ) -> ( x + 1, y + 1 )) matrix
        || hasNInRow count (\( x, y ) -> ( x + 1, y - 1 )) matrix



---- PRIVATE HELPERS ----


getPopulatedCoordinates : Matrix a -> List Coordinate
getPopulatedCoordinates matrix =
    matrix
        |> getDict
        |> Dict.keys


getAllCoordinates : Dimensions -> List Coordinate
getAllCoordinates { height, width } =
    List.range 0 ((height * width) - 1)
        |> List.map (\i -> ( modBy width i, i // width ))


hasNInRow : Int -> (Coordinate -> Coordinate) -> Matrix a -> Bool
hasNInRow n toNextCoordinate matrix =
    let
        hasInRowHelper : Int -> Coordinate -> Bool
        hasInRowHelper count coordinate =
            count
                == n
                || (let
                        nextCoordinate =
                            toNextCoordinate coordinate

                        nextHasValue =
                            matrix
                                |> getDict
                                |> Dict.member nextCoordinate
                    in
                    nextHasValue && hasInRowHelper (count + 1) nextCoordinate
                   )

        loop : List Coordinate -> Bool
        loop coordinatesToCheck =
            case coordinatesToCheck of
                currentCoordinate :: restCoordinates ->
                    hasInRowHelper 1 currentCoordinate || loop restCoordinates

                [] ->
                    False
    in
    getPopulatedCoordinates matrix
        |> loop


getDict : Matrix a -> Dict Coordinate a
getDict (Matrix _ dict) =
    dict
