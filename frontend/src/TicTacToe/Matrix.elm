module TicTacToe.Matrix exposing
    ( Dimensions
    , Matrix
    , create
    , filter
    , fromList
    , get
    , getAllElements
    , getCapacity
    , getDimensions
    , getHeight
    , getRowsWithCoordinates
    , getWidth
    , nConsecutiveDiagonally
    , nConsecutiveHorizontally
    , nConsecutiveVertically
    , set
    , square
    )

import Dict exposing (Dict)
import Html.Attributes exposing (height, value, width)
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
get coordinate (Matrix _ dict) =
    Dict.get coordinate dict


getAllElements : Matrix a -> List a
getAllElements (Matrix _ dict) =
    Dict.values dict


getAllCoordinates : Dimensions -> List Coordinate
getAllCoordinates { height, width } =
    List.range 0 ((height * width) - 1)
        |> List.map (\i -> ( modBy width i, i // width ))


getRowsWithCoordinates : Matrix a -> List (List ( Coordinate, Maybe a ))
getRowsWithCoordinates (Matrix dimensions dict) =
    dimensions
        |> getAllCoordinates
        |> Util.groupBy Coordinate.getY
        |> Dict.values
        |> List.map (List.sortBy Coordinate.getX)
        |> List.map (List.map (\coordinate -> ( coordinate, Dict.get coordinate dict )))


filter : (a -> Bool) -> Matrix a -> List ( Coordinate, a )
filter pred (Matrix _ dict) =
    dict
        |> Dict.toList
        |> List.filter (Tuple.second >> pred)



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


nConsecutiveHorizontally : Int -> (a -> Bool) -> Matrix a -> Bool
nConsecutiveHorizontally n isValidElement matrix =
    matrix
        |> filter isValidElement
        |> List.map Tuple.first
        |> Util.groupBy Coordinate.getY
        |> Dict.map (always (List.map Coordinate.getX >> List.sort))
        |> Dict.values
        |> List.any (hasNConsecutiveNumbers n)


nConsecutiveVertically : Int -> (a -> Bool) -> Matrix a -> Bool
nConsecutiveVertically n isValidElement matrix =
    matrix
        |> filter isValidElement
        |> List.map Tuple.first
        |> Util.groupBy Coordinate.getX
        |> Dict.map (always (List.map Coordinate.getY >> List.sort))
        |> Dict.values
        |> List.any (hasNConsecutiveNumbers n)


{-| --TODO Implement
-}
nConsecutiveDiagonally : Int -> (a -> Bool) -> Matrix a -> Bool
nConsecutiveDiagonally _ _ _ =
    False



---- PRIVATE HELPERS ----


hasNConsecutiveNumbers : Int -> List Int -> Bool
hasNConsecutiveNumbers n list =
    let
        ( _, highestCount ) =
            list
                |> List.foldl
                    (\currX ( prevX, count ) ->
                        if count >= n then
                            ( 0, count )

                        else if prevX + 1 == currX then
                            ( currX, count + 1 )

                        else
                            ( currX, 1 )
                    )
                    ( 0, 0 )
    in
    highestCount >= n
