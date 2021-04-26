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
    , getRows
    , getRowsWithCoordinates
    , getWidth
    , nConsecutiveDiagonally
    , nConsecutiveHorizontally
    , nConsecutiveVertically
    , set
    , square
    )

import Array exposing (Array)
import Dict
import TicTacToe.Coordinate as Coordinate exposing (Coordinate)
import Util exposing (flip)


type alias Dimensions =
    { height : Int
    , width : Int
    }


{-| --TODO Implement with Dict so that not every cell has to be initialized
-}
type Matrix a
    = Matrix Dimensions (Array (Array a))



---- CREATE ----


create : Dimensions -> (Coordinate -> a) -> Matrix a
create ({ height, width } as dimensions) toElement =
    Array.initialize height (createRow width toElement)
        |> Matrix dimensions


square : Int -> (Coordinate -> a) -> Matrix a
square size =
    create { height = size, width = size }


fromList : Dimensions -> List ( Coordinate, a ) -> Matrix (Maybe a)
fromList dimensions values =
    let
        byCoordinates =
            Dict.fromList values
    in
    create dimensions (flip Dict.get byCoordinates)



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


getRows : Matrix a -> List (List a)
getRows (Matrix _ matrix) =
    matrix
        |> Array.toList
        |> List.map Array.toList



---- ACCESS ----


get : Coordinate -> Matrix a -> Maybe a
get ( x, y ) (Matrix _ matrix) =
    matrix
        |> Array.get y
        |> Maybe.andThen (Array.get x)


getAllElements : Matrix a -> List a
getAllElements =
    getRows >> List.concat


getRowsWithCoordinates : Matrix a -> List (List ( a, Coordinate ))
getRowsWithCoordinates (Matrix _ matrix) =
    matrix
        |> Array.toList
        |> List.indexedMap (\y row -> row |> Array.indexedMap (\x a -> ( a, ( x, y ) )) |> Array.toList)


filter : (a -> Bool) -> Matrix a -> List ( a, Coordinate )
filter pred matrix =
    getRowsWithCoordinates matrix
        |> List.concat
        |> List.filter (Tuple.first >> pred)



---- MODIFY ----


set : Coordinate -> a -> Matrix a -> Matrix a
set ( coordX, coordY ) value (Matrix dimensions matrix) =
    -- Fucking disgusting
    matrix
        |> Array.indexedMap
            (\y row ->
                if y == coordY then
                    row
                        |> Array.indexedMap
                            (\x a ->
                                if x == coordX then
                                    value

                                else
                                    a
                            )

                else
                    row
            )
        |> Matrix dimensions



---- CALCULATE ----


nConsecutiveHorizontally : Int -> (a -> Bool) -> Matrix a -> Bool
nConsecutiveHorizontally n isValidElement matrix =
    matrix
        |> filter isValidElement
        |> List.map Tuple.second
        |> Util.groupBy Coordinate.getY
        |> Dict.map (always (List.map Coordinate.getX >> List.sort))
        |> Dict.values
        |> List.any (hasNConsecutiveNumbers n)


nConsecutiveVertically : Int -> (a -> Bool) -> Matrix a -> Bool
nConsecutiveVertically n isValidElement matrix =
    matrix
        |> filter isValidElement
        |> List.map Tuple.second
        |> Util.groupBy Coordinate.getX
        |> Dict.map (always (List.map Coordinate.getY >> List.sort))
        |> Dict.values
        |> List.any (hasNConsecutiveNumbers n)


{-| --TODO Implement
-}
nConsecutiveDiagonally : Int -> (a -> Bool) -> Matrix a -> Bool
nConsecutiveDiagonally n pred matrix =
    False



---- PRIVATE HELPERS ----


createRow : Int -> (Coordinate -> a) -> Int -> Array a
createRow width toElement y =
    Array.initialize width (\x -> toElement ( x, y ))


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
