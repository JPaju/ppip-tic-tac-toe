module Matrix exposing (Coordinate, Matrix, get, getRows, getRowsWithCoordinates, set, square)

import Array exposing (Array)


type alias Coordinate =
    ( Int, Int )


type Matrix a
    = Matrix (Array (Array a))



---- CREATE ----


square : Int -> (Coordinate -> a) -> Matrix a
square size toElement =
    Array.initialize size (createRow size toElement)
        |> Matrix



---- ACCESS ----


get : Coordinate -> Matrix a -> Maybe a
get ( x, y ) (Matrix matrix) =
    matrix
        |> Array.get y
        |> Maybe.andThen (Array.get x)


getRows : Matrix a -> List (List a)
getRows (Matrix matrix) =
    matrix
        |> Array.toList
        |> List.map Array.toList


getRowsWithCoordinates : Matrix a -> List (List ( a, Coordinate ))
getRowsWithCoordinates (Matrix matrix) =
    matrix
        |> Array.toList
        |> List.indexedMap (\y row -> row |> Array.indexedMap (\x a -> ( a, ( x, y ) )) |> Array.toList)



---- MODIFY ----


set : Coordinate -> a -> Matrix a -> Matrix a
set ( coordX, coordY ) value (Matrix matrix) =
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
        |> Matrix



---- PRIVATE HELPERS ----


createRow : Int -> (Coordinate -> a) -> Int -> Array a
createRow width toElement y =
    Array.initialize width (\x -> toElement ( x, y ))
