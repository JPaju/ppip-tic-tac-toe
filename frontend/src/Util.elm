module Util exposing (average, flip, foldResult, groupBy, hasValue, indexOf)

import Dict exposing (Dict)



---- FUNCTION ----


flip : (a -> b -> c) -> (b -> a -> c)
flip fn =
    \b a -> fn a b



---- LIST ----


indexOf : a -> List a -> Maybe Int
indexOf value list =
    let
        loop : Int -> List a -> Maybe Int
        loop index acc =
            case acc of
                head :: tail ->
                    if head == value then
                        Just index

                    else
                        loop (index + 1) tail

                [] ->
                    Nothing
    in
    loop 0 list


average : List Int -> Float
average list =
    toFloat (List.sum list) / toFloat (List.length list)



---- DICT ----


groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy toKey list =
    List.foldl
        (\a dict ->
            Dict.update (toKey a) (Maybe.map ((::) a) >> Maybe.withDefault [ a ] >> Just) dict
        )
        Dict.empty
        list



---- MAYBE ----


hasValue : Maybe a -> Bool
hasValue maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False



---- RESULT ----


foldResult : (e -> b) -> (a -> b) -> Result e a -> b
foldResult ifErr ifOk result =
    case result of
        Ok value ->
            ifOk value

        Err err ->
            ifErr err
