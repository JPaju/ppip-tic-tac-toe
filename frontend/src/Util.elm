module Util exposing (flip, foldResult, groupBy, hasValue)

import Dict exposing (Dict)



---- FUNCTION ----


flip : (a -> b -> c) -> (b -> a -> c)
flip fn =
    \b a -> fn a b



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
