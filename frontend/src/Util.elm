module Util exposing (filterMaybe, flip, foldResult, groupBy, hasValue, traverseFirst)

import Dict exposing (Dict)
import Html exposing (a, b)



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


filterMaybe : (a -> Bool) -> Maybe a -> Maybe a
filterMaybe pred =
    Maybe.andThen
        (\a ->
            if pred a then
                Just a

            else
                Nothing
        )


traverseFirst : ( Maybe a, b ) -> Maybe ( a, b )
traverseFirst ( ma, b ) =
    Maybe.map (\a -> ( a, b )) ma



---- RESULT ----


foldResult : (e -> b) -> (a -> b) -> Result e a -> b
foldResult ifErr ifOk result =
    case result of
        Ok value ->
            ifOk value

        Err err ->
            ifErr err
