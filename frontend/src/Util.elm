module Util exposing (flip, foldResult, hasValue)

import Html exposing (b)


hasValue : Maybe a -> Bool
hasValue maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


flip : (a -> b -> c) -> (b -> a -> c)
flip fn =
    \b a -> fn a b


foldResult : (e -> b) -> (a -> b) -> Result e a -> b
foldResult ifErr ifOk result =
    case result of
        Ok value ->
            ifOk value

        Err err ->
            ifErr err
