module Main exposing (..)

import Browser
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, image, mouseOver, none, padding, pointer, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events exposing (onClick)
import Matrix exposing (Coordinate, Matrix)
import Ui
import Util exposing (hasValue)



---- GAME ----


type Mark
    = X
    | O


type alias Board =
    Matrix (Maybe Mark)



-- type alias Move =
--     ( Matrix.Coordinate, Mark )


type GameResult
    = Tie
    | Won Mark


type Game
    = Finished Board GameResult
    | OnGoing
        { board : Board
        , turn : Mark
        }


initBoard : Int -> Board
initBoard size =
    Matrix.square size (always Nothing)


initGame : Game
initGame =
    OnGoing { board = initBoard 8, turn = X }


getBoard : Game -> Board
getBoard game =
    case game of
        OnGoing { board } ->
            board

        Finished board _ ->
            board



---- MODEL ----


type alias Model =
    { game : Game }


init : ( Model, Cmd Msg )
init =
    ( { game = initGame }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = MarkMade Matrix.Coordinate
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MarkMade coordinate ->
            ( { model
                | game =
                    case model.game of
                        OnGoing { board, turn } ->
                            OnGoing
                                { turn = changeTurn turn
                                , board = Matrix.set coordinate (Just turn) board
                                }

                        (Finished _ _) as game ->
                            game
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


changeTurn : Mark -> Mark
changeTurn mark =
    case mark of
        X ->
            O

        O ->
            X



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [ height fill, width fill ] <|
        column [ height fill, width fill ]
            [ viewBoard [ centerY, centerX ] (getBoard model.game) ]


viewBoard : List (Attribute Msg) -> Board -> Element Msg
viewBoard attributes board =
    column attributes
        (board
            |> Matrix.getRowsWithCoordinates
            |> List.map viewRow
        )


viewRow : List ( Maybe Mark, Coordinate ) -> Element Msg
viewRow columns =
    columns
        |> List.map square
        |> row []


{-| TODO Refactor this
-}
square : ( Maybe Mark, Coordinate ) -> Element Msg
square ( maybeMark, coord ) =
    let
        size =
            [ height (px 100), width (px 100) ]

        marked =
            hasValue maybeMark

        icon =
            maybeMark
                |> Maybe.map viewMark
                |> Maybe.withDefault none

        mouseIcon =
            if marked then
                Ui.notAllowed

            else
                pointer

        hoverEffects =
            mouseOver <|
                if marked then
                    []

                else
                    [ Background.color Ui.grey ]

        borders =
            [ Border.color Ui.black, Border.solid, Border.width 1 ]

        onClick =
            if not marked then
                Just (MarkMade coord)

            else
                Nothing
    in
    Input.button (size ++ borders ++ [ mouseIcon, hoverEffects ]) { onPress = onClick, label = icon }



-- el (size ++ borders ++ [ mouseIcon, hoverEffects ]) icon


viewMark : Mark -> Element msg
viewMark mark =
    let
        icon =
            case mark of
                X ->
                    '❌'

                O ->
                    '⭕'
    in
    icon
        |> String.fromChar
        |> text
        |> el [ centerY, centerX, Font.size 36 ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
