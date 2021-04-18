module Main exposing (..)

import Browser
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, image, mouseOver, none, padding, pointer, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Ui
import Util exposing (hasValue)



---- GAME ----


type Mark
    = X
    | O


type alias Coordinate =
    ( Int, Int )


type alias Move =
    ( Coordinate, Mark )


type alias Board =
    List Move


type Result
    = Tie
    | Won Mark


type Game
    = Finished Board Result
    | OnGoing
        { board : Board
        , turn : Mark
        }


initGame : Game
initGame =
    OnGoing { board = [], turn = X }



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
    = MoveMade Move
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveMade _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


header : List (Attribute msg) -> Element msg
header attributes =
    column (spacing 20 :: padding 20 :: attributes)
        [ image [ height (px 200), centerX ] { src = "/logo.svg", description = "Elm logo" }
        , el [ Font.size 30, Font.bold ] (text "Your Elm App is working!")
        ]


view : Model -> Html Msg
view model =
    Element.layout [ height fill, width fill ] <|
        column [ height fill, width fill ]
            [ header [ centerX ]
            , viewBoard [ centerY, centerX ] model.game
            ]


viewBoard : List (Attribute msg) -> Game -> Element msg
viewBoard attributes _ =
    -- column (spacing 10 :: explain Debug.todo :: attributes)
    column attributes
        [ row [] [ square (Just X), square (Just O), square Nothing ]
        , row [] [ square Nothing, square (Just O), square Nothing ]
        , row [] [ square Nothing, square Nothing, square (Just X) ]
        ]


square : Maybe Mark -> Element msg
square mark =
    let
        size =
            [ height (px 100), width (px 100) ]

        icon =
            mark
                |> Maybe.map viewMark
                |> Maybe.withDefault none

        hasMark =
            hasValue mark

        pointerIcon =
            if hasMark then
                Ui.notAllowed

            else
                pointer

        hoverEffects =
            mouseOver <|
                if hasMark then
                    []

                else
                    [ Background.color Ui.grey ]

        borders =
            [ Border.color Ui.black, Border.solid, Border.width 1 ]
    in
    el (size ++ borders ++ [ pointerIcon, hoverEffects ]) icon


viewMark : Mark -> Element msg
viewMark mark =
    let
        icon =
            -- TODO Better icons?
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
