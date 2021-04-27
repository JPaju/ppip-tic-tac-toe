module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Element exposing (Element, centerX, centerY, fill, height, layout, mouseOver, paddingXY, pointer, px, row, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Page.OfflineGame as OfflineGame
import Page.OnlineGame as OnlineGame
import Route exposing (Route)
import Ui
import Url exposing (Url)


type alias Model =
    { navKey : Nav.Key
    , page : Page
    }


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    changeRoute (Route.fromUrl url) { navKey = key, page = Home }


type Page
    = Home
    | Offline OfflineGame.Model
    | Online OnlineGame.Model


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | OnlineMsg OnlineGame.Msg
    | OfflineMsg OfflineGame.Msg


changeRoute : Route -> Model -> ( Model, Cmd Msg )
changeRoute route model =
    case route of
        Route.Root ->
            ( { model | page = Home }, Cmd.none )

        Route.OfflineRoute ->
            ( { model | page = Offline OfflineGame.init }, Cmd.none )

        Route.OnlineRoute ->
            let
                ( subModel, subCmd ) =
                    OnlineGame.init
            in
            ( { model | page = Online subModel }, Cmd.map OnlineMsg subCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External extUrl ->
                    ( model, Nav.load extUrl )

        ( UrlChanged url, _ ) ->
            changeRoute (Route.fromUrl url) model

        ( OfflineMsg gameMsg, Offline gameModel ) ->
            ( { model | page = Offline (OfflineGame.update gameMsg gameModel) }, Cmd.none )

        ( OnlineMsg gameMsg, Online gameModel ) ->
            let
                ( newModel, cmd ) =
                    OnlineGame.update gameMsg gameModel
            in
            ( { model | page = Online newModel }, Cmd.map OnlineMsg cmd )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Offline offlineModel ->
            pageLayout OfflineMsg (OfflineGame.view offlineModel)

        Online onlineModel ->
            pageLayout OnlineMsg (OnlineGame.view onlineModel)

        Home ->
            pageLayout never homePage


pageLayout : (a -> msg) -> Element a -> Browser.Document msg
pageLayout toMsg content =
    { title = "The title"
    , body =
        [ layout [ height fill, width fill ] <| Element.map toMsg content ]
    }


homePage : Element msg
homePage =
    row [ width fill, centerX, centerY, spacing 20 ]
        [ navLink "Play offline" "/offline"
        , navLink "Play online" "/online"
        ]


navLink : String -> String -> Element msg
navLink label href =
    Element.link
        [ centerX
        , width (px 300)
        , Font.color Ui.white
        , Font.center
        , Font.size 32
        , Background.color Ui.blue
        , mouseOver [ Background.color Ui.black ]
        , Border.rounded 10
        , paddingXY 20 50
        , pointer
        ]
        { url = href, label = Element.text label }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Online _ ->
            OnlineGame.subscriptions |> Sub.map OnlineMsg

        _ ->
            Sub.none


main : Program () Model Msg
main =
    Browser.application
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
