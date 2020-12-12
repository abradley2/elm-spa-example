module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Shared exposing (AppKey(..), Flags)
import Spa.Document as Document exposing (Document)
import Spa.Generated.Pages as Pages
import Spa.Generated.Route as Route exposing (Route)
import Url exposing (Url)


type Effect
    = EffCmd (Cmd Msg)
    | EffBatch (List Effect)
    | EffPushUrl AppKey Url
    | EffLoadUrl String


runEffect : Effect -> Cmd Msg
runEffect effect =
    case effect of
        EffCmd cmd ->
            cmd

        EffBatch effectList ->
            Cmd.batch <| List.map runEffect effectList

        EffPushUrl (AppKey key) url ->
            Nav.pushUrl key (Url.toString url)

        EffPushUrl TestKey _ ->
            Cmd.none

        EffLoadUrl href ->
            Nav.load href


main : Program Flags Model Msg
main =
    Browser.application
        { init = \flags url key -> init flags url (AppKey key)
        , update = \msg model -> update msg model |> Tuple.mapSecond runEffect
        , subscriptions = subscriptions
        , view = view >> Document.toBrowserDocument
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- INIT


type alias Model =
    { shared : Shared.Model
    , page : Pages.Model
    }


init : Flags -> Url -> AppKey -> ( Model, Cmd Msg )
init flags url key =
    let
        ( shared, sharedCmd ) =
            Shared.init flags url key

        ( page, pageCmd ) =
            Pages.init (fromUrl url) shared

        savedShare =
            Pages.save page shared
    in
    ( Model savedShare page
    , Cmd.batch
        [ Cmd.map Shared sharedCmd
        , Cmd.map Pages pageCmd
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Shared Shared.Msg
    | Pages Pages.Msg


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model
            , EffPushUrl model.shared.key url
            )

        LinkClicked (Browser.External href) ->
            ( model
            , EffLoadUrl href
            )

        UrlChanged url ->
            let
                original =
                    model.shared

                shared =
                    { original | url = url }

                ( page, pageCmd ) =
                    Pages.init (fromUrl url) shared
            in
            ( { model | page = page, shared = Pages.save page shared }
            , EffCmd <| Cmd.map Pages pageCmd
            )

        Shared sharedMsg ->
            let
                ( shared, sharedCmd ) =
                    Shared.update sharedMsg model.shared

                ( page, pageCmd ) =
                    Pages.load model.page shared
            in
            ( { model | page = page, shared = shared }
            , EffCmd <|
                Cmd.batch
                    [ Cmd.map Shared sharedCmd
                    , Cmd.map Pages pageCmd
                    ]
            )

        Pages pageMsg ->
            let
                ( page, pageCmd ) =
                    Pages.update pageMsg model.page

                shared =
                    Pages.save page model.shared
            in
            ( { model | page = page, shared = shared }
            , EffCmd <| Cmd.map Pages pageCmd
            )


view : Model -> Document Msg
view model =
    Shared.view
        { page =
            Pages.view model.page
                |> Document.map Pages
        , toMsg = Shared
        }
        model.shared


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Shared.subscriptions model.shared
            |> Sub.map Shared
        , Pages.subscriptions model.page
            |> Sub.map Pages
        ]



-- URL


fromUrl : Url -> Route
fromUrl =
    Route.fromUrl >> Maybe.withDefault Route.NotFound
