module Pages.Top exposing (Model, Msg, Params, page)

import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)


type Effect
    = EffCmd (Cmd Msg)
    | EffBatch (List Effect)


runEffect : Effect -> Cmd Msg
runEffect effect =
    case effect of
        EffCmd cmd ->
            cmd

        EffBatch effects ->
            Cmd.batch <| List.map runEffect effects


page : Page Params Model Msg
page =
    Page.application
        { init = \shared url -> init shared url |> Tuple.mapSecond runEffect
        , update = \msg model -> update msg model |> Tuple.mapSecond runEffect
        , subscriptions = subscriptions
        , view = view
        , save = save
        , load = \shared model -> load shared model |> Tuple.mapSecond runEffect
        }



-- INIT


type alias Params =
    ()


type alias Model =
    {}


init : Shared.Model -> Url Params -> ( Model, Effect )
init shared { params } =
    ( {}, EffCmd Cmd.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, EffCmd Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Effect )
load shared model =
    ( model, EffCmd Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Top"
    , body = []
    }
