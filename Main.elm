port module Owntracks exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { url : String
    }



-- MODEL


type alias Model =
    { url : String
    , locations : List LocationInfo
    }


type alias LocationInfo =
    { topic : String
    , info : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags.url [], Cmd.none )



-- UPDATE


type Msg
    = UrlChange String
    | Connect
    | LocationUpdate LocationInfo


type alias Location =
    { ltype : String
    , acc : Int
    , alt : Int
    , batt : Int
    , cog : Int
    , desc : String
    , event : String
    , lat : Float
    , lon : Float
    , rad : Int
    , trig : String
    , tid : String
    , tst : Int
    , vac : Int
    , vel : Int
    , p : Int
    , conn : String
    }


port connect : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange newUrl ->
            ( Model newUrl [], Cmd.none )

        Connect ->
            ( model, connect model.url )

        LocationUpdate newLocation ->
            ( Model model.url (model.locations ++ [ newLocation ]), Cmd.none )



-- SUBSCRIPTIONS


port locationUpdate : (LocationInfo -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    locationUpdate LocationUpdate



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder model.url, onInput UrlChange ] []
        , button [ onClick Connect ] [ text "Connect" ]
        , div [] (List.map (\info -> div [] [ text (info.topic ++ info.info) ]) model.locations)
        ]
