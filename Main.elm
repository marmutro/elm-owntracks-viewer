port module Owntracks exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (..)
import Json.Decode exposing (int, bool, string, float, decodeValue, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


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
    , info : Location
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
    , tid : String
    , conn : String
    , doze : Bool
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
    , tst : Int
    , vac : Int
    , vel : Int
    , p : Int
    }


emptyLocation : Location
emptyLocation =
    Location "" "" "" False 0 0 0 0 "" "" 0.0 0.0 0 "" 0 0 0 0


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


port locationUpdate : (Json.Decode.Value -> msg) -> Sub msg


infoDecoder : Decoder LocationInfo
infoDecoder =
    decode LocationInfo
        |> required "topic" string
        |> required "info" locationDecoder


locationDecoder : Decoder Location
locationDecoder =
    decode Location
        |> required "_type" string
        |> required "tid" string
        |> optional "conn" string ""
        |> required "doze" bool
        |> required "acc" int
        |> optional "alt" int 0
        |> required "batt" int
        |> optional "cog" int 0
        |> optional "desc" string ""
        |> optional "event" string ""
        |> required "lat" float
        |> required "lon" float
        |> optional "rad" int 0
        |> optional "trig" string ""
        |> required "tst" int
        |> optional "vac" int 0
        |> optional "vel" int 0
        |> optional "p" int 0


fromJson : Json.Decode.Value -> LocationInfo
fromJson json =
    let
        result =
            decodeValue infoDecoder json
    in
        case result of
            Ok v ->
                v

            Err e ->
                LocationInfo e emptyLocation


subscriptions : Model -> Sub Msg
subscriptions model =
    locationUpdate (\v -> LocationUpdate (fromJson v))



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder model.url, onInput UrlChange ] []
        , button [ onClick Connect ] [ text "Connect" ]
        , div [] (List.map (\info -> div [] [ text (info.topic ++ toString (info.info)) ]) model.locations)
        ]
