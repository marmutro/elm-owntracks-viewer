port module Owntracks exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (..)
import Json.Decode exposing (int, bool, string, float, decodeValue, fail, field, andThen, Decoder)
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
    , locations : List OwntracksInfo
    }


type alias OwntracksInfo =
    { topic : String
    , payload : Payload
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags.url [], Cmd.none )



-- UPDATE


type Msg
    = UrlChange String
    | Connect
    | OwntracksUpdate OwntracksInfo


type Payload
    = PayloadLocation Location
    | PayloadLastWill LastWill
    | PayloadWaypoint Waypoint
    | PayloadNone


type alias Location =
    { tid : String
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


type alias LastWill =
    { tst : Int }


type alias Waypoint =
    {}


emptyPayload : Payload
emptyPayload =
    PayloadNone


port connect : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange newUrl ->
            ( Model newUrl [], Cmd.none )

        Connect ->
            ( model, connect model.url )

        OwntracksUpdate newLocation ->
            ( Model model.url (model.locations ++ [ newLocation ]), Cmd.none )



-- SUBSCRIPTIONS


port owntracksUpdate : (Json.Decode.Value -> msg) -> Sub msg


owntracksDecoder : Decoder OwntracksInfo
owntracksDecoder =
    decode OwntracksInfo
        |> required "topic" string
        |> required "payload" payloadDecoder


payloadDecoder : Decoder Payload
payloadDecoder =
    field "_type" string
        |> andThen payloadTypeDecoder


payloadTypeDecoder : String -> Decoder Payload
payloadTypeDecoder payloadType =
    case payloadType of
        "location" ->
            Json.Decode.map PayloadLocation locationDecoder

        "lwt" ->
            Json.Decode.map PayloadLastWill lastWillDecoder

        _ ->
            fail
                <| "Trying to decode payload, but _type "
                ++ toString payloadType
                ++ " is not supported."


lastWillDecoder : Decoder LastWill
lastWillDecoder =
    decode LastWill
        |> required "tst" int


locationDecoder : Decoder Location
locationDecoder =
    decode Location
        |> required "tid" string
        |> optional "conn" string ""
        |> required "doze" bool
        |> optional "acc" int 0
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


fromJson : Json.Decode.Value -> OwntracksInfo
fromJson json =
    let
        result =
            decodeValue owntracksDecoder json
    in
        case result of
            Ok v ->
                v

            Err e ->
                OwntracksInfo e emptyPayload


subscriptions : Model -> Sub Msg
subscriptions model =
    owntracksUpdate (\v -> OwntracksUpdate (fromJson v))



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder model.url, onInput UrlChange ] []
        , button [ onClick Connect ] [ text "Connect" ]
        , div [] (List.map (\info -> div [] [ text info.topic, viewPayload info.payload ]) model.locations)
        ]


viewPayload : Payload -> Html Msg
viewPayload payload =
    case payload of
        PayloadLocation loc ->
            div [] [ text ("Lat=" ++ (toString loc.lat) ++ ", Lon=" ++ (toString loc.lon)) ]

        _ ->
            div [] []
