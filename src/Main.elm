module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, pre, text, textarea)
import Html.Events exposing (onInput)
import Json.Decode exposing (decodeString)
import Json.Decode.Generic exposing (Json(..), json)
import Json.Encode
import Set exposing (Set)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type Model
    = Empty
    | JsonError Json.Decode.Error
    | Ruby RubyObject


init : Model
init =
    Empty



-- UPDATE


type RubyType
    = RBool
    | RFloat
    | RInt
    | RNil
    | RString


type alias RubyObject =
    Dict String RubyType


jsonToSorbet : Json -> RubyType
jsonToSorbet json =
    case json of
        JBool _ ->
            RBool

        JFloat _ ->
            RFloat

        JInt _ ->
            RInt

        JNull ->
            RNil

        JString _ ->
            RString

        _ ->
            Debug.todo "Still need to handle recursive structures"


jsToRuby : Json -> RubyObject
jsToRuby json =
    case json of
        JObj object ->
            Dict.map (\_ v -> jsonToSorbet v) object

        _ ->
            Debug.todo "Still need to handle non-object structures"


type Msg
    = NewJson String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewJson "" ->
            Empty

        NewJson string ->
            case decodeString json string of
                Err err ->
                    JsonError err

                Ok json ->
                    Ruby <| jsToRuby json



-- VIEW


fieldLine : ( String, RubyType ) -> String
fieldLine ( field, rubyType ) =
    "  const :" ++ field ++ ", " ++ rubyTypeToString rubyType


rubyTypeToString : RubyType -> String
rubyTypeToString rubyType =
    case rubyType of
        RBool ->
            "T::Boolean"

        RFloat ->
            "Float"

        RInt ->
            "Integer"

        RNil ->
            "NilClass"

        RString ->
            "String"


rubyToString : RubyObject -> String
rubyToString rubyObject =
    let
        firstLine =
            "class Thing < T::Struct"

        fields =
            Dict.toList rubyObject |> List.sortBy Tuple.first |> List.map fieldLine

        lastLine =
            "end"
    in
    List.concat
        [ [ firstLine ]
        , fields
        , [ lastLine ]
        ]
        |> String.join "\n"


view : Model -> Html Msg
view model =
    div [] [ inputView, resultView model ]


inputView : Html Msg
inputView =
    textarea [ onInput NewJson ] []


resultView : Model -> Html Msg
resultView model =
    case model of
        Empty ->
            div [] [ text "nothing" ]

        JsonError err ->
            div [] [ text <| Json.Decode.errorToString err ]

        Ruby value ->
            div []
                [ pre [] [ text <| rubyToString value ]
                ]
