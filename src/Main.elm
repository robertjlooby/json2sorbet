module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (Element, el, fill, height, html, htmlAttribute, paragraph, spacing, text, width)
import Element.Font as Font
import Element.Input exposing (focusedOnLoad, multiline)
import EverySet exposing (EverySet)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode exposing (decodeString)
import Json.Decode.Generic exposing (Json(..), json)
import Json.Encode
import Regex


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
    | RDate
    | RDateTime
    | RFloat
    | RInt
    | RNil
    | RString


type alias RubyObject =
    Dict String (EverySet RubyType)


merge : RubyObject -> RubyObject -> RubyObject
merge ruby1 ruby2 =
    Dict.merge
        Dict.insert
        (\field types1 types2 ruby -> Dict.insert field (EverySet.union types1 types2) ruby)
        Dict.insert
        ruby1
        ruby2
        Dict.empty


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

        JString str ->
            if Regex.contains date str then
                RDate

            else if Regex.contains datetime str then
                RDateTime

            else
                RString

        _ ->
            Debug.todo "Still need to handle recursive structures"


date : Regex.Regex
date =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\d{4}-\\d{2}-\\d{2}$"


datetime : Regex.Regex
datetime =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}"


jsToRuby : Json -> RubyObject
jsToRuby json =
    case json of
        JObj object ->
            Dict.map (\_ v -> jsonToSorbet v |> EverySet.singleton) object

        JArr objects ->
            List.foldl (\js ruby -> merge ruby <| jsToRuby js) Dict.empty objects

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


fieldLine : ( String, EverySet RubyType ) -> String
fieldLine ( field, rubyTypes ) =
    "  const :" ++ field ++ ", " ++ rubyTypesFieldLine rubyTypes


rubyTypesFieldLine : EverySet RubyType -> String
rubyTypesFieldLine rubyTypes =
    case EverySet.toList rubyTypes of
        [] ->
            -- shouldn't actually happen
            "NilClass"

        [ oneRubyType ] ->
            rubyTypeToString oneRubyType

        multipleRubyTypes ->
            if List.member RNil multipleRubyTypes then
                "T.nilable(" ++ rubyTypesFieldLine (EverySet.remove RNil rubyTypes) ++ ")"

            else
                let
                    types =
                        List.map rubyTypeToString multipleRubyTypes
                            |> List.sort
                            |> String.join ", "
                in
                "T.any(" ++ types ++ ")"


rubyTypeToString : RubyType -> String
rubyTypeToString rubyType =
    case rubyType of
        RBool ->
            "T::Boolean"

        RDate ->
            "Date"

        RDateTime ->
            "DateTime"

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
    Element.row
        [ height fill
        , width fill
        , spacing 10
        ]
        [ inputView, resultView model ]
        |> Element.layout []


inputView : Element Msg
inputView =
    multiline
        [ height fill
        , width fill
        , focusedOnLoad
        ]
        { label = Element.Input.labelHidden "JSON"
        , onChange = NewJson
        , placeholder = Nothing
        , spellcheck = False
        , text = ""
        }


resultView : Model -> Element Msg
resultView model =
    let
        contents =
            case model of
                Empty ->
                    "Enter some JSON in the box on the left"

                JsonError err ->
                    Json.Decode.errorToString err

                Ruby value ->
                    rubyToString value
    in
    paragraph
        [ height fill
        , width fill
        , Font.family [ Font.monospace ]
        , htmlAttribute (style "white-space" "pre-wrap")
        ]
        [ html (Html.text contents) ]
