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


type Json2SorbetError
    = Json2SorbetError String


type Model
    = Empty
    | JsonError Json.Decode.Error
    | Unsupported Json2SorbetError
    | Success SorbetStruct


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


type alias SorbetStruct =
    Dict String SorbetType


type alias SorbetType =
    EverySet RubyType


merge : SorbetStruct -> SorbetStruct -> SorbetStruct
merge struct1 struct2 =
    Dict.merge
        (\field types1 struct -> Dict.insert field (EverySet.insert RNil types1) struct)
        (\field types1 types2 struct -> Dict.insert field (EverySet.union types1 types2) struct)
        (\field types2 struct -> Dict.insert field (EverySet.insert RNil types2) struct)
        struct1
        struct2
        Dict.empty


jsonValueToRubyType : Json -> Result Json2SorbetError RubyType
jsonValueToRubyType json =
    case json of
        JBool _ ->
            Ok RBool

        JFloat _ ->
            Ok RFloat

        JInt _ ->
            Ok RInt

        JNull ->
            Ok RNil

        JString str ->
            if Regex.contains date str then
                Ok RDate

            else if Regex.contains datetime str then
                Ok RDateTime

            else
                Ok RString

        JArr _ ->
            Err <| Json2SorbetError "Can't handle a nested array in this position yet"

        JObj _ ->
            Err <| Json2SorbetError "Can't handle a nested object in this position yet"


date : Regex.Regex
date =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\d{4}-\\d{2}-\\d{2}$"


datetime : Regex.Regex
datetime =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}"


jsonToSorbetStruct : Json -> Result Json2SorbetError SorbetStruct
jsonToSorbetStruct json =
    case json of
        JObj object ->
            Dict.foldl
                (\k v struct ->
                    Result.map2
                        (Dict.insert k)
                        (jsonValueToRubyType v |> Result.map EverySet.singleton)
                        struct
                )
                (Ok Dict.empty)
                object

        JArr [] ->
            Ok Dict.empty

        JArr (o :: os) ->
            -- Convert first item separately so that merge doesn't consider all fields nilable
            List.foldl
                (\js struct -> Result.map2 merge struct (jsonToSorbetStruct js))
                (jsonToSorbetStruct o)
                os

        _ ->
            Err <| Json2SorbetError "Top level value must be an object or array of objects"


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
                    case jsonToSorbetStruct json of
                        Ok struct ->
                            Success struct

                        Err err ->
                            Unsupported err



-- VIEW


fieldLine : ( String, SorbetType ) -> String
fieldLine ( field, sorbetType ) =
    "  const :" ++ field ++ ", " ++ sorbetTypeToString sorbetType


sorbetTypeToString : SorbetType -> String
sorbetTypeToString sorbetType =
    case EverySet.toList sorbetType of
        [] ->
            -- shouldn't actually happen
            "NilClass"

        [ oneRubyType ] ->
            rubyTypeToString oneRubyType

        multipleRubyTypes ->
            if List.member RNil multipleRubyTypes then
                "T.nilable(" ++ sorbetTypeToString (EverySet.remove RNil sorbetType) ++ ")"

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


sorbetStructToString : SorbetStruct -> String
sorbetStructToString sorbetStruct =
    let
        firstLine =
            "class Thing < T::Struct"

        fields =
            Dict.toList sorbetStruct
                |> List.sortBy Tuple.first
                |> List.map fieldLine

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

                Unsupported (Json2SorbetError errMsg) ->
                    "Unsupported: " ++ errMsg

                Success sorbetStruct ->
                    sorbetStructToString sorbetStruct
    in
    paragraph
        [ height fill
        , width fill
        , Font.family [ Font.monospace ]
        , htmlAttribute (style "white-space" "pre-wrap")
        ]
        [ html (Html.text contents) ]
