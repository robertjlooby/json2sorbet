module Main exposing (main)

import AssocList as Dict exposing (Dict)
import Browser
import Dict as CoreDict
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
    | Success SorbetStructs


init : Model
init =
    Empty



-- UPDATE


type RubyType
    = RArr SorbetType
    | RBool
    | RDate
    | RDateTime
    | RFloat
    | RInt
    | RNil
    | RString
    | RSorbetType ClassName


type ClassName
    = ClassName String


type alias SorbetStructs =
    Dict ClassName SorbetStruct


type alias SorbetStruct =
    Dict String SorbetType


type alias SorbetType =
    EverySet RubyType


merges : SorbetStructs -> SorbetStructs -> SorbetStructs
merges structs1 structs2 =
    Dict.merge
        (\field struct1 structs -> Dict.insert field struct1 structs)
        (\field struct1 struct2 structs -> Dict.insert field (merge struct1 struct2) structs)
        (\field struct2 structs -> Dict.insert field struct2 structs)
        structs1
        structs2
        Dict.empty


merge : SorbetStruct -> SorbetStruct -> SorbetStruct
merge struct1 struct2 =
    Dict.merge
        (\field types1 struct -> Dict.insert field (EverySet.insert RNil types1) struct)
        (\field types1 types2 struct -> Dict.insert field (EverySet.union types1 types2) struct)
        (\field types2 struct -> Dict.insert field (EverySet.insert RNil types2) struct)
        struct1
        struct2
        Dict.empty


jsonValueToSorbetType : String -> Json -> SorbetType
jsonValueToSorbetType label json =
    case json of
        JBool _ ->
            EverySet.singleton RBool

        JFloat _ ->
            EverySet.singleton RFloat

        JInt _ ->
            EverySet.singleton RInt

        JNull ->
            EverySet.singleton RNil

        JString str ->
            if Regex.contains date str then
                EverySet.singleton RDate

            else if Regex.contains datetime str then
                EverySet.singleton RDateTime

            else
                EverySet.singleton RString

        JArr arr ->
            List.foldl
                (\js st -> EverySet.union (jsonValueToSorbetType label js) st)
                EverySet.empty
                arr
                |> RArr
                |> EverySet.singleton

        JObj _ ->
            toClassName label
                |> RSorbetType
                |> EverySet.singleton


date : Regex.Regex
date =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\d{4}-\\d{2}-\\d{2}$"


datetime : Regex.Regex
datetime =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}"


fromCoreDict : CoreDict.Dict k v -> Dict k v
fromCoreDict dict =
    CoreDict.toList dict |> Dict.fromList


jsonToSorbetStructs : SorbetStructs -> ClassName -> Json -> Result Json2SorbetError SorbetStructs
jsonToSorbetStructs structsIn label json =
    case json of
        JObj object ->
            Dict.foldl
                (\k v struct ->
                    Dict.insert k
                        (jsonValueToSorbetType k v)
                        struct
                )
                Dict.empty
                (fromCoreDict object)
                |> Dict.singleton label
                |> Ok

        JArr [] ->
            Ok Dict.empty

        JArr (o :: os) ->
            -- Convert first item separately so that merge doesn't consider all fields nilable
            List.foldl
                (\js structsOut -> Result.map2 merges structsOut (jsonToSorbetStructs structsIn label js))
                (jsonToSorbetStructs structsIn label o)
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
                    case jsonToSorbetStructs Dict.empty (toClassName "Json2Sorbet") json of
                        Ok structs ->
                            Success structs

                        Err err ->
                            Unsupported err



-- VIEW


emptyArray : RubyType
emptyArray =
    RArr EverySet.empty


isNonEmptyArray : RubyType -> Bool
isNonEmptyArray rubyType =
    case rubyType of
        RArr sorbetType ->
            EverySet.size sorbetType > 0

        _ ->
            False


refine : SorbetType -> SorbetType
refine sorbetType =
    if EverySet.member emptyArray sorbetType && EverySet.size (EverySet.filter isNonEmptyArray sorbetType) > 0 then
        -- given [{"a": []}, {"a": [1]}], refine to just T::Array[Integer] instead of T.any(T::Array[T.untyped], T::Array[Integer])
        EverySet.remove emptyArray sorbetType

    else
        sorbetType


fieldLine : ( String, SorbetType ) -> String
fieldLine ( field, sorbetType ) =
    "  const :" ++ field ++ ", " ++ sorbetTypeToString sorbetType


sorbetTypeToString : SorbetType -> String
sorbetTypeToString sorbetType =
    case EverySet.toList (refine sorbetType) of
        [] ->
            "T.untyped"

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
        RArr sorbetType ->
            "T::Array[" ++ sorbetTypeToString sorbetType ++ "]"

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

        RSorbetType (ClassName className) ->
            className


toClassName : String -> ClassName
toClassName label =
    String.split "_" label
        |> List.map capitalize
        |> String.join ""
        |> ClassName


capitalize : String -> String
capitalize string =
    case String.uncons string of
        Just ( head, rest ) ->
            String.cons (Char.toUpper head) rest

        Nothing ->
            string


sorbetStructsToString : SorbetStructs -> String
sorbetStructsToString sorbetStructs =
    Dict.foldl
        (\label struct output -> output ++ sorbetStructToString label struct)
        ""
        sorbetStructs


sorbetStructToString : ClassName -> SorbetStruct -> String
sorbetStructToString (ClassName label) sorbetStruct =
    let
        firstLine =
            "class " ++ label ++ " < T::Struct"

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

                Success sorbetStructs ->
                    sorbetStructsToString sorbetStructs
    in
    paragraph
        [ height fill
        , width fill
        , Font.family [ Font.monospace ]
        , htmlAttribute (style "white-space" "pre-wrap")
        ]
        [ html (Html.text contents) ]
