module Main exposing (main)

import AssocList as Dict exposing (Dict)
import Browser
import Dict as CoreDict
import Element exposing (Element, centerX, el, fill, height, html, htmlAttribute, link, paragraph, scrollbars, spacing, text, width)
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


jsonValueToSorbetType : SorbetStructs -> String -> Json -> ( SorbetType, SorbetStructs )
jsonValueToSorbetType structsIn label json =
    case json of
        JBool _ ->
            ( EverySet.singleton RBool, structsIn )

        JFloat _ ->
            ( EverySet.singleton RFloat, structsIn )

        JInt _ ->
            ( EverySet.singleton RInt, structsIn )

        JNull ->
            ( EverySet.singleton RNil, structsIn )

        JString str ->
            if Regex.contains date str then
                ( EverySet.singleton RDate, structsIn )

            else if Regex.contains datetime str then
                ( EverySet.singleton RDateTime, structsIn )

            else
                ( EverySet.singleton RString, structsIn )

        JArr arr ->
            List.foldl
                (\js ( arrTypesIn, structsAcc ) ->
                    case jsonValueToSorbetType structsAcc label js of
                        ( arrTypesOut, structsOut ) ->
                            ( EverySet.union arrTypesIn arrTypesOut, structsOut )
                )
                ( EverySet.empty, structsIn )
                arr
                |> (\( t, structsOut ) -> ( EverySet.singleton (RArr t), structsOut ))

        (JObj _) as object ->
            let
                className =
                    toClassName label

                t =
                    className
                        |> RSorbetType
                        |> EverySet.singleton

                structsOut =
                    jsonToSorbetStructs structsIn className object
            in
            ( t, structsOut )


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


jsonToSorbetStructs : SorbetStructs -> ClassName -> Json -> SorbetStructs
jsonToSorbetStructs structsIn label json =
    case json of
        JObj object ->
            Dict.foldl
                (\k v ( struct, structs ) ->
                    case jsonValueToSorbetType structs k v of
                        ( typeOut, structsOut ) ->
                            ( Dict.insert k typeOut struct, structsOut )
                )
                ( Dict.empty, structsIn )
                (fromCoreDict object)
                |> (\( struct, structsOut ) -> merges structsOut (Dict.singleton label struct))

        JArr (o :: os) ->
            -- Convert first item separately so that merge doesn't consider all fields nilable
            List.foldl
                (\js structsOut -> merges structsOut (jsonToSorbetStructs structsIn label js))
                (jsonToSorbetStructs structsIn label o)
                os

        _ ->
            case jsonValueToSorbetType structsIn "singleValue" json of
                ( typeOut, structsOut ) ->
                    merges (Dict.singleton label (Dict.singleton "value" typeOut)) structsOut


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
                    jsonToSorbetStructs Dict.empty (toClassName "Json2Sorbet") json
                        |> Success



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
        (\label struct output -> sorbetStructToString label struct ++ output)
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
        , [ lastLine ++ "\n\n" ]
        ]
        |> String.join "\n"


view : Model -> Html Msg
view model =
    let
        mainRow =
            Element.row
                [ height fill
                , width fill
                , spacing 10
                ]
                [ inputView, resultView model ]

        sourceLink =
            link
                [ width fill ]
                { url = "https://github.com/robertjlooby/json2sorbet"
                , label = el [ centerX ] (text "source code")
                }
    in
    Element.column [ height fill, width fill, spacing 10 ] [ mainRow, sourceLink ]
        |> Element.layout []


inputView : Element Msg
inputView =
    multiline
        [ height fill
        , width fill
        , focusedOnLoad
        , scrollbars
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

                Success sorbetStructs ->
                    sorbetStructsToString sorbetStructs
    in
    paragraph
        [ height fill
        , width fill
        , Font.family [ Font.monospace ]
        , htmlAttribute (style "white-space" "pre-wrap")
        , scrollbars
        ]
        [ html (Html.text contents) ]
