
module Main exposing (..)

import Browser
import Html exposing (Html, input, div, text, label, h3, p, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Debug exposing (log)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    {
        min: String,
        max: String,
        valid: Int,
        triples: Bool
    }

init : Model
init =
    Model "0" "0" 0 False

type Msg
    = Min String
    | Max String
    | Check
    | ToggleTriples

update : Msg -> Model -> Model
update msg model =
    case msg of
        Min m ->
            { model | min = m }
        Max m ->
            { model | max = m }
        Check ->
            let
                p = possible model
            in
                { model | valid = p }
        ToggleTriples ->
            { model | triples = (not model.triples) }

strToDigits : String -> List Int
strToDigits str =
    String.toList str
    |> List.map String.fromChar
    |> List.map String.toInt
    |> List.map (Maybe.withDefault 0)

-- Verify list is strictly increasing
increasing : List Int -> Bool
increasing x =
    case (List.length x) of
        0 -> False
        1 -> True
        _ ->
            let
                cur = List.head x |> Maybe.withDefault 0
                safeTail = List.drop 1 x
                remains = List.minimum safeTail
                    |> Maybe.withDefault 0
            in
                if (cur <= remains) then
                    increasing safeTail
                else
                    False

-- verify list has a pair of same values
hasPair : List Int -> Bool
hasPair x =
    case (List.length x) of
        0 -> False
        1 -> False
        _ ->
            let
                a = List.head x |> Maybe.withDefault 0
                b = List.take 2 x
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault 0
            in
                if (a == b) then
                    True
                else
                    hasPair (List.drop 1 x)

-- count occurences of target value starting from
-- the head of the list
consecutives : Int -> Int -> List Int -> Int
consecutives target soFar x =
    let
        next = List.head x |> Maybe.withDefault -1
    in
        if (List.length x) == 0 || next /= target then
            soFar
        else
            consecutives target (soFar + 1) (List.drop 1 x)

-- trim occurences of the target value from
-- the head of the list
trim : Int -> List Int -> List Int
trim target x =
    let
        next = List.head x |> Maybe.withDefault -1
    in
        if next == target then
            trim target (List.drop 1 x)
        else
            x

-- verify list has a pair of values not part of
-- a longer sequences (triple, etc)
standalonePair : List Int -> Bool
standalonePair x =
    case (List.length x) of
        0 -> False
        1 -> False
        _ ->
            let
                a = List.head x |> Maybe.withDefault 0
                b = List.take 2 x
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault 0
                tail = List.drop 1 x
                repeats = consecutives b 0 tail
            in
                if a == b && repeats == 1 then
                    True
                else if a == b then
                    standalonePair (trim b tail)
                else
                    standalonePair (List.tail x |> Maybe.withDefault [])

-- find number of values in range that match rules
possible : Model -> Int
possible model =
    let
        a = String.toInt model.min |> Maybe.withDefault 0
        b = String.toInt model.max |> Maybe.withDefault 0
        all = List.range a b |> List.map String.fromInt
    in
        List.map strToDigits all
        |> List.filter increasing
        |> List.filter hasPair
        |> List.map (\i -> if model.triples then i else [1, 1])
        |> List.filter standalonePair
        |> List.length

view : Model -> Html Msg
view model =
    div [ style "margin" "15px" ]
        [ 
            h3 [] [ text "Range" ],
            p [] [
                label [] [ text "Min" ],
                input [ value model.min, onInput Min ] []
            ],
            p [] [
                label [] [ text "Max" ],
                input [ value model.max, onInput Max ] []
            ],
            p [] [
                label [] [
                    input [ type_ "checkbox", onClick ToggleTriples ] [],
                    text "No triples allowed"
                ]
            ],
            p [] [
                button [ onClick Check ] [ text "Check" ]
            ],
            h3 [] [ text "Valid possibilities" ],
            div [] [ text (String.fromInt model.valid) ]
        ]
