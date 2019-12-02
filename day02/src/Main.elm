module Main exposing (..)

import Browser
import Array exposing (Array)
import Html exposing (Html, input, div, text, textarea, h3, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Debug exposing (log)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    {
        input: String,
        intcode: Array Int,
        pc: Int,
        target: Int,
        pair: Pair
    }

type alias Pair =
    {
        noun: Int,
        verb: Int
    }

init : Model
init =
    Model "" (Array.empty) 0 0 (Pair 0 0)

type Msg
    = Load String
    | Execute
    | Find String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Load str ->
            let
                ic = String.split "," str
                    |> List.map String.trim
                    |> List.map String.toInt
                    |> List.map (Maybe.withDefault -1)
                    |> List.filter (\i -> i >= 0)
                    |> Array.fromList
            in
                Model str ic 0 0 (Pair 0 0)
        Execute ->
            execute model
        Find t ->
            let
                i = String.toInt t |> Maybe.withDefault 0
                model2 = { model | target=i }
                p = find model2 (Pair 0 0)
            in
                { model2 | pair=p }

--
-- Helper: load array value @ position
--
load : Array Int -> Int -> Int
load code pos =
    Array.get pos code
    |> Maybe.withDefault 0

-- Add two values & store @ position
add : Array Int -> Int -> Int -> Int -> Array Int
add code val1 val2 pos =
    Array.set pos (val1 + val2) code

-- Multiply two values & store @ position
mult : Array Int -> Int -> Int -> Int -> Array Int
mult code val1 val2 pos =
    Array.set pos (val1 * val2) code

-- execute program until halt/error
execute : Model -> Model
execute model =
    let
        opcode = load model.intcode (model.pc)
        pos1 = load model.intcode (model.pc + 1)
        pos2 = load model.intcode (model.pc + 2)
        pos3 = load model.intcode (model.pc + 3)
        val1 = load model.intcode pos1
        val2 = load model.intcode pos2
    in
        case opcode of
            1 ->
                Model model.input (add model.intcode val1 val2 pos3) (model.pc + 4) model.target (Pair 0 0)
                |> execute
            2 ->
                Model model.input (mult model.intcode val1 val2 pos3) (model.pc + 4) model.target (Pair 0 0)
                |> execute
            99 ->
                model
            _ ->
                model

--
-- helper: get next pair to check
-- no inf. loop check
nextPair : Pair -> Pair
nextPair p =
    if p.noun >= 99 then
        Pair 0 (p.verb + 1)
    else
        Pair (p.noun + 1) (p.verb)

-- load pair into proper array slots
setup : Array Int -> Pair -> Array Int
setup code p =
    Array.set 1 p.noun code
    |> Array.set 2 p.verb

-- find pair of inputs that gives the target result
find : Model -> Pair -> Pair
find model p =
    let
        prepped = setup model.intcode p
        final = execute { model | intcode = prepped }
        result = Array.get 0 final.intcode |> Maybe.withDefault 0
    in
        if model.target == result then
            p
        else
            find model (nextPair p)

stringify : Array Int -> String
stringify inp =
    Array.map String.fromInt inp
        |> Array.foldl (\i c -> c ++ "," ++ i) ""

view : Model -> Html Msg
view model =
    div [ style "margin" "15px" ]
        [ 
            h3 [] [ text "Int Code" ],
            textarea [ placeholder "code", value model.input, onInput Load] [],
            h3 [] [ text "Result" ],
            div [] [ text (stringify model.intcode) ],
            button [ onClick Execute ] [ text "Execute" ],
            h3 [] [ text "Find" ],
            input [ placeholder "Target", value (String.fromInt model.target), onInput Find ] [],
            h3 [] [ text "Result" ],
            div [] [ text ("Noun: " ++ (String.fromInt model.pair.noun)) ],
            div [] [ text ("Verb: " ++ (String.fromInt model.pair.verb)) ],
            div [] [ text ("Output: " ++ (String.fromInt ((100 * model.pair.noun) + model.pair.verb))) ]
        ]
