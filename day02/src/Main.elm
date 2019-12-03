module Main exposing (..)

import Browser
import Array exposing (Array)
import Html exposing (Html, input, div, text, textarea, h3, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Debug exposing (log)

import Computer exposing (Computer, StartCode)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    {
        input: String,
        pc: Computer,
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
    Model "" (Computer.init "") 0 (Pair 0 0)

type Msg
    = Load String
    | Execute
    | Find String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Load str ->
            Model str (Computer.init str) 0 (Pair 0 0)
        Execute ->
            let
                pc = Computer.setup (StartCode 12 2) model.pc
                    |> Computer.execute
            in
                { model | pc = pc }
        Find t ->
            let
                i = String.toInt t |> Maybe.withDefault 0
                model2 = { model | target=i }
                p = find model2 (Pair 0 0)
            in
                { model2 | pair=p }

--
-- helper: get next pair to check
-- no inf. loop check
nextPair : Pair -> Pair
nextPair p =
    if p.noun >= 99 then
        Pair 0 (p.verb + 1)
    else
        Pair (p.noun + 1) (p.verb)

-- find pair of inputs that gives the target result
find : Model -> Pair -> Pair
find model p =
    let
        prepped = Computer.setup (StartCode p.noun p.verb) model.pc
        final = Computer.execute prepped
        result = Array.get 0 final.code |> Maybe.withDefault 0
    in
        if model.target == result then
            p
        else
            find model (nextPair p)

view : Model -> Html Msg
view model =
    div [ style "margin" "15px" ]
        [ 
            h3 [] [ text "Int Code" ],
            textarea [ placeholder "code", value model.input, onInput Load] [],
            h3 [] [ text "Result" ],
            div [] [ text (Computer.stringify model.pc) ],
            button [ onClick Execute ] [ text "Execute" ],
            h3 [] [ text "Find" ],
            input [ placeholder "Target", value (String.fromInt model.target), onInput Find ] [],
            h3 [] [ text "Result" ],
            div [] [ text ("Noun: " ++ (String.fromInt model.pair.noun)) ],
            div [] [ text ("Verb: " ++ (String.fromInt model.pair.verb)) ],
            div [] [ text ("Output: " ++ (String.fromInt ((100 * model.pair.noun) + model.pair.verb))) ]
        ]
