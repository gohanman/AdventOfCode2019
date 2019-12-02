module Main exposing (..)

import Browser
import Html exposing (Html, input, div, text, textarea, h3)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Debug exposing (log)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
    {
        weights: String,
        total: Int,
        smartTotal: Int
    }

init : Model
init =
    Model "" 0 0

-- UPDATE

type Msg
    = Weights String

fuel: Int -> Int
fuel i = (i // 3) - 2

recursiveFuel: List Int -> Int -> Int
recursiveFuel acc i =
    let next = fuel i
    in
        if next <= 0 then
            List.foldl (+) 0 acc
        else
            recursiveFuel (next::acc) next

update : Msg -> Model -> Model
update msg model =
    case msg of
        Weights w ->
            let
                total = w
                    |> String.split "\n"
                    |> List.map String.trim
                    |> List.map String.toInt
                    |> List.map (Maybe.withDefault 0)
                    |> List.filter (\i -> i > 0)
                    |> List.foldl (\i c -> c + (fuel i)) 0
                smart = w
                    |> String.split "\n"
                    |> List.map String.trim
                    |> List.map String.toInt
                    |> List.map (Maybe.withDefault 0)
                    |> List.filter (\i -> i > 0)
                    |> List.foldl (\i c -> c + (recursiveFuel [] i)) 0
            in
                Model w total smart

-- VIEW

view : Model -> Html Msg
view model =
    div [ style "margin" "15px" ]
        [ 
            h3 [] [ text "Weights" ],
            textarea [ placeholder "weights", value model.weights, onInput Weights ] [],
            h3 [] [ text "Total" ],
            div [] [ text (String.fromInt model.total) ],
            h3 [] [ text "Smarter Total" ],
            div [] [ text (String.fromInt model.smartTotal) ]
        ]
