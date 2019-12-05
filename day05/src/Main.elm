module Main exposing (..)

import Browser
import Array exposing (Array)
import Html exposing (Html, input, div, text, textarea, h3, button, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Debug exposing (log)

import Computer exposing (Computer, StartCode)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    {
        input: String,
        pc: Computer
    }

init : Model
init =
    Model "" (Computer.init "")

type Msg
    = Load String
    | Execute
    | Inputs String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Load str ->
            let
                pc = Computer.init str
            in
                Model str ({ pc | inputs = [5] })
        Inputs str ->
            { model | pc = (Computer.setInputs str model.pc) }
        Execute ->
            let
                pc = Computer.execute model.pc
            in
                { model | pc = pc }
--
view : Model -> Html Msg
view model =
    div [ style "margin" "15px" ]
        [ 
            h3 [] [ text "Int Code" ],
            textarea [ placeholder "code", value model.input, onInput Load] [],
            h3 [] [ text "Input(s)" ],
            textarea [ placeholder "inputs",  onInput Inputs] [],
            h3 [] [ text "Output" ],
            div [] [
                pre [] [ text model.pc.outputs ]
            ],
            button [ onClick Execute ] [ text "Execute" ],
            div [] [ text (Computer.stringify model.pc) ]
        ]
