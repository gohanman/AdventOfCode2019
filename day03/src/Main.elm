module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (Html, input, div, text, textarea, h3, button, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Debug exposing (log)

main =
  Browser.sandbox { init = init, update = update, view = view }

type Direction
    = Left
    | Right
    | Up
    | Down
    | Invalid

type alias Move =
    {
        num: Int,
        dir: Direction
    }

strToMove : String -> Move
strToMove str =
    let
        dir = String.left 1 str
        num = String.dropLeft 1 str
            |> String.toInt
            |> Maybe.withDefault 0
    in
        case dir of
            "R" ->
                Move num Right
            "L" ->
                Move num Left
            "U" ->
                Move num Up
            "D" ->
                Move num Down
            _ ->
                Move num Invalid

type alias Wire = List Move

strToWire : String -> Wire
strToWire str =
    String.split "," str
    |> List.map String.trim
    |> List.map strToMove
    |> List.filter (\i -> i.num > 0 && i.dir /= Invalid)

type alias Point =
    {
        x: Int,
        y: Int
    }

type alias Model =
    {
        wires: List Wire,
        min: Point,
        max: Point,
        grid: Dict (Int, Int) Int
    }

moveToPoints : Point -> Move -> List Point
moveToPoints origin move =
    case move.dir of
        Right ->
            List.range 1 move.num
            |> List.map (\i -> Point (origin.x + i) origin.y)
        Left ->
            List.range 1 move.num
            |> List.map (\i -> Point (origin.x - i) origin.y)
        Down ->
            List.range 1 move.num
            |> List.map (\i -> Point origin.x (origin.y - i))
        Up ->
            List.range 1 move.num
            |> List.map (\i -> Point origin.x (origin.y + i))
        Invalid ->
            []

wireToPoints : Point -> List Point -> Wire -> List Point
wireToPoints origin points w =
    case w of
        [] -> points
        x::xs ->
            let
                newPoints = moveToPoints origin x
                    |> List.append points
                newOrigin = List.reverse newPoints
                    |> List.head
                    |> Maybe.withDefault origin
            in
                wireToPoints newOrigin newPoints xs

addPoint : Int -> Point -> Dict (Int, Int) Int -> Dict (Int, Int) Int
addPoint wireID p grid =
    case (Dict.get (p.x, p.y) grid) of
        Just a ->
            let
                next = if wireID + 1 > a then a + 1 else a
            in
                Dict.insert (p.x, p.y) next grid
        Nothing ->
            Dict.insert (p.x, p.y) 1 grid

addPoints : Int -> List Point -> Dict (Int, Int) Int -> Dict (Int, Int) Int
addPoints wireID points grid =
    List.foldl (addPoint wireID) grid points

plot : Int -> Model -> Model
plot wireID model =
    case (List.drop wireID model.wires |> List.head) of
        Just a ->
            let
                points = wireToPoints (Point 0 0) [] a
                newGrid = addPoints wireID points model.grid
                x = getXMinMax newGrid
                y = getYMinMax newGrid
                newModel = { model | grid = newGrid }
            in
                plot (wireID + 1) newModel
        Nothing ->
            let
                x = getXMinMax model.grid
                y = getYMinMax model.grid
            in
                { model | min = (Point x.x y.x), max = (Point x.y y.y) }

getSprite : Point -> Int -> String -> String
getSprite p entry newline =
    if p.x == 0 && p.y == 0 then
        "O" ++ newline
    else
        case entry of
            0 -> " " ++ newline
            1 -> "*" ++ newline
            _ -> "X" ++ newline

draw : Point -> Point -> Point -> Dict (Int, Int) Int -> String -> String
draw min max current grid output =
    let
        entry = Dict.get (current.x, current.y) grid
            |> Maybe.withDefault 0
        newline =
            if (current.x + 1 > max.x) then
                "\n"
            else
                ""
        sprite = getSprite current entry newline
        newPoint =
            if (current.x + 1 > max.x) then
                Point min.x (current.y - 1)
            else
                Point (current.x + 1) current.y
    in
         if (newPoint.y < min.y) then
             output ++ sprite
         else
             draw min max newPoint grid (output ++ sprite)

getXMinMax : Dict (Int, Int) Int -> Point
getXMinMax grid =
    let 
        xCoords = Dict.toList grid
            |> List.map Tuple.first
            |> List.map Tuple.first
        xMax = List.maximum xCoords
            |> Maybe.withDefault 0
        xMin = List.minimum xCoords
            |> Maybe.withDefault 0
    in
        Point xMin xMax

getYMinMax : Dict (Int, Int) Int -> Point
getYMinMax grid =
    let
        yCoords = Dict.toList grid
            |> List.map Tuple.first
            |> List.map Tuple.second
        yMax = List.maximum yCoords
            |> Maybe.withDefault 0
        yMin = List.minimum yCoords
            |> Maybe.withDefault 0
    in
        Point yMin yMax

collisions : Dict (Int, Int) Int -> List Point
collisions grid =
    Dict.filter (\k v -> v > 1) grid
    |> Dict.toList
    |> List.map Tuple.first
    |> List.map (\i -> Point (Tuple.first i) (Tuple.second i))

distance : Point -> Point -> Int
distance p1 p2 =
    abs(p1.x - p2.x) + abs(p1.y - p2.y)

closest: Point -> Dict (Int, Int) Int -> Int
closest origin grid =
    collisions grid
    |> List.map (\i -> distance origin i)
    |> List.minimum
    |> Maybe.withDefault 0

close2 : Point -> Model -> Int
close2 origin model =
    let
        first = List.head model.wires
            |> Maybe.withDefault []
            |> wireToPoints origin []
            |> List.map (\i -> (i.x, i.y))
            |> List.foldl (\i c -> Set.insert i c) Set.empty
        _ = Debug.log "first" first
        overlap = List.drop 1 model.wires
            |> List.head
            |> Maybe.withDefault []
            |> wireToPoints origin []
            |> List.map(\i -> (i.x, i.y))
            |> List.filter (\i -> Set.member i first)
            |> List.map (\i -> Point (Tuple.first i) (Tuple.second i))
        _ = Debug.log "overlap" overlap
    in
        List.map (distance origin) overlap
        |> List.minimum
        |> Maybe.withDefault 0

close3 : Point -> Model -> Int
close3 origin model =
    let
        first = List.head model.wires
            |> Maybe.withDefault []
            |> wireToPoints origin []
            |> List.indexedMap (\idx i -> ((i.x, i.y), idx))
            |> List.foldl (\i c -> Dict.insert (Tuple.first i) (Tuple.second i) c) Dict.empty
        _ = Debug.log "first" first
        overlap = List.drop 1 model.wires
            |> List.head
            |> Maybe.withDefault []
            |> wireToPoints origin []
            |> List.indexedMap (\idx i -> ((i.x, i.y), idx))
            |> List.filter (\i -> Dict.member (Tuple.first i) first)
        _ = Debug.log "overlap" overlap
    in
        overlap |> List.map (\i ->
                (Tuple.second i) + (Dict.get (Tuple.first i) first |> Maybe.withDefault 99999) + 2
            )
            |> List.minimum
            |> Maybe.withDefault 0

init : Model
init =
    Model [] (Point 0 0) (Point 0 0) (Dict.empty)

type Msg
    = Load String
    | Draw

update : Msg -> Model -> Model
update msg model =
    case msg of
        Load str ->
            let
                wires = String.split "\n" str
                    |> List.map strToWire
            in
                Model wires (Point 0 0) (Point 0 0) (Dict.empty)
        Draw ->
            model

view : Model -> Html Msg
view model =
    div [ style "margin" "15px" ]
        [
            h3 [] [ text "Wires" ],
            textarea [ placeholder "wires", onInput Load] [],
            h3 [] [ text "Closest Overlap" ],
            div [] [ text (String.fromInt (close3 (Point 0 0) model)) ],
            h3 [] [ text "Diagram" ],
            div [ style "font-family" "monospace" ]
            [
                --pre [] [text (draw model.min model.max (Point model.min.x model.max.y) model.grid "")]
            ]
        ]
