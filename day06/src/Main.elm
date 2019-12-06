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

type SpaceObject
    = Orbiting String
    | COM

type alias Orbiter =
    {
        id: String,
        link: SpaceObject,
        reverseLink: List SpaceObject
    }

strToSpaceObject : String -> SpaceObject
strToSpaceObject str =
    case str of
        "COM" -> COM
        _ -> Orbiting str

strToOrbiter : String -> Orbiter
strToOrbiter str =
    let
        pair = String.split ")" str
        orbiting = List.head pair |> Maybe.withDefault "COM"
        id = List.drop 1 pair |> List.head |> Maybe.withDefault "COM"
        obj = strToSpaceObject orbiting
    in
        Orbiter id obj []

type alias Model =
    {
        orbiters: Dict String Orbiter
    }

init : Model
init =
    Model Dict.empty

load : String -> Model -> Model
load str model =
    let
        all = String.split "\n" str
            |> List.map String.trim
            |> List.map strToOrbiter
            |> List.foldl (\i c -> Dict.insert i.id i c) Dict.empty
        finish = Dict.foldl (\k v c -> revLink v c) all all
    in
        { model | orbiters = finish }

revLink : Orbiter -> Dict String Orbiter -> Dict String Orbiter
revLink obj orbiters =
    case obj.link of
        COM -> orbiters
        Orbiting o ->
            case (Dict.get o orbiters) of
                Just a ->
                    let
                        link = (strToSpaceObject obj.id) :: a.reverseLink
                        linked = { a | reverseLink = link }
                    in
                        Dict.insert a.id linked orbiters
                Nothing ->
                    orbiters

countOrbits : Orbiter -> Int -> Model -> Int
countOrbits node soFar model =
    case node.link of
        COM -> soFar + 1
        Orbiting obj ->
            case (Dict.get obj model.orbiters) of
                Just a -> countOrbits a (soFar + 1) model
                Nothing -> soFar
                
sumOrbits : Model -> Int
sumOrbits model =
    Dict.foldl (\k v c -> c + (countOrbits v 0 model)) 0 model.orbiters

nextSteps : Orbiter -> Model -> Set String -> List String
nextSteps node model visited =
    (node.link :: node.reverseLink)
    |> List.map (\i -> case i of
                    COM -> ""
                    Orbiting o -> o
        )
    |> List.filter (\i -> i /= "")
    |> List.filter (\i -> not (Set.member i visited))

bfs : Int -> List Orbiter -> Model -> String -> Set String -> Int
bfs steps nodes model target visited =
    if (List.member target (List.map (\i -> i.id) nodes)) then
        steps
    else
        let
            newVisited = List.foldl (\i c -> Set.insert i.id c) visited nodes
            newNodes = List.foldl (\i c -> List.append c (nextSteps i model newVisited)) [] nodes
                |> Set.fromList |> Set.toList
                |> List.filterMap (\i -> Dict.get i model.orbiters) 
        in
            if (List.length newNodes) == 0 then
                -1
            else
                bfs (steps + 1) newNodes model target newVisited

shortestPath :  String -> String -> Model -> Int
shortestPath start end model =
    case (Dict.get start model.orbiters) of
        Just a -> (bfs 0 [a] model end Set.empty) - 2
        Nothing -> 0

type Msg
    = Load String
    | Execute

update : Msg -> Model -> Model
update msg model =
    case msg of
        Load str ->
            load str model
        Execute ->
            model

view : Model -> Html Msg
view model =
    div [ style "margin" "15px" ]
        [ 
            h3 [] [ text "Map info" ],
            textarea [ placeholder "map", onInput Load] [],
            h3 [] [ text "Num. Orbits" ],
            div [] [ text (String.fromInt (sumOrbits model)) ],
            h3 [] [ text "Shortest path" ],
            div [] [ text (String.fromInt (shortestPath "YOU" "SAN" model)) ]
        ]
