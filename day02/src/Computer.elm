module Computer exposing (..)

import Array exposing (Array)

type alias Computer =
    {
        code: Array Int,
        pc: Int
    }

type alias StartCode =
    {
        noun: Int,
        verb: Int
    }

init: String -> Computer
init str =
    let
        ic = String.split "," str
            |> List.map String.trim
            |> List.map String.toInt
            |> List.map (Maybe.withDefault -1)
            |> List.filter (\i -> i >= 0)
            |> Array.fromList
    in
        Computer ic 0

setup : StartCode -> Computer -> Computer
setup sc pc =
    let
        ic = Array.set 1 sc.noun pc.code
            |> Array.set 2 sc.verb
    in
        { pc | code = ic }

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

execute : Computer -> Computer
execute model =
    let
        opcode = load model.code (model.pc)
        pos1 = load model.code (model.pc + 1)
        pos2 = load model.code (model.pc + 2)
        pos3 = load model.code (model.pc + 3)
        val1 = load model.code pos1
        val2 = load model.code pos2
    in
        case opcode of
            1 ->
                Computer (add model.code val1 val2 pos3) (model.pc + 4)
                |> execute
            2 ->
                Computer (mult model.code val1 val2 pos3) (model.pc + 4)
                |> execute
            99 ->
                model
            _ ->
                model

stringify : Computer -> String
stringify pc =
    Array.map String.fromInt pc.code
        |> Array.foldl (\i c -> c ++ "," ++ i) ""

