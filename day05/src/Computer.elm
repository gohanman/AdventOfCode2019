module Computer exposing (..)

import Array exposing (Array)

type alias Computer =
    {
        code: Array Int,
        pc: Int,
        inputs: List Int,
        outputs: String
    }

type alias StartCode =
    {
        noun: Int,
        verb: Int
    }

type ParamMode
    = Position
    | Immediate

init: String -> Computer
init str =
    let
        ic = String.split "," str
            |> List.map String.trim
            |> List.map String.toInt
            |> List.filterMap identity
            |> Array.fromList
    in
        Computer ic 0 [] ""

setInputs : String -> Computer -> Computer
setInputs str pc =
    let
        inp = String.split "\n" str
            |> List.map String.trim
            |> List.filterMap String.toInt
    in
        { pc | inputs = inp, outputs = "Input changed!\n" }

setup : StartCode -> Computer -> Computer
setup sc pc =
    let
        ic = Array.set 1 sc.noun pc.code
            |> Array.set 2 sc.verb
    in
        { pc | code = ic }

getOpcode : Int -> Int
getOpcode x =
    String.fromInt x
    |> String.toList
    |> List.reverse
    |> List.take 2
    |> List.reverse
    |> List.map String.fromChar
    |> List.foldr (++) ""
    |> String.toInt
    |> Maybe.withDefault 0

getMode : Int -> Int -> ParamMode
getMode x pos =
    let
        digit = String.fromInt x
            |> String.toList
            |> List.reverse
            |> List.drop (2 + (pos - 1))
            |> List.head
            |> Maybe.withDefault '0'
    in
        case digit of
            '1' -> Immediate
            _ -> Position

printMode : Int -> Int -> String
printMode x pos =
    case (getMode x pos) of
        Immediate -> "imm"
        Position -> "pos"
--
-- Helper: load array value @ position
--
load : Array Int -> Int -> ParamMode -> Int
load code pos mode =
    case mode of
        Immediate ->
            Array.get pos code
            |> Maybe.withDefault 0
        Position ->
            let
                newPos = Array.get pos code
                    |> Maybe.withDefault 0
            in
                load code newPos Immediate

-- Add two values & store @ position
add : Array Int -> Int -> Int -> Int -> Array Int
add code a b c =
    Array.set c (a + b) code

-- Multiply two values & store @ position
mult : Array Int -> Int -> Int -> Int -> Array Int
mult code a b c =
    Array.set c (a * b) code

debug : String -> Computer -> Computer
debug str pc =
    { pc | outputs = (pc.outputs ++ str ++ "\n") }

execute : Computer -> Computer
execute model =
    let
        fullcode = load model.code (model.pc) Immediate
        opcode = getOpcode fullcode
        val1 = load model.code (model.pc + 1) (getMode fullcode 1)
        val2 = load model.code (model.pc + 2) (getMode fullcode 2)
        val3 = load model.code (model.pc + 3) (getMode fullcode 3)
    in
        case opcode of
            1 ->
                let
                    pos3 = load model.code (model.pc + 3) Immediate
                    newCode = add model.code val1 val2 pos3
                    newPC = model.pc + 4
                in
                    { model | code = newCode, pc = newPC } 
                        |> debug "add"
                        |> execute
            2 ->
                let
                    pos3 = load model.code (model.pc + 3) Immediate
                    newCode = mult model.code val1 val2 pos3
                    newPC = model.pc + 4
                in
                    { model | code = newCode, pc = newPC } 
                        |> debug "mul"
                        |> execute
            3 ->
                let
                    pos1 = load model.code (model.pc + 1) Immediate
                    newVal = List.head model.inputs
                    newInputs = List.drop 1 model.inputs
                    newCode = case newVal of
                        Just a -> Array.set pos1 a model.code
                        Nothing -> model.code
                    newPC = model.pc + 2
                in
                    { model | code = newCode, pc = newPC, inputs = newInputs }
                        |> debug ("inp" ++ (Maybe.withDefault 0 newVal |> String.fromInt))
                        |> execute
            4 ->
                let
                    newOutput = model.outputs ++ (String.fromInt val1) ++ "\n"
                    newPC = model.pc + 2
                in
                    { model | pc = newPC, outputs = newOutput }
                        |> debug "out"
                        |> execute
            5 ->
                let 
                    newModel = case val1 of
                        0 -> { model | pc = model.pc + 3 }
                        _ -> { model | pc = val2 }
                in
                    newModel
                    |> debug "jnz"
                    |> execute
            6 ->
                let
                    newModel = case val1 of
                        0 -> { model | pc = val2 }
                        _ -> { model | pc = model.pc + 3 }
                in
                    newModel
                    |> debug "jez"
                    |> execute
            7 ->
                let
                    out = if val1 < val2 then 1 else 0
                    pos3 = load model.code (model.pc + 3) Immediate
                    newCode = Array.set pos3 out model.code
                    newPC = model.pc + 4
                in
                    { model | code = newCode, pc = newPC }
                        |> debug "lti"
                        |> execute
            8 ->
                let
                    out = if val1 == val2 then 1 else 0
                    pos3 = load model.code (model.pc + 3) Immediate
                    newCode = Array.set pos3 out model.code
                    newPC = model.pc + 4
                in
                    { model | code = newCode, pc = newPC }
                        |> debug "eqi"
                        |> execute
            99 ->
                model |> debug "halt"
            _ ->
                model |> debug "error!"

stringify : Computer -> String
stringify pc =
    Array.indexedMap (\i x -> (String.fromInt x ++ (if i == pc.pc then "*" else ""))) pc.code
        |> Array.foldl (\i c -> c ++ "\t" ++ i) ""

