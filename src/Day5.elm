module Day5 exposing (solution)

import Dict exposing (Dict)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    loadProgram input
        |> init
        |> run
        |> lastOutput


part2 : Solver
part2 input =
    "not implemented"


lastOutput : Computer -> String
lastOutput comp =
    comp.output
        |> List.head
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "error: the program did not produce any output"



-- Intcode Computer


type alias Computer =
    { pos : Int
    , mem : Memory
    , input : Int
    , output : List Int
    }


type Arg
    = Position Int
    | Immediate Int


type alias Memory =
    Dict.Dict Int Int


type Opcode
    = Add Arg Arg Int
    | Mult Arg Arg Int
    | Input Int
    | Output Arg
    | Halt
    | Unknown


init : Memory -> Computer
init mem =
    { pos = 0, input = 1, output = [], mem = mem }


loadProgram : String -> Memory
loadProgram input =
    input
        |> String.split ","
        |> List.map (String.toInt >> Maybe.withDefault 0)
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


get : Int -> Computer -> Int
get pos comp =
    Dict.get pos comp.mem |> Maybe.withDefault -1


set : Int -> Int -> Computer -> Computer
set pos value comp =
    { comp | mem = Dict.insert pos value comp.mem }


nextOp : Computer -> Opcode
nextOp comp =
    let
        opcode =
            get comp.pos comp

        mode n =
            (opcode // 100) // 10 ^ n |> modBy 10

        param n =
            get (comp.pos + n + 1) comp

        arg : Int -> Arg
        arg n =
            if mode n == 1 then
                Immediate (param n)

            else
                Position (param n)
    in
    case opcode |> modBy 100 of
        1 ->
            Add (arg 0) (arg 1) (param 2)

        2 ->
            Mult (arg 0) (arg 1) (param 2)

        3 ->
            Input (param 0)

        4 ->
            Output (arg 0)

        99 ->
            Halt

        _ ->
            Unknown


incPos : Int -> Computer -> Computer
incPos inc comp =
    { comp | pos = comp.pos + inc }


output : Int -> Computer -> Computer
output value comp =
    { comp | output = value :: comp.output }


run : Computer -> Computer
run comp =
    let
        value : Arg -> Int
        value arg =
            case arg of
                Position p ->
                    get p comp

                Immediate v ->
                    v
    in
    case nextOp comp of
        Add a b dst ->
            comp |> set dst (value a + value b) |> incPos 4 |> run

        Mult a b dst ->
            comp |> set dst (value a * value b) |> incPos 4 |> run

        Input dst ->
            comp |> set dst comp.input |> incPos 2 |> run

        Output a ->
            comp |> output (value a) |> incPos 2 |> run

        Halt ->
            comp

        Unknown ->
            comp


runWithResult : Computer -> Int
runWithResult comp =
    comp
        |> run
        |> get 0
