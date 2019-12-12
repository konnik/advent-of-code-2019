module Day5 exposing (solution)

import Dict
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    computer
        |> load input
        |> withInput 1
        |> run
        |> output


part2 : Solver
part2 input =
    computer
        |> load input
        |> withInput 5
        |> run
        |> output


output : Computer -> String
output comp =
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


type Opcode
    = Add Arg Arg Address
    | Mult Arg Arg Address
    | Input Address
    | Output Arg
    | JumpIfTrue Arg Arg
    | JumpIfFalse Arg Arg
    | LessThan Arg Arg Address
    | Equals Arg Arg Address
    | Halt
    | Unknown


type Address
    = Address Int


type Arg
    = Position Address
    | Immediate Int


type alias Memory =
    Dict.Dict Int Int


computer : Computer
computer =
    { pos = 0, input = 0, output = [], mem = Dict.empty }


withInput : Int -> Computer -> Computer
withInput id comp =
    { comp | input = id }


load : String -> Computer -> Computer
load program comp =
    let
        mem =
            program
                |> String.split ","
                |> List.map (String.toInt >> Maybe.withDefault 0)
                |> List.indexedMap Tuple.pair
                |> Dict.fromList
    in
    { comp | mem = mem }


get : Address -> Computer -> Int
get (Address pos) comp =
    Dict.get pos comp.mem |> Maybe.withDefault -1


set : Address -> Int -> Computer -> Computer
set (Address pos) value comp =
    { comp | mem = Dict.insert pos value comp.mem }


nextOp : Computer -> Opcode
nextOp comp =
    let
        opcode =
            get (Address comp.pos) comp

        mode n =
            (opcode // 100) // 10 ^ n |> modBy 10

        param n =
            get (Address (comp.pos + n + 1)) comp

        arg : Int -> Arg
        arg n =
            if mode n == 1 then
                Immediate (param n)

            else
                Position (Address (param n))
    in
    case opcode |> modBy 100 of
        1 ->
            Add (arg 0) (arg 1) (Address (param 2))

        2 ->
            Mult (arg 0) (arg 1) (Address (param 2))

        3 ->
            Input (Address (param 0))

        4 ->
            Output (arg 0)

        5 ->
            JumpIfTrue (arg 0) (arg 1)

        6 ->
            JumpIfFalse (arg 0) (arg 1)

        7 ->
            LessThan (arg 0) (arg 1) (Address (param 2))

        8 ->
            Equals (arg 0) (arg 1) (Address (param 2))

        99 ->
            Halt

        _ ->
            Unknown


incPos : Int -> Computer -> Computer
incPos inc comp =
    { comp | pos = comp.pos + inc }


setPos : Int -> Computer -> Computer
setPos pos comp =
    { comp | pos = pos }


setOutput : Int -> Computer -> Computer
setOutput value comp =
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
            comp |> setOutput (value a) |> incPos 2 |> run

        JumpIfTrue a dst ->
            if value a /= 0 then
                comp |> setPos (value dst) |> run

            else
                comp |> incPos 3 |> run

        JumpIfFalse a dst ->
            if value a == 0 then
                comp |> setPos (value dst) |> run

            else
                comp |> incPos 3 |> run

        LessThan a b dst ->
            if value a < value b then
                comp |> set dst 1 |> incPos 4 |> run

            else
                comp |> set dst 0 |> incPos 4 |> run

        Equals a b dst ->
            if value a == value b then
                comp |> set dst 1 |> incPos 4 |> run

            else
                comp |> set dst 0 |> incPos 4 |> run

        Halt ->
            comp

        Unknown ->
            comp
