module Day7 exposing (perm, phaseSetting, solution)

import Dict exposing (Dict)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    let
        amp =
            computer |> load input
    in
    perm [ 0, 1, 2, 3, 4 ]
        |> List.map phaseSetting
        |> List.map (runAmplifier amp)
        |> List.filterMap identity
        |> List.maximum
        |> Maybe.withDefault -1
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"


type PhaseSetting
    = PhaseSetting Int Int Int Int Int


perm : List a -> List (List a)
perm input =
    case input of
        [] ->
            [ [] ]

        _ ->
            let
                delete : a -> List a -> List a
                delete a =
                    List.filter (\b -> a /= b)

                combine : a -> List a -> List a
                combine y ys =
                    y :: ys
            in
            List.concatMap (\y -> List.map (combine y) (perm (delete y input))) input


phaseSetting : List Int -> PhaseSetting
phaseSetting settings =
    case settings of
        [ a, b, c, d, e ] ->
            PhaseSetting a b c d e

        _ ->
            PhaseSetting 0 0 0 0 0


runAmplifier : Computer -> PhaseSetting -> Maybe Int
runAmplifier prog (PhaseSetting a b c d e) =
    let
        progA =
            prog |> setInput (Just a) |> step

        progB =
            prog |> setInput (Just b) |> step

        progC =
            prog |> setInput (Just c) |> step

        progD =
            prog |> setInput (Just d) |> step

        progE =
            prog |> setInput (Just e) |> step
    in
    Just 0
        |> runWithInput progA
        |> runWithInput progB
        |> runWithInput progC
        |> runWithInput progD
        |> runWithInput progE



-- Intcode Computer


type alias Computer =
    { pos : Int
    , mem : Memory
    , input : Maybe Int
    , output : Maybe Int
    , halted : Bool
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
    { pos = 0
    , input = Nothing
    , output = Nothing
    , mem = Dict.empty
    , halted = False
    }


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


runWithInput : Computer -> Maybe Int -> Maybe Int
runWithInput comp inputVal =
    comp |> setInput inputVal |> run |> .output


run : Computer -> Computer
run comp =
    if comp.halted then
        comp

    else
        step comp |> run


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
    { comp | output = Just value }


setInput : Maybe Int -> Computer -> Computer
setInput value comp =
    { comp | input = value }


clearInput : Computer -> Computer
clearInput comp =
    { comp | input = Nothing }


clearOutput : Computer -> Computer
clearOutput comp =
    { comp | output = Nothing }


halt : Computer -> Computer
halt comp =
    { comp | halted = True }


step : Computer -> Computer
step comp =
    let
        argValue : Arg -> Int
        argValue arg =
            case arg of
                Position p ->
                    get p comp

                Immediate v ->
                    v
    in
    case nextOp comp of
        Add a b dst ->
            comp |> set dst (argValue a + argValue b) |> incPos 4

        Mult a b dst ->
            comp |> set dst (argValue a * argValue b) |> incPos 4

        Input dst ->
            case comp.input of
                Just inputValue ->
                    comp |> set dst inputValue |> clearInput |> incPos 2

                Nothing ->
                    comp

        Output a ->
            comp |> setOutput (argValue a) |> incPos 2

        JumpIfTrue a dst ->
            if argValue a /= 0 then
                comp |> setPos (argValue dst)

            else
                comp |> incPos 3

        JumpIfFalse a dst ->
            if argValue a == 0 then
                comp |> setPos (argValue dst)

            else
                comp |> incPos 3

        LessThan a b dst ->
            if argValue a < argValue b then
                comp |> set dst 1 |> incPos 4

            else
                comp |> set dst 0 |> incPos 4

        Equals a b dst ->
            if argValue a == argValue b then
                comp |> set dst 1 |> incPos 4

            else
                comp |> set dst 0 |> incPos 4

        Halt ->
            comp |> halt

        Unknown ->
            comp |> halt
