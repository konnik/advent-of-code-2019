module Day7 exposing (perm, solution)

import Dict exposing (Dict)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    let
        initialComputer =
            computer |> load input
    in
    perm [ 0, 1, 2, 3, 4 ]
        |> List.map (runAmplifier 0 initialComputer)
        |> List.maximum
        |> Maybe.withDefault -1
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"


type alias PhaseSetting =
    List Int


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


runAmplifier : Int -> Computer -> PhaseSetting -> Int
runAmplifier input comp phases =
    case phases of
        [] ->
            input

        phase :: restPhases ->
            runAmplifier (run [ phase, input ] comp) comp restPhases



-- Intcode Computer


type alias Computer =
    { pos : Int
    , mem : Memory
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
    , mem = Dict.empty
    }


run : List Int -> Computer -> Int
run inputs comp =
    let
        argValue : Arg -> Int
        argValue arg =
            case arg of
                Position p ->
                    read p comp

                Immediate v ->
                    v
    in
    case nextOp comp of
        Add a b dst ->
            comp |> write dst (argValue a + argValue b) |> incPos 4 |> run inputs

        Mult a b dst ->
            comp |> write dst (argValue a * argValue b) |> incPos 4 |> run inputs

        Input dst ->
            case inputs of
                input :: rest ->
                    comp |> write dst input |> incPos 2 |> run rest

                _ ->
                    -9999

        Output output ->
            argValue output

        JumpIfTrue a dst ->
            if argValue a /= 0 then
                comp |> setPos (argValue dst) |> run inputs

            else
                comp |> incPos 3 |> run inputs

        JumpIfFalse a dst ->
            if argValue a == 0 then
                comp |> setPos (argValue dst) |> run inputs

            else
                comp |> incPos 3 |> run inputs

        LessThan a b dst ->
            if argValue a < argValue b then
                comp |> write dst 1 |> incPos 4 |> run inputs

            else
                comp |> write dst 0 |> incPos 4 |> run inputs

        Equals a b dst ->
            if argValue a == argValue b then
                comp |> write dst 1 |> incPos 4 |> run inputs

            else
                comp |> write dst 0 |> incPos 4 |> run inputs

        Halt ->
            List.head inputs |> Maybe.withDefault -77

        Unknown ->
            -999


read : Address -> Computer -> Int
read (Address pos) comp =
    Dict.get pos comp.mem |> Maybe.withDefault -1


write : Address -> Int -> Computer -> Computer
write (Address pos) value comp =
    { comp | mem = Dict.insert pos value comp.mem }


incPos : Int -> Computer -> Computer
incPos inc comp =
    { comp | pos = comp.pos + inc }


setPos : Int -> Computer -> Computer
setPos pos comp =
    { comp | pos = pos }


nextOp : Computer -> Opcode
nextOp comp =
    let
        opcode =
            read (Address comp.pos) comp

        mode n =
            (opcode // 100) // 10 ^ n |> modBy 10

        param n =
            read (Address (comp.pos + n + 1)) comp

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
