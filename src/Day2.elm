module Day2 exposing (solution)

import Dict
import Types exposing (Solution, Solver)


type alias Program =
    Dict.Dict Int Int


type Opcode
    = Add Int Int Int
    | Mult Int Int Int
    | Halt
    | Unknown


loadProgram : String -> Program
loadProgram input =
    input
        |> String.split ","
        |> List.map (String.toInt >> Maybe.withDefault 0)
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


get : Int -> Program -> Int
get pos prog =
    Dict.get pos prog |> Maybe.withDefault -1


set : Int -> Int -> Program -> Program
set pos value prog =
    Dict.insert pos value prog


opcodeAt : Int -> Program -> Opcode
opcodeAt pos program =
    let
        quad =
            [ get pos program
            , get (pos + 1) program
            , get (pos + 2) program
            , get (pos + 3) program
            ]
    in
    case quad of
        [ 1, a, b, c ] ->
            Add a b c

        [ 2, a, b, c ] ->
            Mult a b c

        [ 99, _, _, _ ] ->
            Halt

        _ ->
            Unknown


run : Int -> Program -> Program
run pos prog =
    case opcodeAt pos prog of
        Add a b c ->
            run (pos + 4) (set c (get a prog + get b prog) prog)

        Mult a b c ->
            run (pos + 4) (set c (get a prog * get b prog) prog)

        Halt ->
            prog

        Unknown ->
            prog


runWithParams : Int -> Int -> Program -> Int
runWithParams p1 p2 prog =
    prog
        |> set 1 p1
        |> set 2 p2
        |> run 0
        |> get 0


part1 : Solver
part1 input =
    input
        |> loadProgram
        |> runWithParams 12 2
        |> String.fromInt


findNounVerbPair : Program -> List ( Int, Int ) -> ( Int, Int )
findNounVerbPair prog pairs =
    case pairs of
        [] ->
            ( -1, -1 )

        ( noun, verb ) :: rest ->
            if runWithParams noun verb prog == 19690720 then
                ( noun, verb )

            else
                findNounVerbPair prog rest


part2 : Solver
part2 input =
    let
        prog =
            loadProgram input

        range =
            List.range 0 99

        pairs =
            List.concatMap (\a -> List.map (Tuple.pair a) range) range

        ( noun, verb ) =
            findNounVerbPair prog pairs

        result =
            100 * noun + verb
    in
    String.fromInt result


solution : Solution
solution =
    ( part1, part2 )
