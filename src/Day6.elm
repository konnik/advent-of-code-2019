module Day6 exposing (solution)

import Dict exposing (Dict)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    parse input
        |> Dict.fromList
        |> countOrbits
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"



-- types


type alias Orbit =
    ( Object, Object )


type alias Object =
    String



-- count


countOrbits : Dict Object Object -> Int
countOrbits orbitMap =
    let
        count : Object -> Int
        count obj =
            case Dict.get obj orbitMap of
                Just around ->
                    1 + count around

                Nothing ->
                    0
    in
    Dict.keys orbitMap
        |> List.map count
        |> List.sum



-- parse


parse : String -> List Orbit
parse input =
    String.lines input
        |> List.map parseLine


parseLine : String -> Orbit
parseLine line =
    case String.split ")" line of
        [ a, b ] ->
            ( b, a )

        _ ->
            ( "ERROR", "ERROR" )
