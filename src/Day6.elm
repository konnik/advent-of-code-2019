module Day6 exposing (solution)

import Dict exposing (Dict)
import Set
import Types exposing (Solution, Solver)


part1 : Solver
part1 input =
    parse input
        |> countOrbits
        |> String.fromInt


part2 : Solver
part2 input =
    parse input
        |> countSteps
        |> String.fromInt



-- types


type alias Orbit =
    ( Object, Object )


type alias Object =
    String



-- count


countSteps : Dict Object Object -> Int
countSteps orbitMap =
    let
        you =
            pathToCom "YOU" orbitMap |> Set.fromList

        san =
            pathToCom "SAN" orbitMap |> Set.fromList

        steps =
            Set.diff
                (Set.union san you)
                (Set.intersect san you)
    in
    steps
        |> Set.size


pathToCom : Object -> Dict Object Object -> List Object
pathToCom from orbitMap =
    let
        path : Object -> List Object -> List Object
        path curr objects =
            case Dict.get curr orbitMap of
                Just orbited ->
                    path orbited (orbited :: objects)

                Nothing ->
                    objects
    in
    path from []


countOrbits : Dict Object Object -> Int
countOrbits orbitMap =
    let
        count : Object -> Int
        count obj =
            case Dict.get obj orbitMap of
                Just orbiting ->
                    1 + count orbiting

                Nothing ->
                    0
    in
    Dict.keys orbitMap
        |> List.map count
        |> List.sum



-- parse


parse : String -> Dict Object Object
parse input =
    String.lines input
        |> List.map parseLine
        |> Dict.fromList


parseLine : String -> Orbit
parseLine line =
    case String.split ")" line of
        [ orbited, orbiting ] ->
            ( orbiting, orbited )

        _ ->
            ( "ERROR", "ERROR" )



-- export solution


solution : Solution
solution =
    ( part1, part2 )
