module Day3 exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Types exposing (Solution, Solver)


type alias Wire =
    List Move


type alias Move =
    ( Direction, Int )


type Direction
    = Up
    | Down
    | Left
    | Right
    | Unknown


type alias Pos =
    ( Int, Int )


part1 : Solver
part1 input =
    parseInput input
        |> traceWirePositions
        |> findIntersections
        |> smallestManhattanDistance
        |> String.fromInt


part2 : Solver
part2 input =
    parseInput input
        |> traceWirePositionsWithSteps
        |> findIntersectionsWithSteps
        |> fewestSteps
        |> String.fromInt


parseInput : String -> ( Wire, Wire )
parseInput input =
    case String.lines input of
        [ a, b ] ->
            ( parseLine a, parseLine b )

        _ ->
            ( [], [] )


parseLine : String -> Wire
parseLine line =
    line
        |> String.split ","
        |> List.map parseMove


parseMove : String -> Move
parseMove token =
    let
        steps =
            String.dropLeft 1 token |> String.toInt |> Maybe.withDefault 0
    in
    case String.left 1 token of
        "U" ->
            ( Up, steps )

        "D" ->
            ( Down, steps )

        "L" ->
            ( Left, steps )

        "R" ->
            ( Right, steps )

        _ ->
            ( Unknown, 0 )


traceMove : Move -> ( Pos, Set Pos ) -> ( Pos, Set Pos )
traceMove ( dir, steps ) ( pos, path ) =
    let
        nextPos =
            takeOneStep dir pos

        nextPath =
            Set.insert nextPos path
    in
    if steps == 1 then
        ( nextPos, nextPath )

    else
        traceMove ( dir, steps - 1 ) ( nextPos, nextPath )


traceWire : Wire -> Set Pos
traceWire moves =
    let
        ( _, path ) =
            List.foldl traceMove ( ( 0, 0 ), Set.empty ) moves
    in
    path


countSteps : List Pos -> Dict Pos Int
countSteps positions =
    positions
        |> List.indexedMap (\i pos -> ( pos, i ))
        |> Dict.fromList


manhattan : Pos -> Int
manhattan ( x, y ) =
    abs x + abs y


findIntersections : ( Set Pos, Set Pos ) -> Set Pos
findIntersections ( a, b ) =
    Set.intersect a b


smallestManhattanDistance : Set Pos -> Int
smallestManhattanDistance positions =
    positions
        |> Set.map manhattan
        |> Set.filter (\x -> x > 0)
        |> Set.toList
        |> List.sort
        |> List.head
        |> Maybe.withDefault -99


traceWirePositions : ( Wire, Wire ) -> ( Set Pos, Set Pos )
traceWirePositions ( a, b ) =
    ( traceWire a, traceWire b )


traceWirePositionsWithSteps : ( Wire, Wire ) -> ( Dict Pos Int, Dict Pos Int )
traceWirePositionsWithSteps ( a, b ) =
    ( traceWireWithSteps a, traceWireWithSteps b )


traceWireWithSteps : Wire -> Dict Pos Int
traceWireWithSteps moves =
    let
        ( _, _, path ) =
            List.foldl traceMoveWithSteps ( 0, ( 0, 0 ), Dict.empty ) moves
    in
    path


takeOneStep : Direction -> Pos -> Pos
takeOneStep dir ( x, y ) =
    case dir of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )

        _ ->
            ( x, y )


traceMoveWithSteps : Move -> ( Int, Pos, Dict Pos Int ) -> ( Int, Pos, Dict Pos Int )
traceMoveWithSteps ( dir, steps ) ( step, pos, path ) =
    if steps == 0 then
        ( step, pos, path )

    else
        let
            nextStep =
                step + 1

            nextPos =
                takeOneStep dir pos

            nextPath =
                if Dict.member nextPos path then
                    path

                else
                    Dict.insert nextPos nextStep path
        in
        traceMoveWithSteps ( dir, steps - 1 ) ( nextStep, nextPos, nextPath )


findIntersectionsWithSteps : ( Dict Pos Int, Dict Pos Int ) -> Dict Pos Int
findIntersectionsWithSteps ( a, b ) =
    let
        ignore : Pos -> Int -> Dict Pos Int -> Dict Pos Int
        ignore pos d result =
            result

        sumDistance : Pos -> Int -> Int -> Dict Pos Int -> Dict Pos Int
        sumDistance pos distA distB result =
            Dict.insert pos (distA + distB) result
    in
    Dict.merge ignore sumDistance ignore a b Dict.empty


fewestSteps : Dict Pos Int -> Int
fewestSteps steps =
    Dict.values steps
        |> List.filter (\x -> x > 0)
        |> List.sort
        |> List.head
        |> Maybe.withDefault -99


solution : Solution
solution =
    ( part1, part2 )
