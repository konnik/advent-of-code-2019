module Day3 exposing (..)

import Set exposing (Set)
import Types exposing (Solution, Solver)


type Direction
    = Up
    | Down
    | Left
    | Right
    | Unknown


type alias Move =
    ( Direction, Int )


type alias Pos =
    ( Int, Int )


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


parseLine : String -> List Move
parseLine line =
    line
        |> String.split ","
        |> List.map parseMove


move : Move -> List Pos -> List Pos
move ( dir, steps ) path =
    let
        ( x, y ) =
            List.head path |> Maybe.withDefault ( 0, 0 )

        nextPos =
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
    in
    if steps == 0 then
        path

    else
        move ( dir, steps - 1 ) (nextPos :: path)


trace : List Move -> List Pos
trace moves =
    List.foldl move [ ( 0, 0 ) ] moves


part1 : Solver
part1 input =
    input
        |> String.lines
        |> List.map parseLine
        |> List.map trace
        |> findIntersections
        |> List.map manhattan
        |> List.filter (\x -> x > 0)
        |> List.sort
        |> List.head
        |> Maybe.withDefault -99
        |> String.fromInt


splitLines : String -> ( String, String )
splitLines input =
    case String.lines input of
        [ a, b ] ->
            ( a, b )

        _ ->
            ( "", "" )


manhattan : Pos -> Int
manhattan ( x, y ) =
    abs x + abs y


findIntersections : List (List Pos) -> List Pos
findIntersections paths =
    let
        ( a, b ) =
            case paths of
                [ a_, b_ ] ->
                    ( a_, b_ )

                _ ->
                    ( [], [] )
    in
    Set.intersect (Set.fromList a) (Set.fromList b)
        |> Set.toList


zip : List (List a) -> List ( a, a )
zip lists =
    case lists of
        [ a, b ] ->
            List.map2 Tuple.pair a b

        _ ->
            []


part2 : Solver
part2 input =
    "not implemented"


solution : Solution
solution =
    ( part1, part2 )
