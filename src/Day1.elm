module Day1 exposing (solution)

import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> String.lines
        |> List.map String.toInt
        |> List.map (Maybe.withDefault 0)
        |> List.map requiredFuel
        |> List.sum
        |> String.fromInt


requiredFuel : Int -> Int
requiredFuel mass =
    mass // 3 - 2


part2 : Solver
part2 input =
    input
        |> String.lines
        |> List.map String.toInt
        |> List.map (Maybe.withDefault 0)
        |> List.map requiredFuel2
        |> List.sum
        |> String.fromInt


requiredFuel2 : Int -> Int
requiredFuel2 mass =
    let
        fuel =
            mass // 3 - 2
    in
    if fuel <= 0 then
        0

    else
        fuel + requiredFuel2 fuel
