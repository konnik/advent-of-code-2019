module Day8 exposing (solution)

import Types exposing (Solution, Solver)


width =
    25


height =
    6


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    parseInput input
        |> List.sortBy (count 0)
        |> List.head
        |> Maybe.map (\l -> count 1 l * count 2 l)
        |> Debug.toString


part2 : Solver
part2 input =
    "not implemented"


type alias Layer =
    List Int


count : Int -> Layer -> Int
count digit pixels =
    pixels
        |> List.filter (\p -> p == digit)
        |> List.length



-- parse


parseInput : String -> List Layer
parseInput input =
    input
        |> String.toList
        |> List.map (String.fromChar >> String.toInt >> Maybe.withDefault -1)
        |> parseLayers


parseLayers : List Int -> List Layer
parseLayers pixels =
    case pixels of
        [] ->
            []

        _ ->
            List.take (25 * 6) pixels :: parseLayers (List.drop (25 * 6) pixels)
