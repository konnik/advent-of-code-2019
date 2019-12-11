module Day4 exposing (solution)

import Types exposing (Solution, Solver)


type Num
    = Num Int Int Int Int Int Int


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> parseInput
        |> toRange
        |> List.map toNum
        |> List.filter (\n -> isIncreasing n && hasTwin n)
        |> List.length
        |> String.fromInt


part2 : Solver
part2 input =
    input
        |> parseInput
        |> toRange
        |> List.map toNum
        |> List.filter (\n -> isIncreasing n && hasStrictTwin n)
        |> List.length
        |> String.fromInt



-- FILTER


isIncreasing : Num -> Bool
isIncreasing (Num a b c d e f) =
    a <= b && b <= c && c <= d && d <= e && e <= f


hasTwin : Num -> Bool
hasTwin (Num a b c d e f) =
    a == b || b == c || c == d || d == e || e == f


hasStrictTwin : Num -> Bool
hasStrictTwin (Num a b c d e f) =
    (a == b && b /= c)
        || (b == c && b /= a && c /= d)
        || (c == d && c /= b && d /= e)
        || (d == e && d /= c && e /= f)
        || (e == f && e /= d)


toNum : Int -> Num
toNum n =
    let
        a =
            (n // 100000) |> modBy 10

        b =
            (n // 10000) |> modBy 10

        c =
            (n // 1000) |> modBy 10

        d =
            (n // 100) |> modBy 10

        e =
            (n // 10) |> modBy 10

        f =
            (n // 1) |> modBy 10
    in
    Num a b c d e f



-- PARSE INPUT


parseInput : String -> ( Int, Int )
parseInput input =
    let
        ints =
            input
                |> String.split "-"
                |> List.map String.toInt
    in
    case ints of
        [ Just a, Just b ] ->
            ( a, b )

        _ ->
            ( 0, 0 )


toRange : ( Int, Int ) -> List Int
toRange ( a, b ) =
    List.range a b
