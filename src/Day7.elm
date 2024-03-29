module Day7 exposing (solution)

import Dict
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 =
    solve [ 0, 1, 2, 3, 4 ] runOnce


part2 : Solver
part2 =
    solve [ 5, 6, 7, 8, 9 ] runLoop


solve : List Int -> (Int -> List Computer -> Int) -> String -> String
solve phaseNumbers runFunc input =
    let
        program =
            computer |> load input
    in
    permutations phaseNumbers
        |> List.map (initAmplifiers program)
        |> List.map (runFunc 0)
        |> List.maximum
        |> Maybe.withDefault -1
        |> String.fromInt


initAmplifiers : Computer -> List Int -> List Computer
initAmplifiers program phases =
    List.map (\phase -> program |> setIn phase) phases


runLoop : Int -> List Computer -> Int
runLoop inputVal computers =
    if List.all .halted computers then
        inputVal

    else
        case computers of
            comp :: rest ->
                let
                    ( next, outVal ) =
                        comp |> setIn inputVal |> run |> getOut
                in
                runLoop outVal (rest ++ [ next ])

            _ ->
                -666


runOnce : Int -> List Computer -> Int
runOnce inputVal computers =
    case computers of
        [] ->
            inputVal

        comp :: rest ->
            let
                ( _, outVal ) =
                    comp |> setIn inputVal |> run |> getOut
            in
            runOnce outVal rest



-- Intcode Computer


type alias Computer =
    { pos : Int
    , mem : Memory
    , halted : Bool
    , input : List Int
    , output : List Int
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
    , halted = False
    , input = []
    , output = []
    }


run : Computer -> Computer
run comp =
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
        Input dst ->
            case comp.input of
                inputVal :: rest ->
                    { comp | input = rest } |> write dst inputVal |> incPos 2 |> run

                [] ->
                    comp

        Output a ->
            comp |> setOut (argValue a) |> incPos 2

        Add a b dst ->
            comp |> write dst (argValue a + argValue b) |> incPos 4 |> run

        Mult a b dst ->
            comp |> write dst (argValue a * argValue b) |> incPos 4 |> run

        JumpIfTrue a dst ->
            if argValue a /= 0 then
                comp |> setPos (argValue dst) |> run

            else
                comp |> incPos 3 |> run

        JumpIfFalse a dst ->
            if argValue a == 0 then
                comp |> setPos (argValue dst) |> run

            else
                comp |> incPos 3 |> run

        LessThan a b dst ->
            if argValue a < argValue b then
                comp |> write dst 1 |> incPos 4 |> run

            else
                comp |> write dst 0 |> incPos 4 |> run

        Equals a b dst ->
            if argValue a == argValue b then
                comp |> write dst 1 |> incPos 4 |> run

            else
                comp |> write dst 0 |> incPos 4 |> run

        Halt ->
            comp |> halt

        Unknown ->
            comp |> halt


setIn : Int -> Computer -> Computer
setIn value comp =
    { comp | input = comp.input ++ [ value ] }


setOut : Int -> Computer -> Computer
setOut value comp =
    { comp | output = value :: comp.output }


getOut : Computer -> ( Computer, Int )
getOut comp =
    case comp.output of
        outValue :: _ ->
            ( comp, outValue )

        _ ->
            ( comp, -77 )


halt : Computer -> Computer
halt comp =
    { comp | halted = True }


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



-- UTILS


permutations : List a -> List (List a)
permutations input =
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
            List.concatMap (\y -> List.map (combine y) (permutations (delete y input))) input
