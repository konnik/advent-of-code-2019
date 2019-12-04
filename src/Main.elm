module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Day1
import Day2
import Dict exposing (Dict)
import Element exposing (Element, column, el, padding, row, spacing, text)
import Http
import Input
import Types exposing (Solution)
import Url


solutions : Dict Int Solution
solutions =
    Dict.fromList
        [ ( 1, Day1.solution )
        , ( 2, Day2.solution )
        ]


type alias Model =
    { input : String
    , day : Int
    , answer1 : String
    , answer2 : String
    }


type Msg
    = NoOp
    | InputLoaded (Result Http.Error String)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    let
        day =
            2
    in
    ( { input = ""
      , day = day
      , answer1 = ""
      , answer2 = ""
      }
    , Input.load day InputLoaded
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputLoaded (Ok input) ->
            let
                ( a1, a2 ) =
                    solveDay model.day input
            in
            ( { model
                | input = input
                , answer1 = a1
                , answer2 = a2
              }
            , Cmd.none
            )

        InputLoaded (Err _) ->
            ( { model | input = "Could not load input for day " ++ String.fromInt model.day }, Cmd.none )


solveDay : Int -> String -> ( String, String )
solveDay day input =
    Dict.get day solutions
        |> Maybe.andThen
            (\( part1, part2 ) ->
                Just ( part1 input, part2 input )
            )
        |> Maybe.withDefault ( "", "" )


view : Model -> Browser.Document Msg
view model =
    { title = "AoE 2019"
    , body = [ Element.layout [ padding 20 ] (mainView model) ]
    }


mainView : Model -> Element Msg
mainView model =
    column [ spacing 20 ]
        [ text <| "Solution for day " ++ String.fromInt model.day
        , text <| "Part 1: " ++ model.answer1
        , text <| "Part 2: " ++ model.answer2
        , text "Input: "
        , text model.input
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }
