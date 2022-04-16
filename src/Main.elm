module Main exposing (..)

import Browser
import Html exposing (Html)
import Maze exposing (BacktrackStack, Direction, Maze, viewLines)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = ExpandMaze BacktrackStack (List Direction)


type alias Model =
    { maze : Maze
    }


expandMaze : BacktrackStack -> Cmd Msg
expandMaze bts =
    Random.generate (ExpandMaze bts) Maze.directionsGenerator


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Maze.init 20 10), expandMaze [ Maze.startingPosition ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExpandMaze stack directions ->
            let
                ( newMaze, parameters ) =
                    Maze.expand stack directions model.maze

                command =
                    case parameters of
                        Just backtrackStack ->
                            expandMaze backtrackStack

                        _ ->
                            Cmd.none
            in
            ( { model | maze = newMaze }, command )


subscriptions : model -> Sub msg
subscriptions m =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Title goes here"
    , body = [ viewMaze model.maze ]
    }


viewMaze : Maze -> Html msg
viewMaze maze =
    svg
        [ viewBox
            ("-1 -1 "
                ++ String.fromInt (maze.width + 2)
                ++ " "
                ++ String.fromInt (maze.height + 2)
            )
        , width "700"
        , height "700"
        ]
        (viewLines maze)
