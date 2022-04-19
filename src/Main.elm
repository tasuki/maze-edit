module Main exposing (..)

import Browser
import Html exposing (Html)
import Maze exposing (BacktrackStack, Direction, Maze, viewFields)
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
    let
        maze =
            Maze.init 10 20

        startingPosition =
            Maze.Position (maze.height // 2) (maze.width // 2)
    in
    ( Model maze, expandMaze [ startingPosition ] )


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
        (viewFields maze)
