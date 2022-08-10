module Main exposing (..)

import Browser
import Html exposing (Html)
import Maze exposing (BacktrackStack, Direction, Maze, Position)
import Random
import Search
import Svg
import Svg.Attributes as SA


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = ExpandMaze BacktrackStack (List Direction)
    | FindPaths
    | FlipWall Position Direction


type alias Model =
    { maze : Maze
    , paths : List Search.Path
    }


expandMaze : BacktrackStack -> Cmd Msg
expandMaze bts =
    Random.generate (ExpandMaze bts) Maze.directionsGenerator


init : () -> ( Model, Cmd Msg )
init _ =
    let
        maze =
            -- Maze.init 50 26
            Maze.init 10 20

        startingPosition =
            Position (maze.height // 2) (maze.width // 2)
    in
    ( Model maze [], expandMaze [ startingPosition ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExpandMaze stack directions ->
            let
                ( newMaze, parameters ) =
                    Maze.expand stack directions model.maze
            in
            case parameters of
                Just backtrackStack ->
                    ( { model | maze = newMaze }, expandMaze backtrackStack )

                _ ->
                    update FindPaths { model | maze = newMaze }

        FindPaths ->
            ( { model
                | paths =
                    Search.findPaths model.maze
                        (Position 0 0)
                        (Position
                            (model.maze.height - 1)
                            (model.maze.width - 1)
                        )
              }
            , Cmd.none
            )

        FlipWall pos dir ->
            update FindPaths
                { model | maze = Maze.flipWall pos dir model.maze }


subscriptions : model -> Sub msg
subscriptions m =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "A Maze-ing Editor"
    , body = [ viewMaze model.maze model.paths ]
    }


viewMaze : Maze -> List Search.Path -> Html Msg
viewMaze maze paths =
    Svg.svg
        [ SA.viewBox
            ("-1 -1 "
                ++ String.fromInt (maze.width + 2)
                ++ " "
                ++ String.fromInt (maze.height + 2)
            )
        , SA.width "700"
        , SA.height "700"
        ]
        (Maze.viewFields FlipWall maze ++ Search.viewPaths paths)
