module Main exposing (..)

import Browser
import Html exposing (Html)
import Maze exposing (BacktrackStack, Direction, Maze, viewFields)
import Random
import Search exposing (viewPaths)
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)


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
    | FlipWall Maze.Position Direction


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
            Maze.Position (maze.height // 2) (maze.width // 2)
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
                        (Maze.Position 0 0)
                        (Maze.Position
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
        (viewFields FlipWall maze ++ viewPaths paths)
