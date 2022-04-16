module Main exposing (..)

import Browser
import Html exposing (Html)
import Maze exposing (BacktrackStack, Maze)
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
    = ExpandMaze BacktrackStack (List (List Maze.Direction))


type alias Model =
    { maze : Maze
    }


expandMazeCmd : Maze.Maze -> BacktrackStack -> Cmd Msg
expandMazeCmd maze bts =
    Random.generate (ExpandMaze bts) (Maze.directionGenerator (maze.width * maze.height * 2))


init : () -> ( Model, Cmd Msg )
init _ =
    let
        maze =
            Maze.init 20 10
    in
    ( Model maze
    , expandMazeCmd maze [ Maze.startingPosition ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExpandMaze stack directions ->
            ( { model | maze = Maze.expandMaze directions stack model.maze }
            , Cmd.none
            )


subscriptions : model -> Sub msg
subscriptions m =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Title goes here"
    , body = [ viewMaze model.maze ]
    }


viewMaze : Maze.Maze -> Html msg
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
        (Maze.viewLines maze)
