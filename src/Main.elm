module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Maze exposing (BacktrackStack, Direction, Maze, Position, viewFields)
import Random
import Search
import Secrets
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
    | FlipKey Position
    | Drag Draggable
    | Drop Position


type Draggable
    = Start
    | End


type alias Model =
    { maze : Maze
    , paths : List Search.Path
    , start : Position
    , end : Position
    , dragged : Maybe Draggable
    , keys : List Position
    }


expandMaze : BacktrackStack -> Cmd Msg
expandMaze bts =
    Random.generate (ExpandMaze bts) Maze.directionsGenerator


firstPath : List Search.Path -> Search.Path
firstPath paths =
    List.head paths |> Maybe.withDefault []


init : () -> ( Model, Cmd Msg )
init _ =
    let
        maze =
            -- Maze.init 10 20
            Maze.init 50 26

        startPos =
            Position 0 0

        endPos =
            Position (maze.height - 1) (maze.width - 1)

        expandStart =
            Position (maze.height // 2) (maze.width // 2)
    in
    ( Model maze [] startPos endPos Nothing []
    , expandMaze [ expandStart ]
    )


reactToDrop : Position -> Model -> Model
reactToDrop pos model =
    case model.dragged of
        Just Start ->
            { model | start = pos }

        Just End ->
            { model | end = pos }

        _ ->
            model


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
                    Search.findPaths model.maze model.start model.end
              }
            , Cmd.none
            )

        FlipWall pos dir ->
            update FindPaths
                { model | maze = Maze.flipWall pos dir model.maze }

        FlipKey pos ->
            ( { model | keys = Secrets.flipKey pos model.keys }, Cmd.none )

        Drag draggable ->
            ( { model | dragged = Just draggable }, Cmd.none )

        Drop position ->
            update FindPaths (reactToDrop position model)


subscriptions : model -> Sub msg
subscriptions m =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "A Maze-ing Editor"
    , body =
        [ viewMaze model
        , Html.div []
            [ Html.text <|
                String.fromList <|
                    Secrets.keyLetters (firstPath model.paths) model.keys
            ]
        , Html.div []
            [ Html.span
                [ HA.draggable "true", HE.on "dragstart" (JD.succeed <| Drag Start) ]
                [ Html.text "drag to place Start" ]
            , Html.text " ... "
            , Html.span
                [ HA.draggable "true", HE.on "dragstart" (JD.succeed <| Drag End) ]
                [ Html.text "drag to place End" ]
            ]
        ]
    }


viewMaze : Model -> Html Msg
viewMaze model =
    Svg.svg
        [ SA.viewBox
            ("-1 -1 "
                ++ String.fromInt (model.maze.width + 2)
                ++ " "
                ++ String.fromInt (model.maze.height + 2)
            )
        , SA.width "700"
        , SA.height "700"
        ]
        (Maze.viewFields FlipWall FlipKey Drop model.maze
            ++ Search.viewPaths model.paths
            ++ Search.viewStartAndEnd model.start model.end
            ++ Secrets.viewSumLetters model.maze (firstPath model.paths)
            ++ Secrets.viewKeys model.keys
        )
