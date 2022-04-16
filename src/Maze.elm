module Maze exposing (..)

import Array exposing (Array)
import Random
import Random.List
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- Directions


type Direction
    = N
    | S
    | E
    | W


directions : List Direction
directions =
    [ N, S, E, W ]


opposite : Direction -> Direction
opposite dir =
    case dir of
        N ->
            S

        S ->
            N

        E ->
            W

        W ->
            E


invertDirections : List Direction -> List Direction
invertDirections dirs =
    List.filter (\dir -> List.member dir dirs == False) directions


directionGenerator : Int -> Random.Generator (List (List Direction))
directionGenerator size =
    Random.list size (Random.List.shuffle [ N, S, E, W ])



-- Position


type alias Position =
    { row : Int
    , col : Int
    }


startingPosition : Position
startingPosition =
    Position 0 0


type alias BacktrackStack =
    List Position



-- Maze


type alias Maze =
    { width : Int
    , height : Int
    , fields : Array (List Direction)
    }


init : Int -> Int -> Maze
init width height =
    Maze width height (Array.repeat (width * height) [])


move : Maze -> Position -> Direction -> Maybe Position
move maze pos dir =
    let
        newPos =
            case dir of
                N ->
                    { pos | row = pos.row - 1 }

                S ->
                    { pos | row = pos.row + 1 }

                E ->
                    { pos | col = pos.col + 1 }

                W ->
                    { pos | col = pos.col - 1 }
    in
    if newPos.row >= 0 && newPos.row < maze.height && newPos.col >= 0 && newPos.col < maze.width then
        Just newPos

    else
        Nothing


fieldIndex : Maze -> Position -> Int
fieldIndex maze pos =
    pos.col + pos.row * maze.width


getField : Maze -> Position -> List Direction
getField maze pos =
    Array.get (fieldIndex maze pos) maze.fields
        |> Maybe.withDefault []


positionFromIndex : Maze -> Int -> Position
positionFromIndex maze i =
    Position (i // maze.width) (modBy maze.width i)


visitNew : Maze -> Position -> Maybe Position
visitNew maze pos =
    if List.isEmpty (getField maze pos) then
        Just pos

    else
        Nothing


expandCell : Maze -> Position -> Direction -> Maybe Position
expandCell maze pos dir =
    move maze pos dir |> Maybe.andThen (visitNew maze)


addDirection : Position -> Direction -> Maze -> Maze
addDirection pos dir maze =
    { maze
        | fields =
            Array.set
                (fieldIndex maze pos)
                (dir :: getField maze pos)
                maze.fields
    }


tryDirections : Position -> BacktrackStack -> List Direction -> Maze -> ( Maze, Maybe BacktrackStack )
tryDirections pos backtrack dirs maze =
    case dirs of
        currentDir :: otherDirs ->
            case expandCell maze pos currentDir of
                Nothing ->
                    tryDirections pos backtrack otherDirs maze

                Just newPos ->
                    let
                        newMaze =
                            maze
                                |> addDirection pos currentDir
                                |> addDirection newPos (opposite currentDir)
                    in
                    ( newMaze, Just (newPos :: pos :: backtrack) )

        _ ->
            ( maze, Just backtrack )


expand : BacktrackStack -> List Direction -> Maze -> ( Maze, Maybe BacktrackStack )
expand stack dirs maze =
    case stack of
        pos :: backtrack ->
            tryDirections pos backtrack dirs maze

        _ ->
            ( maze, Nothing )


expandMaze : List (List Direction) -> BacktrackStack -> Maze -> Maze
expandMaze dirs stack maze =
    case dirs of
        dirsHead :: dirsTail ->
            let
                ( newMaze, parameters ) =
                    expand stack dirsHead maze
            in
            case parameters of
                Just backtrackStack ->
                    expandMaze dirsTail backtrackStack newMaze

                _ ->
                    maze

        _ ->
            maze



-- Views


viewWall : Position -> Direction -> Svg msg
viewWall position dir =
    let
        nil =
            0.0

        one =
            1.0

        ( ( ax, ay ), ( bx, by ) ) =
            case dir of
                N ->
                    ( ( nil, nil ), ( one, nil ) )

                S ->
                    ( ( nil, one ), ( one, one ) )

                E ->
                    ( ( one, nil ), ( one, one ) )

                W ->
                    ( ( nil, nil ), ( nil, one ) )
    in
    line
        [ x1 (String.fromFloat (ax + toFloat position.col))
        , y1 (String.fromFloat (ay + toFloat position.row))
        , x2 (String.fromFloat (bx + toFloat position.col))
        , y2 (String.fromFloat (by + toFloat position.row))
        , stroke "navy"
        , strokeWidth ".1"
        , strokeLinecap "round"
        ]
        []


viewField : Maze -> Int -> List Direction -> List (Svg msg)
viewField maze index dirs =
    List.map (viewWall (positionFromIndex maze index)) (invertDirections dirs)


viewLines : Maze -> List (Svg msg)
viewLines maze =
    Array.indexedMap (viewField maze) maze.fields
        |> Array.toList
        |> List.concat
