module Maze exposing (..)

import Array exposing (Array)
import Random
import Random.List


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


type alias Position =
    { row : Int
    , col : Int
    }


startingPosition : Position
startingPosition =
    Position 0 0


type alias BacktrackStack =
    List Position


type alias DirectionsGenerator =
    Random.Generator (List Direction)


directionsGenerator : Random.Generator (List Direction)
directionsGenerator =
    Random.List.shuffle directions


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
    if newPos.row >= 0 && newPos.row < maze.height && newPos.col >= 0 && newPos.col < maze.height then
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
