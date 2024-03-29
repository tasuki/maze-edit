module Maze exposing
    ( BacktrackStack
    , Direction
    , Maze
    , Position
    , directionsGenerator
    , expand
    , fieldIndex
    , flipWall
    , getField
    , init
    , move
    , viewFields
    )

import Array exposing (Array)
import Json.Decode as JD
import Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE



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


directionsGenerator : Random.Generator (List Direction)
directionsGenerator =
    Random.List.shuffle directions



-- Position


type alias Position =
    { row : Int
    , col : Int
    }


type alias BacktrackStack =
    List Position



-- Maze


type alias Maze =
    { height : Int
    , width : Int
    , fields : Array (List Direction)
    }


init : Int -> Int -> Maze
init height width =
    Maze height width (Array.repeat (width * height) [])


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


flipDirection : Position -> Direction -> Maze -> Maze
flipDirection pos dir maze =
    let
        dirs =
            getField maze pos

        newDirs =
            if List.member dir dirs then
                List.filter (\d -> d /= dir) dirs

            else
                dir :: dirs
    in
    { maze
        | fields =
            Array.set
                (fieldIndex maze pos)
                newDirs
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
                                |> flipDirection pos currentDir
                                |> flipDirection newPos (opposite currentDir)
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


flipWall : Position -> Direction -> Maze -> Maze
flipWall pos dir maze =
    case move maze pos dir of
        Just otherPos ->
            flipDirection pos dir maze |> flipDirection otherPos (opposite dir)

        _ ->
            maze



-- Views


viewWall : (Position -> Direction -> msg) -> Position -> ( Bool, Direction ) -> Svg msg
viewWall flipAction position ( visible, dir ) =
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

        wallStroke =
            if visible then
                "navy"

            else
                "transparent"
    in
    Svg.line
        [ SA.x1 (String.fromFloat (ax + toFloat position.col))
        , SA.y1 (String.fromFloat (ay + toFloat position.row))
        , SA.x2 (String.fromFloat (bx + toFloat position.col))
        , SA.y2 (String.fromFloat (by + toFloat position.row))
        , SA.stroke wallStroke
        , SA.strokeWidth ".1"
        , SA.strokeLinecap "round"
        , SE.onClick (flipAction position dir)
        ]
        []


viewDirections : Position -> List Direction -> List ( Bool, Direction )
viewDirections position passages =
    let
        shouldViewWall wallDirection =
            case wallDirection of
                N ->
                    position.row == 0

                W ->
                    position.col == 0

                _ ->
                    True
    in
    directions
        |> List.filter shouldViewWall
        |> List.map (\dir -> ( not (List.member dir passages), dir ))


viewFieldInteract : (Position -> msg) -> Position -> Svg msg
viewFieldInteract dropAction pos =
    Svg.rect
        [ SA.x <| String.fromInt pos.col ++ ".15"
        , SA.y <| String.fromInt pos.row ++ ".15"
        , SA.width ".7"
        , SA.height ".7"
        , SA.fill "ghostwhite"
        , SE.preventDefaultOn "dragover" (JD.succeed ( dropAction pos, True ))
        ]
        []


viewField : (Position -> Direction -> msg) -> (Position -> msg) -> Maze -> Int -> List Direction -> List (Svg msg)
viewField flipAction dropAction maze index passages =
    let
        position =
            positionFromIndex maze index
    in
    viewFieldInteract dropAction position
        :: List.map
            (viewWall flipAction position)
            (viewDirections position passages)


viewFields : (Position -> Direction -> msg) -> (Position -> msg) -> Maze -> List (Svg msg)
viewFields flipAction dropAction maze =
    Array.indexedMap (viewField flipAction dropAction maze) maze.fields
        |> Array.toList
        |> List.concat
