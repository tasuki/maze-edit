module Secrets exposing (sumLetters, viewSumLetters)

import Array exposing (Array)
import Maze exposing (Maze, Position)
import Search exposing (Path)
import Svg exposing (Svg)
import Svg.Attributes as SA



-- Data


type Key
    = Key Position


alphabet : Array Char
alphabet =
    String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Array.fromList



-- Functions
-- keyLetters : Path -> List Key -> List Char


mapOne : Int -> a -> (a -> a) -> Array a -> Array a
mapOne index default fun arr =
    let
        new =
            Array.get index arr |> Maybe.withDefault default |> fun
    in
    Array.set index new arr


sumHelper : Path -> Array Int -> Array Int
sumHelper path acc =
    case path of
        pos :: pathTail ->
            sumHelper pathTail (mapOne pos.row 0 (\i -> i + 1) acc)

        _ ->
            acc


sumLetters : Maze -> Path -> List Char
sumLetters maze path =
    sumHelper path (Array.initialize maze.height (always 0))
        |> Array.map (\i -> Array.get (i - 1) alphabet |> Maybe.withDefault ' ')
        |> Array.toList



-- Views
-- viewKey : Position -> Svg msg


viewSumLetter : Maze -> Int -> Char -> Svg msg
viewSumLetter maze index char =
    Svg.text_
        [ SA.x <| String.fromInt (maze.width + 1)
        , SA.y <| String.fromInt index ++ ".7"
        , SA.fontSize ".8"
        ]
        [ Svg.text <| String.fromChar char ]


viewSumLetters : Maze -> Path -> List (Svg msg)
viewSumLetters maze path =
    sumLetters maze path |> List.indexedMap (viewSumLetter maze)
