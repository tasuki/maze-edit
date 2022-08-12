module Secrets exposing (flipKey, keyLetters, viewKeys, viewSumLetters)

import Array exposing (Array)
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Maze exposing (Maze, Position)
import Search exposing (Path)
import Svg exposing (Svg)
import Svg.Attributes as SA



-- Data


alphabet : Array Char
alphabet =
    String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Array.fromList



-- Functions


getLetter : Int -> Char
getLetter index =
    Array.get index alphabet |> Maybe.withDefault ' '


flipKey : Position -> List Position -> List Position
flipKey pos keys =
    case ListE.elemIndex pos keys of
        Just index ->
            ListE.removeAt index keys

        Nothing ->
            pos :: keys


keyLettersHelper : Path -> List Position -> List Char -> List Char
keyLettersHelper path keys acc =
    case path of
        pos :: tail ->
            let
                newAcc =
                    if ListE.elemIndex pos keys |> MaybeE.isJust then
                        getLetter pos.col :: acc

                    else
                        acc
            in
            keyLettersHelper tail keys newAcc

        _ ->
            List.reverse acc


keyLetters : Path -> List Position -> List Char
keyLetters path keys =
    keyLettersHelper path keys []


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
        |> Array.map (\i -> getLetter (i - 1))
        |> Array.toList



-- Views


viewKey : Position -> Svg msg
viewKey key =
    let
        style =
            "stroke: black; stroke-width: 2; stroke-linecap: square; fill: none;"

        col =
            String.fromInt key.col ++ ".33"

        row =
            String.fromInt key.row ++ ".2"

        translate =
            "translate(" ++ col ++ "," ++ row ++ ")"

        scale =
            "scale(.03 .03)"
    in
    Svg.g
        [ SA.transform (translate ++ " " ++ scale), SA.style style ]
        [ Svg.circle [ SA.r "3", SA.cx "5", SA.cy "5" ] []
        , Svg.path [ SA.d "m 5, 9 l 0.0,9" ] []
        , Svg.path [ SA.d "m 5,14 l 2.0,0" ] []
        , Svg.path [ SA.d "m 5,18 l 2.5,0" ] []
        ]


viewKeys : List Position -> List (Svg msg)
viewKeys =
    List.map viewKey


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
