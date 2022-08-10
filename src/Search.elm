module Search exposing (Path, findPaths, viewPaths, viewStartAndEnd)

import Array exposing (Array)
import Fifo exposing (Fifo)
import Maze exposing (Direction, Maze, Position)
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias Path =
    List Position


type alias Node =
    { position : Position
    , parent : ParentNode
    , cost : Int
    }


type ParentNode
    = ParentNode (Maybe Node)


type alias OpenClosed =
    ( Fifo Node, Array Bool )


nodeToPath : Node -> Path
nodeToPath node =
    let
        helper : Node -> Path -> Path
        helper n p =
            case n.parent of
                ParentNode Nothing ->
                    n.position :: p

                ParentNode (Just parentNode) ->
                    helper parentNode (n.position :: p)
    in
    helper node []


neighbors : Maze -> Position -> List Position
neighbors maze pos =
    List.filterMap (Maze.move maze pos) (Maze.getField maze pos)


findPaths : Maze -> Position -> Position -> List Path
findPaths maze from to =
    let
        openPosition : Position -> Node -> OpenClosed -> OpenClosed
        openPosition position parentNode ( open, closed ) =
            let
                index =
                    Maze.fieldIndex maze position
            in
            case Array.get index closed of
                Just False ->
                    ( Fifo.insert
                        (Node
                            position
                            (ParentNode <| Just parentNode)
                            (parentNode.cost + 1)
                        )
                        open
                    , Array.set index True closed
                    )

                _ ->
                    ( open, closed )

        expand : List Position -> Node -> OpenClosed -> OpenClosed
        expand positions parentNode openClosed =
            case positions of
                [] ->
                    openClosed

                head :: tail ->
                    expand tail parentNode (openPosition head parentNode openClosed)

        helper : OpenClosed -> List Path
        helper ( open, closed ) =
            case Fifo.remove open of
                ( Nothing, _ ) ->
                    -- path not found
                    []

                ( Just current, newOpen ) ->
                    if current.position == to then
                        -- for now, just return the first path found
                        [ nodeToPath current ]

                    else
                        helper
                            (expand
                                (neighbors maze current.position)
                                current
                                ( newOpen, closed )
                            )

        fromIndex =
            Maze.fieldIndex maze from
    in
    helper
        ( Fifo.fromList [ Node from (ParentNode Nothing) 0 ]
        , Array.indexedMap (\i _ -> i == fromIndex) maze.fields
        )



-- Views


viewPosition : Position -> Svg msg
viewPosition pos =
    Svg.circle
        [ SA.cx <| String.fromInt pos.col ++ ".5"
        , SA.cy <| String.fromInt pos.row ++ ".5"
        , SA.r ".1"
        , SA.fill "orange"
        ]
        []


viewPaths : List Path -> List (Svg msg)
viewPaths paths =
    List.map (\p -> List.drop 1 p |> List.reverse |> List.drop 1) paths
        |> List.concatMap (List.map viewPosition)


viewStartOrEnd : Position -> Char -> Svg msg
viewStartOrEnd pos char =
    Svg.text_
        [ SA.x <| String.fromInt pos.col ++ ".3"
        , SA.y <| String.fromInt pos.row ++ ".8"
        , SA.fill "orange"
        , SA.fontSize ".8"
        , SA.fontWeight "bold"
        ]
        [ Svg.text <| String.fromChar char ]


viewStartAndEnd : Position -> Position -> List (Svg msg)
viewStartAndEnd start end =
    [ viewStartOrEnd start 'S'
    , viewStartOrEnd end 'E'
    ]
