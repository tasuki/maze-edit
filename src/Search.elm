module Search exposing (..)

import Array exposing (Array)
import Fifo exposing (Fifo)
import Maze exposing (Direction, Maze, Position)
import Svg exposing (Svg)


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
        expand : List Position -> Node -> OpenClosed -> OpenClosed
        expand positions node ( open, closed ) =
            case positions of
                [] ->
                    ( open, closed )

                head :: tail ->
                    expand
                        tail
                        node
                        ( Fifo.insert (Node head (ParentNode <| Just node) (node.cost + 1)) open
                        , Array.set (Maze.fieldIndex maze head) True closed
                        )

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
                        helper <|
                            expand
                                (neighbors maze current.position)
                                current
                                ( newOpen, closed )

        fromIndex =
            Maze.fieldIndex maze from
    in
    helper
        ( Fifo.fromList [ Node from (ParentNode Nothing) 0 ]
        , Array.indexedMap (\i _ -> i == fromIndex) maze.fields
        )


viewPaths : Maze -> List Path -> List (Svg msg)
viewPaths maze paths =
    []
