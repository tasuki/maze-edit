module Search exposing (..)

import Array exposing (Array)
import Fifo exposing (Fifo)
import Maze exposing (Direction, Maze, Position)
import Svg exposing (..)
import Svg.Attributes exposing (..)


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
                    let
                        index =
                            Maze.fieldIndex maze head

                        openClosed =
                            case Array.get index closed of
                                Just False ->
                                    ( Fifo.insert (Node head (ParentNode <| Just node) (node.cost + 1)) open
                                    , Array.set index True closed
                                    )

                                _ ->
                                    ( open, closed )
                    in
                    expand tail node openClosed

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



-- Views


viewPosition : Position -> Svg msg
viewPosition pos =
    circle
        [ cx <| String.fromInt pos.col ++ ".5"
        , cy <| String.fromInt pos.row ++ ".5"
        , r ".1"
        , fill "orange"
        ]
        []


viewPaths : List Path -> List (Svg msg)
viewPaths =
    List.concatMap (List.map viewPosition)
