module Snake2 exposing (Snake, adjacentList, any, append, coords, head, initSnake, isHeadOnTail, prune, updateSnake)

import Grid exposing (..)
import Tuple exposing (first, second)


type alias Snake =
    ( GridPoint, List GridPoint )


initSnake : GridPoint -> Snake
initSnake p =
    ( p, [] )


updateSnake : GridPoint -> GridUnit -> Snake -> Maybe Snake
updateSnake new_head new_length old_snake =
    let
        d =
            distance new_head (coords old_snake 0)

        s =
            prune (new_length - d) old_snake
    in
    if isHeadOnTail new_head s then
        Nothing

    else if d == 0 then
        Just s

    else
        Just <| append new_head s


coords : Snake -> GridUnit -> GridPoint
coords s x =
    case ( s, x ) of
        ( _, 0 ) ->
            first s

        ( ( h, [] ), _ ) ->
            h

        ( ( h, h2 :: t2 ), _ ) ->
            let
                d =
                    distance h h2
            in
            if x <= d then
                interpolate h h2 x

            else
                coords ( h2, t2 ) (x - d)


head : Snake -> GridPoint
head s =
    coords s 0


prune : GridUnit -> Snake -> Snake
prune x s =
    case s of
        ( _, [] ) ->
            s

        ( h, h2 :: t2 ) ->
            let
                d =
                    distance h h2
            in
            if x <= d then
                ( h, [ interpolate h h2 x ] )

            else
                append h (prune (x - d) ( h2, t2 ))


append : GridPoint -> Snake -> Snake
append a s =
    ( a, first s :: second s )


isHeadOnTail : GridPoint -> Snake -> Bool
isHeadOnTail new_head s =
    let
        l =
            adjacentList <| second s

        new_segment =
            ( new_head, first s )
    in
    any <| List.map (doIntersect new_segment) l



-- Helper functions


adjacentList : List GridPoint -> List ( GridPoint, GridPoint )
adjacentList ps =
    case ps of
        [] ->
            []

        h :: [] ->
            []

        h :: h2 :: t ->
            ( h, h2 ) :: adjacentList (h2 :: t)


any : List Bool -> Bool
any =
    List.foldr (||) False
