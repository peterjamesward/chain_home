module Skyline exposing (EdgeSegment, deriveSkyline, viewEdge, viewLineSegment)

import Constants exposing (..)
import Echo exposing (Echo)
import Html exposing (..)
import Utils exposing (stringifyPoint)


type alias EdgeSegment =
    ( ( Float, Float ), ( Float, Float ) )



-- Skyline algorithm works out the trace from combined echoes.
{- There will be some set of echoes, and shall probably manage these as a set not a list.
   We sort by range of leading edge of echo. Maybe we have one sort of both leading and
   trailing edges.
   We process the echoes in order of range, skipping to the next edge. Range will be fractional,
   and we rely on SVG to draw nice lines. (We may try various quality options.) As we
   encounter leading edges, we add the echo to the active echo set and sum the echoes in
   the active set accounting for phase and magnitude. This gives deflection and is valid
   until the next edge.
   On a trailing edge, we remove the relevant echo from the active set and derive the
   new deflection.
   On each edge, we output a line segment. We can prepend these to a list.
   We end with a line to (800,0).
-}


type alias EdgeInfo =
    ( Float, Echo, Bool )



-- x coord of edge, leading edge,  and whether leading edge.


dummyLeadingEdge =
    ( ( 0.0, 0.0 ), ( 0.0, 0.0 ) )



{-
   Having thought more about the "beating" effect on the CRT videos, I suspect
   there are some unexplained frequencies shifts in the return signals and it
   genuinely is a moiré effect. We can probably simulate that by applying time-varying
   phase shifts to each signal, on a fairly arbitrary basis, as we combine them.
   We do this to deliberately cause the phase to alternate over a 1 to 4 second
   period (say).
   To facilitate, the phase attribute of all echoes will contain the model time.
   We can also use range as a 'seed' to prevent all echoes bouncing in synch!
-}


combineEchoes : List Echo -> Float
combineEchoes activeEchoes =
    let
        -- The beat index will determine for each echo how quickly it's
        -- phase rotates (let's say how many seconds a rotation takes,
        -- this is all empirical "art" anyway).
        beatIndex =
            List.range 1 (List.length activeEchoes)

        milliseconds t =
            truncate t |> modBy 1000 |> toFloat |> (/) 1000

        funkyEchoCombiner ( echo, index ) accumulator =
            accumulator
                + echo.amplitude
                * (1
                    + sin
                        (pi
                            * milliseconds (echo.phase + echo.r)
                            / toFloat index
                        )
                  )
                / 2

        descendingEchoes =
            List.reverse <| List.sortBy .amplitude activeEchoes
    in
    case descendingEchoes of
        [] ->
            0.0

        e :: rest ->
            abs <|
                (+) e.amplitude <|
                    List.foldl funkyEchoCombiner
                        0.0
                        (List.map2 Tuple.pair
                            rest
                            beatIndex
                        )



-- We do not output for each edge. We output only when X and Y change.


processEdge :
    EdgeInfo
    -> ( List EdgeSegment, List Echo, ( Float, Float ) )
    -> ( List EdgeSegment, List Echo, ( Float, Float ) )
processEdge ( p, echo, isLeading ) ( roofline, activeEchoes, ( lastX, lastY ) ) =
    let
        ( ( x1, y ), ( x2, _ ) ) =
            Maybe.withDefault dummyLeadingEdge <| List.head roofline

        -- the last output.
        newActiveEchoes =
            case isLeading of
                True ->
                    echo :: activeEchoes

                False ->
                    List.filter (\e -> e.r /= echo.r) activeEchoes

        newX =
            p

        newY =
            combineEchoes newActiveEchoes
    in
    if newX <= x2 then
        -- We're not moving horizontally but we need to track the roof height.
        ( roofline, newActiveEchoes, ( newX, newY ) )

    else if newY == lastY then
        -- We have moved horizontally, but the height is the same so
        -- there's nothing to say or do. Move along.
        ( roofline, newActiveEchoes, ( x1, y ) )

    else
        -- A significant edge, so we can now output the previous bit of roof.
        ( ( ( x2, lastY ), ( newX, lastY ) )
            :: roofline
        , newActiveEchoes
        , ( newX, newY )
        )



-- Derive skyline as series of flat roof segments.


deriveSkyline : Float -> List Echo -> List EdgeSegment
deriveSkyline maxX allEchoes =
    -- Let's put the echoes in a dictionary, indexed by range (probably unique!).
    -- Then we need a sorted list of edges (front and back).
    -- Sorted list will index into the dictionary, for easy access to each target.
    let
        activeEchoes =
            []

        leadingEdges =
            List.map (\e -> ( e.r, e, True )) allEchoes

        trailingEdges =
            List.map (\e -> ( e.r + pulseWidth, e, False )) allEchoes

        allEdges =
            List.sortBy (\( r, _, _ ) -> r) (leadingEdges ++ trailingEdges)

        -- We have one in hand so we need to flush it out
        ( roofline, remainingEchoes, ( lastX, lastY ) ) =
            List.foldl processEdge ( [], activeEchoes, ( 0.0, 0.0 ) ) allEdges

        finalY =
            combineEchoes remainingEchoes
    in
    ( ( lastX, 0 ), ( maxX, 0 ) ) :: roofline


viewEdge ( ( x1, y1 ), ( x2, y2 ) ) =
    [ Html.text "( "
    , Html.text <| String.fromFloat <| x1
    , Html.text " , "
    , Html.text <| String.fromFloat <| y1
    , Html.text " ), ( "
    , Html.text <| String.fromFloat <| x2
    , Html.text " , "
    , Html.text <| String.fromFloat <| y2
    , Html.text " )"
    , Html.br [] []
    , Html.hr [] []
    ]


viewLineSegment ( x, y ) =
    [ Html.text "( "
    , Html.text <| stringifyPoint ( x, y )
    , Html.text " )"
    , Html.br [] []
    , Html.hr [] []
    ]
