module Skyline exposing (deriveSkyline, EdgeSegment, viewEdge, viewLineSegment)

import Echo exposing (..)
import Constants exposing (..)
import Html exposing (..)    
import Utils exposing (stringifyPoint)

type alias EdgeSegment = ((Float, Float), (Float, Float))

-- Skyline algorithm works out the trace from combined echoes.

{-There will be some set of echoes, and shall probably manage these as a set not a list.
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
  We end with a line to (800,0). -}

type alias EdgeInfo = (Float, Echo, Bool)  -- x coord of edge, leading edge,  and whether leading edge.

dummyLeadingEdge = ((0.0,0.0),(0.0,0.0))

-- This version uses the Echoes, but really this doesn't happen until
-- the echoes are received at the (two) antenna, where they become a voltage.

combineEchoes : List Echo -> Float

combineEchoes activeEchoes = 
  --Dict.foldl (\_ e acc -> e.amplitude + acc) 0.0 activeEchoes
--  100.0 * toFloat (Dict.size activeEchoes)  --OK!
  -- Treat amplitude and phase as vector, sum components, convert back, use amplitude.
  let --asPolar = Dict.map (\_ e -> (e.amplitude, e.phase) ) activeEchoes
      asRect  = List.map (\e -> fromPolar (e.amplitude, e.phase)) activeEchoes
      combinedAsRect = List.foldl (\(x, y) (xAcc, yAcc) -> (x + xAcc, y + yAcc) ) (0.0,0.0) asRect
      (mag, phase) = toPolar combinedAsRect
  in
      mag

-- Why do I find this so hard. Look at each edge. It's the beginning or end of an echo.
-- They should be sorted by X (they are). There may be several with the same X.
-- We do not output for each edge. We output only when X and Y change.
processEdge : EdgeInfo 
              -> (List EdgeSegment, List Echo, (Float, Float))
              -> (List EdgeSegment, List Echo, (Float, Float))
processEdge (p, echo, isLeading) 
            (roofline, activeEchoes, (lastX,lastY)) = 
  let   ((x1,y),(x2,_)) = Maybe.withDefault dummyLeadingEdge <| List.head roofline -- the last output.
        newActiveEchoes = case isLeading of
                              True  -> echo :: activeEchoes
                              False -> List.filter ((/=) echo) activeEchoes
        newX = p
        newY = combineEchoes newActiveEchoes
  in
        if  newX <= x2 then
            -- We're not moving horizontally but we need to track the roof height.
            (roofline, newActiveEchoes, (newX, newY))
        else if newY == y then
            -- We have moved horizontally, but the height is the same so
            -- there's nothing to say or do. Move along.
            (roofline, newActiveEchoes, (x1,y))
        else
            -- A significant edge, so we can now output the previous bit of roof.
            (((x2, lastY), (newX, lastY)) :: roofline, newActiveEchoes, (newX, newY))

-- Derive skyline as series of flat roof segments.
deriveSkyline : Float -> List Echo -> List EdgeSegment
deriveSkyline maxX allEchoes =
  -- Let's put the echoes in a dictionary, indexed by range (probably unique!).
  -- Then we need a sorted list of edges (front and back).
  -- Sorted list will index into the dictionary, for easy access to each target.
  let activeEchoes = []
      leadingEdges = List.map (\e-> (e.r, e, True) ) allEchoes
      trailingEdges = List.map (\e -> (e.r + pulseWidth, e, False )) allEchoes 
      allEdges = List.sortBy (\(r,_,_) -> r) (leadingEdges ++ trailingEdges)
      -- We have one in hand so we need to flush it out
      (roofline, remainingEchoes, ((lastX, lastY))) = List.foldl processEdge ([], activeEchoes, (0.0,0.0)) allEdges
      finalY = combineEchoes remainingEchoes
  in
      ((lastX, finalY), (maxX, finalY)) :: roofline

viewEdge ((x1,y1),(x2,y2)) = [ Html.text "( "
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

viewLineSegment (x,y) = [ Html.text "( "
                , Html.text <| stringifyPoint (x,y)
                --, Html.text <| String.fromFloat <| x
                --, Html.text " , "
                --, Html.text <| String.fromFloat <| y
                , Html.text " )"
                , Html.br [] []
               , Html.hr [] []
               ]
