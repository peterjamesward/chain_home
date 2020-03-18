module Range exposing (..)

-- Range adjusting control based on goniometer dial.

import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import Types exposing (Angle, Point)
import Utils exposing (normalise)


radius =
    120


drawRangeKnob theta =
    let
        originX =
            0

        originY =
            0

        xPoint =
            String.fromFloat <|
                (+) originX <|
                    (*) (radius - 8) <|
                        sin theta

        yPoint =
            String.fromFloat <|
                (-) originY <|
                    (*) (radius - 8) <|
                        cos theta
    in
    svg
        [ viewBox "-140 -140 280 280"
        , S.width "100%"
        , S.height "100%"
        ]
    <|
        [ Svg.circle
            [ cx "0"
            , cy "0"
            , r (String.fromInt radius)
            , stroke "grey"
            , strokeWidth "1"
            , S.fill "darkslategrey"
            ]
            []
        , Svg.circle
            [ cx xPoint
            , cy yPoint
            , r "5"
            , stroke "grey"
            , strokeWidth "1"
            , S.fill "white"
            ]
            []
        ]


rangeTurnAngle : Angle -> Point -> Point -> Angle
rangeTurnAngle startAngle ( startX, startY ) ( newX, newY ) =
    -- Adding check to prevent flipping between +/- pi
    let
        ( _, dragStartAngle ) =
            toPolar ( startX - radius, startY - radius )

        -- where on control was clicked
        ( _, dragNowAngle ) =
            toPolar ( newX - radius, newY - radius )

        -- where that point is now
        newAngle =
            startAngle + dragNowAngle - dragStartAngle
    in
    if abs (dragNowAngle - dragStartAngle) > pi then
        startAngle
        -- i.e. don't let it move

    else
        normalise newAngle
