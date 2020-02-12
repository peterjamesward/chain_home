module Goniometer exposing (drawGoniometer, goniometerTurnAngle)

import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import Types exposing (..)


-- Assumes a suitably styled DIV is in effect ...


drawGoniometer theta =
    svg
        [ viewBox "-200 -200 400 400"
        , S.width "100%"
        , S.height "100%"
        ]
    <|
        drawGoniometerScale
            :: drawGoniometerPointer theta


drawGoniometerPointer theta =
    let
        originX =
            0

        originY =
            0

        radius =
            135

        xPoint =
            String.fromFloat <|
                (+) originX <|
                    (*) radius <|
                        sin theta

        yPoint =
            String.fromFloat <|
                (-) originY <|
                    (*) radius <|
                        cos theta
    in
    [ Svg.circle
        [ cx "0"
        , cy "0"
        , r "140"
        , stroke "grey"
        , strokeWidth "1"
        , S.fill "darkslategrey"
        ]
        []
    , Svg.line
        [ x1 "0"
        , y1 "0"
        , x2 xPoint
        , y2 yPoint
        , stroke "antiquewhite"
        , strokeWidth "10"
        , strokeLinecap "round"
        ]
        []
    ]



-- Image inside SVG.


drawGoniometerScale =
    Svg.image
        [ x "-190"
        , y "-190"
        , S.width "380"
        , S.height "380"
        , xlinkHref "../resources/goniometer.png"
        ]
        []


goniometerTurnAngle : Angle -> Point -> Point -> Angle
goniometerTurnAngle startAngle ( startX, startY ) ( newX, newY ) =
    let
        ( _, dragStartAngle ) =
            toPolar ( startX - 130, startY - 130 )

        -- where on control was clicked
        ( _, dragNowAngle ) =
            toPolar ( newX - 130, newY - 130 )

        -- where that point is now
    in
    startAngle + dragNowAngle - dragStartAngle
