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
            ++ drawGoniometerPointer theta


knobRadius =
    135


dialRadius =
    160


tickRadius =
    150


drawGoniometerPointer theta =
    let
        originX =
            0

        originY =
            0

        xPoint =
            String.fromFloat <|
                (+) originX <|
                    (*) knobRadius <|
                        sin theta

        yPoint =
            String.fromFloat <|
                (-) originY <|
                    (*) knobRadius <|
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


drawGoniometerScaleWIthImage =
    Svg.image
        [ x "-190"
        , y "-190"
        , S.width "380"
        , S.height "380"
        , xlinkHref "../resources/goniometer.png"
        ]
        []


drawGoniometerScale =
    let
        originX =
            0

        originY =
            0

        xFromIndex i =
            String.fromFloat <|
                (+) originX <|
                    (*) dialRadius <|
                        sin <|
                            degrees <|
                                toFloat <|
                                    i
                                        * 10

        yFromIndex i =
            String.fromFloat <|
                (-) (originY + 4) <|
                    (*) dialRadius <|
                        cos <|
                            degrees <|
                                toFloat <|
                                    i
                                        * 10

        xTick i =
            String.fromFloat <|
                (+) originX <|
                    (*) tickRadius <|
                        sin <|
                            degrees <|
                                toFloat <|
                                    i
                                        * 10

        yTick i =
            String.fromFloat <|
                (-) (originY + 4) <|
                    (*) tickRadius <|
                        cos <|
                            degrees <|
                                toFloat <|
                                    i
                                        * 10

        labelPoint i =
            [ Svg.line
                [ x1 "0"
                , y1 "0"
                , x2 (xTick i)
                , y2 (yTick i)
                , stroke "antiquewhite"
                , strokeWidth "2"
                , strokeLinecap "round"
                ]
                []
            , Svg.text_
                [ x <| xFromIndex i
                , y <| yFromIndex i
                , fill "antiquewhite"
                , textAnchor "middle"
                , fontFamily "monospace"
                , fontSize "16"
                ]
                [ Svg.text (String.fromInt (i * 10))
                ]
            ]
    in
    Svg.circle
        [ cx "0"
        , cy "0"
        , r "180"
        , stroke "grey"
        , strokeWidth "1"
        , fill "dimgrey"
        ]
        []
        :: List.concatMap labelPoint (List.range 0 35)


goniometerTurnAngle : Angle -> Point -> Point -> Angle
goniometerTurnAngle startAngle ( startX, startY ) ( newX, newY ) =
    let
        ( _, dragStartAngle ) =
            toPolar ( startX - knobRadius, startY - knobRadius )

        -- where on control was clicked
        ( _, dragNowAngle ) =
            toPolar ( newX - knobRadius, newY - knobRadius )

        -- where that point is now
    in
    startAngle + dragNowAngle - dragStartAngle
