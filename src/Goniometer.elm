module Goniometer exposing (drawGoniometer, showGonioValue, goniometerTurnAngle)

import Html exposing (..)
import Html.Attributes as H exposing (..)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)



-- DEBUG


showGonioValue m =
    Html.text <| String.fromInt <| truncate <| m.goniometer * 180.0 / pi



-- Assumes a suitably styled DIV is in effect ...


drawGoniometer theta =
    svg
        [ viewBox "-200 -200 400 400"
        , S.width "300"
        , S.height "300"
        ]
        (drawGoniometerScale ++ drawGoniometerPointer theta)


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
        , fill "darkslategrey"
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


drawGoniometerScale =
    let
        originX =
            0

        originY =
            0

        radius =
            160

        xFromIndex i =
            String.fromFloat <|
                (+) originX <|
                    (*) radius <|
                        sin <|
                            degrees <|
                                toFloat <|
                                    i
                                        * 10

        yFromIndex i =
            String.fromFloat <|
                (-) (originY + 4) <|
                    (*) radius <|
                        cos <|
                            degrees <|
                                toFloat <|
                                    i
                                        * 10

        labelPoint i =
            Svg.text_
                [ x <| xFromIndex i
                , y <| yFromIndex i
                , fill "antiquewhite"
                , textAnchor "middle"
                , fontFamily "monospace"
                , fontSize "12pt"
                ]
                [ Svg.text (String.fromInt (i * 10))
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
        :: List.map labelPoint (List.range 0 35)

goniometerTurnAngle : Float -> ( Float, Float ) -> ( Float, Float ) -> Float
goniometerTurnAngle startAngle ( startX, startY ) ( newX, newY ) =
    let
        ( _, dragStartAngle ) =
            toPolar ( startX - 150, startY - 150 )

        -- where on control was clicked
        ( _, dragNowAngle ) =
            toPolar ( newX - 150, newY - 150 )

        -- where that point is now
    in
    startAngle + dragNowAngle - dragStartAngle

