module CRT exposing (crt)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (Point)
import Utils exposing (..)


crt : List Point -> Html msg
crt lineData =
    let
        svgPointList =
            polyLineFromCoords lineData
    in
    svg
        [ viewBox "-10 -40 1020 450" -- Establishes the CRT coordinate system, not its rendered size.
        , width "100%"
        , height "100%"
        ]
    <|
        [ rect
            [ x "-10"
            , y "-40"
            , rx "20"
            , ry "20"
            , width "1020"
            , height "450"
            , fill "black"
            , stroke "black"
            , strokeWidth "3"
            , strokeLinejoin "round"
            ]
            []
        , polyline
            [ points svgPointList
            , fill "none"
            , stroke "forestgreen"
            , opacity "60%"
            , strokeWidth "2.5"
            ]
            []
        , polyline
            [ points svgPointList
            , fill "none"
            , stroke "springgreen"
            , strokeWidth "0.8"
            ]
            []
        ]
            ++ rangeScale


rangeScale =
    List.concatMap
        (\i ->
            [ text_
                [ x (String.fromInt <| Basics.min 980 (i * 50 - 5))
                , y "-10"
                , fill "antiquewhite"
                , textAnchor "right"
                , fontFamily "monospace"
                ]
                [ text (String.fromInt (i * 5)) ]
            , Svg.line
                [ x1 (String.fromInt (i * 50))
                , y1 "-5"
                , x2 (String.fromInt (i * 50))
                , y2 "0"
                , stroke "antiquewhite"
                , strokeWidth "1"
                ] []
            ]
        )
        (List.range 0 20)
