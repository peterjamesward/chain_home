module CRT exposing (crt)

import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Utils exposing (..)


crt m =
    let
        svgPointList =
            polyLineFromCoords m.lineData
    in
    svg
        [ viewBox "-10 -40 1020 450" -- Establishes the CRT coordinate system, not its rendered size.
        , SA.width "1020"
        , SA.height "420"
        ]
    <|
        [ rect
            [ x "-10"
            , y "-40"
            , rx "20"
            , ry "20"
            , SA.width "1020"
            , SA.height "450"
            , SA.fill "black"
            , stroke "black"
            , strokeWidth "3"
            , strokeLinejoin "round"
            ]
            []
        , polyline
            [ points svgPointList
            , SA.fill "none"
            , stroke "forestgreen"
            , opacity "60%"
            , strokeWidth "2.5"
            ]
            []
        , polyline
            [ points svgPointList
            , SA.fill "none"
            , stroke "springgreen"
            , strokeWidth "0.8"
            ]
            []
        ]
            ++ rangeScale


rangeScale =
    List.map
        (\i ->
            S.text_
                [ x (String.fromInt (i * 50))
                , y "-10"
                , SA.fill "lightgreen"
                , textAnchor "right"
                , fontFamily "monospace"
                ]
                [ S.text (String.fromInt (i * 5)) ]
        )
        (List.range 0 19)
