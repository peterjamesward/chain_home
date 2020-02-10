module CRT exposing (crt)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Utils exposing (..)


crt m =
    let
        svgPointList =
            polyLineFromCoords m.lineData
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
    List.map
        (\i ->
            text_
                [ x (String.fromInt (i * 50))
                , y "-10"
                , fill "lightgreen"
                , textAnchor "right"
                , fontFamily "monospace"
                ]
                [ text (String.fromInt (i * 5)) ]
        )
        (List.range 0 19)
