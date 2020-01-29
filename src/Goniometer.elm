module Goniometer exposing (showGonio, showGonioImage)

import Html exposing (..)
import Html.Attributes as H exposing (..)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)


showGonio m = Html.text <| String.fromInt <| truncate <| m.goniometer * 180.0 / pi

showGonioImage m = 
    drawGoniometerScale
    --let imageFile = "http://localhost:8000/dial.jpeg"
    --in
    --  img [ H.src imageFile
    --      , H.width 300
    --      , H.height 300 
    --      , H.style "transform" ("rotate(" ++ (String.fromInt <| truncate <| m.goniometer * 180/pi) ++ "deg)") 
    --      ] []


drawGoniometerScale = 
    let originX = 0
        originY = 0
        radius = 160
        xFromIndex i = String.fromFloat <| ((+) originX) 
                                        <| ((*) radius) <| sin <| degrees 
                                        <| toFloat <| i * 10
        yFromIndex i = String.fromFloat <| ((-) originY)
                                        <| ((*) radius) <| cos <| degrees 
                                        <| toFloat <| i * 10
        labelPoint i = Svg.text_  [  x <| xFromIndex i
                                   , y <| yFromIndex i
                                   , fill "black"
                                   , textAnchor "center" 
                                   ] 
                                   [ Svg.text (String.fromInt (i*10)) 
                                   ]
    in
        svg [ viewBox "-200 -200 400 400"
        , S.width "300"
        , S.height "300"
        ]
        (List.map labelPoint (List.range 0 35))
