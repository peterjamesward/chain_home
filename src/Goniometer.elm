module Goniometer exposing (showGonio, clickableGonioImage)

import Html exposing (..)
import Html.Attributes as H exposing (..)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)

-- DEBUG
showGonio m = Html.text <| String.fromInt <| truncate <| m.goniometer * 180.0 / pi


clickableGonioImage styles theta = 
  div 
    styles
    [ svg [ viewBox "-200 -200 400 400"
          , S.width "300"
          , S.height "300"
          ]
       (drawGoniometerScale ++ (drawGoniometerPointer theta))
    ]


drawGoniometerPointer theta = 
    let originX = 0
        originY = 0
        radius = 135
        xPoint = String.fromFloat <| ((+) originX) 
                                  <| ((*) radius) <| sin theta
        yPoint = String.fromFloat <| ((-) originY)
                                  <| ((*) radius) <| cos theta
    in
        [    Svg.circle
                [ cx "0"
                , cy "0"
                , r "140"
                , stroke "grey"
                , strokeWidth "1"
                , fill "grey"
                ]
                []
           , Svg.line
                [ x1 "0"
                , y1 "0"
                , x2 xPoint
                , y2 yPoint
                , stroke "white"
                , strokeWidth "10"
                , strokeLinecap "round"
                ]
                []
        ]

drawGoniometerScale  = 
    let originX = 0
        originY = 0
        radius = 160
        xFromIndex i = String.fromFloat <| ((+) originX) 
                                        <| ((*) radius) <| sin <| degrees 
                                        <| toFloat <| i * 10 
        yFromIndex i = String.fromFloat <| ((-) (originY + 4)) 
                                        <| ((*) radius) <| cos <| degrees 
                                        <| toFloat <| i * 10 
        labelPoint i = Svg.text_  [  x <| xFromIndex i
                                   , y <| yFromIndex i
                                   , fill "black"
                                   , textAnchor "middle" 
                                   ] 
                                   [ Svg.text (String.fromInt (i*10)) 
                                   ]
    in
       (List.map labelPoint (List.range 0 35))

