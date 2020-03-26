module MapPage exposing (..)

{-
   This will display actual tracks of raids and the user's plots.
   ?? What is trigger for plot being stored - the sequence has no clear end ??
   Note that we do not try to correlate. In general we may not know which raid the user is plotting.
-}

import Element exposing (..)
import Grid exposing (gridLettersList)
import Messages exposing (Msg)
import Model exposing (Model)
import Svg exposing (..)
import Svg.Attributes as A exposing (..)


mapVisibleGrid : List (List String)
mapVisibleGrid =
    let
        takeMiddle =
            List.drop 1 >> List.take 5
    in
    -- Only central 5x5 square of calculator grid is really in our range.
    gridLettersList
        |> takeMiddle
        |> List.map takeMiddle


mapPage : Model -> Element Msg
mapPage _ =
    el [ centerX, centerY, Element.width Element.fill ] <|
        html <|
            svg
                [ viewBox "0 0 800 800"
                , A.width "100%"
                , A.height "100%"
                ]
            <|
                [ Svg.image
                    [ x "0"
                    , y "0"
                    , A.width "400"
                    , A.height "400"
                    , xlinkHref "../resources/east_anglia.png"
                    ]
                    []
                ]
                    ++ gridLetters


gridLetters =
    let
        gridLetterRow letters yCoord =
            List.map2 (gridLetter yCoord) letters [ 1, 2, 3, 4, 5 ]

        gridLetter yCoord letter xCoord =
            Svg.text_
                [ x <| String.fromInt <| xCoord * 80 - 50
                , y <| String.fromInt <| yCoord * 80 - 40
                , A.stroke "black"
                , A.fill "none"
                , textAnchor "middle"
                , fontFamily "monospace"
                , fontSize "24"
                ]
                [ Svg.text letter
                ]
    in
    List.concat <|
        List.map2 gridLetterRow mapVisibleGrid [ 1, 2, 3, 4, 5 ]
