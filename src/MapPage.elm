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
import TrainingMode exposing (explanatoryText)
import Types exposing (UiComponent(..))
import Utils exposing (helpButton)


squareSize =
    -- This is 'tuned' to suit the map image.
    160


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
mapPage model =
    row
        [ centerX
        , alignTop
        , padding 10
        , spaceEvenly
        , Element.width Element.fill
        , explain Debug.todo
        ]
        [ theMap model
        , el
            (explanatoryText model UiMapPage)
            (Element.text "Hello world")
        , helpButton
        ]


theMap : Model -> Element Msg
theMap model =
    -- Sizing this is a bit of a fiddle. Probably cause of stuff I don't know.
    el
        [ centerX
        , centerY
        ]
    <|
        Element.html <|
            svg
                [ viewBox "0 0 800 800"
                , A.width "100%"
                , A.height "100%"
                ]
            <|
                [ Svg.image
                    [ x "0"
                    , y "0"
                    , A.width <| String.fromInt <| squareSize * 5
                    , A.height <| String.fromInt <| squareSize * 5
                    , xlinkHref "../resources/east_anglia.png"
                    ]
                    []
                ]
                    ++ gridLetters
                    ++ gridLines
                    ++ raidTracks model
                    ++ userPlots model


gridLetters =
    let
        gridLetterRow letters yCoord =
            List.map2 (gridLetter yCoord) letters [ 1, 2, 3, 4, 5 ]

        gridLetter yCoord letter xCoord =
            Svg.text_
                [ x <| String.fromInt <| xCoord * squareSize - 40
                , y <| String.fromInt <| yCoord * squareSize - 35
                , A.stroke "black"
                , A.fill "none"
                , textAnchor "middle"
                , fontFamily "monospace"
                , fontSize "32"
                ]
                [ Svg.text letter
                ]
    in
    List.concat <|
        List.map2 gridLetterRow mapVisibleGrid [ 1, 2, 3, 4, 5 ]


gridLines =
    let
        horizontalGridLine index =
            Svg.line
                [ x1 <| String.fromInt 0
                , y1 <| String.fromInt <| index * squareSize
                , x2 <| String.fromInt (squareSize * 5)
                , y2 <| String.fromInt <| index * squareSize
                , stroke "black"
                , strokeWidth "1"
                ]
                []

        verticalGridLine index =
            Svg.line
                [ y1 <| String.fromInt 0
                , x1 <| String.fromInt <| index * squareSize
                , y2 <| String.fromInt (squareSize * 5)
                , x2 <| String.fromInt <| index * squareSize
                , stroke "black"
                , strokeWidth "1"
                ]
                []
    in
    List.map horizontalGridLine [ 1, 2, 3, 4 ]
        ++ List.map verticalGridLine [ 1, 2, 3, 4 ]


raidTracks model =
    -- Note that the plots are associated with the polar targets.
    -- We shall use the same conversion from range and bearing to map coordinates as we
    -- do for the calculator grid so this should be fair.
    -- Remember station is notionally in central square.
    -- Scalewise, our map is made of 100km squares and they occupy 'squareSize' on the screen.
    let
        mapScale =
            toFloat squareSize / 100000.0

        noMoreThan100Miles ( _, r, _ ) =
            r <= 160000

        raidTrack target =
            List.map plotArrow <|
                List.filter noMoreThan100Miles
                    target.positionHistory

        plotArrow ( time, range, theta ) =
            Svg.circle
                [ cx <| String.fromFloat <| range * sin theta * mapScale + 2.5 * squareSize
                , cy <| String.fromFloat <| 2.5 * squareSize - range * cos theta * mapScale -- y is +ve downwards!
                , r "3"
                , stroke "orange"
                , strokeWidth "1"
                , A.fill "navy"
                ]
                []
    in
    List.concatMap raidTrack model.targets


userPlots model =
    -- Note that the plots are associated with the polar targets.
    -- We shall use the same conversion from range and bearing to map coordinates as we
    -- do for the calculator grid so this should be fair.
    -- Remember station is notionally in central square.
    -- Scalewise, our map is made of 100km squares and they occupy 'squareSize' on the screen.
    let
        mapScale =
            toFloat squareSize / 100000.0

        plot ( time, range, theta ) =
            Svg.circle
                [ cx <| String.fromFloat <| range * sin theta * mapScale + 2.5 * squareSize
                , cy <| String.fromFloat <| 2.5 * squareSize - range * cos theta * mapScale -- y is +ve downwards!
                , r "3"
                , stroke "navy"
                , strokeWidth "1"
                , A.fill "yellow"
                ]
                []
    in
    List.map plot model.storedPlots
