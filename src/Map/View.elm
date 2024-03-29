module Map.View exposing (..)

{-
   This will display actual tracks of raids and the user's plots.
   ?? What is trigger for plot being stored - the sequence has no clear end ??
   Note that we do not try to correlate. In general we may not know which raid the user is plotting.
-}

import Config exposing (bawdsey)
import Constants exposing (lightCharcoal)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as H
import Messages exposing (Msg(..))
import Model exposing (Model)
import Svg exposing (..)
import Svg.Attributes as A exposing (..)
import Types exposing (PlotType(..), RecordedPlot)
import Utils exposing (edges, imageLocation, showExplanation)


squareSize =
    -- This is 'tuned' to suit the map image.
    150


imageDimensions =
    String.fromInt <| squareSize * 4


mapScale =
    -- As is this but it really should be a properly derived metric.
    toFloat squareSize / 100000.0


stationPos =
    -- Pin our station on the map. This needs to be a station attribute because the calculator needs it.
    ( (1.0 + bawdsey.gridSquareEasting) * squareSize
    , (3.0 - bawdsey.gridSquareNorthing) * squareSize
    )


mapPageLandscape : Model -> Element Msg
mapPageLandscape model =
    row
        [ centerX
        , paddingEach { edges | left = 40, right = 40, top = 50 }
        , Element.spacing 20
        , Element.width Element.fill
        ]
        [ theMap model
        , column
            [ Element.spacing 50
            , alignTop
            , moveDown 100
            ]
            [ checkBoxShowActualTrace model.actualTraceVisibleOnMap
            , checkBoxShowRangeCircle model.rangeCircleVisibleOnMap
            ]
        ]


mapPagePortrait : Model.Model -> Element Msg
mapPagePortrait model =
    column
        [ centerX
        , alignTop
        , paddingEach { edges | left = 50, right = 50 }
        , Element.spacing 20
        , Element.width Element.fill
        ]
        [ row
            [ Element.width Element.fill ]
            [ theMap model
            ]
        , checkBoxShowActualTrace model.actualTraceVisibleOnMap
        , checkBoxShowRangeCircle model.rangeCircleVisibleOnMap
        ]


mapPage : Model.Model -> Element Msg
mapPage model =
    case model.outputDevice.orientation of
        Landscape ->
            mapPageLandscape model

        Portrait ->
            mapPagePortrait model


explainMapPage =
    """
Your plots are relayed to Filter Rooms and placed
on the map. The filterer combines your plots with other information
and deduces the most likely position of the raids.

You should make a fresh plot for each raid every minute or thereabouts, so the
track of the raid becomes clear.

The map is provided only so that you can see your own plots!
"""


checkBoxShowRangeCircle visible =
    Input.checkbox
        [ Element.height (px 40)
        , Border.width 1
        , Border.rounded 5
        , Border.color lightCharcoal
        , Font.color lightCharcoal
        , centerY
        , padding 10
        ]
        { onChange = SetRangeCircleVisible
        , checked = visible
        , label =
            Input.labelRight
                [ htmlAttribute <| H.style "-webkit-user-select" "none"
                ]
            <|
                Element.text "Show the 100 mile range limit"
        , icon = Input.defaultCheckbox
        }


checkBoxShowActualTrace visible =
    Input.checkbox
        [ Element.height (px 40)
        , Border.width 1
        , Border.rounded 5
        , Border.color lightCharcoal
        , Font.color lightCharcoal
        , centerY
        , padding 10
        ]
        { onChange = SetActualTraceVisible
        , checked = visible
        , label =
            Input.labelRight
                [ htmlAttribute <| H.style "-webkit-user-select" "none"
                ]
            <|
                Element.text "Show the actual locations"
        , icon = Input.defaultCheckbox
        }


theMap : Model -> Element Msg
theMap model =
    -- Sizing this is a bit of a fiddle. Probably cause of stuff I don't know.
    el
        ([ centerX -- Needed!
         , centerY
         , Element.width (px 600)
         ]
            ++ showExplanation model.explainModeMap explainMapPage
        )
    <|
        Element.html <|
            svg
                [ viewBox ("0 0 " ++ imageDimensions ++ " " ++ imageDimensions)
                , A.width "600px"
                , A.height "600px"
                ]
            <|
                [ Svg.image
                    [ x "0"
                    , y "0"
                    , A.width imageDimensions
                    , A.height imageDimensions
                    , xlinkHref <| imageLocation ++ "east_anglia.png"
                    ]
                    []
                ]
                    ++ (if model.actualTraceVisibleOnMap then
                            List.concatMap (plotPlotter << .positionHistory) model.targets

                        else
                            []
                       )
                    ++ plotPlotter (List.take 1 model.storedPlots)
                    ++ ourStation model


plotPlotter plots =
    -- Note that the plots are associated with the polar targets.
    -- We shall use the same conversion from range and bearing to map coordinates as we
    -- do for the calculator grid so this should be fair.
    -- Remember station is notionally in central square.
    -- Scalewise, our map is made of 100km squares and they occupy 'squareSize' on the screen.
    let
        ( stationX, stationY ) =
            stationPos

        timedPlotStyles reduction =
            [ r <| String.fromFloat <| 4.0 / toFloat reduction
            , stroke "navy"
            , strokeWidth "1"
            , A.fill "navy"
            ]

        userPlotStyles reduction =
            [ r <| String.fromFloat <| 5.0 / toFloat reduction
            , stroke "navy"
            , strokeWidth "1"
            , A.fill "yellow"
            ]

        correlatedPlotStyles strength reduction =
            [ r "0" --<| String.fromFloat <| 1.0 / toFloat reduction
            , stroke "orange"
            , strokeWidth "0"
            ]

        plotStyles plotType strength reduction =
            case plotType of
                UserPlot ->
                    userPlotStyles reduction

                TimedPlot ->
                    timedPlotStyles reduction

                ActualPlot ->
                    correlatedPlotStyles strength reduction

        plotArrow reduction plot =
            Svg.circle
                ([ cx <| String.fromFloat <| stationX + plot.range * sin plot.bearing * mapScale
                 , cy <| String.fromFloat <| stationY - plot.range * cos plot.bearing * mapScale -- y is +ve downwards!
                 ]
                    ++ plotStyles plot.plotType plot.strength reduction
                )
                []
    in
    List.map2 plotArrow (List.range 1 5) plots


ourStation model =
    let
        ( stationX, stationY ) =
            stationPos
    in
    if model.rangeCircleVisibleOnMap then
        [ Svg.circle
            [ cx <| String.fromFloat <| stationX
            , cy <| String.fromFloat <| stationY
            , r "7"
            , stroke "green"
            , strokeWidth "2"
            , A.fill "blue"
            ]
            []
        , Svg.circle
            [ cx <| String.fromFloat <| stationX
            , cy <| String.fromFloat <| stationY
            , r <| String.fromFloat <| 1.6 * squareSize
            , stroke "red"
            , strokeWidth "1"
            , A.fill "none"
            ]
            []
        ]

    else
        []
