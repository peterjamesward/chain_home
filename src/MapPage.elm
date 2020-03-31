module MapPage exposing (..)

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
import Grid exposing (mapGridLettersList)
import Html.Attributes as H exposing (style)
import Messages exposing (Msg(..))
import Model exposing (Model)
import Svg exposing (..)
import Svg.Attributes as A exposing (..)
import Utils exposing (edges, motorwaySign)


squareSize =
    -- This is 'tuned' to suit the map image.
    150


imageDimensions =
    String.fromInt <| squareSize * 4


mapScale =
    -- As is this but it really should be a properly derived metric.
    toFloat squareSize / 100000.0


stationPos model =
    -- Pin our station on the map. This needs to be a station attribute because the calculator needs it.
    ( (1.0 + bawdsey.gridSquareEasting ) * squareSize
    , (3.0 - bawdsey.gridSquareNorthing) * squareSize )


mapPage : Model -> Element Msg
mapPage model =
    row
        [ centerX
        , alignTop
        , paddingEach { edges | left = 50, right = 50 }
        , Element.spacing 20
        , Element.width Element.fill
        ]
        [ theMap model
        , column [ Element.spacing 50 ]
            [ checkBoxShowActualTrace model.actualTraceVisibleOnMap
            , motorwaySign explainMapPage
            ]

        --, helpButton
        ]


explainMapPage =
    """
Your plots are relayed to the Filter Room where they are placed
on the map. The team there combines your plots with plots from other CH stations
and deduces the most likely position of the raids.
We have the advantage of being able to see exactly where the raids were!
Generally, the range is quite accurate but bearing less so.
       """


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
        [ centerX -- Needed!
        , centerY
        , Element.width (px 600)
        ]
    <|
        Element.html <|
            svg
                [ viewBox ("0 0 " ++ imageDimensions ++ " " ++ imageDimensions)
                , A.width "100%"
                , A.height "100%"
                ]
            <|
                [ Svg.image
                    [ x "0"
                    , y "0"
                    , A.width imageDimensions
                    , A.height imageDimensions
                    , xlinkHref "../resources/east_anglia.png"
                    ]
                    []
                ]
                    ++ raidTracks model
                    ++ userPlots model
                    ++ [ ourStation model, ourRange model ]


raidTracks model =
    -- Note that the plots are associated with the polar targets.
    -- We shall use the same conversion from range and bearing to map coordinates as we
    -- do for the calculator grid so this should be fair.
    -- Remember station is notionally in central square.
    -- Scalewise, our map is made of 100km squares and they occupy 'squareSize' on the screen.
    let
        ( stationX, stationY ) =
            stationPos model

        noMoreThan100Miles ( _, r, _ ) =
            r <= 160000

        raidTrack target =
            List.map plotArrow <|
                List.filter noMoreThan100Miles
                    target.positionHistory

        plotArrow ( time, range, theta ) =
            Svg.circle
                [ cx <| String.fromFloat <| stationX + range * sin theta * mapScale
                , cy <| String.fromFloat <| stationY - range * cos theta * mapScale -- y is +ve downwards!
                , r "5"
                , stroke "orange"
                , strokeWidth "1"
                , A.fill "navy"
                ]
                []
    in
    if model.actualTraceVisibleOnMap then
        List.concatMap raidTrack model.targets

    else
        []


userPlots model =
    -- Note that the plots are associated with the polar targets.
    -- We shall use the same conversion from range and bearing to map coordinates as we
    -- do for the calculator grid so this should be fair.
    -- Remember station is notionally in central square.
    -- Scalewise, our map is made of 100km squares and they occupy 'squareSize' on the screen.
    let
        ( stationX, stationY ) =
            stationPos model

        plot ( time, range, theta ) =
            Svg.circle
                [ cx <| String.fromFloat <| stationX + range * sin theta * mapScale -- x is +ve rightwards.
                , cy <| String.fromFloat <| stationY - range * cos theta * mapScale -- y is +ve downwards!
                , r "5"
                , stroke "navy"
                , strokeWidth "1"
                , A.fill "yellow"
                ]
                []
    in
    List.map plot model.storedPlots


ourStation model =
    let
        ( stationX, stationY ) =
            stationPos model
    in
    Svg.circle
        [ cx <| String.fromFloat <| stationX
        , cy <| String.fromFloat <| stationY
        , r "7"
        , stroke "green"
        , strokeWidth "2"
        , A.fill "blue"
        ]
        []


ourRange model =
    let
        ( stationX, stationY ) =
            stationPos model
    in
    Svg.circle
        [ cx <| String.fromFloat <| stationX
        , cy <| String.fromFloat <| stationY
        , r <| String.fromFloat <| 1.6 * squareSize
        , stroke "red"
        , strokeWidth "1"
        , A.fill "none"
        ]
        []
