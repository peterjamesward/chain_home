module Target exposing (..)

import Html exposing (..)
import Spherical exposing (..)
import Station exposing (Station)
import Types exposing (PlotType(..), Target, TargetProforma)


targetFromProforma : Station -> Int -> TargetProforma -> Target
targetFromProforma station timeNow proforma =
    { startLatitude = proforma.latitude
    , startLongitude = proforma.longitude
    , latitude = proforma.latitude
    , longitude = proforma.longitude
    , height = proforma.height
    , strength = proforma.strength
    , heading = proforma.heading
    , speed = proforma.speed
    , iff = proforma.iff
    , iffActive = False
    , startTime = timeNow
    , rangeInMetres = 0
    , theta = 0
    , alpha = 0
    , positionHistory = []
    }
        |> targetAtTime station timeNow


targetAtTime : Station -> Int -> Target -> Target
targetAtTime station timeNow target =
    -- Targets move! t in seconds to at least centisecond resolution please
    let
        deltaT =
            timeNow - target.startTime

        distanceTravelled =
            -- switch from mph to km/s
            toFloat deltaT * target.speed * 1609 / 3600000

        ( newLat, newLong ) =
            -- Use the spherical stuff.
            newPosition ( target.startLatitude, target.startLongitude )
                distanceTravelled
                target.heading

        stationPos =
            ( station.latitude, station.longitude )

        targetPos =
            ( target.latitude, target.longitude )

        rng =
            range stationPos targetPos

        heightInMetres =
            -- Convert thousands of feet to metres.
            target.height * 304.8

        theta =
            findBearingToTarget stationPos targetPos

        newPlot =
            { plotType = TimedPlot
            , time = timeNow
            , range = rng
            , bearing = theta
            , strength = target.strength
            }
    in
    { target
        | latitude = newLat
        , longitude = newLong
        , iffActive =
            case target.iff of
                Nothing ->
                    False

                Just n ->
                    -- IFF pulses every 6 seconds for 500ms duration.
                    -- Historically it was every 12 seconds for 100ms duration.
                    -- We have made it more visible.
                    modBy 12 (deltaT // 1000) == n && modBy 1000 deltaT > 0 && modBy 1000 deltaT < 500
        , rangeInMetres = rng
        , theta = theta
        , alpha = atan2 heightInMetres rng - asin (heightInMetres / meanRadius)
        , positionHistory =
            case List.head target.positionHistory of
                Just plot ->
                    if timeNow - plot.time > 60000 then
                        newPlot :: target.positionHistory

                    else
                        target.positionHistory

                Nothing ->
                    [ newPlot ]
    }


recordCurrentTargetPosition : Int -> Target -> Target
recordCurrentTargetPosition timeNow target =
    let
        newPlot =
            { plotType = ActualPlot
            , time = timeNow
            , range = target.rangeInMetres
            , bearing = target.theta
            , strength = target.strength
            }
    in
    { target | positionHistory = newPlot :: target.positionHistory }


findTargetHeight : List Target -> Float -> Maybe Float
findTargetHeight targets rangeSlider =
    -- Find target nearest to range pointer
    let
        pairsOfRangeAndHeights =
            List.map (\t -> ( abs (t.rangeInMetres - rangeSlider * 1600), t.height ))
                targets

        minByFst ( r1, h1 ) ( r2, h2 ) =
            if r1 < r2 then
                ( r1, h1 )

            else
                ( r2, h2 )
    in
    case targets of
        [] ->
            Nothing

        [ r1 ] ->
            Just r1.height

        _ ->
            Just <| Tuple.second <| List.foldl minByFst ( 1000000, 0 ) pairsOfRangeAndHeights


viewPolarTarget p1 =
    [ Html.text "r "
    , Html.text <| String.fromFloat <| p1.r
    , Html.br [] []
    , Html.text "theta "
    , Html.text <| String.fromFloat <| (180 / pi) * p1.theta
    , Html.br [] []
    , Html.text "alpha "
    , Html.text <| String.fromFloat <| (180 / pi) * p1.alpha
    , Html.br [] []
    , Html.hr [] []
    ]
