module Target exposing (..)

import Html exposing (..)
import Spherical exposing (..)
import Station exposing (Station)
import Types exposing (PolarTarget, Target)


mapToPolar : Station -> Target -> PolarTarget
mapToPolar station target =
    -- Convert from Cartesian (and imperial) map coordinates to
    -- polar (and metric) relative to station position and line of shoot.
    -- Note that target height are specified in '000 feet.
    let
        stationPos =
            ( station.latitude, station.longitude )

        targetPos =
            ( target.latitude, target.longitude )

        rng =
            range stationPos targetPos

        heightInMetres =
            target.height * 304.8
    in
    { r = rng
    , theta = bearing stationPos targetPos - station.lineOfShoot
    , alpha = atan2 heightInMetres rng - asin (heightInMetres / meanRadius)
    , iff = target.iff
    , iffActive = target.iffActive
    , tutorial = target.tutorial
    , positionHistory = []
    }


trackTargetPositionHistory : Int -> List PolarTarget -> List PolarTarget
trackTargetPositionHistory timeNow targets =
    let
        trackHistory target =
            case List.head target.positionHistory of
                Just ( prevTime, _, _ ) ->
                    if timeNow - prevTime > 60000 then
                        { target
                            | positionHistory = ( timeNow, target.r, target.theta ) :: target.positionHistory
                        }

                    else
                        target

                Nothing ->
                    { target
                        | positionHistory = [ ( timeNow, target.r, target.theta ) ]
                    }
    in
    List.map trackHistory targets


targetAtTime : Int -> Target -> Target
targetAtTime timeNow target =
    -- Targets move! t in seconds to at least centisecond resolution please
    let
        deltaT =
            timeNow - target.startTime

        distanceTravelled =
            -- switch from mph to km/s
            toFloat deltaT * target.speed * 1609 / 3600000

        ( newLat, newLong ) =
            -- Use the spherical stuff.
            newPosition ( target.latitude, target.longitude )
                distanceTravelled
                target.bearing
    in
    { target
        | latitude = newLat
        , longitude = newLong
        , iffActive =
            case target.iff of
                Nothing ->
                    False

                Just n ->
                    modBy 12 (deltaT // 1000) == n && modBy 1000 deltaT > 0 && modBy 1000 deltaT < 500
    }


findTargetElevation : List Target -> List PolarTarget -> Float -> Maybe Float
findTargetElevation targets polarTargets range =
    -- Find target nearest to range pointer
    let
        pairsOfRangeAndHeights =
            List.map2 (\r1 p1 -> ( abs (p1.r - range * 1600), r1.height ))
                targets
                polarTargets

        minByFst ( r1, h1 ) ( r2, h2 ) =
            if r1 < r2 then
                ( r1, h1 )

            else
                ( r2, h2 )
    in
    case ( targets, polarTargets ) of
        ( [], [] ) ->
            Nothing

        ( [ r1 ], [ _ ] ) ->
            Just r1.height

        ( _, _ ) ->
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
