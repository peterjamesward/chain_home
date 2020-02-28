module Echo exposing (Echo, artisticEchoCombiner, combineEchoes, deriveEchoes, viewEcho)

import Constants exposing (pulseDuration, scaleWidthKilometers, transmitterEffectiveHeight, wavelength)
import Html exposing (..)
import Target exposing (PolarTarget)
import Types exposing (Antenna)


type alias Echo =
    { sequence : Int
    , r : Float
    , theta : Float
    , alpha : Float
    , phase : Float
    , duration : Float
    , amplitude : Float
    }


combineEchoes : Int -> List Echo -> Float
combineEchoes time activeEchoes =
    -- Treat amplitude and phase as vector, sum components, convert back, use amplitude.
    let
        asRect =
            List.map
                (\e ->
                    fromPolar
                        ( e.amplitude
                          -- Largely artistic here, trying for convincing "beats".
                        , e.phase
                          --toFloat e.sequence * 2 * pi * toFloat time / 2000
                        )
                )
                activeEchoes

        combinedAsRect =
            List.foldl (\( x, y ) ( xAcc, yAcc ) -> ( x + xAcc, y + yAcc )) ( 0.0, 0.0 ) asRect

        ( mag, phase ) =
            toPolar combinedAsRect
    in
    mag


artisticEchoCombiner : Int -> List Echo -> Float
artisticEchoCombiner time activeEchoes =
    -- Special cases to make 1, 2, 3 or more echoes to look like the training videos.
    let
        triangleWave t =
            toFloat (abs (modBy 2000 t - 1000) - 1000) / 1000.0

        noise t =
            fractional (5000 * sin (toFloat t))

        fractional x =
            x - toFloat (truncate x)

        notNearlyEqual x1 x2 =
            (10 * abs x1 < abs x2) || (10 * abs x2 < abs x1)
    in
    abs <|
        case activeEchoes of
            [] ->
                0.0

            [ e ] ->
                e.amplitude

            [ e1, e2 ] ->
                -- This is our primary special case. There should be beating but if
                -- they are on distinct bearings and one is D/F'd out, no beating.
                -- So if amplitudes are dissimilar, treat as single echo.
                if notNearlyEqual e1.amplitude e2.amplitude then
                    e1.amplitude + e2.amplitude

                else
                    (e1.amplitude + e2.amplitude) * triangleWave time

            es ->
                noise time * combineEchoes time es


deriveEchoes : List PolarTarget -> Antenna -> Int -> List Echo
deriveEchoes targets txAntenna time =
    let
        echoFromDirectBeam target seq =
            { sequence = seq
            , r = target.r
            , theta = target.theta
            , alpha = target.alpha
            , phase = asin <| sin <| target.r / wavelength
            , duration = pulseDuration
            , amplitude =
                abs <|
                    -- Combine the lobe functions
                    txAntenna.horizontalLobeFunction target.theta
                        * txAntenna.verticalLobeFunction target.alpha
                        -- and ad-hoc adjustment for range
                        / logBase 100 (1 + target.r)
            }
    in
    List.map2 echoFromDirectBeam
        targets
    <|
        List.range 1 (List.length targets)



-- Handy function for debuggery.


viewEcho e =
    [ Html.text "r "
    , Html.text <| String.fromFloat <| e.r
    , Html.br [] []
    , Html.text "theta "
    , Html.text <| String.fromFloat <| e.theta * 180 / pi
    , Html.br [] []
    , Html.text "alpha "
    , Html.text <| String.fromFloat <| e.alpha * 180 / pi
    , Html.br [] []
    , Html.text "phase "
    , Html.text <| String.fromFloat <| e.phase
    , Html.br [] []
    , Html.text "duration "
    , Html.text <| String.fromFloat <| e.duration
    , Html.br [] []
    , Html.text "amplitude "
    , Html.text <| String.fromFloat <| e.amplitude
    , Html.br [] []
    , Html.hr [] []
    ]
