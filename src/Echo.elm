module Echo exposing (Echo, combineEchoes, deriveEchoes, viewEcho)

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
                        , toFloat e.sequence * 2 * pi * (toFloat <| modBy 2000 time) / 2000
                        )
                )
                activeEchoes

        combinedAsRect =
            List.foldl (\( x, y ) ( xAcc, yAcc ) -> ( x + xAcc, y + yAcc )) ( 0.0, 0.0 ) asRect

        ( mag, phase ) =
            toPolar combinedAsRect
    in
    mag



{-
   Going to create TWO echoes for each target, one for the direct ray
   and one for the reflected ray from the transmitter ground image. Watch out for phase.
   Did that but either everything beats or nothing beats.
   I see no way to make only two or more targets beat without frequency beats.
   So that's what we'll do. Again.
-}


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
