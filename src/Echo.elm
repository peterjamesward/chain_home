module Echo exposing (Echo, deriveEchoes, viewEcho)

import Constants exposing (pulseDuration, scaleWidthKilometers, transmitterEffectiveHeight, wavelength)
import Html exposing (..)
import Target exposing (PolarTarget)
import Types exposing (Antenna)


type alias Echo =
    { r : Float
    , theta : Float
    , alpha : Float
    , phase : Float
    , duration : Float
    , amplitude : Float
    }



{-
   Going to create TWO echoes for each target, one for the direct ray
   and one for the reflected ray from the transmitter ground image. Watch out for phase.
-}


deriveEchoes : List PolarTarget -> Antenna -> Int -> List Echo
deriveEchoes targets txAntenna time =
    let
        indirectRange target =
            sqrt (target.r ^ 2 + (2 * transmitterEffectiveHeight) ^ 2)

        indirectElevation target =
            (target.r * sin target.alpha + 2 * transmitterEffectiveHeight) / target.r |> asin

        phaseShift target =
            asin <| sin <| target.r / wavelength

        indirectPhase target =
            (*) -1.0 <| asin <| sin <| indirectRange target / wavelength

        echoFromDirectBeam target =
            { r = target.r
            , theta = target.theta
            , alpha = target.alpha
            , phase = phaseShift target
            , duration = pulseDuration
            , amplitude =
                abs <|
                    txAntenna.horizontalLobeFunction target.theta
                        * txAntenna.verticalLobeFunction target.alpha
                        / logBase 100 (1 + target.r)
            }

        echoFromReflectedBeam target =
            { r = indirectRange target
            , theta = target.theta
            , alpha = indirectElevation target
            , phase = indirectPhase target
            , duration = pulseDuration
            , amplitude =
                abs <|
                    txAntenna.horizontalLobeFunction target.theta
                        * txAntenna.verticalLobeFunction target.alpha
                        / logBase 100 (1 + target.r)
            }
    in
    List.map echoFromDirectBeam targets
        ++ List.map echoFromReflectedBeam targets



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
