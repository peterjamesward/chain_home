module Echo exposing (deriveEchoes, viewEcho)

import Constants exposing (pulseDuration, wavelength)
import Html exposing (..)
import Station exposing (Station)
import Types exposing (..)
import Utils exposing (normalise)


deriveEchoes : Station -> Antenna -> List Target -> List Echo
deriveEchoes station txAntenna targets =
    let
        angleFromLineOfSheet target =
            target.theta - station.lineOfShoot

        strengthGivenLobes target =
            abs <|
                -- Combine the lobe functions
                txAntenna.horizontalLobeFunction (angleFromLineOfSheet target)
                    * txAntenna.verticalLobeFunction target.alpha

        echoFromDirectBeam target seq =
            { sequence = seq
            , r = target.rangeInMetres
            , theta = target.theta
            , alpha = target.alpha
            , strength =
                if target.iffActive then
                    0 - target.strength

                else
                    target.strength
            , phase = normalise (target.rangeInMetres / wavelength)
            , duration = pulseDuration
            , amplitude = strengthGivenLobes target
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
