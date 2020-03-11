module Echo exposing (Echo, deriveEchoes, viewEcho)

import Constants exposing (pulseDuration, wavelength)
import Html exposing (..)
import Target exposing (PolarTarget)
import Types exposing (Antenna)
import Utils exposing (choose, noise, notNearlyEqual, triangleWave)


type alias Echo =
    { sequence : Int
    , r : Float
    , theta : Float
    , alpha : Float
    , phase : Float
    , duration : Float
    , amplitude : Float
    }


deriveEchoes : List PolarTarget -> Antenna -> List Echo
deriveEchoes targets txAntenna =
    let
        echoFromDirectBeam target seq =
            { sequence = seq
            , r = target.r
            , theta = target.theta
            , alpha = target.alpha
            , phase = asin <| sin <| target.r / wavelength
            , duration = pulseDuration
            , amplitude =
                if target.iffActive then
                    -- When IFF cuts in we can ignore the tx lobes!
                    6
                    -- Or something else that looks nice.

                else
                    abs <|
                        -- Combine the lobe functions
                        txAntenna.horizontalLobeFunction target.theta
                            * txAntenna.verticalLobeFunction target.alpha
                            -- and ad-hoc adjustment for range
                            / (logBase 10 (1 + target.r))^1.2
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
