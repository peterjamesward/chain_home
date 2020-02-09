module Echo exposing (Echo, deriveEchoes, viewEcho)

import Constants exposing (pulseDuration, scaleWidthKilometers, wavelength)
import Html exposing (..)
import LobeFunctions exposing (..)
import Target exposing (PolarTarget)


type alias Echo =
    { r : Float
    , theta : Float
    , alpha : Float
    , phase : Float
    , duration : Float
    , amplitude : Float
    }


defaultEcho =
    { r = 0, theta = 0, alpha = 0, phase = 0, amplitude = 0, duration = 0 }


dummyFinalEcho =
    { r = scaleWidthKilometers * 1000
    , theta = 0
    , alpha = 0
    , phase = 0
    , amplitude = 0
    , duration = 0
    }


dummyInitialEcho =
    { dummyFinalEcho | r = 0 }


deriveEchoes : List PolarTarget -> Int -> List Echo
deriveEchoes targets time =
    let
        -- Ground reflection combines depending on difference in path length,
        -- and could combine positively or destructively.
        reflectedSignalAdjustment rng =
            sin <| rng / wavelength

        echoFromTarget target =
            { r = target.r
            , theta = target.theta
            , alpha = target.alpha

            -- Using phase attribute to pass time through
            -- see comments in Skyline re "beating".
            , phase = toFloat time
            , duration = pulseDuration -- microseconds
            , amplitude =
                abs <|
                    txHorizReflectedLobe target.theta
                        * txHiVertOmniLobe target.alpha
                        --* (1 + 0.2 * (reflectedSignalAdjustment target.r))
            }
    in
    List.map echoFromTarget targets



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
