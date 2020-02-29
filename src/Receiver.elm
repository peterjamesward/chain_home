module Receiver exposing (goniometerMix)

{- Here we will have:
   1. Hight set of cross dipoles (good at seeing way out)
   2. Low set of cross dipoles (better at close range)
   3. Goniometer to blend signals from cross dipoles
   4. Gonio in height mode to blend low and high signals
-}

import Echo exposing (Echo)
import LobeFunctions exposing (..)



{-
   When goniometer is aligned with the signal, the combined signal disappears.
   Simplistically, echo at theta = 0, x signal = cos 0 = 1, y signal = sin 0 = 0
   We want output for gonio 0 to be zero so x sin theta + y cos theta would be a good start.
   Echo at 90 deg, x = 0, y = 1. We want output 0 at 09 deg. Same equation works.
   But to be clever we'll mix the signals with phase, like we do in skyline.
-}
-- Let's cheat, since we know theta.


gonioMixEcho : Float -> Echo -> Echo
gonioMixEcho angle echo =
    { echo
        | amplitude = abs <| echo.amplitude * sin (echo.theta - angle)
    }


goniometerMix : Float -> List Echo -> List Echo
goniometerMix angle echoes =
    List.map (gonioMixEcho angle) echoes
