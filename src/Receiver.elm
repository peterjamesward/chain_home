module Receiver exposing (goniometerMix)

import Echo exposing (Echo)

{-  Here we will have:
    1. Hight set of cross dipoles (good at seeing way out)
    2. Low set of cross dipoles (better at close range)
    3. Goniometer to blend signals from cross dipoles
    4. Gonio in height mode to blend low and high signals
-}

import LobeFunctions exposing (..)

{- 
    When goniometer is aligned with the signal, the combined signal disappears.
    Simplistically, echo at theta = 0, x signal = cos 0 = 1, y signal = sin 0 = 0
    We want output for gonio 0 to be zero so x sin theta + y cos theta would be a good start.
    Echo at 90 deg, x = 0, y = 1. We want output 0 at 09 deg. Same equation works.
    But to be clever we'll mix the signals with phase, like we do in skyline.
-}

gonioMixEcho : Float -> Echo -> Echo
gonioMixEcho angle echo =
    let
        xDipoleAmp = abs <| echo.amplitude 
                        * (rxHorizLobe echo.theta) 
                        --* (rxHiVertLobe echo.alpha)
                        * sin angle
        yDipoleAmp = abs <| echo.amplitude 
                         * (rxHorizLobe (pi/2 - echo.theta))
                         --* (rxHiVertLobe echo.alpha)
                         * cos angle
        xDipolePhase = if (abs echo.theta) > pi/2 
                        then 2*pi - echo.phase 
                        else echo.phase
        yDipolePhase = if (abs echo.theta) > pi/2 
                        then 2*pi - echo.phase 
                        else echo.phase
        (xx,xy) = fromPolar (xDipoleAmp, xDipolePhase)
        (yx,yy) = fromPolar (yDipoleAmp, yDipolePhase)
        (amp, ph) = toPolar (xx - yx, xy - yy)
    in 
        { echo | amplitude = amp, phase = ph }


goniometerMix : Float -> List Echo -> List Echo
goniometerMix angle echoes =
    List.map (gonioMixEcho angle) echoes  

