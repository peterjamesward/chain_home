module Receiver exposing (dipoleXreceiving, dipoleYreceiving, goniometerMix)

import Echo exposing (Echo)

{-  Here we will have:
    1. Hight set of cross dipoles (good at seeing way out)
    2. Low set of cross dipoles (better at close range)
    3. Goniometer to blend signals from cross dipoles
    4. Gonio in height mode to blend low and high signals
-}

import LobeFunctions exposing (..)

{- Should be straightforward. We receive the echoes at each 
   dipole. The strength is diminished by the lobe functions.
   Also, the phase depending on which side of the dipole the
   signal hits (applies mostly to the transverse 'Y' dipole).
   In data structure terms, we're probably still dealing with Echoes.
-}

-- Dipole X is oriented along line of shoot.
dipoleXreceiving : List Echo -> List Echo

dipoleXreceiving echoes = 
    List.map (\echo ->  { echo  | amplitude = abs <| echo.amplitude 
                                    * (rxHorizLobe echo.theta) 
                                    * (rxHiVertLobe echo.alpha)
                                , phase = if (abs echo.theta) > pi/2 
                                    then 2*pi - echo.phase 
                                    else echo.phase
                        }
    ) echoes

dipoleYreceiving : List Echo -> List Echo

dipoleYreceiving echoes = 
    List.map (\echo ->  { echo  | amplitude = abs <| echo.amplitude 
                                                     * (rxHorizLobe (echo.theta + pi/2))
                                                     * (rxHiVertLobe echo.alpha)
                                , phase = if echo.theta < 0
                                          then 2*pi - echo.phase 
                                          else echo.phase
                        }
    ) echoes

-- I am tempted here to NOT combine the echoes but simply to make a single
-- list in which each echo has, if you like, an X version and a Y version, 
-- and we let the skyline rotine do its thing. That's not a bad first idea.
goniometerMix : Float -> List Echo -> List Echo -> List Echo
goniometerMix angle xEchoes yEchoes =
    let    
        adjustedXechoes = List.map (\e -> {e | amplitude = e.amplitude * cos angle}) xEchoes
        adjustedYechoes = List.map (\e -> {e | amplitude = e.amplitude * cos angle}) yEchoes
    in 
        adjustedXechoes ++ adjustedYechoes