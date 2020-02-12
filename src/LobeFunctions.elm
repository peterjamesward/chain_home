module LobeFunctions exposing (..)

-- Some RDF lobe functions.
-- These are approximations to curves portrayed in the supervisor's handbook.

import Types exposing (..)


txHiVertReflectedLobe α =
    --    (1 - 6 * α) * abs (sin (24 * α))
    cos (2 * α) * sin (24 * α)



-- fudge so it falls to zero at pi/2.


txHiVertOmniLobe α =
    cos α * sin (7 * α)


txLowVertOmniLobe α =
    cos α * sin (3 * α)


txLowVertReflectedLobe α =
    cos (2 * α) * abs (sin (5 * α))


txHorizReflectedLobe θ =
    cos θ *
     (pi - abs θ) -- getting desperate?


txHorizOmniLobe θ =
    cos θ


rxHorizLobe θ =
    cos θ


rxLoVertLobe α =
    sin (7 * α)


rxHiVertLobe α =
    (1 - 6 * α) * abs (sin (24 * α))


transmitANoReflect =
    Antenna txHiVertOmniLobe txHorizOmniLobe


transmitAReflector =
    Antenna txHiVertReflectedLobe txHorizReflectedLobe


transmitBNoReflect =
    Antenna txLowVertOmniLobe txHorizOmniLobe


transmitBReflector =
    Antenna txLowVertReflectedLobe txHorizReflectedLobe


receiveHigh =
    Antenna rxHiVertLobe rxHorizLobe


receiveLow =
    Antenna rxLoVertLobe rxHorizLobe
