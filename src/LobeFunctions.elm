module LobeFunctions exposing (..)

-- Some RDF lobe functions.
-- These are approximations to curves portrayed in the supervisor's handbook.

import Types exposing (..)


txHiVertReflectedLobe α =
    (1 - 6 * α) * abs (sin (24 * α))


txHiVertOmniLobe α =
    sin (7 * α)


txLowVertOmniLobe α =
    sin (5 * α)


txLowVertReflectedLobe α =
    (1 - 6 * α) * abs (sin (5 * α))


txHorizReflectedLobe θ =
    cos θ ^ 2


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
