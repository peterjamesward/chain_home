module LobeFunctions exposing (..)

-- Some RDF lobe functions.
-- These are approximations to curves portrayed in the supervisor's handbook.

import Types exposing (..)



-- This use of simple series is a pretty good approximation, based on curves from Wolfram Alphs


txFourStack =
    dipoleStackVerticalUnreflectedLobes 4



--abs (cos θ + cos (2 * θ) + cos (3 * θ) + cos (4 * θ))


txSixStack =
    dipoleStackVerticalUnreflectedLobes 6



--abs (cos θ + cos (2 * θ) + cos (3 * θ) + cos (4 * θ) + cos (5 * θ) + cos (6 * θ))


txEightStack =
    dipoleStackVerticalUnreflectedLobes 8



--abs (cos θ + cos (2 * θ) + cos (3 * θ) + cos (4 * θ) + cos (5 * θ) + cos (6 * θ) + cos (7 * θ) + cos (8 * θ))
-- More generally, FWIW:


dipoleStackVerticalUnreflectedLobes : Int -> Float -> Float
dipoleStackVerticalUnreflectedLobes n θ =
    List.range 1 n
        |> List.map
            (\i ->
                cos (θ * toFloat i)
            )
        |> List.sum
        |> abs


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
    cos θ
        * (pi - abs θ)



-- getting desperate?


txHorizOmniLobe θ =
    cos θ


rxHorizLobe θ =
    cos θ


rxLoVertLobe α =
    sin (7 * α)


rxHiVertLobe α =
    (1 - 6 * α) * abs (sin (24 * α))


transmitANoReflect =
    Antenna txEightStack txHorizOmniLobe


transmitAReflector =
    Antenna txHiVertReflectedLobe txHorizReflectedLobe


transmitBNoReflect =
    Antenna txSixStack txHorizOmniLobe


transmitBReflector =
    Antenna txLowVertReflectedLobe txHorizReflectedLobe


receiveHigh =
    Antenna rxHiVertLobe rxHorizLobe


receiveLow =
    Antenna rxLoVertLobe rxHorizLobe
