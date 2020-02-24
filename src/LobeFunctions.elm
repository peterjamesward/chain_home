module LobeFunctions exposing (..)

-- Some RDF lobe functions.
-- These are approximations to curves portrayed in the supervisor's handbook.
-- This use of simple series is a pretty good approximation, based on curves from Wolfram Alpha.
-- Four stack : abs (cos θ + cos (2 * θ) + cos (3 * θ) + cos (4 * θ))
-- Six stack : abs (cos θ + cos (2 * θ) + cos (3 * θ) + cos (4 * θ) + cos (5 * θ) + cos (6 * θ))
-- Eight stack : abs (cos θ + cos (2 * θ) + cos (3 * θ) + cos (4 * θ) + cos (5 * θ) + cos (6 * θ) + cos (7 * θ) + cos (8 * θ))

import Types exposing (..)
import Utils exposing (choose)


txFourStack =
    dipoleStackVerticalUnreflectedLobes 4


txSixStackUnreflected =
    dipoleStackVerticalUnreflectedLobes 6


txEightStackUnreflected =
    dipoleStackVerticalUnreflectedLobes 8



-- More generally, FWIW:


dipoleStackVerticalUnreflectedLobes : Int -> Float -> Float
dipoleStackVerticalUnreflectedLobes n α =
    List.range 1 n
        |> List.map
            (\i ->
                cos (α * toFloat i)
            )
        |> List.sum
        |> abs


dipoleStackVerticalReflectedLobes : Int -> Float -> Float
dipoleStackVerticalReflectedLobes n α =
    let
        -- Simply, the energy is reflected forwards; almost nothing behind.
        coefficient =
            choose (abs α < pi / 2) 3.0 0.1
    in
    dipoleStackVerticalUnreflectedLobes n α * coefficient


txEightStackReflected α =
    -- The main eight dipole array.
    dipoleStackVerticalReflectedLobes 8 α


txSixStackReflected α =
    -- The "gap filler" four dipole array.
    dipoleStackVerticalReflectedLobes 4 α


txHorizontalReflected θ =
    abs <| cos θ ^ 2 / (1 + θ ^ 2)


txHorizontalUnreflected θ =
    abs <| cos θ ^ 2



-- TODO: The receiver lobe functions need more scrutiny.


rxHorizLobe θ =
    cos θ


rxLoVertLobe α =
    sin (7 * α)


rxHiVertLobe α =
    (1 - 6 * α) * abs (sin (24 * α))


transmitANoReflect =
    Antenna txEightStackUnreflected txHorizontalUnreflected


transmitAReflector =
    Antenna txEightStackReflected txHorizontalReflected


transmitBNoReflect =
    Antenna txSixStackUnreflected txHorizontalUnreflected


transmitBReflector =
    Antenna txSixStackReflected txHorizontalReflected


receiveHigh =
    Antenna rxHiVertLobe rxHorizLobe


receiveLow =
    Antenna rxLoVertLobe rxHorizLobe
