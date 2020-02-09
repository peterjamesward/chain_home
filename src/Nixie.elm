module Nixie exposing (nixieDisplay)

-- Displays integer using simulation of "Nixie" tubes.

import Element as E exposing (..)


nixieDigit d =
    E.image
        [ E.width (E.px 30)
        , E.height (E.px 45)
        ]
        { src = "../resources/nixie" ++ String.fromInt d ++ ".png"
        , description = ""
        }


toDigits digits x =
    case digits of
        0 ->
            []

        _ ->
            let
                lastDigit =
                    modBy 10 x

                precedingDigits =
                    x // 10
            in
            toDigits (digits - 1) precedingDigits ++ [ lastDigit ]


nixieDisplay digits value =
    row [] <|
        List.map
            nixieDigit
            (toDigits digits value)
