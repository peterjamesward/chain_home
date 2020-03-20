module Grid exposing (..)

{-
   Making this module for anything Grid related, to break dependency loop.
-}

import Model exposing (Model)
import Utils exposing (choose)


type alias GridPosition =
    { gridSquareEast : Int -- 0 = station, +ve east, -ve west
    , gridSquareNorth : Int -- 0 = station, +ve north, -ve south
    , gridSquareOffsetEast : Int -- Two digits within square
    , gridSquareOffsetNorth : Int -- Two digits within square
    }


gridLettersList : List (List String)
gridLettersList =
    [ [ "Y", "Z", "V", "W", "X", "Y", "Z" ]
    , [ "D", "E", "A", "B", "C", "D", "E" ]
    , [ "J", "K", "F", "G", "H", "J", "K" ]
    , [ "O", "P", "L", "M", "N", "O", "P" ]
    , [ "T", "U", "Q", "R", "S", "T", "U" ]
    , [ "Y", "Z", "V", "W", "X", "Y", "Z" ]
    , [ "D", "E", "A", "B", "C", "D", "E" ]
    ]


letterFromGrid : Maybe GridPosition -> Maybe String
letterFromGrid maybeGridPosition =
    -- NOTE both args are -3..+3 and be careful of sign +ve is East and North I think.
    let
        gridLetter east north =
            let
                gridRow =
                    List.drop (3 - north) gridLettersList |> List.head
            in
            case gridRow of
                Just row ->
                    List.head <| List.drop (east + 3) row

                Nothing ->
                    Nothing
    in
    case maybeGridPosition of
        Nothing ->
            Nothing

        Just gridPos ->
            gridLetter gridPos.gridSquareEast gridPos.gridSquareNorth


gridPosition : Maybe Float -> Maybe Float -> Maybe GridPosition
gridPosition range bearing =
    -- Ideally, use RAF grid, but doesn't really matter.
    -- Each lettered square is 100km. Range is in km.
    -- Without much loss of generality, assume station is centred in centre square.
    let
        easting r b =
            truncate <| r * sin b + 50

        northing r b =
            truncate <| r * cos b + 50

        eastSquareBoundary r b =
            floor <| toFloat (easting r b) / 100.0

        northSquareBoundary r b =
            floor <| toFloat (northing r b) / 100.0
    in
    case ( range, bearing ) of
        ( Just r, Just b ) ->
            Just
                { gridSquareEast = eastSquareBoundary r b
                , gridSquareNorth = northSquareBoundary r b
                , gridSquareOffsetEast = easting r b - 100 * eastSquareBoundary r b
                , gridSquareOffsetNorth = northing r b - 100 * northSquareBoundary r b
                }

        ( _, _ ) ->
            Nothing


tutorialInterpretCalculator : Model -> String
tutorialInterpretCalculator model =
    let
        gridPos =
            gridPosition model.storedAzimuthRange model.storedAzimuth

        letter =
            letterFromGrid gridPos

        twoDigits x =
            String.fromInt (x // 10) ++ String.fromInt (modBy 10 x)
    in
    "Raid at grid position "
        ++ (case ( gridPos, letter ) of
                ( Just grid, Just g ) ->
                    g
                        ++ twoDigits grid.gridSquareOffsetEast
                        ++ twoDigits grid.gridSquareOffsetNorth

                _ ->
                    "unknown"
           )
        ++ ", height "
        ++ (case model.storedElevation of
                Just h ->
                    (String.fromInt <| truncate h)
                        ++ ",000 ft"

                Nothing ->
                    "unknown"
           )
        ++ ", strength "
        ++ (case model.storedStrengthPlus of
                Just True ->
                    "more than "

                _ ->
                    ""
           )
        ++ (case model.storedStrength of
                Just n ->
                    String.fromInt n

                _ ->
                    " unknown"
           )
        ++ (case model.storedFriendly of
                Just True ->
                    ", friendly."

                _ ->
                    "."
           )
