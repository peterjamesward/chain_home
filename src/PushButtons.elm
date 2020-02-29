module PushButtons exposing (..)

import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background exposing (..)
import Element.Border as Border exposing (..)
import Element.Input as Input exposing (..)
import Messages exposing (Msg)
import Utils exposing (choose)



-- Buttons to change for historical accuracy. 2020-02-27.
-- Buttons will be small, circular with label outside (aside or below).
-- Separate indicator lights may (or not) be used to show state or recommended actions.
-- It follows that we don't have switch groups or their use as toggles (there were
-- toggles but not in our scope).


commonStyles =
    [ Element.centerX
    , Element.spacing 5
    ]


buttonStyles =
    [ Element.height (px 30)
    , Element.width (px 30)
    , Border.rounded 15
    , Border.widthEach { bottom = 3, top = 2, right = 3, left = 3 }
    , Element.focused []
    ]
        ++ commonStyles


indicatorStyles =
    [ Element.height (px 20)
    , Element.width (px 20)
    , Border.rounded 10
    , Border.width 2
    ]
        ++ commonStyles


actionButton : String -> Msg -> Element Msg
actionButton label msg =
    column commonStyles
        [ Input.button
            (Background.color blue :: buttonStyles)
            { onPress = Just msg
            , label = none
            }
        , el [ centerX ] <| Element.text label
        ]


actionButtonLabelLeft : String -> Msg -> Element Msg
actionButtonLabelLeft label msg =
    row [ alignRight ]
        [ el [] <| Element.text label
        , el [ Element.width (px 5) ] none
        , Input.button
            (alignRight :: Background.color blue :: buttonStyles)
            { onPress = Just msg
            , label = none
            }
        ]


actionButtonLabelRight : String -> Msg -> Element Msg
actionButtonLabelRight label msg =
    row [ alignLeft ]
        [ Input.button
            (alignLeft :: Background.color blue :: buttonStyles)
            { onPress = Just msg
            , label = none
            }
        , el [ Element.width (px 5) ] none
        , el [] <| Element.text label
        ]


indicator : String -> Bool -> Element Msg
indicator label state =
    let
        colour =
            choose state vividGreen paletteDarkGreen

        borderColour =
            choose state paletteSand paletteGrey
    in
    column indicatorStyles
        [ Input.button
            (Background.color colour
                :: Border.color borderColour
                :: indicatorStyles
            )
            { onPress = Nothing
            , label = none
            }
        , el [ centerX ] <| Element.text label
        ]
