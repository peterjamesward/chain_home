module PushButtons exposing (..)

import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background exposing (..)
import Element.Border as Border exposing (..)
import Element.Font as Font
import Element.Input as Input exposing (..)
import Messages exposing (Msg)
import Utils exposing (choose, commonStyles, edges)



-- Buttons to change for historical accuracy. 2020-02-27.
-- Buttons will be small, circular with label outside (aside or below).
-- Separate indicator lights may (or not) be used to show state or recommended actions.
-- It follows that we don't have switch groups or their use as toggles (there were
-- toggles but not in our scope).


buttonStyles =
    [ Element.height (px 20)
    , Element.width (px 20)
    , Element.centerX
    , Border.rounded 10
    , Border.widthEach { bottom = 2, top = 1, right = 2, left = 2 }
    , Element.focused []
    , Font.bold
    ]


indicatorStyles =
    [ Element.height (px 10)
    , Element.width (px 10)
    , Element.centerX
    , Element.spacing 5
    , Border.rounded 5
    , Border.width 2
    , Font.bold
    ]


actionButtonNoLabel : String -> Msg -> Element Msg
actionButtonNoLabel _ msg =
    Input.button
        (Background.color blue :: buttonStyles)
        { onPress = Just msg
        , label = none
        }


actionButtonLabelAbove : String -> Msg -> Element Msg
actionButtonLabelAbove label msg =
    column commonStyles
        [ el
            [ centerX
            , paddingEach { edges | bottom = 5 }
            , Font.bold
            ]
          <|
            Element.text label
        , Input.button
            (Background.color blue
                :: buttonStyles
            )
            { onPress = Just msg
            , label = none
            }
        ]


actionButtonLabelAboveWithIndicator : String -> Bool -> Msg -> Element Msg
actionButtonLabelAboveWithIndicator label state msg =
    -- Hell, these buttons are becoming too specialised!
    column commonStyles
        [ el
            [ centerX
            , paddingEach { edges | bottom = 5 }
            , above
                (el
                    [ centerX
                    , paddingEach { edges | bottom = 5 }
                    ]
                 <|
                    indicatorNoLabel state
                )
            ]
          <|
            Element.text label
        , Input.button
            (Background.color blue
                :: buttonStyles
            )
            { onPress = Just msg
            , label = none
            }
        ]


actionButtonLabelLeft : String -> Msg -> Element Msg
actionButtonLabelLeft label msg =
    row [ alignRight, height <| minimum 30 <| fill ]
        [ el [ paddingEach { edges | right = 10 } ] <| Element.text label
        , Input.button
            (alignRight :: Background.color green :: buttonStyles)
            { onPress = Just msg
            , label = none
            }
        ]


actionButtonLabelRight : String -> Msg -> Element Msg
actionButtonLabelRight label msg =
    row [ alignLeft, height <| minimum 30 <| fill ]
        [ Input.button
            (alignLeft :: Background.color green :: buttonStyles)
            { onPress = Just msg
            , label = none
            }
        , el [ paddingEach { edges | left = 10 } ] <| Element.text label
        ]


indicatorLabelBelow : String -> Bool -> Element Msg
indicatorLabelBelow label state =
    let
        colour =
            choose state vividGreen paletteDarkGreen

        borderColour =
            choose state paletteSand paletteGrey
    in
    column commonStyles
        [ Input.button
            (Background.color colour
                :: Border.color borderColour
                :: indicatorStyles
            )
            { onPress = Nothing
            , label = none
            }
        , el
            [ centerX
            , paddingEach { edges | top = 5 }
            , Font.bold
            ]
          <|
            Element.text label
        ]


indicatorLabelAbove : String -> Bool -> Element Msg
indicatorLabelAbove label state =
    let
        colour =
            choose state vividGreen paletteDarkGreen

        borderColour =
            choose state paletteSand paletteGrey
    in
    column commonStyles
        [ el [ centerX, paddingEach { edges | bottom = 5 } ] <| Element.text label
        , Input.button
            (Background.color colour
                :: Border.color borderColour
                :: indicatorStyles
            )
            { onPress = Nothing
            , label = none
            }
        ]


indicatorNoLabel : Bool -> Element Msg
indicatorNoLabel state =
    let
        colour =
            choose state vividGreen paletteDarkGreen

        borderColour =
            choose state paletteSand paletteGrey
    in
    Input.button
        (Background.color colour
            :: Border.color borderColour
            :: focused []
            :: indicatorStyles
        )
        { onPress = Nothing
        , label = none
        }
