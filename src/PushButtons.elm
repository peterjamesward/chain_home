module PushButtons exposing (..)

import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background exposing (..)
import Element.Border as Border exposing (..)
import Element.Font as Font
import Element.Input as Input exposing (..)
import Html.Attributes exposing (style)
import Messages exposing (Msg)
import Utils exposing (choose, commonStyles, disableSelection, edges)


buttonStyles =
    [ Element.height (px 20)
    , Element.width (px 20)
    , Element.centerX
    , Border.rounded 10
    , Border.widthEach { bottom = 2, top = 1, right = 2, left = 2 }
    , Element.focused []
    , Font.bold
    , htmlAttribute <| style "-webkit-user-select" "none"
    ]


indicatorStyles =
    [ Element.height (px 10)
    , Element.width (px 10)
    , Element.centerX
    , Element.spacing 5
    , Border.rounded 5
    , Border.width 2
    , Font.bold
    , htmlAttribute <| style "-webkit-user-select" "none"
    ]


actionButtonNoLabel : String -> Msg -> Element Msg
actionButtonNoLabel _ msg =
    el
        [ paddingEach { edges | top = 10 }
        , centerX
        ]
    <|
        Input.button
            (Background.color blue
                :: buttonStyles
            )
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
            , htmlAttribute <| style "-webkit-user-select" "none"
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
            ([ centerX
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
                ++ disableSelection
            )
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


raidStrengthButtonLabelLeft : String -> Msg -> Bool -> Element Msg
raidStrengthButtonLabelLeft label msg pressed =
    row [ alignRight, height <| minimum 30 <| fill ]
        [ el
            ([ paddingEach { edges | right = 10 }
             ]
                ++ disableSelection
            )
          <|
            Element.text label
        , Input.button
            (alignRight
                :: Background.color (choose pressed flatMidnightBlue green)
                :: buttonStyles
            )
            { onPress = Just msg
            , label = none
            }
        ]


raidStrengthButtonLabelRight : String -> Msg -> Bool -> Element Msg
raidStrengthButtonLabelRight label msg pressed =
    row [ alignLeft, height <| minimum 30 <| fill ]
        [ Input.button
            (alignLeft
                :: Background.color (choose pressed flatMidnightBlue green)
                :: buttonStyles
            )
            { onPress = Just msg
            , label = none
            }
        , el
            ([ paddingEach { edges | left = 10 }
             ]
                ++ disableSelection
            )
          <|
            Element.text label
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
            ([ centerX
             , paddingEach { edges | top = 5 }
             , Font.bold
             ]
                ++ disableSelection
            )
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
        [ el
            ([ centerX
             , paddingEach { edges | bottom = 5 }
             ]
                ++ disableSelection
            )
          <|
            Element.text label
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
