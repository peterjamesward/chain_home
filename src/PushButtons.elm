module PushButtons exposing (..)

import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background exposing (..)
import Element.Border as Border exposing (..)
import Element.Font as Font exposing (..)
import Element.Input as Input exposing (..)
import Messages exposing (Msg)
import Utils exposing (choose)


toggleSwitch : String -> String -> String -> Bool -> (Bool -> Msg) -> Element Msg
toggleSwitch groupName trueLabel falseLabel state msg =
    pushButtonGroup groupName
        [ ( trueLabel, msg True, state )
        , ( falseLabel, msg False, not state )
        ]


commonStyles =
    [ Background.color flatWetAsphalt
    , Border.color black
    , Element.height (px 30)
    , Element.width fill
    , Border.rounded 4
    , center
    , Element.focused
        [ Border.color white ]
    ]


activeButtonStyle =
    [ innerGlow flatSunflower 0.5
    , Font.color vividGreen
    ]
        ++ commonStyles


inactiveButtonStyle =
    [ innerGlow flatMidnightBlue 1.0
    , Font.color lightCharcoal
    ]
        ++ commonStyles


activeAlertStyle =
    [ innerGlow flatSunflower 0.5
    , Font.color flatSunflower
    ]
        ++ commonStyles


pushButtonGroup : String -> List ( String, Msg, Bool ) -> Element Msg
pushButtonGroup groupName buttonDetails =
    let
        makeButton ( label, msg, state ) =
            Input.button
                (choose state activeButtonStyle inactiveButtonStyle)
                { onPress = Just msg
                , label = Element.text label
                }
    in
    column
        [ center
        , spacing 5
        , pointer
        ]
        (List.map makeButton buttonDetails
            ++ [ el [ centerX ]
                    (Element.text groupName)
               ]
        )


alert : String -> Bool -> Element Msg
alert label enabled =
    -- Abusing a button to give us an illuminating alert indicator.
    Input.button
        (choose enabled activeAlertStyle inactiveButtonStyle)
        { onPress = Nothing
        , label = Element.text label
        }
