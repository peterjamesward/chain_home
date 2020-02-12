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


pushButtonGroup : String -> List ( String, Msg, Bool ) -> Element Msg
pushButtonGroup groupName buttonDetails =
    let
        commonStyles =
            [ Background.color flatWetAsphalt
            , Border.color black
            , Element.height (px 30)
            , Element.width (px 100)
            , Border.rounded 4
            , center
            , Element.focused
                [ Background.color flatWetAsphalt ]
            ]

        activeButtonStyle =
            [ innerGlow flatSunflower 0.5
            , Font.color flatSunflower
            ]
                ++ commonStyles

        inactiveButtonStyle =
            [ innerGlow flatMidnightBlue 1.0
            , Font.color lightCharcoal
            ]
                ++ commonStyles

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
