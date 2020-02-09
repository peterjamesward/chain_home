module ToggleSwitch exposing (toggleSwitch)

import Constants exposing (black, flatSunflower)
import Element as E exposing (..)
import Element.Background as Background exposing (..)
import Element.Border as Border
import Element.Input as Input exposing (..)
import Messages exposing (Msg)


toggleSwitch : String -> Bool -> (Float -> Msg) -> E.Element Msg
toggleSwitch switchLabel switchState switchMessage =
    E.el [ E.centerX ] <|
        Input.slider
            [ E.height (E.px 40)
            , E.width (E.px 20)
            , E.centerX

            -- Here is where we're creating/styling the "track"
            , E.behindContent
                (E.el
                    [ E.height E.fill
                    , E.width (E.px 10)
                    , E.centerY
                    , E.centerX
                    , Background.color black
                    , Border.rounded 5
                    ]
                    E.none
                )
            ]
            { onChange = switchMessage
            , label =
                Input.labelBelow [ E.centerX ] (E.text switchLabel)
            , min = 0
            , max = 1
            , step = Just 1
            , value = if switchState then 1 else 0
            , thumb =
                Input.thumb
                    [ E.width (E.px 20)
                    , E.height (E.px 20)
                    , E.centerY
                    , Border.rounded 10
                    , Border.width 1
                    , Border.color flatSunflower
                    , Background.color flatSunflower
                    ]
            }
