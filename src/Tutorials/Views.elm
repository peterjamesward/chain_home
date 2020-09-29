module Tutorials.Views exposing (..)

-- These are the functions that have to use Model information
-- to complete the views in Tutorial mode.

import Calculator.View exposing (interpretCalculator)
import Constants exposing (blue, flatSunflower, lightGray, midGray, tutorialBackground, white)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Messages exposing (Msg(..))
import Model exposing (ApplicationMode(..), Model)
import Tutorials.ActionCodes exposing (TutorialTextFunction(..))
import Tutorials.Messages exposing (TutorialMsg(..))
import Tutorials.Model exposing (Tutorial)
import Zipper


tutorialText : Tutorial -> Model -> Maybe String
tutorialText tutorial model =
    case tutorial of
        Nothing ->
            Nothing

        Just tut ->
            applyTextFunction (Zipper.current tut).tutorialText model


applyTextFunction : TutorialTextFunction -> Model -> Maybe String
applyTextFunction fnCode model =
    case fnCode of
        TextConstant s ->
            Just s

        TextInterpretCalculator ->
            Just <| interpretCalculator model.calculator


viewCalculatorInTutorial : Model -> Element Msg
viewCalculatorInTutorial model =
    let
        rawPage =
            Calculator.View.view model.outputDevice model.calculator

        calcText =
            Calculator.View.interpretCalculator model.calculator

        showArrows =
            case model.applicationMode of
                TutorialMode _ ->
                    True

                Model.KioskMode _ _ ->
                    False

                InteractiveMode ->
                    False
    in
    el
        [ centerX
        , width fill
        , alignBottom
        , below <| tutorialControls showArrows calcText
        ]
        rawPage


tutorialControls : Bool -> String -> Element Msg
tutorialControls withArrows someText =
    let
        arrowStyles =
            [ Background.color midGray
            , Font.size 40
            , Font.color white
            , width (px 60)
            , height (px 60)
            , Border.color flatSunflower
            , Border.width 2
            , Border.rounded 2
            , Border.glow flatSunflower 0.3
            ]

        backArrow =
            button arrowStyles
                { onPress = Just <| TutorialMsg TutorialBack
                , label = el [ centerX ] <| text "◀︎"
                }

        textBlock =
            paragraph
                [ Background.color tutorialBackground
                , alignBottom
                , centerX
                , spacing 4
                , width fill
                , padding 10
                , Font.size 28
                , Font.family [ Font.typeface "Courier New" ]
                , Font.color lightGray
                ]
                [ text someText ]

        forwardArrow =
            button arrowStyles
                { onPress = Just <| TutorialMsg TutorialAdvance
                , label = el [ centerX ] <| text "►"
                }
    in
    row [ centerX, width fill, alignBottom, spacing 10 ] <|
        if withArrows then
            [ el [ height (px 15) ]
                none
            , backArrow
            , textBlock
            , forwardArrow
            , el [ height (px 15) ]
                none
            ]

        else
            [ el [ height (px 15) ]
                none
            , textBlock
            , el [ height (px 15) ]
                none
            ]
