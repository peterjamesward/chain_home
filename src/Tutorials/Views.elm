module Tutorials.Views exposing (..)

-- These are the functions that have to use Model information
-- to complete the views in Tutorial mode.

import Calculator.View exposing (interpretCalculator)
import Constants exposing (blue, flatSunflower, white)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Messages exposing (Msg(..))
import Model exposing (Model)
import Tutorials.ActionCodes exposing (TutorialTextFunction(..))
import Tutorials.Messages exposing (TutorialMsg(..))
import Zipper


tutorialText : Model -> Maybe String
tutorialText model =
    case model.tutorialActive of
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
    in
    el
        [ centerX
        , alignBottom
        , below <| tutorialControls calcText
        ]
        rawPage


tutorialControls someText =
    column [ centerX, width fill]
        [ el [ height (px 20) ] none
        , el tutorialControlHolderStyles <|
            row
                [ width fill, centerY ]
                [ el [ onClick (TutorialMsg TutorialBack), pointer ] <| text "◀︎"
                , paragraph
                    [ Background.color blue
                    , spacing 4
                    , padding 10
                    , Font.size 20
                    ]
                    [ text someText ]
                , el [ onClick (TutorialMsg TutorialAdvance), alignRight, pointer ] <|
                    text "▶︎"
                ]
        ]


tutorialControlHolderStyles =
    [ width fill
    , centerX
    , Background.color blue
    , Border.color flatSunflower
    , Border.width 2
    , Border.rounded 5
    , Font.center
    , Font.color white
    , Font.size 32
    , Font.family [ Font.typeface "Helvetica" ]
    ]
