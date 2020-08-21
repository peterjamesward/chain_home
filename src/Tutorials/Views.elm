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
            Just <| tutorialInterpretCalculator model


tutorialTextBox : Model -> List (Attribute Msg) -> Element Msg
tutorialTextBox model adjustments =
    -- Use a single central text box for all tutorial text.
    -- Second argument allows caller to finesse the position
    let
        theText =
            tutorialText model
    in
    case theText of
        Nothing ->
            none

        Just someText ->
            el
                (tutorialControlHolderStyles ++ adjustments)
            <|
                tutorialControls someText


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
        , inFront <| tutorialControls calcText
        ]
        rawPage


tutorialControls someText =
    el tutorialControlHolderStyles <|
        row
            [ width fill, centerY ]
            [ el [ onClick TutorialBack, pointer ] <| text "◀︎"
            , paragraph
                [ Background.color blue
                , spacing 4
                , padding 10
                , Font.size 16
                ]
                [ text someText ]
            , el [ onClick TutorialAdvance, alignRight, pointer ] <|
                text "▶︎"
            ]


tutorialControlHolderStyles =
    [ width (px 500)
    , height (px 160)
    , centerY
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


tutorialInterpretCalculator : Model -> String
tutorialInterpretCalculator model =
    interpretCalculator model.calculator
