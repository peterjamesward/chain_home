module Tutorials.Views exposing (..)

-- These are the functions that have to use Model information
-- to complete the views in Tutorial mode.

import Calculator.View exposing (interpretCalculator)
import Constants exposing (blue, flatSunflower, lightGray, midGray, tutorialBackground, white)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Messages exposing (Msg(..))
import Model exposing (Model)
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
    in
    el
        [ centerX
        , width fill
        , alignBottom
        , below <| tutorialControls calcText
        ]
        rawPage

--TODO: Cater for interactive and kiosk modes!

tutorialControlsSAVE someText =
    row [ centerX, width fill, alignBottom ]
        [ el [ height (px 15) ]
            none
        , paragraph
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
        ]


tutorialControls someText =
    row [ centerX, width fill, alignBottom, spacing 10 ]
        [ el [ height (px 15) ]
            none
        , button
            [ Background.color midGray
            , Font.size 40
            , Font.color white
            ]
            { onPress = Just <| TutorialMsg TutorialBack
            , label = el [ centerX ] <| text "◀︎"
            }
        , paragraph
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
        , button
            [ Background.color midGray
            , Font.size 40
            , Font.color white
            ]
            { onPress = Just <| TutorialMsg TutorialAdvance
            , label = el [ centerX ] <| text "►"
            }
        , el [ height (px 15) ]
            none
        ]
