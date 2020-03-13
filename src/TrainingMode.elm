module TrainingMode exposing (advanceScenario, tutorialMode, welcomePrompt)

{-
   This aims to be a "wrapper" of sorts for the Operator page,
   allowing us to walk through a basic scenario.
   Tried to do this as non-invasive overlay but positioning is nigh impossible.
   Other way is to make all components "tutorial" aware and just use this to
   provide common formatting etc.
-}

import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Messages exposing (..)
import Model exposing (Model)
import Types exposing (..)


advanceScenario : Maybe Scenario -> Maybe Scenario
advanceScenario current =
    case current of
        Just ScenarioWelcome ->
            Just ScenarioDescribeCRT

        Just ScenarioDescribeCRT ->
            Just ScenarioRangeScale

        Just _ ->
            Just ScenarioWelcome

        Nothing ->
            Nothing


welcomePrompt =
    ScenarioWelcome


tutorialMode : Model -> Scenario -> List (Attribute Msg)
tutorialMode model scenario =
    if Just scenario == model.trainingScenario then
        [ Border.color flatSunflower
        , Border.rounded 10
        , Border.width 1
        , Border.glow flatSunflower 2.0
        , Border.innerGlow flatSunflower 2.0
        , alpha 0.8
        , inFront <| promptText scenario
        ]

    else
        []


promptText : Scenario -> Element Msg
promptText prompt =
    el [ padding 10, width fill, alignBottom ] <|
        paragraph
            [ Background.color blue
            , Border.color white
            , Font.color white
            , spacing 4
            , padding 4
            , alignBottom
            , onClick ScenarioAdvance
            ]
            [ text <|
                case prompt of
                    ScenarioWelcome ->
                        """Click on these text boxes to learn about the Chain Home receiver and the operator's work."""

                    ScenarioDescribeCRT ->
                        """The main feature is the Cathode Ray Tube (CRT) that displays signals
returned from radio wave pulses sent out from the transmitter.
"""

                    ScenarioRangeScale ->
                        """The numbers along the top show the range in miles. The range of this station is
                        100 miles. Generally, incoming aircraft will appear towards the right hand side.
                        The operator will move the pointer at the top, using the knob below, to identify
                        a particular echo for study.
"""

                    _ ->
                        "Somebody needs to write my explanation."
            ]
