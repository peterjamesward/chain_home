module TrainingMode exposing (advanceTutorial, tutorialMode, welcomePrompt)

{-
   This aims to be a "wrapper" of sorts for the Operator page,
   allowing us to walk through a basic scenario.
   Tried to do this as non-invasive overlay but positioning is nigh impossible.
   Other way is to make all components "tutorial" aware and just use this to
   provide common formatting etc.
-}

import Config exposing (trainingMode)
import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Messages exposing (..)
import Model exposing (Model)
import Types exposing (..)


advanceTutorial : Model -> Model
advanceTutorial current =
    { current
        | trainingScenario =
            case current.trainingScenario of
                Just TutorialWelcome ->
                    Just TutorialDescribeCRT

                Just TutorialDescribeCRT ->
                    Just TutorialRangeScale

                Just TutorialRangeScale ->
                    Just TutorialCRTTrace

                Just TutorialCRTTrace ->
                    Just TutorialIncomingRaid

                Just _ ->
                    Just TutorialWelcome

                Nothing ->
                    Nothing
        , activeConfigurations =
            if current.trainingScenario == Just TutorialCRTTrace then
                trainingMode

            else
                current.activeConfigurations
    }


welcomePrompt =
    TutorialWelcome


tutorialMode : Model -> Tutorial -> List (Attribute Msg)
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


promptText : Tutorial -> Element Msg
promptText prompt =
    el [ padding 10, width fill, alignBottom ] <|
        paragraph
            [ Background.color blue
            , Border.color white
            , Font.color white
            , spacing 4
            , padding 4
            , alignBottom
            , onClick TutorialAdvance
            ]
            [ text <|
                case prompt of
                    TutorialWelcome ->
                        """Click on these text boxes to learn about the Chain Home receiver and the operator's work."""

                    TutorialDescribeCRT ->
                        """The main feature is the Cathode Ray Tube (CRT) that displays signals
                        returned from radio wave pulses sent out from the transmitter.
                        """

                    TutorialRangeScale ->
                        """The numbers along the top show the range in miles.
                        Generally, incoming aircraft will appear towards the right hand side.
                        The operator will move the pointer at the top, using the right hand knob below, to
                        identify a particular echo for study.
                        """

                    TutorialCRTTrace ->
                        """The line on the CRT moves rapidly from left to right and "dips" for any
                        returned signals detected. The continuous movement is just noise in the system.
                        The large dips between 0 and 10 miles are from fixed objects near the station.
                        """
                    TutorialIncomingRaid ->
                        """In fact, there is an incoming raid now. It's highlighted in white but would
                        normally be green. We have to learn as much information about the raid as we can.
                        """

                    _ ->
                        "Somebody needs to write my explanation."
            ]
