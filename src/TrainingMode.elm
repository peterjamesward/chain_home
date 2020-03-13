module TrainingMode exposing (advanceTutorial, tutorialModeNSEW, tutorialTextBox, welcomePrompt)

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
        | tutorialStage =
            case current.tutorialStage of
                Just TutorialWelcome ->
                    Just TutorialDescribeCRT

                Just TutorialDescribeCRT ->
                    Just TutorialRangeScale

                Just TutorialRangeScale ->
                    Just TutorialCRTTrace

                Just TutorialCRTTrace ->
                    Just TutorialIncomingRaid

                Just TutorialIncomingRaid ->
                    Just TutorialAdjustRange

                Just TutorialAdjustRange ->
                    Just TutorialFindBearing

                Just _ ->
                    Just TutorialWelcome

                Nothing ->
                    Nothing
        , activeConfigurations =
            if current.tutorialStage == Just TutorialCRTTrace then
                trainingMode

            else
                current.activeConfigurations
    }


welcomePrompt =
    TutorialWelcome


tutorialModeNSEW : Model -> Tutorial -> List (Attribute Msg)
tutorialModeNSEW model scenario =
    if Just scenario == model.tutorialStage then
        [ Border.color flatSunflower
        , Border.rounded 10
        , Border.width 1
        , Border.glow flatSunflower 2.0
        , Border.innerGlow flatSunflower 2.0
        , alpha 0.8
        , pointer
        ]

    else
        []


tutorialTextBox : Model -> Attribute Msg
tutorialTextBox model =
    -- Use a single central text box for all tutorial text.
    inFront <|
        case model.tutorialStage of
            Nothing ->
                none

            Just stage ->
                el
                    [ width fill
                    , centerY
                    , centerX
                    , width (px 500)
                    , moveUp 100
                    , moveLeft 80
                    , inFront <|
                        el
                            []
                        <|
                            paragraph
                                [ Background.color blue
                                , Border.color white
                                , Border.width 1
                                , Border.rounded 5
                                , Font.color white
                                , spacing 4
                                , padding 4
                                , pointer
                                , onClick TutorialAdvance
                                ]
                                (tutorialText stage)
                    ]
                    (text "")


tutorialText stage =
    [ text <|
        case stage of
            TutorialWelcome ->
                """Click on these text boxes to learn about the Chain Home receiver and the operator's work.
                            """

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

            TutorialAdjustRange ->
                """Turn the right hand knob (you can click on it or touch it and drag it around)
                            and see that the range indicator moves. Try to position the range indicator at the
                            left edge of the "dip" that marks the raid. This will tell you the range of the raid.
                            """

            TutorialFindBearing ->
                """Now turn the left hand knob (the goniometer) until the "dip" disappears, or
                            as small as you can make it. The goniometer now indicates the bearing of the incoming
                            raid. Be careful though,
                            """

            _ ->
                "Somebody needs to write my explanation."
    ]
