module TrainingMode exposing (advanceTutorial, tutorialHighlighting, tutorialTextBox, welcomePrompt)

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



-- Better structure is that Tutorial~ information is local and
-- UI components don't know what will be displayed. This means that
-- we need a component identifier and we will use a type for that.


type alias TutorialEntry =
    { tutorialStep : Tutorial
    , uiComponent : UiComponent
    , tutorialText : String
    }


tutorial : List TutorialEntry
tutorial =
    [ TutorialEntry
        TutorialWelcome
        UiOperatorPage
        """Click on these text panels to learn about the Chain Home receiver and the operator's work."""
    , TutorialEntry
        TutorialDescribeCRT
        UiCRT
        """The main feature is the Cathode Ray Tube (CRT) that displays signals
        returned from radio wave pulses sent out from the transmitter.
        """
    , TutorialEntry
        TutorialRangeScale
        UiRangeSlider
        """The numbers along the top show the range in miles.
        Generally, incoming aircraft will appear towards the right hand side.
        The operator will move the pointer at the top, using the right hand knob below, to
        identify a particular echo for study."""
    , TutorialEntry
        TutorialCRTTrace
        UiCRT
        """The line on the CRT moves rapidly from left to right and "dips" for any
        returned signals detected. The continuous movement is just noise in the system.
        The large dips between 0 and 10 miles are from fixed objects near the station.
        """
    , TutorialEntry
        TutorialIncomingRaid
        UiCRT
        """In fact, there is an incoming raid now. It's highlighted in white but would
        normally be green. We have to learn as much information about the raid as we can.
        """
    , TutorialEntry
        TutorialAdjustRange
        UiRangeKnob
        """Turn the right hand knob (you can click on it or touch it and drag it around)
        and see that the range indicator moves. Try to position the range indicator at the
        left edge of the "dip" that marks the raid. This will tell you the range of the raid.
        """
    , TutorialEntry
        TutorialFindBearing
        UiGoniometer
        """Now turn the left hand knob (the goniometer) until the "dip" disappears, or
        as small as you can make it. The goniometer now indicates the bearing of the incoming
        raid. Be careful though,
        """
    ]


findNextStep : Maybe Tutorial -> Maybe Tutorial
findNextStep current =
    let
        findNextStepHelper steps =
            case steps of
                [] ->
                    Just TutorialWelcome

                step1 :: step2 :: more ->
                    if Just step1.tutorialStep == current then
                        Just step2.tutorialStep

                    else
                        findNextStepHelper (step2 :: more)

                _ :: more ->
                    findNextStepHelper more
    in
    findNextStepHelper tutorial


advanceTutorial : Model -> Model
advanceTutorial current =
    { current
        | tutorialStage = findNextStep current.tutorialStage
        , activeConfigurations =
            if current.tutorialStage == Just TutorialCRTTrace then
                trainingMode

            else
                current.activeConfigurations
    }


findMatchingStep : Maybe Tutorial -> UiComponent -> Maybe TutorialEntry
findMatchingStep tutorialStep uiComponent =
    let
        findHelper steps =
            case steps of
                [] ->
                    Nothing

                step1 :: more ->
                    if
                        Just step1.tutorialStep
                            == tutorialStep
                            && step1.uiComponent
                            == uiComponent
                    then
                        Just step1

                    else
                        findHelper more
    in
    findHelper tutorial

findStep : Maybe Tutorial -> Maybe TutorialEntry
findStep tutorialStep =
    let
        findHelper steps =
            case steps of
                [] ->
                    Nothing

                step1 :: more ->
                    if
                        Just step1.tutorialStep
                            == tutorialStep
                    then
                        Just step1

                    else
                        findHelper more
    in
    findHelper tutorial

welcomePrompt =
    TutorialWelcome


tutorialHighlighting : Model -> UiComponent -> List (Attribute Msg)
tutorialHighlighting model uiComponent =
    -- Apply highlighting if there is a tutorial detail entry that
    -- matches the ui component and the current tutorial step.
    case findMatchingStep model.tutorialStage uiComponent of
        Just _ ->
            [ Border.color flatSunflower
            , Border.rounded 10
            , Border.width 1
            , Border.glow flatSunflower 2.0
            , Border.innerGlow flatSunflower 2.0
            , alpha 0.8
            , pointer
            ]

        Nothing ->
            []


tutorialTextBox : Model -> Attribute Msg
tutorialTextBox model =
    -- Use a single central text box for all tutorial text.
    inFront <|
        case findStep model.tutorialStage of
            Nothing ->
                none

            Just step ->
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
                                [text step.tutorialText]
                    ]
                    (text "")
