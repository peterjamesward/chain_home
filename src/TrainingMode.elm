module TrainingMode exposing (advanceTutorial, rangeMayUpdateTutorial, tutorialEntryPoint, tutorialHighlighting, tutorialTextBox)

{-
   This aims to be a "wrapper" of sorts for the Operator page,
   allowing us to walk through a basic scenario.
   Tried to do this as non-invasive overlay but positioning is nigh impossible.
   Other way is to make all components "tutorial" aware and just use this to
   provide common formatting etc.
-}

import Config exposing (getAllTargets, trainingMode)
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


tutorialEntryPoint =
    Just TutorialDescribeCRT


tutorial : List TutorialEntry
tutorial =
    [ TutorialEntry
        TutorialDescribeCRT
        UiCRT
        """This is the operator's screen, called the CRT.
        """
    , TutorialEntry
        TutorialCRTTrace
        UiCRT
        """The line on the CRT  "dips" for any
        returned signals detected. The continuous movement is just noise in the system.
        The large dips between 0 and 10 miles are from fixed objects near the station.
        """
    , TutorialEntry
        TutorialRangeScale
        UiRangeScale
        """The numbers along the top show the range in miles.
        Generally, incoming aircraft will appear towards the right hand side.
        The operator will move the pointer at the top, using the right hand knob below, to
        identify a particular echo for study."""
    , TutorialEntry
        TutorialGoniometer
        UiGoniometer
        """This is the goniometer. It is used to estimate the bearing of a raid by
        adjusting the control until the signal disappears; the bearing is then shown
        directly on the dial. The button to the right is used to store the setting in
        the electrical calculator.
        """
    , TutorialEntry
        TutorialRangeKnob
        UiRangeKnob
        """The smaller knob without a dial is used to move the range pointer along the
        range scale to identify the range of the raid that the operator is currently
        focusing on. Again, the button is used to store the setting in the calculator.
        """
    , TutorialEntry
        TutorialABSwitch
        UiAB
        """The operator can choose to look at signals from two aerial systems - 'A"
        and 'B'. 'A' is usually better at longer range and 'B' at close range, but
        both systems have gaps in their coverage. The lights show which system is
        active, and whether in Height or Direction Finding (D/F) mode.
        """
    , TutorialEntry
        TutorialHeight
        UiHeight
        """The operator alternates between working out the bearing of a range and working
        out the height. Both modes use the goniometer to try and make the signal on the
        CRT as small as possible. Height is only approximate and is done by the electrical
        calculator for speed.
        """
    , TutorialEntry
        TutorialSense
        UiSense
        """Radar signals spread from the rear of the transmitter as well as the front.
        This means that aircraft can be detectd behind the station (over land). The
        'Sense' button turns on a reflector behind that transmitter aerial that sends
        most of the signal forwards. With this on, signals from behind become weaker and
        signals from in front become stronger.
        """
    , TutorialEntry
        TutorialOperatorPrompts
        UiOperatorPrompts
        """These two lights prompt the operator to store the goniometer and range settings
        in the electrical calculator. The operator will do this first in D/F mode and then in Height mode.
        """
    , TutorialEntry
        TutorialRaidStrength
        UiRaidStrength
        """The operator presses one of the numbered buttons to show her estimate of the
        number of aircraft in a raid. The '+' button can be used to indicate that the number
        is higher than the button label. The 'F' button is used to show that a raid is known
        to be a friendly aircraft, typically because they use IFF signals.
        """
    , TutorialEntry
        TutorialClear
        UiClear
        """The 'Clear' button removes stored entries from the calculator.
        """
    , TutorialEntry
        TutorialIncomingRaid
        UiCRT
        """There is an incoming raid now. It's highlighted in white but would
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
        """Now turn the left hand knob (the goniometer) until the "dip" disappears, or is
        as small as you can make it. The goniometer now indicates the bearing of the incoming
        raid. The raid is travelling at 200mph so you need to keep an eye on the range.
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
    let
        nextStep =
            findNextStep current.tutorialStage
    in
    { current
        | tutorialStage = nextStep
        , targets =
            if nextStep == Just TutorialIncomingRaid then
                trainingMode

            else
                current.targets
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
                            [ width (px 500)
                            , height (px 200)
                            , centerX
                            , centerY
                            , moveUp 100
                            , Background.color blue
                            , Border.color white
                            , Border.width 1
                            , Border.rounded 5
                            , onClick TutorialAdvance
                            ]
                        <|
                            paragraph
                                [ Font.color white
                                , spacing 4
                                , padding 4
                                , pointer
                                ]
                                [ text step.tutorialText ]
                    ]
                    (text "")


rangeMayUpdateTutorial : Model -> Maybe Tutorial
rangeMayUpdateTutorial model =
    let
        nearEnough =
            True
    in
    case model.tutorialStage of
        Nothing ->
            Nothing

        Just TutorialAdjustRange ->
            if nearEnough then
                findNextStep model.tutorialStage

            else
                model.tutorialStage

        _ ->
            model.tutorialStage
