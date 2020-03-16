module TrainingMode exposing (advanceTutorial, goBackInTutorial, tutorialAutomation, tutorialEntryPoint, tutorialHighlighting, tutorialTextBox)

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
import Keys exposing (Keys, updateKeys)
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
    Maybe.map .tutorialStep <| List.head tutorial


tutorial : List TutorialEntry
tutorial =
    [ TutorialEntry
        TutorialWelcome
        UiCRT
        """We'll watch the operator work out the position of an incoming raid.

        Click ► to begin.
        """
    , TutorialEntry
        TutorialIncomingRaid
        UiCRT
        """The white V shape under the 100 is a new raid.

        Click ► to see the operator start to examine the raid.
        """
    , TutorialEntry
        TutorialAdjustRange
        UiRangeKnob
        """The operator turns the range knob until the range indicator
        lines up with the left edge of the raid on the CRT.
        """
    , TutorialEntry
        TutorialFindBearing
        UiGoniometer
        """The operator 'swings' the goniometer until the 'V' on the CRT vanishes.
        The goniometer scale now shows the bearing of the raid.
        """
    , TutorialEntry
        TutorialStoreBearing
        UiGonioButton
        """Pressing the GONIO button stores the bearing in the calculator.
        """
    , TutorialEntry
        TutorialStoreRange1
        UIRangeButton
        """Pressing the RANGE button stores the range in the calculator.

        """
    , TutorialEntry
        TutorialDummy
        UiBothKnobs
        """ To Be Continued ..."""
    ]


uiExplanations : List ( UiComponent, String )
uiExplanations =
    [ ( UiCRT, """The operators screen, or CRT""" )
    , ( UiGoniometer, """Goniometer""" )
    , ( UiRangeKnob, """Range knob""" )
    , ( UiRangeScale, """Range scale (miles) and range indicator""" )
    , ( UiSwitchPanel, """Mode switches""" )
    , ( UiRaidStrength, """Raid strength entry buttons""" )
    , ( UiCalcStrength, """Estimate of number of planes in raid""" )
    , ( UiCalcGrid, """The 100km map grid square containing the raid""" )
    , ( UiCalcHeight, """The approximate height of the raid""" )
    , ( UiCalcOffset, """The approximate position within the grid square""" )
    ]


lookupUiExplanation : UiComponent -> Maybe String
lookupUiExplanation ui =
    List.head <|
        List.filterMap
            (\( i, s ) ->
                if i == ui then
                    Just s

                else
                    Nothing
            )
            uiExplanations


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


findPrevStep : Maybe Tutorial -> Maybe Tutorial
findPrevStep current =
    let
        findPrevStepHelper steps =
            case steps of
                [] ->
                    Just TutorialWelcome

                step1 :: step2 :: more ->
                    if Just step2.tutorialStep == current then
                        Just step1.tutorialStep

                    else
                        findPrevStepHelper (step2 :: more)

                _ :: more ->
                    findPrevStepHelper more
    in
    findPrevStepHelper tutorial


advanceTutorial : Model -> Model
advanceTutorial current =
    --TODO:  Code more neatly by passing the model struct through a cascade of updates.
    let
        nextStep =
            findNextStep current.tutorialStage
    in
    { current
        | tutorialStage = nextStep
        , targets =
            if nextStep == Just TutorialIncomingRaid then
                trainingMode
                -- start the incoming raid.

            else
                current.targets
    }


goBackInTutorial : Model -> Model
goBackInTutorial model =
    { model
        | tutorialStage = findPrevStep model.tutorialStage
    }


tutorialAutomation : Model -> Model
tutorialAutomation model =
    -- If we're running a tutorial, we can take over using fake keys presses.
    case model.tutorialStage of
        Nothing ->
            model

        Just TutorialAdjustRange ->
            chaseTheRaidRange model

        Just TutorialFindBearing ->
            swingThatGoniometer model

        Just TutorialStoreBearing ->
            --TODO: Factor out this copied code.
            { model
                | storedAzimuth = Just (model.goniometerAzimuth + model.station.lineOfShoot)
                , inputState = BearingRangeInput
            }

        Just TutorialStoreRange1 ->
            { model
                | storedAzimuthRange = Just (1.6 * model.rangeSlider)
                , inputState = BearingInput
            }

        Just _ ->
            model


chaseTheRaidRange : Model -> Model
chaseTheRaidRange model =
    -- Use simulated key presses to mimic the operator tracking the raid
    let
        rangeInMetres =
            Maybe.withDefault 50000 <|
                List.head <|
                    List.map .r model.polarTargets

        rangeInMiles =
            toFloat <| floor <| rangeInMetres / 1600

        currentKeys =
            model.keys
    in
    { model
        | keys =
            { currentKeys
                | rangeLeft = model.rangeSlider > rangeInMiles + 1
                , rangeRight = model.rangeSlider < rangeInMiles - 1
            }
    }


swingThatGoniometer : Model -> Model
swingThatGoniometer model =
    -- Use simulated key presses to mimic the operator tracking the raid
    let
        targetBearing =
            Maybe.withDefault 0.0 <|
                List.head <|
                    List.map .theta model.polarTargets

        currentKeys =
            model.keys
    in
    { model
        | keys =
            { currentKeys
                | gonioClock = model.goniometerAzimuth < targetBearing - degrees 1
                , gonioAnti = model.goniometerAzimuth > targetBearing + degrees 1
            }
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
    case ( findMatchingStep model.tutorialStage uiComponent, model.explainMode ) of
        ( Just _, _ ) ->
            [ Border.color flatSunflower
            , Border.rounded 10
            , Border.width 1
            , Border.glow flatSunflower 2.0
            , Border.innerGlow flatSunflower 2.0
            , alpha 0.8
            , pointer
            ]

        ( Nothing, False ) ->
            []

        ( Nothing, True ) ->
            [ inFront <| explanatoryText uiComponent
            ]


explanatoryText : UiComponent -> Element Msg
explanatoryText uiComponent =
    let
        uiComponentDescription =
            lookupUiExplanation uiComponent
    in
    case uiComponentDescription of
        Just txt ->
            el
                [ centerX
                , centerY
                , Background.color blue
                , Border.color white
                , Border.width 1
                , Border.rounded 5
                ]
            <|
                paragraph
                    [ spacing 1
                    , Font.size 16
                    , Font.family [ Font.typeface "Helvetica" ]
                    , Font.color white
                    , padding 5
                    ]
                    [ text txt ]

        _ ->
            none


tutorialTextBox : Model -> Attribute Msg
tutorialTextBox model =
    -- Use a single central text box for all tutorial text.
    inFront <|
        case findStep model.tutorialStage of
            Nothing ->
                none

            Just step ->
                el
                    [ width (px 500)
                    , centerX
                    , centerY
                    , moveUp 100
                    , moveLeft 80
                    , Border.color flatSunflower
                    , Border.width 2
                    , Border.rounded 5
                    , Font.center
                    , Font.color white
                    , Font.size 32
                    , Font.family [ Font.typeface "Helvetica" ]
                    ]
                <|
                    row [ width fill ]
                        [ el [ onClick TutorialBack, pointer ] <| text "◀︎"
                        , paragraph
                            [ Background.color blue
                            , spacing 4
                            , padding 10
                            , Font.size 16
                            ]
                            [ text step.tutorialText ]
                        , el [ onClick TutorialAdvance, alignRight, pointer ] <|
                            text "►"
                        ]
