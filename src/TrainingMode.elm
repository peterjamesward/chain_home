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
import ElevationCurves exposing (estimateEchoInElevationMode)
import Keys exposing (Keys, updateKeys)
import Messages exposing (..)
import Model exposing (Model)
import Types exposing (..)



-- Better structure is that Tutorial~ information is local and
-- UI components don't know what will be displayed. This means that
-- we need a component identifier and we will use a type for that.


type alias TutorialEntry =
    { tutorialStep : Tutorial -- The unique step identifier
    , uiComponent : UiComponent -- The UI component to be highlighted
    , entryActions : TutorialActionList -- Changes to the model so we can be idempotent
    , stateActions : TutorialActionList -- Things we must do whilst in this state.
    , exitActions : TutorialActionList -- Changes to the model, to make sure we exit cleanly
    , tutorialText : String -- What we display to the user.
    }


tutorialEntryPoint =
    Maybe.map .tutorialStep <| List.head tutorial


tutorial : List TutorialEntry
tutorial =
    [ TutorialEntry
        TutorialWelcome
        UiCRT
        noEntryActions
        noStateActions
        noExitActions
        """We'll watch the operator work out the position of an incoming raid.

        Click ► to begin.
        """
    , TutorialEntry
        TutorialIncomingRaid
        UiCRT
        [ setupTutorialRaid ]
        noStateActions
        noExitActions
        """The white V shape under the 100 on the 'tube' is a new raid.
        Click ► to see the operator start to examine the raid.
        """
    , TutorialEntry
        TutorialAdjustRange
        UiRangeKnob
        noEntryActions
        [ chaseTheRaidRange True ]
        [ chaseTheRaidRange False ]
        """The operator turns the range knob until the pointer
        lines up with the left edge of the raid on the CRT.
        """
    , TutorialEntry
        TutorialFindBearing
        UiGoniometer
        noEntryActions
        [ swingThatGoniometer True ]
        [ swingThatGoniometer False ]
        """The operator 'swings' the gonio until the V on the CRT vanishes.
        """
    , TutorialEntry
        TutorialStoreBearing
        UiGonioButton
        noEntryActions
        noStateActions
        [ tutorialStoreBearing ]
        """Pressing the GONIO button stores the bearing in the calculator.
        """
    , TutorialEntry
        TutorialStoreRange1
        UIRangeButton
        noEntryActions
        noStateActions
        [ tutorialStoreRange1 ]
        """Pressing the RANGE button stores the range in the calculator.

        """
    , TutorialEntry
        TutorialHeightMode
        UiHeight
        [ tutorialHeightMode ]
        noStateActions
        noExitActions
        """The operator will now try to work out the height.
        """
    , TutorialEntry
        TutorialFindElevation
        UiGoniometer
        noEntryActions
        [ tutorialSeekElevation True ]
        [ tutorialSeekElevation False ]
        """The operator swings the gonio again, to minimise the V.
        """
    , TutorialEntry
        TutorialDummy
        UiBothKnobs
        noEntryActions
        noStateActions
        noExitActions
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


type alias TutorialAction =
    Model -> Model


type alias TutorialActionList =
    List TutorialAction


noEntryActions : TutorialActionList
noEntryActions =
    []


noExitActions : TutorialActionList
noExitActions =
    []


noStateActions : TutorialActionList
noStateActions =
    []


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
    let
        nextStepId =
            findNextStep current.tutorialStage

        currentStep =
            findStep current.tutorialStage

        maybeNextStep =
            findStep nextStepId
    in
    case ( currentStep, maybeNextStep ) of
        ( Just thisStage, Just nextStep ) ->
            applyActions nextStep.entryActions <|
                applyActions thisStage.exitActions <|
                    { current | tutorialStage = nextStepId }

        ( Just thisStage, Nothing ) ->
            applyActions thisStage.exitActions <|
                { current | tutorialStage = Nothing }

        _ ->
            current


applyActions : TutorialActionList -> Model -> Model
applyActions actions model =
    -- Applicative style of applying functions in a chain.
    List.foldl (\a m -> a m) model actions


goBackInTutorial : Model -> Model
goBackInTutorial current =
    let
        prevStepId =
            findPrevStep current.tutorialStage

        currentStep =
            findStep current.tutorialStage

        maybePrevStep =
            findStep prevStepId
    in
    case ( currentStep, maybePrevStep ) of
        ( Just thisStage, Just prevStep ) ->
            applyActions prevStep.entryActions <|
                applyActions thisStage.exitActions <|
                    { current | tutorialStage = prevStepId }

        ( Just thisStage, Nothing ) ->
            applyActions thisStage.exitActions <|
                { current | tutorialStage = Nothing }

        _ ->
            current


tutorialAutomation : TutorialAction
tutorialAutomation model =
    -- This is the hook where we need to keep track of control positions etc.
    -- I.E. not only on state transitions.
    let
        currentStep =
            findStep model.tutorialStage
    in
    case currentStep of
        Just thisStage ->
            applyActions thisStage.stateActions model

        _ ->
            model


setupTutorialRaid : TutorialAction
setupTutorialRaid model =
    { model | targets = trainingMode }


tutorialStoreBearing : TutorialAction
tutorialStoreBearing model =
    { model
        | storedAzimuth = Just (model.goniometerAzimuth + model.station.lineOfShoot)
        , inputState = BearingRangeInput
    }


tutorialStoreRange1 : TutorialAction
tutorialStoreRange1 model =
    { model
        | storedAzimuthRange = Just (1.6 * model.rangeSlider)
        , inputState = BearingInput
    }


tutorialHeightMode : TutorialAction
tutorialHeightMode model =
    { model
        | goniometerMode = Elevation
        , inputState = HeightInput
    }


chaseTheRaidRange : Bool -> Model -> Model
chaseTheRaidRange active model =
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
                | rangeLeft = active && model.rangeSlider > rangeInMiles + 1
                , rangeRight = active && model.rangeSlider < rangeInMiles - 1
            }
    }


swingThatGoniometer : Bool -> Model -> Model
swingThatGoniometer active model =
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
                | gonioClock = active && model.goniometerAzimuth < targetBearing - degrees 1
                , gonioAnti = active && model.goniometerAzimuth > targetBearing + degrees 1
            }
    }


tutorialSeekElevation : Bool -> Model -> Model
tutorialSeekElevation active model =
    -- Use simulated key presses to mimic the operator tracking the raid.
    -- Unlike bearing, we don't know which way to move, so just go clockwise and
    -- don't try for zero!
    let
        targetElevation =
            Maybe.withDefault 1.5 <|
                List.head <|
                    List.map .alpha model.elevation_A_trace

        currentKeys =
            model.keys
    in
    { model
        | keys =
            { currentKeys
                | gonioClock = active && targetElevation > 0.1
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
