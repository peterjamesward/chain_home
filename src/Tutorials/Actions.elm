module Tutorials.Actions exposing (..)

{- Note, in this file there should be reference to Model but not to Tutorial,Model. -}

import Calculator.Model exposing (InputState(..), setInputState, storeAzimuth, storeAzimuthRange, storeElevation, storeFriendly, storeStrength, storeStrengthPlus)
import Config exposing (..)
import Model exposing (Model)
import Target exposing (findTargetHeight)
import Tutorials.ActionCodes exposing (TutorialActionCode(..), TutorialScenario)
import Tutorials.Model exposing (TutorialActionList)
import Types exposing (GoniometerMode(..), Page(..))


type alias TutorialAction =
    Model -> Model


applyTutorialActions : TutorialActionList -> Model -> Model
applyTutorialActions actions model =
    List.foldl (\a m -> performTheAction a m) model actions


performTheAction : TutorialActionCode -> Model -> Model
performTheAction a model =
    let
        actionFunction =
            case a of
                ActionBearingMode ->
                    actionBearingMode

                ActionInitialiseTutorial ->
                    actionInitialiseTutorial

                ActionCentraliseKnobs ->
                    actionCentraliseKnobs

                ActionChaseTheRaidRange b ->
                    actionChaseTheRaidRange b

                ActionClearCalculator ->
                    actionClearCalculator

                ActionClearTargets ->
                    actionClearTargets

                ActionEndHeightMode ->
                    actionEndHeightMode

                ActionExitTutorial ->
                    actionExitAction

                ActionFindBearingOfNumberedTarget b i ->
                    actionFindBearingOfNumberedTarget b i

                ActionHeightMode ->
                    actionHeightMode

                ActionRecordScenarioDone t ->
                    actionRecordScenarioDone t

                ActionSeekElevation b ->
                    actionSeekElevation b

                ActionSetupRaid ->
                    actionSetupTutorialRaid

                ActionSetupRaidFriendlyOutbound ->
                    actionSetupRaidFriendlyOutbound

                ActionSetupRaid2DifferentBearings ->
                    actionSetupRaid2DifferentBearings

                ActionSetupRaid3to6 ->
                    actionSetupRaid3to6

                ActionSetupRaid2SameBearing ->
                    actionSetupRaid2SameBearing

                ActionShowCalculator ->
                    actionShowCalculator

                ActionShowOperator ->
                    actionShowOperator

                ActionStopGonioAwayFromRaidBearing ->
                    actionStopGonioAwayFromRaidBearing

                ActionStoreBearing ->
                    actionStoreBearing

                ActionStoreElevation ->
                    actionStoreElevation

                ActionStoreFriendly ->
                    actionStoreFriendly

                ActionStorePlus ->
                    actionStorePlus

                ActionStoreRange ->
                    actionStoreRange

                ActionStoreStrength i ->
                    actionStoreStrength i

                ActionSwingGoniometer ->
                    actionSwingGoniometer
    in
    actionFunction model


actionInitialiseTutorial : Model -> Model
actionInitialiseTutorial m =
    { m
        | currPage = OperatorPageInTutorial
        , startTime = m.modelTime
        , webGLtime = 0.0
    }


actionSetupTutorialRaid : Model -> Model
actionSetupTutorialRaid model =
    { model
        | targets = trainingMode model.modelTime
    }


actionCentraliseKnobs : Model -> Model
actionCentraliseKnobs model =
    { model
        | goniometerAzimuth = 0
        , rangeSlider = 50
    }


actionSetupRaid2SameBearing : Model -> Model
actionSetupRaid2SameBearing model =
    { model | targets = trainingMode2 model.modelTime }


actionSetupRaid2DifferentBearings : Model -> Model
actionSetupRaid2DifferentBearings model =
    { model | targets = trainingMode3 model.modelTime }


actionSetupRaid3to6 : Model -> Model
actionSetupRaid3to6 model =
    { model | targets = trainingMode3to6 model.modelTime }


actionSetupRaidFriendlyOutbound : Model -> Model
actionSetupRaidFriendlyOutbound model =
    { model | targets = trainingModeFriendlyOutbound model.modelTime }


actionStopTutorialRaid : Model -> Model
actionStopTutorialRaid model =
    { model | targets = [] }


actionClearCalculator : Model -> Model
actionClearCalculator model =
    { model
        | calculator = Calculator.Model.init
    }


actionRecordScenarioDone : TutorialScenario -> Model -> Model
actionRecordScenarioDone scenario model =
    -- Needs to be Set but defined classes not Comparable.
    -- Minor problem, in the scheme of things.
    { model
        | tutorialsCompleted =
            if List.member scenario model.tutorialsCompleted then
                model.tutorialsCompleted

            else
                scenario :: model.tutorialsCompleted
    }


actionStoreBearing : Model -> Model
actionStoreBearing model =
    { model
        | calculator =
            setInputState BearingRangeInput <|
                storeAzimuth (model.goniometerAzimuth + model.station.lineOfShoot)
                    model.calculator
    }


actionStoreElevation : Model -> Model
actionStoreElevation model =
    -- Use the actual known elevation for target near range setting.
    { model
        | calculator =
            setInputState HeightRangeInput <|
                storeElevation (findTargetHeight model.targets model.rangeSlider) model.calculator
    }


actionStoreStrength : Int -> Model -> Model
actionStoreStrength strength model =
    { model | calculator = storeStrength strength model.calculator }


actionStorePlus : Model -> Model
actionStorePlus model =
    { model | calculator = storeStrengthPlus True model.calculator }


actionStoreFriendly : Model -> Model
actionStoreFriendly model =
    { model | calculator = storeFriendly True model.calculator }


actionStoreRange : Model -> Model
actionStoreRange model =
    { model
        | calculator =
            storeAzimuthRange (1.6 * model.rangeSlider) <|
                setInputState BearingInput model.calculator
    }


actionHeightMode : Model -> Model
actionHeightMode model =
    { model
        | goniometerMode = Elevation
        , calculator = setInputState HeightInput model.calculator
    }


actionEndHeightMode : Model -> Model
actionEndHeightMode model =
    { model
        | goniometerMode = Azimuth
        , calculator = setInputState BearingInput model.calculator
    }


actionBearingMode : Model -> Model
actionBearingMode model =
    { model
        | goniometerMode = Azimuth
        , calculator = setInputState BearingInput model.calculator
    }


actionShowCalculator : Model -> Model
actionShowCalculator model =
    { model | currPage = CalculatorInTutorial }


actionShowOperator : Model -> Model
actionShowOperator model =
    { model | currPage = OperatorPage }


actionExitAction : Model -> Model
actionExitAction model =
    { model
        | tutorialActive = Nothing
        , currPage = InputPage
    }


actionClearTargets : Model -> Model
actionClearTargets model =
    { model | targets = [] }


actionSwingGoniometer model =
    { model | goniometerAzimuth = 1.2 * sin (toFloat model.modelTime / 1000) }


actionStopGonioAwayFromRaidBearing model =
    if model.goniometerAzimuth >= degrees -50 && model.goniometerAzimuth <= degrees 0 then
        actionSwingGoniometer model
        -- keep turning.

    else
        model


actionChaseTheRaidRange active model =
    -- Use simulated key presses to mimic the operator tracking the raid
    let
        rangeInMetres =
            Maybe.withDefault 50000 <|
                List.head <|
                    List.map .rangeInMetres model.targets

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


actionFindBearingOfNumberedTarget active index model =
    -- Use simulated key presses to mimic the operator tracking the raid
    -- Note index is zero based.
    let
        targetBearing =
            Maybe.withDefault 0.0 <|
                List.head <|
                    List.drop index <|
                        List.map .theta model.targets

        currentKeys =
            model.keys

        angleRelativeToLineOfShoot =
            model.goniometerAzimuth + model.station.lineOfShoot
    in
    { model
        | keys =
            { currentKeys
                | gonioClock = active && angleRelativeToLineOfShoot < targetBearing - degrees 1
                , gonioAnti = active && angleRelativeToLineOfShoot > targetBearing + degrees 1
            }
    }


actionSeekElevation active model =
    -- Use simulated key presses to mimic the operator tracking the raid.
    -- Unlike bearing, we don't know which way to move, so just go clockwise and
    -- don't try for zero!
    let
        targetElevation =
            Maybe.withDefault 1.5 <|
                List.head <|
                    List.map .amplitude model.elevation_A_trace

        currentKeys =
            model.keys
    in
    { model
        | keys =
            { currentKeys
                | gonioClock = active && targetElevation > 0.2
            }
    }
