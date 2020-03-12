module TrainingMode exposing (Scenario, advanceScenario, trainingPage, welcomePrompt)

{-
   This aims to be a "wrapper" of sorts for the Operator page,
   allowing us to walk through a basic scenario.
-}

import Constants exposing (blue, white)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Messages exposing (Msg(..))
import OperatorPage exposing (operatorPage)
import Utils exposing (edges)


type Scenario
    = ScenarioWelcome
    | ScenarioSetRange
    | ScenarioFindBearing
    | ScenarioEnterBearing
    | ScenarioBadBearing
    | ScenarioEnterRange
    | ScenarioHeightMode
    | ScenarioFindHeight
    | ScenarioEnterHeight
    | ScenarioBadHeight
    | ScenarioLeaveHeightMode
    | ScenarioRaidStrength
    | ScenarioDone


advanceScenario scenario =
    case scenario of
        Just ScenarioWelcome ->
            Just ScenarioSetRange

        Just x ->
            Just x

        Nothing ->
            Nothing


welcomePrompt =
    ScenarioWelcome


trainingPage model =
    html <|
        layoutWith { options = [ noStaticStyleSheet ] }
            [ inFront (displayPrompt model.trainingScenario)
            ]
        <|
            operatorPage model


displayPrompt prompt =
    case prompt of
        Nothing ->
            none

        Just promptCode ->
            el
                [ centerY
                , centerX
                , Background.color blue
                , Font.color white
                , Font.center
                , padding 10
                , Border.color white
                , Border.rounded 5
                , Border.width 1
                , onClick ScenarioAdvance
                ]
                (promptText promptCode)


promptText : Scenario -> Element Msg
promptText prompt =
    text <|
        case prompt of
            ScenarioWelcome ->
                """Welcome to the Chain Home emulator.
                We shall walk through a typical scenario to help you understand the
                controls and the role of the operator.

                Click on this box to continue.
                """

            ScenarioSetRange ->
                """Look closely at the wiggling line on the display above.
                You should see that there is a distinct dip at about 100 miles,
                and it's coming closer.

                Click on this box to continue.
                """

            _ ->
                "Somebody needs to write my explanation."
