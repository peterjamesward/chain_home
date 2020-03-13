module TrainingMode exposing (advanceScenario, trainingPage, welcomePrompt)

{-
   This aims to be a "wrapper" of sorts for the Operator page,
   allowing us to walk through a basic scenario.
-}

import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Messages exposing (Msg(..))
import Model exposing (Model)
import OperatorPage exposing (operatorPage)
import Types exposing (..)
import Utils exposing (choose, edges)


type alias ScenarioDetails =
    { scenario : Scenario
    , showWhenCondition : Model -> Bool
    , nextPrompt : Scenario
    , prevPrompt : Scenario
    , verticalOffset : Int
    , horizontalOffset : Int
    , locusX : Int
    , locusY : Int
    , locusWidth : Int
    , locusHeight : Int
    }


scenarioDefaults =
    { scenario = ScenarioWelcome
    , showWhenCondition = \_ -> True
    , nextPrompt = ScenarioWelcome
    , prevPrompt = ScenarioWelcome
    , verticalOffset = 0
    , horizontalOffset = 0
    , locusX = 0
    , locusY = 0
    , locusHeight = 0
    , locusWidth = 0
    }


scenarioDetails : List ScenarioDetails
scenarioDetails =
    [ { scenarioDefaults | scenario = ScenarioWelcome, nextPrompt = ScenarioDescribeCRT }
    , { scenarioDefaults
        | scenario = ScenarioDescribeCRT
        , verticalOffset = -50
        , horizontalOffset = -50
        , locusX = 260
        , locusY = 80
        , locusHeight = 360
        , locusWidth = 660
      }
    ]


lookupScenarioDetails : Scenario -> ScenarioDetails
lookupScenarioDetails sc =
    let
        details =
            List.filterMap (\s -> choose (s.scenario == sc) (Just s) Nothing) scenarioDetails
    in
    case details of
        d :: _ ->
            d

        _ ->
            scenarioDefaults


advanceScenario : Maybe Scenario -> Maybe Scenario
advanceScenario current =
    case current of
        Just sc ->
            Just (lookupScenarioDetails sc).nextPrompt

        Nothing ->
            Nothing


welcomePrompt =
    ScenarioWelcome


trainingPage model =
    html <|
        layoutWith { options = [ noStaticStyleSheet ] }
            []
        <|
            el
                [ inFront (maybeDisplayOverlay model) ]
                (operatorPage model)


maybeDisplayOverlay : Model -> Element Msg
maybeDisplayOverlay model =
    case model.trainingScenario of
        Nothing ->
            none

        Just sc ->
            displayOverlay (lookupScenarioDetails sc) model


displayOverlay : ScenarioDetails -> Model -> Element Msg
displayOverlay details model =
    column
        [ centerY
        , centerX
        , width (px 500)
        , Background.color blue
        , Font.color white
        , Font.center
        , padding 10
        , Border.color white
        , Border.rounded 5
        , Border.width 1
        , alpha 0.8
        ]
        [ paragraph
            [ spacing 5
            ]
            [ promptText details.scenario ]
        , row [ width fill ]
            [ el [ alignLeft ] <| text "<<"
            , el [ alignRight, onClick ScenarioAdvance ] <| text ">>"
            ]
        ]


promptText : Scenario -> Element Msg
promptText prompt =
    text <|
        case prompt of
            ScenarioWelcome ->
                """Click ">>" to learn about the Chain Home receiver and the operator's work.
                """

            ScenarioDescribeCRT ->
                """The main feature is the Cathode Ray Tube (CRT) that displays signals
                returned from radio wave pulses sent out from the transmitter.
                """

            _ ->
                "Somebody needs to write my explanation."
