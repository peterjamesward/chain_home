module Main exposing (main)

{-
   This Main module now based on the elm-ui example by Alex Korban
   in his book "Practical Elm"".
-}
-- Project imports

import Browser
import Config exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html


type Page
    = InputPage


type Msg
    = NoOp
    | ChangePlanText String
    | SubmitPlan
    | SetConfigStateMsg Int Bool


type alias Model =
    { currPage : Page
    , currPlanText : String
    , activeConfigurations : List TargetSelector
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = InputPage
      , currPlanText = "hello world"
      , activeConfigurations = targetConfigurations
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetConfigStateMsg index newState ->
            ( { model
                | activeConfigurations =
                    updateConfig model.activeConfigurations index newState
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "VisExp"
    , body =
        [ layout [] <|
            column [ width fill, spacingXY 0 20 ]
                [ navBar
                , inputPage model
                ]
        ]
    }


blue : Color
blue =
    rgb255 52 101 164


lightCharcoal : Color
lightCharcoal =
    rgb255 136 138 133


green : Color
green =
    rgb255 52 164 100


darkGreen : Color
darkGreen =
    rgb255 36 200 33


white : Color
white =
    rgb255 200 200 200


navBar : Element Msg
navBar =
    row
        [ width fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color blue
        ]
        [ el [ alignLeft ] <| text "Chain Home"
        , el [ alignRight ] <| text "Menu"
        ]



-- Show list of configurations with Checkboxes.


setConfig : TargetSelector -> Bool -> Msg
setConfig selector newState =
    SetConfigStateMsg selector.id newState


targetSelector : List TargetSelector -> List (Element Msg)
targetSelector active =
    let
        display : TargetSelector -> Element Msg
        display g =
            Input.checkbox
                [ height (px 40)
                , Border.width 1
                , Border.rounded 5
                , Border.color lightCharcoal
                , padding 3
                ]
                { onChange = setConfig g
                , checked = g.active
                , label =
                    Input.labelRight [] <|
                        text g.description
                , icon = Input.defaultCheckbox
                }
    in
    List.map display active


inputPage : Model -> Element Msg
inputPage model =
    column
        [ width (px 800)
        , spacingXY 0 10
        , centerX
        ]
    <|
        targetSelector model.activeConfigurations
            ++ [ Input.button
                    [ Background.color green
                    , Border.color darkGreen
                    , Border.rounded 3
                    , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
                    , Font.bold
                    , Font.color white
                    , paddingXY 20 6
                    , alignRight
                    , width (px 200)
                    , height (px 40)
                    ]
                    { onPress = Just SubmitPlan
                    , label = el [ centerX ] <| text "Go!"
                    }
               ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
