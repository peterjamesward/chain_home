module Main exposing (main)

{-
   This Main module now based on the elm-ui example by Alex Korban
   in his book "Practical Elm"".
-}

import BeamSmoother exposing (beamPath)
import Browser
import Browser.Events exposing (..)
import Config exposing (..)
import Constants exposing (..)
import Echo exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Goniometer exposing (clickableGonioImage, showGonio)
import Json.Decode as D exposing (..)
import Receiver exposing (goniometerMix)
import Skyline exposing (EdgeSegment, deriveSkyline, viewEdge, viewLineSegment)
import Station exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Target exposing (..)
import Time
import Utils exposing (..)


type Page
    = InputPage
    | OperatorPage


type Msg
    = NoOp
    | SetConfigStateMsg Int Bool
    | DisplayReceiver
    | DisplayConfiguration
    | Tick Time.Posix
    | KeyChanged Bool String


type alias Model =
    { currPage : Page
    , zone : Time.Zone
    , startTime : Int -- millseconds from t-zero
    , time : Int
    , lineData : List ( Float, Float )
    , station : Station
    , targets : List Target
    , movedTargets : List Target
    , polarTargets : List PolarTarget
    , echoes : List Echo
    , skyline : List ( ( Float, Float ), ( Float, Float ) )
    , goniometer : Float
    , gonioOutput : List Echo
    , keys : Keys
    , gonioDrag : Maybe ( Float, ( Float, Float ) ) -- angle and mouse position when mouse down
    , activeConfigurations : List TargetSelector
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = InputPage
      , zone = Time.utc
      , startTime = 0
      , time = 0
      , lineData = beamPath []
      , station = bawdsey
      , targets = getAllTargets targetConfigurations
      , movedTargets = []
      , polarTargets = []
      , echoes = []
      , skyline = []
      , goniometer = degrees 10 -- relative to Line Of Shoot.
      , gonioOutput = []
      , keys = noKeys
      , gonioDrag = Nothing
      , activeConfigurations = targetConfigurations
      }
    , Cmd.none
    )


type alias Keys =
    { gonioClock : Bool -- A
    , gonioAnti : Bool -- Q
    }


noKeys : Keys
noKeys =
    Keys False False


updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
    case key of
        "q" ->
            { keys | gonioAnti = isDown }

        "a" ->
            { keys | gonioClock = isDown }

        _ ->
            keys



-- When we have mouse tracking we can have non-integer movements.


swingGoniometer : Float -> Keys -> Float
swingGoniometer angle keys =
    case ( keys.gonioClock, keys.gonioAnti ) of
        ( True, True ) ->
            angle

        ( True, False ) ->
            angle + degrees 1.0

        ( False, True ) ->
            angle - degrees 1.0

        _ ->
            angle



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 50 Tick
        , onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        ]


type alias LineData =
    List ( Float, Float )


rangeScale =
    List.map
        (\i ->
            S.text_
                [ x (String.fromInt (i * 50))
                , y "-10"
                , SA.fill "lightgreen"
                , textAnchor "right"
                , fontFamily "monospace"
                ]
                [ S.text (String.fromInt (i * 5)) ]
        )
        (List.range 0 19)


scalePathToDisplay : LineData -> LineData
scalePathToDisplay unscaled =
    let
        scalePoint ( x, y ) =
            ( viewWidth * x / scaleWidthKilometers / 1000, y * strengthToHeightFactor )

        -- TODO: constants!
    in
    List.map scalePoint unscaled



-- UPDATE


deriveModelAtTime : Model -> Int -> Model
deriveModelAtTime model t =
    let
        targetsNow =
            List.map (targetAtTime t model.startTime) model.targets

        convertedTargets =
            List.map (mapToPolar bawdsey) targetsNow

        echoSignals =
            deriveEchoes convertedTargets t

        gonioOut =
            goniometerMix model.goniometer echoSignals

        newSkyline =
            deriveSkyline (scaleWidthKilometers * 1000) gonioOut
    in
    { model
        | startTime =
            if model.startTime == 0 then
                t

            else
                model.startTime
        , time = t
        , targets = getAllTargets model.activeConfigurations
        , movedTargets = targetsNow
        , polarTargets = convertedTargets
        , echoes = echoSignals
        , skyline = newSkyline
        , gonioOutput = gonioOut
        , lineData = scalePathToDisplay <| beamPath newSkyline
        , goniometer = swingGoniometer model.goniometer model.keys
    }


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

        DisplayReceiver ->
            ( { model | currPage = OperatorPage }
            , Cmd.none
            )

        DisplayConfiguration ->
            ( { model | currPage = InputPage }
            , Cmd.none
            )

        Tick newTime ->
            ( deriveModelAtTime model (Time.posixToMillis newTime)
            , Cmd.none
            )

        KeyChanged isDown key ->
            ( { model | keys = updateKeys isDown key model.keys }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


operatorPage : Model -> Element Msg
operatorPage model =
    row
        [ E.spacing 10
        , E.padding 10
        , E.centerX
        ]
    <|
        [ E.html <| clickableGonioImage <| model.goniometer + model.station.lineOfShoot
        , E.html (crt model)
        ]


crt m =
    let
        svgPointList =
            polyLineFromCoords m.lineData
    in
    svg
        [ viewBox "-10 -40 1020 450"
        , SA.width "1020"
        , SA.height "420"
        ]
    <|
        [ rect
            [ x "-10"
            , y "-40"
            , SA.width "1020"
            , SA.height "450"
            , SA.fill "black"
            , stroke "black"
            , strokeWidth "3"
            , strokeLinejoin "round"
            ]
            []
        , polyline
            [ points svgPointList
            , SA.fill "none"
            , stroke "forestgreen"
            , opacity "60%"
            , strokeWidth "2.5"
            ]
            []
        , polyline
            [ points svgPointList
            , SA.fill "none"
            , stroke "springgreen"
            , strokeWidth "0.8"
            ]
            []
        ]
            ++ rangeScale


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                OperatorPage ->
                    operatorPage model

                InputPage ->
                    inputPage model
    in
    { title = "VisExp"
    , body =
        [ layout [] <|
            column [ E.width E.fill, spacingXY 0 20 ]
                [ navBar
                , content
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
        [ E.width E.fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color blue
        ]
        [ el [ alignLeft ] <| E.text "Chain Home"
        , el [ alignRight ] <| E.text "Menu"
        ]



-- Show list of configurations with Checkboxes.
-- This will now be on its own page with elm-ui.


setConfig : TargetSelector -> Bool -> Msg
setConfig selector newState =
    SetConfigStateMsg selector.id newState


targetSelector : List TargetSelector -> List (Element Msg)
targetSelector active =
    let
        display : TargetSelector -> Element Msg
        display g =
            Input.checkbox
                [ E.height (px 40)
                , Border.width 1
                , Border.rounded 5
                , Border.color lightCharcoal
                , padding 10
                ]
                { onChange = setConfig g
                , checked = g.active
                , label =
                    Input.labelRight [] <|
                        E.text g.description
                , icon = Input.defaultCheckbox
                }
    in
    List.map display active


inputPage : Model -> Element Msg
inputPage model =
    column
        [ E.width (px 800)
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
                    , E.width (px 200)
                    , E.height (px 40)
                    ]
                    { onPress = Just DisplayReceiver
                    , label = el [ centerX ] <| E.text "Go!"
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
