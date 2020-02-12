module Main exposing (main)

{-
   This Main module now based on the elm-ui example by Alex Korban
   in his book "Practical Elm"".
-}
-- TODO: Add another knob to move the range slider.

import BeamSmoother exposing (beamPath, scalePathToDisplay)
import Browser
import Browser.Events exposing (..)
import CRT exposing (crt)
import Config exposing (..)
import Constants exposing (..)
import Echo exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Goniometer exposing (drawGoniometer, goniometerTurnAngle)
import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D exposing (..)
import LobeFunctions exposing (..)
import Messages exposing (..)
import Nixie exposing (nixieDisplay)
import PushButtons exposing (toggleSwitch)
import Range exposing (drawRangeKnob)
import Receiver exposing (goniometerMix)
import Skyline exposing (EdgeSegment, deriveSkyline)
import Station exposing (..)
import Target exposing (..)
import Time
import Types exposing (..)


type Page
    = InputPage
    | OperatorPage


type alias Model =
    { currPage : Page
    , zone : Time.Zone
    , startTime : Int
    , time : Int -- milliseconds from t-zero
    , lineData : List Point
    , station : Station
    , targets : List Target
    , movedTargets : List Target
    , polarTargets : List PolarTarget
    , echoes : List Echo
    , skyline : List Line
    , goniometerBearing : Angle
    , gonioOutput : List Echo
    , keys : Keys
    , gonioDrag : Maybe ( Angle, Point ) -- angle and mouse position when mouse down
    , activeConfigurations : List TargetSelector
    , rangeSlider : Range
    , outputDevice : Device
    , rangeDrag : Maybe ( Angle, Point )
    , rangeKnobAngle : Angle
    , goniometerMode : GoniometerMode
    , goniometerElevation : Angle
    , transmitAntenna : Antenna
    , transmitAB : Bool
    , reflector : Bool
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
      , goniometerBearing = degrees 10 -- relative to Line Of Shoot.
      , gonioOutput = []
      , keys = noKeys
      , gonioDrag = Nothing
      , activeConfigurations = targetConfigurations
      , rangeSlider = 50.0
      , outputDevice = { class = Desktop, orientation = Landscape }
      , rangeDrag = Nothing
      , rangeKnobAngle = 0.0
      , goniometerMode = Bearing
      , goniometerElevation = degrees 0
      , transmitAntenna = transmitAReflector
      , transmitAB = True
      , reflector = False
      }
    , Cmd.none
    )



-- Keep track of any significant keys' state, such as for adjusting goniometer or range slider.


type alias Keys =
    { gonioClock : Bool -- A
    , gonioAnti : Bool -- Q
    , rangeLeft : Bool -- left arrow
    , rangeRight : Bool -- right arrow
    }


noKeys : Keys
noKeys =
    Keys False False False False


updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
    case key of
        "q" ->
            { keys | gonioAnti = isDown }

        "a" ->
            { keys | gonioClock = isDown }

        "," ->
            { keys | rangeLeft = isDown }

        "." ->
            { keys | rangeRight = isDown }

        _ ->
            keys



-- Mouse tracking permits non-integer movements.


swingGoniometer : Angle -> Keys -> Angle
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


slideRangeSlider : Range -> Keys -> Range
slideRangeSlider range keys =
    case ( keys.rangeLeft, keys.rangeRight ) of
        ( True, True ) ->
            range

        ( True, False ) ->
            Basics.max 0.0 (range - 0.5)

        ( False, True ) ->
            Basics.min 100.0 (range + 0.5)

        _ ->
            range



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 100 Tick
        , onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        , onResize (\w h -> DeviceResize w h)
        ]



-- UPDATE


deriveModelAtTime : Model -> Int -> Model
deriveModelAtTime model t =
    let
        targetsNow =
            List.map (targetAtTime t model.startTime) model.targets

        convertedTargets =
            List.map (mapToPolar bawdsey) targetsNow

        echoSignals =
            deriveEchoes convertedTargets model.transmitAntenna t

        gonioOut =
            goniometerMix model.goniometerBearing echoSignals

        newSkyline =
            deriveSkyline (scaleWidthKilometers * 1000) gonioOut
    in
    { model
        | startTime =
            --TODO: Get rid of this somehow.
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
        , goniometerBearing = swingGoniometer model.goniometerBearing model.keys
        , rangeSlider = slideRangeSlider model.rangeSlider model.keys
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

        GonioGrab offset ->
            ( { model
                | gonioDrag = Just ( model.goniometerBearing, offset )
              }
            , Cmd.none
            )

        GonioMove offset ->
            case model.gonioDrag of
                Nothing ->
                    ( model, Cmd.none )

                Just ( startAngle, startXY ) ->
                    let
                        newAngle =
                            goniometerTurnAngle startAngle startXY offset
                    in
                    ( { model
                        | goniometerBearing = newAngle
                        , gonioDrag = Just ( newAngle, offset )
                      }
                    , Cmd.none
                    )

        GonioRelease offset ->
            ( { model
                | gonioDrag = Nothing
              }
            , Cmd.none
            )

        RangeGrab offset ->
            ( { model
                | rangeDrag = Just ( model.rangeKnobAngle, offset )
              }
            , Cmd.none
            )

        RangeMove offset ->
            case model.rangeDrag of
                Nothing ->
                    ( model, Cmd.none )

                Just ( startAngle, startXY ) ->
                    let
                        newAngle =
                            goniometerTurnAngle startAngle startXY offset

                        angleChange =
                            (newAngle - startAngle) |> sin |> asin |> (*) (60 / pi)

                        newRange =
                            max 0 <| min 100 <| (model.rangeSlider + angleChange)
                    in
                    ( if angleChange > 0 then
                        -- Turn clockwise, increase range
                        { model
                            | rangeSlider = newRange
                            , rangeDrag =
                                if newRange <= 100 then
                                    Just ( newAngle, offset )

                                else
                                    Nothing
                            , rangeKnobAngle = newAngle
                        }

                      else if angleChange < 0 then
                        -- Turn anticlockwise, decrease range
                        { model
                            | rangeSlider = newRange
                            , rangeDrag =
                                if newRange >= 0 then
                                    Just ( newAngle, offset )

                                else
                                    Nothing
                            , rangeKnobAngle = newAngle
                        }

                      else
                        model
                    , Cmd.none
                    )

        RangeRelease offset ->
            ( { model
                | rangeDrag = Nothing
              }
            , Cmd.none
            )

        AdjustRangeValue newRange ->
            ( { model
                | rangeSlider = newRange
              }
            , Cmd.none
            )

        DeviceResize w h ->
            ( { model | outputDevice = classifyDevice { width = w, height = h } }
            , Cmd.none
            )

        SelectTransmitAntenna val ->
            ( { model
                | transmitAB = val
                , transmitAntenna = selectTransmitAntenna val model.reflector
              }
            , Cmd.none
            )

        EnableReflector val ->
            ( { model
                | reflector = val
                , transmitAntenna = selectTransmitAntenna model.transmitAB val
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


selectTransmitAntenna ab reflect =
    case ( ab, reflect ) of
        ( True, True ) ->
            transmitAReflector

        ( False, True ) ->
            transmitBReflector

        ( True, False ) ->
            transmitANoReflect

        ( False, False ) ->
            transmitBNoReflect



-- VIEW


clickableRangeKnob =
    div
        [ Pointer.onDown (\event -> RangeGrab event.pointer.offsetPos)
        , Pointer.onMove (\event -> RangeMove event.pointer.offsetPos)
        , Pointer.onUp (\event -> RangeRelease event.pointer.offsetPos)
        , style "touch-action" "none"
        ]


clickableGonioImage theta =
    div
        [ Pointer.onDown (\event -> GonioGrab event.pointer.offsetPos)
        , Pointer.onMove (\event -> GonioMove event.pointer.offsetPos)
        , Pointer.onUp (\event -> GonioRelease event.pointer.offsetPos)
        , style "touch-action" "none"
        ]
        [ drawGoniometer theta ]


rangeSlider model =
    Input.slider
        [ E.height (E.px 30)
        , E.width E.fill
        , E.centerX
        , pointer

        -- Here is where we're creating/styling the "track"
        , E.behindContent
            (E.el
                [ E.width E.fill
                , E.height (E.px 2)
                , E.centerY
                , E.centerX
                , Background.color midGray
                , Border.rounded 2
                ]
                E.none
            )
        ]
        { onChange = AdjustRangeValue
        , label =
            Input.labelHidden "Range (miles)"
        , min = 0
        , max = 100.0
        , step = Nothing
        , value = model.rangeSlider
        , thumb =
            Input.thumb
                [ E.width (E.px 16)
                , E.height (E.px 50)
                , Border.rounded 8
                , Border.width 1
                , Border.color <| E.rgb 0 0 0
                , Background.color <| E.rgb255 180 20 20
                ]
        }


rangeDisplay rangeValue =
    column [ E.centerX ]
        [ nixieDisplay 3 (truncate rangeValue)
        , el [ E.centerX ] (E.text "RANGE")
        ]


showMouseCoordinates model =
    let
        ( _, ( x, y ) ) =
            Maybe.withDefault ( 0, ( 0, 0 ) ) model.rangeDrag
    in
    column [ E.centerX ]
        [ el [ E.centerX ] (E.text "X")
        , nixieDisplay 4 (truncate x)
        , el [ E.centerX ] (E.text "Y")
        , nixieDisplay 4 (truncate y)
        ]


bearingDisplay bearing =
    column [ E.centerX ]
        [ nixieDisplay 3
            (modBy 360 <|
                truncate
                    (bearing
                        * 180
                        / pi
                    )
            )
        , el [ E.centerX ] (E.text "BEARING")
        ]


commonStyles =
    [ E.spacing 20
    , E.padding 30
    , E.width E.fill
    , E.centerX
    , Font.color paletteSand
    , Font.size 14
    , Font.family
        [ Font.typeface "monospace"
        , Font.sansSerif
        ]
    ]


modeToggles model =
    row
        [ E.width <| fillPortion <| 3
        , E.spacing 20
        ]
        [ toggleSwitch "MODE" "BEARING" "ELEVATION" (model.goniometerMode == Bearing) SelectGoniometerMode
        , toggleSwitch "RECEIVER" "HIGH" "LOW" False SelectReceiveAntenna
        , toggleSwitch "TRANSMITTER" "A" "B" model.transmitAB SelectTransmitAntenna
        , toggleSwitch "REFLECTOR" "ON" "OFF" model.reflector EnableReflector
        ]


operatorPageLandscape : Model -> Element Msg
operatorPageLandscape model =
    column
        commonStyles
        [ row
            [ E.width E.fill
            ]
            [ E.el
                [ E.width <| fillPortion 1 ]
                none
            , column
                [ E.width <| E.fillPortion 6 ]
                [ rangeSlider model
                , E.html (crt model)
                ]
            , E.el
                [ E.width <| fillPortion 1 ]
                none
            ]
        , row
            [ E.centerX
            , E.spacing 50
            ]
            [ E.el
                [ E.width <| minimum 200 <| fillPortion 3
                , pointer
                ]
              <|
                E.html <|
                    clickableGonioImage <|
                        model.goniometerBearing
                            + model.station.lineOfShoot

            --, showMouseCoordinates model
            , modeToggles model
            , column [ E.width <| fillPortion 1 ]
                [ rangeDisplay model.rangeSlider
                , bearingDisplay (model.goniometerBearing + model.station.lineOfShoot)
                ]
            , column
                [ E.width <| minimum 100 <| fillPortion 2
                , pointer
                , centerX
                ]
                [ E.el
                    [ E.width <| maximum 200 <| fill
                    , pointer
                    ]
                  <|
                    E.html <|
                        clickableRangeKnob <|
                            [ drawRangeKnob model.rangeKnobAngle ]
                , E.el [ centerX ] (E.text "RANGE")
                ]
            ]
        ]


operatorPagePortrait : Model -> Element Msg
operatorPagePortrait model =
    column
        commonStyles
        [ rangeSlider model
        , E.html (crt model)
        , row [ E.width E.fill ]
            [ E.el [ pointer, E.width <| fillPortion 3 ] <|
                E.html <|
                    clickableGonioImage <|
                        model.goniometerBearing
                            + model.station.lineOfShoot
            , E.el [ pointer, E.width <| fillPortion 2 ] <|
                E.html <|
                    clickableRangeKnob <|
                        [ drawRangeKnob model.rangeKnobAngle ]
            ]
        , row [ E.width E.fill, spacing 10, padding 5 ]
            [ modeToggles model
            , column [ E.width <| fillPortion 1 ]
                [ rangeDisplay model.rangeSlider
                , bearingDisplay (model.goniometerBearing + model.station.lineOfShoot)
                ]
            ]
        ]


operatorPage : Model -> Element Msg
operatorPage model =
    case model.outputDevice.orientation of
        Landscape ->
            operatorPageLandscape model

        Portrait ->
            operatorPagePortrait model


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
    { title = "Chain Home emulation"
    , body =
        [ layout
            [ Background.color flatMidnightBlue
            ]
          <|
            column [ E.width E.fill, spacingXY 0 20 ]
                [ navBar
                , content
                ]
        ]
    }


navBar : Element Msg
navBar =
    row
        [ E.width E.fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 5, top = 0, left = 0, right = 0 }
        , Border.color midGray
        , Font.color white
        ]
        [ el
            [ alignLeft
            ]
          <|
            E.text "Chain Home"
        , el
            [ alignRight
            ]
          <|
            E.text "Menu"
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
        [ E.width (px 500)
        , spacingXY 0 10
        , centerX
        , Font.color lightCharcoal
        ]
    <|
        targetSelector model.activeConfigurations
            ++ [ Input.button
                    [ Background.color paletteDarkGreen
                    , Border.color paletteLightGreen
                    , Border.rounded 3
                    , Border.widthEach { bottom = 3, top = 2, right = 3, left = 2 }
                    , Font.bold
                    , Font.color paletteLightGreen
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
