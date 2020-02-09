module Main exposing (main)

{-
   This Main module now based on the elm-ui example by Alex Korban
   in his book "Practical Elm"".
-}

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
import Html as H exposing (..)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode as D exposing (..)
import Nixie exposing (nixieDisplay)
import Receiver exposing (goniometerMix)
import Skyline exposing (EdgeSegment, deriveSkyline)
import Station exposing (..)
import Target exposing (..)
import Time


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
    | GonioGrab ( Float, Float )
    | GonioMove ( Float, Float )
    | GonioRelease ( Float, Float )
    | AdjustRangeValue Float


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
    , rangeSlider : Float
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
      , rangeSlider = 50.0
      }
    , Cmd.none
    )


type alias Keys =
    { gonioClock : Bool -- A
    , gonioAnti : Bool -- Q
    , rangeLeft : Bool -- left arrow
    , rangeRight : Bool -- right arrow
    }



-- Keep track of any significant keys' state, such as for adjusting goniometer or range slider.


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


slideRangeSlider : Float -> Keys -> Float
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
        ]


type alias LineData =
    List ( Float, Float )



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
                | gonioDrag = Just ( model.goniometer, offset )
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
                        | goniometer = newAngle
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

        AdjustRangeValue newRange ->
            ( { model
                | rangeSlider = newRange
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


clickableGonioImage theta =
    div
        [ Mouse.onDown (\event -> GonioGrab event.offsetPos)
        , Mouse.onMove (\event -> GonioMove event.offsetPos)
        , Mouse.onUp (\event -> GonioRelease event.offsetPos)
        , Touch.onStart (GonioGrab << touchCoordinates)
        , Touch.onMove (GonioMove << touchCoordinates)
        , Touch.onEnd (GonioRelease << touchCoordinates)
        ]
        [ drawGoniometer theta ]


rangeSlider model =
    E.el [ E.centerX ] <|
        Input.slider
            [ E.height (E.px 30)
            , E.width (E.px 950)
            , E.centerX

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
            , max = 100
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


operatorPage : Model -> Element Msg
operatorPage model =
    row
        [ E.spacing 10
        , E.padding 10
        , E.width E.fill
        , E.centerX
        ]
        [ E.html <| clickableGonioImage <| model.goniometer + model.station.lineOfShoot
        , column
            [ E.width E.shrink
            ]
            [ rangeSlider model
            , E.html (crt model)
            , row
                [ E.height (E.px 100)
                , E.centerX
                , E.spacing 50
                , Font.color white
                , Font.size 14
                , Font.family
                    [ Font.typeface "monospace"
                    , Font.sansSerif
                    ]
                ]
                [ column [ E.centerX ]
                    [ nixieDisplay 3 (truncate model.rangeSlider)
                    , el [ E.centerX ] (E.text "RANGE")
                    ]
                , column [ E.centerX ]
                    [ nixieDisplay 3
                        (modBy 360 <|
                            truncate
                                ((model.goniometer + model.station.lineOfShoot)
                                    * 180
                                    / pi
                                )
                        )
                    , el [ E.centerX ] (E.text "BEARING")
                    ]
                ]
            ]
        ]


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
