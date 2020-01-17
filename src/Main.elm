module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import List
import Tuple exposing (..)
import String
import Browser
import Html exposing (..)
import Task
import Time

-- This is dummy line for me to practise with floats and trig.
myLineData t= List.map (\x -> ( x, truncate <| ((*) 100) <| sin <| toFloat <| x * t ))
  (List.range 1 800)

stringifyPoint (x, y) = (String.fromInt x )++ 
    "," ++ 
    (String.fromInt y) ++ " "

polyLineFromCoords coords = List.foldr (++) "" 
  (List.map stringifyPoint coords)


-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , lineData : List (Int, Int)
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { zone = Time.utc 
    , time = Time.millisToPosix 0
    , lineData = myLineData 0
    }
  , Task.perform AdjustTimeZone Time.here
  )

-- UPDATE
type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime
                , lineData = myLineData (Time.toMillis model.zone model.time)}
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone 
                , lineData = myLineData (Time.toSecond model.zone model.time)}
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 40 Tick

-- VIEW

view : Model -> Svg Msg
view m = svg
    [ viewBox "0 0 800 400"
    , width "800"
    , height "400"
    ]
    [ rect
        [ x "0"
        , y "0"
        , width "800"
        , height "400"
        , fill "black"
        , stroke "black"
        , strokeWidth "2"
        ]
        []
    , polyline
        [ points (polyLineFromCoords m.lineData)
        , fill "none"
        , stroke "lightgreen"
        , strokeWidth "4"
        ]
        []
    ]
