module Calculator.Model exposing (..)


type
    InputState
    -- Required sequence of operator actions
    = BearingInput
    | BearingRangeInput
    | HeightInput
    | HeightRangeInput


type alias Model =
    { inputState : InputState
    , storedAzimuth : Maybe Float
    , storedElevation : Maybe Float
    , storedAzimuthRange : Maybe Float
    , storedElevationRange : Maybe Float
    , storedStrength : Maybe Int
    , storedFriendly : Maybe Bool
    , storedStrengthPlus : Maybe Bool
    , explainMode : Bool
    }


init =
    { inputState = BearingInput
    , storedAzimuth = Nothing
    , storedElevation = Nothing
    , storedAzimuthRange = Nothing
    , storedElevationRange = Nothing
    , storedStrength = Nothing
    , storedFriendly = Nothing
    , storedStrengthPlus = Nothing
    , explainMode = False
    }



-- Oh, we need some "putters" in old OO style.


setInputState : InputState -> Model -> Model
setInputState newState model =
    { model | inputState = newState }


storeAzimuth : Float -> Model -> Model
storeAzimuth newAzimuth model =
    { model | storedAzimuth = Just newAzimuth }


storeElevation : Maybe Float -> Model -> Model
storeElevation newElevation model =
    { model | storedElevation = newElevation }


storeAzimuthRange : Float -> Model -> Model
storeAzimuthRange newAzimuthRange model =
    { model | storedAzimuthRange = Just newAzimuthRange }


storeElevationRange : Float -> Model -> Model
storeElevationRange newElevationRange model =
    { model | storedElevationRange = Just newElevationRange }


storeStrength : Int -> Model -> Model
storeStrength newStrength model =
    { model | storedStrength = Just newStrength }


storeStrengthPlus : Bool -> Model -> Model
storeStrengthPlus newStrengthPlus model =
    { model | storedStrengthPlus = Just newStrengthPlus }


storeFriendly : Bool -> Model -> Model
storeFriendly newFriendly model =
    { model | storedFriendly = Just newFriendly }

getStoredStrength : Model -> Maybe Int
getStoredStrength model =
    model.storedStrength

toggleExplainMode : Model -> Model
toggleExplainMode model =
    { model | explainMode = not model.explainMode }