module ElevationCurves exposing (aElevationAdjustedEchoes, bElevationAdjustedEchoes, estimateEchoInElevationMode)


import Types exposing (Echo)
type alias ElevationCurve =
    Float -> Float



{-
   When we are in elevation mode, we need high and low receiver inputs to derive elevation.
   But we going to cheat here, since height finding is iffy at best. We know the
   elevation of each target, and we can hard-code the height response curves for A and B
   systems (Supervisor's Handbook chap 12). We then know the goniometer setting that
   will make a target "D/F out" -- we only need a suitable function to make it appear
   that we are actually using a goniometer.
-}


aSystemElevationCurveBasic : ElevationCurve
aSystemElevationCurveBasic x =
    -- For a given target elevation, returns the goniometer setting that would cause
    -- that target to be "D/F out" in height mode when using the A system.
    -- Empirically and strictly within these bounds: 1 - cos (x^2.5 / 60 - 0.5) for x = 0 to 10
    -- NOte that return values are 0 to 2, where:
    -- 0.0 = line of shoot + 20 degrees
    -- 1.0 = line of shoot + 65 degrees
    -- 2.0 = line of shoot + 150 degrees
    if (x >= 0) && (x <= 10) then
        1 - cos (x ^ 2.5 / 60 - 0.5)

    else
        0.0


bSystemElevationCurveBasic : ElevationCurve
bSystemElevationCurveBasic x =
    -- For a given target elevation, returns the goniometer setting that would cause
    -- that target to be "D/F out" in height mode when using the B system.
    -- Empirically and strictly within these bounds: 1 - cos (x^2.5 / 400 + 1) for x = 0 to 20
    if (x >= 0) && (x <= 20) then
        1 - cos (x ^ 2.5 / 400 + 1)

    else
        0.0


goniometerPositionForAlpha : ElevationCurve -> Float -> Float
goniometerPositionForAlpha curve elevation =
    let
        curveValue =
            curve elevation / 2.0

        gonioOffset =
            (curveValue - 0.0) * degrees 150 + (2.0 - curveValue) * degrees 20
    in
    gonioOffset


aElevationAdjustedEchoes : Float -> List Echo -> List Echo
aElevationAdjustedEchoes goniometerSetting echoes =
    -- The amplitude of the return signal will be the sine of the angle
    -- difference between the goniometer setting and the setting obtained
    -- by reading the known elevation off the height curve.
    List.map
        (estimateEchoInElevationMode aSystemElevationCurveBasic goniometerSetting)
        echoes


estimateEchoInElevationMode : ElevationCurve -> Float -> Echo -> Echo
estimateEchoInElevationMode curve gonio echo =
    { echo
        | amplitude =
            abs <|
                4.0
                    * sin
                        (gonio
                            - goniometerPositionForAlpha curve echo.alpha
                        )
    }


bElevationAdjustedEchoes : Float -> List Echo -> List Echo
bElevationAdjustedEchoes goniometerSetting echoes =
    -- The amplitude of the return signal will be the sine of the angle
    -- difference between the goniometer setting and the setting obtained
    -- by reading the known elevation off the height curve.
    List.map
        (estimateEchoInElevationMode bSystemElevationCurveBasic goniometerSetting)
        echoes
