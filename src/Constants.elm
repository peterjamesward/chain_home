module Constants exposing (..)

lightSpeed = 300000000.0
frequency  = 20000000.0
wavelength = lightSpeed / frequency
pulseDuration = 40.0  -- microseconds 
pulseWidth = lightSpeed * pulseDuration / 10000000.0  -- TODO: scale seems wrong here.

-- Some facts about our display
beamSweepIncreasingAmplitude = 0.001
beamSweepDecreasingAmplitude = -0.0001 
scaleWidthKilometers = 150
viewWidth = 1000
viewHeight = 400
strengthToHeightFactor = 400.0
rangeToXFactor = viewWidth / (scaleWidthKilometers * 1000.0)
rangeDeltaPerMicrosecond = scaleWidthKilometers * 2
