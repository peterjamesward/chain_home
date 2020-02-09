module BeamSmoother exposing (beamPath, scalePathToDisplay)

import Constants exposing (..)
import Skyline exposing (EdgeSegment)



-- Real CH CRTs would not draw vertical lines - it takes time to build up the voltages
-- on the deflection plated. We shall simulate that here.
-- We shall not bother to limit acceleration, unless we have to.


dummyTrailingEdge =
    ( ( 1000.0, 0.0 ), ( 1000.0, 0.0 ) )



-- Latest attempt at a simple beam movement smoother, to simulate limitation on beam vertical slope.
-- Should be MUCH easier now the skyline is represented by horizontal roof segments
-- instead of vertical wall segments!
-- Note that the roof segments are horizontal but not vertically contiguous- the height transitions are abrupt/
-- Note the Float here is where the beam ended, not necessarily on the last edge, if that
-- edge was too short.


beamSmoothingFunction :
    EdgeSegment
    -> ( List ( Float, Float ), Float )
    -> ( List ( Float, Float ), Float )
beamSmoothingFunction newRoof ( lines, beamY ) =
    let
        ( ( roofLeft, roofHeight ), ( roofRight, _ ) ) =
            newRoof

        beamVelocity =
            if roofHeight > beamY then
                beamSweepIncreasingAmplitude

            else
                beamSweepDecreasingAmplitude

        beamSweepTime =
            abs <| (roofHeight - beamY) / beamVelocity

        beamYForShortRoof =
            beamY + (roofRight - roofLeft) * beamVelocity
    in
    if beamSweepTime <= roofRight - roofLeft then
        -- We have time to get the beam on the roof.
        ( ( roofRight, roofHeight )
            :: ( roofLeft + beamSweepTime, roofHeight )
            :: ( roofLeft, beamY )
            :: lines
        , roofHeight
        )

    else
        -- Won't reach the roof but see how far we go.
        ( ( roofRight, beamYForShortRoof )
            :: ( roofLeft, beamY )
            :: lines
        , beamYForShortRoof
        )



-- We are now taking a list of horizontal segments and we have to join them.


beamPath : List EdgeSegment -> List ( Float, Float )
beamPath edges =
    let
        ( lines, _ ) =
            List.foldr beamSmoothingFunction
                ( [], 0.0 )
                edges
    in
    lines


scalePathToDisplay : List ( Float, Float ) -> List ( Float, Float )
scalePathToDisplay unscaled =
    let
        scalePoint ( x, y ) =
            ( viewWidth * x / scaleWidthKilometers / 1000
            , y * strengthToHeightFactor / logBase 10 (10 + y)
              -- This adjustment is empirical.
            )

        -- TODO: constants!
    in
    List.map scalePoint unscaled
