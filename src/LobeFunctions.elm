module LobeFunctions exposing (..)

-- Some RDF lobe functions TO GO IN DIFFERENT NODULE
txHiVertReflectedLobe alpha = (1 - 6 * alpha) * abs (sin (24 * alpha))
txHiVertOmniLobe alpha      = sin (7 * alpha)
txHorizReflectedLobe θ  = (cos θ)^2
txHorizOmniLobe θ       = cos θ

rxHorizLobe θ  = cos θ
rxLoVertLobe alpha = sin (7 * alpha)
rxHiVertLobe alpha = (1 - 6 * alpha) * abs (sin (24 * alpha))

