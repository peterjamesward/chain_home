module CRT_WebGL exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Messages exposing (Msg)
import Types exposing (Echo, Point)
import WebGL exposing (Mesh, Shader, clearColor)


crt : Bool -> Float -> List Echo -> Html Msg
crt fullScreen time echoes =
    let
        styles =
            if fullScreen then
                [ width 1800
                , height 800
                , style "display" "block"
                , style "width" "1100px"
                ]

            else
                [ width 1800
                , height 800
                , style "display" "block"
                , style "width" "640px"
                ]
    in
    WebGL.toHtmlWith
        [ clearColor 0.02 0.02 0.02 0.0 ]
        styles
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            (uniforms time echoes)
        ]



-- Leave a border around the CRT line, to accommodate the range scale


xLimit =
    1.0


yLimit =
    1.0


mesh : Mesh { position : Vec3 }
mesh =
    -- The mesh corners adjusted empirically to align with range scale.
    WebGL.triangles
        [ ( { position = vec3 -1 1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 -1 -1 0 }
          )
        , ( { position = vec3 -1 -1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 1 -1 0 }
          )
        ]


type alias Uniforms =
    { iResolution : Vec3
    , iTime : Float
    , numRaids : Int
    , raid0 : Vec4
    , raid1 : Vec4
    , raid2 : Vec4
    , raid3 : Vec4
    , raid4 : Vec4
    , raid5 : Vec4
    , raid6 : Vec4
    , raid7 : Vec4
    , raid8 : Vec4
    , raid9 : Vec4
    , raid10 : Vec4
    , raid11 : Vec4
    , raid12 : Vec4
    , raid13 : Vec4
    , raid14 : Vec4
    , raid15 : Vec4
    }


normaliseEcho : Echo -> Vec4
normaliseEcho echo =
    vec4
        -- distance in metres, map to [0,1]
        (echo.r / 160000)
        -- empirically seems to be [0, 10], map to [0,1]
        (logBase 10 (1 + echo.amplitude) / 8.0)
        -- number of craft.
        (toFloat echo.strength)
        echo.phase


echoToVec : Int -> Array Vec4 -> Vec4
echoToVec i echoes =
    -- With new graphics, keep this in world space, let the GPU sort it out.
    Array.get i echoes
        |> Maybe.withDefault
            (vec4 0.0 0.0 0.0 0.0)


uniforms : Float -> List Echo -> Uniforms
uniforms time echoes =
    let
        echoArray =
            Array.fromList <| List.map normaliseEcho echoes

        _ =
            Debug.log "Amplitudes" echoArray
    in
    -- Apologies this is chugly but the Elm GLSL parser does not accept array, for now.
    { iResolution = vec3 1600 800 0
    , iTime = time / 10
    , numRaids = min 16 <| List.length echoes
    , raid0 = echoToVec 0 echoArray
    , raid1 = echoToVec 1 echoArray
    , raid2 = echoToVec 2 echoArray
    , raid3 = echoToVec 3 echoArray
    , raid4 = echoToVec 4 echoArray
    , raid5 = echoToVec 5 echoArray
    , raid6 = echoToVec 6 echoArray
    , raid7 = echoToVec 7 echoArray
    , raid8 = echoToVec 8 echoArray
    , raid9 = echoToVec 9 echoArray
    , raid10 = echoToVec 10 echoArray
    , raid11 = echoToVec 11 echoArray
    , raid12 = echoToVec 12 echoArray
    , raid13 = echoToVec 13 echoArray
    , raid14 = echoToVec 14 echoArray
    , raid15 = echoToVec 15 echoArray
    }


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


vertexShader : Shader { position : Vec3 } Uniforms { vFragCoord : Vec2 }
vertexShader =
    [glsl|

        precision mediump float;
        attribute vec3 position;
        varying vec2 vFragCoord;
        uniform vec3 iResolution;
        void main () {
            gl_Position = vec4(position, 1.0);
            vFragCoord = (position.xy + 1.0) / 2.0 * iResolution.xy;
        }

  |]


fragmentShader : WebGL.Shader {} Uniforms { vFragCoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec2 vFragCoord;
        uniform vec3 iResolution;
        uniform float iTime;
        uniform int numRaids;
        uniform vec4 raid0;
        uniform vec4 raid1;
        uniform vec4 raid2;
        uniform vec4 raid3;
        uniform vec4 raid4;
        uniform vec4 raid5;
        uniform vec4 raid6;
        uniform vec4 raid7;
        uniform vec4 raid8;
        uniform vec4 raid9;
        uniform vec4 raid10;
        uniform vec4 raid11;
        uniform vec4 raid12;
        uniform vec4 raid13;
        uniform vec4 raid14;
        uniform vec4 raid15;

        vec3 COL2 = vec3(0.0,1.0,0.2);

        float random(in vec3 pos) {
            return fract(sin(dot(pos.xyz, vec3(70.9898, 78.233, 32.4355))) * 43758.5453123);
        }

        float cubic(float x) {
            return x * x * (3.0 - 2.0 * x) ;
        }

        // A "two plane" field pulsates.
        float f2(float x) {
            return 2.0 * sin(iTime * 3.0);
        }

        // Think of this as a mass raid "field", deliberately messy.
        float fn(float x) {
            float f = 0.0;
            f += sin(x * 512.0 + 0.0) * sin(iTime * 9.0);
            f += sin(x * 256.0 + 0.0) * sin(iTime * 8.0);
            f += sin(x * 128.0 + 0.0) * sin(iTime * 6.0);
            f += sin(x * 64.0 + 0.1) * sin(iTime * 4.0);
            f += sin(x * 32.0 + 0.2) * sin(iTime * 3.0);
            f /= 5.0;
            return f;
        }

        float includeRaid(vec4 raid, float x) {
            // Note lack of flow control means we calculate all options here!
            float f1Component = float(raid.z == 1.0);
            float f2Component = f2(x) * float(raid.z == 2.0);
            float fnComponent = fn(x) * float(raid.z > 2.0);
            float depth = raid.y * clamp(0.0,1.0,f1Component + f2Component + fnComponent);
            return depth;
        }

       // We need an envelope with cubic sidewalls and a flat top.
       // So this is a variant of "cubicPulse".
       // Aim is that this will work in all cases.
       // l = left edge (not centre)
       // w = width of flat top
       // x = the location to be evaluated
        float envelope( float l, float w, float x ) {
            float cubicWidth = 0.06;
            float leadingEdgeX = (x - l)/cubicWidth;
            float trailingEdgeX = (l + w - x)/cubicWidth;
            leadingEdgeX = clamp(leadingEdgeX, 0.0, 1.0);
            trailingEdgeX = clamp(trailingEdgeX, 0.0, 1.0);
            float leadingEdgeY = cubic(leadingEdgeX);
            float trailingEdgeY = cubic(trailingEdgeX);

            return min(leadingEdgeY, trailingEdgeY);
        }

        // Each raid has an amplitude (x) and a phase (y).
        vec2 raidContribution(vec4 raid, vec2 xy) {
            // x and y in [0,1],
            // raid.x is range in [0,1].
            // raid.y is calculated amplitude nominally in [0,1] ??

            // If x = range, return amplitude.
            // As x moves away from range, use smoothstep.
            float xDist = abs ( xy.x - raid.x );
            float amp = includeRaid(raid, xy.x) ;//raid.y;

            // Increase factor here to get a narrower pulse!
            //float xDistForCube = clamp(0.0, 1.0, xDist * 20.0);
            //float smoothed = smoothstep(1.0, 0.0, xDistForCube);

            // Using previous 'envelope' function.
            float smoothed = envelope(raid.x, sqrt(raid.z)/10.0, xy.x);

            // Return phase shifted vector for 'correct' addition.
            return vec2(amp * smoothed * cos(raid.w), amp * smoothed * sin(raid.w));
        }

        // NOTE: Important to separate the notions of:
        // 1, The deflection of the beam caused by echoes.
        // 2, The brightness of a pixel caused by distance from beam.
        // 3. Some randomness makes it more "real".

        float deriveSignalFromFieldsAt(vec2 xy) {
            // Each raid has a TBD signature that contributes to deflection.
            vec2 deflection = vec2 (0.01, 0.0);
            deflection += raidContribution(raid0, xy);
            deflection += raidContribution(raid1, xy);
            deflection += raidContribution(raid2, xy);
            deflection += raidContribution(raid3, xy);
            deflection += raidContribution(raid4, xy);
            deflection += raidContribution(raid5, xy);
            deflection += raidContribution(raid6, xy);
            deflection += raidContribution(raid7, xy);
            deflection += raidContribution(raid8, xy);
            deflection += raidContribution(raid9, xy);
            deflection += raidContribution(raid10, xy);
            deflection += raidContribution(raid11, xy);
            deflection += raidContribution(raid12, xy);
            deflection += raidContribution(raid13, xy);
            deflection += raidContribution(raid14, xy);
            deflection += raidContribution(raid15, xy);

            float amplitude = clamp(1.0 - length(deflection), 0.0, 0.95);

            // Intensity decays rapidly away from derived deflection.
            // Increase factor to focus the beam
            float beamDistance = clamp(0.0,1.0,abs(amplitude - xy.y) * 80.0);
            float intensity = smoothstep( 1.0, 0.0, beamDistance );

            // Add some simple random noise, strong near the beam.
            float d3 = random(vec3(xy, iTime));
            float randomSpread = clamp(0.0,d3,pow(1.0 - abs(amplitude - xy.y),32.0));

            return  intensity + randomSpread;
        }

        void mainImage( out vec4 fragColor, in vec2 fragCoord )
        {
            // uv should be [0,1] 0 being bottom left BTW.
        //    vec2 uv = (fragCoord * 2.0 - iResolution.xy) / iResolution.xy;
            vec2 uv = fragCoord.xy/iResolution.xy;

            float strength = deriveSignalFromFieldsAt(uv);

            fragColor = vec4(COL2 * strength,1.0);
        }

        void main() {
          mainImage(gl_FragColor, vFragCoord);
        }

  |]
