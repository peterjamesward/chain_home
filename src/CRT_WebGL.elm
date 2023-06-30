module CRT_WebGL exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
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
    0.93


yLimit =
    0.9


mesh : Mesh { position : Vec3 }
mesh =
    -- The mesh corners adjusted empirically to align with range scale.
    WebGL.triangles
        [ ( { position = vec3 (0 - xLimit) yLimit 0 }
          , { position = vec3 xLimit yLimit 0 }
          , { position = vec3 (0 - xLimit) (0 - yLimit) 0 }
          )
        , ( { position = vec3 (0 - xLimit) (0 - yLimit) 0 }
          , { position = vec3 xLimit yLimit 0 }
          , { position = vec3 xLimit (0 - yLimit) 0 }
          )
        ]


type alias Uniforms =
    { iResolution : Vec3
    , iTime : Float
    , numRaids : Int
    , raid0 : Vec3
    , raid1 : Vec3
    , raid2 : Vec3
    , raid3 : Vec3
    , raid4 : Vec3
    , raid5 : Vec3
    , raid6 : Vec3
    , raid7 : Vec3
    , raid8 : Vec3
    , raid9 : Vec3
    , raid10 : Vec3
    , raid11 : Vec3
    , raid12 : Vec3
    , raid13 : Vec3
    , raid14 : Vec3
    , raid15 : Vec3
    }


echoToVec : Array Echo -> Int -> Vec3
echoToVec echoes i =
    -- Echoes are massaged into the "raids" uniforms and the WebGL -1..+1 coordinates.
    -- We can use z to allow us to colour the raids differently in tutorial mode.
    -- Use z as raid size, which will decide the number of cubic curves it needs.
    -- We should here do the conversion into 0..+1 coordinates.
    case Array.get i echoes of
        Just echo ->
            vec3
                ((1 - xLimit) / 2 + xLimit * echo.r / 160000)
                (echo.amplitude * 40.0)
                (toFloat echo.strength)

        _ ->
            vec3 -1.0 0.0 0.0


uniforms : Float -> List Echo -> Uniforms
uniforms time echoes =
    let
        echoArray =
            Array.fromList echoes
    in
    -- Apologies this is chugly but the Elm GLSL parser does not accept array, for now.
    { iResolution = vec3 1600 800 0
    , iTime = time / 10
    , numRaids = min 16 <| List.length echoes
    , raid0 = echoToVec echoArray 0
    , raid1 = echoToVec echoArray 1
    , raid2 = echoToVec echoArray 2
    , raid3 = echoToVec echoArray 3
    , raid4 = echoToVec echoArray 4
    , raid5 = echoToVec echoArray 5
    , raid6 = echoToVec echoArray 6
    , raid7 = echoToVec echoArray 7
    , raid8 = echoToVec echoArray 8
    , raid9 = echoToVec echoArray 9
    , raid10 = echoToVec echoArray 10
    , raid11 = echoToVec echoArray 11
    , raid12 = echoToVec echoArray 12
    , raid13 = echoToVec echoArray 13
    , raid14 = echoToVec echoArray 14
    , raid15 = echoToVec echoArray 15
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
        uniform vec3 raid0;
        uniform vec3 raid1;
        uniform vec3 raid2;
        uniform vec3 raid3;
        uniform vec3 raid4;
        uniform vec3 raid5;
        uniform vec3 raid6;
        uniform vec3 raid7;
        uniform vec3 raid8;
        uniform vec3 raid9;
        uniform vec3 raid10;
        uniform vec3 raid11;
        uniform vec3 raid12;
        uniform vec3 raid13;
        uniform vec3 raid14;
        uniform vec3 raid15;

        vec3 COL1 = vec3(0.8,0.0,0.0);
        vec3 COL2 = vec3(0.0,0.8,0.2);

        float cubicPulse( float c, float w, float x ) {
            x = abs(x - c); // NOTE 0 <= x <= +w
            if (x > w) return 0.0;
            x /= w; // 0 <= x <= +1 (width is actually 2w)
            return 1.0 - x * x * (3.0 - 2.0 * x);
        }

        float cubic(float x) {
            return x * x * (3.0 - 2.0 * x) ;
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

        // Think of this as a single target "field"
        float f1(float x) {
            return -2.0;
        }

        // A "two plane" field
        float f2(float x) {
            return 2.0 * sin(iTime * 3.0);
        }

        // Think of this as a mass raid "field".
        float fn(float x) {
            float f = -1.3;
            f += 1.0 * sin(x * 512.0 + 0.0) * sin(iTime * 9.0);
            f += 1.0 * sin(x * 256.0 + 0.0) * sin(iTime * 8.0);
            f += 1.0 * sin(x * 128.0 + 0.0) * sin(iTime * 6.0);
            f += 1.0 * sin(x * 64.0 + 0.1) * sin(iTime * 4.0);
            f += 1.0 * sin(x * 32.0 + 0.2) * sin(iTime * 3.0);
            f /= 4.0;
            return f;
        }

        float includeRaid(vec3 raid, float x) {
            float f1Component = f1(x) * float(raid.z == 1.0);
            float f2Component = f2(x) * float(raid.z == 2.0);
            float fnComponent = fn(x) * float(raid.z > 2.0);
            float depth = min(f1Component + f2Component + fnComponent, 0.0);
            float shape = raid.y * envelope(raid.x, sqrt(raid.z)/100.0, x);
            depth /= 1.0 + sqrt(raid.z);
            return shape * depth;
        }

        float deriveSignalFromFieldsAt(float x) {

            // Now expose a section of the field where we have raids.
            float r0 = includeRaid(raid0, x);
            float r1 = includeRaid(raid1, x);
            float r2 = includeRaid(raid2, x);
            float r3 = includeRaid(raid3, x);
            float r4 = includeRaid(raid4, x);
            float r5 = includeRaid(raid5, x);
            float r6 = includeRaid(raid6, x);
            float r7 = includeRaid(raid7, x);
            float r8 = includeRaid(raid8, x);
            float r9 = includeRaid(raid9, x);
            float r10 = includeRaid(raid10, x);
            float r11 = includeRaid(raid11, x);
            float r12 = includeRaid(raid12, x);
            float r13 = includeRaid(raid13, x);
            float r14 = includeRaid(raid14, x);
            float r15 = includeRaid(raid15, x);

            //Combine with artficial phase differences to re-create beating effect.
            // Even though it may be rarely seen.
            vec2 combined = vec2(0.0, 0.0);
            combined.x += r0 * cos(iTime * 1.0);
            combined.x += r1 * cos(iTime * 2.0);
            combined.x += r2 * cos(iTime * 3.0);
            combined.x += r3 * cos(iTime * 4.0);
            combined.x += r4 * cos(iTime * 5.0);
            combined.x += r5 * cos(iTime * 6.0);
            combined.x += r6 * cos(iTime * 7.0);
            combined.x += r7 * cos(iTime * 8.0);
            combined.x += r8 * cos(iTime * 9.0);
            combined.x += r9 * cos(iTime * 10.0);
            combined.x += r10 * cos(iTime * 11.0);
            combined.x += r11 * cos(iTime * 12.0);
            combined.x += r12 * cos(iTime * 13.0);
            combined.x += r13 * cos(iTime * 14.0);
            combined.x += r14 * cos(iTime * 15.0);
            combined.x += r15 * cos(iTime * 16.0);
            combined.y += r0 * sin(iTime * 1.0);
            combined.y += r1 * sin(iTime * 2.0);
            combined.y += r2 * sin(iTime * 3.0);
            combined.y += r3 * sin(iTime * 4.0);
            combined.y += r4 * sin(iTime * 5.0);
            combined.y += r5 * sin(iTime * 6.0);
            combined.y += r6 * sin(iTime * 7.0);
            combined.y += r7 * sin(iTime * 8.0);
            combined.y += r8 * sin(iTime * 9.0);
            combined.y += r9 * sin(iTime * 10.0);
            combined.y += r10 * sin(iTime * 11.0);
            combined.y += r11 * sin(iTime * 12.0);
            combined.y += r12 * sin(iTime * 13.0);
            combined.y += r13 * sin(iTime * 14.0);
            combined.y += r14 * sin(iTime * 15.0);
            combined.y += r15 * sin(iTime * 16.0);

            return sqrt(length(combined));
        }

        void mainImage( out vec4 fragColor, in vec2 fragCoord )
        {
            vec2 uv = fragCoord.xy/iResolution.xy;
            vec2 uvn = 2.0 * uv - 1.0;
            float xMinus = (fragCoord.x - 0.6)/iResolution.x;
            float xPlus = (fragCoord.x + 0.6)/iResolution.x;

            // Add a noise field (use our existing one).
            // Lower the resolution of the x line to make the noise less noisy.
            // add time to the noise parameters so it's animated.
            // I want a component that is more 'spikey'.
            float noise = 0.0;
            noise += sin(uv.x * 256.0 + 0.0) * fract(1000.0 * sin(iTime * 15.0)); //sin(iTime * 7.0);
            noise += sin(uv.x * 512.0 + 0.0) * sin(iTime * 5.0);
            noise += sin(uv.x * 1024.0 + 0.0) * sin(iTime * 11.0);
            noise /= 40.0;

            // Spikey looking noise.
            float lumpy = 0.0;
            lumpy += sin(uv.x * 256.0);
            lumpy = 0.0 - float(lumpy < -0.8);
            float lumpyTime = 0.0;
            lumpyTime += sin(iTime * 7.0) * fract(1000.0 * sin(iTime * 15.0));
            lumpyTime = float(lumpyTime > 0.98);

            float sawtooth = 0.0;
            sawtooth = abs(0.5 - fract(uv.x * 20.0));
            float bumps = 3.0 * lumpy * lumpyTime * sawtooth;
            float signalAmplitude0 = deriveSignalFromFieldsAt(xMinus);
            float signalAmplitude1 = deriveSignalFromFieldsAt(uv.x);
            float signalAmplitude2 = deriveSignalFromFieldsAt(xPlus);
            float signalAmplitude = (signalAmplitude0 + signalAmplitude1 + signalAmplitude1 + signalAmplitude2)/4.0;
            //float slope = abs(signalAmplitude2 - signalAmplitude0) / 10.0;

            // Fiddle with coordinate (needs some work).
            float beamY = bumps - signalAmplitude - noise + signalAmplitude * noise;
            beamY = beamY/10.0 + 0.78;

            float beamYprev = (bumps - signalAmplitude0 - noise)/10.0 + 0.78;
            float beamYnext = (bumps - signalAmplitude2 - noise)/10.0 + 0.78;

            //create the beam by simple y distance that falls off quickly.
            float i = pow(1.0 - abs(uv.y - beamY), 50.0);
            i += pow(1.0 - abs(uv.y - beamYprev), 50.0) * 0.5;
            i += pow(1.0 - abs(uv.y - beamYnext), 50.0) * 0.5;
            i /= 2.0;

            vec3 col = vec3(i) * mix(COL1,COL2,i);

            fragColor = vec4(col,1.0);
        }

        void main() {
          mainImage(gl_FragColor, vFragCoord);
        }

  |]
