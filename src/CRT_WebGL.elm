module CRT_WebGL exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Messages exposing (Msg)
import Types exposing (Echo, Point)
import WebGL exposing (Mesh, Shader, clearColor)


crt : Float -> List Echo -> Html Msg
crt time echoes =
    WebGL.toHtmlWith
        [ clearColor 0.02 0.02 0.02 0.0
        ]
        [ width 800
        , height 400
        , style "display" "block"
        , style "width" "640px"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            (uniforms time echoes)
        ]

mesh : Mesh { position : Vec3 }
mesh =
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
    case Array.get i echoes of
        Just echo ->
            vec3
                (echo.r / 80000 - 1.0)
                (echo.amplitude / 10.0)
                1.0

        _ ->
            vec3 -1.0 0.0 0.0


uniforms : Float -> List Echo -> Uniforms
uniforms time echoes =
    let
        echoArray =
            Array.fromList echoes
    in
    -- Apologies this is chugly but the Elm GLSL parser does not accept array, for now.
    { iResolution = vec3 800 400 0
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

        vec3 COL1 = vec3(1.0,0.0,0.0);
        vec3 COL2 = vec3(0.0,1.0,0.0);

        float cubicPulse( float c, float w, float x ) {
            x = abs(x - c); // NOTE 0 <= x <= +w
            if (x > w) return 0.0;
            x /= w; // 0 <= x <= +1 (width is actually 2w)
            return 1.0 - x * x * (3.0 - 2.0 * x);
        }

        void mainImage( out vec4 fragColor, in vec2 fragCoord )
        {
            vec2 uv = fragCoord.xy/iResolution.xy;
            vec2 uvn = 2.0 * uv - 1.0;

            // Think of this as a single target "field"
            float f1 = -3.0;

            // A "two plane" field
            float f2 = f1 - (3.0 * sin(iTime * 3.0));

            // Think of this as a mass raid "field".
            float mrf = f1 + f2 - 6.0;
            mrf += 2.0 * sin(uv.x * 512.0 + 0.0) * sin(iTime * 9.0);
            mrf += 2.0 * sin(uv.x * 256.0 + 0.0) * sin(iTime * 8.0);
            mrf += 2.0 * sin(uv.x * 128.0 + 0.0) * sin(iTime * 6.0);
            mrf += 2.0 * sin(uv.x * 128.0 + 0.5) * sin(iTime * 5.0);
            mrf += 1.0 * sin(uv.x * 64.0 + 0.1) * sin(iTime * 4.0);
            mrf += 1.0 * sin(uv.x * 32.0 + 0.2) * sin(iTime * 3.0);
            mrf /= 2.0;

            // Add a noise field (use our existing one).
            // Lower the resolution of the x line to make the noise less noisy.
            // add time to the noise parameters so it's animated.
            // I want a component that is more 'spikey'.
            float noise = 0.0;
            noise += sin(uv.x * 256.0 + 0.0) * sin(iTime * 7.0);
            noise += sin(uv.x * 512.0 + 0.0) * sin(iTime * 5.0);
            noise += sin(uv.x * 1024.0 + 0.0) * sin(iTime * 11.0);
            noise /= 8.0;

            // Spikey looking noise.
            float lumpy = 0.0;
            lumpy += sin(uv.x * 256.0);
            lumpy = 0.0 - float(lumpy < -0.8);
            float lumpyTime = 0.0;
            lumpyTime += sin(iTime * 11.0) + sin(iTime * 19.0);
            lumpyTime = float(lumpyTime > 1.9);

            float sawtooth = 0.0;
            sawtooth = abs(0.5 - fract(uv.x * 20.0));
            float bumps = 5.0 * lumpy * lumpyTime * sawtooth;

            // Now expose a section of the field where we have raids.
            // Note pulse width sqrt(N)/100 looks ok.
            float raid1 = f1 * cubicPulse(0.2, 0.01, uv.x); // One
            float raid2 = f2 * cubicPulse(0.3, 0.01, uv.x);  // Two
            float raid3 = mrf * cubicPulse(0.5, 0.0140, uv.x); // Three
            float raid4 = mrf * cubicPulse(0.7, 0.0200, uv.x); // Many
            raid4 += mrf * cubicPulse(0.72, 0.0200, uv.x); // Many

            float beamY = bumps + noise + raid1 + raid1 + raid2 + raid3 + raid4;

            // Fiddle with coordinate (needs some work).
            beamY = beamY/50.0 + 0.78;

            //create the beam by simple y distance that falls off quickly. (? smoothstep ?)
            float i = pow(1.0 - abs(uv.y - beamY), 25.0);
            //float i = cubicPulse(beamY, 0.04, uv.y);

            vec3 col = vec3(i) * mix(COL1,COL2,i);

            fragColor = vec4(col,1.0);
        }

        void main() {
          mainImage(gl_FragColor, vFragCoord);
        }

  |]
