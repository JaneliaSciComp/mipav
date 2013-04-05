//----------------------------------------------------------------------------
vec3 MapToUnit (vec3 kVector)
{
    // map [-1,1] to [0,1]
    vec3 kV = vec3(0.0,0.0,0.0);
    kV.x = 0.5*kVector.x + 0.5;
    kV.y = 0.5*kVector.y + 0.5;
    kV.z = 0.5*kVector.z + 0.5;
    //return 0.5*kVector + 0.5;
    return kV;
}
//----------------------------------------------------------------------------
vec3 MapFromUnit (vec3 kVector)
{
    // map [0,1] to [-1,1]
    vec3 kV = vec3(0.0,0.0,0.0);
    kV.x = 2.0*kVector.x - 1.0;
    kV.y = 2.0*kVector.y - 1.0;
    kV.z = 2.0*kVector.z - 1.0;
    //return 2.0*kVector - (1.0,1.0,1.0);
    return kV;
}
//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
uniform vec3   CameraWorldPosition;
uniform vec3   LightDir;
uniform vec4   WaveDirX;
uniform vec4   WaveDirY;
uniform vec4   WaveSpeed;
uniform vec4   WaveOffset;
uniform vec4   WaveHeight;
uniform vec4   BumpSpeed;
uniform vec4   Constants;

in vec3 inPosition;
in vec3 inNormal;
in vec2 inTexcoord0;
in vec4 inColor0;
out vec4 varTexcoord0;
out vec4 varTexcoord1;
out vec4 varTexcoord2;
out vec4 varTexcoord3;
out vec4 varTexcoord4;
out vec4 varTexcoord5;
out vec4 varTexcoord6;
void v_RipplingOceanV()
{
    // A numerical constant.
    float fTwoPi = 6.28318530717958647692;

    // The shader has 4 waves.  Each wave has a direction (in tangent space)
    // of (kWaveDirX[i],kWaveDirY[i]) and some speed, height, and offset into
    // the sin function.
 
    // Wave position at a given time is an input to the sinusoidal function.
    vec4 kOffset = inTexcoord0.x*WaveDirX + inTexcoord0.y*WaveDirY + 
        WaveSpeed*Constants.w + WaveOffset;

    // Map the offset components into the interval [-pi/2,pi/2).
    vec4 kFrcOffset = fract(kOffset) - 0.5;
    kFrcOffset *= fTwoPi;

    // Get the sin and cos values of the fractional offset components.
    //sincos(kFrcOffset,kSin,kCos);
    vec4 kSin = sin(kFrcOffset);
    vec4 kCos = cos(kFrcOffset);

    // Add each of the four waves to the wave height.  The sin portion is the
    // wave height.  The cos part (derivative) will be the change in normal.
    float fWaveHeight = dot(kSin,WaveHeight);

    // Add this wave height to the original position (along the normal).
    vec4 kWaveModelPosition;
    kWaveModelPosition.xyz = fWaveHeight*inNormal.xyz + inPosition.xyz;
    kWaveModelPosition.w = 1.0;
    gl_Position = WVPMatrix*kWaveModelPosition;

    // Get the cos height of the wave.
    vec4 kCosWaveHeight = kCos*WaveHeight;
    
    // Calculate a normal, tangent, and binormal for a coordinate system for
    // the pixel shader to use.
    vec3 kNormalOffset;
    kNormalOffset.x = -dot(kCosWaveHeight,WaveDirX);
    float fTemp = -dot(kCosWaveHeight,WaveDirY);
    //kNormalOffset.yz = -dot(kCosWaveHeight,WaveDirY);
    kNormalOffset.y = fTemp;
    kNormalOffset.z = fTemp;
    kNormalOffset *= Constants.x;

    vec3 kTmpNormal = inNormal.xyz;
    kTmpNormal.xy += kNormalOffset.xy;
    kTmpNormal = normalize(kTmpNormal);    
    varTexcoord5.xyz = MapToUnit(kTmpNormal);
    
    vec3 kTmpTangent = MapFromUnit(inColor0.xyz);
    kTmpTangent.z += kNormalOffset.z;
    kTmpTangent = normalize(kTmpTangent);
    varTexcoord3.xyz = MapToUnit(kTmpTangent);

    varTexcoord4.xyz = MapToUnit(normalize(cross(kTmpNormal,kTmpTangent)));

    // Calculate the view direction for the vertex.
    varTexcoord2.xyz = MapToUnit(
        normalize(kWaveModelPosition.xyz - CameraWorldPosition));

    // Create texture coordinates.  The bump maps have a speed of bumpspeed
    // which is offset from their original texture coordinates.  If you want
    // the texture to repeat on the quad more often, then ramp up TexRepeat,
    // which gives the impression of being farther away.
    varTexcoord0.xy = (Constants.w*BumpSpeed.xy + Constants.z*inTexcoord0.xy);
    
    // Swizzle so that the textures will never line up.
    varTexcoord1.yx = (Constants.w*BumpSpeed.wz + Constants.z*inTexcoord0.xy);

    // Return the light direction, which assumes kLightDir is normalized.
    // The w-component is used to pass through the ambient value.
    varTexcoord6.xyz = MapToUnit(LightDir);
    varTexcoord6.w = Constants.y;
}
//----------------------------------------------------------------------------
