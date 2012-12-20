
uniform float ReverseFace;
uniform mat4 WVPMatrix;

in vec3 inPosition;
in vec3 inNormal;
in vec3 inTexCoord0;
in vec4 inColor0;

out vec3 varTexCoord;
out vec4 varPos;
out vec3 varNormal;
out vec4 varColor;

void v_MipavLightingFragmentV()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);

    // Pass through the texture coordinate.
    varTexCoord = inTexCoord0;

    varPos = vec4(inPosition, 1.0);
    varNormal = inNormal;
    if ( ReverseFace == 1.0 )
    {
        varNormal.x *= -1.0;
        varNormal.y *= -1.0;
        varNormal.z *= -1.0;
    }
    
    varColor = inColor0;
}
