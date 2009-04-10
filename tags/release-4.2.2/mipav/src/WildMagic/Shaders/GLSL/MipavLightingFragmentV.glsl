varying vec4       kInPos;
varying vec3       kInNormal;
uniform float ReverseFace;
uniform mat4 WVPMatrix;

void v_MipavLightingFragmentV()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;

    kInPos = gl_Vertex;
    kInNormal = gl_Normal;
    if ( ReverseFace == 1.0 )
    {
        kInNormal.x *= -1.0;
        kInNormal.y *= -1.0;
        kInNormal.z *= -1.0;
    }
    
    gl_FrontColor = gl_Color;
}
