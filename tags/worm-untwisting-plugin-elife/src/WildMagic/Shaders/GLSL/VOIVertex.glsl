uniform mat4 WVPMatrix;
uniform float Blend;
uniform float WhichSlice;

in vec3 inPosition;
in vec4 inColor0;

out float ZVal;
out vec4 varColor;

//----------------------------------------------------------------------------
void v_VOIVertex()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);

    // Pass through the vertex color.
    varColor.xyz = inColor0.xyz;
    varColor.a = Blend;

    ZVal = inPosition.z;
    if ( WhichSlice == 0.0 )
    {
        ZVal = inPosition.x;
    }
    if ( WhichSlice == 1.0 )
    {
        ZVal = inPosition.y;
    }
}
