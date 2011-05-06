uniform mat4 WVPMatrix;
uniform float Blend;
uniform float WhichSlice;
varying float ZVal;
//----------------------------------------------------------------------------
void v_VOIVertex()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    // Pass through the vertex color.
    gl_FrontColor.xyz = gl_Color.xyz;
    gl_FrontColor.a = Blend;

    ZVal = gl_Vertex.z;
    if ( WhichSlice == 0.0 )
    {
        ZVal = gl_Vertex.x;
    }
    if ( WhichSlice == 1.0 )
    {
        ZVal = gl_Vertex.y;
    }
}
