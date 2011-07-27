//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
varying vec4 position;
void v_BoundingBoxGridV ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;
    
    // Pass through the vertex color.
    gl_FrontColor = gl_Color;

    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;

    position.xyz = gl_Vertex.xyz;
    position.w = gl_Position.w;
}
//----------------------------------------------------------------------------
