uniform vec4 SurfaceScale;
uniform mat4 WVPMatrix;
void v_SurfaceClipV()
{
    if ( SurfaceScale.w != 0.0 )
    {
        gl_Vertex.x *= 2.0/SurfaceScale.x;
        gl_Vertex.y *= 2.0/SurfaceScale.y;
        gl_Vertex.z *= 2.0/SurfaceScale.z;
    }
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;
    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;
    // Pass through the color.
    gl_FrontColor = gl_Color;
}
