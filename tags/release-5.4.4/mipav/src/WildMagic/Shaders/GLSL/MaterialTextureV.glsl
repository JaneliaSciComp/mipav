//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
uniform vec4 MaterialDiffuse;
void v_MaterialTextureV()
   
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*gl_Vertex;

    // Pass through the material diffuse color.
    gl_FrontColor = MaterialDiffuse;

    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
