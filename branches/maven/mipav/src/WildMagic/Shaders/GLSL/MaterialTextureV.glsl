//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
uniform vec4 MaterialDiffuse;
in vec3  inPosition;  
in vec3 inTexcoord0;
out vec3 varTexCoord;
out vec4 varColor;
void v_MaterialTextureV()
   
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*vec4(inPosition, 1.0);

    // Pass through the material diffuse color.
    varColor = MaterialDiffuse;

    // Pass through the texture coordinate.
    varTexCoord = inTexcoord0;
}
