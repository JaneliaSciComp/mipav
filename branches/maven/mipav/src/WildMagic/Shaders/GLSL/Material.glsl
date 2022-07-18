uniform mat4 WVPMatrix;
uniform vec4   MaterialDiffuse;
in vec3  inPosition;  
out vec4 varColor;
//----------------------------------------------------------------------------
void v_Material ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);

    // Use the material diffuse color as the vertex color.
    varColor = MaterialDiffuse;
}
//----------------------------------------------------------------------------
