uniform mat4 WVPMatrix;

in vec3  inPosition;  
out vec4 varColor;

//----------------------------------------------------------------------------
void  v_BlackColor4()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);
    
    // Black.
    varColor = vec4(0.0, 0.0, 0.0, 1.0);
} 

