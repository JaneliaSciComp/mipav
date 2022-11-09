uniform mat4 WVPMatrix;
in vec3 inPosition;
in vec3 inTexcoord0;
out vec4 outPos;
out vec3 varTexCoord;

/** Raycasting vertex program implementation */
void v_VolumeShaderVertex()
{    
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);
    // Pass the position to the pixel-shader.
    outPos = gl_Position; 
    // Pass through the texture coordinate.
    varTexCoord = inTexcoord0;
}
