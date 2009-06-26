uniform mat4 WVPMatrix;
varying vec4 outPos;

/** Raycasting vertex program implementation */
void v_VolumeShaderVertex()
{    
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;
    // Pass the position to the pixel-shader.
    outPos = gl_Position; 
    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
