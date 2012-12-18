uniform mat4 WVPMatrix;
in vec3 inPosition;
void  v_DefaultV()
{
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);
} 
