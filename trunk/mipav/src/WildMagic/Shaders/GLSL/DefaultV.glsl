uniform mat4 WVPMatrix;
void  v_DefaultV()
{
    gl_Position = WVPMatrix * gl_Vertex;
} 
