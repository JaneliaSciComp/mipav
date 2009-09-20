//----------------------------------------------------------------------------
uniform sampler3D imageA; 

void v_VolumeHistogramColumnsV()
{
    vec4 color = texture3D(imageA, gl_MultiTexCoord0.xyz );
    gl_FrontColor = vec4(0.0,0.0,0.0,1.0);
    gl_FrontColor.g = color.r;
    gl_Position = gl_Vertex;
    gl_Position.y = 0;
}
//----------------------------------------------------------------------------
