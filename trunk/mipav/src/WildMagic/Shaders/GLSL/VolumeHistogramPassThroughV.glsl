//----------------------------------------------------------------------------
uniform sampler3D imageA; 

void v_VolumeHistogramPassThroughV()
{
    vec4 color = texture3D(imageA, gl_MultiTexCoord0.xyz );
    gl_FrontColor = vec4(0.0,0.0,0.0,1.0);
    gl_FrontColor.r = color.r;
    gl_Position = gl_Vertex;
}
//----------------------------------------------------------------------------
