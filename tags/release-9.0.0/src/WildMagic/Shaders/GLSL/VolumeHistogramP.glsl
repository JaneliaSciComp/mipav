//----------------------------------------------------------------------------
void p_VolumeHistogramP()
{
    gl_FragColor = gl_Color;
    if ( gl_FragColor.a == 0.0 )
    {
        discard;
    }
//     gl_FragColor.xyz = gl_Color.xyz;
//     gl_FragColor.a = 1;
}
//----------------------------------------------------------------------------
