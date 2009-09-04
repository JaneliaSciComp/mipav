//----------------------------------------------------------------------------
void p_PassThrough3 ()
{
    vec4 kPixelColor;

    kPixelColor.xyz = gl_Color.xyz;
    kPixelColor.w = 1.0;
    gl_FragColor = kPixelColor;
}

//----------------------------------------------------------------------------
void p_PassThrough4 ()
{
    gl_FragColor = gl_Color;
}
//----------------------------------------------------------------------------
