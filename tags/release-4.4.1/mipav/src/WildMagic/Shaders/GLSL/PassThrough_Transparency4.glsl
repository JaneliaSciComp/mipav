//----------------------------------------------------------------------------
void p_PassThrough_Transparency4 ()
{
    gl_FragData[0] = vec4(gl_Color.rgb * gl_Color.a, gl_Color.a);
    gl_FragData[1] = vec4(1.0);
}
//----------------------------------------------------------------------------
