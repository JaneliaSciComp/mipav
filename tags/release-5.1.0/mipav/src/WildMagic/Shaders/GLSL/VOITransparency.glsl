//----------------------------------------------------------------------------
uniform float UseSlice;
uniform float Slice;
varying float ZVal;
void p_VOITransparency ()
{
    if ( UseSlice == 1.0 )
    {
        if ( Slice != ZVal )
        {
            discard;
        }
    }
    gl_FragData[0] = vec4(gl_Color.rgb * gl_Color.a, gl_Color.a);
    gl_FragData[1] = vec4(1.0);
}
//----------------------------------------------------------------------------
