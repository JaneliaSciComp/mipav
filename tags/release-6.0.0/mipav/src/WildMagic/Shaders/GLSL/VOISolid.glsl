//----------------------------------------------------------------------------
uniform float UseSlice;
uniform float Slice;
varying float ZVal;
void p_VOISolid ()
{
    if ( UseSlice == 1.0 )
    {
        if ( Slice != ZVal )
        {
            discard;
        }
    }
    gl_FragColor = gl_Color;
}
//----------------------------------------------------------------------------
