//----------------------------------------------------------------------------
uniform float UseSlice;
uniform float Slice;

in float ZVal;
in vec4 varColor;
out vec4 fragColor;

void p_VOISolid ()
{
    if ( UseSlice == 1.0 )
    {
        if ( Slice != ZVal )
        {
            discard;
        }
    }
    fragColor = varColor;
}
//----------------------------------------------------------------------------
