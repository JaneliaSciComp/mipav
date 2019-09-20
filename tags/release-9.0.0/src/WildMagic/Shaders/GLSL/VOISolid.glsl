//----------------------------------------------------------------------------
uniform float UseSlice;
uniform float Slice;
uniform float Range;

in float ZVal;
in vec4 varColor;
out vec4 fragColor;

void p_VOISolid ()
{
    if ( UseSlice == 1.0 )
    {
        if ( abs(Slice - ZVal) > Range )
        {
            discard;
        }
    }
    fragColor = varColor;
}
//----------------------------------------------------------------------------
