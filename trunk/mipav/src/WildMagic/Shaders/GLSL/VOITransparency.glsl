//----------------------------------------------------------------------------
uniform float UseSlice;
uniform float Slice;

in float ZVal;
in vec4 varColor;
out vec4 outFragData0;
out vec4 outFragData1;
void p_VOITransparency ()
{
    if ( UseSlice == 1.0 )
    {
        if ( Slice != ZVal )
        {
            discard;
        }
    }
    outFragData0 = vec4(varColor.rgb * varColor.a, varColor.a);
    outFragData1 = vec4(1.0);
}
//----------------------------------------------------------------------------
