//----------------------------------------------------------------------------
uniform sampler3D imageA; 
uniform float dLogN;
void v_EntropyV()
{
    vec4 data0 = texture3D(imageA, gl_MultiTexCoord0.xyz );
    gl_FrontColor = vec4(0.0,0.0,0.0,0.0);

    if ( data0.r > 0.0 )
    {
        data0.r = ceil(data0.r);
        gl_FrontColor.r += (-data0.r * (log(data0.r) - dLogN));
        gl_FrontColor.a += data0.r;
    }
    if ( data0.g > 0.0 )
    {
        data0.g = ceil(data0.g);
        gl_FrontColor.g += (-data0.g * (log(data0.g) - dLogN));
    }
    if ( data0.b > 0.0 )
    {
        data0.b = ceil(data0.b);
        gl_FrontColor.b += (-data0.b * (log(data0.b) - dLogN));
    }

    gl_Position = gl_Vertex;
    gl_Position.x = 0;
    gl_Position.y = 0;
}
//----------------------------------------------------------------------------
