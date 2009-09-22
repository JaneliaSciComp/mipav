//----------------------------------------------------------------------------
uniform sampler3D imageA; 
uniform float dLogN;
void v_EntropyV()
{
    vec4 dataA0 = texture3D(imageA, gl_MultiTexCoord0.xyz );
    gl_FrontColor = vec4(0.0,0.0,0.0,0.0);

    if ( dataA0.r > 0.0 )
    {
        dataA0.r = ceil(dataA0.r);
        gl_FrontColor.r += (-dataA0.r * (log(dataA0.r) - dLogN));
        gl_FrontColor.a += dataA0.r;
    }
    if ( dataA0.g > 0.0 )
    {
        dataA0.g = ceil(dataA0.g);
        gl_FrontColor.g += (-dataA0.g * (log(dataA0.g) - dLogN));
    }
    if ( dataA0.b > 0.0 )
    {
        dataA0.b = ceil(dataA0.b);
        gl_FrontColor.b += (-dataA0.b * (log(dataA0.b) - dLogN));
    }

    gl_Position = gl_Vertex;
    gl_Position.x = 0;
    gl_Position.y = 0;
}
//----------------------------------------------------------------------------
