//----------------------------------------------------------------------------
uniform sampler2D imageA; 
uniform float dLogN;
void p_EntropyP()
{
    vec4 dataA0 = texture2D(imageA, gl_TexCoord[0].xy );
    gl_FragColor = vec4(0.0);

    if ( dataA0.r > 0.0 )
    {
        dataA0.r = ceil(dataA0.r);
        gl_FragColor.r += (-dataA0.r * (log(dataA0.r) - dLogN));
        gl_FragColor.a += dataA0.r;
    }
    if ( dataA0.g > 0.0 )
    {
        dataA0.g = ceil(dataA0.g);
        gl_FragColor.g += (-dataA0.g * (log(dataA0.g) - dLogN));
    }
    if ( dataA0.b > 0.0 )
    {
        dataA0.b = ceil(dataA0.b);
        gl_FragColor.b += (-dataA0.b * (log(dataA0.b) - dLogN));
    }
}
//----------------------------------------------------------------------------
