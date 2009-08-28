//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
uniform vec2 Step;
uniform float dLogN;
uniform float nVoxels;

void p_ImageReduce_Entropy_1D_y ()
{
    gl_FragColor = vec4(0.0);
    // Sample the texture image.
    vec2 index0 = gl_TexCoord[0].xy;
    index0.y -= Step.y;

    vec2 index1 =  gl_TexCoord[0].xy;
    index1.y += Step.y;

    vec4 data0 = texture2D(BaseSampler, index0);
    vec4 data1 = texture2D(BaseSampler, index1);
    
    //float p = 0.0;
    if ( data0.r > 0.0 )
    {
        data0.r = ceil(data0.r);
        //p = data0.r/nVoxels;
        gl_FragColor.r += (-data0.r * (log(data0.r) - dLogN));
        gl_FragColor.g += data0.r;
        //gl_FragColor.r += (-p * log(p));
    }
    if ( data1.r > 0.0 )
    {
        data1.r = ceil(data1.r);
        //p = data1.r/nVoxels;
        gl_FragColor.r += (-data1.r * (log(data1.r) - dLogN));
        gl_FragColor.g += data1.r;
        //gl_FragColor.r += (-p * log(p));
    }
}
//----------------------------------------------------------------------------
