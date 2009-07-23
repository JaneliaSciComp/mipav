//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
uniform vec2 Step;
uniform float dLogN;
uniform float nVoxels;
void p_ImageReduce_Entropy_1DP ()
{
    gl_FragColor = vec4(0.0);
    // Sample the texture image.
    vec2 index0 = gl_TexCoord[0].xy;
    index0.x -= Step.x;
    index0.y = 0.0;

    vec2 index1 =  gl_TexCoord[0].xy;
    index1.x += Step.x;
    index1.y = 0.0;

    vec4 data0 = texture2D(BaseSampler, index0);
    vec4 data1 = texture2D(BaseSampler, index1);
    
    //float p = 0.0;
    if ( data0.r > 0.0 )
    {
        //p = data0.r/nVoxels;
        gl_FragColor.r += (-data0.r * (log(data0.r) - dLogN));
        //gl_FragColor.r += (-p * log(p));
    }
    if ( data1.r > 0.0 )
    {
        //p = data1.r/nVoxels;
        gl_FragColor.r += (-data1.r * (log(data1.r) - dLogN));
        //gl_FragColor.r += (-p * log(p));
    }
}
//----------------------------------------------------------------------------
