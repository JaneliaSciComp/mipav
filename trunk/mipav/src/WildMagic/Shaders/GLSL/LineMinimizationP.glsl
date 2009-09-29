//----------------------------------------------------------------------------
uniform float logSamples;
uniform float nSamples;
uniform float nSamplesInv;
uniform float nDims;
void p_LineMinimizationP()
{
    float fEntropyDual = gl_Color.r*nSamplesInv;
    float fEntropyY = gl_Color.g*nSamplesInv;
    float fEntropyX = gl_Color.b*nSamplesInv;
    float fOverlap = gl_Color.a;

    if ( ( (nDims < 3) && (fOverlap > 1000) ) ||
         ( (nDims == 3) && (fOverlap > (0.15 * nSamples)) ) )
    {
        float nRatio = nDims / fOverlap;
        float logRatio = log(nRatio);
        fEntropyX  = (nRatio * fEntropyX) - logRatio;
        fEntropyY  = (nRatio * fEntropyY) - logRatio;
        fEntropyDual = (nRatio * fEntropyDual) - logRatio;
    } else
    {
        fEntropyX = logSamples;
        fEntropyY = logSamples;
        fEntropyDual = 2.0 * logSamples;
    }

    gl_FragColor.r = fEntropyDual/(fEntropyX + fEntropyY);
    gl_FragColor.g = 0.0;
    gl_FragColor.b = 0.0;
    gl_FragColor.a = 0.0;
}
//----------------------------------------------------------------------------
