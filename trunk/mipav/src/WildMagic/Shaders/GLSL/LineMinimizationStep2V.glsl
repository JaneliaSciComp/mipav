//----------------------------------------------------------------------------
uniform sampler2D bracketImage;
uniform sampler2D entropy;
uniform float minDist;

uniform float logSamples;
uniform float nSamples;
uniform float nSamplesInv;
uniform float nDims;

float estimateMinimum(vec2 A, vec2 B, vec2 C )
{
    float ad = 0.0;
    float bd = 0.0;
    float det = 0.0;
    float xNew;

    ad = ((B.x - C.x) * (B.y - A.y)) - ((B.x - A.x) * (B.y - C.y));
    bd = (-((B.x * B.x) - (C.x * C.x)) * (B.y - A.y)) +
        (((B.x * B.x) - (A.x * A.x)) * (B.y - C.y));
    det = (B.x - C.x) * (C.x - A.x) * (A.x - B.x);
    
    /** Used to prevent division by zero. */
    float TINY = 1.0 * pow(10.0, -20.0);
    if ((abs(det) > TINY) && ((ad / det) < 0.0)) { // quadratic only has a maxima
        A.y = -1.0;
        B.y = -1.0;
        B.y = -1.0;
        xNew = 0.0;
    }
    
    if (abs(ad) > TINY) {
        xNew = -bd / (2.0 * ad);
    }

    else
    {
        A.y = -1.0;
        B.y = -1.0;
        B.y = -1.0;
        xNew = 0.0;
    }
    return xNew;
}

float extrapolatePoint( vec2 A, vec2 B, vec2 C )
{
    // bracket.b must be between bracket.a and bracket.c
    // use the golden ratio (scale similar result)
    /** Golden ratio is .38197 and .61803. */
    float RGOLD = 0.618034;
    /** Golden ratio - second part. */
    float CGOLD = 0.3819660;

    float xNew;

    if (abs(C.x - B.x) > abs(A.x - B.x)) {
        xNew = (CGOLD * C.x) + (RGOLD * B.x);
    } else {
        xNew = (CGOLD * A.x) + (RGOLD * B.x);
    }
    
    return xNew;
}


float nextPoint( vec2 A, vec2 B, vec2 C )
{
    float xNew = estimateMinimum( A, B, C );
    float min = (A.x < C.x) ? A.x : C.x;
    float max = (C.x < A.x) ? A.x : C.x;
    if ( ((A.y == -1.0) && (B.y == -1.0) && (C.y == -1.0)) || (xNew < min) || (xNew > max) )
    {
        xNew = extrapolatePoint( A, B, C );
    }
    return xNew;
}

void v_LineMinimizationStep2V()
{
    vec2 texCoord = vec2(0,0);
    vec4 entropyVal = texture2D(entropy, texCoord );

    float fEntropyDual = entropyVal.r*nSamplesInv;
    float fEntropyY = entropyVal.g*nSamplesInv;
    float fEntropyX = entropyVal.b*nSamplesInv;
    float fOverlap = entropyVal.a;

    if ( ( (nDims < 3) && (fOverlap > 1000) ) ||
         ( (nDims == 3) && (fOverlap > (0.15 * nSamples)) ) )
    {
        float nRatio = nSamples / fOverlap;
        float logRatio = log(nRatio);
        fEntropyX  = (nRatio * fEntropyX) - logRatio;
        fEntropyY  = (nRatio * fEntropyY) - logRatio;
        fEntropyDual = (nRatio * fEntropyDual) - logRatio;
    } else
    {
        fEntropyX = 1.0;
        fEntropyY = 1.0;
        fEntropyDual = 2.0;
    }

    float yNew = fEntropyDual/(fEntropyX + fEntropyY);

    vec2 A = texture2D(bracketImage, texCoord ).xy;

    texCoord.y = 0.5;
    vec2 B = texture2D(bracketImage, texCoord ).xy;

    texCoord.y = 1.0;
    vec2 C = texture2D(bracketImage, texCoord ).xy;
    
    float xNew = nextPoint( A, B, C );
    float directionN = 1.0;

    if (C.x < A.x) {
        directionN = -1.0;
    }
    
    if (abs(xNew - A.x) < minDist) {
        xNew = A.x + (directionN * minDist);
    }
    
    if (abs(xNew - C.x) < minDist) {
        xNew = C.x - (directionN * minDist);
    }
    
    if (abs(xNew - B.x) < minDist) {
        xNew = extrapolatePoint(A, B, C);
    }
    
    if (abs(B.x - A.x) < (4.0 * minDist)) {
        xNew = B.x + (directionN * 5.0 * minDist);
    }
    
    if (abs(B.x - C.x) < (4.0 * minDist)) {
        xNew = B.x - (directionN * 5.0 * minDist);
    }

    float temp;
    if (((xNew - B.x) * (C.x - B.x)) > 0.0) { // is xnew between bracket.c and bracket.b ?
        // swap bracket.a and bracket.c so that xnew is between bracket.a and bracket.b
        temp = A.x;
        A.x = C.x;
        C.x = temp;
        
        temp = A.y;
        A.y = C.y;
        C.y = temp;
    }
    
    if (yNew < B.y) {
        // new interval is [bracket.b,bracket.a] with xNew as best point in the middle
        C.x = B.x;
        C.y = B.y;
        B.x = xNew;
        B.y = yNew;
    } else {
        // new interval is  [bracket.c,xnew] with bracket.b as best point still
        A.x = xNew;
        A.y = yNew;
    }

    gl_FrontColor = vec4(0.0,0.0,0.0,1.0);
    // write bracketA
    gl_FrontColor.r = A.x;
    gl_FrontColor.g = A.y;
    gl_FrontColor.b = yNew;
    gl_FrontColor.a = entropyVal.r;
    if ( gl_Vertex.z == 0.5 ) // write bracketB
    {
        gl_FrontColor.r = B.x;
        gl_FrontColor.g = B.y;
        gl_FrontColor.b = entropyVal.g;
        gl_FrontColor.a = entropyVal.b;
    }
    else if ( gl_Vertex.z == 1.0 ) // write bracketC
    {
        gl_FrontColor.r = C.x;
        gl_FrontColor.g = C.y;
        gl_FrontColor.b = entropyVal.a;
        gl_FrontColor.a = 1.0;
    }
    gl_Position = gl_Vertex;
    gl_Position.z = 0;
}
//----------------------------------------------------------------------------
