//----------------------------------------------------------------------------
uniform sampler2D bracketImage;

uniform mat4 toOrigin;
uniform mat4 fromOrigin;
uniform float rigid;
uniform mat4 startPoint;
uniform mat4 pt;
uniform float ptLength;
uniform mat4 unit_directions;
uniform float minDist;
uniform float unit_tolerance;

mat4 constructPoint( mat4 defaultPoint, mat4 point )
{
    float r = defaultPoint[0][0];
    float tx = defaultPoint[1][0];
    float ty = defaultPoint[2][0];
    float sx = defaultPoint[3][0];
    float sy = defaultPoint[0][1];
    float skX = defaultPoint[1][1];
    float skY = defaultPoint[2][1];


    if (ptLength == 2.0)
    {
        tx = point[0][0];
        ty = point[1][0];
    }
    else if ( ptLength == 3.0 )
    {
        if (rigid == 1.0)
        {
            r = point[0][0];
            tx = point[1][0];
            ty = point[2][0];
        }
        else
        {
            sx = point[0][0];
            sy = point[0][0];
            tx = point[1][0];
            ty = point[2][0];
        }
    }
    else if (ptLength == 4.0)
    {
        r = point[0][0];
        tx = point[1][0];
        ty = point[2][0];
        sx = point[3][0];
        sy = point[3][0];
    }
    else if (ptLength > 4.0)
    {
        r = point[0][0];
        tx = point[1][0];
        ty = point[2][0];
        sx = point[3][0];
        sy = point[0][1];
        skX = point[1][1];
        skY = point[2][1];
    }
    // Translate * Rotate:
    float angle = radians(r);
    float sinR = sin(angle);
    float cosR = cos(angle);

    float M00 = cosR;
    float M01 = -sinR;
    float M02 = tx;
    float M10 = sinR;
    float M11 = cosR;
    float M12 = ty;
    float M20 = 0.0;
    float M21 = 0.0;
    float M22 = 1.0;

    // Skew:
    float tmpM00 = M00;
    float tmpM01 = M01;
    float tmpM10 = M10;
    float tmpM11 = M11;
    M00 = tmpM00 + (skY * tmpM01);
    M10 = tmpM10 + (skY * tmpM11);
    M01 = (skX * tmpM00) + tmpM01;
    M11 = (skX * tmpM10) + tmpM11;

    // Zoom: 
    M00 = sx * M00;
    M10 = sx * M10;
    
    M01 = sy * M01;
    M11 = sy * M11;

//      mat4 matrix = mat4( r, tx, ty, 0.0,
//                          sx, sy, skY, skY,
//                          0.0, 0.0, 1.0, 0.0,
//                          0.0, 0.0, 0.0, 1.0 );
     mat4 matrix = mat4( M00, M01, 0.0, M02,
                         M10, M11, 0.0, M12,
                         0.0, 0.0, 1.0, 0.0,
                         0.0, 0.0, 0.0, 1.0 );
    return matrix;
}

mat4 oneDimension( float x )
{
    mat4 xt = pt + x * unit_directions;
    mat4 matrix = constructPoint( startPoint, xt );
    return fromOrigin * matrix * toOrigin;
    //return matrix;
}

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
        xNew = -999;
    }
    
    if (abs(ad) > TINY) {
        xNew = -bd / (2.0 * ad);
    }

    else
    {
        xNew = -999;
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
    if ( (xNew==-999) || (xNew < min) || (xNew > max) )
    {
        xNew = extrapolatePoint( A, B, C );
    }
    return xNew;
}


void v_LineMinimizationV()
{
    vec2 bracketTCoord = vec2(0,0);
    vec3 A1 = texture2D(bracketImage, bracketTCoord ).xyz;
    vec2 A = A1.xy;

    bracketTCoord.y = 0.5;
    vec2 B = texture2D(bracketImage, bracketTCoord ).xy;

    bracketTCoord.y = 1.0;
    vec2 C = texture2D(bracketImage, bracketTCoord ).xy;
    
    mat4 InverseTransform = mat4(1.0); 
    if ( abs( C.x - A.x ) <= unit_tolerance )
    {
        InverseTransform[0] = vec4(0.0);
        InverseTransform[1] = vec4(0.0);
        InverseTransform[2] = vec4(0.0);
        InverseTransform[3] = vec4(0.0);
    }
    else
    {
        float xNew;
//         if ( A1.z == -1 )
//         {
//             xNew = extrapolatePoint( A, B, C );
//         }
//         else
//         {
            xNew = nextPoint( A, B, C );
//         }
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

        InverseTransform = oneDimension( xNew );
    }
    
    gl_FrontColor = InverseTransform[0];
    if ( gl_Vertex.z == 0.3 )
    {
        gl_FrontColor = InverseTransform[1];
    }
    else if ( gl_Vertex.z == 0.7 )
    {
        gl_FrontColor = InverseTransform[2];
    }
    else if ( gl_Vertex.z == 1.0 )
    {
        gl_FrontColor = InverseTransform[3];
    }
    gl_Position = gl_Vertex;
    gl_Position.z = 0;
}
//----------------------------------------------------------------------------
