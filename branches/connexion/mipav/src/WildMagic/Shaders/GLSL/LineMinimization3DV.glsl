//----------------------------------------------------------------------------
uniform sampler2D bracketImage;

uniform mat4 toOrigin;
uniform mat4 fromOrigin;
uniform mat4 startPoint;
uniform mat4 pt;
uniform mat4 unit_directions;
//uniform vec3 params;
uniform float ptLength;
uniform float minDist;
uniform float unit_tolerance;

mat4 constructPoint( mat4 defaultPoint, mat4 point )
{

    float rx = defaultPoint[0][0];
    float ry = defaultPoint[1][0];
    float rz = defaultPoint[2][0];
    float tx = defaultPoint[3][0];
    float ty = defaultPoint[0][1];
    float tz = defaultPoint[1][1];
    float sx = defaultPoint[2][1];
    float sy = defaultPoint[3][1];
    float sz = defaultPoint[0][2];
    float skX = defaultPoint[1][2];
    float skY = defaultPoint[2][2];
    float skZ = defaultPoint[3][2];

    //float ptLength = params.x;

    if (ptLength == 3.0) {
        tx = point[0][0];
        ty = point[1][0];
        tz = point[2][0];
    }
    else if (ptLength == 4.0) {
        tx = point[1][0];
        ty = point[2][0];
        tz = point[3][0];
        sx = point[0][0];
        sy = point[0][0];
        sz = point[0][0];
    }
    else if ((ptLength == 6.0) || (ptLength == 7.0) 
             || (ptLength == 9.0) || (ptLength == 12.0)) {
        rx = point[0][0];
        ry = point[1][0];
        rz = point[2][0];
        tx = point[3][0];
        ty = point[0][1];
        tz = point[1][1];

        if (ptLength == 7.0)
        {
            sx = point[2][1];
            sy = point[2][1];
            sz = point[2][1];
        } else if (ptLength > 7.0) {
            sx = point[2][1];
            sy = point[3][1];
            sz = point[0][2];

            if (ptLength > 9.0) {
                skX = point[1][2];
                skY = point[2][2];
                skZ = point[3][2];
            }
        }
    }

    float angleX = radians(rx);
    float angleY = radians(ry);
    float angleZ = radians(rz);
    float cosrX = cos(angleX);
    float sinrX = sin(angleX);
    float cosrY = cos(angleY);
    float sinrY = sin(angleY);
    float cosrZ = cos(angleZ);
    float sinrZ = sin(angleZ);

    float M00 = (cosrZ * cosrY * sx);
    float M01 = (((cosrZ * cosrY * skX) - (sinrZ * cosrY)) * sy);
    float M02 = (((cosrZ * cosrY * skY) - (sinrZ * cosrY * skZ) + sinrY) * sz);
    float M03 = (tx);
    float M10 = (((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * sx);
    float M11 = (((((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * skX) - (sinrZ * sinrY * sinrX) +
                  (cosrZ * cosrX)) * sy);
    float M12 = (((((cosrZ * sinrY * sinrX) + (sinrZ * cosrX)) * skY) +
                  (((-sinrZ * sinrY * sinrX) + (cosrZ * cosrX)) * skZ) - (cosrY * sinrX)) * sz);
    float M13 = (ty);
    float M20 = (((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * sx);
    float M21 = (((((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * skX) + (sinrZ * sinrY * cosrX) +
                  (cosrZ * sinrX)) * sy);
    float M22 = (((((-cosrZ * sinrY * cosrX) + (sinrZ * sinrX)) * skY) +
                  (((sinrZ * sinrY * cosrX) + (cosrZ * sinrX)) * skZ) + (cosrY * cosrX)) * sz);
    float M23 = (tz);
    
//      mat4 matrix = mat4( tx, ty, tz, rx,
//                          ry, rz, sx, sy,
//                          sz, skX, skY, skZ,
//                          0.0, 0.0, 0.0, 1.0 );


     mat4 matrix = mat4( M00, M01, M02, M03,
                         M10, M11, M12, M13,
                         M20, M21, M22, M23,
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


void v_LineMinimization3DV()
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

// mat4 oneDimension( float x )
// {
//     mat4 xt = pt + x * unit_directions;
//     mat4 matrix = constructPoint( startPoint, xt );
//     return fromOrigin * matrix * toOrigin;
//     //return matrix;
// }

// float estimateMinimum(vec2 A, vec2 B, vec2 C )
// {
//     float ad = 0.0;
//     float bd = 0.0;
//     float det = 0.0;
//     float xNew;

//     ad = ((B.x - C.x) * (B.y - A.y)) - ((B.x - A.x) * (B.y - C.y));
//     bd = (-((B.x * B.x) - (C.x * C.x)) * (B.y - A.y)) +
//         (((B.x * B.x) - (A.x * A.x)) * (B.y - C.y));
//     det = (B.x - C.x) * (C.x - A.x) * (A.x - B.x);
    
//     /** Used to prevent division by zero. */
//     float TINY = 1.0 * pow(10.0, -20.0);
//     if ((abs(det) > TINY) && ((ad / det) < 0.0)) { // quadratic only has a maxima
//         xNew = -999;
//     }
    
//     if (abs(ad) > TINY) {
//         xNew = -bd / (2.0 * ad);
//     }

//     else
//     {
//         xNew = -999;
//     }
//     return xNew;
// }

// float extrapolatePoint( vec2 A, vec2 B, vec2 C )
// {
//     // bracket.b must be between bracket.a and bracket.c
//     // use the golden ratio (scale similar result)
//     /** Golden ratio is .38197 and .61803. */
//     float RGOLD = 0.618034;
//     /** Golden ratio - second part. */
//     float CGOLD = 0.3819660;

//     float xNew;

//     if (abs(C.x - B.x) > abs(A.x - B.x)) {
//         xNew = (CGOLD * C.x) + (RGOLD * B.x);
//     } else {
//         xNew = (CGOLD * A.x) + (RGOLD * B.x);
//     }
    
//     return xNew;
// }


// float nextPoint( vec2 A, vec2 B, vec2 C )
// {
//     float xNew = estimateMinimum( A, B, C );
//     float min = (A.x < C.x) ? A.x : C.x;
//     float max = (C.x < A.x) ? A.x : C.x;
//     if ( (xNew==-999) || (xNew < min) || (xNew > max) )
//     {
//         xNew = extrapolatePoint( A, B, C );
//     }
//     return xNew;
// }


// void v_LineMinimization3DV()
// {
//     vec2 bracketTCoord = vec2(0,0);
//     vec2 A = texture2D(bracketImage, bracketTCoord ).xy;

//     bracketTCoord.y = 0.5;
//     vec2 B = texture2D(bracketImage, bracketTCoord ).xy;

//     bracketTCoord.y = 1.0;
//     vec2 C = texture2D(bracketImage, bracketTCoord ).xy;
    
//     mat4 InverseTransform = mat4(1.0); 
//     float unit_tolerance = params.z;
//     if ( abs( C.x - A.x ) <= unit_tolerance )
//     {
//         InverseTransform[0] = vec4(0.0);
//         InverseTransform[1] = vec4(0.0);
//         InverseTransform[2] = vec4(0.0);
//         InverseTransform[3] = vec4(0.0);
//     }
//     else
//     {
//         float xNew = nextPoint( A, B, C );
//         float directionN = 1.0;
//         float minDist = params.y;

//         if (C.x < A.x) {
//             directionN = -1.0;
//         }
    
//         if (abs(xNew - A.x) < minDist) {
//             xNew = A.x + (directionN * minDist);
//         }
    
//         if (abs(xNew - C.x) < minDist) {
//             xNew = C.x - (directionN * minDist);
//         }
    
//         if (abs(xNew - B.x) < minDist) {
//             xNew = extrapolatePoint(A, B, C);
//         }
    
//         if (abs(B.x - A.x) < (4.0 * minDist)) {
//             xNew = B.x + (directionN * 5.0 * minDist);
//         }
    
//         if (abs(B.x - C.x) < (4.0 * minDist)) {
//             xNew = B.x - (directionN * 5.0 * minDist);
//         }

//         InverseTransform = oneDimension( xNew );
//     }
    
//     gl_FrontColor = InverseTransform[0];
//     if ( gl_Vertex.z == 0.3 )
//     {
//         gl_FrontColor = InverseTransform[1];
//     }
//     else if ( gl_Vertex.z == 0.7 )
//     {
//         gl_FrontColor = InverseTransform[2];
//     }
//     else if ( gl_Vertex.z == 1.0 )
//     {
//         gl_FrontColor = InverseTransform[3];
//     }
//     gl_Position = gl_Vertex;
//     gl_Position.z = 0;
// }



// //----------------------------------------------------------------------------
