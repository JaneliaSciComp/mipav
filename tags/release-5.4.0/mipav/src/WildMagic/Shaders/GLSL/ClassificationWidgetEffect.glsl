//----------------------------------------------------------------------------
float computeX( float fY,
                float fInvY0MY1,
                vec4 LevLine )
{
    float x0 = LevLine.x;
    float y0 = LevLine.y;
    float x1 = LevLine.z;
    float y1 = LevLine.w;
    float t = (y0 - fY) * fInvY0MY1;
    float x = x0 - x0 * t + x1 * t;
    return x;
}
//----------------------------------------------------------------------------
float computeAlpha( float fX,
                    float fY,
                    vec2  fShift,
                    vec3  fInvY0MY1,
                    vec4  LevMidLine,
                    vec4  LevLeftLine,
                    vec4  LevRightLine )
{
    if ( (fY < LevLeftLine.y) || fY > LevLeftLine.w )
    {
        return 0.0;
    }
    float xMid = LevMidLine.x;
    float fShiftL = fShift.x;
    float fShiftR = fShift.y;
    if ( LevMidLine.y != LevMidLine.w )
    {
        xMid = computeX( fY, fInvY0MY1.y, LevMidLine );
    }
    float xLeft = computeX( fY, fInvY0MY1.x, LevLeftLine );
    float xRight = computeX( fY, fInvY0MY1.z, LevRightLine );
    
    float fAlpha = 0.0;
    if ( (fX > (xMid - fShiftL)) && (fX < (xMid + fShiftR)) )
    {
        fAlpha = 1.0;
    }
    if ( (fX <= (xMid-fShiftL)) && (fX >= xLeft) )
    {
        fAlpha = (fX - xLeft) / ((xMid-fShiftL) - xLeft);
    }
    if ( (fX >= (xMid+fShiftR)) && (fX <= xRight) )
    {
        fAlpha = (fX - xRight) / ((xMid+fShiftR) - xRight);
    }
    return fAlpha;
}
//----------------------------------------------------------------------------
uniform vec4 LevColor;
uniform vec2 Shift;
uniform vec3 InvY0MY1;
uniform vec4 LevMidLine;
uniform vec4 LevLeftLine;
uniform vec4 LevRightLine;
uniform sampler2D BaseSampler;
uniform sampler1D ColorMap; 
uniform float UseColorMap;
void main()
{
    vec4 kBase = texture2D(BaseSampler,gl_TexCoord[0].xy);
    float fAlpha = computeAlpha( gl_TexCoord[0].x, gl_TexCoord[0].y, Shift, InvY0MY1,
                                 LevMidLine, LevLeftLine, LevRightLine );
    vec4 widgetColor = LevColor;
    if ( UseColorMap != -1.0 )
    {
        widgetColor = texture1D(ColorMap, fAlpha );
        widgetColor.a = LevColor.a;
    }
    fAlpha *= widgetColor.a;
    gl_FragColor.r = widgetColor.r*fAlpha + (1.0 - fAlpha)*kBase.r;
    gl_FragColor.g = widgetColor.g*fAlpha + (1.0 - fAlpha)*kBase.g;
    gl_FragColor.b = widgetColor.b*fAlpha + (1.0 - fAlpha)*kBase.b;
    gl_FragColor.a = 1.0;
}
//----------------------------------------------------------------------------
