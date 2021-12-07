
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.io.IOException;
import java.io.Serializable;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.Shader;
import WildMagic.LibGraphics.Shaders.VertexShader;

/**
 * This class sets up and communicates with the GLSL shader program used to render the
 * interior of the widget in the multi-histogram panel. The parameters that control how the widget is rendered
 * in the multi-histogram panel are also used to determine how the widget is applied to the volume rendered data
 * in the GLSL volume renderer shader program. Those parameters are encapsulated in the ClassificationWidgetState class
 * which is used to pass the information on to the volume render GLSL program.
 * 
 * This class sets up the histogram tool GLSL shader programs for rendering the widgets directly.
 */
public class ClassificationWidgetEffect extends TextureEffect implements Serializable
{
	private static final long serialVersionUID = 3379141203039832123L;
	/** Current state of the widget encapsulated in the GLSL parameters needed for the Volume renderer GLSL program: */
	private ClassificationWidgetState m_kWidgetState = new ClassificationWidgetState();
	/** Texture used in the histogram. */
	private Texture m_kTexture;
	/** color look-up table name. */
	private String m_kLUTName;
	/** color look-up table Texture: */
	private Texture m_kLUTMap;

	/** First part of the GLSL code for the pixel shader program for all widgets: */
	private String mainPixelShader1 = "" + 
			"uniform vec4 LevColor;" + "\n" +
			"uniform vec2 Shift;" + "\n" +
			"uniform vec4 LevMidLine;" + "\n" +
			"uniform vec4 LevLeftLine;" + "\n" +
			"uniform vec4 LevRightLine;" + "\n" +
			"uniform vec4 Center;" + "\n" +
			"uniform vec4 Radius;" + "\n" +
			"uniform sampler2D BaseSampler;" + "\n" +
			"uniform sampler1D ColorMap;" + "\n" +
			"in vec2 varTexCoord;" + "\n" +
			"out vec4 fragColor;" + "\n" +
			"void main()" + "\n" +
			"{" + "\n" +
			"    vec4 kBase = texture(BaseSampler,varTexCoord, 0.0);" + "\n";

	/** GLSL function call for the triangle widget: */
	private String mainPixelShaderTriangle = "" + 
			"    float fAlpha = computeAlphaTriangle( varTexCoord.x, varTexCoord.y, Shift," + "\n" +
			"                                 LevMidLine, LevLeftLine, LevRightLine );" + "\n" +
			"    vec4 widgetColor = LevColor;" + "\n";

	/** GLSL function call for the square widget: */
	private String mainPixelShaderSquare = "" + 
			"    float fAlpha = computeAlphaSquare( varTexCoord.x, varTexCoord.y, Shift," + "\n" +
			"                                 LevMidLine, LevLeftLine, LevRightLine );" + "\n" +
			"    vec4 widgetColor = LevColor;" + "\n";

	/** GLSL function call for the circle widget: */
	private String mainPixelShaderCircle = "" + 
			"    float fAlpha = computeAlphaCircle( varTexCoord.x, varTexCoord.y," + "\n" +
			"                                 Center, LevMidLine, Radius );" + "\n" +
			"    vec4 widgetColor = LevColor;" + "\n";

	/** GLSL main code part 2 for all widget types. */
	private String mainPixelShader2 = "" + 
			"    fAlpha *= widgetColor.a;" + "\n" +
			"    fragColor.r = widgetColor.r*fAlpha + (1.0 - fAlpha)*kBase.r;" + "\n" +
			"    fragColor.g = widgetColor.g*fAlpha + (1.0 - fAlpha)*kBase.g;" + "\n" +
			"    fragColor.b = widgetColor.b*fAlpha + (1.0 - fAlpha)*kBase.b;" + "\n" +
			"    fragColor.a = 1.0;" + "\n" +
			"}" + "\n";

	/** GLSL computeAlpha function definition for the triangle widgets: */
	private static String computeAlphaTriangle = "" +
			"float areaTwice(float ptAx, float ptAy, float ptBx, float ptBy, float ptCx, float ptCy) {" + "\n" +
			"return (((ptAx - ptCx) * (ptBy - ptCy)) - ((ptAy - ptCy) * (ptBx - ptCx)));" + "\n" +
			"}" + "\n" +
			"float computeAlphaTriangle( float fX," + "\n" +
			"                float fY," + "\n" +
			"                vec2  fShift," + "\n" +
			"                vec4  LevMidLine," + "\n" +
			"                vec4  LevLeftLine," + "\n" +
			"                vec4  LevRightLine )" + "\n" +
			"{" + "\n" +
			// test that the point falls within the data bounds specified by the triangle:
			"float area1 = areaTwice( LevRightLine.x, LevRightLine.y, LevRightLine.z, LevRightLine.w, fX, fY);" + "\n" +
			"float area2 = areaTwice( LevRightLine.z, LevRightLine.w, LevLeftLine.z, LevLeftLine.w, fX, fY);" + "\n" +
			"float area3 = areaTwice( LevLeftLine.z, LevLeftLine.w, LevLeftLine.x, LevLeftLine.y, fX, fY);" + "\n" +
			"int inside = 0;" + "\n" +
			"if ( (area1 >= 0) && (area2 >= 0) && (area3 >= 0) ) { inside = 1; }" + "\n" +
			"if ( (area1 <= 0) && (area2 <= 0) && (area3 <= 0) ) { inside = 1; }" + "\n" +
			"if ( inside == 0 ) { return 0; }" + "\n" +
			"" + "\n" +
			"" + "\n" +
			"float fShiftL = fShift.x;" + "\n" +
			"float fShiftR = fShift.y;" + "\n" +
			// Calculate where the point intersects the mid-line, using the opposite edge as the direction vector:
			"vec3 perpendicDir = vec3( LevLeftLine.z - LevRightLine.z, LevLeftLine.w - LevRightLine.w, 0 );" + "\n" +
			"vec3 midStart = vec3( LevMidLine.x, LevMidLine.y, 0);" + "\n" +
			"vec3 midDir = vec3( LevMidLine.z - LevMidLine.x, LevMidLine.w - LevMidLine.y, 0);" + "\n" +
			"vec3 currentPt = vec3( fX, fY, 0 );" + "\n" +
			"vec3 closestPt = computeIntersect( currentPt, perpendicDir, midStart, midDir);" + "\n" +

			// Compute the direction vector from the current point to the mid-line intersection point:
			"perpendicDir = vec3( fX - closestPt.x, fY - closestPt.y, 0 );" + "\n" +
			"vec3 leftStart = vec3( LevLeftLine.x, LevLeftLine.y, 0);" + "\n" +
			"vec3 leftDir = vec3( LevLeftLine.z - LevLeftLine.x, LevLeftLine.w - LevLeftLine.y, 0);" + "\n" +
			// compute intersection with left edge:
			"vec3 leftPt = computeIntersect( closestPt, perpendicDir, leftStart, leftDir);" + "\n" +
			"vec3 rightStart = vec3( LevRightLine.x, LevRightLine.y, 0);" + "\n" +
			"vec3 rightDir = vec3( LevRightLine.z - LevRightLine.x, LevRightLine.w - LevRightLine.y, 0);" + "\n" +
			// compute intersection with right edge:
			"vec3 rightPt = computeIntersect( closestPt, perpendicDir, rightStart, rightDir);" + "\n" +
			// compute distances to the edges from the mid-line intersection point and the current point:
			"float distMidR = (closestPt.x - rightPt.x) * (closestPt.x - rightPt.x) +  (closestPt.y - rightPt.y) * (closestPt.y - rightPt.y);" + "\n" +
			"float distMidL = (closestPt.x - leftPt.x)  * (closestPt.x - leftPt.x)  +  (closestPt.y - leftPt.y)  * (closestPt.y - leftPt.y);" + "\n" +
			"float distMidPt = (closestPt.x - fX)  * (closestPt.x - fX)  +  (closestPt.y - fY)  * (closestPt.y - fY);" + "\n" +
			"float distPtR =  (fX - rightPt.x) * (fX - rightPt.x) +  (fY - rightPt.y) * (fY - rightPt.y);" + "\n" +
			"float distPtL = (fX - leftPt.x)  * (fX - leftPt.x)  +  (fY - leftPt.y)  * (fY - leftPt.y);" + "\n" +
			"" + "\n" +
			// compute alpha:
			"float length = fShift.x;" + "\n" +
			"float fAlpha = 0.0;" + "\n" +
			"if ( (fX > closestPt.x) && (fX < rightPt.x) )" + "\n" +
			"{" + "\n" +
			"   fAlpha = length + (distPtR) / (distMidR);" + "\n" +
			"}" + "\n" +
			"else if ( (fX > closestPt.x) && (fX < leftPt.x) )" + "\n" +
			"{" + "\n" +
			"   fAlpha = length + (distPtL) / (distMidL);" + "\n" +
			"}" + "\n" +
			"else if ( (fX < closestPt.x) && (fX > leftPt.x) )" + "\n" +
			"{" + "\n" +
			"   fAlpha = length + (distPtL) / (distMidL);" + "\n" +
			"}" + "\n" +
			"else if ( (fX < closestPt.x) && (fX > rightPt.x) )" + "\n" +
			"{" + "\n" +
			"   fAlpha = length + (distPtR) / (distMidR);" + "\n" +
			"}" + "\n" +
			"return min( 1, fAlpha );" + "\n" +
			"}" + "\n";

	/** GLSL computeAlpha function definition for the square widgets: */
	private static String computeAlphaSquare = "" +
			"float computeAlphaSquare( float fX," + "\n" +
			"                float fY," + "\n" +
			"                vec2  fShift," + "\n" +
			"                vec4  LevMidLine," + "\n" +
			"                vec4  LevLeftLine," + "\n" +
			"                vec4  LevRightLine )" + "\n" +
			"{" + "\n" +
			"int inside = 1;" + "\n" +
			"if ( (fX < LevLeftLine.x) || (fX > LevRightLine.x) || (fY < LevRightLine.y) || (fY > LevRightLine.w) ) { inside = 0; }" + "\n" +
			"if ( inside == 0 ) { return 0; }" + "\n" +
			"float fShiftL = fShift.x;" + "\n" +
			"float fShiftR = fShift.y;" + "\n" +
			"vec3 closestPt = computeClosestPoint( fX, fY, LevMidLine );" + "\n" +
			"vec3 perpendicDir = vec3( fX - closestPt.x, fY - closestPt.y, 0 );" + "\n" +
			"vec3 leftStart = vec3( LevLeftLine.x, LevLeftLine.y, 0);" + "\n" +
			"vec3 leftDir = vec3( LevLeftLine.z - LevLeftLine.x, LevLeftLine.w - LevLeftLine.y, 0);" + "\n" +
			"vec3 leftPt = computeIntersect( closestPt, perpendicDir, leftStart, leftDir);" + "\n" +
			"vec3 rightStart = vec3( LevRightLine.x, LevRightLine.y, 0);" + "\n" +
			"vec3 rightDir = vec3( LevRightLine.z - LevRightLine.x, LevRightLine.w - LevRightLine.y, 0);" + "\n" +
			"vec3 rightPt = computeIntersect( closestPt, perpendicDir, rightStart, rightDir);" + "\n" +
			"" + "\n" +
			"float fAlpha = 0.0;" + "\n" +
			"if ( (fX > (closestPt.x - fShiftL)) && (fX < (closestPt.x + fShiftR)) )" + "\n" +
			"{" + "\n" +
			"    fAlpha = 1.0;" + "\n" +
			"}" + "\n" +
			"if ( (fX <= (closestPt.x-fShiftL)) && (fX >= leftPt.x) )" + "\n" +
			"{" + "\n" +
			"    fAlpha = (fX - leftPt.x) / ((closestPt.x-fShiftL) - leftPt.x);" + "\n" +
			"}" + "\n" +
			"if ( (fX >= (closestPt.x+fShiftR)) && (fX <= rightPt.x) )" + "\n" +
			"{" + "\n" +
			"    fAlpha = (fX - rightPt.x) / ((closestPt.x+fShiftR) - rightPt.x);" + "\n" +
			"}" + "\n" +
			"return fAlpha;" + "\n" +
			"}" + "\n";
	
	/** GLSL support function definition for the triangle widgets: */
	private static String computeClosestPoint = "" +
			"vec3 computeClosestPoint( float fX, float fY," + "\n" +
			"vec4 LevLine )" + "\n" +
			"{" + "\n" +
			"   vec3 dir = vec3( LevLine.z - LevLine.x, LevLine.w - LevLine.y, 0);" + "\n" +
			"   dir = normalize(dir);" + "\n" +
			"   vec3 diff = vec3( fX - LevLine.x, fY - LevLine.y, 0);" + "\n" +
			"   float dot = dot(dir, diff);" + "\n" +
			"   vec3 closest = vec3( dir.x * dot + LevLine.x, dir.y * dot + LevLine.y, 0);" + "\n" +
			"   return closest;" + "\n" +
			"}" + "\n";
	
	/** GLSL support function definition for the triangle widgets: */
	private static String intersect = "" +
			"vec3 computeIntersect( vec3 p0, vec3 v0, vec3 p1, vec3 v1 )" + "\n" +
			"{" + "\n" +
			"   float fDet = (v1.x * v0.y - v1.y * v0.x);" + "\n" +
			"   float len0 = v0.x * v0.x + v0.y * v0.y;" + "\n" +
			"   float len1 = v1.x * v1.x + v1.y * v1.y;" + "\n" +
			"   if ( (fDet * fDet) < (0.00000012 * len0 * len1)) {" + "\n" +
			"       return p0;" + "\n" +
			"    }" + "\n" +
			"   float fInvDet = 1.0 / fDet;" + "\n" +
			"   vec3 diff = vec3( p1.x - p0.x, p1.y - p0.y, 0);" + "\n" +
			"   float s = (v1.x * diff.y - v1.y * diff.x) * fInvDet;" + "\n" +
			"   vec3 intersectPoint = vec3( v0.x * s + p0.x, v0.y * s + p0.y, 0);" + "\n" +
			"   return intersectPoint;" + "\n" +
			"}" + "\n";


	/** GLSL computeAlpha function definition for the circle widgets: */
	private static String computeAlphaCircle = ""+ 
			"float computeAlphaCircle( float fX," + "\n"
			+ "                    float fY," + "\n"
			+ "                    vec4  Center," + "\n"
			+ "                    vec4  MidLine," + "\n"
			+ "                    vec4  Radius )" + "\n"
			+ "{" + "\n"
			+ "    vec2 p0, p1;" + "\n"
			+ "    p0.x = MidLine.x - Center.x;" + "\n"
			+ "    p0.y = MidLine.y - Center.y;" + "\n"
			+ "    p1.x = fX - Center.x;" + "\n"
			+ "    p1.y = fY - Center.y;" + "\n"
			+ "    float b = Radius.y;" + "\n"
			+ "    float a = Radius.x;" + "\n"
			+ "    float slope = (p1.y - p0.y) / (p1.x - p0.x);" + "\n"
			+ "    float intercept = p1.y - slope * p1.x;" + "\n"
			+ "    float A = b*b + a*a*slope*slope;" + "\n"
			+ "    float B = 2*a*a*intercept*slope;" + "\n"
			+ "    float C = a*a*intercept*intercept - b*b*a*a;" + "\n"
			+ "    float r = B*B - 4*A*C;" + "\n"
			+ "    vec2 intersect0;" + "\n"
			+ "    vec2 intersect1;" + "\n"
			+ "    if ( r >= 0 )" + "\n"
			+ "    {" + "\n"
			+ "        // solve for x values - using the quadratic equation" + "\n"
			+ "        float x3 = (-B-sqrt(r))/(2*A);" + "\n"
			+ "        float x4 = (-B+sqrt(r))/(2*A);" + "\n"
			+ "        // calculate y, since we know it's on the line at that point (otherwise there would be no intersection)" + "\n"
			+ "        float y3 = slope*x3+intercept;" + "\n"
			+ "        float y4 = slope*x4+intercept;				" + "\n"
			+ "        intersect0.x = Center.x + x3;" + "\n"
			+ "        intersect0.y = Center.y + y3;" + "\n"
			+ "        intersect1.x = Center.x + x4;" + "\n"
			+ "        intersect1.y = Center.y + y4;" + "\n"
			+ "        vec2 shade;" + "\n"
			+ "        shade.x = fX - MidLine.x;" + "\n"
			+ "        shade.y = fY - MidLine.y;" + "\n"
			+ "        vec2 edge;" + "\n"
			+ "        edge.x = intersect0.x - MidLine.x;" + "\n"
			+ "        edge.y = intersect0.y - MidLine.y;" + "\n"
			+ "        if ( dot( edge, shade ) <= 0 )" + "\n"
			+ "        {" + "\n"
			+ "            intersect0 = intersect1;" + "\n"
			+ "        }" + "\n"
			+ "    }" + "\n"
			+ "    else" + "\n"
			+ "    {" + "\n"
			+ "        float x3 = (-B-sqrt(r))/(2*A);	" + "\n"
			+ "        float y3 = slope*x3+intercept;" + "\n"  
			+ "        intersect0.x = Center.x + x3;" + "\n"
			+ "        intersect0.y = Center.y + y3;" + "\n"
			+ "    }" + "\n"
			+ "    vec2 direction;" + "\n"
			+ "    direction.x = fX - MidLine.x;" + "\n"
			+ "    direction.y = fY - MidLine.y; " + "\n"
			+ "    float lengthShade = sqrt(direction.x*direction.x + direction.y*direction.y);" + "\n"
			+ "    float diffX = intersect0.x - MidLine.x;" + "\n"
			+ "    float diffY = intersect0.y - MidLine.y;" + "\n"
			+ "    float length =  sqrt(diffX * diffX + diffY * diffY );" + "\n"
			+ "    float fAlpha = max( 0.0, 1.0 - (lengthShade / length) );" + "\n"
			+ "    return fAlpha;" + "\n"
			+ "}" + "\n";

	/** combined GLSL code for the histogram widgets: */
	private static String multiHistogramFunctions = computeClosestPoint + intersect + computeAlphaTriangle + computeAlphaSquare + computeAlphaCircle;

	/**
	 * Returns the combined GLSL code and support functions for the histogram widgets. 
	 * This code is also used in the GLSL programs for the volume renderer.
	 * @return
	 */
	public static String getMultiHistogramFunctions()
	{
		return new String(multiHistogramFunctions);
	}
	
	/** Current GLSL code for the widget program. */
	private String m_kCurrentText;
	

	/** Creates a new ClassificationWidgetEffect with the texture specified.
	 * @param kTexture input histogram Texture.
	 * @param type Classification widget type.
	 */
	public ClassificationWidgetEffect (Texture kTexture, int type)
	{
		SetPassQuantity(1);
		m_kVShader.set(0, new VertexShader("TextureV", Shader.vertexShaderTexture2  ));
		m_kPShader.set(0, new PixelShader("ClassificationWidgetEffect", createProgramText()));

		m_kWidgetState.UseWidget[0] = 1.0f;

		m_kWidgetState.UseColorMap[0] = -1.0f;

		m_kTexture = kTexture;
		m_kWidgetState.Type = type;
	}

	/* (non-Javadoc)
	 * @see WildMagic.LibGraphics.Effects.ShaderEffect#dispose()
	 */
	public void dispose()
	{
		m_kWidgetState.dispose();
		m_kWidgetState = null;
		super.dispose();
	}


	/**
	 * Returns the color of the widget color transfer function.
	 * @return the color of the widget color transfer function.
	 */
	public ColorRGBA GetColor( ) 
	{
		return new ColorRGBA (
				m_kWidgetState.Color[0],
				m_kWidgetState.Color[1],
				m_kWidgetState.Color[2],
				m_kWidgetState.Color[3] );
	}

	/**
	 * Returns the index of the color look-up table color map.
	 * @return the index of the color look-up table color map.
	 */
	public int GetLUTIndex( )
	{
		return (int)m_kWidgetState.UseColorMap[0];
	}

	/**
	 * Returns the current ClassificationWidgetState GLSL Shader program parameters.
	 * @return the current ClassificationWidgetState GLSL Shader program parameters.
	 */
	public ClassificationWidgetState getState()
	{
		return m_kWidgetState;
	}


	/**
	 * Read this object from disk.
	 * @param in
	 * @throws IOException
	 * @throws ClassNotFoundException
	 */
	public void readObject(java.io.ObjectInputStream in)
	throws IOException, ClassNotFoundException
	{
		String rkBaseName = (String)in.readObject();
		SetPassQuantity(1);
		m_kVShader.set(0, new VertexShader("TextureV", Shader.vertexShaderTexture2 ));
		m_kPShader.set(0, new PixelShader("ClassificationWidgetEffect", createProgramText()));
		m_kWidgetState = (ClassificationWidgetState)in.readObject();
	}
	
	/**
	 * Sets the alpha value of the widget.
	 * @param fAlpha
	 */
	public void SetAlpha( float fA ) 
	{
		m_kWidgetState.Color[3] = fA;
		if ( fA == 0 )
		{
			m_kWidgetState.UseWidget[0] = 0;
		}
		else
		{
			m_kWidgetState.UseWidget[0] = 1;
		}
		UpdateColor();
	}

	/**
	 * Sets the contribution of the 2nd derivative to the volume rendering.
	 * @param fAlpha the contribution of the 2nd derivative to the volume rendering.
	 */
	public void setBoundary( float fAlpha )
	{
		m_kWidgetState.BoundaryEmphasis[0] = fAlpha;
	}


	/**
	 * Sets the Center of the widget for the GLSL program.
	 * @param fX
	 * @param fY
	 */
	public void SetCenter( float fX, float fY ) 
	{
		m_kWidgetState.Center[0] = fX;
		m_kWidgetState.Center[1] = fY;
		Program pkCProgram = GetCProgram(0);
		if ( (pkCProgram != null) && (pkCProgram.GetUC("Center") != null) ) 
		{
			pkCProgram.GetUC("Center").SetDataSource(m_kWidgetState.Center); 
			//System.err.println( "Center " + fX + " " + fY );
		}
	}

	/**
	 * Sets the color of the widget color transfer function.
	 * @param the color of the widget color transfer function.
	 */
	public void SetColor( float fR, float fG, float fB, float fA ) 
	{
		m_kWidgetState.Color[0] = fR;
		m_kWidgetState.Color[1] = fG;
		m_kWidgetState.Color[2] = fB;
		m_kWidgetState.Color[3] = fA;
		if ( fA == 0 )
		{
			m_kWidgetState.UseWidget[0] = 0;
		}
		else
		{
			m_kWidgetState.UseWidget[0] = 1;
		}
		m_kWidgetState.UseColorMap[0] = -1.0f;
		UpdateColor();
	}

	/**
	 * Sets the left-line parameter to the GLSL Shader Program.
	 * @param fX1 bottom x-coordinate in Texture Coordinates.
	 * @param fY1 bottom y-coordinate in Texture Coordinates.
	 * @param fX2 top x-coordinate in Texture Coordinates.
	 * @param fY2 top y-coordinate in Texture Coordinates.
	 */
	public void SetLeftLine( float fX1, float fY1, float fX2, float fY2 ) 
	{
		m_kWidgetState.LeftLine[0] = fX1;
		m_kWidgetState.LeftLine[1] = fY1;
		m_kWidgetState.LeftLine[2] = fX2;
		m_kWidgetState.LeftLine[3] = fY2;
		Program pkCProgram = GetCProgram(0);
		if ( (pkCProgram != null) && (pkCProgram.GetUC("LevLeftLine") != null) ) 
		{
			pkCProgram.GetUC("LevLeftLine").SetDataSource(m_kWidgetState.LeftLine);
			//System.err.println( "EFFECT : LevLeftLine " + m_kWidgetState.LeftLine[0] +
			//		" "  + m_kWidgetState.LeftLine[1] +
			//		" "  + m_kWidgetState.LeftLine[2] +
			//		" "  + m_kWidgetState.LeftLine[3] );
		}
		checkProgramText();
	}

	public void SetLUT( Texture kMap, int index, boolean bReverse )
	{
		m_kLUTName = kMap.GetName();
		m_kLUTMap = kMap;

		m_kPShader.get(0).SetImageName(1,m_kLUTName, "ColorMap");
		m_kPShader.get(0).SetTexture(1,m_kLUTMap, "ColorMap");

		m_kWidgetState.UseColorMap[0] = index;
		m_kWidgetState.InvertLUT = bReverse;
		checkProgramText();
	}

	/**
	 * Sets the mid-line parameter to the GLSL Shader Program.
	 * @param fX1 bottom x-coordinate in Texture Coordinates.
	 * @param fY1 bottom y-coordinate in Texture Coordinates.
	 * @param fX2 top x-coordinate in Texture Coordinates.
	 * @param fY2 top y-coordinate in Texture Coordinates.
	 */
	public void SetMidLine( float fX1, float fY1, float fX2, float fY2 ) 
	{
		m_kWidgetState.MidLine[0] = fX1;
		m_kWidgetState.MidLine[1] = fY1;
		m_kWidgetState.MidLine[2] = fX2;
		m_kWidgetState.MidLine[3] = fY2;
		Program pkCProgram = GetCProgram(0);
		if ( (pkCProgram != null) && (pkCProgram.GetUC("LevMidLine") != null) ) 
		{
			pkCProgram.GetUC("LevMidLine").SetDataSource(m_kWidgetState.MidLine); 
			//System.err.println( "EFFECT : LevMidLine " + m_kWidgetState.MidLine[0] +
			//		" "  + m_kWidgetState.MidLine[1] +
			//		" "  + m_kWidgetState.MidLine[2] +
			//		" "  + m_kWidgetState.MidLine[3] );            
		}
		checkProgramText();
	}

	public void SetRadius( float fRX, float fRY ) 
	{
		m_kWidgetState.Radius[0] = fRX;
		m_kWidgetState.Radius[1] = fRY;
		Program pkCProgram = GetCProgram(0);
		if ( (pkCProgram != null) && (pkCProgram.GetUC("Radius") != null) ) 
		{
			pkCProgram.GetUC("Radius").SetDataSource(m_kWidgetState.Radius); 
			//System.err.println( "Radius " + fR );
		}
	}

	/**
	 * Sets the right-line parameter to the GLSL Shader Program.
	 * @param fX1 bottom x-coordinate in Texture Coordinates.
	 * @param fY1 bottom y-coordinate in Texture Coordinates.
	 * @param fX2 top x-coordinate in Texture Coordinates.
	 * @param fY2 top y-coordinate in Texture Coordinates.
	 */
	public void SetRightLine( float fX1, float fY1, float fX2, float fY2 ) 
	{
		m_kWidgetState.RightLine[0] = fX1;
		m_kWidgetState.RightLine[1] = fY1;
		m_kWidgetState.RightLine[2] = fX2;
		m_kWidgetState.RightLine[3] = fY2;
		Program pkCProgram = GetCProgram(0);
		if ( (pkCProgram != null) && (pkCProgram.GetUC("LevRightLine") != null) ) 
		{
			pkCProgram.GetUC("LevRightLine").SetDataSource(m_kWidgetState.RightLine);
			//System.err.println( "EFFECT : LevRightLine " + m_kWidgetState.RightLine[0] +
			//		" "  + m_kWidgetState.RightLine[1] +
			//		" "  + m_kWidgetState.RightLine[2] +
			//		" "  + m_kWidgetState.RightLine[3] );
		}
		checkProgramText();
	}


	public void SetShift( float fX, float fY ) 
	{
		m_kWidgetState.Shift[0] = fX;
		m_kWidgetState.Shift[1] = fY;
		Program pkCProgram = GetCProgram(0);
		if ( (pkCProgram != null) && (pkCProgram.GetUC("Shift") != null) ) 
		{
			pkCProgram.GetUC("Shift").SetDataSource(m_kWidgetState.Shift);
		}
	}

	/**
	 * Sets the ClassificationWidgetState, representing the GLSL shader program parameters.
	 * @param kState ClassificationWidgetState, representing the GLSL shader program parameters.
	 */
	public void setState(ClassificationWidgetState kState)
	{
		m_kWidgetState = kState;
		Program pkCProgram = GetCProgram(0);
		if ( (pkCProgram != null) && (pkCProgram.GetUC("LevColor") != null) ) 
		{
			pkCProgram.GetUC("LevColor").SetDataSource(m_kWidgetState.Color);
		}
		if ( (pkCProgram != null) && (pkCProgram.GetUC("LevMidLine") != null) ) 
		{
			pkCProgram.GetUC("LevMidLine").SetDataSource(m_kWidgetState.MidLine);
		}
		if ( (pkCProgram != null) && (pkCProgram.GetUC("LevLeftLine") != null) ) 
		{
			pkCProgram.GetUC("LevLeftLine").SetDataSource(m_kWidgetState.LeftLine);
		}
		if ( (pkCProgram != null) && (pkCProgram.GetUC("LevRightLine") != null) ) 
		{
			pkCProgram.GetUC("LevRightLine").SetDataSource(m_kWidgetState.RightLine);
		}
		if ( (pkCProgram != null) && (pkCProgram.GetUC("BoundaryEmphasis") != null) ) 
		{
			pkCProgram.GetUC("BoundaryEmphasis").SetDataSource(m_kWidgetState.BoundaryEmphasis);
		}
		computeUniformVariables();
	}

	/**
	 * Updates the color in the GLSL shader program used to render the ClassificationWidget. 
	 */
	public void UpdateColor( ) 
	{
		Program pkCProgram = GetCProgram(0);
		if ( (pkCProgram != null) && (pkCProgram.GetUC("LevColor") != null) ) 
		{
			pkCProgram.GetUC("LevColor").SetDataSource(m_kWidgetState.Color);
		}
		checkProgramText();
	}



	/**
	 * Stream this object to disk.
	 * @param out
	 * @throws IOException
	 */
	public void writeObject(java.io.ObjectOutputStream out)
			throws IOException 
			{
		//out.writeObject( m_kPShader.get(0).GetImageName(0) );
		//out.writeObject(m_kWidgetState);
			}

	/**
	 * Computes the input parameters to the GLSL shader program based on the ClassificationWidgetState and passes
	 * them to the program to render the widget in the multi-histogram panel.
	 */
	protected void computeUniformVariables( )
	{
		checkProgramText();
	}

	private void checkProgramText()
	{    	
		String programText = createProgramText();
		if ( !m_kCurrentText.equals( programText ) )
		{
			//System.err.println( programText );
			m_kCurrentText = new String(programText);
			m_kPShader.get(0).GetProgram().SetProgramText( m_kCurrentText );
			if ( GetCProgram(0) != null )
			{
				GetCProgram(0).Reload(true);
			}
		}
	}
	private String createProgramText()
	{   	

		boolean bUseColorTexture = false;
		String mainPixelShader = "" + mainPixelShader1;
		if ( m_kWidgetState.Type == ClassificationWidgetState.Circle )
		{
			mainPixelShader += mainPixelShaderCircle;
		}
		else if ( m_kWidgetState.Type == ClassificationWidgetState.Square )
		{
			mainPixelShader += mainPixelShaderSquare;
		}
		else if ( m_kWidgetState.Type == ClassificationWidgetState.Triangle )
		{
			mainPixelShader += mainPixelShaderTriangle;
		}

		if ( m_kWidgetState.UseColorMap[0] != -1 )
		{
			mainPixelShader += "" +
					"  widgetColor = texture(ColorMap, fAlpha, 0.0);" + "\n" +
					"  widgetColor.a = LevColor.a;" + "\n";
			bUseColorTexture = true;
		}
		mainPixelShader += mainPixelShader2;

		String programText = new String( multiHistogramFunctions +  mainPixelShader);
		if ( m_kCurrentText == null )
		{
			m_kCurrentText = new String(programText);
		}

		int iTex = 0;    	
		PixelShader pShader = m_kPShader.get(0);
		if ( pShader != null )
		{
			pShader.SetTextureQuantity(2);
			pShader.SetImageName(iTex, m_kTexture.GetName(), "BaseSampler");
			pShader.SetTexture(iTex++, m_kTexture, "BaseSampler");
			if ( bUseColorTexture )
			{		
				pShader.SetImageName(iTex,m_kLUTName, "ColorMap");
				pShader.SetTexture(iTex++,m_kLUTMap, "ColorMap");	
			}
		}
		return programText;
	}

}
