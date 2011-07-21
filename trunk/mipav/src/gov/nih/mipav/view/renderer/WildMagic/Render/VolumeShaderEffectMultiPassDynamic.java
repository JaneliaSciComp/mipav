package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.VertexShader;


public class VolumeShaderEffectMultiPassDynamic extends VolumeShaderEffectMultiPass
    implements StreamInterface
{       
	private static final long serialVersionUID = -4552046274082477860L;

	private static String basicParameters = ""
    	+ "varying vec4 outPos;" + "\n"
    	+ "uniform mat4 WVPMatrix;" + "\n"
    	+ "uniform sampler2D aSceneImage; " + "\n"
    	+ "uniform sampler3D bVolumeImageA; " + "\n"
    	+ "uniform sampler1D cColorMapA;" + "\n"
    	+ "uniform vec4 BackgroundColor;" + "\n"
    	+ "uniform float StepSize;" + "\n"
    	+ "uniform float iPass;" + "\n"
    	+ "" + "\n";

    private static String blendParameters = ""
    	+ "uniform float Blend;" + "\n";
    	
    private static String clipParameters = ""
    	+ "uniform float clipX;" + "\n"
    	+ "uniform float clipXInv;" + "\n"
    	+ "uniform float clipY;" + "\n"
    	+ "uniform float clipYInv;" + "\n"
    	+ "uniform float clipZ;" + "\n"
    	+ "uniform float clipZInv;" + "\n";
    
    private static String clipAEParameters = ""
    	+ "uniform vec4 clipArb;" + "\n"
    	+ "uniform vec4 clipEye;" + "\n"
    	+ "uniform vec4 clipEyeInv;" + "\n"
    	+ "" + "\n";
    
    private static String lightingParametersBasic = ""
    	+ "uniform sampler3D eNormalMapA;" + "\n"
    	+ "uniform vec3 MaterialEmissive;" + "\n"
    	+ "uniform vec3 MaterialAmbient;" + "\n"
    	+ "uniform vec4 MaterialDiffuse;" + "\n"
    	+ "uniform vec4 MaterialSpecular;" + "\n"    	
    	+ "uniform vec3 CameraModelPosition;" + "\n"
    	+ "uniform vec3 CameraWorldPosition;" + "\n";
    	
    private static String lightParameters = ""
    	+ "uniform vec4 Light#Ambient;" + "\n"
    	+ "uniform vec4 Light#Diffuse;" + "\n"
    	+ "uniform vec4 Light#Specular;" + "\n"
    	+ "uniform vec4 Light#SpotCutoff;" + "\n"
    	+ "uniform vec4 Light#Attenuation;" + "\n"
    	+ "uniform vec4 Light#WorldPosition;" + "\n"
    	+ "uniform vec4 Light#WorldDirection;" + "\n";
    
    private static String gradientMagnitudeParameters = ""
    	+ "uniform sampler3D fVolumeImageA_GM;" + "\n"
    	+ "uniform sampler1D gOpacityMapA_GM;" + "\n"
    	+ "" + "\n";
    
    private static String mainSetup = ""
    	+ "void p_VolumeShaderMultiPass() {" + "\n"
    	+ "vec2 texc = ((outPos.xy / outPos.w) + 1.0) * 0.5;" + "\n"
    	+ "vec3 back_position  = texture2D(aSceneImage, texc).xyz;" + "\n"
    	+ "if ( (back_position.x == 0) && (back_position.y == 0) && (back_position.z == 0) ) {" + "\n"
    	+ "   gl_FragColor = BackgroundColor;" + "\n"
    	+ "   return;" + "\n"
    	+ "}" + "\n"
    	+ "vec3 start = gl_TexCoord[0].xyz;" + "\n"
    	+ "vec3 dir = back_position - start;" + "\n"
    	+ "dir = normalize(dir);" + "\n"
    	+ "vec4 color = vec4(0.0);" + "\n"
    	+ "float opacity = 1.0;" + "\n"
    	+ "float fPos = iPass;" + "\n"
    	+ "vec3 position = start + fPos * StepSize * dir;" + "\n"
    	+ "vec3 dir2 = position - start;" + "\n"
    	+ "dir = back_position - start;" + "\n"
    	+ "if ( length(dir2) > length(dir) ) {" + "\n"
    	+ "   gl_FragColor = vec4(0);" + "\n"
    	+ "   return;" + "\n"
    	+ "}" + "\n";
    
    private static String colorParameters = ""
    	+ "uniform vec3 ColorLUTOnA;" + "\n"
    	+ "\n";

    private static String readImage = ""
    	+ "color = texture3D(bVolumeImageA,position);" + "\n";

    private static String readColorMap = ""
    	+ "color = texture1D(cColorMapA,color.r);" + "\n"
    	+ "opacity = color.a;" + "\n";

    private static String readColorMapRGB = ""
    	+ "vec4 colorTemp = vec4(0);" + "\n"
    	+ "opacity = 0;" + "\n"
    	+ "if ( ColorLUTOnA.x != 0.0 ) {" + "\n"
    	+ "   colorTemp = texture1D(cColorMapA,color.r);" + "\n"
    	+ "   color.r = colorTemp.r;" + "\n"
    	+ "   opacity += colorTemp.a;" + "\n"
    	+ "}" + "\n"
    	+ "else {" + "\n"
    	+ "   color.r = 0.0;" + "\n"
    	+ "}" + "\n"
    	+ "if ( ColorLUTOnA.y != 0.0 ) {" + "\n"
    	+ "   colorTemp = texture1D(cColorMapA,color.g);" + "\n"
    	+ "   color.g = colorTemp.g;" + "\n"
    	+ "   opacity += colorTemp.a;" + "\n"
    	+ "}" + "\n"
    	+ "else {" + "\n"
    	+ "   color.g = 0.0;" + "\n"
    	+ "}" + "\n"
    	+ "if ( ColorLUTOnA.z != 0.0 ) {" + "\n"
    	+ "   colorTemp = texture1D(cColorMapA,color.b);" + "\n"
    	+ "   color.b = colorTemp.b;" + "\n"
    	+ "   opacity += colorTemp.a;" + "\n"
    	+ "}" + "\n"
    	+ "else {" + "\n"
    	+ "   color.b = 0.0;" + "\n"
    	+ "}" + "\n";
    
    private static String gradientMagnitudeComposite = ""
    	+ "vec4 colorGM = texture3D(fVolumeImageA_GM,position);" + "\n"
    	+ "float opacityGM = texture1D(gOpacityMapA_GM,colorGM.r).r;" + "\n"
    	+ "opacity = opacity * opacityGM;" + "\n";
    
    private static String colorComposite = ""
    	+ "gl_FragColor.rgb = color.rgb;" + "\n"
    	+ "gl_FragColor.a = opacity;" + "\n";
    
    private static String blendComposite = ""
    	+ "gl_FragColor.a *= Blend;" + "\n";
    
    private static String mainEnd = ""
    	+ "}" + "\n";
    
    private static String clipSetup = ""
    	+ "float bClipped = 0.0;"
    	+ "if ( position.x > clipX ) {" + "\n"
    	+ "   bClipped = 1.0;" + "\n"
    	+ "}" + "\n"
    	+ "else if ( position.x < clipXInv ) {" + "\n"
    	+ "   bClipped = 1.0;" + "\n"
    	+ "}" + "\n"
    	+ " else if ( position.y > clipY ) {" + "\n"
    	+ "   bClipped = 1.0;" + "\n"
    	+ "}" + "\n"
    	+ "else if ( position.y < clipYInv ) {" + "\n"
    	+ "   bClipped = 1.0;" + "\n"
    	+ "}" + "\n"
    	+ "else if ( position.z > clipZ ) {" + "\n"
    	+ "   bClipped = 1.0;" + "\n"
    	+ "}" + "\n"
    	+ " else if ( position.z < clipZInv ) {" + "\n"
    	+ "   bClipped = 1.0;" + "\n"
    	+ "}" + "\n";
    
    private static String clipEnd = ""
    	+ "if ( bClipped == 1.0 ) {" + "\n"
    	+ "   gl_FragColor = vec4(0);" + "\n"
    	+ "   return;" + "\n"
    	+ "}" + "\n";
    
    private static String clipAESetup = ""
    	+ "if ( bClipped != 1.0 ) {" + "\n"
    	+ "   vec4 aPosition = vec4(0.0);" + "\n"
    	+ "   aPosition.xyz = position.xyz - (.5,.5,.5);" + "\n"
    	+ "   aPosition = WVPMatrix*aPosition;" + "\n"
    	+ "   aPosition.xyz = aPosition.xyz + (.5,.5,.5);" + "\n"
    	+ "   float fDot = dot( aPosition.xyz, clipEye.xyz );" + "\n"
    	+ "   float fDotInv = dot( aPosition.xyz, clipEyeInv.xyz );" + "\n"
    	+ "   float fDotArb = dot( position.xyz, clipArb.xyz );" + "\n"
    	+ "   if ( (fDot < clipEye.w) || (fDotInv > clipEyeInv.w) || (fDotArb > clipArb.w) ) {" + "\n"
    	+ "      bClipped = 1.0;" + "\n"
    	+ "   }" + "\n"
    	+ "}" + "\n";

    public static String surfaceInit = ""
    	+ "vec4 LocalMaterialDiffuse = MaterialDiffuse;" + "\n"
    	+ "vec4 LocalMaterialSpecular = MaterialSpecular;" + "\n"
    	+ "vec3 LocalMaterialAmbient = MaterialAmbient;" + "\n"
    	+ "vec3 LocalMaterialEmissive = MaterialEmissive;" + "\n"
    	+ "vec4 colorSum = vec4(0);" + "\n"
    	+ ""
    	+ "vec4 normal = texture3D(eNormalMapA,position);" + "\n"
    	+ "normal.w = 0.0;" + "\n"
    	+ "vec3 local_normal = ((2.0,2.0,2.0)* normal.xyz) - (1.0, 1.0, 1.0);" + "\n"
    	+ "local_normal = normalize( local_normal );" + "\n"
    	+ "" + "\n";
    
    public static String surfaceCompositeInit = ""
    	+ "LocalMaterialDiffuse *= color;" + "\n"
    	+ "LocalMaterialSpecular *= color;" + "\n"
    	+ "LocalMaterialAmbient *= color.xyz;" + "\n"
    	+ "LocalMaterialEmissive *= color.xyz;" + "\n"
    	+ "" + "\n";

    public static String surfaceFinish = ""
    	+ "color = colorSum;" + "\n";
    
    public static String surfaceAmbient = ""
    	+ "colorSum += AmbientLight( LocalMaterialEmissive.xyz," + "\n"
    	+ "                          LocalMaterialAmbient.xyz," + "\n"
    	+ "                          Light#Ambient.xyz," + "\n"
    	+ "                          Light#Attenuation );" + "\n";

    public static String surfacePoint = ""
    	+ "colorSum += PointLight( position.xyz," + "\n"
    	+ "                        local_normal.xyz," + "\n"
    	+ "                        CameraWorldPosition," + "\n"
    	+ "                        LocalMaterialEmissive.xyz," + "\n"
    	+ "                        LocalMaterialAmbient.xyz," + "\n"
    	+ "                        LocalMaterialDiffuse.xyzw," + "\n"
    	+ "                        LocalMaterialSpecular.xyzw," + "\n"
    	+ "                        Light#WorldPosition.xyz," + "\n"
    	+ "                        Light#Ambient.xyz," + "\n"
    	+ "                        Light#Diffuse.xyz," + "\n"
    	+ "                        Light#Specular.xyz," + "\n"
    	+ "                        Light#Attenuation.xyzw);" + "\n"
    	+ "" + "\n";
    public static String surfaceDirectional = ""
    	+ "colorSum += DirectionalLight( position.xyz," + "\n"
    	+ "                              local_normal.xyz," + "\n"
    	+ "                              CameraWorldPosition," + "\n"
    	+ "                              LocalMaterialEmissive.xyz," + "\n"
    	+ "                              LocalMaterialAmbient.xyz," + "\n"
    	+ "                              LocalMaterialDiffuse.xyzw," + "\n"
    	+ "                              LocalMaterialSpecular.xyzw," + "\n"
    	+ "                              Light#WorldDirection.xyz," + "\n"
    	+ "                              Light#Ambient.xyz," + "\n"
    	+ "                              Light#Diffuse.xyz," + "\n"
    	+ "                              Light#Specular.xyz," + "\n"
    	+ "                              Light#Attenuation.xyzw);" + "\n"
    	+ "" + "\n";
    
    public static String surfaceSpot = ""
    	+ "colorSum += SpotLight( position.xyz," + "\n"
    	+ "                       local_normal.xyz," + "\n"
    	+ "                       CameraWorldPosition," + "\n"
    	+ "                       LocalMaterialEmissive.xyz," + "\n"
    	+ "                       LocalMaterialAmbient.xyz," + "\n"
    	+ "                       LocalMaterialDiffuse.xyzw," + "\n"
    	+ "                       LocalMaterialSpecular.xyzw," + "\n"
    	+ "                       Light#WorldPosition.xyz," + "\n"
    	+ "                       Light#WorldDirection.xyz," + "\n"
    	+ "                       Light#Ambient.xyz," + "\n"
    	+ "                       Light#Diffuse.xyz," + "\n"
    	+ "                       Light#Specular.xyz," + "\n"
    	+ "                       Light#SpotCutoff.xyzw," + "\n"
    	+ "                       Light#Attenuation.xyzw);" + "\n"
    	+ "" + "\n";
    
    
    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kImageA the VolumeImage containing the data and textures for
     * rendering.
     * @param kVolumeImageB second VolumeImage.
     * @param kSceneTarget the SceneImage texture with the back-facing polygon texture coordinates.
     */
    public VolumeShaderEffectMultiPassDynamic ( VolumeImage kVolumeImageA, VolumeImage kVolumeImageB, 
                                Texture kSceneTarget )
    {
    	super( kVolumeImageA, kVolumeImageB, kSceneTarget );
    }
    /**
     * The VolumeShaderEffect.CreateVolumeTexture() function constructs and
     * initializes the vertex and pixel shader programs for volume
     * rendering. The vertex shader is the same for each rendering type: MIP,
     * DDR, Composite, Surface, and Composite Surface. The pixel shaders are
     * different for each.
     */
    protected void CreateVolumeTexture ()
    {
        /* Create the vertex shader program, shared by all rendering types. It
         * is implemented in the VolumeShaderVertex.cg file: */        
        m_pkVShader = new VertexShader("VolumeShaderVertex");

        if ( m_kVolumeImageB.GetImage() == null )
        {
            m_kPShaderCMP = new PixelShader("VolumeShaderMultiPass", createProgramText(), true );
        }
        else
        {
            m_kPShaderCMP = new PixelShader("VolumeShaderABMultiPass");
        }
        initTexturesVol(m_kPShaderCMP);
                 
        SetVShader(0,m_pkVShader);
        SetPShader(0,m_kPShaderCMP);
    }

    
    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void Blend(float fBlend)
    {       
    	super.Blend(fBlend);
        checkPixelProgram();
    }


    /**
     * memory cleanup.
     */
    public void dispose()
    {
    	super.dispose();
    }

    
    public void SetClip(int iWhich, float data, boolean bEnable)
    {
    	super.SetClip(iWhich, data, bEnable );
    	checkPixelProgram();    	
    }

    public void SetClipArb(float[] afEquation, boolean bEnable)
    {
    	super.SetClipArb(afEquation, bEnable);
    	checkPixelProgram();
    }
    
    public void SetClipEye(float[] afEquation, boolean bEnable)
    {
    	super.SetClipEye(afEquation, bEnable);
    	checkPixelProgram();
    }
    
    public void SetClipEyeInv(float[] afEquation, boolean bEnable)
    {
    	super.SetClipEyeInv(afEquation, bEnable);
    	checkPixelProgram();
    }

    public void SetGradientMagnitude(boolean bShow)
    {
    	m_bGradientMag = bShow;
    	checkPixelProgram();
    }
    
    public boolean SetLight( String kLightType, float[] afType )
    {
       if ( super.SetLight(kLightType, afType) )
       {
    	   m_kPShaderCMP.GetProgram().SetProgramText( createProgramText() );
    	   GetCProgram(0).Release();
    	   return true;
       }
	   return false;
    }


    public void SURFASTMode()
    {
    	super.SURFASTMode();
        checkPixelProgram();
    }

    public void SURMode()
    {
    	super.SURMode();
        checkPixelProgram();
    }
    public void CMPMode()
    {
    	super.CMPMode();
        checkPixelProgram();
    }
    public void MIPMode( )
    {
    	super.MIPMode();
        checkPixelProgram();
    }
    public void DRRMode( )
    {
    	super.DRRMode();
        checkPixelProgram();
    }

    public void SetCustumBlend(int iBlendEquation, int iLogicOp, int iSrcBlend, int iDstBlend, ColorRGBA kColor  )
    {
    	super.SetCustumBlend(iBlendEquation, iLogicOp, iSrcBlend, iDstBlend, kColor );
    	checkPixelProgram();
    }
    
    private void checkPixelProgram()
    {
    	boolean bReloadShaderProgram = false;
    	if ( (m_afDoClip[0] != 0) && !m_kPShaderCMP.GetProgram().GetProgramText().contains(clipParameters) )
    	{
    		// add clip parameters
    		bReloadShaderProgram = true;
    	}
    	else if ( (m_afDoClip[0] == 0) && m_kPShaderCMP.GetProgram().GetProgramText().contains(clipParameters) )
    	{
    		// remove clip parameters
    		bReloadShaderProgram = true;
    	}
        if ( (m_afBlendParam[0] != 1.0) && !m_kPShaderCMP.GetProgram().GetProgramText().contains(blendParameters) )
        {
        	// add blend to the program:
    		bReloadShaderProgram = true;
        }
        else if ( (m_afBlendParam[0] == 1.0) && m_kPShaderCMP.GetProgram().GetProgramText().contains(blendParameters))
        {
        	// remove blend from the program:
    		bReloadShaderProgram = true;
        }
        if ( isClipAE() && !m_kPShaderCMP.GetProgram().GetProgramText().contains(clipAEParameters) )
        {
        	// add arbitrary/eye clipping to the program:
    		bReloadShaderProgram = true;
        }
        else if ( !isClipAE() && m_kPShaderCMP.GetProgram().GetProgramText().contains(clipAEParameters))
        {
        	// remove arbitrary/eye clipping from the program:
    		bReloadShaderProgram = true;
        }
        if ( m_bGradientMag && !m_kPShaderCMP.GetProgram().GetProgramText().contains(gradientMagnitudeParameters) )
        {
        	// add gradient magnitude to the program:
    		bReloadShaderProgram = true;
        }
        else if ( !m_bGradientMag && m_kPShaderCMP.GetProgram().GetProgramText().contains(gradientMagnitudeParameters))
        {
        	// remove gradient magnitude clipping from the program:
    		bReloadShaderProgram = true;
        }
        if ( (m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && !m_kPShaderCMP.GetProgram().GetProgramText().contains(lightingParametersBasic))
        {
        	// add lighting to program
    		bReloadShaderProgram = true;
        }
        else if ( !(m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && m_kPShaderCMP.GetProgram().GetProgramText().contains(lightingParametersBasic))
        {
        	// remove lighting from program
    		bReloadShaderProgram = true;
        }
        
    	if ( bReloadShaderProgram )
    	{
    		m_kPShaderCMP.GetProgram().SetProgramText( createProgramText() );
    		GetCProgram(0).Release();
    	}
    }
    
    private String createProgramText()
    {
    	boolean bAddGM_Textures = false;
    	boolean bAddNormal_Textures = false;
    	boolean bAddLaplace_Texture = false;

    	String text = "";
    	if ( (m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) )
    	{
    		text += lightingFunctions;
    	}
    	
    	// GLSL Program parameters:
    	text += basicParameters;
    	if ( m_kVolumeImageA.GetImage().isColorImage() )
    	{
    		text += colorParameters;
    	}
    	
    	boolean bLightsOn = false;
    	if ( (m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) )
    	{
    		text += lightingParametersBasic;
    		// for ## Lights
    		for ( int i = 0; i < m_aafLight.length; i++ )
    		{
    			if ( m_aafLight[i][0] != -1 )
    			{
    				bLightsOn = true;
    				String lightParametersSpecific = lightParameters.replaceAll("#", String.valueOf(i) );
    				text += lightParametersSpecific;
    			}
    		}
    		bAddNormal_Textures = true;
    	}
    	if ( m_bGradientMag )
    	{
    		text += gradientMagnitudeParameters;
    		bAddGM_Textures = true;
    	}
    	
    	if ( (m_afDoClip[0] != 0) )
    	{
    		text += clipParameters;
    	}
    	if ( isClipAE() )
    	{
    		text += clipAEParameters;
    	}
    	if ( m_afBlendParam[0] != 1.0 )
    	{
    		text += blendParameters;
    	}
    	
    	// main code to compute position in volume:
    	text += mainSetup;

    	if ( (m_afDoClip[0] != 0) )
    	{
    		text += clipSetup;
    		if ( isClipAE() )
    		{
        		text += clipAESetup;    			
    		}
    	}
    	if ( (m_afDoClip[0] != 0) )
    	{
    		text += clipEnd;
    	}

    	text += readImage;
    	if ( m_kVolumeImageA.GetImage().isColorImage() )
    	{
    		text += readColorMapRGB;
    	}
    	else
    	{
    		text += readColorMap;
    	}
    	if ( m_bGradientMag )
    	{
    		text += gradientMagnitudeComposite;
    	}
    	if ( (m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && bLightsOn )
    	{

    		text += surfaceInit;
    		if ( m_iWhichShader == CMP_SUR )
    		{
    			text += surfaceCompositeInit;
    		}
    		
    		for ( int i = 0; i < m_aafLight.length; i++ )
    		{
    			if ( m_aafLight[i][0] != -1 )
    			{
    				switch ( (int)m_aafLight[i][0] )
    				{
    				case 0: // ambient
    					text += surfaceAmbient.replaceAll("#", String.valueOf(i) );
    					break;
    				case 1: // directional
    					text += surfaceDirectional.replaceAll("#", String.valueOf(i) );
    					break;
    				case 2: // point
    					text += surfacePoint.replaceAll("#", String.valueOf(i) );
    					break;
    				default: // spot
    					text += surfaceSpot.replaceAll("#", String.valueOf(i) );
    					break;
    				}
    			}
    		}
    		text += surfaceFinish;
    	}
    	
    	text += colorComposite;   	
    	
    	if ( m_afBlendParam[0] != 1.0 )
    	{
    		text += blendComposite;
    	}
    	// GLSL Program closing bracket: 
    	text += mainEnd;
    	
    	//System.err.println( text );
    	
    	int iTex = 3;
    	if ( bAddNormal_Textures )
    	{
    		m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageA.GetNormalMapTarget().GetName());
    		m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageA.GetNormalMapTarget());    		
    	}
    	if ( bAddGM_Textures )
    	{
    		m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageA.GetGradientMapTarget().GetName());
    		m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageA.GetGradientMapTarget());
    		m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageA.GetOpacityMapGMTarget().GetName() );
    		m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageA.GetOpacityMapGMTarget() );
    	}
    	return text;
    }
    
    
    
    private static String lightingFunctions = ""
    	+ "void GetDirectionalLightFactors" + "\n"
    	+ "(" + "\n"
    	+ " vec3    kModelPosition," + "\n"
    	+ " vec3    kModelNormal," + "\n"
    	+ " vec3    kCameraPosition," + "\n"
    	+ " vec3    kLightDirection," + "\n"
    	+ " float     fSpecularExponent," + "\n"
    	+ " out float fDiffuseFactor," + "\n"
    	+ " out float fSpecularFactor)" + "\n"
    	+ "{" + "\n"
    	+ "    float fDiff = 0.0;" + "\n"
    	+ "    float fSpec = 0.0;" + "\n"
    	+ "" + "\n"
    	+ "    fDiff = -dot(kModelNormal,kLightDirection);" + "\n"
    	+ "    if (fDiff > 0.0)" + "\n"
    	+ "    {" + "\n"
    	+ "        vec3 kViewVector = normalize(kCameraPosition - kModelPosition);" + "\n"
    	+ "        vec3 kHalfVector = normalize(kViewVector - kLightDirection);" + "\n"
    	+ "        fSpec = dot(kModelNormal,kHalfVector);" + "\n"
    	+ "        if (fSpec > 0.0)" + "\n"
    	+ "        {" + "\n"
    	+ "            fSpec = pow(fSpec,fSpecularExponent);" + "\n"
    	+ "        }" + "\n"
    	+ "        else" + "\n"
    	+ "        {" + "\n"
    	+ "            fSpec = 0.0;" + "\n"
    	+ "        }" + "\n"
    	+ "    }" + "\n"
    	+ "    else" + "\n"
    	+ "    {" + "\n"
    	+ "        fDiff = 0.0;" + "\n"
    	+ "        fSpec = 0.0;" + "\n"
    	+ "    }" + "\n"
    	+ "    " + "\n"
    	+ "    fDiffuseFactor = fDiff;" + "\n"
    	+ "    fSpecularFactor = fSpec;" + "\n"
    	+ "}" + "\n"
    	+ "void GetPointLightFactors" + "\n"
    	+ "(" + "\n"
    	+ " vec3    kModelPosition," + "\n"
    	+ " vec3    kModelNormal," + "\n"
    	+ " vec3    kCameraPosition," + "\n"
    	+ " vec3    kLightPosition," + "\n"
    	+ " float     fSpecularExponent," + "\n"
    	+ " out float fDiffuseFactor," + "\n"
    	+ " out float fSpecularFactor)" + "\n"
    	+ "{" + "\n"
    	+ "    float fDiff, fSpec;" + "\n"
    	+ "" + "\n"
    	+ "    vec3 kVertexDirection = normalize(kModelPosition - kLightPosition);" + "\n"
    	+ "    fDiff = -dot(kModelNormal,kVertexDirection);" + "\n"
    	+ "    if (fDiff > 0.0)" + "\n"
    	+ "    {" + "\n"
    	+ "        vec3 kViewVector = normalize(kCameraPosition - kModelPosition);" + "\n"
    	+ "        vec3 kHalfVector = normalize(kViewVector - kVertexDirection);" + "\n"
    	+ "        fSpec = dot(kModelNormal,kHalfVector);" + "\n"
    	+ "        if (fSpec > 0.0)" + "\n"
    	+ "        {" + "\n"
    	+ "            fSpec = pow(fSpec,fSpecularExponent);" + "\n"
    	+ "        }" + "\n"
    	+ "        else" + "\n"
    	+ "        {" + "\n"
    	+ "            fSpec = 0.0;" + "\n"
    	+ "        }" + "\n"
    	+ "    }" + "\n"
    	+ "    else" + "\n"
    	+ "    {" + "\n"
    	+ "        fDiff = 0.0;" + "\n"
    	+ "        fSpec = 0.0;" + "\n"
    	+ "    }" + "\n"
    	+ "" + "\n"
    	+ "    fDiffuseFactor = fDiff;" + "\n"
    	+ "    fSpecularFactor = fSpec;" + "\n"
    	+ "}" + "\n"
    	+ "void GetSpotLightFactors" + "\n"
    	+ "(" + "\n"
    	+ " vec3    kModelPosition," + "\n"
    	+ " vec3    kModelNormal," + "\n"
    	+ " vec3    kCameraPosition," + "\n"
    	+ " vec3    kLightPosition," + "\n"
    	+ " float     fSpecularExponent," + "\n"
    	+ " vec3    kSpotAxis," + "\n"
    	+ " float     fSpotCosAngle," + "\n"
    	+ " float     fSpotExponent," + "\n"
    	+ " out float fDiffuseFactor," + "\n"
    	+ " out float fSpecularFactor," + "\n"
    	+ " out float fSpotFactor)" + "\n"
    	+ "{" + "\n"
    	+ "    float fDiff, fSpec, fSpot;" + "\n"
    	+ "" + "\n"
    	+ "    vec3 kVertexDirection = normalize(kModelPosition - kLightPosition);" + "\n"
    	+ "    float fVertexCosAngle = dot(kSpotAxis,kVertexDirection);" + "\n"
    	+ "    if (fVertexCosAngle >= fSpotCosAngle)" + "\n"
    	+ "    {" + "\n"
    	+ "        fDiff = -dot(kModelNormal,kVertexDirection);" + "\n"
    	+ "        if (fDiff > 0.0)" + "\n"
    	+ "        {" + "\n"
    	+ "            vec3 kViewVector = normalize(kCameraPosition - kModelPosition);" + "\n"
    	+ "            vec3 kHalfVector = normalize(kViewVector - kVertexDirection);" + "\n"
    	+ "            fSpec = dot(kModelNormal,kHalfVector);" + "\n"
    	+ "            if (fSpec > 0.0)" + "\n"
    	+ "            {" + "\n"
    	+ "                fSpec = pow(fSpec,fSpecularExponent);" + "\n"
    	+ "            }" + "\n"
    	+ "            else" + "\n"
    	+ "            {" + "\n"
    	+ "                fSpec = 0.0;" + "\n"
    	+ "            }" + "\n"
    	+ "            fSpot = pow(fVertexCosAngle,fSpotExponent);" + "\n"
    	+ "        }" + "\n"
    	+ "        else" + "\n"
    	+ "        {" + "\n"
    	+ "            fDiff = 0.0;" + "\n"
    	+ "            fSpec = 0.0;" + "\n"
    	+ "            fSpot = 0.0;" + "\n"
    	+ "        }" + "\n"
    	+ "    }" + "\n"
    	+ "    else" + "\n"
    	+ "    {" + "\n"
    	+ "        fDiff = 0.0;" + "\n"
    	+ "        fSpec = 0.0;" + "\n"
    	+ "        fSpot = 0.0;" + "\n"
    	+ "    }" + "\n"
    	+ "" + "\n"
    	+ "    fDiffuseFactor = fDiff;" + "\n"
    	+ "    fSpecularFactor = fSpec;" + "\n"
    	+ "    fSpotFactor = fSpot;" + "\n"
    	+ "}" + "\n"
    	+ "" + "\n"
    	+ "float GetAttenuation" + "\n"
    	+ "(" + "\n"
    	+ " vec3   kModelPos," + "\n"
    	+ " vec3   kLightPos, " + "\n"
    	+ " vec4   kAttenuation)" + "\n"
    	+ "{" + "\n"
    	+ "    vec3 kVertexWorldDir = kModelPos - kLightPos;" + "\n"
    	+ "    float fDistance = sqrt(" + "\n"
    	+ "                           kVertexWorldDir.x*kVertexWorldDir.x +" + "\n"
    	+ "                           kVertexWorldDir.y*kVertexWorldDir.y +" + "\n"
    	+ "                           kVertexWorldDir.z*kVertexWorldDir.z);" + "\n"
    	+ "" + "\n"
    	+ "    float fAttn = kAttenuation.w/(kAttenuation.x + fDistance*(kAttenuation.y" + "\n"
    	+ "                                                              + fDistance*kAttenuation.z));" + "\n"
    	+ "    " + "\n"
    	+ "    return fAttn;" + "\n"
    	+ "}" + "\n"

    	+ "vec4 AmbientLight(  vec3   MaterialEmissive," + "\n"
    	+ "                    vec3   MaterialAmbient," + "\n"
    	+ "                    vec3   LightAmbient," + "\n"
    	+ "                    vec4   LightAttenuation )" + "\n"
    	+ "{" + "\n"
    	+ "    vec4 kResult = vec4(0.0,0.0,0.0,0.0);" + "\n"
    	+ "    vec3 kLAmb = LightAttenuation.w*LightAmbient;" + "\n"
    	+ "    kResult.rgb = MaterialEmissive + MaterialAmbient*kLAmb;" + "\n"
    	+ "    kResult.a = 1.0;" + "\n"
    	+ "    return kResult;" + "\n"
    	+ "}" + "\n"
    	+ "vec4 DirectionalLight(  vec3 kModelPosition," + "\n"
    	+ "                        vec3 kModelNormal," + "\n"
    	+ "                        vec3   CameraWorldPosition," + "\n"
    	+ "                        vec3   MaterialEmissive," + "\n"
    	+ "                        vec3   MaterialAmbient," + "\n"
    	+ "                        vec3   MaterialDiffuse," + "\n"
    	+ "                        vec4   MaterialSpecular," + "\n"
    	+ "                        vec3   LightDirection," + "\n"
    	+ "                        vec3   LightAmbient," + "\n"
    	+ "                        vec3   LightDiffuse," + "\n"
    	+ "                        vec3   LightSpecular," + "\n"
    	+ "                        vec4   LightAttenuation )" + "\n"
    	+ "{" + "\n"
    	+ "    vec4 kResult = vec4(0.0,0.0,0.0,0.0);" + "\n"
    	+ "    float fDiff, fSpec;" + "\n"
    	+ "    GetDirectionalLightFactors(kModelPosition,kModelNormal," + "\n"
    	+ "                               CameraWorldPosition,LightDirection,MaterialSpecular.a," + "\n"
    	+ "                               fDiff,fSpec);" + "\n"
    	+ "    vec3 kColor = MaterialAmbient*LightAmbient;" + "\n"
    	+ "    if (fDiff > 0.0)" + "\n"
    	+ "    {" + "\n"
    	+ "        kColor += fDiff*MaterialDiffuse.rgb*LightDiffuse;" + "\n"
    	+ "        if (fSpec > 0.0)" + "\n"
    	+ "        {" + "\n"
    	+ "            kColor += fSpec*MaterialSpecular.rgb*LightSpecular;" + "\n"
    	+ "        }" + "\n"
    	+ "    }" + "\n"
    	+ "" + "\n"
    	+ "    kResult.rgb = MaterialEmissive + LightAttenuation.w*kColor;" + "\n"
    	+ "    kResult.a = 1.0;" + "\n"
    	+ "    return kResult;" + "\n"
    	+ "}" + "\n"
    	+ "vec4 PointLight(     vec3 kModelPosition," + "\n"
    	+ "                     vec3 kModelNormal," + "\n"
    	+ "                     vec3   CameraWorldPosition," + "\n"
    	+ "                     vec3   MaterialEmissive," + "\n"
    	+ "                     vec3   MaterialAmbient," + "\n"
    	+ "                     vec4   MaterialDiffuse," + "\n"
    	+ "                     vec4   MaterialSpecular," + "\n"
    	+ "                     vec3   LightWorldPosition," + "\n"
    	+ "                     vec3   LightAmbient," + "\n"
    	+ "                     vec3   LightDiffuse," + "\n"
    	+ "                     vec3   LightSpecular," + "\n"
    	+ "                     vec4   LightAttenuation)" + "\n"
    	+ "{" + "\n"
    	+ "    vec4 kResult = vec4(0.0,0.0,0.0,0.0);" + "\n"
    	+ "    float fDiff, fSpec;" + "\n"
    	+ "    GetPointLightFactors(kModelPosition.xyz,kModelNormal," + "\n"
    	+ "                         CameraWorldPosition,LightWorldPosition,MaterialSpecular.a," + "\n"
    	+ "                         fDiff,fSpec);" + "\n"
    	+ "    float fAttn = GetAttenuation(kModelPosition.xyz," + "\n"
    	+ "                                 LightWorldPosition,LightAttenuation);" + "\n"
    	+ "" + "\n"
    	+ "    vec3 kColor = MaterialAmbient*LightAmbient;" + "\n"
    	+ "    if (fDiff > 0.0)" + "\n"
    	+ "    {" + "\n"
    	+ "        kColor += fDiff*MaterialDiffuse.xyz*LightDiffuse;" + "\n"
    	+ "        if (fSpec > 0.0)" + "\n"
    	+ "        {" + "\n"
    	+ "            kColor += fSpec*MaterialSpecular.xyz*LightSpecular;" + "\n"
    	+ "        }" + "\n"
    	+ "    }" + "\n"
    	+ "    kResult.rgb = MaterialEmissive + fAttn*kColor;" + "\n"
    	+ "    kResult.a = MaterialDiffuse.a;" + "\n"
    	+ "    return kResult;" + "\n"
    	+ "}" + "\n"
    	+ "vec4 SpotLight(     vec3 kModelPosition," + "\n"
    	+ "                    vec3 kModelNormal," + "\n"
    	+ "                    vec3 CameraWorldPosition," + "\n"
    	+ "                    vec3 MaterialEmissive," + "\n"
    	+ "                    vec3 MaterialAmbient," + "\n"
    	+ "                    vec4 MaterialDiffuse," + "\n"
    	+ "                    vec4 MaterialSpecular," + "\n"
    	+ "                    vec3 LightWorldPosition," + "\n"
    	+ "                    vec3 LightWorldDirection," + "\n"
    	+ "                    vec3 LightAmbient," + "\n"
    	+ "                    vec3 LightDiffuse," + "\n"
    	+ "                    vec3 LightSpecular," + "\n"
    	+ "                    vec4 LightSpotCutoff," + "\n"
    	+ "                    vec4 LightAttenuation)" + "\n"
    	+ "{" + "\n"
    	+ "    vec4 kResult = vec4(0.0,0.0,0.0,0.0);" + "\n"
    	+ "    float fDiff, fSpec, fSpot;" + "\n"
    	+ "    GetSpotLightFactors(kModelPosition.xyz,kModelNormal," + "\n"
    	+ "                        CameraWorldPosition,LightWorldPosition,MaterialSpecular.a," + "\n"
    	+ "                        LightWorldDirection,LightSpotCutoff.y,LightSpotCutoff.w,fDiff," + "\n"
    	+ "                        fSpec,fSpot);" + "\n"
    	+ "    float fAttn = GetAttenuation(kModelPosition.xyz," + "\n"
    	+ "                                 LightWorldPosition,LightAttenuation);" + "\n"
    	+ "" + "\n"
    	+ "    vec3 kColor = MaterialAmbient*LightAmbient;" + "\n"
    	+ "    if (fSpot > 0.0)" + "\n"
    	+ "    {" + "\n"
    	+ "        if (fDiff > 0.0)" + "\n"
    	+ "        {" + "\n"
    	+ "            kColor += (fSpot*fDiff)*MaterialDiffuse.rgb*LightDiffuse;" + "\n"
    	+ "            if (fSpec > 0.0)" + "\n"
    	+ "            {" + "\n"
    	+ "                kColor += (fSpot*fSpec)*MaterialSpecular.rgb*LightSpecular;" + "\n"
    	+ "            }" + "\n"
    	+ "        }" + "\n"
    	+ "    }" + "\n"
    	+ "    " + "\n"
    	+ "    kResult.rgb = MaterialEmissive + fAttn*kColor;" + "\n"
    	+ "    kResult.a = MaterialDiffuse.a;" + "\n"
    	+ "    return kResult;" + "\n"
    	+ "}" + "\n"
    	+ "" + "\n";

    
}
