package gov.nih.mipav.view.renderer.WildMagic.Render;


import java.util.Vector;

import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetEffect;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetState;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Ellipsoid3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;


public class VSEMD_MultipleImages extends VolumeShaderEffectMultiPass
    implements StreamInterface
{       
	private static final long serialVersionUID = -4552046274082477860L;

	private static String basicParameters = ""
	    	+ "in vec4 outPos;" + "\n"
	    	+ "in vec3 varTexCoord;" + "\n"
	    	+ "out vec4 fragColor;" + "\n"
    	+ "uniform mat4 WVPMatrix;" + "\n"
    	+ "uniform sampler2D sceneImage; " + "\n"
    	+ "uniform vec4 BackgroundColor;" + "\n"
    	+ "uniform float StepSize;" + "\n"
    	+ "uniform float nPasses;" + "\n"
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
        + "uniform vec4 clipArbInv;" + "\n"
    	+ "uniform vec4 clipEye;" + "\n"
    	+ "uniform vec4 clipEyeInv;" + "\n"
    	+ "" + "\n";
    
    private static String clipSphereParameters = ""
            + "uniform vec4 sphereCenter;" + "\n"
            + "uniform vec4 sphereScale;" + "\n"
        + "" + "\n";
    
    private static String clipOBBParameters = ""
            + "uniform vec4 sphereCenter;" + "\n"
            + "uniform vec4 sphereScale;" + "\n"
            + "uniform vec4 obbClipAxis0;" + "\n"
            + "uniform vec4 obbClipAxis1;" + "\n"
            + "uniform vec4 obbClipAxis2;" + "\n"
            + "uniform vec4 obbClipExtent;" + "\n"
        + "" + "\n";
    		
    
    private static String mainSetup = ""
    	+ "void p_VolumeShaderMultiPass() {" + "\n"
    	+ "vec2 texc = ((outPos.xy / outPos.w) + 1.0) * 0.5;" + "\n"
    	+ "vec3 back_position  = texture(sceneImage, texc, 0.0).xyz;" + "\n"
    	+ "if ( (back_position.x == 0) && (back_position.y == 0) && (back_position.z == 0) ) {" + "\n"
    	+ "   discard;" + "\n"
    	+ "   return;" + "\n"
    	+ "}" + "\n"
    	+ "vec3 start = varTexCoord.xyz;" + "\n"
    	+ "vec3 dir = back_position - start;" + "\n"
    	+ "dir = normalize(dir);" + "\n"
    	+ "fragColor = vec4(0);" + "\n"
    	+ "int count = int(nPasses);" + "\n"
    	+ "for ( int p = count -1; p > 0; p-- ) {" + "\n"
    	+ "   float fPos = p;" + "\n"
    	+ "   vec3 position = start + fPos * StepSize * dir;" + "\n"
    	+ "   vec3 dir2 = position - start;" + "\n"
    	+ "   vec3 dir3 = back_position - start;" + "\n"
    	+ "   if ( dot(dir2,dir2) > dot(dir3,dir3) ) {" + "\n"
        + "       continue;" + "\n"
    	+ "   }" + "\n";


    private static String mainEnd = ""
            + "fragColor = clamp(fragColor, vec4(0), vec4(1));" + "\n"
            + "if ( fragColor == vec4(0) ) {" + "\n"
        	+ "   discard;" + "\n"
        	+ "}" + "\n"
        	+ "}" + "\n";
        //+ "if ( fragColor.a == 0 ) {" + "\n"
    	//+ "   discard;" + "\n"
    	//+ "}" + "\n"
        //+ "if ( (fragColor.r == 0) && (fragColor.g == 0) && (fragColor.b == 0) ) {" + "\n"
    	//+ "   discard;" + "\n"
    	//+ "}" + "\n"
    	//+ "}" + "\n";
    
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
    	//+ "   fragColor = vec4(0);" + "\n"
    	//    	+ "   discard;" + "\n"
        //+ "   return;" + "\n"
        + "   continue;" + "\n"
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
    	+ "   if ( (fDot < clipEye.w) || (fDotInv > clipEyeInv.w) || (fDotArb > clipArb.w)  || (fDotArb < clipArbInv.w) ) {" + "\n"
    	+ "      bClipped = 1.0;" + "\n"
    	+ "   }" + "\n"
    	+ "}" + "\n";

    private static String clipEllipsoidSetup = ""
        + "if ( bClipped != 1.0 ) {" + "\n"
    	+ "   vec3 clipDif = position.xyz - ellipsoidCenter.xyz;" + "\n"
    	+ "   float aDot0 = dot(ellipsoidAxis0.xyz, clipDif);" + "\n"
    	+ "   float ratio0 = aDot0 / ellipsoidExtent.x;" + "\n"
    	+ "   float aDot1 = dot(ellipsoidAxis1.xyz, clipDif);" + "\n"
    	+ "   float ratio1 = aDot1 / ellipsoidExtent.y;" + "\n"
    	+ "   float aDot2 = dot(ellipsoidAxis2.xyz, clipDif);" + "\n"
    	+ "   float ratio2 = aDot2 / ellipsoidExtent.z;" + "\n"
    	+ "   float clipEllipsoid = ratio0*ratio0+ratio1*ratio1+ratio2*ratio2-1.0f;" + "\n"
    	+ "   if ( clipEllipsoid > 0 ) {" + "\n"
    	+ "     bClipped = 1.0;" + "\n"
    	+ "   }" + "\n"
    	+ "}" + "\n";

    private static String clipSphereSetup = ""
        + "if ( bClipped != 1.0 ) {" + "\n"
    	+ "   vec3 scaleP = position.xyz * sphereScale.xyz;"
    	+ "   float clipSphere = (scaleP.x - sphereCenter.x) * (scaleP.x - sphereCenter.x) + " + "\n"
    	+ "                      (scaleP.y - sphereCenter.y) * (scaleP.y - sphereCenter.y) + " + "\n"
    	+ "                      (scaleP.z - sphereCenter.z) * (scaleP.z - sphereCenter.z);" + "\n"
    	+ "   if ( clipSphere > sphereCenter.w ) {" + "\n"
    	+ "     bClipped = 1.0;" + "\n"
    	+ "   }" + "\n"
    	+ "}" + "\n";
    

    private static String clipOBBSetup = ""
        + "if ( bClipped != 1.0 ) {" + "\n"
    	+ "   vec3 scaleP = position.xyz * sphereScale.xyz;" + "\n"
    	+ "   vec3 diff = scaleP.xyz - sphereCenter.xyz;" + "\n"
    	+ "   float clipOBB = dot( diff, obbClipAxis0.xyz);" + "\n"
    	+ "   if ( abs(clipOBB) > obbClipExtent.r ) {" + "\n"
    	+ "     bClipped = 1.0;" + "\n"
    	+ "   }" + "\n"
    	+ "   if ( bClipped != 1.0 ) {" + "\n"
    	+ "      clipOBB = dot( diff, obbClipAxis1.xyz);" + "\n"
    	+ "      if ( abs(clipOBB) > obbClipExtent.g ) {" + "\n"
    	+ "         bClipped = 1.0;" + "\n"
    	+ "      }" + "\n"
    	+ "   }" + "\n"
    	+ "   if ( bClipped != 1.0 ) {" + "\n"
    	+ "      clipOBB = dot( diff, obbClipAxis2.xyz);" + "\n"
    	+ "      if ( abs(clipOBB) > obbClipExtent.b ) {" + "\n"
    	+ "         bClipped = 1.0;" + "\n"
    	+ "      }" + "\n"
    	+ "   }" + "\n"
    	+ "}" + "\n";

    private boolean[] imageOn;
    private VolumeImage[] hyperstack;
    private Texture hyperstackColormap;
    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kImageA the VolumeImage containing the data and textures for
     * rendering.
     * @param kVolumeImageB second VolumeImage.
     * @param kSceneTarget the SceneImage texture with the back-facing polygon texture coordinates.
     */
    public VSEMD_MultipleImages ( VolumeImage[] volumeImages, Texture colormap, Texture kSceneTarget )
    {
    	super( volumeImages[0], null, kSceneTarget );
    	hyperstack = volumeImages;
    	hyperstackColormap = colormap;
    	imageOn = new boolean[volumeImages.length];
    	for ( int i = 0; i < imageOn.length; i++ ) {
    		imageOn[i] = true;
    	}
		
        CreateVolumeTexture();
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

        m_kPShaderCMP = new PixelShader("VolumeShaderMultiPass", "" );
        createProgramText();
                 
        SetVShader(0,m_pkVShader);
        SetPShader(0,m_kPShaderCMP);
    }

    
    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void Blend(float fBlend)
    {       
//    	super.Blend(fBlend);
//        checkPixelProgram();
    }

    public void setABBlend(float fBlend)
    {}
    
    /**
     * memory cleanup.
     */
    public void dispose()
    {
    	super.dispose();
    }

    public void printProgram()
    {
    	if ( (m_kPShaderCMP != null) && (m_kPShaderCMP.GetProgram() != null) )
    	{
    		System.err.println( m_kPShaderCMP.GetProgram().GetProgramText() );
    	}
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
    
    public void SetClipSphere(Vector3f center, Vector3f scale, float radius, boolean bEnable ) {
    	super.SetClipSphere(center, scale, radius, bEnable);
    	checkPixelProgram();
    }

    
    public void SetClipOBB( Vector3f center, Vector3f scale, Vector3f[] axes, float[] extents, boolean bEnable ) {
    	super.SetClipOBB(center, scale, axes, extents, bEnable);
    	checkPixelProgram();
    }
    
    public void SetImageOn(int which, boolean on) {
    	if ( which >= 0 && which < imageOn.length ) {
    		if ( imageOn[which] != on ) {
    			imageOn[which] = on;
    			createProgramText();
    		}
    	}
    }


    private void checkPixelProgram()
    {
    	createProgramText();
    }
    
    private String[] lut = new String[] {"a", "b", "c", "d" };
    private String createProgramText()
    {
    	String text = "";

    	// Start Parameters:
    	
    	// GLSL Program parameters:
    	text += basicParameters;
		text += "uniform sampler2D colormap;\n";
    	for ( int i = 0; i < imageOn.length; i++ ) {
    		if ( imageOn[i] ) {
    	    	text += "uniform sampler3D volume" + (i) + ";\n";
    		}
    	}
    	
    	if ( (m_afDoClip[0] != 0) )
    	{
    		text += clipParameters;
    	}
    	if ( isClipAE() )
    	{
    		text += clipAEParameters;
    	}
    	if ( isClipSphere() )
    	{
    		text += clipSphereParameters;
    	}
    	if ( isClipOBB() )
    	{
    		text += clipOBBParameters;
    	}
    	
    	
    	
    	
    	//if ( (m_afBlendParam[0] != 1.0) )
    	{
    		text += blendParameters;
    	}
    	
    	// End Parameters
    	    	
    	// Start code:
    	// main code to compute position in volume:
    	// loops over the ray from back to front, compositing the color:
    	text += mainSetup;

    	if ( (m_afDoClip[0] != 0) )
    	{
    		text += clipSetup;
    		if ( isClipAE() )
    		{
        		text += clipAESetup;    			
    		}
    		if ( isClipSphere() )
    		{
    			text += clipSphereSetup;
    		}
        	if ( isClipOBB() )
        	{
        		text += clipOBBSetup;
        	}
    	}
    	if ( (m_afDoClip[0] != 0) )
    	{
    		text += clipEnd;
    	}
    	// end setup:
    	
    	// generate color function:    	
    	text += "   vec4 data = vec4(0.0);" + "\n";
    	text += "   vec4 colorTemp = vec4(0.0);" + "\n";
    	text += "   vec4 color = vec4(0.0);" + "\n";
    	text += "   float opacity = 0.0;" + "\n";
    	text += "   vec2 cm = vec2(0.0);" + "\n";
    	float scale = imageOn.length > 1 ? (1f/(float)(imageOn.length-1)) : 1f;
    	for ( int i = 0; i < imageOn.length; i++ ) 
    	{
    		if ( imageOn[i] ) 
    		{
    			String readImage = ""
    					+ "   data = texture(volume"+ (i) + ",position, 0.0);" + "\n";
    			String readColorMap = ""
						+ "   cm.r = data.r;" + "\n"	
						+ "   cm.g = " + ((float)i * scale) + ";" + "\n"	
						+ "   colorTemp = texture(colormap,cm, 0.0);" + "\n"
    					+ "   color.rgb += colorTemp.a*colorTemp.rgb;" + "\n"
    		        	+ "   opacity += colorTemp.a;" + "\n";
    			
    	    	text += readImage;
    	    	text += readColorMap;
    		}
    	}
		String finalColor = ""
				//	            + "   fragColor.rgb = (1 - color.a)*fragColor.rgb + color.a*color.rgb;" + "\n"
//	            + "   fragColor.a   += color.a;" + "\n";

				+ "   color = clamp(color, vec4(0), vec4(1));" + "\n"
				+ "   opacity = clamp(opacity, 0, 1);" + "\n"
				+ "   if ( (color.r != 0) || (color.g != 0) || (color.b != 0) ) { "
	            + "     fragColor.rgb = (1 - opacity)*fragColor.rgb + color.rgb;" + "\n"
	            + "     fragColor.a   += opacity;" + "\n"
	            + "   }" + "\n";
		
    	text += finalColor;
    	// loop end:
    	text += "}" + "\n";

    	
    	// GLSL Program closing bracket: 
    	text += mainEnd;
    	// Done generating program text.
    	

    	if ( (m_kPShaderCMP != null) && (m_kPShaderCMP.GetProgram() != null) )
    	{
    			// Add the used textures to the shader program data structures:
    			int iTex = 0;
//    			m_kPShaderCMP.SetImageName(iTex, m_kSceneTarget.GetName(), "sceneImage");
    			m_kPShaderCMP.SetTexture(iTex++, m_kSceneTarget, "sceneImage");
				m_kPShaderCMP.SetTexture(iTex++, hyperstackColormap, "colormap");  
    			for ( int i = 0; i < imageOn.length; i++ )
    			{
    				if ( imageOn[i] ) {
    					m_kPShaderCMP.SetTexture(iTex++, hyperstack[i].GetVolumeTarget(), "volume" + (i) );
    				}
    			}

        		if ( !text.equals( m_kPShaderCMP.GetProgram().GetProgramText() ))
        		{
//    			System.err.println("");
//    			System.err.println("");
//    			System.err.println("createProgram");
    			m_kPShaderCMP.GetProgram().SetProgramText( text );
//    			System.err.println( text );
    			if ( GetCProgram(0) != null )
    			{
    				GetCProgram(0).Reload(true);
    			}
    		}
        	
//        	System.err.println("START");
//    		System.err.println( text );
//        	System.err.println("END");
    		return text;
    	}
    	
//    	System.err.println("START");
//		System.err.println( text );
//    	System.err.println("END");
    	return text;
    }
    

    
}
