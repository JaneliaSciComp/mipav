package gov.nih.mipav.view.renderer.WildMagic.Render;


import java.util.Vector;

import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetState;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
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
    	+ "uniform vec4 BackgroundColor;" + "\n"
    	+ "uniform float StepSize;" + "\n"
    	+ "uniform float iPass;" + "\n"
    	+ "" + "\n";
	
	private static String basicParametersB = ""
		+ "uniform sampler3D jVolumeImageB; " + "\n"
    	+ "uniform float ABBlend;" + "\n";

	private static String colorMapA = ""
    	+ "uniform sampler1D cColorMapA;" + "\n";
	
	private static String colorMapB = ""
	    	+ "uniform sampler1D kColorMapB;" + "\n";

	private static String colorMapGMA = ""
    	+ "uniform sampler1D gOpacityMapA_GM;" + "\n";

	private static String colorMapGMB = ""
    	+ "uniform sampler1D gOpacityMapA_GM;" + "\n";

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

    private static String lightingParametersBasicColorA = ""
    	+ "uniform sampler3D eNormalMapA;" + "\n";

    private static String lightingParametersBasicColorB = ""
    	+ "uniform sampler3D mNormalMapB;" + "\n";
    		
    private static String lightingParametersBasic = ""
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
    	+ "uniform vec4 Light#ModelPosition;" + "\n"
    	+ "uniform vec4 Light#ModelDirection;" + "\n";
    
    private static String gradientMagnitudeParametersA = ""
    	+ "uniform sampler3D fVolumeImageA_GM;" + "\n"
    	+ "" + "\n";
    
    private static String gradientMagnitudeParametersB = ""
    	+ "uniform sampler3D nVolumeImageB_GM;" + "\n"
    	+ "" + "\n";
        
    private static String multiHistogramWidgetParameters = ""
    	+ "uniform vec2 Shift#;" + "\n"
    	+ "uniform vec3 InvY0MY1#;" + "\n"
    	+ "uniform vec4 LevColor#;" + "\n"
    	+ "uniform vec4 LevMidLine#;" + "\n"
    	+ "uniform vec4 LevLeftLine#;" + "\n"
    	+ "uniform vec4 LevRightLine#;" + "\n"
    	+ "uniform float BoundaryEmphasis#;" + "\n"
    	+ "uniform vec4 Center#;" + "\n"
    	+ "uniform vec4 Radius#;" + "\n"
    	+ "" + "\n";

    private static String multiHistogramWidgetColorParameters = ""
    	+ "uniform sampler1D hColorMap#;" + "\n"
    	+ "" + "\n";
    
    private static String mainSetup = ""
    	+ "void p_VolumeShaderMultiPass() {" + "\n"
    	+ "vec2 texc = ((outPos.xy / outPos.w) + 1.0) * 0.5;" + "\n"
    	+ "vec3 back_position  = texture2D(aSceneImage, texc).xyz;" + "\n"
    	+ "if ( (back_position.x == 0) && (back_position.y == 0) && (back_position.z == 0) ) {" + "\n"
    	+ "   discard;" + "\n"
    	+ "   return;" + "\n"
    	+ "}" + "\n"
    	+ "vec3 start = gl_TexCoord[0].xyz;" + "\n"
    	+ "vec3 dir = back_position - start;" + "\n"
    	+ "dir = normalize(dir);" + "\n"
    	+ "float fPos = iPass;" + "\n"
    	+ "vec3 position = start + fPos * StepSize * dir;" + "\n"
    	+ "vec3 dir2 = position - start;" + "\n"
    	+ "dir = back_position - start;" + "\n"
    	+ "if ( dot(dir2,dir2) > dot(dir,dir) ) {" + "\n"
    	+ "   discard;" + "\n"
    	+ "   return;" + "\n"
    	+ "}" + "\n";
    
    private static String colorParameters = ""
    	+ "uniform vec3 ColorLUTOnA;" + "\n"
    	+ "\n";

    private static String readImageA = ""
        + "vec4 color = vec4(0.0);" + "\n"
        + "float opacity = 1.0;" + "\n"
    	+ "color = texture3D(bVolumeImageA,position);" + "\n"
    	+ "vec4 normal = vec4(color.g, color.b, color.a, 0);" + "\n"
    	+ "color = vec4(color.r, color.r, color.r, color.r);" + "\n";

    private static String readImageB = ""
        + "vec4 color = vec4(0.0);" + "\n"
        + "float opacity = 1.0;" + "\n"
    	+ "color = texture3D(jVolumeImageB,position);" + "\n"
    	+ "vec4 normal = vec4(color.g, color.b, color.a, 0);" + "\n"
    	+ "color = vec4(color.r, color.r, color.r, color.r);" + "\n";


    private static String readImageColorA = ""
        + "vec4 color = vec4(0.0);" + "\n"
        + "float opacity = 1.0;" + "\n"
    	+ "color = texture3D(bVolumeImageA,position);" + "\n";

    private static String readImageColorB = ""
        + "vec4 color = vec4(0.0);" + "\n"
        + "float opacity = 1.0;" + "\n"
    	+ "color = texture3D(jVolumeImageB,position);" + "\n";

    private static String readColorMapA = ""
    	+ "color = texture1D(cColorMapA,color.r);" + "\n"
    	+ "opacity = color.a;" + "\n";

    private static String readColorMapB = ""
    	+ "color = texture1D(kColorMapB,color.r);" + "\n"
    	+ "opacity = color.a;" + "\n";

    private static String readColorMapRGBA = ""
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

    private static String readColorMapRGBB = ""
    	+ "vec4 colorTemp = vec4(0);" + "\n"
    	+ "opacity = 0;" + "\n"
    	+ "if ( ColorLUTOnA.x != 0.0 ) {" + "\n"
    	+ "   colorTemp = texture1D(kColorMapB,color.r);" + "\n"
    	+ "   color.r = colorTemp.r;" + "\n"
    	+ "   opacity += colorTemp.a;" + "\n"
    	+ "}" + "\n"
    	+ "else {" + "\n"
    	+ "   color.r = 0.0;" + "\n"
    	+ "}" + "\n"
    	+ "if ( ColorLUTOnA.y != 0.0 ) {" + "\n"
    	+ "   colorTemp = texture1D(kColorMapB,color.g);" + "\n"
    	+ "   color.g = colorTemp.g;" + "\n"
    	+ "   opacity += colorTemp.a;" + "\n"
    	+ "}" + "\n"
    	+ "else {" + "\n"
    	+ "   color.g = 0.0;" + "\n"
    	+ "}" + "\n"
    	+ "if ( ColorLUTOnA.z != 0.0 ) {" + "\n"
    	+ "   colorTemp = texture1D(kColorMapB,color.b);" + "\n"
    	+ "   color.b = colorTemp.b;" + "\n"
    	+ "   opacity += colorTemp.a;" + "\n"
    	+ "}" + "\n"
    	+ "else {" + "\n"
    	+ "   color.b = 0.0;" + "\n"
    	+ "}" + "\n";

    private static String gradientMagnitudeCompositeA = ""
        	+ "vec4 colorGM = texture3D(fVolumeImageA_GM,position);" + "\n"
        	+ "float opacityGM = texture1D(gOpacityMapA_GM,colorGM.r).r;" + "\n";

    private static String gradientMagnitudeCompositeOpacityA = ""
        	+ "opacity = opacity * opacityGM;" + "\n";

    private static String gradientMagnitudeCompositeB = ""
        	+ "vec4 colorGM = texture3D(nVolumeImageB_GM,position);" + "\n"
        	+ "float opacityGM = texture1D(oOpacityMapB_GM,colorGM.r).r;" + "\n"
        	+ "opacity = opacity * opacityGM;" + "\n";
    
    private static String blendComposite = ""
    	+ "opacity *= Blend;" + "\n";

    private static String compositeMIP_DRR = ""
    	+ "color.rgb *= opacity;" + "\n";
    private static String blendMIP_DRR = ""
    	+ "color.rgb *= Blend * opacity;" + "\n";

    private static String calcColorAStart = ""
    	+ "vec4 calcColorA(vec3 position) {" + "\n";

    private static String calcColorBStart = ""
    	+ "vec4 calcColorB(vec3 position) {" + "\n";
    
    private static String calcColorEnd = ""
        + "color.a = opacity;" + "\n"
        + "return color;" + "\n"
        + "\n" + "}" + "\n";

    private static String calcColorA = ""
   		+ "vec4 colorA = calcColorA(position);" + "\n";
    
    private static String calcColorB = ""
       	+ "vec4 colorB = calcColorB(position);" + "\n";

    private static String finalColorA = ""
    	+ "gl_FragColor.rgb = colorA.rgb;" + "\n"
    	+ "gl_FragColor.a = colorA.a;" + "\n";

    private static String finalColorB = ""
    	+ "gl_FragColor.rgb = colorB.rgb;" + "\n"
    	+ "gl_FragColor.a = colorB.a;" + "\n";

    private static String finalColorAB = ""
    	+ "gl_FragColor.rgb = (ABBlend * colorA.rgb) + ((1 - ABBlend) * colorB.rgb);" + "\n"
    	+ "gl_FragColor.a = (ABBlend * colorA.a) + ((1 - ABBlend) * colorB.a);" + "\n";
    
    private static String mainEnd = ""
            + "if ( gl_FragColor == vec4(0) ) {" + "\n"
        	+ "   discard;" + "\n"
        	+ "}" + "\n"
        	+ "}" + "\n";
        //+ "if ( gl_FragColor.a == 0 ) {" + "\n"
    	//+ "   discard;" + "\n"
    	//+ "}" + "\n"
        //+ "if ( (gl_FragColor.r == 0) && (gl_FragColor.g == 0) && (gl_FragColor.b == 0) ) {" + "\n"
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
    	//+ "   gl_FragColor = vec4(0);" + "\n"
    	+ "   discard;" + "\n"
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
    	+ "vec3 local_normal = ((2.0,2.0,2.0)* normal.xyz) - (1.0, 1.0, 1.0);" + "\n"
    	+ "local_normal = normalize( local_normal );" + "\n"
    	+ "" + "\n";

    public static String surfaceInitColorA = ""
        	+ "vec4 normal = texture3D(eNormalMapA,position);" + "\n"
        	+ "normal.w = 0.0;" + "\n";

    public static String surfaceInitColorB = ""
        	+ "vec4 normal = texture3D(mNormalMapB,position);" + "\n"
        	+ "normal.w = 0.0;" + "\n";
    
    public static String surfaceCompositeInit = ""
    	+ "LocalMaterialDiffuse = color;" + "\n"
    	+ "LocalMaterialAmbient *= color.xyz;" + "\n"
    	+ "" + "\n";

    
    public static String multiHistoSurfaceCompositeInit = ""
    	+ "LocalMaterialDiffuse = color;" + "\n"
    	+ "" + "\n";

    public static String surfaceFinish = ""
    	+ "color = colorSum;" + "\n";
    
    public static String surfaceAmbient = ""
    	+ "colorSum += AmbientLight( LocalMaterialEmissive.xyz," + "\n"
    	+ "                          LocalMaterialAmbient.xyz," + "\n"
    	+ "                          Light#Ambient.xyz," + "\n"
    	+ "                          Light#Attenuation );" + "\n";

    public static String surfacePointStatic = ""
    	+ "colorSum += PointLight( position.xyz," + "\n"
    	+ "                        local_normal.xyz," + "\n"
    	+ "                        CameraModelPosition," + "\n"
    	+ "                        LocalMaterialEmissive.xyz," + "\n"
    	+ "                        LocalMaterialAmbient.xyz," + "\n"
    	+ "                        LocalMaterialDiffuse.xyzw," + "\n"
    	+ "                        LocalMaterialSpecular.xyzw," + "\n"
    	+ "                        Light#ModelPosition.xyz," + "\n"
    	+ "                        Light#Ambient.xyz," + "\n"
    	+ "                        Light#Diffuse.xyz," + "\n"
    	+ "                        Light#Specular.xyz," + "\n"
    	+ "                        Light#Attenuation.xyzw);" + "\n"
    	+ "" + "\n";
    public static String surfacePoint = ""
    	+ "colorSum += PointLight( position.xyz," + "\n"
    	+ "                        local_normal.xyz," + "\n"
    	+ "                        CameraWorldPosition," + "\n"
    	+ "                        LocalMaterialEmissive.xyz," + "\n"
    	+ "                        LocalMaterialAmbient.xyz," + "\n"
    	+ "                        LocalMaterialDiffuse.xyzw," + "\n"
    	+ "                        LocalMaterialSpecular.xyzw," + "\n"
    	+ "                        Light#ModelPosition.xyz," + "\n"
    	+ "                        Light#Ambient.xyz," + "\n"
    	+ "                        Light#Diffuse.xyz," + "\n"
    	+ "                        Light#Specular.xyz," + "\n"
    	+ "                        Light#Attenuation.xyzw);" + "\n"
    	+ "" + "\n";
    public static String surfaceDirectionalStatic = ""
    	+ "colorSum += DirectionalLight( position.xyz," + "\n"
    	+ "                              local_normal.xyz," + "\n"
    	+ "                              CameraModelPosition," + "\n"
    	+ "                              LocalMaterialEmissive.xyz," + "\n"
    	+ "                              LocalMaterialAmbient.xyz," + "\n"
    	+ "                              LocalMaterialDiffuse.xyzw," + "\n"
    	+ "                              LocalMaterialSpecular.xyzw," + "\n"
    	+ "                              Light#ModelDirection.xyz," + "\n"
    	+ "                              Light#Ambient.xyz," + "\n"
    	+ "                              Light#Diffuse.xyz," + "\n"
    	+ "                              Light#Specular.xyz," + "\n"
    	+ "                              Light#Attenuation.xyzw);" + "\n"
    	+ "" + "\n";
    public static String surfaceDirectional = ""
    	+ "colorSum += DirectionalLight( position.xyz," + "\n"
    	+ "                              local_normal.xyz," + "\n"
    	+ "                              CameraWorldPosition," + "\n"
    	+ "                              LocalMaterialEmissive.xyz," + "\n"
    	+ "                              LocalMaterialAmbient.xyz," + "\n"
    	+ "                              LocalMaterialDiffuse.xyzw," + "\n"
    	+ "                              LocalMaterialSpecular.xyzw," + "\n"
    	+ "                              Light#ModelDirection.xyz," + "\n"
    	+ "                              Light#Ambient.xyz," + "\n"
    	+ "                              Light#Diffuse.xyz," + "\n"
    	+ "                              Light#Specular.xyz," + "\n"
    	+ "                              Light#Attenuation.xyzw);" + "\n"
    	+ "" + "\n";

    public static String surfaceSpotStatic = ""
    	+ "colorSum += SpotLight( position.xyz," + "\n"
    	+ "                       local_normal.xyz," + "\n"
    	+ "                       CameraModelPosition," + "\n"
    	+ "                       LocalMaterialEmissive.xyz," + "\n"
    	+ "                       LocalMaterialAmbient.xyz," + "\n"
    	+ "                       LocalMaterialDiffuse.xyzw," + "\n"
    	+ "                       LocalMaterialSpecular.xyzw," + "\n"
    	+ "                       Light#ModelPosition.xyz," + "\n"
    	+ "                       Light#ModelDirection.xyz," + "\n"
    	+ "                       Light#Ambient.xyz," + "\n"
    	+ "                       Light#Diffuse.xyz," + "\n"
    	+ "                       Light#Specular.xyz," + "\n"
    	+ "                       Light#SpotCutoff.xyzw," + "\n"
    	+ "                       Light#Attenuation.xyzw);" + "\n"
    	+ "" + "\n";
    public static String surfaceSpot = ""
    	+ "colorSum += SpotLight( position.xyz," + "\n"
    	+ "                       local_normal.xyz," + "\n"
    	+ "                       CameraWorldPosition," + "\n"
    	+ "                       LocalMaterialEmissive.xyz," + "\n"
    	+ "                       LocalMaterialAmbient.xyz," + "\n"
    	+ "                       LocalMaterialDiffuse.xyzw," + "\n"
    	+ "                       LocalMaterialSpecular.xyzw," + "\n"
    	+ "                       Light#ModelPosition.xyz," + "\n"
    	+ "                       Light#ModelDirection.xyz," + "\n"
    	+ "                       Light#Ambient.xyz," + "\n"
    	+ "                       Light#Diffuse.xyz," + "\n"
    	+ "                       Light#Specular.xyz," + "\n"
    	+ "                       Light#SpotCutoff.xyzw," + "\n"
    	+ "                       Light#Attenuation.xyzw);" + "\n"
    	+ "" + "\n";


    private static String multiHistogramInitA = ""
    	+ "vec4 colorGM = texture3D(fVolumeImageA_GM,position);" + "\n"
    	+ "float fMapZ = colorGM.a;" + "\n"
    	+ "float multiHOpacityTemp = 0;" + "\n"
    	+ "float multiHOpacitySum = 0;" + "\n"
    	+ "vec4 multiHColorSum = 0;" + "\n"
    	+ "vec4 widgetColor = 0;" + "\n"
    	+ "" + "\n";

    private static String multiHistogramInitB = ""
    	+ "vec4 colorGM = texture3D(nVolumeImageB_GM,position);" + "\n"
    	+ "float fMapZ = colorGM.a;" + "\n"
    	+ "float multiHOpacityTemp = 0;" + "\n"
    	+ "float multiHOpacitySum = 0;" + "\n"
    	+ "vec4 multiHColorSum = 0;" + "\n"
    	+ "vec4 widgetColor = 0;" + "\n"
    	+ "" + "\n";

    private static String multiHistogramInitMapColor = ""
    	+ "float fMapX = dot( color.rgb, vec3(0.299, 0.587, 0.114) );" + "\n"
    	+ "float fMapY = dot( colorGM.rgb, vec3(0.299, 0.587, 0.114) );" + "\n";

    private static String multiHistogramInitMap = ""
    	+ "float fMapX = color.r;" + "\n"
    	+ "float fMapY = colorGM.r;" + "\n";
    
    private static String multiHistogramComposite = ""
    	+ "multiHOpacityTemp = computeAlpha( fMapX, fMapY, Shift#, InvY0MY1#, LevMidLine#, LevLeftLine#, LevRightLine# );" + "\n"
    	+ "widgetColor = LevColor#;" + "\n"
    	+ "" + "\n";

    private static String multiHistogramCompositeCircle = ""
    	+ "multiHOpacityTemp = computeAlphaCircle( fMapX, fMapY, Center#, LevMidLine#, Radius# );" + "\n"
    	+ "widgetColor = LevColor#;" + "\n"
    	+ "" + "\n";
    private static String multiHistogramReadColorMap = ""
        + "widgetColor = texture1D(hColorMap#, multiHOpacityTemp );" + "\n"
        + "widgetColor.a = LevColor#.a;" + "\n"
    	+ "" + "\n";
    private static String multiHistogramCompositeColorMap = ""
    	+ "multiHOpacityTemp *= (1.0 - BoundaryEmphasis# * 2.0 * (0.5 - fMapZ));" + "\n"
    	//+ "multiHOpacityTemp *= widgetColor.a;" + "\n"
    	//+ "multiHColorSum = (widgetColor * multiHOpacityTemp) + (1 - multiHOpacityTemp)*multiHColorSum;" + "\n"
    	//+ "multiHOpacitySum = multiHOpacityTemp + (1 - multiHOpacityTemp) * multiHOpacitySum;" + "\n"
    	+ "multiHColorSum += (widgetColor * multiHOpacityTemp);" + "\n"
    	+ "multiHOpacitySum += multiHOpacityTemp;" + "\n"
    	+ "Blend += (multiHOpacityTemp * LevColor#.a);" + "\n"
    	//+ "Blend += (multiHOpacityTemp);" + "\n"
    	+ "" + "\n";

    private static String multiHistogramFinish = ""
    	+ "color = multiHColorSum;" + "\n"
    	+ "opacity = multiHOpacitySum;" + "\n";
    
    
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

        //if ( m_kVolumeImageB.GetImage() == null )
        //{
            m_kPShaderCMP = new PixelShader("VolumeShaderMultiPass", createProgramText(), true );
        //}
        //else
        //{
         //   m_kPShaderCMP = new PixelShader("VolumeShaderABMultiPass");
        //}
        initTexturesVol(m_kPShaderCMP);
                 
        SetVShader(0,m_pkVShader);
        SetPShader(0,m_kPShaderCMP);
    }

    
    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
    public void Blend(float fBlend)
    {       
    	super.Blend(fBlend);
        checkPixelProgram();
    }
     */

    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void setABBlend(float fBlend)
    {
    	m_afABBlendParam[0] = fBlend;
    	if ( fBlend == 0 )
    	{
    		System.err.println( fBlend );
    	}
    	if ( fBlend == 1 )
    	{
    		System.err.println( fBlend );
    	}
        Program kCProgram = GetCProgram(0);  
        if ( (kCProgram != null) && kCProgram.GetUC("ABBlend") != null ) 
        {
            kCProgram.GetUC("ABBlend").GetData()[0] = fBlend;
        }
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


    public void updateLevWidgetState( Vector<ClassificationWidget> kLWS )
    {
    	if ( m_iUsedWidgets != kLWS.size() )
    	{
    		for ( int i = 0; i < kLWS.size(); i++ )
    		{
    			m_akLevWidget[i].Copy( kLWS.elementAt(i).getState() );
    		}
    		m_iUsedWidgets = kLWS.size();
    		m_kPShaderCMP.GetProgram().SetProgramText( createProgramText() );
    		GetCProgram(0).Release();
    		return;
    	}
    	boolean bUpdate = false;
		for ( int i = 0; i < kLWS.size(); i++ )
		{
			if ( (m_akLevWidget[i].UseColorMap[0] != kLWS.elementAt(i).getState().UseColorMap[0]) )
			{
    			m_akLevWidget[i].Copy( kLWS.elementAt(i).getState() );
    			bUpdate = true;
			}
			if ( (m_akLevWidget[i].UseWidget[0] != kLWS.elementAt(i).getState().UseWidget[0]) )
			{
    			m_akLevWidget[i].Copy( kLWS.elementAt(i).getState() );
    			bUpdate = true;
			}
			if ( (m_akLevWidget[i].InvertLUT != kLWS.elementAt(i).getState().InvertLUT) )
			{
    			m_akLevWidget[i].Copy( kLWS.elementAt(i).getState() );
    			bUpdate = true;
			}
		}
		if ( bUpdate )
		{
    		m_kPShaderCMP.GetProgram().SetProgramText( createProgramText() );
    		GetCProgram(0).Release();
    		//System.err.println( "Widget Texture Use Changed" );
    		return;
		}
    	super.updateLevWidgetState(kLWS);
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
    
    public void MULTIHISTOMode(boolean bOn)
    {
        if ( m_bMultiHisto == bOn )
        {
        	return;
        }
    	super.MULTIHISTOMode(bOn);
        checkPixelProgram();
    }

    public void SetCustumBlend(int iBlendEquation, int iLogicOp, int iSrcBlend, int iDstBlend, ColorRGBA kColor  )
    {
    	super.SetCustumBlend(iBlendEquation, iLogicOp, iSrcBlend, iDstBlend, kColor );
    	checkPixelProgram();
    }
    
    private boolean useImageA()
    {
    	return ((m_kVolumeImageA.GetImage() != null) && (m_afABBlendParam[0] != 0));
    }
    
    private boolean useImageB()
    {
    	return ((m_kVolumeImageB.GetImage() != null) && (m_afABBlendParam[0] != 1.0));
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
        if ( (m_afABBlendParam[0] != 1.0) && !m_kPShaderCMP.GetProgram().GetProgramText().contains(basicParametersB) )
        {
        	// add blend to the program:
    		bReloadShaderProgram = true;
        }
        else if ( (m_afABBlendParam[0] == 1.0) && m_kPShaderCMP.GetProgram().GetProgramText().contains(basicParametersB))
        {
        	// remove blend from the program:
    		bReloadShaderProgram = true;
        } 
        if ( (m_afABBlendParam[0] == 0.0) && m_kPShaderCMP.GetProgram().GetProgramText().contains(finalColorAB))
        {
        	// remove blend from the program:
    		bReloadShaderProgram = true;
        } 
        else if ( ((m_afABBlendParam[0] < 1.0) && (m_afABBlendParam[0] > 0.0)) && !m_kPShaderCMP.GetProgram().GetProgramText().contains(finalColorAB))
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
        if ( useImageA() && m_bGradientMag && !m_kPShaderCMP.GetProgram().GetProgramText().contains(gradientMagnitudeParametersA) )
        {
        	// add gradient magnitude to the program:
    		bReloadShaderProgram = true;
        }
        else if ( useImageA() && !m_bGradientMag && !m_bMultiHisto && m_kPShaderCMP.GetProgram().GetProgramText().contains(gradientMagnitudeParametersA))
        {
        	// remove gradient magnitude clipping from the program:
    		bReloadShaderProgram = true;
        }
        if ( useImageB() && m_bGradientMag && !m_kPShaderCMP.GetProgram().GetProgramText().contains(gradientMagnitudeParametersB) )
        {
        	// add gradient magnitude to the program:
    		bReloadShaderProgram = true;
        }
        else if ( useImageB() && !m_bGradientMag && !m_bMultiHisto && m_kPShaderCMP.GetProgram().GetProgramText().contains(gradientMagnitudeParametersB))
        {
        	// remove gradient magnitude clipping from the program:
    		bReloadShaderProgram = true;
        }
        if ( ((m_iWhichShader == SUR) || (m_iWhichShader == CMP_SUR)) && !m_kPShaderCMP.GetProgram().GetProgramText().contains(lightingParametersBasic))
        {
        	// add lighting to program
    		bReloadShaderProgram = true;
        }
        else if ( ((m_iWhichShader != SUR) && (m_iWhichShader != CMP_SUR)) && m_kPShaderCMP.GetProgram().GetProgramText().contains(lightingParametersBasic))
        {
        	// remove lighting from program
    		bReloadShaderProgram = true;
        }
        if ( (m_iWhichShader == CMP_SUR) && !m_kPShaderCMP.GetProgram().GetProgramText().contains(surfaceCompositeInit))
        {
        	// add lighting to program
    		bReloadShaderProgram = true;
        }
        else if ( (m_iWhichShader != CMP_SUR) && m_kPShaderCMP.GetProgram().GetProgramText().contains(surfaceCompositeInit))
        {
        	// remove lighting from program
    		bReloadShaderProgram = true;
        }
        
        
        if ( useImageA() && m_bMultiHisto && !m_kPShaderCMP.GetProgram().GetProgramText().contains(gradientMagnitudeParametersA))
        {
        	// add multihistogram widget to program
    		bReloadShaderProgram = true;
        }
        else if ( useImageA() && !m_bMultiHisto && m_kPShaderCMP.GetProgram().GetProgramText().contains(gradientMagnitudeParametersA))
        {
        	// remove multihistogram widget from program
    		bReloadShaderProgram = true;
        }
        if ( useImageB() && m_bMultiHisto && !m_kPShaderCMP.GetProgram().GetProgramText().contains(gradientMagnitudeParametersB))
        {
        	// add multihistogram widget to program
    		bReloadShaderProgram = true;
        }
        else if ( useImageB() && !m_bMultiHisto && m_kPShaderCMP.GetProgram().GetProgramText().contains(gradientMagnitudeParametersB))
        {
        	// remove multihistogram widget from program
    		bReloadShaderProgram = true;
        }
        if ( (m_iWhichShader == DRR || m_iWhichShader == MIP) &&  
        		!(m_kPShaderCMP.GetProgram().GetProgramText().contains(blendMIP_DRR)) ||
        		  m_kPShaderCMP.GetProgram().GetProgramText().contains(compositeMIP_DRR))
        {
    		bReloadShaderProgram = true;        	
        }
        else if ( !(m_iWhichShader == DRR || m_iWhichShader == MIP) && 
        		(m_kPShaderCMP.GetProgram().GetProgramText().contains(blendMIP_DRR)) ||
        		 m_kPShaderCMP.GetProgram().GetProgramText().contains(compositeMIP_DRR) )
        {
    		bReloadShaderProgram = true;        	
        }
        
        
        
        if ( m_kVolumeImageA.GetImage().isColorImage() && ((m_iWhichShader == SUR) || (m_iWhichShader == CMP_SUR)) )
        {
        	boolean bFoundNormals = false;
        	for ( int i = 0; i < m_kPShaderCMP.GetTextureQuantity(); i++ )
        	{
        		if ( m_kPShaderCMP.GetImageName(i).equals(m_kVolumeImageA.GetNormalMapTarget().GetName()) )
        		{
        			bFoundNormals = true;
            		break;
        		}
        	}
        	if ( !bFoundNormals )
        	{
        		bReloadShaderProgram = true;
        	}
        }
        if ( m_bGradientMag || m_bMultiHisto ) 
        {
        	boolean bFoundGM = false;
        	for ( int i = 0; i < m_kPShaderCMP.GetTextureQuantity(); i++ )
        	{
        		if ( m_kPShaderCMP.GetImageName(i).equals(m_kVolumeImageA.GetGradientMapTarget().GetName()) )
        		{
        			bFoundGM = true;
            		break;
        		}
        	}
        	if ( !bFoundGM )
        	{
        		bReloadShaderProgram = true;
        	}
        }
        
    	if ( bReloadShaderProgram )
    	{
    		m_kPShaderCMP.GetProgram().SetProgramText( createProgramText() );
    		if ( GetCProgram(0) != null )
    		{
    			GetCProgram(0).Release();
    		}
    	}
    }
    
    private String createProgramText()
    {
    	boolean bAddColorMap_TexturesA = false;
    	boolean bAddColorMapGM_TexturesA = false;
    	boolean bAddGM_TexturesA = false;
    	boolean bAddNormal_TexturesA = false;
    	

    	boolean bAddColorMap_TexturesB = false;
    	boolean bAddColorMapGM_TexturesB = false;
    	boolean bAddGM_TexturesB = false;
    	boolean bAddNormal_TexturesB = false;
    	
    	boolean bAddWidgetColorMap_Textures = false;

    	String text = "";
    	// Start Parameters:
    	// Add Helper Functions if necessary:
    	if ( (m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) )
    	{
    		// lighting helper functions:
    		text += lightingFunctions;
    	}
    	if ( m_bMultiHisto )
    	{
    		// multi-histogram helper functions:
    		text += multiHistogramFunctions;
    		// multi-histogram helper functions:
    		text += multiHistogramFunctionsCircle;
    	}
    	
    	// GLSL Program parameters:
    	text += basicParameters;
		if ( useImageB() )
		{
			text += basicParametersB;
		}
		
    	if ( !m_bMultiHisto )
    	{
    		if ( useImageA() )
    		{
    			text += colorMapA;
    			bAddColorMap_TexturesA = true;
    		}
    		if ( useImageB() )
    		{
    			text += colorMapB;
    			bAddColorMap_TexturesB = true;
    		}
    	}
    	if ( m_kVolumeImageA.GetImage().isColorImage() )
    	{
    		text += colorParameters;
    	}
    	
    	boolean bLightsOn = false;
    	if ( (m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) )
    	{
    		if ( useImageA() && m_kVolumeImageA.GetImage().isColorImage() )
    		{
    			bAddNormal_TexturesA = true;
        		text += lightingParametersBasicColorA;
    		}
    		if ( useImageB() && m_kVolumeImageB.GetImage().isColorImage() )
    		{
    			bAddNormal_TexturesB = true;
        		text += lightingParametersBasicColorB;
    		}
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
    	}
    	
    	
    	if ( m_bGradientMag || m_bMultiHisto )
    	{
    		if ( useImageA() ) {
    			text += gradientMagnitudeParametersA;
        		bAddGM_TexturesA = true;
    		}
    		if ( useImageB() ) {
    			text += gradientMagnitudeParametersB;
        		bAddGM_TexturesB = true;
    		}
        	if ( !m_bMultiHisto )
        	{
        		if ( useImageA() ) {
        			text += colorMapGMA;
        			bAddColorMapGM_TexturesA = true;
        		}
        		if ( useImageB() ) {
        			text += colorMapGMB;
        			bAddColorMapGM_TexturesB = true;
        		}
        	}
    	}
    	if ( m_bMultiHisto )
    	{
    		for ( int i = 0; i < m_iUsedWidgets; i++ )
    		{
    			if ( m_akLevWidget[i].UseWidget[0] != 0f )
    			{
    				text += multiHistogramWidgetParameters.replaceAll( "#", String.valueOf(i) );
    			}
    			if ( m_akLevWidget[i].UseColorMap[0] != -1f )
    			{
    				text += multiHistogramWidgetColorParameters.replaceAll( "#", String.valueOf(i) );
    				bAddWidgetColorMap_Textures = true;
    			}
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
    	//if ( (m_afBlendParam[0] != 1.0) )
    	{
    		text += blendParameters;
    	}
    	// End Parameters
    	
    	// add generated color code:
    	if ( useImageA() )
    	{
    		text += createColorFunctionA( bLightsOn );
    	}
    	if ( useImageB() )
    	{
    		text += createColorFunctionB( bLightsOn );
    	}
    	
    	// Start code:
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
    	// end setup:
    	
    	// generate color function:
    	if ( useImageA() )
    	{
    		text += calcColorA;
    	}
    	if ( useImageB() )
    	{
    		text += calcColorB;
    	}
    	
    	if ( useImageA() && !useImageB() )
    	{
    		text += finalColorA;
    	}
    	else if ( !useImageA() && useImageB() )
    	{
    		text += finalColorB;
    	}
    	else if ( useImageA() && useImageB() )
    	{
    		text += finalColorAB;
    	}

    	
    	// GLSL Program closing bracket: 
    	text += mainEnd;
    	// Done generating program text.
    	
    	// Add the used textures to the shader program data structures:
    	int iTex = 0;
    	if ( m_kPShaderCMP != null )
    	{
    		m_kPShaderCMP.SetImageName(iTex, m_kSceneTarget.GetName());
    		m_kPShaderCMP.SetTexture(iTex++, m_kSceneTarget);
    		if ( useImageA() )
    		{
    			m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageA.GetVolumeTarget().GetName() );
    			m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageA.GetVolumeTarget() );
    		}
    	}
    	if ( bAddColorMap_TexturesA && (m_kPShaderCMP != null))
    	{
    		m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageA.GetColorMapTarget().GetName());
    		m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageA.GetColorMapTarget());   
    	}
    	if ( bAddNormal_TexturesA && (m_kPShaderCMP != null) )
    	{
    		m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageA.GetNormalMapTarget().GetName());
    		m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageA.GetNormalMapTarget());    		
    	}
    	if ( bAddGM_TexturesA && (m_kPShaderCMP != null) )
    	{
    		m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageA.GetGradientMapTarget().GetName());
    		m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageA.GetGradientMapTarget());
        	if ( bAddColorMapGM_TexturesA && (m_kPShaderCMP != null))
        	{
        		m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageA.GetOpacityMapGMTarget().GetName() );
        		m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageA.GetOpacityMapGMTarget() );
        	}
    	}
    	if ( bAddWidgetColorMap_Textures && (m_kPShaderCMP != null) )
    	{
    		for ( int i = 0; i < m_iUsedWidgets; i++ )
    		{
    			if ( (m_akLevWidget[i].UseWidget[0] != 0f) && (m_akLevWidget[i].UseColorMap[0] != -1f) )
    			{
    				Texture kMap = VolumeTriPlanarRender.getHistogramLUTTexture( (int)m_akLevWidget[i].UseColorMap[0], false );
    				//System.err.println( iTex + " " + i + " " + kMap.GetName() );
    	    		m_kPShaderCMP.SetImageName(iTex, kMap.GetName());
    	    		m_kPShaderCMP.SetTexture(iTex++, kMap);       				
    			}
    		}    				
    	}
    	
    	
    	if ( useImageB() )
    	{
    		if ( m_kPShaderCMP != null )
    		{
    			m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageB.GetVolumeTarget().GetName() );
    			m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageB.GetVolumeTarget() );
    		}
    		if ( bAddColorMap_TexturesB && (m_kPShaderCMP != null))
    		{
    			m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageB.GetColorMapTarget().GetName());
    			m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageB.GetColorMapTarget());   
    		}
    		if ( bAddNormal_TexturesB && (m_kPShaderCMP != null) )
    		{
    			m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageB.GetNormalMapTarget().GetName());
    			m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageB.GetNormalMapTarget());    		
    		}
    		if ( bAddGM_TexturesB && (m_kPShaderCMP != null) )
    		{
    			m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageB.GetGradientMapTarget().GetName());
    			m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageB.GetGradientMapTarget());
    			if ( bAddColorMapGM_TexturesB && (m_kPShaderCMP != null))
    			{
    				m_kPShaderCMP.SetImageName(iTex, m_kVolumeImageB.GetOpacityMapGMTarget().GetName() );
    				m_kPShaderCMP.SetTexture(iTex++, m_kVolumeImageB.GetOpacityMapGMTarget() );
    			}
    		}
    	}
    	
		System.err.println( text );
    	return text;
    }
    
    private String createColorFunctionA(boolean bLightsOn)
    {
    	String text = calcColorAStart;

    	// Start color computation:
		if ( m_kVolumeImageA.GetImage().isColorImage() )
		{
			text += readImageColorA;
		}
		else
		{
			text += readImageA;
		}
		
    	if ( (m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && bLightsOn )
    	{
    		if ( m_kVolumeImageA.GetImage().isColorImage() )
    		{
    			text += surfaceInitColorA;
    		}
    		text += surfaceInit;
    	}

    	if ( m_bMultiHisto )
    	{
    		text += multiHistogramInitA;
    		if ( m_kVolumeImageA.GetImage().isColorImage() )
    		{
    			text += multiHistogramInitMapColor;
    		}
    		else
    		{
    			text += multiHistogramInitMap;
    		}
    		for ( int i = 0; i < m_iUsedWidgets; i++ )
    		{
    			if ( m_akLevWidget[i].UseWidget[0] != 0f )
    			{
    				if ( m_akLevWidget[i].Radius[0] != -1 )
    				{
    					text += multiHistogramCompositeCircle.replaceAll( "#", String.valueOf(i) );
    				}
    				else
    				{
    					text += multiHistogramComposite.replaceAll( "#", String.valueOf(i) );
    				}

    				if ( m_akLevWidget[i].UseColorMap[0] != -1f )
    				{
    					text += multiHistogramReadColorMap.replaceAll( "#", String.valueOf(i) );
    				}
    				text += multiHistogramCompositeColorMap.replaceAll( "#", String.valueOf(i) );
    			}
    		}
			text += multiHistogramFinish;    		
			if ( m_iWhichShader == SUR )
			{
				text += multiHistoSurfaceCompositeInit;
			}
    	}
    	else
    	{
    		if ( m_bGradientMag )
    		{
    			text += gradientMagnitudeCompositeA;
    		}
    		if ( m_kVolumeImageA.GetImage().isColorImage() )
    		{
    			text += readColorMapRGBA;
    		}
    		else
    		{
    			text += readColorMapA;
    		}
    		if ( m_bGradientMag )
    		{
    			text += gradientMagnitudeCompositeOpacityA;
    		}
    	}
    	
    	
    	if ( (m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && bLightsOn )
    	{
    		if ( m_iWhichShader == CMP_SUR )
    		{
    			text += surfaceCompositeInit;
    		}
    		
    		for ( int i = 0; i < m_aafLight.length; i++ )
    		{
    			if ( m_aafLight[i][0] != -1 )
    			{
    				// first light is static light:
    				if ( i == 0 )
    				{
        				switch ( (int)m_aafLight[i][0] )
        				{
        				case 0: // ambient
        					text += surfaceAmbient.replaceAll("#", String.valueOf(i) );
        					break;
        				case 1: // directional
        					text += surfaceDirectionalStatic.replaceAll("#", String.valueOf(i) );
        					break;
        				case 2: // point
        					text += surfacePointStatic.replaceAll("#", String.valueOf(i) );
        					break;
        				default: // spot
        					text += surfaceSpotStatic.replaceAll("#", String.valueOf(i) );
        					break;
        				}       	
    				}
    				else {
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
    		}
    		text += surfaceFinish;
    	}
    	// end color computation:
    	
    	// Start Blend:
    	
    	//if ( m_afBlendParam[0] != 1.0 )
    	{
    		if ( m_iWhichShader != MIP && m_iWhichShader != DRR )
    		{
    			text += blendComposite;
    		}
    		else if ( m_iWhichShader == MIP || m_iWhichShader == DRR )
    		{
    			text += blendMIP_DRR;
    		}
    	}
    	/*
    	else
    	{
    		if ( m_iWhichShader == MIP || m_iWhichShader == DRR )
    		{
    			text += compositeMIP_DRR;
    		}
    	}*/
    	// end Blend
    	text += calcColorEnd;
    	return text;
    }
    
    

    
    private String createColorFunctionB(boolean bLightsOn)
    {
    	String text = calcColorBStart;

    	// Start color computation:
		if ( m_kVolumeImageB.GetImage().isColorImage() )
		{
			text += readImageColorB;
		}
		else
		{
			text += readImageB;
		}
		
    	if ( (m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && bLightsOn )
    	{
    		if ( m_kVolumeImageB.GetImage().isColorImage() )
    		{
    			text += surfaceInitColorB;
    		}
    		text += surfaceInit;
    	}

    	if ( m_bMultiHisto )
    	{
    		text += multiHistogramInitB;
    		if ( m_kVolumeImageB.GetImage().isColorImage() )
    		{
    			text += multiHistogramInitMapColor;
    		}
    		else
    		{
    			text += multiHistogramInitMap;
    		}
    		for ( int i = 0; i < m_iUsedWidgets; i++ )
    		{
    			if ( m_akLevWidget[i].UseWidget[0] != 0f )
    			{
    				if ( m_akLevWidget[i].Radius[0] != -1 )
    				{
    					text += multiHistogramCompositeCircle.replaceAll( "#", String.valueOf(i) );
    				}
    				else
    				{
    					text += multiHistogramComposite.replaceAll( "#", String.valueOf(i) );
    				}

    				if ( m_akLevWidget[i].UseColorMap[0] != -1f )
    				{
    					text += multiHistogramReadColorMap.replaceAll( "#", String.valueOf(i) );
    				}
    				text += multiHistogramCompositeColorMap.replaceAll( "#", String.valueOf(i) );
    			}
    		}
			text += multiHistogramFinish;    		
			if ( m_iWhichShader == SUR )
			{
				text += multiHistoSurfaceCompositeInit;
			}
    	}
    	else
    	{
    		if ( m_kVolumeImageB.GetImage().isColorImage() )
    		{
    			text += readColorMapRGBB;
    		}
    		else
    		{
    			text += readColorMapB;
    		}
    		if ( m_bGradientMag )
    		{
    			text += gradientMagnitudeCompositeB;
    		}
    	}
    	
    	
    	if ( (m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && bLightsOn )
    	{
    		if ( m_iWhichShader == CMP_SUR )
    		{
    			text += surfaceCompositeInit;
    		}
    		
    		for ( int i = 0; i < m_aafLight.length; i++ )
    		{
    			if ( m_aafLight[i][0] != -1 )
    			{
    				// first light is static light:
    				if ( i == 0 )
    				{
        				switch ( (int)m_aafLight[i][0] )
        				{
        				case 0: // ambient
        					text += surfaceAmbient.replaceAll("#", String.valueOf(i) );
        					break;
        				case 1: // directional
        					text += surfaceDirectionalStatic.replaceAll("#", String.valueOf(i) );
        					break;
        				case 2: // point
        					text += surfacePointStatic.replaceAll("#", String.valueOf(i) );
        					break;
        				default: // spot
        					text += surfaceSpotStatic.replaceAll("#", String.valueOf(i) );
        					break;
        				}       	
    				}
    				else {
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
    		}
    		text += surfaceFinish;
    	}
    	// end color computation:
    	
    	// Start Blend:
    	
    	//if ( m_afBlendParam[0] != 1.0 )
    	{
    		if ( m_iWhichShader != MIP && m_iWhichShader != DRR )
    		{
    			text += blendComposite;
    		}
    		else if ( m_iWhichShader == MIP || m_iWhichShader == DRR )
    		{
    			text += blendMIP_DRR;
    		}
    	}
    	/*
    	else
    	{
    		if ( m_iWhichShader == MIP || m_iWhichShader == DRR )
    		{
    			text += compositeMIP_DRR;
    		}
    	}*/
    	// end Blend
    	text += calcColorEnd;
    	return text;
    }
    
    
    private static String multiHistogramFunctions = ""
    	+ "float computeX( float fY, float fInvY0MY1, vec4 LevLine ) {" + "\n"
    	+ "   float x0 = LevLine.x;" + "\n"
    	+ "   float y0 = LevLine.y;" + "\n"
    	+ "   float x1 = LevLine.z;" + "\n"
    	+ "   float y1 = LevLine.w;" + "\n"
    	+ "   float t = (y0 - fY) * fInvY0MY1;" + "\n"
    	+ "   float x = x0 - x0 * t + x1 * t;" + "\n"
    	+ "   return x;" + "\n"
    	+ "}" + "\n"
    	+ "" + "\n"
    	+ "float computeAlpha( float fX, float fY," + "\n"
    	+ "                    vec2  fShift, vec3  fInvY0MY1," + "\n"
    	+ "                    vec4  LevMidLine, vec4  LevLeftLine, vec4  LevRightLine ) {" + "\n"
    	+ "   if ( (fY < LevLeftLine.y) || fY > LevLeftLine.w ) {" + "\n"
    	+ "      return 0.0;" + "\n"
    	+ "   }" + "\n"
    	+ "   float xMid = LevMidLine.x;" + "\n"
    	+ "   float fShiftL = fShift.x;" + "\n"
    	+ "   float fShiftR = fShift.y;" + "\n"
    	+ "   if ( LevMidLine.y != LevMidLine.w ) {" + "\n"
    	+ "      xMid = computeX( fY, fInvY0MY1.y, LevMidLine );" + "\n"
    	+ "   }" + "\n"
    	+ "   float xLeft = computeX( fY, fInvY0MY1.x, LevLeftLine );" + "\n"
    	+ "   float xRight = computeX( fY, fInvY0MY1.z, LevRightLine );" + "\n"
    	+ "   float fAlpha = 0.0;" + "\n"
    	+ "   if ( (fX > (xMid - fShiftL)) && (fX < (xMid + fShiftR)) ) {" + "\n"
    	+ "      fAlpha = 1.0;" + "\n"
    	+ "   }" + "\n"
    	+ "   if ( (fX <= (xMid-fShiftL)) && (fX >= xLeft) ) {" + "\n"
    	+ "      fAlpha = (fX - xLeft) / ((xMid-fShiftL) - xLeft);" + "\n"
    	+ "   }" + "\n"
    	+ "   if ( (fX >= (xMid+fShiftR)) && (fX <= xRight) ) {" + "\n"
    	+ "      fAlpha = (fX - xRight) / ((xMid+fShiftR) - xRight);" + "\n"
    	+ "   }" + "\n"
    	+ "   return (fAlpha);" + "\n"
    	+ "}" + "\n";

    private static String multiHistogramFunctionsCircle = ""+ "float computeAlphaCircle( float fX," + "\n"
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
    + "        float x3 = (float)(-B-sqrt(r))/(2*A);" + "\n"
    + "        float x4 = (float)(-B+sqrt(r))/(2*A);" + "\n"
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
    + "        float x3 = (float)(-B-sqrt(r))/(2*A);	" + "\n"
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
