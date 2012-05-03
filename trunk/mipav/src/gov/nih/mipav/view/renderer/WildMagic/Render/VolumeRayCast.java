package gov.nih.mipav.view.renderer.WildMagic.Render;

import java.util.Vector;

import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetState;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget;

import javax.media.opengl.GLAutoDrawable;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.Camera;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.FrameBuffer;
import WildMagic.LibGraphics.Rendering.GlobalState;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLFrameBuffer;

/**
 * 
 * VolumeRayCast and VolumeShaderEffect implement ray-cast volume shading.
 *
 * VolumeRayCast applies a VolumeShaderEffect to the proxy geometry. The
 * VolumeShaderEffect contains the shader programs, and shader UserConstant
 * parameters needed for producing the ray-traced images. It extends the
 * ShaderEffect class in Wild Magic.  Volume rendering with proxy geometry is
 * presented in chapters 39 and 40 of GPU Gems: Programming Techniques, Tips,
 * and Tricks for Real-Time Graphics by Randima Fernando. The volume shaders
 * need to compute the start- and end-points of the ray to trace in the 3D
 * volume texture. The proxy geometry provides a way of generating this
 * information.
 * 
 * In the VolumeRayCast class, the proxy geometry is a cube with x,y,z
 * dimensions based on the ModelImage data. During every rendering pass, cube
 * proxy-geometry is rendered twice. The first rendering pass renders to an
 * off-screen buffer, creating a texture image named SceneImage, which is used
 * during the second rendering pass.
 * 
 * In the first rendering pass the cube is rendered with the vertex colors set
 * equal to the texture coordinates, and with all front-facing polygons
 * removed. The resulting texture image, SceneImage shows the back faces of
 * the cube, where the color represents the texture coordinate of that
 * pixel the cube face.
 *
 * The SceneImage texture is passed to the volume pixel-shader on the second
 * rendering pass. The front-facing polygons of the cube are rendered, and the
 * texture-coordinates for each pixel calculated in the vertex-shader. The
 * pixel-shader thus has both the texture-coordinates of the front-facing
 * polygons and the back-facing polygons and can calculate a ray through the
 * volume in texture coordinates to trace.
 */
public class VolumeRayCast extends VolumeObject
{

    /** VolumeShaderEffect applied to proxy-geometry: */
    private VolumeShaderEffectMultiPass m_kVolumeShaderEffect = null;

    /** Vertex-color shader effect used for the polylines and the first-pass
     * rendering of the proxy-geometry:*/
    private ShaderEffect m_spkVertexColor3Shader;

    /** Normalized volume extents: */
    private float m_fMax;

    /** Material properties for Volume Surface (and Composite Surface) mode*/
    private MaterialState m_kMaterial;

    /** Volume proxy-geometry (cube) */
    private TriMesh m_kMesh;
   

    /**
     * Creates a new VolumeRayCast object.
     * @param kImageA the VolumeImage containing the data and textures for
     * rendering.
     * @param kVolumeImageB second VolumeImage.
     */
    public VolumeRayCast( VolumeImage kImageA, VolumeImage kImageB )
    {
        super(kImageA,kImageB);
    }

    /**
     * Display the volume in Composite mode.
     */
    public void CMPMode()
    {
        m_kVolumeShaderEffect.CMPMode();
    }

    /**
     * Called by the init() function. Creates and initialized the scene-graph.
     * @param eFormat FrameBuffer.FormatType 
     * @param eDepth FrameBuffer.DepthType
     * @param eStencil FrameBuffer.StencilType
     * @param eBuffering FrameBuffer.BufferingType
     * @param eMultisampling FrameBuffer.MultisamplingType
     * @param iWidth canvas width
     * @param iHeight canvas height
     * @param arg0 the GLCanvas
     * @param kRenderer the OpenGLRenderer.
     */
    public void CreateScene ( )
    {            	
        // Create a scene graph with the face model as the leaf node.
        m_kScene = new Node();
        CreateBox();
        m_kScene.AttachChild( m_kMesh );
        m_kCull = new CullState();
        m_kScene.AttachGlobalState(m_kCull);

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kScene.AttachGlobalState(m_kAlpha);
        
        m_kScene.UpdateGS();
        m_kTranslate = new Vector3f( m_kScene.WorldBound.GetCenter() );
        m_kTranslate.Neg();
        m_kScene.GetChild(0).Local.SetTranslate( m_kTranslate );
    }

    /** delete local memory. */
    public void dispose(Renderer kRenderer)
    {
        if ( m_kMaterial != null )
        {
            m_kMaterial.dispose();
            m_kMaterial = null;
        }
        if ( m_kMesh != null )
        {
        	kRenderer.ReleaseVBuffer( m_kMesh.VBuffer );
        	kRenderer.ReleaseIBuffer( m_kMesh.IBuffer );
            m_kMesh.dispose();
            m_kMesh = null;
        }
        if ( m_kVolumeShaderEffect != null )
        {
        	kRenderer.ReleaseResources( m_kVolumeShaderEffect );
            m_kVolumeShaderEffect.dispose();
            m_kVolumeShaderEffect = null;
        }
        if ( m_spkVertexColor3Shader != null )
        {
        	kRenderer.ReleaseResources( m_spkVertexColor3Shader );
            m_spkVertexColor3Shader.dispose();
            m_spkVertexColor3Shader = null;
        }
        super.dispose(kRenderer);
    }

    /**
     * Display the volume in DDR mode.
     */
    public void DRRMode()
    {
        m_kVolumeShaderEffect.DRRMode();
    }

    /**
     * Return current clipping state.
     * @return current clipping state.
     */
    public VolumeClipEffect GetClipEffect()
    {
        return m_kVolumeShaderEffect;
    }

    /**
     * Called from the JPanelDisplay dialog. Gets the material properties for
     * the VolumeShaderSUR (Surface and Composite Surface volume shaders.)
     * @return material properties for the surface mode.
     */
    public MaterialState GetMaterialState( )
    {
        return m_kMaterial;
    }

    /** Returns the VolumeShaderEffect.
     * @return the VolumeShaderEffect.
     */
    public VolumeShaderEffectMultiPass GetShaderEffect()
    {
        return m_kVolumeShaderEffect;
        
    }

    /** Returns the translation vector.
     * @return the translation vector.
     */
    public Vector3f GetTranslate()
    {
        return m_kTranslate;
    }

    public Matrix4f GetWorld()
    {
        m_kScene.UpdateGS();
        return m_kMesh.HWorld;
    }
    

    /**
     * Display the volume in MIP mode.
     */
    public void MIPMode( )
    {
        m_kVolumeShaderEffect.MIPMode();
    }

    /**
     * Display the volume in Multi-histo mode.
     */
    public void MULTIHISTOMode(boolean bOn)
    {
        m_kVolumeShaderEffect.MULTIHISTOMode(bOn);
    }
           

    /** Reloads the VolumeShaderEffect current shader program.
     * @param kRenderer the OpenGLRenderer object.
     */
    public void ReloadVolumeShader( Renderer kRenderer )
    {
        m_kVolumeShaderEffect.Reload( kRenderer );
    }

    /**
     * Render the object.
     * @param kRenderer the OpenGLRenderer object.
     * @param kCuller the Culler object.
     */
    public void Render( Renderer kRenderer, Culler kCuller, boolean bPreRender, boolean bSolid )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        if ( bPreRender )
        {
            PreRender( kRenderer, kCuller );
        }
        else
        {
            m_kMesh.DetachAllEffects();
            m_kMesh.AttachEffect( m_kVolumeShaderEffect );
            kCuller.ComputeVisibleSet(m_kScene);        
            kRenderer.Draw(m_kMesh);        

            // Draw scene polygon:
            //kRenderer.SetCamera(m_spkScreenCamera);
            //kRenderer.Draw(m_spkScenePolygon);
        }
    }

    public void releaseShaderEffect( Renderer kRenderer )
    {
    	if ( m_kVolumeShaderEffect != null )
    	{
            m_kMesh.DetachAllEffects();
    		kRenderer.ReleaseResources( m_kMesh );
    		kRenderer.ReleaseResources( m_kVolumeShaderEffect );
    	}
    }

    public void recreateShaderEffect( Renderer kRenderer, Texture targetTexture )
    {
    	//if ( m_kVolumeImageB.GetImage() != null )
    	{
    		//m_kVolumeShaderEffect = new VolumeShaderEffectMultiPass( m_kVolumeImageA, m_kVolumeImageB,
            //    targetTexture);
    	}
    	//else
    	{    		
    		m_kVolumeShaderEffect = new VolumeShaderEffectMultiPassDynamic( m_kVolumeImageA, m_kVolumeImageB,
    				targetTexture);
    	}
        kRenderer.LoadResources(m_kVolumeShaderEffect);
        kRenderer.LoadResources(m_kMesh);
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
        Vector3f kLength = new Vector3f( m_fX, m_fY, m_fZ );
        m_kVolumeShaderEffect.setMaxLength( kLength.Length() );
    }
    
    /**
     * Enables/Disables self-shadowing in the Surface mode.
     * @param bShadow shadow on/off.
     */
    public void SelfShadow(boolean bShadow)
    {
        m_kVolumeShaderEffect.SelfShadow(bShadow);
    }

    /**
     * Sets blending between imageA and imageB.
     * @param fValue the blend value (0-1)
     */
    public void setABBlend( float fValue )
    {
        m_kVolumeShaderEffect.setABBlend(fValue);
    }



    /**
     * Sets the background color.
     * @param kColor new background color.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        m_kVolumeShaderEffect.SetBackgroundColor( kColor );
    }    
    
    /** Sets axis-aligned clipping for the VolumeShaderEffect.
     * @param afClip the clipping parameters for axis-aligned clipping.
     */
    public void SetClip( int iWhich, float data, boolean bEnable)
    {
        m_kVolumeShaderEffect.SetClip(iWhich, data, bEnable);
    }
    
    /** Sets arbitrary clipping for the VolumeShaderEffect.
     * @param afEquation the arbitrary-clip plane equation.
     */
    public void SetClipArb( float[] afEquation, boolean bEnable )
    {
        m_kVolumeShaderEffect.SetClipArb(afEquation, bEnable);
    }

    /** Sets eye clipping for the VolumeShaderEffect.
     * @param afEquation the eye clipping equation.
     */
    public void SetClipEye( float[] afEquation, boolean bEnable )
    {
        m_kVolumeShaderEffect.SetClipEye(afEquation, bEnable);
    }

    /** Sets inverse-eye clipping for the VolumeShaderEffect.
     * @param afEquation the inverse-eye clipping equation.
     */
    public void SetClipEyeInv( float[] afEquation, boolean bEnable )
    {
        m_kVolumeShaderEffect.SetClipEyeInv(afEquation, bEnable);
    }
    
    public void SetCustumBlend(int iBlendEquation, int iLogicOp, int iSrcBlend, int iDstBlend, ColorRGBA kColor  )
    {
        m_kVolumeShaderEffect.SetCustumBlend( iBlendEquation, iLogicOp, iSrcBlend, iDstBlend, kColor );
    }

    /**
     * Enables/Disables Gradient Magnitude filter.
     * @param bShow gradient magnitude filter on/off
     */
    public void SetGradientMagnitude(boolean bShow)
    {
        m_kVolumeShaderEffect.SetGradientMagnitude(bShow);
    }

    /** Sets lighting in the VolumeShaderEffect.
     * @param kLightType name of the light to set.
     * @param afType the type of light to set.
     */
    public void SetLight( String kLightType, float[] afType )
    {
        m_kVolumeShaderEffect.SetLight(kLightType, afType);
    }
    /**
     * Called from the AdvancedMaterialProperties dialog. Sets the material
     * properties for the VolumeShaderSUR (Surface and Composite Surface
     * volume shaders.)
     * @param kMaterial new material properties for the surface mode.
     */
    public void SetMaterialState( MaterialState kMaterial )
    {
        m_kMesh.DetachGlobalState(GlobalState.StateType.MATERIAL);
        m_kMaterial = kMaterial;
        m_kMesh.AttachGlobalState(m_kMaterial);
        m_kMesh.UpdateMS(true);
        m_kMesh.UpdateRS();
    }


    public void setRGBTA(ModelRGB RGBT) {
        m_kVolumeShaderEffect.setRGBTA(RGBT);
    }
    public void setRGBTB(ModelRGB RGBT) {
        m_kVolumeShaderEffect.setRGBTB(RGBT);
    }
    
    /** Sets the blend factor for displaying the ray-cast volume with other objects in the scene.
     * @param fBlend the blend factor for the ray-cast volume.
     */
    public void setVolumeBlend( float fBlend )
    {
        if ( m_kVolumeShaderEffect != null )
        {
            m_kVolumeShaderEffect.Blend(fBlend);
        }
    }
    public int setVolumeSamples( float fSample )
    {
        boolean bTemp = m_bDisplay;
        m_bDisplay = false;
        int samples = m_kVolumeShaderEffect.setVolumeSamples( fSample );
        m_bDisplay = bTemp;
        return samples;
    }
    /**
     * Display the volume in Surface mode.
     */
    public void SURFASTMode()
    {
        m_kVolumeShaderEffect.SURFASTMode();
    }
    /**
     * Display the volume in Composite Surface mode.
     */
    public void SURMode()
    {
        m_kVolumeShaderEffect.SURMode();
    }
    
    public void updateLevWidgetState( Vector<ClassificationWidget> kLWS )
    {
        m_kVolumeShaderEffect.updateLevWidgetState( kLWS );
    }


    
    /**
     * Called by CreateBox. Creates the bounding-box proxy geometry (VertexBuffer, IndexBuffer).
     * @param iXBound image x-extent.
     * @param iYBound image y-extent.
     * @param iZBound image z-extent.
     * @return TriMesh, new geometry.
     */
    private TriMesh Box (int iXBound, int iYBound, int iZBound)
    {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        kAttr.SetTChannels(0,3);
        kAttr.SetTChannels(1,3);

        float fMaxX = (iXBound - 1) * m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[0];
        float fMaxY = (iYBound - 1) * m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[1];
        float fMaxZ = (iZBound - 1) * m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[2];

        m_fMax = fMaxX;
        if (fMaxY > m_fMax) {
            m_fMax = fMaxY;
        }
        if (fMaxZ > m_fMax) {
            m_fMax = fMaxZ;
        }
        m_fX = fMaxX/m_fMax;
        m_fY = fMaxY/m_fMax;
        m_fZ = fMaxZ/m_fMax;

        int iVQuantity = 24;
        int iTQuantity = 12;
        VertexBuffer pkVB = new VertexBuffer(kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate connectivity (outside view)
        int i = 0;
        int[] aiIndex = pkIB.GetData();

        // generate geometry
        // front
        pkVB.SetPosition3(0,0,0,0);
        pkVB.SetPosition3(1,m_fX,0,0);
        pkVB.SetPosition3(2,m_fX,m_fY,0);
        pkVB.SetPosition3(3,0,m_fY,0);
        pkVB.SetColor3(0,0,0,0,0);
        pkVB.SetColor3(0,1,1,0,0);
        pkVB.SetColor3(0,2,1,1,0);
        pkVB.SetColor3(0,3,0,1,0);
        aiIndex[i++] = 0;  aiIndex[i++] = 2;  aiIndex[i++] = 1;
        aiIndex[i++] = 0;  aiIndex[i++] = 3;  aiIndex[i++] = 2;

        // back
        pkVB.SetPosition3(4,0,0,m_fZ);
        pkVB.SetPosition3(5,m_fX,0,m_fZ);
        pkVB.SetPosition3(6,m_fX,m_fY,m_fZ);
        pkVB.SetPosition3(7,0,m_fY,m_fZ);
        pkVB.SetColor3(0,4,0,0,1);
        pkVB.SetColor3(0,5,1,0,1);
        pkVB.SetColor3(0,6,1,1,1);
        pkVB.SetColor3(0,7,0,1,1);
        aiIndex[i++] = 4;  aiIndex[i++] = 5;  aiIndex[i++] = 6;
        aiIndex[i++] = 4;  aiIndex[i++] = 6;  aiIndex[i++] = 7;

        // top
        pkVB.SetPosition3(8,0,m_fY,0);
        pkVB.SetPosition3(9,m_fX,m_fY,0);
        pkVB.SetPosition3(10,m_fX,m_fY,m_fZ);
        pkVB.SetPosition3(11,0,m_fY,m_fZ);
        pkVB.SetColor3(0,8,0,1,0);
        pkVB.SetColor3(0,9,1,1,0);
        pkVB.SetColor3(0,10,1,1,1);
        pkVB.SetColor3(0,11,0,1,1);
        aiIndex[i++] = 8;  aiIndex[i++] = 10;  aiIndex[i++] = 9;
        aiIndex[i++] = 8;  aiIndex[i++] = 11;  aiIndex[i++] = 10;

        // bottom
        pkVB.SetPosition3(12,0,0,0);
        pkVB.SetPosition3(13,m_fX,0,0);
        pkVB.SetPosition3(14,m_fX,0,m_fZ);
        pkVB.SetPosition3(15,0,0,m_fZ);
        pkVB.SetColor3(0,12,0,0,0);
        pkVB.SetColor3(0,13,1,0,0);
        pkVB.SetColor3(0,14,1,0,1);
        pkVB.SetColor3(0,15,0,0,1);
        aiIndex[i++] = 12;  aiIndex[i++] = 13;  aiIndex[i++] = 14;
        aiIndex[i++] = 12;  aiIndex[i++] = 14;  aiIndex[i++] = 15;

        // right
        pkVB.SetPosition3(16,m_fX,0,0);
        pkVB.SetPosition3(17,m_fX,m_fY,0);
        pkVB.SetPosition3(18,m_fX,m_fY,m_fZ);
        pkVB.SetPosition3(19,m_fX,0,m_fZ);
        pkVB.SetColor3(0,16,1,0,0);
        pkVB.SetColor3(0,17,1,1,0);
        pkVB.SetColor3(0,18,1,1,1);
        pkVB.SetColor3(0,19,1,0,1);
        aiIndex[i++] = 16;  aiIndex[i++] = 17;  aiIndex[i++] = 18;
        aiIndex[i++] = 16;  aiIndex[i++] = 18;  aiIndex[i++] = 19;

        // left
        pkVB.SetPosition3(20,0,0,0);
        pkVB.SetPosition3(21,0,m_fY,0);
        pkVB.SetPosition3(22,0,m_fY,m_fZ);
        pkVB.SetPosition3(23,0,0,m_fZ);
        pkVB.SetColor3(0,20,0,0,0);
        pkVB.SetColor3(0,21,0,1,0);
        pkVB.SetColor3(0,22,0,1,1);
        pkVB.SetColor3(0,23,0,0,1);
        aiIndex[i++] = 20;  aiIndex[i++] = 22;  aiIndex[i++] = 21;
        aiIndex[i++] = 20;  aiIndex[i++] = 23;  aiIndex[i++] = 22;

        if (kAttr.GetMaxTCoords() > 0)
        {
            for (int iUnit = 0; iUnit < kAttr.GetMaxTCoords(); iUnit++)
            {
                if (kAttr.HasTCoord(iUnit))
                {
                    pkVB.SetTCoord3(iUnit,0,0,0,0);
                    pkVB.SetTCoord3(iUnit,1,1,0,0);
                    pkVB.SetTCoord3(iUnit,2,1,1,0);
                    pkVB.SetTCoord3(iUnit,3,0,1,0);

                    pkVB.SetTCoord3(iUnit,4,0,0,1);
                    pkVB.SetTCoord3(iUnit,5,1,0,1);
                    pkVB.SetTCoord3(iUnit,6,1,1,1);
                    pkVB.SetTCoord3(iUnit,7,0,1,1);

                    pkVB.SetTCoord3(iUnit,8,0,1,0);
                    pkVB.SetTCoord3(iUnit,9,1,1,0);
                    pkVB.SetTCoord3(iUnit,10,1,1,1);
                    pkVB.SetTCoord3(iUnit,11,0,1,1);

                    pkVB.SetTCoord3(iUnit,12,0,0,0);
                    pkVB.SetTCoord3(iUnit,13,1,0,0);
                    pkVB.SetTCoord3(iUnit,14,1,0,1);
                    pkVB.SetTCoord3(iUnit,15,0,0,1);

                    pkVB.SetTCoord3(iUnit,16,1,0,0);
                    pkVB.SetTCoord3(iUnit,17,1,1,0);
                    pkVB.SetTCoord3(iUnit,18,1,1,1);
                    pkVB.SetTCoord3(iUnit,19,1,0,1);

                    pkVB.SetTCoord3(iUnit,20,0,0,0);
                    pkVB.SetTCoord3(iUnit,21,0,1,0);
                    pkVB.SetTCoord3(iUnit,22,0,1,1);
                    pkVB.SetTCoord3(iUnit,23,0,0,1);
                }
            }
        }
        m_kMesh = new TriMesh(pkVB,pkIB);
        m_kMaterial = new MaterialState();
        m_kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        m_kMaterial.Ambient = new ColorRGB(0.1f,0.1f,0.1f);
        m_kMaterial.Diffuse = new ColorRGB(1f,1f,1f);
        m_kMaterial.Specular = new ColorRGB(1f,1f,1f);
        m_kMaterial.Shininess = 128f;
        m_kMesh.AttachGlobalState(m_kMaterial);
        m_kMesh.UpdateMS(true);
        return m_kMesh;
    }
    
    
    /**
     * Called by CreateScene. Creates the bounding-box proxy geometry scene
     * node.
     */
    private void CreateBox ()
    {
        int iXBound = m_kVolumeImageA.GetImage().getExtents()[0];
        int iYBound = m_kVolumeImageA.GetImage().getExtents()[1];
        int iZBound = m_kVolumeImageA.GetImage().getExtents()[2];
        Box(iXBound,iYBound,iZBound);
        m_spkVertexColor3Shader = new VertexColor3Effect();
    }
    
    /** 
     * PreRender renders the proxy geometry into the PBuffer texture.
     * @param kRenderer the OpenGLRenderer object.
     * @param kCuller the Culler object.
     */
    private void PreRender( Renderer kRenderer, Culler kCuller )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        m_kScene.UpdateGS();
        
        // First rendering pass:
        // Draw the proxy geometry to a color buffer, to generate the
        // back-facing texture-coordinates:
        m_kMesh.DetachAllEffects();
        m_kMesh.AttachEffect( m_spkVertexColor3Shader );
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.SetBackgroundColor(ColorRGBA.BLACK);
        kRenderer.ClearBuffers();

        kCuller.ComputeVisibleSet(m_kScene);

        // Cull front-facing polygons:
        m_kCull.CullFace = CullState.CullMode.CT_FRONT;
        kRenderer.DrawScene(kCuller.GetVisibleSet());
        // Undo culling:
        m_kCull.CullFace = CullState.CullMode.CT_BACK;
        
    }
}
