package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget;

import java.util.Vector;

import WildMagic.LibFoundation.Intersection.IntrSegment3Plane3f;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.GlobalState;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

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
/**
 * @author alexa
 *
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

    private final int numPoints = 8;
    private Vector3f centerBox;
	private boolean[] clippedPoints = new boolean[numPoints];
    private Vector3f[] cornerPoints = new Vector3f[numPoints];
    private Vector3f[] rotatedPoints = new Vector3f[numPoints];
    private int[] reorderedList = new int[numPoints];

    private final int frontBottomLeftIndex = 0;
    private final int frontBottomRightIndex = 1;
    private final int frontTopRightIndex = 2;        
    private final int frontTopLeftIndex = 3;

    private final int backBottomLeftIndex = 4;
    private final int backBottomRightIndex = 5;
    private final int backTopRightIndex = 6;
    private final int backTopLeftIndex = 7;
    
    private Vector3f frontBottomLeft;
    private Vector3f frontBottomRight;
    private Vector3f frontTopRight;        
    private Vector3f frontTopLeft;

    private Vector3f backBottomLeft;
    private Vector3f backBottomRight;
    private Vector3f backTopRight;
    private Vector3f backTopLeft;

    private Plane3f m_kCullPlane;
    private float planeConstant = 0.005f;
    private Renderer m_kRenderer;

    private Matrix4f kWorldInv;

    private Vector<Vector3f> topFace = new Vector<Vector3f>();
    private Vector<Vector3f> bottomFace = new Vector<Vector3f>();

    private Vector<Vector3f> leftFace = new Vector<Vector3f>();
    private Vector<Vector3f> rightFace = new Vector<Vector3f>();

    private Vector<Vector3f> frontFace = new Vector<Vector3f>();    
    private Vector<Vector3f> backFace = new Vector<Vector3f>();
    
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
     * The volume data is rendered with a fragment (pixel) shader on the GPU. The shader is activated by rendering a volume proxy geometry, 
     * which is a cube that matches the volume bounding box.  One problem with this technique for rendering volume data is that when the 
     * bounding-volume proxy geometry is clipped by the view-frustum near-plane the geometry is no longer rendered on the GPU and 
     * therefore the fragment shader is no longer activated. This causes the volume rendering to disappear as the user tries to zoom into it. 

     * This function automatically detects clipping of the volume proxy-geometry by the view-frustum near-plane as the 
     * camera moves into the volume.  When clipping is detected, the algorithm generates a new triangle mesh by intersecting 
     * the near-plane with the original bounding-cube.  The new triangle mesh serves as the volume proxy geometry. 
     * The result is the appearance of moving inside the volume as the user zooms the camera.
     * 
     * The default cube is always the starting point for the algorithm, even if a non-cube shape is currently 
     * displayed as the volume proxy geometry.
     * 
     * @param kRenderer
     * @param kCuller
     */
    public void CheckViewIntersection(Renderer kRenderer, Culler kCuller)
    {   	    	
    	m_kRenderer = kRenderer;
    	/** Default clip plane */
    	m_kCullPlane = new Plane3f(kCuller.GetPlanes()[0]);
    	m_kCullPlane.Constant += planeConstant;
    	m_kCullPlane.Normal = new Vector3f(0,0,1);
    	    	
    	// Get the camera view-transformation -- only use the rotation.
    	// The camera translation is used in the default clip plane constant.
    	float[] afData = new float[16];
    	kRenderer.SetConstantVMatrix (0, afData);
    	Matrix4f kV = new Matrix4f( );

    	kV.M00 = afData[0];
    	kV.M01 = afData[1];
    	kV.M02 = afData[2];
    	kV.M03 = afData[3];
    	kV.M10 = afData[4];
    	kV.M11 = afData[5];
    	kV.M12 = afData[6];
    	kV.M13 = afData[7];
    	kV.M20 = afData[8];
    	kV.M21 = afData[9];
    	kV.M22 = afData[10];
    	kV.M23 = afData[11];
    	kV.M30 = 0;
    	kV.M31 = 0;
    	kV.M32 = 0;
    	kV.M33 = 1;

    	// Get the model transformations (rotation and translation)
    	Matrix4f kWorld = Matrix4f.mult(GetWorld(), kV);
    	kWorldInv = Matrix4f.inverse(kWorld);
    	Vector4f vtemp = new Vector4f();
    	
		int clippedCount = 0;
		float[] clippedDistance = new float[numPoints];
    	for ( int i = 0; i < cornerPoints.length; i++ )
    	{
    		// Apply the model-view transformation to the original
    		// corners of the cube:
    		Vector3f temp = cornerPoints[i];

    		vtemp.X = temp.X;
    		vtemp.Y = temp.Y;
    		vtemp.Z = temp.Z;
    		vtemp.W = 1f;
    		Vector4f resultW = kWorld.multLeft( vtemp );

			clippedPoints[i] = false;
			clippedDistance[i] = resultW.Z;

			rotatedPoints[i] = new Vector3f( resultW.X, resultW.Y, resultW.Z );
			// Check if the transformed point is clipped with the default clip plane:
    		if ( clippedDistance[i] < m_kCullPlane.Constant )
    		{
    			clippedPoints[i] = true;
    			clippedCount++;
    		}
    	}
    	if ( (clippedCount > 0) && (clippedCount < 8) )
    	{
    		// If at least one corners is clipped 
    		// but not all corners are clipped:
    		cullPoints( clippedDistance );
    	}
    	else
    	{
    		// Otherwise test that the current proxy-geometry is the default cube,
    		// if not reload it onto the GPU:
    		checkMeshPoints();
    	}
    	m_kRenderer = null;
    }

    /**
     * Set the front the clipping plane constant
     * @param _constant  contant between 0 to 0.01;
     */
    public void setPlaneConstant(float _constant) {
    	planeConstant = _constant;
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

        m_kCull = new CullState();
        //m_kCull.CullFace = CullState.CullMode.CT_FRONT;
        m_kCull.FrontFace = CullState.FrontMode.FT_CCW;
        m_kScene.AttachGlobalState(m_kCull);
        
        CreateBox();
        m_kMesh.SetName(new String("RayCastVolume"));
        m_kScene.AttachChild( m_kMesh );

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kScene.AttachGlobalState(m_kAlpha);
        
        m_kScene.UpdateGS();
        m_kTranslate = new Vector3f( m_kScene.WorldBound.GetCenter() );
        m_kTranslate.neg();
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
        	kRenderer.ReleaseVAO( m_kMesh );
        	//kRenderer.ReleaseVBuffer( m_kMesh.VBuffer );
        	//kRenderer.ReleaseIBuffer( m_kMesh.IBuffer );
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
	public boolean GetGradientMagnitude()
    {
    	return m_kVolumeShaderEffect.GetGradientMagnitude();
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

    public void printProgram()
    {
    	if ( m_kVolumeShaderEffect != null )
    	{
    		m_kVolumeShaderEffect.printProgram();
    	}
    }

    public void recreateShaderEffect( Renderer kRenderer, Texture targetTexture )
    {
    	m_kVolumeShaderEffect = new VolumeShaderEffectMultiPassDynamic( m_kVolumeImageA, m_kVolumeImageB, targetTexture);
    	m_kVolumeShaderEffect.LoadResources(kRenderer,m_kMesh);
    	kRenderer.LoadResources(m_kMesh);
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
        Vector3f kLength = new Vector3f( m_fX, m_fY, m_fZ );
        m_kVolumeShaderEffect.setMaxLength( kLength.length() );
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

    public void SetCustomBlend(int iBlendEquation, int iLogicOp, int iSrcBlend, int iDstBlend, ColorRGBA kColor  )
    {
        m_kVolumeShaderEffect.SetCustomBlend( iBlendEquation, iLogicOp, iSrcBlend, iDstBlend, kColor );
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

        int iVQuantity = 8;
        int iTQuantity = 12;
        VertexBuffer pkVB = new VertexBuffer(kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate geometry
        frontBottomLeft = new Vector3f(0f,0f,0f);
        frontBottomRight = new Vector3f(m_fX,0f,0f);
        frontTopRight = new Vector3f(m_fX,m_fY,0f);        
        frontTopLeft = new Vector3f(0f,m_fY,0f);

        backBottomLeft = new Vector3f(0f,0f,m_fZ);
        backBottomRight = new Vector3f(m_fX,0f,m_fZ);
        backTopRight = new Vector3f(m_fX,m_fY,m_fZ);
        backTopLeft = new Vector3f(0f,m_fY,m_fZ);

        cornerPoints[frontBottomLeftIndex] = frontBottomLeft;
        cornerPoints[frontBottomRightIndex] = frontBottomRight;
        cornerPoints[frontTopRightIndex] = frontTopRight;
        cornerPoints[frontTopLeftIndex] = frontTopLeft;
        cornerPoints[backBottomLeftIndex] = backBottomLeft;
        cornerPoints[backBottomRightIndex] = backBottomRight;
        cornerPoints[backTopRightIndex] = backTopRight;
        cornerPoints[backTopLeftIndex] = backTopLeft;
        
        centerBox = new Vector3f();
        for ( int i = 0; i < cornerPoints.length; i++ )
        {
        	centerBox.add(cornerPoints[i]);
        }
        centerBox.scale(1f/(cornerPoints.length));

        fillBox( pkVB );
        fillBox( pkIB );
        
        m_kMesh = new TriMesh(pkVB,pkIB);
        m_kMesh.VBuffer.SetName("BOX");
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
     * Check that the currently loaded proxy geometry is the default cube, if it is not
     * reload it onto the GPU.
     */
    private void checkMeshPoints()
    {
    	boolean reLoad = false;
    	if ( m_kMesh.VBuffer.GetVertexQuantity() != 8 )
    	{
    		reLoad = true;
    	}
    	else
    	{
    		for ( int i = 0; i < cornerPoints.length; i++ )
    		{
    			if ( !m_kMesh.VBuffer.GetPosition3(i).equals( cornerPoints[i]  ) )
    			{
    				reLoad = true;
    				break;
    			}
    		}
    	}
    	if ( reLoad )
    	{
            VertexBuffer pkVB = new VertexBuffer( m_kMesh.VBuffer.GetAttributes(), 8);
            IndexBuffer pkIB = new IndexBuffer(3*12);
            fillBox( pkVB );
            fillBox( pkIB );
    		m_kMesh.Release(m_kRenderer);
    		m_kMesh.VBuffer = pkVB;
    		m_kMesh.IBuffer = pkIB;
    	}
    }

   
    /**
     *  Compute the cube-plane intersection. Remove clipped points from each face
     *  of the cube that intersects the clipping-plane. Add the intersection points
     *  to clipped faces. This is done while maintaining the counter-clockwise ordering of
     *  the points on the cube faces.
     */
    private void computeIntersections(  ) 
    {
    	// Create top face list, using the re-mapped corners of the cube.
    	// This means the corner in position 0 is the furtherest clipped point:
    	topFace.clear();
    	topFace.add( rotatedPoints[reorderedList[0]] );
    	topFace.add( rotatedPoints[reorderedList[1]] );
    	topFace.add( rotatedPoints[reorderedList[4]] );
    	topFace.add( rotatedPoints[reorderedList[3]] );

    	// Create bottom face list, using the re-mapped corners of the cube.
    	bottomFace.clear();
    	bottomFace.add( rotatedPoints[reorderedList[5]] );
    	bottomFace.add( rotatedPoints[reorderedList[2]] );
    	bottomFace.add( rotatedPoints[reorderedList[6]] );
    	bottomFace.add( rotatedPoints[reorderedList[7]] );

    	// Create left face list, using the re-mapped corners of the cube.
    	leftFace.clear();
    	leftFace.add( rotatedPoints[reorderedList[3]] );
    	leftFace.add( rotatedPoints[reorderedList[4]] );
    	leftFace.add( rotatedPoints[reorderedList[7]] );
    	leftFace.add( rotatedPoints[reorderedList[6]] );

    	// Create right face list, using the re-mapped corners of the cube.
    	rightFace.clear();
    	rightFace.add( rotatedPoints[reorderedList[0]] );
    	rightFace.add( rotatedPoints[reorderedList[2]] );
    	rightFace.add( rotatedPoints[reorderedList[5]] );
    	rightFace.add( rotatedPoints[reorderedList[1]] );

    	// Create front face list, using the re-mapped corners of the cube.
    	frontFace.clear();
    	frontFace.add( rotatedPoints[reorderedList[0]] );
    	frontFace.add( rotatedPoints[reorderedList[3]] );
    	frontFace.add( rotatedPoints[reorderedList[6]] );
    	frontFace.add( rotatedPoints[reorderedList[2]] );

    	// Create back face list, using the re-mapped corners of the cube.
    	backFace.clear();
    	backFace.add( rotatedPoints[reorderedList[1]] );
    	backFace.add( rotatedPoints[reorderedList[5]] );
    	backFace.add( rotatedPoints[reorderedList[7]] );
    	backFace.add( rotatedPoints[reorderedList[4]] );

    	// keep track of points that are clipped from the faces:
        Vector<Vector3f> clippedList = new Vector<Vector3f>();  
        // keep track of the new points for generating the new polygon:
    	Vector<Vector3f> intersectionPoints = new Vector<Vector3f>();
    	
    	// Test Edge 0-1:
    	Segment3f segment = new Segment3f( rotatedPoints[reorderedList[0]], rotatedPoints[reorderedList[1]] );
    	IntrSegment3Plane3f intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
    	if ( intersection.Find() )
    	{
    		// add intersection point:
    		Vector3f intersectionPoint = new Vector3f();
    		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
    		intersectionPoints.add(intersectionPoint);
    		
    		topFace.add( topFace.indexOf(rotatedPoints[reorderedList[0]]) + 1, intersectionPoint );
    		rightFace.add( intersectionPoint );
    		
    		clippedList.add( rotatedPoints[reorderedList[0]] );
    	}
    	else
    	{
    		// Test Edge 1-4:
    		segment = new Segment3f( rotatedPoints[reorderedList[1]], rotatedPoints[reorderedList[4]] );
    		intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
        	if ( intersection.Find() )
        	{
        		// add intersection point:
        		Vector3f intersectionPoint = new Vector3f();
        		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
        		intersectionPoints.add(intersectionPoint);
        		
        		topFace.add( topFace.indexOf(rotatedPoints[reorderedList[1]]) + 1, intersectionPoint );
        		backFace.add( intersectionPoint );

        		clippedList.add( rotatedPoints[reorderedList[0]] );
        		clippedList.add( rotatedPoints[reorderedList[1]] );
        	}
        	else
        	{
        		// Test Edge 4-7
        		segment = new Segment3f( rotatedPoints[reorderedList[4]], rotatedPoints[reorderedList[7]] );
        		intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
            	if ( intersection.Find() )
            	{
            		// add intersection point:
            		Vector3f intersectionPoint = new Vector3f();
            		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
            		intersectionPoints.add(intersectionPoint);

            		leftFace.add( leftFace.indexOf( rotatedPoints[reorderedList[4]] ) + 1, intersectionPoint);
            		backFace.add( backFace.indexOf( rotatedPoints[reorderedList[7]] ) + 1, intersectionPoint);

            		clippedList.add( rotatedPoints[reorderedList[0]] );
            		clippedList.add( rotatedPoints[reorderedList[1]] );
            		clippedList.add( rotatedPoints[reorderedList[4]] );
            	}
        	}
    	}
    	// Test Edge 1-5:
    	segment = new Segment3f( rotatedPoints[reorderedList[1]], rotatedPoints[reorderedList[5]] );
    	intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
    	if ( intersection.Find() )
    	{
    		// add intersection point:
    		Vector3f intersectionPoint = new Vector3f();
    		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
    		intersectionPoints.add(intersectionPoint);
    		
    		rightFace.add( rightFace.indexOf( rotatedPoints[reorderedList[5]] ) + 1, intersectionPoint );
    		backFace.add( backFace.indexOf( rotatedPoints[reorderedList[1]] ) + 1, intersectionPoint );

    		clippedList.add( rotatedPoints[reorderedList[0]] );
    		clippedList.add( rotatedPoints[reorderedList[1]] );
    	}
    	// Test Edge 0-2:
    	segment = new Segment3f( rotatedPoints[reorderedList[0]], rotatedPoints[reorderedList[2]] );
    	intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
    	if ( intersection.Find() )
    	{
    		// add intersection point:
    		Vector3f intersectionPoint = new Vector3f();
    		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
    		intersectionPoints.add(intersectionPoint);

    		frontFace.add( intersectionPoint );
    		rightFace.add( rightFace.indexOf( rotatedPoints[reorderedList[0]] ) + 1, intersectionPoint );
    		
    		clippedList.add( rotatedPoints[reorderedList[0]] );
    	}
    	else
    	{
        	// Test Edge 2-5:
        	segment = new Segment3f( rotatedPoints[reorderedList[2]], rotatedPoints[reorderedList[5]] );
        	intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
        	if ( intersection.Find() )
        	{
        		// add intersection point:
        		Vector3f intersectionPoint = new Vector3f();
        		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
        		intersectionPoints.add(intersectionPoint);

        		bottomFace.add( bottomFace.indexOf( rotatedPoints[reorderedList[5]] ) + 1, intersectionPoint );
        		rightFace.add( rightFace.indexOf( rotatedPoints[reorderedList[2]] ) + 1, intersectionPoint );

        		clippedList.add( rotatedPoints[reorderedList[0]] );
        		clippedList.add( rotatedPoints[reorderedList[2]] );
        	}
        	else
        	{
            	// Test Edge 5-7:
            	segment = new Segment3f( rotatedPoints[reorderedList[5]], rotatedPoints[reorderedList[7]] );
            	intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
            	if ( intersection.Find() )
            	{
            		// add intersection point:
            		Vector3f intersectionPoint = new Vector3f();
            		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
            		intersectionPoints.add(intersectionPoint);

            		bottomFace.add( intersectionPoint );
            		backFace.add( backFace.indexOf( rotatedPoints[reorderedList[5]] ) + 1, intersectionPoint );

            		clippedList.add( rotatedPoints[reorderedList[0]] );
            		clippedList.add( rotatedPoints[reorderedList[2]] );
            		clippedList.add( rotatedPoints[reorderedList[5]] );
            	}           	        		
        	}
    	}
    	// Test Edge 2-6:
    	segment = new Segment3f( rotatedPoints[reorderedList[2]], rotatedPoints[reorderedList[6]] );
    	intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
    	if ( intersection.Find() )
    	{
    		// add intersection point:
    		Vector3f intersectionPoint = new Vector3f();
    		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
    		intersectionPoints.add(intersectionPoint);

    		frontFace.add( frontFace.indexOf( rotatedPoints[reorderedList[6]] ) + 1, intersectionPoint );
    		bottomFace.add( bottomFace.indexOf( rotatedPoints[reorderedList[2]] ) + 1, intersectionPoint );

    		clippedList.add( rotatedPoints[reorderedList[0]] );
    		clippedList.add( rotatedPoints[reorderedList[2]] );
    	}
    	// Test Edge 0-3:
    	segment = new Segment3f( rotatedPoints[reorderedList[0]], rotatedPoints[reorderedList[3]] );
    	intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
    	if ( intersection.Find() )
    	{
    		// add intersection point:
    		Vector3f intersectionPoint = new Vector3f();
    		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
    		intersectionPoints.add(intersectionPoint);

    		topFace.add( intersectionPoint );
    		frontFace.add( frontFace.indexOf( rotatedPoints[reorderedList[0]] ) + 1, intersectionPoint );
    		
    		clippedList.add( rotatedPoints[reorderedList[0]] );
    	}
    	else
    	{
        	// Test Edge 3-6:
        	segment = new Segment3f( rotatedPoints[reorderedList[3]], rotatedPoints[reorderedList[6]] );
        	intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
        	if ( intersection.Find() )
        	{
        		// add intersection point:
        		Vector3f intersectionPoint = new Vector3f();
        		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
        		intersectionPoints.add(intersectionPoint);

        		leftFace.add( intersectionPoint );
        		frontFace.add( frontFace.indexOf( rotatedPoints[reorderedList[3]] ) + 1, intersectionPoint );

        		clippedList.add( rotatedPoints[reorderedList[0]] );
        		clippedList.add( rotatedPoints[reorderedList[3]] );
        	}
        	else
        	{
            	// Test Edge 6-7:
            	segment = new Segment3f( rotatedPoints[reorderedList[6]], rotatedPoints[reorderedList[7]] );
            	intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
            	if ( intersection.Find() )
            	{
            		// add intersection point:
            		Vector3f intersectionPoint = new Vector3f();
            		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
            		intersectionPoints.add(intersectionPoint);

            		leftFace.add( leftFace.indexOf( rotatedPoints[reorderedList[7]] ) + 1, intersectionPoint );
            		bottomFace.add( bottomFace.indexOf( rotatedPoints[reorderedList[6]] ) + 1, intersectionPoint );

            		clippedList.add( rotatedPoints[reorderedList[0]] );
            		clippedList.add( rotatedPoints[reorderedList[3]] );
            		clippedList.add( rotatedPoints[reorderedList[6]] );
            	}        		
        	}
    	}
    	// Test Edge 3-4:
    	segment = new Segment3f( rotatedPoints[reorderedList[3]], rotatedPoints[reorderedList[4]] );
    	intersection = new IntrSegment3Plane3f( segment, m_kCullPlane );
    	if ( intersection.Find() )
    	{
    		// add intersection point:
    		Vector3f intersectionPoint = new Vector3f();
    		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), segment.Direction, segment.Center);
    		intersectionPoints.add(intersectionPoint);

    		topFace.add( topFace.indexOf( rotatedPoints[reorderedList[4]] ) + 1, intersectionPoint );
    		leftFace.add( leftFace.indexOf( rotatedPoints[reorderedList[3]]) + 1, intersectionPoint );

    		clippedList.add( rotatedPoints[reorderedList[0]] );
    		clippedList.add( rotatedPoints[reorderedList[3]] );
    	}
    
    	// Remove clipped points from the face lists:
    	for ( int i = 0; i < clippedList.size(); i++ )
    	{
    		topFace.remove( clippedList.elementAt(i) );
    		bottomFace.remove( clippedList.elementAt(i) );
    		frontFace.remove( clippedList.elementAt(i) );
    		backFace.remove( clippedList.elementAt(i) );
    		leftFace.remove( clippedList.elementAt(i) );
    		rightFace.remove( clippedList.elementAt(i) );
    	}
    	    	
    	// Retriangulate all faces and the new polygon:
    	retriangulate(intersectionPoints);
    	
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
     *  Creates a re-mapping of the cube corners so the
     *  back bottom left corner is at position 0 in the reorderedList.
     *  The other points are added to the list based on the
     *  cube connectivity.
     */
    private void cullBackBottomLeft()
    {
//    	System.err.println("cullBackBottomLeft");
    	reorderedList[0] = backBottomLeftIndex;
    	reorderedList[1] = backBottomRightIndex;
    	reorderedList[2] = frontBottomLeftIndex;
    	reorderedList[3] = backTopLeftIndex;
    	reorderedList[4] = backTopRightIndex;
    	reorderedList[5] = frontBottomRightIndex;
    	reorderedList[6] = frontTopLeftIndex;
    	reorderedList[7] = frontTopRightIndex;
    	
    	computeIntersections();
    }
    
    /**
     *  Creates a re-mapping of the cube corners so the
     *  back bottom right corner is at position 0 in the reorderedList.
     *  The other points are added to the list based on the
     *  cube connectivity.
     */
    private void cullBackBottomRight()
    {
//    	System.err.println("cullBackBottomRight");
    	reorderedList[0] = backBottomRightIndex;
    	reorderedList[1] = backTopRightIndex;
    	reorderedList[2] = frontBottomRightIndex;
    	reorderedList[3] = backBottomLeftIndex;
    	reorderedList[4] = backTopLeftIndex;
    	reorderedList[5] = frontTopRightIndex;
    	reorderedList[6] = frontBottomLeftIndex;
    	reorderedList[7] = frontTopLeftIndex;
    	
    	computeIntersections();
    }

    /**
     *  Creates a re-mapping of the cube corners so the
     *  back top left corner is at position 0 in the reorderedList.
     *  The other points are added to the list based on the
     *  cube connectivity.
     */
    private void cullBackTopLeft()
    {
//    	System.err.println("cullBackTopLeft");
    	reorderedList[0] = backTopLeftIndex;
    	reorderedList[1] = backBottomLeftIndex;
    	reorderedList[2] = frontTopLeftIndex;
    	reorderedList[3] = backTopRightIndex;
    	reorderedList[4] = backBottomRightIndex;
    	reorderedList[5] = frontBottomLeftIndex;
    	reorderedList[6] = frontTopRightIndex;
    	reorderedList[7] = frontBottomRightIndex;
    	
    	computeIntersections();
    }

    /**
     *  Creates a re-mapping of the cube corners so the
     *  back top right corner is at position 0 in the reorderedList.
     *  The other points are added to the list based on the
     *  cube connectivity.
     */
    private void cullBackTopRight()
    {
//    	System.err.println("cullBackTopRight");
    	reorderedList[0] = backTopRightIndex;
    	reorderedList[1] = backTopLeftIndex;
    	reorderedList[2] = frontTopRightIndex;
    	reorderedList[3] = backBottomRightIndex;
    	reorderedList[4] = backBottomLeftIndex;
    	reorderedList[5] = frontTopLeftIndex;
    	reorderedList[6] = frontBottomRightIndex;
    	reorderedList[7] = frontBottomLeftIndex;
    	
    	computeIntersections();
    }

    /**
     *  Creates a re-mapping of the cube corners so the
     *  front bottom left corner is at position 0 in the reorderedList.
     *  The other points are added to the list based on the
     *  cube connectivity.
     */
    private void cullFrontBottomLeft()
    {    	
//    	System.err.println("cullFrontBottomLeft");
    	reorderedList[0] = frontBottomLeftIndex;
    	reorderedList[1] = frontBottomRightIndex;
    	reorderedList[2] = frontTopLeftIndex;
    	reorderedList[3] = backBottomLeftIndex;
    	reorderedList[4] = backBottomRightIndex;
    	reorderedList[5] = frontTopRightIndex;
    	reorderedList[6] = backTopLeftIndex;
    	reorderedList[7] = backTopRightIndex;
    	
    	computeIntersections();
    }
    
    /**
     *  Creates a re-mapping of the cube corners so the
     *  front bottom right corner is at position 0 in the reorderedList.
     *  The other points are added to the list based on the
     *  cube connectivity.
     */
    private void cullFrontBottomRight()
    {
//    	System.err.println("cullFrontBottomRight");
    	reorderedList[0] = frontBottomRightIndex;
    	reorderedList[1] = frontBottomLeftIndex;
    	reorderedList[2] = backBottomRightIndex;
    	reorderedList[3] = frontTopRightIndex;
    	reorderedList[4] = frontTopLeftIndex;
    	reorderedList[5] = backBottomLeftIndex;
    	reorderedList[6] = backTopRightIndex;
    	reorderedList[7] = backTopLeftIndex;
    	
    	computeIntersections();
    }

    /**
     *  Creates a re-mapping of the cube corners so the
     *  front top left corner is at position 0 in the reorderedList.
     *  The other points are added to the list based on the
     *  cube connectivity.
     */
    private void cullFrontTopLeft()
    {
//    	System.err.println("cullFrontTopLeft");
    	reorderedList[0] = frontTopLeftIndex;
    	reorderedList[1] = frontTopRightIndex;
    	reorderedList[2] = backTopLeftIndex;
    	reorderedList[3] = frontBottomLeftIndex;
    	reorderedList[4] = frontBottomRightIndex;
    	reorderedList[5] = backTopRightIndex;
    	reorderedList[6] = backBottomLeftIndex;
    	reorderedList[7] = backBottomRightIndex;
    	
    	computeIntersections();
    }
    

    /**
     *  Creates a re-mapping of the cube corners so the
     *  front top right corner is at position 0 in the reorderedList.
     *  The other points are added to the list based on the
     *  cube connectivity.
     */
    private void cullFrontTopRight()
    {
//    	System.err.println("cullFrontTopRight");
    	reorderedList[0] = frontTopRightIndex;
    	reorderedList[1] = frontBottomRightIndex;
    	reorderedList[2] = backTopRightIndex;
    	reorderedList[3] = frontTopLeftIndex;
    	reorderedList[4] = frontBottomLeftIndex;
    	reorderedList[5] = backBottomRightIndex;
    	reorderedList[6] = backTopLeftIndex;
    	reorderedList[7] = backBottomLeftIndex;
    	
    	computeIntersections();
    }
    
    /**
     * 1. Determine which of the cube corners is the furthest along the negative z-direction 
     * (0,0,-1). This corner is clipped by the view-frustum and will serve and the starting 
     * corner in creating the new mesh surface.
     * 
     * @param clippedDistance
     */
    private void cullPoints( float[] clippedDistance )
    {
    	int minIndex = -1;
    	float minDist = Float.MAX_VALUE;
    	for ( int i = 0; i < clippedPoints.length; i++ )
    	{
    		if ( clippedPoints[i] )
    		{
    			if ( clippedDistance[i] < minDist )
    			{
    				minDist = clippedDistance[i];
    				minIndex = i;
    			}
    		}
    	}
    	// Create a remapping of the points on the cube so that the nearest point is at index 0 
    	// and is on the cube top-face.  The remainder of the algorithm uses the remapped cube 
    	// index values.
    	switch (minIndex)
    	{
    	case frontBottomLeftIndex:  cullFrontBottomLeft();  break;
    	case frontBottomRightIndex: cullFrontBottomRight(); break;
    	case frontTopRightIndex:    cullFrontTopRight();    break;
    	case frontTopLeftIndex:     cullFrontTopLeft();     break;
    	case backBottomLeftIndex:   cullBackBottomLeft();   break;
    	case backBottomRightIndex:  cullBackBottomRight();  break;
    	case backTopRightIndex:     cullBackTopRight();     break;
    	case backTopLeftIndex:      cullBackTopLeft();      break;
    	default: break;
    	}
    }
    

    /**
     * Generates the triangle -index array connectivity for the default cube,
     * stores the index array in the input IndexBuffer.
     * @param pkIB
     */
    private void fillBox( IndexBuffer pkIB )
    {
        // generate connectivity (outside view)
        int i = 0;
        int[] aiIndex = pkIB.GetData();
        // front
        aiIndex[i++] = frontBottomLeftIndex;  aiIndex[i++] = frontTopRightIndex;  aiIndex[i++] = frontBottomRightIndex;
        aiIndex[i++] = frontBottomLeftIndex;  aiIndex[i++] = frontTopLeftIndex;  aiIndex[i++] = frontTopRightIndex;
        // back
        aiIndex[i++] = backBottomLeftIndex;  aiIndex[i++] = backBottomRightIndex;  aiIndex[i++] = backTopRightIndex;
        aiIndex[i++] = backBottomLeftIndex;  aiIndex[i++] = backTopRightIndex;  aiIndex[i++] = backTopLeftIndex;
        // top
        aiIndex[i++] = frontTopLeftIndex;  aiIndex[i++] = backTopRightIndex;  aiIndex[i++] = frontTopRightIndex;
        aiIndex[i++] = frontTopLeftIndex;  aiIndex[i++] = backTopLeftIndex;  aiIndex[i++] = backTopRightIndex;
        // bottom
        aiIndex[i++] = frontBottomLeftIndex;  aiIndex[i++] = frontBottomRightIndex;  aiIndex[i++] = backBottomRightIndex;
        aiIndex[i++] = frontBottomLeftIndex;  aiIndex[i++] = backBottomRightIndex;  aiIndex[i++] = backBottomLeftIndex;
        // right
        aiIndex[i++] = frontTopRightIndex;  aiIndex[i++] = backTopRightIndex;  aiIndex[i++] = backBottomRightIndex;
        aiIndex[i++] = frontTopRightIndex;  aiIndex[i++] = backBottomRightIndex;  aiIndex[i++] = frontBottomRightIndex;
        // left
        aiIndex[i++] = frontBottomLeftIndex;  aiIndex[i++] = backTopLeftIndex;  aiIndex[i++] = frontTopLeftIndex;
        aiIndex[i++] = frontBottomLeftIndex;  aiIndex[i++] = backBottomLeftIndex;  aiIndex[i++] = backTopLeftIndex;
    }
    
    
    /**
     * Fills in the values for the default cube. Writes the values into the input VertexBuffer.
     * Fills in the position, color, and texture coordinates.
     * @param pkVB
     */
    private void fillBox( VertexBuffer pkVB )
    {
        pkVB.SetPosition3(frontBottomLeftIndex,frontBottomLeft);
        pkVB.SetColor3(0,frontBottomLeftIndex,0,0,0);
        pkVB.SetTCoord3(0,frontBottomLeftIndex,0,0,0);
        pkVB.SetTCoord3(1,frontBottomLeftIndex,0,0,0);
        
        pkVB.SetPosition3(frontBottomRightIndex,frontBottomRight);
        pkVB.SetColor3(0,frontBottomRightIndex,1,0,0);
        pkVB.SetTCoord3(0,frontBottomRightIndex,1,0,0);
        pkVB.SetTCoord3(1,frontBottomRightIndex,1,0,0);
        
        pkVB.SetPosition3(frontTopRightIndex,frontTopRight);
        pkVB.SetColor3(0,frontTopRightIndex,1,1,0);
        pkVB.SetTCoord3(0,frontTopRightIndex,1,1,0);
        pkVB.SetTCoord3(1,frontTopRightIndex,1,1,0);
        
        pkVB.SetPosition3(frontTopLeftIndex,frontTopLeft);
        pkVB.SetColor3(0,frontTopLeftIndex,0,1,0);
        pkVB.SetTCoord3(0,frontTopLeftIndex,0,1,0);
        pkVB.SetTCoord3(1,frontTopLeftIndex,0,1,0);

        pkVB.SetPosition3(backBottomLeftIndex,backBottomLeft);
        pkVB.SetColor3(0,backBottomLeftIndex,0,0,1);
        pkVB.SetTCoord3(0,backBottomLeftIndex,0,0,1);
        pkVB.SetTCoord3(1,backBottomLeftIndex,0,0,1);
        
        pkVB.SetPosition3(backBottomRightIndex,backBottomRight);
        pkVB.SetColor3(0,backBottomRightIndex,1,0,1);
        pkVB.SetTCoord3(0,backBottomRightIndex,1,0,1);
        pkVB.SetTCoord3(1,backBottomRightIndex,1,0,1);
        
        pkVB.SetPosition3(backTopRightIndex,backTopRight);
        pkVB.SetColor3(0,backTopRightIndex,1,1,1);
        pkVB.SetTCoord3(0,backTopRightIndex,1,1,1);
        pkVB.SetTCoord3(1,backTopRightIndex,1,1,1);
        
        pkVB.SetPosition3(backTopLeftIndex,backTopLeft);
        pkVB.SetColor3(0,backTopLeftIndex,0,1,1);
        pkVB.SetTCoord3(0,backTopLeftIndex,0,1,1);
        pkVB.SetTCoord3(1,backTopLeftIndex,0,1,1);
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

        //kCuller.ComputeVisibleSet(m_kScene);

        // Cull front-facing polygons:
        m_kCull.Enabled = true;
        m_kCull.CullFace = CullState.CullMode.CT_FRONT;
        kRenderer.DrawScene(kCuller.GetVisibleSet());
        // Undo culling:
        m_kCull.CullFace = CullState.CullMode.CT_BACK;
        
    }
    
    /**
     * Generates the new VertexBuffer and IndexBuffer for the retriangulated faces of the cube
     * based on how the cube intersects with the clipping plane.  Each of the faces:
     * top, bottom, front, back, left, right have points listed in counter-clockwise order.
     * Where the clipping plane intersects the face the clipped points have been removed
     * and the new intersection points have been added -- always maintaining the ccw ordering
     * of the points.  This way we can automatically generate the triangle mesh by adding
     * up the points and calculating the number of triangles per face.
     * 
     * The input list represents the new face -- the one generated by the clip plane intersecting 
     * the cube.
     * @param intersectionPoints
     */
    private void retriangulate( Vector<Vector3f> intersectionPoints )
    {
    	
    	// Calculate the total number of points and new triangles:
    	int newNumPoints = topFace.size() + bottomFace.size() + frontFace.size() + backFace.size() + leftFace.size() + rightFace.size() + intersectionPoints.size();
    	int newNumTris = 0;
    	if ( topFace.size() >=3 )
    	{
    		newNumTris += (topFace.size() - 2);
    	}
    	if ( bottomFace.size() >=3 )
    	{
    		newNumTris += (bottomFace.size() - 2);
    	}
    	if ( frontFace.size() >=3 )
    	{
    		newNumTris += (frontFace.size() - 2);
    	}
    	if ( backFace.size() >=3 )
    	{
    		newNumTris += (backFace.size() - 2);
    	}
    	if ( leftFace.size() >=3 )
    	{
    		newNumTris += (leftFace.size() - 2);
    	}
    	if ( rightFace.size() >=3 )
    	{
    		newNumTris += (rightFace.size() - 2);
    	}
    	if ( intersectionPoints.size() >=3 )
    	{
    		newNumTris += (intersectionPoints.size() - 2);
    	}
    	
    	
		VertexBuffer newVBuffer = new VertexBuffer( m_kMesh.VBuffer.GetAttributes(), newNumPoints );
		IndexBuffer newIBuffer = new IndexBuffer(3*newNumTris);
		
		// Add points and triangles for each face:

		// Top Face:
		int pIndex = 0;
		int firstPIndex = 0;
    	for ( int i = 0; i < topFace.size(); i++ )
    	{
    		Vector3f temp = topFace.elementAt(i);
    		Vector4f temp4 = new Vector4f( temp.X, temp.Y, temp.Z, 1);
    		Vector4f result = kWorldInv.multLeft(temp4);
    		newVBuffer.SetPosition3( pIndex, result.X, result.Y, result.Z );
    		result.X /= m_fX;
    		result.Y /= m_fY;
    		result.Z /= m_fZ;

			newVBuffer.SetColor3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 1, pIndex, result.X, result.Y, result.Z );     
    		pIndex++;
    	}
		int index = 0;
		int nextPIndex = firstPIndex + 1;
    	int numTris = (topFace.size() - 2);
		for ( int i = 0; i < numTris; i++ )
		{	  
    		newIBuffer.GetData()[index++] = firstPIndex; 
    		newIBuffer.GetData()[index++] = nextPIndex;     	
    		nextPIndex++;
    		newIBuffer.GetData()[index++] = nextPIndex;			
		}

		// Bottom Face:
		firstPIndex = pIndex;
    	for ( int i = 0; i < bottomFace.size(); i++ )
    	{
    		Vector3f temp = bottomFace.elementAt(i);
    		Vector4f temp4 = new Vector4f( temp.X, temp.Y, temp.Z, 1);
    		Vector4f result = kWorldInv.multLeft(temp4);
    		newVBuffer.SetPosition3( pIndex, result.X, result.Y, result.Z );
    		result.X /= m_fX;
    		result.Y /= m_fY;
    		result.Z /= m_fZ;

			newVBuffer.SetColor3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 1, pIndex, result.X, result.Y, result.Z );     
    		pIndex++;
    	}
		nextPIndex = firstPIndex + 1;
    	numTris = (bottomFace.size() - 2);
		for ( int i = 0; i < numTris; i++ )
		{	
    		newIBuffer.GetData()[index++] = firstPIndex;   
    		newIBuffer.GetData()[index++] = nextPIndex;     	
    		nextPIndex++;
    		newIBuffer.GetData()[index++] = nextPIndex;			
		}

		// Left Face:
		firstPIndex = pIndex;
    	for ( int i = 0; i < leftFace.size(); i++ )
    	{
    		Vector3f temp = leftFace.elementAt(i);
    		Vector4f temp4 = new Vector4f( temp.X, temp.Y, temp.Z, 1);
    		Vector4f result = kWorldInv.multLeft(temp4);
    		newVBuffer.SetPosition3( pIndex, result.X, result.Y, result.Z );
    		result.X /= m_fX;
    		result.Y /= m_fY;
    		result.Z /= m_fZ;

			newVBuffer.SetColor3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 1, pIndex, result.X, result.Y, result.Z );     
    		pIndex++;
    	}
		nextPIndex = firstPIndex + 1;
    	numTris = (leftFace.size() - 2);
		for ( int i = 0; i < numTris; i++ )
		{	 
    		newIBuffer.GetData()[index++] = firstPIndex;  
    		newIBuffer.GetData()[index++] = nextPIndex;     	
    		nextPIndex++;
    		newIBuffer.GetData()[index++] = nextPIndex;			
		}

		// Right Face:
		firstPIndex = pIndex;
    	for ( int i = 0; i < rightFace.size(); i++ )
    	{
    		Vector3f temp = rightFace.elementAt(i);
    		Vector4f temp4 = new Vector4f( temp.X, temp.Y, temp.Z, 1);
    		Vector4f result = kWorldInv.multLeft(temp4);
    		newVBuffer.SetPosition3( pIndex, result.X, result.Y, result.Z );
    		result.X /= m_fX;
    		result.Y /= m_fY;
    		result.Z /= m_fZ;

			newVBuffer.SetColor3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 1, pIndex, result.X, result.Y, result.Z );     
    		pIndex++;
    	}
		nextPIndex = firstPIndex + 1;
    	numTris = (rightFace.size() - 2);
		for ( int i = 0; i < numTris; i++ )
		{	
    		newIBuffer.GetData()[index++] = firstPIndex;   
    		newIBuffer.GetData()[index++] = nextPIndex;     	
    		nextPIndex++;
    		newIBuffer.GetData()[index++] = nextPIndex;			
		}

		// Front Face:
		firstPIndex = pIndex;
    	for ( int i = 0; i < frontFace.size(); i++ )
    	{
    		Vector3f temp = frontFace.elementAt(i);
    		Vector4f temp4 = new Vector4f( temp.X, temp.Y, temp.Z, 1);
    		Vector4f result = kWorldInv.multLeft(temp4);
    		newVBuffer.SetPosition3( pIndex, result.X, result.Y, result.Z );
    		result.X /= m_fX;
    		result.Y /= m_fY;
    		result.Z /= m_fZ;

			newVBuffer.SetColor3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 1, pIndex, result.X, result.Y, result.Z );     
    		pIndex++;
    	}
		nextPIndex = firstPIndex + 1;
    	numTris = (frontFace.size() - 2);
		for ( int i = 0; i < numTris; i++ )
		{	
    		newIBuffer.GetData()[index++] = firstPIndex;    
    		newIBuffer.GetData()[index++] = nextPIndex;    	
    		nextPIndex++;
    		newIBuffer.GetData()[index++] = nextPIndex;			
		}
		

		// Back Face:
		firstPIndex = pIndex;
    	for ( int i = 0; i < backFace.size(); i++ )
    	{
    		Vector3f temp = backFace.elementAt(i);
    		Vector4f temp4 = new Vector4f( temp.X, temp.Y, temp.Z, 1);
    		Vector4f result = kWorldInv.multLeft(temp4);
    		newVBuffer.SetPosition3( pIndex, result.X, result.Y, result.Z );
    		result.X /= m_fX;
    		result.Y /= m_fY;
    		result.Z /= m_fZ;

			newVBuffer.SetColor3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 1, pIndex, result.X, result.Y, result.Z );     
    		pIndex++;
    	}
		nextPIndex = firstPIndex + 1;
    	numTris = (backFace.size() - 2);
		for ( int i = 0; i < numTris; i++ )
		{	
    		newIBuffer.GetData()[index++] = firstPIndex;    
    		newIBuffer.GetData()[index++] = nextPIndex;    	
    		nextPIndex++;
    		newIBuffer.GetData()[index++] = nextPIndex;			
		}

		// New Face:
		firstPIndex = pIndex;
    	for ( int i = 0; i < intersectionPoints.size(); i++ )
    	{
    		Vector3f temp = intersectionPoints.elementAt(i);
    		Vector4f temp4 = new Vector4f( temp.X, temp.Y, temp.Z, 1);
    		Vector4f result = kWorldInv.multLeft(temp4);
    		newVBuffer.SetPosition3( pIndex, result.X, result.Y, result.Z );
    		result.X /= m_fX;
    		result.Y /= m_fY;
    		result.Z /= m_fZ;

			newVBuffer.SetColor3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 0, pIndex, result.X, result.Y, result.Z );
			newVBuffer.SetTCoord3( 1, pIndex, result.X, result.Y, result.Z );     
    		pIndex++;
    	}
		nextPIndex = firstPIndex + 1;
    	numTris = (intersectionPoints.size() - 2);
		for ( int i = 0; i < numTris; i++ )
		{	  
    		newIBuffer.GetData()[index++] = nextPIndex;     	
    		newIBuffer.GetData()[index++] = firstPIndex; 
    		nextPIndex++;
    		newIBuffer.GetData()[index++] = nextPIndex;			
		}
    	

		m_kMesh.Release(m_kRenderer);
		m_kMesh.VBuffer = newVBuffer;
		m_kMesh.IBuffer = newIBuffer;   	    	
    }
    
}
