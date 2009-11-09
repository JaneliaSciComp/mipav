package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.view.ViewJFrameAnimateClip;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Render.Sculptor_WM;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeBoundingBox;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeClip;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeDTI;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageCrop;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageExtract;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageNormalGM;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageSurfaceMask;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeNode;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeOrientationCube;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeRayCast;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSlices;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;
import gov.nih.mipav.view.renderer.WildMagic.Render.OrderIndpTransparencyEffect;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetState;
import gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork.*;

import java.awt.Cursor;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLContext;
import javax.media.opengl.GLEventListener;

import WildMagic.LibApplications.OpenGLApplication.ApplicationGUI;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.Camera;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.FrameBuffer;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.Rendering.ZBufferState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Geometry;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.Shaders.SamplerInformation;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLFrameBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.sun.opengl.util.Animator;

public class VolumeTriPlanarRender extends GPURenderBase
implements GLEventListener, KeyListener, MouseMotionListener,  MouseListener
{

    /** New sculpting object for WM-based sculpting. */
    private Sculptor_WM m_kSculptor = null;

    /** Arbitrary clip plane equation: */
    private float[] m_afArbEquation = new float[4];
    private Vector4f m_kArbitraryClip;
    /** Enable Arbitrary clip plane: */
    private boolean m_bArbClipOn = false;

    /** Window with the shader parameter interface: */
    private ApplicationGUI m_kShaderParamsWindow = null;

    /** Enables/Disables rendering the second pass. When disabled, the
     * back-facing polygons of the proxy-geometry are shown instead of the volume: */
    private boolean m_bDisplaySecond = true;

    /** 3D Slice renderer. */
    private VolumeSlices m_kSlices = null;
    /** GPU-based ray cast renderer. */
    private VolumeRayCast m_kVolumeRayCast = null;
    /** Clipping planes renderer */
    private VolumeClip m_kVolumeClip = null;
    /** DTI renderer. */
    private VolumeDTI m_kDTIDisplay = null;
    /** Volume bounding box renderer. */
    private VolumeBoundingBox m_kVolumeBox = null;
    /** Orientation cube renderer */
    private VolumeOrientationCube m_kVolumeCube = null;

    /** The first time the frame is rendererd use the shader to calculate the normals for the volume data. */
    private boolean m_bFirstRender = true;

    /** Painting parameters: */
    private boolean m_bPaintEnabled = false;
    private boolean m_bPaint = false;
    private boolean m_bErase = false;
    private boolean m_bDropper = false;
    private boolean m_bPaintCan = false;
    private ColorRGBA m_kPaintColor = null;
    private int m_iBrushSize = 1;
    
    /** Geodesic enabled on/off. */
    protected boolean m_bGeodesicEnabled = false;

    /** Stereo on/off. */
    private int m_iStereo = 0;  
    
    /** BrainSurfaceFlattener pick correspondence enabled on/off. */
    private boolean m_bPickCorrespondence = false;
    
    /** Set to true when cropping the volume in the shader. */
    private boolean m_bCrop = false;
    /** Intensity level for GPU-surface extraction. */
    private int m_iExtractLevel = 1;
    
    /** sample rate when mouse is released. */
    private float fSample_mouseReleased;
    /** sample rate when mouse is dragged. */
    private float fSample_mouseDragged;
    
    private boolean m_bPlay4D = false;
    private float m_fAnimateRate = 0;
    private int m_iAnimateCount = 0;
    
    

    private OpenGLFrameBuffer m_kFBO;    
    private ShaderEffect m_spkPlaneEffect;
    private TriMesh m_pkPlane;
    private TriMesh m_pkPlanePreRender;
    private Camera m_pkScreenCamera;
    
    /**
     * Default Constructor.
     */
    public VolumeTriPlanarRender()
    {
        super();
    }
        
    /**
     * Construct the Volume/Surface/Tri-Planar renderer.
     * @param kParent parent user-interface and frame.
     * @param kAnimator animator used to display the canvas.
     * @param kVolumeImageA volume data and textures for ModelImage A.
     * @param kVolumeImageB volume data and textures for ModelImage B.
     */
    public VolumeTriPlanarRender( VolumeTriPlanarInterface kParent, Animator kAnimator, 
            VolumeImage kVolumeImageA, VolumeImage kVolumeImageB  )
    {
        super();
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseWheelListener( this );       

        m_kAnimator = kAnimator;
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        m_kParent = kParent;        
        m_kRotate.FromAxisAngle(Vector3f.UNIT_Z, (float)Math.PI/18.0f);
    }
    
    
    /**
     * Add a new geodesic component to the surface.
     * @param kSurface the surface the geodesic component is added to.
     * @param kNew the new geodesic component.
     * @param iGroup the display group to add to.
     */
    public void addGeodesic( TriMesh kSurface, Geometry kNew, int iGroup )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetMesh() != null )
            {
                if ( m_kDisplayList.get(i).GetMesh() == kSurface)
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).AddGeodesic(kNew, iGroup);
                }
            }
        }
    }

    /** Add a polyline to the display. Used to display fiber tract bundles.
     * @param kLine new polyline to display.
     * @param iGroup the group the polyline belongs to.
     */
    public void addPolyline( Polyline kLine, int iGroup )
    {
        if ( m_kDTIDisplay == null )
        {
            m_kDTIDisplay = new VolumeDTI( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
            m_kDisplayList.add(1, m_kDTIDisplay);
        }
        m_kDTIDisplay.addPolyline( kLine, iGroup );
        m_kDTIDisplay.SetDisplay( true );
    }

    /**
     * Add surfaces to the display list.
     * @param akSurfaces
     */
    public void addSurface(TriMesh[] akSurfaces)
    {
        for ( int i = 0; i < akSurfaces.length; i++ )
        {
            VolumeSurface kVolumeSurfaces = new VolumeSurface( m_kVolumeImageA, m_kVolumeImageB,
                    m_kTranslate,
                    m_fX, m_fY, m_fZ,
                    akSurfaces[i] );
            kVolumeSurfaces.SetPerPixelLighting( m_pkRenderer, true );
            m_kDisplayList.add( kVolumeSurfaces );
        }
        UpdateSceneRotation();
        m_bSurfaceUpdate = true;
    }

    /**
     * Add tract into the DTI display
     * @param kLine   polyline
     * @param iGroup  counter number
     * @param centerIndex  center index color
     */
    public void addTract( Polyline kLine, int iGroup, int centerIndex )
    {
        if ( m_kDTIDisplay == null )
        {
            m_kDTIDisplay = new VolumeDTI( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
            m_kDisplayList.add(1, m_kDTIDisplay);
        }
        m_kDTIDisplay.setCenterIndex(centerIndex);
        m_kDTIDisplay.addPolyline( kLine, iGroup );
        m_kDTIDisplay.SetDisplay( true );
        UpdateSceneRotation();
        /*
        TriMesh[] akMeshes = new TriMesh[1];
        akMeshes[0] = m_kDTIDisplay.createTube(kLine);
        akMeshes[0].SetName( "TUBE" );
        m_kParent.getSurfacePanel().addSurfaces(akMeshes);
        */
    }
    
    /**
     * Apply the sculpt region to the volume.
     */
    public void applySculpt(boolean bAll)
    {
        if ( m_kSculptor == null )
        {
            return;
        }

        float[] afData = new float[16];
        Matrix4f kWorld = new Matrix4f();
        
        m_pkRenderer.SetConstantVPMatrix (0, afData);
        Matrix4f kVP = new Matrix4f( afData, true );
        kWorld.Mult(m_kSceneToWorld, kVP);
        m_kSculptor.setWVPMatrix(kWorld);   
        boolean bSculptTrue = false;
        int iTSize = (m_kVolumeImageA.GetImage().is4DImage()&&bAll) ? m_kVolumeImageA.GetImage().getExtents()[3] : 1;
        for ( int i = 0; i < iTSize; i++ )
        {
            if ( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() != null )
            {
                m_kSculptor.setTextureImageDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() );
            }
            else
            {
                m_kSculptor.setTextureImageFloatDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetFloatData() );
            }

            if ( m_kVolumeImageB.GetImage() != null )
            {
                if ( m_kVolumeImageB.GetVolumeTarget().GetImage().GetData() != null )
                {
                    m_kSculptor.setTextureImageDataB( m_kVolumeImageB.GetVolumeTarget().GetImage().GetData() );
                }
                else
                {
                    m_kSculptor.setTextureImageFloatDataB( m_kVolumeImageB.GetVolumeTarget().GetImage().GetFloatData() );
                }
            }
            if ( m_kSculptor.applySculpt(m_kVolumeImageA.GetTimeSlice()) )
            {
                bSculptTrue = true;
            }
            if ( iTSize > 1 )
            {
                m_kVolumeImageA.update4D(true);
            }
        }
        if ( bSculptTrue )
        {
            m_kVolumeImageA.ReleaseVolume();
            if ( m_kVolumeImageB.GetImage() != null )
            {
                m_kVolumeImageB.ReleaseVolume();
            }
            m_kSculptor.clearSculpt();
            m_kParent.setModified();
        }
    }

    /**
     * Sets blending between imageA and imageB.
     * @param fValue the blend value (0-1)
     */
    public void blend( String kSurfaceName, float fValue )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    m_kDisplayList.get(i).Blend( fValue );
                    // save the opacity property in surface attributes
                    ((VolumeSurface)(m_kDisplayList.get(i))).SetOpacity(fValue);
                }
            }
        }
    }

    /**
     * Clear the sculpt region.
     */
    public void clearSculpt()
    {
        if ( m_kSculptor != null )
        {
            m_kSculptor.clearSculpt();
        }
    }

    
    /**
     * Display the volume in Composite mode.
     */
    public void CMPMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.CMPMode();
        }
        ResetShaderParamsWindow();
    }


    /**
     * Crop the clipped volume.
     */
    public void cropClipVolume()
    {
        m_bCrop = true;
        GetCanvas().display();
    }


    /* (non-Javadoc)
     * @see javax.media.opengl.GLEventListener#display(javax.media.opengl.GLAutoDrawable)
     */
    public void display(GLAutoDrawable arg0) {
        if ( !m_bVisible )
        {
            return;
        }
        
        if ( m_kVolumeImageA == null ) {
        	return;
        }      
        if ( m_pkRenderer != null )
        {
            ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        }
        
        if ( !m_bInit )
        {
            init(arg0);
        }
        
        if ( m_bFirstRender )
        {
            m_kVolumeRayCast.setVolumeSamples( 0 );
            m_kVolumeRayCast.SetDisplay(true);   
            m_kSlices.SetDisplay(false);   
        }
        MeasureTime();
        Move();
        Pick();

        
        Render();

        UpdateFrameCount();

        if ( m_bFirstRender )
        {
            m_bFirstRender = false;
            m_kVolumeRayCast.SetDisplay(false);   
            m_kSlices.SetDisplay(true);   
            m_kParent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            m_kVolumeImageA.GenerateNormalFiles( );
            //VolumeImageNormalGM.main(m_kParent, m_kVolumeImageA);
            //if ( m_kVolumeImageB != null )
            //{
            //    VolumeImageNormalGM.main(m_kParent, m_kVolumeImageB);
            //}
            m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            m_kVolumeRayCast.setVolumeSamples( (m_kVolumeImageA.GetImage().getExtents()[2]*2.0f)/1000.0f );
            CMPMode();
            m_kAnimator.setRunAsFastAsPossible(true); 
        }
        if ( m_bSurfaceUpdate )
        {
            m_bSurfaceUpdate = false;
            updateLighting( m_akLights );
            VolumeImageSurfaceMask.main(m_kParent, m_kVolumeImageA, m_kDisplayList);
        }
        if ( m_bCrop )
        {
            m_bCrop = false;
            m_kParent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            VolumeImageCrop.main(m_kParent, m_kVolumeImageA, m_kVolumeRayCast.GetClipEffect());
            if ( m_kVolumeImageB.GetImage() != null )
            {
                VolumeImageCrop.main(m_kParent, m_kVolumeImageB, m_kVolumeRayCast.GetClipEffect());
            }
            m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }
        if ( m_bExtract )
        {
            m_kParent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            VolumeImageExtract.main(m_kParent, m_kVolumeImageA, m_kVolumeRayCast.GetClipEffect(), m_iExtractLevel);
            m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            m_bExtract = false;
        }
        if ( m_bPlay4D )
        {
            if ( m_iAnimateCount++ > m_fAnimateRate )
            {
                m_iAnimateCount = 0;
                update4D( true );
            }
        }
    }

    /**
     * Displays the arbitrary clip plane position.
     * @param bDisplay on/off.
     */
    public void displayArbitraryClipPlane( boolean bDisplay )
    {
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.DisplayArb(bDisplay);
        }
    }
    
    /**
     * Called from JPanelDisplay. Sets the bounding box display on/off.
     * @param bDisplay on/off.
     */
    public void displayBoundingBox( boolean bDisplay )
    {
        if ( m_kVolumeBox != null )
        {
            m_kVolumeBox.SetDisplay(bDisplay);
        }
    }

    /**
     * Called from JPanelClip. Sets the axis-aligned clip plane display on/off.
     * @param iWhich the clip plane to set.
     * @param bDisplay on/off.
     */
    public void displayClipPlane( int iWhich, boolean bDisplay )
    {
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.displayClipPlane(iWhich, bDisplay);
        }
    }

    /**
     * Called from JPanelClip. Sets the axis-aligned clip plane display on/off and color.
     * @param iWhich the clip plane to set.
     * @param bDisplay on/off.
     * @param kColor the clipping plane color.
     */
    public void displayClipPlane( int iWhich, boolean bDisplay, ColorRGB kColor )
    {
        setClipPlaneColor( iWhich, kColor );
        displayClipPlane( iWhich, bDisplay );
    }

    /**
     * Toggle display of the scene-graphe node on/off.
     * @param kNode the node to toggle.
     * @param bDisplay on/off.
     */
    public void displayNode( Node kNode, boolean bDisplay )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeNode )
            {
                if ( ((VolumeNode)m_kDisplayList.get(i)).GetNode() == kNode )
                {
                    m_kDisplayList.get(i).SetDisplay(bDisplay);
                }
            }
        }
    }

    /**
     * Called from JPanelDisplay. Sets the orientation cube display on/off.
     * @param bDisplay on/off.
     */
    public void displayOrientationCube( boolean bDisplay )
    {
        if ( m_kVolumeCube != null )
        {
            m_kVolumeCube.SetDisplay(bDisplay);
        }
    }

    /**
     * Launches the ApplicationGUI window displaying the currently-loaded
     * shader parameters.
     */
    public void displayShaderParameters()
    {
        if ( m_kShaderParamsWindow == null )
        {
            m_kShaderParamsWindow = new ApplicationGUI();
            m_kShaderParamsWindow.setParent(this);
        }
        m_kShaderParamsWindow.close();
        m_kShaderParamsWindow.AddUserVariables(m_kVolumeRayCast.GetShaderEffect().GetCProgram(0));
        m_kShaderParamsWindow.Display();
        m_kShaderParamsWindow.setParent(this);
    }

    /**
     * Toggle surface display on/off.
     * @param bDisplay
     */
    public void displaySurface(boolean bDisplay)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                m_kDisplayList.get(i).SetDisplay(bDisplay);
            }
        }
    }

    /**
     * Toggle volume display on/off.
     * @param bDisplay
     */
    public void displayVolumeRaycast( boolean bDisplay )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetDisplay(bDisplay);
        }
    }

    /**
     * Toggle 3D Slice display on/off.
     * @param bDisplay
     */
    public void displayVolumeSlices( boolean bDisplay )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetDisplay( bDisplay );
        }
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.GPURenderBase#dispose()
     */
    public void dispose()
    {
        if ( m_kSculptor != null )
        { 
            m_kSculptor.disposeLocal();
            m_kSculptor = null;
        }
        if ( m_kShaderParamsWindow != null )
        {
            m_kShaderParamsWindow.close();
        }
        if ( m_kPaintColor != null )
        {
            m_kPaintColor = null;
        }
        if ( m_kArbitraryClip != null )
        {
            m_kArbitraryClip = null;
        }

        super.dispose();
    }

    /**
     * Display the volume in DDR mode.
     */
    public void DRRMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.DRRMode();
        }
        ResetShaderParamsWindow();
    }

    /**
     * Enables the arbitrary clip plane position.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor the arbitrary clip plane color.
     */
    public void enableArbitraryClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setArbColor(kColor);
            m_kVolumeClip.DisplayArb(bDisplay);
        }
        if ( !bEnable )
        {
            setArbitraryClipPlane((m_kVolumeImageA.GetImage().getExtents()[0] -1), bEnable);
        }
    }

    /**
     * Enables the axis-aligned clipping planes.
     * @param iWhich the clip plane to enable.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     */
    public void enableClipPlane( int iWhich, boolean bEnable, boolean bDisplay )
    {
        displayClipPlane( iWhich, bDisplay );

        float fValue = 0;
        if ( bEnable )
        {
            fValue = m_kVolumeClip.GetValue(iWhich);
        }
        else
        { 
            if ( iWhich%2 == 0 )
                fValue = 0;
            else
                fValue = 1;
        }

        float[] data = new float[4];
        data[0] = fValue;
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClip(iWhich,fValue,bEnable);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClip(iWhich,fValue,bEnable);
            }
        }
    }

    /**
     * Enables the eye clip plane.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor the eye clip plane color.
     */
    public void enableEyeClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setEyeColor(kColor);
            m_kVolumeClip.DisplayEye(bDisplay);
        }
        if ( !bEnable )
        {
            setEyeClipPlane(0, bDisplay, bEnable);
        }
    }

    /**
     * Enables the inverse-eye clip plane.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor the inverse-eye clip plane color.
     */
    public void enableEyeInvClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
    {
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setEyeInvColor(kColor);
            m_kVolumeClip.DisplayEyeInv(bDisplay);
        }
        if ( !bEnable )
        {
            setEyeInvClipPlane(m_kVolumeImageA.GetImage().getExtents()[2] - 1, bDisplay, bEnable);
        }
    }

    /**
     * Enable geodesic curve calculations/display.
     * @param bEnable
     */
    public void enableGeodesic( boolean bEnable )
    {
        m_bGeodesicEnabled = bEnable;
    }

    /**
     * Enable painting on TriMesh surfaces.
     * @param kPaintColor paint color.
     * @param iBrushSize brush size.
     * @param bEnabled painting on/off.
     * @param bPaint when true apply paint.
     * @param bDropper when true do dropper mode.
     * @param bPaintCan when true do paint can mode.
     * @param bErase when true erase.
     */
    public void enablePaint( ColorRGBA kPaintColor, int iBrushSize, boolean bEnabled, boolean bPaint, boolean bDropper, boolean bPaintCan, boolean bErase )
    {
        m_kPaintColor = kPaintColor;
        m_iBrushSize = iBrushSize;
        m_bPaintEnabled = bEnabled;
        m_bPaint = bPaint;
        m_bDropper = bDropper;
        m_bPaintCan = bPaintCan;
        m_bErase = bErase;
    }

    /**
     * Enables and disables sculpting.
     * @param bSculpt true to enable sculpting, false to disable.
     */
    public void enableSculpt( boolean bSculpt )
    {
        if ( m_kSculptor == null )
        {
            m_kSculptor = new Sculptor_WM( ((OpenGLRenderer)m_pkRenderer).GetCanvas() );
            if ( m_kVolumeImageB.GetImage() != null )
            {
                m_kSculptor.setImage(m_kVolumeImageA.GetImage(), m_kVolumeImageB.GetImage());
            }
            else
            {
                m_kSculptor.setImage(m_kVolumeImageA.GetImage(), null);
            }
        }
        m_kSculptor.enableSculpt(bSculpt);
    }
    
    /**
     * Erase all surface paint.
     */
    public void eraseAllPaint()
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)(m_kDisplayList.get(i))).EraseAllPaint(m_pkRenderer);
            }
        }
    }

    /** Extract a TriMesh surface from the Volume Rendering. */
    public void extractMeshFromVolume()
    {
        m_bExtract = true;
    }
    
    /**
     * Return the material properties of the given surface.
     * @param kSurfaceName the surface to query.
     * @return the material properties of the surface.
     */
    public MaterialState getMaterial( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    return ((VolumeSurface)(m_kDisplayList.get(i))).GetMaterial();
                }
            }
        }
        return null;
    }
    
    /**
     * Return the opacity properties of the given surface.
     * @param kSurfaceName the surface to query.
     * @return the opacity value of the surface.
     */
    public float getOpacity( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    return ((VolumeSurface)(m_kDisplayList.get(i))).GetOpacity();
                }
            }
        }
        return 0;
    }
    

    /** Returns the polyline color for the specified fiber bundle tract group. 
     * @param iGroup the fiber bundle group to query.
     * @return the polyline color for the specified fiber bundle tract group. 
     */
    public ColorRGB getPolylineColor( int iGroup )
    {
        if ( m_kDTIDisplay != null )
        {
            return m_kDTIDisplay.getPolylineColor(iGroup);
        }
        return null;
    }

    /**
     * Returns true when sculpting is enabled.
     * @return true when sculpting is enabled, false otherwise.
     */
    public boolean getSculptEnabled()
    {
        if ( m_kSculptor == null )
        {
            return false;
        }
        return m_kSculptor.getEnable();
    }

    /**
     * Return the TriMesh surface with the given name.
     * @param kSurfaceName the name of the surface.
     * @return TriMesh.
     */
    public TriMesh getSurface( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    return m_kDisplayList.get(i).GetMesh();
                }
            }
        }
        return null;
    }


    /**
     * Return the surface area for the given TriMesh surface.
     * @param kSurfaceName the surface name.
     * @return the surface-area of the mesh.
     */
    public float getSurfaceArea( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    return ((VolumeSurface)(m_kDisplayList.get(i))).GetSurfaceArea();
                }
            }
        }
        return 0;
    }
    
    /**
     * Return the center of the TriMesh surface.
     * @param kSurfaceName the surface name.
     * @return the calculated center.
     */
    public Vector3f getSurfaceCenter( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    return ((VolumeSurface)(m_kDisplayList.get(i))).GetCenter();
                }
            }
        }
      return null;
    }

    /**
     * Return the volume of the TriMesh surface.
     * @param kSurfaceName the surface name.
     * @return the calculated volume.
     */
    public float getSurfaceVolume( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    return ((VolumeSurface)(m_kDisplayList.get(i))).GetVolume();
                }
            }
        }
        return 0;
    }
    
    /**
     * Part of the GLEventListener interface. Init is called once when the
     * GLCanvas is first displayed. Called again if the GLCanvas is removed
     * from the frame and added to another window or panel. Initializes the
     * display.
     * @param arg0 GLCanvas
     */
    public void init(GLAutoDrawable arg0) {	
    	if ( m_kVolumeImageA == null ) {
        	return;
        }      
        if ( m_pkRenderer != null )
        {
            ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        }
    	
        m_bInit = true;
        m_pkRenderer.InitializeState();
        m_pkRenderer.SetLineWidth(3);

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
        Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCUp = new Vector3f(0.0f, -1.0f,0.0f);
        Vector3f kCRight = new Vector3f();
        kCRight.Cross( kCDir, kCUp );
        Vector3f kCLoc = new Vector3f(kCDir);
        kCLoc.Scale(-1.4f);
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        CreateScene( arg0 );

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);
        m_kCuller.ComputeVisibleSet(m_spkScene);

        InitializeCameraMotion(.05f,0.001f);
        InitializeObjectMotion(m_spkScene);

        CreateRenderTarget( arg0, m_iWidth, m_iHeight );
        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );

        m_kAnimator.add( GetCanvas() );
        m_kAnimator = new Animator( GetCanvas() );
        m_kAnimator.start();
    }
    
    /**
     * Invert the sculpt region.
     */
    public void invertSculpt()
    {
        if ( m_kSculptor != null )
        {
            m_kSculptor.invertSculpt();
        }
    }

    /**
     * Part of the KeyListener interface. Pressing 'b' toggles displaying the
     * proxy-geometry versus the ray-traced volume.
     * @param e the key event.
     */
    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        super.keyPressed(e);
        switch (ucKey)
        {
        case 'b':
            m_bDisplaySecond = !m_bDisplaySecond;
            if ( m_kVolumeRayCast != null )
            {
                m_kVolumeRayCast.SetDisplaySecond( m_bDisplaySecond );
            }
            return;
        case 'p':
            displayBoundingBox( !m_kVolumeBox.GetDisplay() );
            return;
        case 'o':
            displayOrientationCube( !m_kVolumeCube.GetDisplay() );
            return;
        case 'c':
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
            	if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            	{
            		VolumeSurface kSurface = (VolumeSurface)m_kDisplayList.get(i);
            		kSurface.SetFrontface(true);
            	}
            }
            break;
        case 'C':
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
            	if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            	{
            		VolumeSurface kSurface = (VolumeSurface)m_kDisplayList.get(i);
            		kSurface.SetBackface(true);
            	}
            }
            break;
            /*


        case 'v':
            m_bDisplayEllipsoids = !m_bDisplayEllipsoids;
            return;
        case 'd':
            m_iEllipsoidMod--;
            if ( m_iEllipsoidMod < 1 )
            {
                m_iEllipsoidMod = 1;
            }
            return;
        case 'f':
            m_iEllipsoidMod++;
            if ( m_iEllipsoidMod > 100 )
            {
                m_iEllipsoidMod = 100;
            }
            return;
        case 'c':
            System.err.println(m_iActive);
            if (m_iActive == 0)
            {
                m_spkAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
                m_spkAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
                m_iActive = 1;
            }
            else if (m_iActive == 1)
            {
                // soft additive
                m_spkAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
                m_spkAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
                m_iActive = 2;
            }
            else
            {
                // multiplicative
                m_spkAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_DST_COLOR;
                m_spkAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ZERO;
                m_iActive = 0;
            }
            return;
                    */
        }
        m_kParent.setCameraParameters();
    	m_kParent.setObjectParameters();
        return;
    }

    /**
     * Display the volume in MIP mode.
     */
    public void MIPMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.MIPMode();
        }
        ResetShaderParamsWindow();
    }
    

    /** Rotates the object with a virtual trackball:
     * @param e the MouseEvent
     */
    public void mouseDragged(MouseEvent e)
    {
    	
        if ( !getSculptEnabled() )
        {
            if ( !e.isControlDown() )
            {
                super.mouseDragged(e);
            }
            else if ( e.isControlDown() && (m_kVolumeClip != null) && (m_kVolumeClip.DisplayArb()) )
            {
                InitializeObjectMotion(m_kVolumeClip.ArbRotate());
                super.mouseDragged(e);
                InitializeObjectMotion(m_spkScene);
                doClip(m_bArbClipOn);
            }
            else if ( e.isControlDown() && m_bPaintEnabled )
            {
                m_iXPick = e.getX();
                m_iYPick = e.getY();
                m_bPickPending = true;
            }
        }
        
        if ( m_kParent != null ) {
        	m_kParent.setCameraParameters();
        	m_kParent.setObjectParameters();
        }
    	if ( m_kVolumeRayCast != null ) {
    		m_kVolumeRayCast.setVolumeSamples( fSample_mouseDragged );
    	}
    }


    /** Rotates the object with a virtual trackball:
     * @param e the MouseEvent
     */
    public void mousePressed(MouseEvent e)
    {
        if ( !e.isControlDown() )
        {
            super.mousePressed(e);
        }
        else if ( (m_kVolumeClip != null) && (m_kVolumeClip.DisplayArb()) )
        {
            InitializeObjectMotion(m_kVolumeClip.ArbRotate());
            super.mousePressed(e);
            InitializeObjectMotion(m_spkScene);
        }
        if ( e.isControlDown() && m_bPaintEnabled )
        {
            m_iXPick = e.getX();
            m_iYPick = e.getY();
            m_bPickPending = true;
        }
        if ( e.isControlDown() && m_bGeodesicEnabled )
        {
            m_iXPick = e.getX();
            m_iYPick = e.getY();
            m_bPickPending = true;
        }
        if ( e.isControlDown() && m_bPickCorrespondence )
        {
            m_iXPick = e.getX();
            m_iYPick = e.getY();
            m_bPickPending = true;
        }
    }

    /** Rotates the object with a virtual trackball:
     * @param e the MouseEvent
     */
    public void mouseReleased(MouseEvent e)
    {
    	super.mouseReleased(e);
    	if ( m_kVolumeRayCast != null ) {
    		m_kVolumeRayCast.setVolumeSamples( fSample_mouseReleased );
    	}
    }


    /**
     * Display the volume in Mullti-histo mode.
     */
    public void MULTIHISTOMode(boolean bOn)
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.MULTIHISTOMode(bOn);
        }
        ResetShaderParamsWindow();
    }

    /**
     * Enables/disables picking correspondence points between the mesh and BrainSurfaceFlattener render.
     * @param bOn
     */
    public void pickCorrespondence( boolean bOn )
    {
        m_bPickCorrespondence = bOn;
    }
    
    /**
     * Causes the texture representation of all the surface meshes to be recalculated.
     */
    public void redrawSurfaceTexture()
    {
        m_bSurfaceUpdate = true;
    }
    
    public void play4D( boolean bOn )
    {
        m_bPlay4D = bOn;
    }

    /**
     * Called by the ApplicationGUI. Causes the current shader to be reloaded
     * from file, compiled and applied to the proxy-geometry.
     */
    public void reloadShaders()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.ReloadVolumeShader( m_pkRenderer );
        }
        updateLighting(m_akLights);
    }

    /**
     * Removes all geodesic components from the given surface.
     * @param kSurface
     */
    public void removeAllGeodesic( TriMesh kSurface )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetMesh() != null )
            {
                if ( m_kDisplayList.get(i).GetMesh() == kSurface)
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).RemoveAllGeodesic();
                }
            }
        }
    }

    /**
     * Removes the specific geodesic component from the given surface.
     * @param kSurface
     * @param iNode
     * @param iGroup
     */
    public void removeGeodesic( TriMesh kSurface, int iNode, int iGroup )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetMesh() != null )
            {
                if ( m_kDisplayList.get(i).GetMesh() == kSurface)
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).RemoveGeodesic(iNode, iGroup);
                }
            }
        }
    }

    /** 
     * Removes the specified polyline tract group.
     * @param iGroup the group of polylines to remove.
     */
    public void removePolyline( int iGroup )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.removePolyline(iGroup);
        }
        m_kDTIDisplay.SetDisplay( m_kDTIDisplay.GetDisplayTract() );
    }

    /**
     * Sets blending between imageA and imageB.
     * @param fValue the blend value (0-1)
     */
    public void removeSurface( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    VolumeObject kObj = m_kDisplayList.remove(i);
                    if ( kObj instanceof VolumeSurface )
                    {
                        m_bSurfaceUpdate = true;                        
                    }
                    kObj.dispose();
                    kObj = null;
                }
            }
        }
    }

    /**
     * When the Geodesic object cuts the mesh along an open curve, the old mesh changes, but does not need to be deleted
     * and no new mesh needs to be added. This function allows the Geodesic object to replace the original mesh with the
     * sliced mesh in the surface renderer. ReplaceMesh is also used to undo cutting operations.
     *
     * @param  kOld  TriMesh old surface mesh
     * @param  kNew  TriMesh new surface mesh
     */
    public void replaceGeodesic(TriMesh kOld, TriMesh kNew)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetMesh() != null )
            {
                if ( m_kDisplayList.get(i).GetMesh() == kOld)
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).ReplaceGeodesic(kNew);
                }
            }
        }
    }

    /**
     * Save the sculpted volume.
     * @param options file writing options
     * @param filtertype 
     * @return boolean, true on sucess.
     */
    public boolean save(FileWriteOptions options, int filterType)
    {
        if ( m_kSculptor == null )
        {
            return false;
        }
        return m_kSculptor.save(options, filterType);
    }
    
    /**
     * Copies the volume data from the texture representation into the ModelImage and writes it to disk.
     */
    public void saveImageFromTexture()
    {
        ModelImage kImage = VolumeImage.CreateImageFromTexture(m_kVolumeImageA.GetVolumeTarget().GetImage(), true);
        kImage.setImageName(JDialogBase.makeImageName(m_kVolumeImageA.GetImage().getImageName(), "_Crop"));
        kImage.copyFileTypeInfo(m_kVolumeImageA.GetImage());
        kImage.calcMinMax();
        kImage.saveImage(m_kVolumeImageA.GetImage().getImageDirectory(), kImage.getImageName(), kImage.getType(), true);
    }


    /**
     * Enables/Disables self-shadowing in the Surface mode.
     * @param bShadow shadow on/off.
     */
    public void selfShadow(boolean bShadow)
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SelfShadow(bShadow);
        }
    }

    /**
     * Sets blending between imageA and imageB.
     * @param fValue the blend value (0-1)
     */
    public void setABBlend( float fValue )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.setABBlend(fValue);
        }

        if ( m_kSlices != null )
        {
            m_kSlices.setABBlend(fValue);
        }
    }
    

    public void setAnimationSpeed( float fValue )
    {
        m_fAnimateRate = 32 * ( 1 - fValue );
    }

    /**
     * Sets the arbitrary clip plane color.
     * @param kColor the new color.
     */
    public void setArbColor( ColorRGB kColor )
    {
        kColor.R = (float)(kColor.R/255.0);
        kColor.G = (float)(kColor.G/255.0);
        kColor.B = (float)(kColor.B/255.0);
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setArbColor(kColor);
        }
    }

    /**
     * Enables the arbitrary clip plane position.
     * @param f4 clip position (same value as aSlice in JPanelClip)
     */
    public void setArbitraryClipPlane( float f4, boolean bEnable )
    {
        f4 /= (m_kVolumeImageA.GetImage().getExtents()[0] -1);     
        m_kArbitraryClip = new Vector4f(1,0,0,f4);
        doClip(bEnable);
    }
    
    /**
     * Turn backface culling on/off for the given surface.
     * @param kSurfaceName surface name.
     * @param bOn
     */
    public void setBackface(String kSurfaceName, boolean bOn)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).SetBackface( bOn );
                }
            }
        }
    }

    /**
     * Sets the background color.
     * @param kColor new background color.
     */
    public void setBackgroundColor( ColorRGBA kColor )
    {
        m_kBackgroundColor.Copy(kColor);
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetBackgroundColor( kColor );
        }
        if ( m_kSlices != null )
        {
            m_kSlices.SetBackgroundColor( kColor );
        }
    }    
    
    /**
     * Called from JPanelDisplay. Sets the bounding box color.
     * @param kColor bounding box color.
     */
    public void setBoundingBoxColor( ColorRGB kColor )
    {
        if ( m_kVolumeBox != null )
        {
            m_kVolumeBox.SetBoundingBoxColor(kColor);
        }
    }
    
    /**
     * Sets the color for the input slice frame.
     *
     * @param  i, the slice bounding frame.
     * @param  color  the new color.
     */
    public void setBoundingBoxColor( int i, ColorRGB kColor )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetBoundingBoxColor( i, kColor );
        }
    }

    /**
     * Sets the position of the 3D slices.
     * @param kCenter the intersection point of the three slices.
     */
    public void setCenter( Vector3f kCenter )
    {
        if ( m_kSlices != null )
        {
            int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
            m_kSlices.SetCenter( new Vector3f( (kCenter.X / (aiExtents[0] -1)),
                                               (kCenter.Y / (aiExtents[1] -1)),
                                               (kCenter.Z / (aiExtents[2] -1))  ) );

        }
    }

    /**
     * Enable clipping for the given surface.
     * @param kSurfaceName the surface to modify.
     * @param bClip true enables clipping, false disables clipping.
     */
    public void setClipping(String kSurfaceName, boolean bClip)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).SetClipping( bClip );
                }
            }
        }
    }

    /**
     * Called from JPanelClip. Sets the axis-aligned clip plane.
     * @param iWhich the clip plane to set.
     * @param fValue the new position of the clip plane (the same value as
     * the slider in JPanelClip).
     */
    public void setClipPlane( int iWhich, float fValue, boolean bEnable )
    {
        int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
        if ( iWhich < 2 )
        {
            fValue /= (aiExtents[0] -1);
        }
        else if ( iWhich < 4 )
        {
            fValue /= (aiExtents[1]-1);
        }
        else
        {
            fValue /= (aiExtents[2] -1);
        }
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setClipPlane(iWhich, fValue);
        }

        float[] data = new float[4];
        data[0] = fValue;
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClip(iWhich, fValue, bEnable);
        }  
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClip(iWhich,fValue,bEnable);
            }
        }
    }

    /**
     * Sets the axis-aligned clip plane color.
     * @param iWhich one of the 6 clip planes
     * @param kColor the new color.
     */
    public void setClipPlaneColor( int iWhich, ColorRGB kColor )
    {
        kColor.R = (float)(kColor.R/255.0);
        kColor.G = (float)(kColor.G/255.0);
        kColor.B = (float)(kColor.B/255.0);

        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setClipPlaneColor(iWhich, kColor);
        }
    }

    /**
     * Sets blending between imageA and imageB.
     * @param fValue the blend value (0-1)
     */
    public void setColor( String kSurfaceName, ColorRGB kColor )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    m_kDisplayList.get(i).SetColor( kColor );
                }
            }
        }
    }

    public void SetCustumBlend(int iBlendEquation, int iLogicOp, int iSrcBlend, int iDstBlend, ColorRGBA kColor  )
    {
        m_kVolumeRayCast.SetCustumBlend( iBlendEquation, iLogicOp, iSrcBlend, iDstBlend, kColor );
    }

    /** Turns on/off displaying all the glyphs.
     * @param bDisplay when true display all the glyphs in the volume.
     */
    public void setDisplayAllGlyphs( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayAllGlyphs(bDisplay);
        }
    }
    
    
    /** Turns on/off displaying the fiber bundle tracts with ellipsoids.
     * @param bDisplay when true display the tracts with Cylinders.
     */
    public void setDisplayCylinders( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayCylinders( bDisplay );
        }
    }

    /** Turns on/off displaying the fiber bundle tracts with ellipsoids.
     * @param bDisplay when true display the tracts with ellipsods.
     */
    public void setDisplayEllipsoids( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayEllipsoids( bDisplay );
        }
    }

    /** Turns on/off displaying the fiber bundle tracts with tubes.
     * @param bDisplay when true display the tracts with tubes.
     */
    public void setDisplayTubes( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayTubes( bDisplay );
        }
    }

    /** Turns on/off displaying the fiber bundle tracts with 3D arrows.
     * @param bDisplay when true display the tracts with 3D arrows.
     */
    public void setDisplayArrows( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayArrows( bDisplay );
        }
    }
    
    /**
     * Sets the sculpt drawing shape.
     * @param shape (0 = free-hand, 1 = rectangular)
     */
    public void setDrawingShape(int shape)
    {
        if ( m_kSculptor == null )
        {
            return;
        }
        m_kSculptor.setDrawingShape(shape);
    }

    /** Sets the DTI Image for displaying the tensors as ellipsoids.
     * @param kDTIImage.
     */
    public void setDTIImage( ModelImage kDTIImage, boolean bNegX, boolean bNegY, boolean bNegZ )
    {
        if ( m_kDTIDisplay == null )
        {
            m_kDTIDisplay = new VolumeDTI( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
            m_kDisplayList.add(1, m_kDTIDisplay);
        }
        m_kDTIDisplay.setDTIImage(kDTIImage, bNegX, bNegY, bNegZ, m_pkRenderer );
        m_bSurfaceUpdate = true;
    }


    /** Set the m_iEllipsoidMod value. 
     * @param iMod new m_iEllipsoidMod value.
     */
    public void setEllipseMod( int iMod )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setEllipseMod( iMod );
        }
    }

    /**
     * Sets the eye clip plane position.
     * @param f4 clip position (same value as sSlice in JPanelClip)
     * @param bDisplay on/off.
     */
    public void setEyeClipPlane( float f4, boolean bDisplay, boolean bEnable )
    {
        f4 /= (m_kVolumeImageA.GetImage().getExtents()[2] -1);
        float[] afEquation = new float[]{0,0,1,f4};
        float fZ = afEquation[3] * m_fZ;

        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.DisplayEye(bDisplay);
            m_kVolumeClip.setEyeClipPlane( fZ );
        }
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClipEye(afEquation, bEnable);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClipEye(afEquation, bEnable);
            }
        }
    }
    
    /**
     * Sets the eye clip plane color.
     * @param kColor the new color.
     */
    public void setEyeColor( ColorRGB kColor )
    {
        kColor.R = (float)(kColor.R/255.0);
        kColor.G = (float)(kColor.G/255.0);
        kColor.B = (float)(kColor.B/255.0);
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setEyeColor(kColor);
        }
    }

    /**
     * Sets the inverse-eye clip plane position.
     * @param f4 clip position (same value as sSliceInv in JPanelClip)
     * @param bDisplay on/off.
     */
    public void setEyeInvClipPlane( float f4, boolean bDisplay, boolean bEnable )
    {
        f4 /= (m_kVolumeImageA.GetImage().getExtents()[2] -1);
        float[] afEquation = new float[]{0,0,1,f4};
        float fZ = afEquation[3] * m_fZ;

        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.DisplayEyeInv(bDisplay);
            m_kVolumeClip.setEyeInvClipPlane( fZ );
        }
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClipEyeInv(afEquation, bEnable);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClipEyeInv(afEquation, bEnable);
            }
        }
    }
    
    /**
     * Sets the inverse-eye clip plane color.
     * @param kColor the new color.
     */
    public void setEyeInvColor( ColorRGB kColor )
    {
        kColor.R = (float)(kColor.R/255.0);
        kColor.G = (float)(kColor.G/255.0);
        kColor.B = (float)(kColor.B/255.0);
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setEyeInvColor(kColor);
        }
    }    
    
    
    /**
     * Enables/Disables Gradient Magnitude filter.
     * @param bShow gradient magnitude filter on/off
     */
    public void setGradientMagnitude(boolean bShow)
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetGradientMagnitude(bShow);
        }
    }  
    

    /**
     * Sets the ModelImage to use as an alternative to the volume ModelImage for surface texturing.
     * @param kSurfaceName the surface to modify.
     * @param kImage the alternate ModelImage to use for the surface texture.
     */
    public void setImageNew( String kSurfaceName, ModelImage kImage )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).SetImageNew(kImage);
                }
            }
        }
    }
    
    /**
     * Set intensity level for GPU-based surface extraction.
     * @param iLevel
     */
    public void setIntenstityLevel( int iLevel )
    {
        m_iExtractLevel = iLevel;
    }
    
    /**
     * Sets the inter-pupillary distance for stereo rendering.
     * @param fIPD the IPD value.
     */
    public void setIPD( float fIPD )
    {
        m_fTrnSpeed = fIPD;        
    }
    
    /**
     * Sets the LUT to use as an alternative to the volume lut for surface texturing.
     * @param kSurfaceName the surface to modify.
     * @param kLUT the new LUT.
     * @param kRGBT the new ModelRGB (for color images).
     */
    public void setLUTNew( String kSurfaceName, ModelLUT kLUT, ModelRGB kRGBT )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).SetLUTNew(kLUT, kRGBT);
                }
            }
        }
    }
    
    /**
     * Sets the material for the given surface.
     * @param kSurfaceName the surface to update.
     * @param kMaterial the new material.
     */
    public void setMaterial( String kSurfaceName, MaterialState kMaterial )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).SetMaterial(kMaterial);
                }
            }
        }
    }
    
    /**
     * Enable surface picking for the given surface.
     * @param kSurfaceName surface name.
     * @param bOn picking on/off.
     */
    public void setPickable(String kSurfaceName, boolean bOn)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    m_kDisplayList.get(i).SetPickable( bOn );
                }
            }
        }
    }

    
    /**
     * Set the polygon mode (FILL, LINE, POINT) for the given surface.
     * @param kSurfaceName the surface to modify.
     * @param eMode FILL, LINE, or POINT.
     */
    public void setPolygonMode(String kSurfaceName, WireframeState.FillMode eMode)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    m_kDisplayList.get(i).SetPolygonMode( true, eMode );
                }
            }
        }
    }
    
    /** Sets the polyline color for the specified fiber bundle tract group. 
     * @param iGroup the fiber bundle group to set.
     * @param kColor the new polyline color for the specified fiber bundle tract group. 
     */
    public void setPolylineColor( int iGroup, ColorRGB kColor )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setPolylineColor(iGroup, kColor);
        }
    }

    public void setRGBTA(ModelRGB RGBT) {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.setRGBTA(RGBT);
        } 
        if ( m_kSlices != null )
        {
            m_kSlices.setRGBTA(RGBT);
        }
    }
    
    public void setRGBTB(ModelRGB RGBT) {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.setRGBTB(RGBT);
        } 
        if ( m_kSlices != null )
        {
            m_kSlices.setRGBTB(RGBT);
        }
    }
    
    /**
     * Set the transparency value for the slice.
     * @param i the slice to modify.
     * @param fAlpha the new transparency value.
     */
    public void setSliceOpacity( int i, float fAlpha )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetSliceOpacity( i, fAlpha );
        }
    }

    /**
     * Enable/disable stereo rendering.
     * @param bEnable
     */
    public void setStereo( int iWhich )
    {
        m_iStereo = iWhich;
        if ( iWhich == 1 ) // Red/Green
        {
            m_kVolumeRayCast.SetDisplay(false);
        }
    }
      
    /**
     * Turns on surface texture display for the given surface. The user can use a separate ModelImage and LUT than the one displayed in the volume renderer.
     * @param kSurfaceName the name of the surface to texture.
     * @param bOn texture on/off.
     * @param bUseNewImage when false use the current ModelImage, when true the user specifies the model image.
     * @param bUseNewLUT when false use the current LUT, when true the user specifies the LUT.
     */
    public void setSurfaceTexture(String kSurfaceName, boolean bOn, boolean bUseNewImage, boolean bUseNewLUT)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).SetSurfaceTexture( bOn, bUseNewImage, bUseNewLUT );
                }
            }
        }
    }
    
    /** Sets the blend factor for displaying the ray-cast volume with other objects in the scene.
     * @param fBlend the blend factor for the ray-cast volume.
     */
    public void setVolumeBlend( float fBlend )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.setVolumeBlend(fBlend);
        }
    }
    
    /** Set the sampling rate when the mouse is released. */
    public void setVolumeSamplesMouseReleased( float fSample )
    {
    	fSample_mouseReleased = fSample;
    	if ( m_kVolumeRayCast != null )
    		m_kVolumeRayCast.setVolumeSamples( fSample );
    }
    
    /** Set the sampling rate when the mouse is Dragged. */
    public void setVolumeSamplesMouseDragged( float fSample )
    {
    	fSample_mouseDragged = fSample;
    	if ( m_kVolumeRayCast != null )
    		m_kVolumeRayCast.setVolumeSamples( fSample );
    }
    
    
    
    /** Turns on/off displaying the bounding box for the given plane.
     * @param i the plane index (0-3) in file coordinates.
     * @param bShow when true, the bounding box is displayed.
     */
    public void showBoundingBox( int i, boolean bShow )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.ShowBoundingBox( i, bShow );
        }
    }
    /** Turns on/off displaying the given plane.
     * @param i the plane index (0-3) in file coordinates.
     * @param bShow when true, the plane is displayed.
     */
    public void showSlice( int i, boolean bShow )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.ShowSlice( i, bShow );
        }
    }
        
    /**
     * Smooth the given surface.
     * @param kSurfaceName the name of the surface to smooth.
     * @param iteration smooth iterations.
     * @param alpha smooth factor.
     * @param volumeLimit whether to use a volume % change limit.
     * @param volumePercent the % volume change limiting factor
     */
    public void smoothMesh( String kSurfaceName, int iteration, float alpha, boolean volumeLimit, float volumePercent)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).smoothMesh(iteration, alpha, volumeLimit, volumePercent);
                }
            }
        }
    }
  
    
    /**
     * Smooth the given surface.
     * @param kSurfaceName the name of the surface to smooth.
     * @param iteration smooth iterations.
     * @param lambda smooth factor.
     * @param mu smooth factor.
     */
    public void smoothThree( String kSurfaceName, int iteration, float lambda, float mu)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).smoothThree(iteration, lambda, mu);
                }
            }
        }
    }
    
    /**
     * Smooth the given surface.
     * @param kSurfaceName the name of the surface to smooth.
     * @param iteration smooth iterations.
     * @param fStiffness stiffness factor.
     * @param volumeLimit whether to use a volume % change limit.
     * @param volumePercent the % volume change limiting factor.
     */
    public void smoothTwo( String kSurfaceName, int iteration, float fStiffness, boolean volumeLimit, float volumePercent)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).smoothTwo(iteration, fStiffness, volumeLimit, volumePercent);
                }
            }
        }
    }
    /**
     * Display the volume in Surface mode.
     */
    public void SURFASTMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SURFASTMode();
        }
        ResetShaderParamsWindow();
    }
    
    /**
     * Display the volume in Composite Surface mode.
     */
    public void SURMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SURMode();
        }
        ResetShaderParamsWindow();
    }    
    
    /**
     * Switches between different ways of displaying the geodesic path (Euclidean, Geodesic, or Mesh).
     * @param kSurfaceName the surface the path is on.
     * @param iWhich the type of display.
     */
    public void toggleGeodesicPathDisplay(String kSurfaceName, int iWhich)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)(m_kDisplayList.get(i))).ToggleGeodesicPathDisplay(iWhich);
                }
            }
        }
    }

    
    /**
     * Changes the translation vector for the surface with the given name.
     * @param kSurfaceName the surface to move.
     * @param kTranslate the new translation vector
     */
    public void translateSurface(String kSurfaceName, Vector3f kTranslate)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    m_kDisplayList.get(i).Translate(kTranslate);
                }
            }
        }
    }
    
    /**
     * Return the translation vector for the surface with the given name.
     * @param kSurfaceName the surface to move.
     * @return the translation vector
     */
    public Vector3f getTranslateSurface(String kSurfaceName)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    return m_kDisplayList.get(i).GetTranslate();
                }
            }
        }
        return new Vector3f(Vector3f.ZERO);
    }
    
    /**
     * Undo applying the sculpt region to the volume.
     */
    public void undoSculpt(boolean bAll)
    {
        if ( m_kSculptor == null )
        {
            return;
        }
        int iTSize = (m_kVolumeImageA.GetImage().is4DImage()&&bAll) ? m_kVolumeImageA.GetImage().getExtents()[3] : 1;
        for ( int i = 0; i < iTSize; i++ )
        {
            if ( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() != null )
            {
                m_kSculptor.setTextureImageDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() );
            }
            else
            {
                m_kSculptor.setTextureImageFloatDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetFloatData() );
            }
            if ( m_kVolumeImageB.GetImage() != null )
            {
                if ( m_kVolumeImageB.GetVolumeTarget().GetImage().GetData() != null )
                {
                    m_kSculptor.setTextureImageDataB( m_kVolumeImageB.GetVolumeTarget().GetImage().GetData() );
                }
                else
                {
                    m_kSculptor.setTextureImageFloatDataB( m_kVolumeImageB.GetVolumeTarget().GetImage().GetFloatData() );
                }
            }
            m_kSculptor.undoSculpt(m_kVolumeImageA.GetTimeSlice());
            m_kVolumeImageA.ReleaseVolume();
            if ( m_kVolumeImageB.GetImage() != null )
            {
                m_kVolumeImageB.ReleaseVolume();
            }
            if ( iTSize > 1 )
            {
                m_kVolumeImageA.update4D(true);
            }
        }
        m_kParent.setModified();
    }
    
    private void update4D( boolean bForward )
    {
        m_kVolumeImageA.update4D(bForward);
        m_kParent.setModified();
    }
    
    /**
     * Causes the VolumeShader to update the copy of the ModelImage on the
     * GPU.
     * @param kImage the new image.
     */
    public void updateData()
    {
        if ( m_kSculptor != null )
        {
            m_kSculptor.setImage(m_kVolumeImageA.GetImage(), m_kVolumeImageB.GetImage());
        }

    }    
    
    public void updateLevWidgetState( ClassificationWidgetState kLWS, int iState )
    {
        m_kVolumeRayCast.updateLevWidgetState( kLWS, iState );
    }
    
    /**
     * Picking. If a display list object has picking enabled, find the picked polygon based on the mouse position. 
     */
    protected void Pick()
    {
        Vector3f kPos = new Vector3f(0,0,10);
        Vector3f kDir = new Vector3f(0,0,1);  // the pick ray

        if (m_bPickPending)
        {
            if (m_spkCamera.GetPickRay(m_iXPick,m_iYPick,GetWidth(),
                                       GetHeight(),kPos,kDir))
            {
                m_bPickPending = false;
                for ( int i = 0; i < m_kDisplayList.size(); i++ )
                {
                    if ( m_kDisplayList.get(i).GetPickable() )
                    {
                        m_kPicker.Execute(m_kDisplayList.get(i).GetScene(),kPos,kDir,0.0f,
                                          Float.MAX_VALUE);
                        if (m_kPicker.Records.size() > 0)
                        {
                            //System.err.println( kPos.X() + " " + kPos.Y() + " " + kPos.Z() );
                            //System.err.println( kDir.X() + " " + kDir.Y() + " " + kDir.Z() );
                            if ( m_bPaintEnabled )
                            {
                                //System.err.println("Picked " + m_kDisplayList.get(i).getClass().getName());
                                if ( m_bPaint )
                                {
                                    m_kDisplayList.get(i).Paint( m_pkRenderer, m_kPicker.GetClosestNonnegative(), m_kPaintColor, m_iBrushSize );
                                }
                                else if ( m_bDropper || m_bPaintCan )
                                {
                                    ColorRGBA kDropperColor = new ColorRGBA();
                                    Vector3f kPickPoint = new Vector3f();
                                    m_kDisplayList.get(i).Dropper( m_kPicker.GetClosestNonnegative(), kDropperColor, kPickPoint );
                                    m_kParent.setDropperColor( kDropperColor, kPickPoint );
                                } 
                                else if ( m_bErase )
                                {
                                    m_kDisplayList.get(i).Erase( m_pkRenderer, m_kPicker.GetClosestNonnegative(), m_iBrushSize );
                                }
                            }
                            if ( m_bGeodesicEnabled )
                            {
                                m_kParent.setGeodesic( m_kDisplayList.get(i).GetMesh(), m_kPicker.GetClosestNonnegative() );
                            }
                            if ( m_bPickCorrespondence )
                            {
                                PickRecord kRecord = m_kPicker.GetClosestNonnegative();
                                m_kParent.PickCorrespondence( kRecord.iV0, kRecord.iV1, kRecord.iV2 );
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Called by the init() function. Creates and initialized the scene-graph.
     * @param arg0 the GLCanvas
     */
    private void CreateScene (GLAutoDrawable arg0)
    {
        // Create a scene graph with the face model as the leaf node.
        m_spkScene = new Node();
        m_spkCull = new CullState();
        m_spkScene.AttachGlobalState(m_spkCull);
        /*
        m_spkAlpha = new AlphaState();
        m_spkAlpha.BlendEnabled = false;
        m_spkAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
        m_spkAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_spkScene.AttachGlobalState(m_spkAlpha);
        */

        m_kVolumeRayCast = new VolumeRayCast( m_kVolumeImageA, m_kVolumeImageB );
        m_kDisplayList.add(0, m_kVolumeRayCast);
        m_kVolumeRayCast.CreateScene( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight, arg0, m_pkRenderer );

        m_kTranslate = m_kVolumeRayCast.GetTranslate();

        m_fX = m_kVolumeImageA.GetScaleX();
        m_fY = m_kVolumeImageA.GetScaleY();
        m_fZ = m_kVolumeImageA.GetScaleZ();

        m_kSlices = new VolumeSlices( m_kVolumeImageA, m_kVolumeImageB, m_kTranslate, m_fX, m_fY, m_fZ );
        displayVolumeSlices( true );
        m_kDisplayList.add(m_kDisplayList.size(), m_kSlices);

        m_kVolumeClip = new VolumeClip( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
        m_kDisplayList.add( m_kDisplayList.size(), m_kVolumeClip);

        m_kVolumeBox = new VolumeBoundingBox( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
        m_kDisplayList.add( m_kDisplayList.size(), m_kVolumeBox);

        m_kVolumeCube = new VolumeOrientationCube( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
        m_kDisplayList.add( m_kDisplayList.size(), m_kVolumeCube);

        for ( int i = 0; i < m_pkRenderer.GetMaxLights(); i++ )
        {
            m_pkRenderer.SetLight( i, new Light() );
        }
        
        m_kParent.addSlices(m_kSlices);
    }
    
    
    /**
     * Calculates the rotation for the arbitrary clip plane.
     * @param bEnable arbitrary clip plane on/off.
     */
    private void doClip( boolean bEnable ) 
    {           
        m_bArbClipOn = bEnable;
        if ( m_kArbitraryClip == null )
        {
            m_kArbitraryClip = new Vector4f(1,0,0,0);            
        }
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.SetArbPlane( m_kArbitraryClip.W * m_fX );
        }

        // Rotate normal vector:
        Matrix3f kClipRotate = m_kVolumeClip.ArbRotate().Local.GetRotate();
        Vector3f kNormal = new Vector3f( 1,0,0 );
        kClipRotate.Mult(kNormal, kNormal);
        kNormal.Normalize();

        // Scale kNormal based on the scaled volume:
        kNormal.Set( kNormal.X * m_fX, kNormal.Y * m_fY, kNormal.Z * m_fZ );
        float fLength = kNormal.Length();
        kNormal.Normalize();
        m_afArbEquation[0] = kNormal.X;
        m_afArbEquation[1] = kNormal.Y;
        m_afArbEquation[2] = kNormal.Z;

        // Calculate the distance to the plane, scaled based on the scaled kNormal:
        Vector3f kPos = new Vector3f();
        kPos.Scale( (m_kArbitraryClip.W - 0.5f)/fLength, kNormal );
        kPos.Add( new Vector3f( .5f, .5f, .5f ));
        m_afArbEquation[3] = kNormal.Dot(kPos);   
        
        // Update shader with rotated normal and distance:
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClipArb(m_afArbEquation, bEnable);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClipArb(m_afArbEquation, bEnable);
            }
        }
    }
    
    /**
     * Render the display list objects without the raycast volume.
     */
    private void Render()
    {

        if ( m_iStereo == 0 )
        { 
            m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
            m_pkRenderer.ClearBuffers();
            if ( !m_bDisplaySecond )
            {
                for ( int i = 0; i < m_kDisplayList.size(); i++ )
                {
                    m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller, true, true );
                }
            }
            else
            {
                if ( m_kVolumeRayCast.GetDisplay() )
                {
                    RenderWithTransparency(true);            
                }
                RenderWithTransparency(false);
                if ( m_kVolumeRayCast.GetDisplay() )
                {
                    m_kVolumeRayCast.Render( m_pkRenderer, m_kCuller, false, true );
                    m_pkRenderer.SetCamera(m_spkCamera);
                }
                RenderSculpt();
                
                RenderFrameRate();                
                if ( m_bSnapshot )
                {
                    writeImage();
                }
                m_pkRenderer.DisplayBackBuffer();
            }
        }
        else
        {          
            m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
            MoveRight();
            {
                m_kCuller.ComputeVisibleSet(m_spkScene);
                if ( m_iStereo == 1 ) // red/green:
                {
                    m_pkRenderer.ClearBuffers();
                    m_pkRenderer.SetColorMask( false, false, true, true );
                }
                else // shutter:
                {
                    m_pkRenderer.DrawRight();
                    m_pkRenderer.ClearBuffers();
                }           
                if ( m_kVolumeRayCast.GetDisplay() )
                {
                    RenderWithTransparency(true);            
                }
                RenderWithTransparency(false);
                if ( m_kVolumeRayCast.GetDisplay() )
                {
                    m_kVolumeRayCast.Render( m_pkRenderer, m_kCuller, false, true );
                    m_pkRenderer.SetCamera(m_spkCamera);
                }
                if ( m_iStereo == 1 ) // red/green:
                {
                    m_pkRenderer.ClearZBuffer();
                }                
                else // shutter:
                {
                    m_pkRenderer.DisplayBackBuffer();
                }
            }
            MoveLeft();
            MoveLeft();
            m_kCuller.ComputeVisibleSet(m_spkScene);  
            if ( m_iStereo == 1 ) // red/green:
            {                
                m_pkRenderer.SetColorMask( true, false, false, true );
            }
            else
            {
                m_pkRenderer.DrawLeft();
                m_pkRenderer.ClearBuffers();
            }
            if ( m_kVolumeRayCast.GetDisplay() )
            {
                RenderWithTransparency(true);            
            }
            RenderWithTransparency(false);
            if ( m_kVolumeRayCast.GetDisplay() )
            {
                m_kVolumeRayCast.Render( m_pkRenderer, m_kCuller, false, true );
                m_pkRenderer.SetCamera(m_spkCamera);
            }
                        
            RenderFrameRate();                
            if ( m_bSnapshot )
            {
                writeImage();
            }
            m_pkRenderer.DisplayBackBuffer();
            
            MoveRight();
            m_pkRenderer.SetColorMask( true, true, true, true );
            m_pkRenderer.DrawDefault();
        }
    }

    /**
     * 
     * Rendering multiple translucent surfaces is accomplished with the following steps:
     *   1. A FrameBuffer Object (FBO) is created with three render-to-texture targets, and a depth buffer.
     *   2. The first render target is activated. Solid objects are rendered: writing color and depth
     *      information to the FBO target1. The color information displays the solid objects, the depth
     *      information enables the solid objects to properly occlude the translucent objects.
     *   3. The second and third render targets are activated. Translucent objects are rendered: writing
     *      accumulated color and alpha to the second target and the number of overlapping surfaces per
     *      pixel to the third target. In this pass the depth buffer is read-only.
     *   4. Rendering to the three texture targets is disabled.
     *   5. A screen-space polygon is rendered, it reads the three textures generated in the previous pass.
     *      The first texture contains the color from the solid objects, the next two textures contain the
     *      color information and weighted-average for rendering the translucent textures.
     *
     * @param bPreRender when true rendering into the VolumeRayCast back-image texture for compositing with 
     * the VolumeRayCast rendering.
     */
    private void RenderWithTransparency(boolean bPreRender)
    {
        // First: render opaque objects to an off-screen color/depth buffer
        //m_kSolidFBO.Enable();        
        m_kFBO.Enable();
        m_kFBO.DrawBuffers(new int[]{0});
        m_pkRenderer.SetBackgroundColor( m_kBackgroundColor );
        m_pkRenderer.ClearBuffers();

        if ( bPreRender )
        {
            m_kVolumeRayCast.Render( m_pkRenderer, m_kCuller, bPreRender, true );
        }
        for ( int i = 1; i < m_kDisplayList.size(); i++ )
        {
            m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller, bPreRender, true );
        }

        // Second: render semi-transparent objects:
        m_kFBO.DrawBuffers(new int[]{1,2});
        m_pkRenderer.SetBackgroundColor( new ColorRGBA(0.0f, 0.0f,0.0f,0.0f));
        m_pkRenderer.ClearBackBuffer();
        
        for ( int i = 1; i < m_kDisplayList.size(); i++ )
        {
            m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller, bPreRender, false );
        }

        m_kFBO.Disable();        
        
        m_pkRenderer.SetCamera(m_pkScreenCamera);
        
        if ( bPreRender )
        {
            m_kVolumeRayCast.PreRenderB(m_pkRenderer);
        }
        m_pkRenderer.SetBackgroundColor( m_kBackgroundColor );
        m_pkRenderer.ClearBuffers();
        m_pkRenderer.Draw( m_pkPlane ); 
        if ( bPreRender )
        { 
            m_kVolumeRayCast.PostPreRenderB(m_pkRenderer);
        }
        
        m_pkRenderer.SetCamera(m_spkCamera);
    }
    
    
    /**
     * Render the sculpt image.
     */
    private void RenderSculpt()
    {
        if ( (m_kSculptor != null) && m_kSculptor.IsSculptDrawn() )
        {
            m_pkRenderer.Draw( m_kSculptor.getSculptImage() );
        }
    }

    
    /**
     * Render the display list objects embedded in the ray-cast volume.
     * This is a two-pass rendering. First the background is rendered with all objects displayed with
     * the color corresponding to the texture coordinates. The resulting color image is used to calculate
     * the ray end-points for rendering the ray-cast volume. In the second pass the display list
     * objects are rendered normally and the ray-cast volume is rendered last.
    private void RenderVolume()
    {
        if ( !m_bDisplaySecond )
        {
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller, true, true );
            }
        }
        else
        {
            RenderWithTransparency(true);
            RenderWithTransparency(false);
            m_kVolumeRayCast.Render( m_pkRenderer, m_kCuller, false, true );
            m_pkRenderer.SetCamera(m_spkCamera);
        }
    }
     */
    
    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
        super.reshape( arg0, iX, iY, iWidth, iHeight );
        UpdateRenderTarget(iWidth, iHeight);
        m_kVolumeRayCast.reshape(iWidth, iHeight);
    }
    /**
     * Closes the shader parameters window.
     */
    private void ResetShaderParamsWindow()
    {
        if ( m_kShaderParamsWindow != null )
        {
            m_kShaderParamsWindow.close();
        }
    }
    
    /**
     * Add tubes group color. 
     * @param groupID   group ID
     */
    public void addGroupColor() {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.addGroupColor();
        }
    }
    
    /**
     * reduce the fiber tract color index
     * @param groupID   group ID
     */
    public void removeTractColor(Integer index) {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.removeTractColor(index);
        }
    }
    
    /**
     * reduce the group color index
     */
    public void removeGroupColor() {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.removeGroupColor();
        }
    }
    
    /** Sets the polyline color for the specified fiber bundle tract group. 
     * @param iGroup the fiber bundle group to set.
     * @param kColor the new polyline color for the specified fiber bundle tract group. 
     */
    public void setTubesGroupColor( int iGroup, ColorRGB kColor )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setTubesGroupColor(iGroup, kColor);
        }
    }
    
    /** Get the group color with given group ID. 
     * @param groupID  given group id
     * @return  ColorRGB group color
     */
    public ColorRGB getGroupColor(int groupID) {
        if ( m_kDTIDisplay != null )
        {
            return m_kDTIDisplay.getGroupColor(groupID);
        }
        return null;
    }
    
    
    public void setVolumeColor(boolean flag) {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setVolumeColor(flag);
        }
    }
    
    

    

    /**
     * Creates the three render-to-texture targets for rendering semi-transparent surfaces. 
     * Creates the screen-space polygon and OrderIndTransparencyEffect that display the
     * final result.
     * The first render target is for the solid objects.
     * Second render target is for translucent objects.
     * Third render target is for accumulating the count of translucent objects per-pixel.
     * @param arg0
     * @param iWidth
     * @param iHeight
     */
    private void CreateRenderTarget( GLAutoDrawable arg0, int iWidth, int iHeight )
    {        
        Texture[] akSceneTarget = new Texture[3];
        GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,(byte[])null,
                "ColorTex0");
        akSceneTarget[0] = new Texture();
        akSceneTarget[0].SetImage(pkSceneImage);
        akSceneTarget[0].SetShared(true);
        akSceneTarget[0].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[0].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[0].SetWrapType(1,Texture.WrapType.CLAMP);
        //akSceneTarget[0].SetSamplerInformation( new SamplerInformation( "ColorTex0", 0, 0 ) );
        m_pkRenderer.LoadTexture( akSceneTarget[0] );
        
        
        pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,(float[])null,
        "ColorTex1");
        akSceneTarget[1] = new Texture();
        akSceneTarget[1].SetImage(pkSceneImage);
        akSceneTarget[1].SetShared(true);
        akSceneTarget[1].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[1].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[1].SetWrapType(1,Texture.WrapType.CLAMP);
        //akSceneTarget[1].SetSamplerInformation( new SamplerInformation( "ColorTex1", 0, 0 ) );
        m_pkRenderer.LoadTexture( akSceneTarget[1] );
        
        pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,(float[])null,
                "ColorTex2");
        akSceneTarget[2] = new Texture();
        akSceneTarget[2].SetImage(pkSceneImage);
        akSceneTarget[2].SetShared(true);
        akSceneTarget[2].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[2].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[2].SetWrapType(1,Texture.WrapType.CLAMP);
        //akSceneTarget[2].SetSamplerInformation( new SamplerInformation( "ColorTex2", 0, 0 ) );
        m_pkRenderer.LoadTexture( akSceneTarget[2] );
      
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        m_pkPlane = kSM.Rectangle(2,2,1.0f,1.0f);    
            
        m_spkPlaneEffect = new OrderIndpTransparencyEffect(akSceneTarget );
        m_pkPlane.AttachEffect( m_spkPlaneEffect);
        
        ZBufferState kZState = new ZBufferState();
        kZState.Enabled = true;
        kZState.Writable = false;
        m_pkPlane.AttachGlobalState(kZState);
        m_pkPlane.UpdateGS();
        m_pkPlane.UpdateRS();
        
        m_kFBO = new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer,akSceneTarget, arg0);
        

        m_pkScreenCamera = new Camera();
        m_pkScreenCamera.Perspective = false;
        m_pkScreenCamera.SetFrustum(-1,1,-1,1,1f,10.0f);
    }
    

    private void UpdateRenderTarget( int iWidth, int iHeight )
    {        
        Texture[] akSceneTarget = new Texture[3];
        for ( int i = 0; i < 3; i++ )
        {
            akSceneTarget[i] = m_kFBO.GetTarget(i);
            akSceneTarget[i].GetImage().SetData((byte[])null, iWidth, iHeight );
            akSceneTarget[i].Release();
            m_pkRenderer.LoadTexture( akSceneTarget[i] );
        }
        m_kFBO.TerminateBuffer();
        for ( int i = 0; i < 3; i++ )
        {
            m_kFBO.SetTarget(i, akSceneTarget[i]);
        }
        m_kFBO.InitializeBuffer();
        m_kFBO.Enable();
        m_kFBO.Disable();     
    }
    
    protected void UpdateSceneRotation()
    {
        super.UpdateSceneRotation();
        m_kSceneToWorld.Copy(m_kVolumeRayCast.GetWorld());
    }
}
