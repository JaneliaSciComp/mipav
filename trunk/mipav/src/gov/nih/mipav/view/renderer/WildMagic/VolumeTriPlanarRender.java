package gov.nih.mipav.view.renderer.WildMagic;

import javax.media.opengl.*;
import com.sun.opengl.util.*;

import java.awt.Cursor;
import java.awt.event.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.WildMagic.Render.*;

import WildMagic.LibApplications.OpenGLApplication.*;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Collision.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibRenderers.OpenGLRenderer.*;

public class VolumeTriPlanarRender extends GPURenderBase
implements GLEventListener, KeyListener, MouseMotionListener,  MouseListener
{

    /** New sculpting object for WM-based sculpting. */
    private Sculptor_WM m_kSculptor = null;

    /** Stores the transfer functions: */
    private TransferFunction[] m_akTransfer = new TransferFunction[4];

    /** Arbitrary clip plane equation: */
    private Vector4f m_kArbitraryClip;

    /** Window with the shader paramter interface: */
    private ApplicationGUI m_kShaderParamsWindow = null;

    /** Enables/Disables rendering the second pass. When disabled, the
     * back-facing polygons of the proxy-geometry are shown instead of the volume: */
    private boolean m_bDisplaySecond = true;

    private VolumeSlices m_kSlices = null;
    private VolumeRayCast m_kVolumeRayCast = null;
    private VolumeClip m_kVolumeClip = null;
    private VolumeDTI m_kDTIDisplay = null;
    private VolumeBoundingBox m_kVolumeBox = null;
    private VolumeOrientationCube m_kVolumeCube = null;

    private boolean m_bFirstRender = true;

    private float[] m_afArbEquation = new float[4];

    private boolean m_bPaintEnabled = false;
    private boolean m_bPaint = false;
    private boolean m_bErase = false;
    private boolean m_bDropper = false;
    private boolean m_bPaintCan = false;
    private ColorRGBA m_kPaintColor = null;
    private int m_iBrushSize = 1;
    
    private boolean m_bGeodesicEnabled = false;

    private boolean m_bStereo = false;
    private boolean m_bLeft = true;
    private boolean m_bRight = true;    

    private boolean m_bPickCorrespondence = false;


    public VolumeTriPlanarRender( final String acWindowTitle, int iXPosition,
            int iYPosition, int iWidth, int iHeight,
            final ColorRGBA rkBackgroundColor )
    {
        super("GPUVolumeRender",0,0,512,512, new ColorRGBA(0.0f,0.0f,0.0f,0.0f));
    }
    
    
    /**
     * Constructs a new GPUVolumeRender object.
     * @param kImageA ModelImage A
     * @param kLUTa, LUT for ModelImage A
     * @param kRGBTa, RGB lookup table for ModelImage A
     * @param kImageB ModelImage B
     * @param kLUTb, LUT for ModelImage B
     * @param kRGBTb, RGB lookup table for ModelImage B
     */
    public VolumeTriPlanarRender( VolumeTriPlanarInterface kParent, Animator kAnimator, VolumeImage kVolumeImageA, ModelImage kImageA, ModelLUT kLUTa, ModelRGB kRGBTa,
            VolumeImage kVolumeImageB, ModelImage kImageB, ModelLUT kLUTb, ModelRGB kRGBTb  )
    {
        super("GPUVolumeRender",0,0,512,512, new ColorRGBA(0.0f,0.0f,0.0f,0.0f));
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       

        m_kAnimator = kAnimator;
        m_kVolumeImageA = kVolumeImageA;
        m_kImageA = kImageA;
        m_kLUTa = kLUTa;
        m_kRGBTa = kRGBTa;

        m_kVolumeImageB = kVolumeImageB;
        m_kImageB = kImageB;
        m_kLUTb = kLUTb;
        m_kRGBTb = kRGBTb;
        m_kParent = kParent;
        
        m_kRotate.FromAxisAngle(Vector3f.UNIT_Z, (float)Math.PI/18.0f);
    }
    
    
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


    public void addNode(Node kNode)
    {
        m_kDisplayList.add( new VolumeNode( m_pkRenderer, m_kVolumeImageA,
                m_kTranslate,
                m_fX, m_fY, m_fZ, kNode) );
        UpdateSceneRotation();
    }

    /** Add a polyline to the display. Used to display fiber tract bundles.
     * @param kLine, new polyline to display.
     * @param iGroup, the group the polyline belongs to.
     */
    public void addPolyline( Polyline kLine, int iGroup )
    {
        if ( m_kDTIDisplay == null )
        {
            m_kDTIDisplay = new VolumeDTI( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
            m_kDisplayList.add(m_kDisplayList.size(), m_kDTIDisplay);
        }
        m_kDTIDisplay.addPolyline( kLine, iGroup );
        m_kDTIDisplay.SetDisplay( true );
    }

    public VolumeSurface[] addSurface(TriMesh[] akSurfaces, boolean bReplace)
    {
        VolumeSurface[] akVolumeSurfaces = new VolumeSurface[akSurfaces.length];
        for ( int i = 0; i < akSurfaces.length; i++ )
        {
            akVolumeSurfaces[i] = new VolumeSurface( m_pkRenderer, m_kVolumeImageA,
                    m_kTranslate,
                    m_fX, m_fY, m_fZ,
                    akSurfaces[i], bReplace );
            akVolumeSurfaces[i].SetPerPixelLighting( m_pkRenderer, true );
            m_kDisplayList.add( akVolumeSurfaces[i] );
        }
        UpdateSceneRotation();
        m_bSurfaceAdded = true;
        return akVolumeSurfaces;
    }


    public VolumeSurface[] getSurfaces( String[] akSurfaceNames )
    {
        VolumeSurface[] akVolumeSurfaces = new VolumeSurface[akSurfaceNames.length];
        int iCount = 0;
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                for ( int j = 0; j < akSurfaceNames.length; j++ )
                {
                    if ( m_kDisplayList.get(i).GetName().equals(akSurfaceNames[j]))
                    {
                        akVolumeSurfaces[iCount++] = (VolumeSurface)m_kDisplayList.get(i);
                    }
                }
            }
        }
        return akVolumeSurfaces;
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
            m_kDisplayList.add(m_kDisplayList.size(), m_kDTIDisplay);
        }
        m_kDTIDisplay.setCenterIndex(centerIndex);
        m_kDTIDisplay.addPolyline( kLine, iGroup );
        m_kDTIDisplay.SetDisplay( true );
       
    }

    /**
     * Apply the sculpt region to the volume.
     */
    public void applySculpt()
    {
        if ( m_kSculptor == null )
        {
            return;
        }

        float[] afData = new float[16];
        m_pkRenderer.SetConstantWVPMatrix (0, afData);
        m_kSculptor.setWVPMatrix(new Matrix4f(afData, true));
        if ( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() != null )
        {
            m_kSculptor.setTextureImageDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() );
        }
        else
        {
            m_kSculptor.setTextureImageFloatDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetFloatData() );
        }
        /*
        if ( m_kImageB != null )
        {
            if ( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetData() != null )
            {
                m_kSculptor.setTextureImageDataB( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetData() );
            }
            else
            {
                m_kSculptor.setTextureImageFloatDataB( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetFloatData() );
            }
        }
        */
        if ( m_kSculptor.applySculpt() )
        {
            m_kVolumeImageA.ReleaseVolume();
            m_kSculptor.clearSculpt();
            m_kParent.setModified();
        }
    }

    /**
     * Sets blending between imageA and imageB.
     * @param fValue, the blend value (0-1)
     */
    public void Blend( float fValue )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.Blend(fValue);
        }
    }

    
    /**
     * Sets blending between imageA and imageB.
     * @param fValue, the blend value (0-1)
     */
    public void Blend( String kSurfaceName, float fValue )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    m_kDisplayList.get(i).Blend( fValue );
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
            m_kVolumeRayCast.CMPMode(m_pkRenderer);
        }
        ResetShaderParamsWindow();
    }

    /**
     * Display the volume in DDR mode.
     */
    public void DDRMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.DDRMode(m_pkRenderer);
        }
        ResetShaderParamsWindow();
    }

    /**
     * Part of the GLEventListener interface.
     * display is called by the Animator object.
     * @param arg0, the GLAutoDrawable (GLCanvas) to display.
     */
    public void display(GLAutoDrawable arg0) {
        if ( !m_bVisible )
        {
            return;
        }
        
        if ( m_kImageA == null ) {
        	return;
        }
        
        if ( !m_bInit )
        {
            init(arg0);
        }
        
        if ( m_bFirstRender )
        {
            m_kVolumeRayCast.SetDisplay(true);   
            m_kSlices.SetDisplay(false);   
        }

        //((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        MeasureTime();

        Move();
        Pick();

        // Draw the scene to the back buffer/
        if (m_pkRenderer.BeginScene())
        {
            m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
            m_pkRenderer.ClearBuffers();

            
            if ( m_kVolumeRayCast.GetDisplay() )
            {
                RenderVolume();
            }
            else
            {
                RenderNoVolume();
            }
            RenderFrameRate();
            RenderSculpt();
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();

        UpdateFrameCount();

        if ( m_bFirstRender )
        {
            m_bFirstRender = false;
            m_kVolumeRayCast.SetDisplay(false);   
            m_kSlices.SetDisplay(true);   
            m_kParent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            VolumeImageViewer.main(m_kVolumeImageA);
            m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            CMPMode();
        }
        if ( m_bSurfaceAdded )
        {
            m_bSurfaceAdded = false;
            updateLighting( m_akLights );
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
    public void DisplayBoundingBox( boolean bDisplay )
    {
        if ( m_kVolumeBox != null )
        {
            m_kVolumeBox.SetDisplay(bDisplay);
        }
    }

    /**
     * Called from JPanelClip. Sets the axis-aligned clip plane display on/off.
     * @param iWhich, the clip plane to set.
     * @param bDisplay on/off.
     */
    public void displayClipPlane( int iWhich, boolean bDisplay )
    {
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.displayClipPlane(iWhich, bDisplay);
            //m_kVolumeClip.SetDisplay(bDisplay);
        }
    }

    /**
     * Called from JPanelClip. Sets the axis-aligned clip plane display on/off and color.
     * @param iWhich, the clip plane to set.
     * @param bDisplay on/off.
     * @param kColor, the clipping plane color.
     */
    public void displayClipPlane( int iWhich, boolean bDisplay, ColorRGB kColor )
    {
        setClipPlaneColor( iWhich, kColor );
        displayClipPlane( iWhich, bDisplay );
    }

    public void DisplayNode( Node kNode, boolean bDisplay )
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
    public void DisplayOrientationCube( boolean bDisplay )
    {
        if ( m_kVolumeCube != null )
        {
            m_kVolumeCube.SetDisplay(bDisplay);
        }
    }

    public void DisplayPolyline(boolean bDisplay)
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
        m_kShaderParamsWindow.AddUserVariables(m_kVolumeRayCast.GetShaderEffect().GetPProgram());
        m_kShaderParamsWindow.Display();
        m_kShaderParamsWindow.setParent(this);
    }

    public void DisplaySurface(boolean bDisplay)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                m_kDisplayList.get(i).SetDisplay(bDisplay);
            }
        }
    }

    public void DisplayVolumeRaycast( boolean bDisplay )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetDisplay(bDisplay);
        }
    }

    public void DisplayVolumeSlices( boolean bDisplay )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetDisplay( bDisplay );
        }
    }

    /**
     * memory cleanup.
     */
    public void dispose()
    {
        if ( m_kSculptor != null )
        { 
            m_kSculptor.disposeLocal();
            m_kSculptor = null;
        }
        for ( int i = 0; i < 4; i++ )
        {
            m_akTransfer[i] = null;
        }
        if ( m_kShaderParamsWindow != null )
        {
            m_kShaderParamsWindow.close();
        }

        super.dispose();
    }

    /**
     * Enables the arbitrary clip plane position.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor, the arbitrary clip plane color.
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
            setArbitraryClipPlane((float)(m_kImageA.getExtents()[0] -1));
        }
    }

    /**
     * Enables the axis-aligned clipping planes.
     * @param iWhich, the clip plane to enable.
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
            m_kVolumeRayCast.SetClip(iWhich,fValue);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClip(iWhich,fValue);
            }
        }
    }

    /**
     * Enables the eye clip plane.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor, the eye clip plane color.
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
            setEyeClipPlane(0, bDisplay);
        }
    }


    /**
     * Enables the inverse-eye clip plane.
     * @param bEnable clipping enabled
     * @param bDisplay on/off.
     * @param kColor, the inverse-eye clip plane color.
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
            setEyeInvClipPlane(m_kImageA.getExtents()[2] - 1, bDisplay);
        }
    }

    public void enableGeodesic( boolean bEnable )
    {
        m_bGeodesicEnabled = bEnable;
    }

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
     * @param bSculpt, true to enable sculpting, false to disable.
     */
    public void enableSculpt( boolean bSculpt )
    {
        if ( m_kSculptor == null )
        {
            m_kSculptor = new Sculptor_WM( ((OpenGLRenderer)m_pkRenderer).GetCanvas() );
            m_kSculptor.setImage(m_kImageA, m_kImageB);
        }
        m_kSculptor.enableSculpt(bSculpt);
    }

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


    /**
     */
    public Vector3f GetCenter( String kSurfaceName )
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
        return new Vector3f( Vector3f.ZERO );
    }

    /**
     */
    public MaterialState GetMaterial( String kSurfaceName )
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
     * Called from the JPanelDisplay dialog. Gets the material properties for
     * the VolumeShaderSUR (Surface and Composite Surface volume shaders.)
     * @return material properties for the surface mode.
     */
    public MaterialState GetMaterialState( )
    {
        if ( m_kVolumeRayCast != null )
        {
            return m_kVolumeRayCast.GetMaterialState();
        }
        return null;
    }

    /** Returns the polyline color for the specified fiber bundle tract group. 
     * @param iGroup, the fiber bundle group to query.
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
     */
    public float GetSurfaceArea( String kSurfaceName )
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
     */
    public float GetVolume( String kSurfaceName )
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
    
    /** Rotates the object with a virtual trackball:
     * @param e, the MouseEvent
     */

    /**
     * Part of the GLEventListener interface. Init is called once when the
     * GLCanvas is first displayed. Called again if the GLCanvas is removed
     * from the frame and added to another window or panel. Initializes the
     * display.
     * @param arg0, GLCanvas
     */
    public void init(GLAutoDrawable arg0) {
//         if ( m_bInit )
//         {
//             if ( !m_kAnimator.isAnimating() )
//             {
//                 m_kAnimator.start();
//             }        

//             return;
//         }
    	
    	if ( m_kImageA == null ) {
        	return;
        }
    	
        m_bInit = true;

        arg0.setAutoSwapBufferMode( false );

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        ((OpenGLRenderer)m_pkRenderer).InitializeState();
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

        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );

        m_kAnimator.add( GetCanvas() );
        //m_kAnimator = new Animator( GetCanvas() );
        m_kAnimator.setRunAsFastAsPossible(true); 
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
     * @param e, the key event.
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
            DisplayBoundingBox( !m_kVolumeBox.GetDisplay() );
            return;
        case 'o':
            DisplayOrientationCube( !m_kVolumeCube.GetDisplay() );
            return;
        case '1':
            m_bStereo = false;
            break;
        case '2':
            m_bStereo = true;
            m_bLeft = true;
            m_bRight = true;
            break;
        case 'l':
            m_bLeft = true;
            m_bRight = false;
            break;
        case 'r':
            m_bLeft = false;
            m_bRight = true;
            break;
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
            m_kVolumeRayCast.MIPMode(m_pkRenderer);
        }
        ResetShaderParamsWindow();
    }

    /** Rotates the object with a virtual trackball:
     * @param e, the MouseEvent
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
                doClip();
            }
            else if ( e.isControlDown() && m_bPaintEnabled )
            {
                m_iXPick = e.getX();
                m_iYPick = e.getY();
                m_bPickPending = true;
            }
        }
        m_kParent.setCameraParameters();
    	m_kParent.setObjectParameters();
    }


    /** Rotates the object with a virtual trackball:
     * @param e, the MouseEvent
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
     * @param e, the MouseEvent
     */
    public void mouseReleased(MouseEvent e)
    {
    	super.mouseReleased(e);
    }

    public void PickCorrespondence( boolean bOn )
    {
        m_bPickCorrespondence = bOn;
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
     * @param iGroup, the group of polylines to remove.
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
     * @param fValue, the blend value (0-1)
     */
    public void removeSurface( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    VolumeSurface kSurface = (VolumeSurface)m_kDisplayList.remove(i);
                    kSurface.dispose();
                    kSurface = null;
                }
            }
        }
    }

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
     * @param options, file writing options
     * @param filtertype, 
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
     * Enables/Disables self-shadowing in the Surface mode.
     * @param bShadow, shadow on/off.
     */
    public void SelfShadow(boolean bShadow)
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SelfShadow(bShadow);
        }
    }

    /**
     * Sets the arbitrary clip plane color.
     * @param kColor, the new color.
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
    public void setArbitraryClipPlane( float f4 )
    {
        f4 /= (m_kImageA.getExtents()[0] -1);     
        m_kArbitraryClip = new Vector4f(1,0,0,f4);
        doClip();
    }

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
     * @param kColor, new background color.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        m_kBackgroundColor = kColor;
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetBackgroundColor( kColor );
        }
    }

    /**
     * Called from JPanelDisplay. Sets the bounding box color.
     * @param kColor bounding box color.
     */
    public void SetBoundingBoxColor( ColorRGB kColor )
    {
        if ( m_kVolumeBox != null )
        {
            m_kVolumeBox.SetBoundingBoxColor(kColor);
        }
    }

    public void SetBoundingBoxColor( int i, ColorRGB kColor )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetBoundingBoxColor( i, kColor );
        }
    }


    public void SetCenter( Vector3f kCenter )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetCenter( new Vector3f( (kCenter.X / (m_kImageA.getExtents()[0] -1)),
                                               (kCenter.Y / (m_kImageA.getExtents()[1] -1)),
                                               (kCenter.Z / (m_kImageA.getExtents()[2] -1))  ) );

        }
    }

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
     * @param iWhich, the clip plane to set.
     * @param fValue, the new position of the clip plane (the same value as
     * the slider in JPanelClip).
     */
    public void setClipPlane( int iWhich, float fValue )
    {
        if ( iWhich < 2 )
        {
            fValue /= (m_kImageA.getExtents()[0] -1);
        }
        else if ( iWhich < 4 )
        {
            fValue /= (m_kImageA.getExtents()[1]-1);
        }
        else
        {
            fValue /= (m_kImageA.getExtents()[2] -1);
        }
        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.setClipPlane(iWhich, fValue);
        }

        float[] data = new float[4];
        data[0] = fValue;
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClip(iWhich, fValue);
        }  
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClip(iWhich,fValue);
            }
        }
    }

    /**
     * Sets the axis-aligned clip plane color.
     * @param iWhich, one of the 6 clip planes
     * @param kColor, the new color.
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
     * @param fValue, the blend value (0-1)
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

    /** Turns on/off displaying all the ellipsoids.
     * @param bDisplay, when true display all the cylinders in the volume.
     */
    public void setDisplayAllCylinders( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayAllCylinders(bDisplay);
        }
    }
    
    /** Turns on/off displaying all the ellipsoids.
     * @param bDisplay, when true display all the ellipsods in the volume.
     */
    public void setDisplayAllEllipsoids( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayAllEllipsoids(bDisplay);
        }
    }
    
    /** Turns on/off displaying the fiber bundle tracts with ellipsoids.
     * @param bDisplay, when true display the tracts with Cylinders.
     */
    public void setDisplayCylinders( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayCylinders( bDisplay );
        }
    }

    /** Turns on/off displaying the fiber bundle tracts with ellipsoids.
     * @param bDisplay, when true display the tracts with ellipsods.
     */
    public void setDisplayEllipsoids( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayEllipsoids( bDisplay );
        }
    }

    /** Turns on/off displaying the fiber bundle tracts with ellipsoids.
     * @param bDisplay, when true display the tracts with Cylinders.
     */
    public void setDisplayTubes( boolean bDisplay )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setDisplayTubes( bDisplay );
        }
    }
    
    /**
     * Sets the sculpt drawing shape.
     * @param shape, (0 = free-hand, 1 = rectangular)
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
    public void setDTIImage( ModelImage kDTIImage )
    {
        if ( m_kDTIDisplay == null )
        {
            m_kDTIDisplay = new VolumeDTI( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
            m_kDisplayList.add(m_kDisplayList.size(), m_kDTIDisplay);
        }
        m_kDTIDisplay.setDTIImage(kDTIImage);
    }


    /** Set the m_iEllipsoidMod value. 
     * @param iMod, new m_iEllipsoidMod value.
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
    public void setEyeClipPlane( float f4, boolean bDisplay )
    {
        f4 /= (m_kImageA.getExtents()[2] -1);
        float[] afEquation = new float[]{0,0,1,f4};
        float fZ = afEquation[3] * m_fZ;

        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.DisplayEye(bDisplay);
            m_kVolumeClip.setEyeClipPlane( fZ );
        }
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClipEye(afEquation);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClipEye(afEquation);
            }
        }
    }

    /**
     * Sets the eye clip plane color.
     * @param kColor, the new color.
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
    public void setEyeInvClipPlane( float f4, boolean bDisplay )
    {
        f4 /= (m_kImageA.getExtents()[2] -1);
        float[] afEquation = new float[]{0,0,1,f4};
        float fZ = afEquation[3] * m_fZ;

        if ( m_kVolumeClip != null )
        {
            m_kVolumeClip.DisplayEyeInv(bDisplay);
            m_kVolumeClip.setEyeInvClipPlane( fZ );
        }
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetClipEyeInv(afEquation);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClipEyeInv(afEquation);
            }
        }
    }
    
    /**
     * Sets the inverse-eye clip plane color.
     * @param kColor, the new color.
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
     * @param bShow, gradient magnitude filter on/off
     */
    public void SetGradientMagnitude(boolean bShow)
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetGradientMagnitude(bShow);
        }
    }  
    

    public void SetImageNew( String kSurfaceName, ModelImage kImage )
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
    
    public void setIPD( float fIPD )
    {
        m_fTrnSpeed = fIPD;        
    }
    
    
    
    public void SetLUTNew( String kSurfaceName, ModelLUT kLUT, ModelRGB kRGBT )
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
     */
    public void SetMaterial( String kSurfaceName, MaterialState kMaterial )
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
     * Called from the AdvancedMaterialProperties dialog. Sets the material
     * properties for the VolumeShaderSUR (Surface and Composite Surface
     * volume shaders.)
     * @param kMaterial, new material properties for the surface mode.
     */
    public void SetMaterialState( MaterialState kMaterial )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SetMaterialState(  kMaterial );
        }
    }

    public void setPerPixelLighting(String kSurfaceName, boolean bOn)
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    ((VolumeSurface)m_kDisplayList.get(i)).SetPerPixelLighting( m_pkRenderer, bOn );
                }
            }
        }
        updateLighting(m_akLights);
    }

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
     * @param iGroup, the fiber bundle group to set.
     * @param kColor the new polyline color for the specified fiber bundle tract group. 
     */
    public void setPolylineColor( int iGroup, ColorRGB kColor )
    {
        if ( m_kDTIDisplay != null )
        {
            m_kDTIDisplay.setPolylineColor(iGroup, kColor);
        }
    }

    public void SetSliceOpacity( int i, float fAlpha )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.SetSliceOpacity( i, fAlpha );
        }
    }
    
    public void SetStereo( boolean bEnable )
    {
        m_bStereo = bEnable;
    }
    
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

    
    /**
     * Sets the currently visible flag. Used when the GLCanvas is removed from
     * the display panel or frame.
     * @param bVisible, set to false when the GLCanvas container is not displayed.
     */
    public void setVisible( boolean bVisible )
    {
        m_bVisible = bVisible;
    }
    
    public void setVolumeBlend( float fBlend )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.setVolumeBlend(fBlend);
        }
    }
    
    public void ShowBoundingBox( int i, boolean bShow )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.ShowBoundingBox( i, bShow );
        }
    }
    
    
    public void ShowSlice( int i, boolean bShow )
    {
        if ( m_kSlices != null )
        {
            m_kSlices.ShowSlice( i, bShow );
        }
    }
    
    
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
     * Sets the raytracing steps size.
     * @param fValue, the steps value (0-450)
     */
    public void StepsSize( float fValue )
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.StepsSize(fValue);
        }
    }
    
    
    
    /**
     * Display the volume in Surface mode.
     */
    public void SURFASTMode()
    {
        if ( m_kVolumeRayCast != null )
        {
            m_kVolumeRayCast.SURFASTMode(m_pkRenderer);
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
            m_kVolumeRayCast.SURMode(m_pkRenderer);
        }
        ResetShaderParamsWindow();
    }
    
    
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
     * Undo applying the sculpt region to the volume.
     */
    public void undoSculpt()
    {
        if ( m_kSculptor == null )
        {
            return;
        }

        if ( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() != null )
        {
            m_kSculptor.setTextureImageDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() );
        }
        else
        {
            m_kSculptor.setTextureImageFloatDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetFloatData() );
        }
        /*
        if ( m_kImageB != null )
        {
            if ( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetData() != null )
            {
                m_kSculptor.setTextureImageDataB( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetData() );
            }
            else
            {
                m_kSculptor.setTextureImageFloatDataB( m_kVolumeShaderEffect.GetVolumeTargetB().GetImage().GetFloatData() );
            }
        }
        */
        m_kSculptor.undoSculpt();
        m_kVolumeImageA.ReleaseVolume();
        m_kParent.setModified();
    }    
    
    /**
     * Causes the VolumeShader to update the copy of the ModelImage on the
     * GPU.
     * @param kImage, the new image.
     */
    public void updateData( ModelImage kImage )
    {
        m_kImageA = kImage;
        if ( m_kSculptor != null )
        {
            m_kSculptor.setImage(m_kImageA, m_kImageB);
        }
        if ( m_kVolumeImageA != null )
        {
            m_kVolumeImageA.UpdateData(kImage, 0);
        }
    }

    
    /**
     * Called by the init() function. Creates and initialized the scene-graph.
     * @param arg0, the GLCanvas
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

        m_kVolumeRayCast = new VolumeRayCast( m_kVolumeImageA );
        m_kDisplayList.add(0, m_kVolumeRayCast);
        m_kVolumeRayCast.CreateScene( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight, arg0, m_pkRenderer );

        m_kTranslate = m_kVolumeRayCast.GetTranslate();


        float fMaxX = (float) (m_kImageA.getExtents()[0] - 1) * m_kImageA.getFileInfo(0).getResolutions()[0];
        float fMaxY = (float) (m_kImageA.getExtents()[1] - 1) * m_kImageA.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (float) (m_kImageA.getExtents()[2] - 1) * m_kImageA.getFileInfo(0).getResolutions()[2];

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

        for ( int i = 0; i < 4; i++ )
        {
            if ( m_akTransfer[i] != null )
            {
                m_kVolumeImageA.UpdateImages(m_akTransfer[i], i);
            }
        }
        if ( m_kRGBTa != null )
        {
            m_kVolumeImageA.SetRGBT( m_kRGBTa, 0 );
        }
        if ( m_kRGBTb != null )
        {
            m_kVolumeImageB.SetRGBT( m_kRGBTb, 1 );
        }
        m_kSlices = new VolumeSlices( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
        DisplayVolumeSlices( true );
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
        
        m_kParent.AddSlices(m_kSlices);
    }
    
    /**
     * Calculates the rotation for the arbitrary clip plane.
     */
    private void doClip( ) 
    {           
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
            m_kVolumeRayCast.SetClipArb(m_afArbEquation);
        }
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
            {
                ((VolumeSurface)m_kDisplayList.get(i)).SetClipArb(m_afArbEquation);
            }
        }
    }
    
    private void RenderNoVolume()
    {
        m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
        m_pkRenderer.ClearBuffers();

        if ( !m_bStereo )
        {
            for ( int i = 1; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller );
            }
            for ( int i = 1; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).PostRender( m_pkRenderer, m_kCuller );
            }
        }
        else
        {          
            MoveRight();
            if ( m_bRight )
            {
                m_kCuller.ComputeVisibleSet(m_spkScene);
                m_pkRenderer.SetColorMask( false, false, true, true );
                for ( int i = 1; i < m_kDisplayList.size(); i++ )
                {
                    m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller );
                }
                for ( int i = 1; i < m_kDisplayList.size(); i++ )
                {
                    m_kDisplayList.get(i).PostRender( m_pkRenderer, m_kCuller );
                }
            }
            m_pkRenderer.ClearZBuffer();
            MoveLeft();
            MoveLeft();
            if ( m_bLeft )
            {
                m_kCuller.ComputeVisibleSet(m_spkScene);
                m_pkRenderer.SetColorMask( true, false, false, true );
                for ( int i = 1; i < m_kDisplayList.size(); i++ )
                {
                    m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller );
                }
                for ( int i = 1; i < m_kDisplayList.size(); i++ )
                {
                    m_kDisplayList.get(i).PostRender( m_pkRenderer, m_kCuller );
                }
            }
            MoveRight();
            m_pkRenderer.SetColorMask( true, true, true, true );
        }
    }    
    
    private void RenderSculpt()
    {
        if ( (m_kSculptor != null) && m_kSculptor.IsSculptDrawn() )
        {
            m_pkRenderer.Draw( m_kSculptor.getSculptImage() );
        }
    }
    
    private void RenderVolume()
    {
        if ( !m_bDisplaySecond )
        {
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).PreRender( m_pkRenderer, m_kCuller );
            }
        }
        else
        {
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).PreRender( m_pkRenderer, m_kCuller );
            }
            m_kVolumeRayCast.PostPreRender();

            m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
            m_pkRenderer.ClearBuffers();

            for ( int i = 1; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller );
            }
            m_kDisplayList.get(0).Render( m_pkRenderer, m_kCuller );

            for ( int i = 1; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).PostRender( m_pkRenderer, m_kCuller );
            }
        }
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
    
}
