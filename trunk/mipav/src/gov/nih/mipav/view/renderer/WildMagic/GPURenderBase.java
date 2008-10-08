package gov.nih.mipav.view.renderer.WildMagic;

import javax.imageio.ImageIO;
import javax.media.opengl.*;
import com.sun.opengl.util.*;

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Vector;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.*;

import WildMagic.LibApplications.OpenGLApplication.*;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Collision.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibRenderers.OpenGLRenderer.*;


public abstract class GPURenderBase extends JavaApplication3D
implements GLEventListener, KeyListener, MouseMotionListener,  MouseListener
{
    protected VolumeTriPlanarInterface m_kParent = null;
    protected VolumeImage m_kVolumeImageA;
    protected VolumeImage m_kVolumeImageB;
    
    /** Scene-graph root node: */
    protected Node m_spkScene;
    /** Culling: turns backface/frontface culling on/off: */
    protected CullState m_spkCull;
    /** Culling out-of-view objects: */
    protected Culler m_kCuller = new Culler(0,0,null);

    /** Reference to imageA in ViewJFrameVolumeView: */
    protected ModelImage m_kImageA = null;
    /** ModelLUT applied to m_kImageA */
    protected ModelLUT m_kLUTa = null;
    /** ModelRGB applied to m_kImageA */
    protected ModelRGB m_kRGBTa = null;

    /** Reference to ModelImage imageB in ViewJFrameVolumeView */
    protected ModelImage m_kImageB = null;
    /** ModelLUT applied to m_kImageB */
    protected ModelLUT m_kLUTb = null;
    /** ModelRGB applied to m_kImageB */
    protected ModelRGB m_kRGBTb = null;

    /** Animator object, displays scene in rendering loop (similar to GLUTMainLoop() */
    protected Animator m_kAnimator;

    /** Set to true when init() is called: */
    protected boolean m_bInit = false;
    /** Scene translation, centers the scene: */
    protected Vector3f m_kTranslate = new Vector3f(Vector3f.ZERO);
    /** Normalized volume extents: */
    protected float m_fX = 1f, m_fY = 1f, m_fZ = 1f, m_fMax = 1f;
    /** Flag for indicating the that Java Container is visible or not: */
    protected boolean m_bVisible = true;

    /** Lights from JPanelLight */
    protected Light[] m_akLights = null;

    /** For testing the frame rate: */
    protected boolean m_bTestFrameRate = false;
    /** Rotation during frame rate tests: */
    protected Matrix3f m_kRotate = new Matrix3f();

    protected Vector<VolumeObject> m_kDisplayList = new Vector<VolumeObject>();

    protected boolean m_bSurfaceUpdate = false;
   

    protected int m_iXPick = -1;
    protected int m_iYPick = -1;
    protected boolean m_bPickPending = false;
    protected Picker m_kPicker = new Picker();

    protected boolean m_bModified = true;
    
    //protected GraphicsImage m_kScreenCaptureImage = null;
    //protected Texture m_kScreenCaptureTarget = null;
    //protected boolean m_bScreenCapture = false;
    private int m_iScreenCaptureCounter = 0;

    /**
     * Constructs a new GPUVolumeRender object.
     * @param kImageA ModelImage A
     * @param kLUTa, LUT for ModelImage A
     * @param kRGBTa, RGB lookup table for ModelImage A
     * @param kImageB ModelImage B
     * @param kLUTb, LUT for ModelImage B
     * @param kRGBTb, RGB lookup table for ModelImage B
     */
    public GPURenderBase( VolumeTriPlanarInterface kParent, Animator kAnimator, VolumeImage kVolumeImageA, ModelImage kImageA, ModelLUT kLUTa, ModelRGB kRGBTa,
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
    
    public GPURenderBase( final String acWindowTitle, int iXPosition,
            int iYPosition, int iWidth, int iHeight,
            final ColorRGBA rkBackgroundColor )
    {
        super("GPUVolumeRender",0,0,512,512, new ColorRGBA(0.0f,0.0f,0.0f,0.0f));
    }
    
    /**
     * memory cleanup.
     */
    public void dispose()
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            m_kDisplayList.get(i).dispose();
        }

        if ( m_spkScene != null )
        {
            m_spkScene.dispose();
            m_spkScene = null;
        }

        if ( m_spkCull != null )
        {
            m_spkCull.dispose();
            m_spkCull = null;
        }
        if ( m_kCuller != null )
        {
            m_kCuller.dispose();
            m_kCuller = null;
        }

        m_kImageA = null;
        m_kLUTa = null;
        m_kRGBTa = null;
        m_kImageB = null;
        m_kLUTb = null;
        m_kRGBTb = null;

        m_kTranslate = null;

        m_akLights = null;

        m_kRotate = null;

        m_kLUTa = null;

        super.dispose();
    }

    /** Returns model image A
     * @return model image A
     */
    public ModelImage getImage()
    {
        return m_kImageA;
    }

    protected void Move()
    {
        if (MoveCamera())
        {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }        
        if (MoveObject())
        {
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
            
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
            }
        }

        if ( m_bTestFrameRate )
        {
            Matrix3f kRotate = m_spkScene.Local.GetRotate();
            kRotate.Mult(m_kRotate);
            m_spkScene.Local.SetRotate(kRotate);
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
            
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
            }
        }
    }

    protected void RenderFrameRate()
    {
        //Draw frame rate:
        m_pkRenderer.SetCamera(m_spkCamera);
        if ( m_bTestFrameRate )
        {
            DrawFrameRate(8,16,ColorRGBA.WHITE);
        }
    }
    /*
    public void CreateScene()
    {
        m_kScreenCaptureImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,m_iWidth,m_iHeight,
                m_kVolumeImageA.GetImage().getExtents()[2],(byte[])null,
        "ScreenCaptureImage");
        m_kScreenCaptureTarget = new Texture();
        m_kScreenCaptureTarget.SetImage(m_kScreenCaptureImage);
    }
*/
    /**
     * Part of the GLEventListener interface.
     */
    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {}

    

    /**
     * Part of the GLEventListener interface. Called when the GLCanvas is resized.
     * @param arg0, the GLCanvas
     * @param iX, the new x-position in screen coordinates.
     * @param iY, the new y-position in screen coordinates.
     * @param iWidth, the new width
     * @param iHeight, the new height
     */
    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
    	
    	if ( m_kImageA == null ) {
        	return;
        }
        if (iWidth > 0 && iHeight > 0)
        {
            m_iWidth = iWidth;
            m_iHeight = iHeight;
            if ( m_spkCamera.Perspective )
            {
                m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
            }
            else
            {
                m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
            }

            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }
        }
    }

    /**
     * Returns the GLCanvas in the m_pkRenderer object.
     * @return OpenGLRenderer.GLCanvas
     */
    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    public Vector3f getTranslate() {
    	return m_kTranslate;
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
        case 'f':
            m_bTestFrameRate = !m_bTestFrameRate;
            if ( m_bTestFrameRate )
            {
                ResetTime();
            }
            return;
        case 'q':
            setOrthographicProjection();
            return;
        case 'Q':
            setPerspectiveProjection();
            return;
        }

        return;
    }

    /** 
     * Changes the projection to Orthographic Projection.
     */
    public void setOrthographicProjection()
    {
        m_spkCamera.Perspective = false;
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
        m_pkRenderer.OnFrustumChange();
    }

    /** 
     * Changes the projection to Perspective Projection.
     */
    public void setPerspectiveProjection()
    {
        m_spkCamera.Perspective = true;
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
        m_pkRenderer.OnFrustumChange();
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

    /**
     * Called from JPanelLight. Updates the lighting parameters.
     * @param akGLights, the set of GeneralLight objects.
     */
    public void updateLighting(Light[] akGLights )
    {
        if ( akGLights == null )
        {
            return;
        }
        m_akLights = akGLights;
        if ( m_bInit )
        {
            for ( int i = 0; i < akGLights.length; i++ )
            {
                String kLightType = new String("Light"+(i)+"Type");
                float[] afType = new float[]{0,0,0,0};
                //if ( i < m_pkRenderer.GetMaxLights() )
                if ( i < 4 )
                {
                    if ( akGLights[i].On )
                    {
                        Light kLight = akGLights[i];
                        m_pkRenderer.SetLight( i, kLight );
                        if ( akGLights[i].Type == Light.LightType.LT_AMBIENT )
                        {
                            afType[0] = 0;
                        }
                        else if ( akGLights[i].Type == Light.LightType.LT_DIRECTIONAL )
                        {
                            afType[0] = 1;
                        }
                        else if ( akGLights[i].Type == Light.LightType.LT_POINT )
                        {
                            afType[0] = 2;
                        }
                        else if ( akGLights[i].Type == Light.LightType.LT_SPOT )
                        {
                            afType[0] = 3;
                        }
                        for ( int j = 0; j < m_kDisplayList.size(); j++ )
                        {
                            m_kDisplayList.get(j).SetLight(kLightType, afType);
                        }
                    }
                    else
                    {
                        m_pkRenderer.SetLight( i, new Light() );
                        afType[0] = -1;
                        for ( int j = 0; j < m_kDisplayList.size(); j++ )
                        {
                            m_kDisplayList.get(j).SetLight(kLightType, afType);
                        }
                    }
                }
            }
        }
    }

    public void resetAxisX()
    {
        m_spkScene.Local.SetRotateCopy( Matrix3f.IDENTITY );
        m_spkScene.Local.GetRotate().FromAxisAngle( Vector3f.UNIT_X, (float)Math.PI/2.0f );
        UpdateSceneRotation();
    }

    public void resetAxisY()
    {
        m_spkScene.Local.SetRotateCopy( Matrix3f.IDENTITY );
        m_spkScene.Local.GetRotate().FromAxisAngle( Vector3f.UNIT_Y, -(float)Math.PI/2.0f );
        UpdateSceneRotation();
    }

    public void resetAxis()
    {
        m_spkScene.Local.SetRotateCopy( Matrix3f.IDENTITY );
        UpdateSceneRotation();
    }

    protected void UpdateSceneRotation()
    {
        m_spkScene.UpdateGS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
        }
    }
    
    
    public Light[] GetLights()
    {
        return m_akLights;
    }
    
    /**
     * Build a image with the current rotational transformation matrix. Step.1
     * convert the java3D transform matrix into a quaternion component. Step.2
     * convert the quaternion into image( our world ) coordinate
     * system. Step.3 convert the quaternion into a rotatin matrix. Quaternion
     * q ( w, x, y, z ): rotation of w around the vector ( x, y, z ); Convert
     * the quaternion into a rotation matrix. / \ | 1-2*y^2-2*z^2 2*x*y-2*w*z
     * 2*x*z+2*w*y | | 2*xy+2*w*z 1-2*x^2-2*z^2 2*y*z-2*w*x | | 2*x*z-2*w*y
     * 2*y*z+2*w*x 1-2*x^2-2*y^2 | \ / Step.4 Calling the transform algorithm
     * to extract the image.
     */
    public void updateImageFromRotation() {
        int interp = 0;
        double w, x, y, z;
        double[][] result = new double[4][4];

        // Step.1
        Quaternion quat = new Quaternion();
        Matrix3f kRotate = m_spkScene.Local.GetRotate();
        quat.FromRotationMatrix(kRotate);
        
        // Step.2
        w = quat.W();
        x = quat.X();
        y = quat.Y();
        z = quat.Z();

        // Step.3
        TransMatrix transMtx = new TransMatrix(4);

        transMtx.set(0, 0, 1 - (2 * (y * y)) - (2 * (z * z)));
        transMtx.set(0, 1, 2 * ((x * y) - (w * z)));
        transMtx.set(0, 2, 2 * ((x * z) + (w * y)));
        transMtx.set(0, 3, 0);
        transMtx.set(1, 0, 2 * ((x * y) + (w * z)));
        transMtx.set(1, 1, 1 - (2 * (x * x)) - (2 * (z * z)));
        transMtx.set(1, 2, 2 * ((y * z) - (w * x)));
        transMtx.set(1, 3, 0);
        transMtx.set(2, 0, 2 * ((x * z) - (w * y)));
        transMtx.set(2, 1, 2 * ((y * z) + (w * x)));
        transMtx.set(2, 2, 1 - (2 * (x * x)) - (2 * (y * y)));
        transMtx.set(2, 3, 1);
        transMtx.set(3, 0, 0);
        transMtx.set(3, 1, 0);
        transMtx.set(3, 2, 0);
        transMtx.set(3, 3, 1);

        TransMatrix xfrm = new TransMatrix(4);

        WildMagic.LibFoundation.Mathematics.Vector3f center = m_kImageA.getImageCentermm(false);

        xfrm.setTranslate(center.X, center.Y, center.Z);
        xfrm.Mult(transMtx);
        xfrm.setTranslate(-center.X, -center.Y, -center.Z);

        // Step.4
        float[] resols = m_kImageA.getFileInfo()[0].getResolutions();
        int[] dim = m_kImageA.getFileInfo()[0].getExtents();
        int xDim = dim[0];
        int yDim = dim[1];
        int zDim = dim[2];
        AlgorithmTransform algoTrans = new AlgorithmTransform(m_kImageA, xfrm, interp, resols[0], resols[1], resols[2],
                                                              xDim, yDim, zDim, false, false, false);

        algoTrans.setUpdateOriginFlag(false);
        algoTrans.run();

        ModelImage resultImage1 = algoTrans.getTransformedImage();

        if ((algoTrans.isCompleted() == true) && (resultImage1 != null)) {
            resultImage1.calcMinMax();
           
            // The algorithm has completed and produced a new image to be displayed.
            try {
                new ViewJFrameImage(resultImage1, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
           
        }
    }
    
    public boolean writeImage()
    {
        BufferedImage kScreenShot = m_pkRenderer.Screenshot();
        //m_bScreenCapture = true;
        //GetCanvas().display();
        try {
            ImageIO.write(kScreenShot, "jpg", new File("captureImage" + m_iScreenCaptureCounter++ + "." + "jpg"));
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return true;
    }
    

    public int getCounter() {
        return m_iScreenCaptureCounter;
    }
    
}
