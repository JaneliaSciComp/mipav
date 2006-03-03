package gov.nih.mipav.view.renderer.surfaceview;


import gov.nih.mipav.view.renderer.surfaceview.rfaview.*;
import gov.nih.mipav.view.renderer.volumeview.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.model.algorithms.*;

import com.sun.j3d.utils.universe.*;
import com.sun.j3d.utils.image.TextureLoader;
import com.sun.j3d.utils.geometry.Box;
import com.sun.j3d.utils.behaviors.mouse.*;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import javax.media.j3d.*;

import javax.vecmath.*;
import javax.swing.event.*;
import gov.nih.mipav.MipavMath;


/**
 *	Frame that holds the surface renderer.  This frame is only possible to activate if a
 *	three dimensional image is loaded.  The image shows up in the frame as the three planes,
 *	with an axial view, a coronal view, and a sagittal view.  The user can slide these planes
 *	up and down and can turn them on and off.  The user can also load in surfaces created from
 *	the original image.  These 3D surfaces will appear in the proper place within the three image
 *	planes.  There are many options for viewing the surfaces.  Additionally, the user can change the
 *	view mode, so that the mouse causes the view to "fly".  The user can then record the different
 *	mouse actions and play them back.
 *
 *	@see		ViewJComponentSurfaceImage
 *	@see		JDialogSurface
 *	@see		JDialogView
 *	@see		JDialogMouseRecorder
 */
public class SurfaceRender extends RenderViewBase {

    /** Dialog for loading and displaying surfaces. */
    private JPanelSurface surfacePanel;

    /** Dialog for loading and displaying probes. */
    private JPanelProbe probePanel;

    /** Dialog for recording and playing back mouse events. */
    private JPanelMouse mousePanel;

    /** Dialog to turn bounding box of surface renderer on and off,
     and to change the color of the frame. */
    private JPanelDisplay boxPanel;

    /** Dialog to turn the slices control box on and off. */
    private JPanelSlices slicePanel;

    /** Dialog to turn the clipping palne box on and off. */
    private JPanelClip clipPanel;

    /** Controls for the image.  ViewJFrameVolumeView */
    private ViewJFrameBase parent;

    /** XY image plane. */
    private ViewJComponentTriSliceImage componentImageXY;

    /** ZY image plane. */
    private ViewJComponentTriSliceImage componentImageZY;

    /** XZ image plane. */
    private ViewJComponentTriSliceImage componentImageXZ;

    /*********************************************************************************/

    /** Tri planar view and the 3D texture volume view switch group */
    private Switch switchGroup;

    /** Volume image object */
    private ViewJComponentSurfaceVolume componentVolImage;

    /** Buffer that holds image A volume data. */
    private float[] imageVolBufferA;

    /** Buffer that holds image B volume data. */
    private float[] imageVolBufferB;

    /** If true display 3D volume as a 3D texture map or 2D array of texture maps. If false
     *  display 3 orthogonal image planes. */
    private boolean volumeDisplayMode3D = false;

    /** Add here */
    private NodeVolumeTextureRender volRenderNode;

    /** Reference to 3D texture node for the volume */
    private VolumeTexture texture = null;

    /** Volume opacity control dialog */
    private JPanelVolOpacityBase volOpacityPanel;

    /*********************************************************************************/

    /** Buffer that holds image A data for the XY plane. */
    private float[] imageBufferA_XY;

    /** Buffer that holds image A data for the ZY plane. */
    private float[] imageBufferA_ZY;

    /** Buffer that holds image A data for the XZ plane. */
    private float[] imageBufferA_XZ;

    /** Buffer that holds image B data for the XY plane. */
    private float[] imageBufferB_XY;

    /** Buffer that holds image B data for the ZY plane. */
    private float[] imageBufferB_ZY;

    /** Buffer that holds image B data for the XZ plane. */
    private float[] imageBufferB_XZ;

    /** Buffer factor, 1 usually, 4 for color images. */
    private int bufferFactor = 1;

    /** Currently unused. */
    private TransparencyAttributes taXY, taXZ, taZY;

    /** Parent of the XY plane image. */
    private BranchGroup objXYPlaneBG;

    /** Parent of the XZ plane image. */
    private BranchGroup objXZPlaneBG;

    /** Parent of the ZY plane image. */
    private BranchGroup objZYPlaneBG;

    /** Parent of the x slice. */
    private BranchGroup objBoxSliceX_BG;

    /** Parent of the y slice. */
    private BranchGroup objBoxSliceY_BG;

    /** Parent of the z slice. */
    private BranchGroup objBoxSliceZ_BG;

    /* TransformGroups used to rotate the x,y,z slices and x,y,z boxes based
     * on the probe angle and position: */
    private TransformGroup m_kObjBoxSliceProbeX_TG; /* Rotates the X Box */
    private TransformGroup m_kObjPlaneProbeX_TG; /* Rotates the X plane*/
    private Point3f[] m_kObjBoxVertsX = null; /* Reference to the X-Box vertices */

    private TransformGroup m_kObjBoxSliceProbeY_TG; /* Rotates the Y Box */
    private TransformGroup m_kObjPlaneProbeY_TG; /* Rotates the Y plane*/
    private Point3f[] m_kObjBoxVertsY = null; /* Reference to the Y-Box vertices */

    private TransformGroup m_kObjBoxSliceProbeZ_TG; /* Rotates the Z Box */
    private TransformGroup m_kObjPlaneProbeZ_TG; /* Rotates the Z plane*/
    private Point3f[] m_kObjBoxVertsZ = null; /* Reference to the Z-Box vertices */

    /* The Transform3D used to position the planes based on the Probe position
     * and angle. The planes are rotates about the intersection between the
     * probe and the plane, which may or may not be in the center of the
     * plane. This transform allows non-origin rotations by conactenating the
     * translation to the origin, rotation, and translation matricies into one
     * transform, which is used to set the Transforms for the TransformGroups
     * above and also to sample the ModelImage data on the diagonal slice: */
    private Transform3D m_kProbeTransform = null;

    /** Group dictating how the XY plane is translated. */
    private TransformGroup objTransXY_TG;

    /** Group dictating how the XZ plane is translated. */
    private TransformGroup objTransXZ_TG;

    /** Group dictating how the ZY plane is translated. */
    private TransformGroup objTransZY_TG;

    /** The frame around the x slice. */
    private ViewJComponentBoxSlice boxSliceX;

    /** The frame around the y slice. */
    private ViewJComponentBoxSlice boxSliceY;

    /** The frame around the z slice. */
    private ViewJComponentBoxSlice boxSliceZ;

    /** Numbers dicatating the sizes of the planes based on the extents and resolutions of the image. */
    private float xBox, yBox, zBox, maxBox;

    /** Current image A. */
    private ModelImage imageA;

    /** Current image B. */
    private ModelImage imageB;

    /** Resolutions of image A. */
    private float[] resols = new float[4];

    /** Units of measure of imageA. */
    private int[] units = new int[4];

    /** Dimensions of image A. */
    private int xDim, yDim, zDim, tDim;

    /** Flags indicating if the slices have changed position. */
    private boolean xChanged = false, yChanged = false, zChanged = false;

    /** Mouse Rotate behavior.  */
    private MouseRotateExt mouseRotateBehavior;

    /** Mouse Zoom behavior.  */
    private MouseZoom mouseZoomBehavior;

    /** Mouse Translate behavior.  */
    private MouseTranslate mouseTranslateBehavior;

    /** Setup the initial position of the image, added to ScenerootTG transform group */
    private Transform3D transformNode3d;

    /** Initial center viewing point of the image */
    private Point3d myEyePoint = new Point3d();

    /** Transformation matrix */
    private Transform3D parallelScaleT3D;

    /** Volume render branch group */
    private BranchGroup volRenderBG;

    /** Volume render order group, specify the rendering order */
    private OrderedGroup volRenderOG;

    /** Volume branch group */
    private BranchGroup volBG;

    /** Cubic branch group */
    private BranchGroup cubicBG;

    /** Cubic transform group */
    private TransformGroup cubicTG;

    /** Cubic tansform3D. */
    private Transform3D transformCubic;

    /** flag indicate the first time 6 clipping plane branch switch. */
    private boolean first6ClipPlane = true;

    /** flag indicate the first time arbitrary clipping plane branch switch. */
    private boolean firstClipArbi = true;

    /** Cubic mouse rotation behavior. */
    private MouseRotate cubicRotate;

    /** fix one bug from threading. */
    private int rotationCount = 1;

    /** Flag to indicate whether the view volume texture is aligned or not. */
    protected boolean isViewTextureAligned;

    /** Mode is tri-planar volume view or not. */
    boolean isTriPlanarVolView = false;

    /** Axis orietation */
    private int axisXOrient, axisYOrient, axisZOrient;

    /** The cubic roational behavior branch group. */
    private BranchGroup cubicBehaviorBG;

    /** Image with areas marked with values indicating that they are special tissue types (vessels, etc). */
    private ModelImage segmentationImage;

    /** Value which indicates a voxel that is part of the arterial vasculature tree. */
    public static final int ARTERIAL_SEG = 1;

    /** Value which indicates a voxel in the segmenation image that is part of the veinous vasculature tree. */
    public static final int VEINOUS_SEG = 2;

    /** Value which indicates a voxel in the segmenation image that is part of the vasculature (arterial or veinous). */
    public static final int VASCULATURE_SEG = 3;

    /** Value which indicates a voxel in the segmenation image that is part of the tumor. */
    public static final int TUMOR_SEG = 4;

    /** Value which indicates a bone in the image*/
    public static final int BONE_SEG = 5;

    /** Value which indicates the probe entry point color. */
    public static final int ENTRY_POINT = 6;

    /** Flag indicates that probe rotate around the entry point or not. */
    private boolean isEntryPoint = false;

    /** Raycast based renderer reference, raycast renderer or shear warp renderer. */
    protected VolumeRenderer rayBasedRender;

    // Zoomfactor, zooming in screen scale space, and is relative to original
    // position, this stores that original position:
    private double m_dOriginalScreenScale = 1.0;

    /** Current transform changes type. */
    private int currentTransformType;

    /** Current transform matrix. */
    private Transform3D currentTransform = new Transform3D();

    /** Parallel rotation flag. */
    private boolean parallelRotation = true;

    /** Geodesic panel. */
    private JPanelGeodesic geodesicPanel;

    /** Sculptor panel. */
    private JPanelSculptor sculptorPanel;

    /** Flag set when texture volume is being rendered using lights. */
    private boolean m_bRenderModeLighting = false;

    /** Used to compute software lighting of composite texture volume. */
    private SoftwareLightSet m_kSoftwareLightSet = new SoftwareLightSet();
    private SoftwareMaterial m_kSoftwareMaterial = new SoftwareMaterial();

    /** View object in rendering a three dimensional scene from one viewpoint. */
    private View view;

    /** Tri planar slice use Texture2D to display the images.  */
    private Texture2D texXY, texXZ, texZY;

    /** Each texture2D add the ImageComponent2D as image. */
    private ImageComponent2D texXYImageComp2D, texXZImageComp2D, texZYImageComp2D;

    /** Map vectors.  Mapping from the pixel space(0-256) to texture space(0, 1). */
    private Vector4f m_kCoordMapX, m_kCoordMapY, m_kCoordMapZ, m_kCoordMapZNeg;

    /** X, Y, Z label constants. */
    public static final int X = 0;
    public static final int Y = 1;
    public static final int Z = 2;

    /** normal vector array. */
    public Vector3f[] akNormalsImageA;

    /**
     *   Makes a frame and puts the three image planes into it.  Creates the scene graph
     *	which dictates the behavior of the image planes and surfaces.  Initializes the surface
     *	dialog and the mouse recorder dialog, so that this original view is saved.  When
     *	the user opens these dialogs, they have already been created; they are just set to
     *	visible.
     *   @param _imageA        First image to display, cannot be null.
     *   @param LUTa           LUT of the imageA (if null grayscale LUT is constructed).
     *   @param _imageB        Second loaded image, may be null.
     *   @param LUTb           LUT of the imageB, may be null.
     *   @param _parent        Frame base reference.
     *   @param _config        configuration reference.
     *   @param _pBar          progress bar
     */
    public SurfaceRender( ModelImage _imageA, ModelLUT _LUTa, ModelImage _imageB, ModelLUT _LUTb, ViewJFrameBase _parent,
            GraphicsConfiguration _config, ViewJProgressBar _pBar ) {
        super( _imageA, _imageB, _config );
        this.imageA = _imageA;
        this.imageB = _imageB;
        this.LUTa = _LUTa;
        this.LUTb = _LUTb;
        this.parent = _parent;

        imageA.setImageOrder( ModelImage.IMAGE_A );
        if ( imageB != null ) {
            imageB.setImageOrder( ModelImage.IMAGE_B );
        }

        bufferFactor = 1;
        if ( imageA.isColorImage() ) {
            bufferFactor = 4;
        }

        background.setColor( new Color3f( Color.black ) );

        xDim = imageA.getExtents()[0];
        yDim = imageA.getExtents()[1];
        zDim = imageA.getExtents()[2];
        resols[0] = Math.abs( imageA.getFileInfo()[0].getResolutions()[0] );
        resols[1] = Math.abs( imageA.getFileInfo()[0].getResolutions()[1] );
        resols[2] = Math.abs( imageA.getFileInfo()[0].getResolutions()[2] );


        // Retrieve the normal vectors for the voxels in image A.
        akNormalsImageA = RenderViewBase.getNormals();


        // if the slice spacing value is greater than the z-res, use the slice spacing instead
        if ( resols[2] < imageA.getFileInfo( 0 ).getSliceSpacing() ) {
            resols[2] = imageA.getFileInfo( 0 ).getSliceSpacing();
        }

        if ( ( resols[0] == 0.0f ) || ( resols[1] == 0.0f ) || ( resols[2] == 0.0f ) ) {
            resols[0] = 1.0f;
            resols[1] = 1.0f;
            resols[2] = 1.0f;
        }
        units[0] = imageA.getFileInfo()[0].getUnitsOfMeasure()[0];
        units[1] = imageA.getFileInfo()[0].getUnitsOfMeasure()[1];
        units[2] = imageA.getFileInfo()[0].getUnitsOfMeasure()[2];

        setupOrientation();

        parallelScaleT3D = new Transform3D();
        boxPanel = new JPanelDisplay( this );
        slicePanel = new JPanelSlices( this );
        mousePanel = new JPanelMouse( this );

        configureSliceFrame();
        createImageSceneGraph();
        _pBar.updateValueImmed( 5 );
        universe = new SimpleUniverse( canvas );
        // This will move the ViewPlatform back a bit so the
        // objects in the scene can be viewed.

        universe.getViewingPlatform().setNominalViewingTransform();

        view = universe.getViewer().getView();
        // Make sure that front and back clip planes are specified as a
        // virtual distance from the eye
        view.setFrontClipPolicy( View.VIRTUAL_EYE );
        view.setBackClipPolicy( View.VIRTUAL_EYE );
        double backClipDistance = view.getBackClipDistance();

        view.setBackClipDistance( backClipDistance );
        // (back / front) must be < 3000 to avoid loosing z-buffer resolution.
        // The larger the number, the more the z-buffer space will be used
        // for objects in the front of the scene.  If you loose z-buffer
        // resolution for objects at the back of your scene, decrease this
        // number.  If your objects are getting front clipped, adjust both
        // the front and back clip distances to keep this ratio.
        view.setFrontClipDistance( backClipDistance / 600.0 );

        surfacePanel = new JPanelSurface( this, canvas, sceneRootTG, xBox, yBox, zBox );
        probePanel = new JPanelProbe( this, parent, xBox, yBox, zBox );
        _pBar.updateValueImmed( 10 );
        clipPanel = new JPanelClip( this, xBox, yBox, zBox );
        _pBar.updateValueImmed( 15 );
        viewPanel = new JPanelView( this );
        _pBar.updateValueImmed( 20 );

        if ( imageA.isColorImage() ) {
            volOpacityPanel = new JPanelVolOpacityRGB( this, imageA, imageB );
        } else {
            volOpacityPanel = new JPanelVolOpacity( this, imageA, imageB );
        }

        _pBar.updateValueImmed( 25 );

        rotationControlPanel = new JPanelCamera( this );
        geodesicPanel = new JPanelGeodesic( this );
        sculptorPanel = new JPanelSculptor( this );
        rotationControlPanel.setVisible( false );
        _pBar.updateValueImmed( 30 );
        mousePanel.setup();
        objRootBG.compile();
        universe.addBranchGraph( objRootBG );

        updateBoxSliceX();
        updateBoxSliceY();
        updateBoxSliceZ();

        update3DTriplanar( null, null, false );
    }

    /**
     * Constructor called by the VirtualEndoscopyView.  Used for instantiation.
     * @param _imageA ModelImage   imageA
     * @param _imageB ModelImage   imageB always null
     * @param _config GraphicsConfiguration      graphics configuration
     */
    public SurfaceRender( ModelImage _imageA, ModelImage _imageB,
            GraphicsConfiguration _config ) {
        super( _imageA, _imageB, _config );
        this.imageA = _imageA;
        this.imageB = _imageB;
    }

    /**
     * Setup the X, Y, Z axis orientation.
     */
    private void setupOrientation() {
        axisXOrient = imageA.getFileInfo( 0 ).getAxisOrientation()[0];
        axisYOrient = imageA.getFileInfo( 0 ).getAxisOrientation()[1];
        axisZOrient = imageA.getFileInfo( 0 ).getAxisOrientation()[2];
    }

    /**
     * Set the volume view mode to true from the ViewJFrameVolumeView
     * During mouse recorder displaying, the flag control the updating
     * of the opacity histogram
     * @param flag   <code>true</code> means mode on, <code>false</code>
     *                  means mode off.
     */
    public void setVolView( boolean flag ) {
        isTriPlanarVolView = flag;
    }

    /**
     * Check is volume view frame or not
     * @return boolean  <code>true</code> means mode on, <code>false</code>
     *                  means mode off.
     */
    public boolean isVolViewMode() {
        return isTriPlanarVolView;
    }

    /**
     *  Constructs main frame structures for 3 image planes.  Makes the LUT if necessary,
     *	then sets up the buffer arrays appropriately and calls the constructors for the
     *	three image planes.
     */
    private void configureSliceFrame() {

        /** Extents of image A, same as xDim, yDim, etc. */
        int[] extents;

        if ( imageA == null ) {
            return;
        }

        if ( ( ( imageA.getNDims() == 3 ) && ( imageB == null ) )
                || ( ( imageA.getNDims() == 3 ) && ( imageB != null ) && ( imageB.getNDims() == 3 ) ) ) {
            extents = new int[3];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
        } else {
            extents = new int[4];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
            if ( imageA.getNDims() == 4 ) {
                extents[3] = tDim;
            } else {
                extents[3] = tDim;
            }
        }

        // if not a color image and LUTa is null then make a LUT
        if ( imageA.isColorImage() == false ) {
            int[] dimExtentsLUT = new int[2];

            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;
            if ( LUTa == null ) {
                LUTa = new ModelLUT( ModelLUT.GRAY, 256, dimExtentsLUT );
                float min, max;

                if ( imageA.getType() == ModelStorageBase.UBYTE ) {
                    min = 0;
                    max = 255;
                } else if ( imageA.getType() == ModelStorageBase.BYTE ) {
                    min = -128;
                    max = 127;
                } else {
                    min = (float) imageA.getMin();
                    max = (float) imageA.getMax();
                }
                float imgMin = (float) imageA.getMin();
                float imgMax = (float) imageA.getMax();

                LUTa.resetTransferLine( min, imgMin, max, imgMax );
            }
            if ( ( imageB != null ) && ( LUTb == null ) ) {
                LUTb = new ModelLUT( ModelLUT.HOTMETAL, 256, dimExtentsLUT );
                float min, max;

                if ( imageB.getType() == ModelStorageBase.UBYTE ) {
                    min = 0;
                    max = 255;
                } else if ( imageB.getType() == ModelStorageBase.BYTE ) {
                    min = -128;
                    max = 127;
                } else {
                    min = (float) imageB.getMin();
                    max = (float) imageB.getMax();
                }
                float imgMin = (float) imageB.getMin();
                float imgMax = (float) imageB.getMax();

                LUTb.resetTransferLine( min, imgMin, max, imgMax );
            }
        }

        // XY -- Z plane
        imageBufferA_XY = new float[bufferFactor * xDim * yDim];
        if ( imageB != null ) {
            imageBufferB_XY = new float[bufferFactor * imageB.getSliceSize()];
        }
        componentImageXY = new ViewJComponentTriSliceImage( this, imageA, LUTa, imageBufferA_XY, imageB, LUTb,
                imageBufferB_XY, extents, ViewJComponentBase.AXIAL );
        componentImageXY.setBuffers( imageBufferA_XY, imageBufferB_XY );

        // XZ -- Y Plane
        imageBufferA_XZ = new float[bufferFactor * imageA.getExtents()[0] * imageA.getExtents()[2]];
        if ( imageB != null ) {
            imageBufferB_XZ = new float[bufferFactor * imageB.getExtents()[0] * imageB.getExtents()[2]];
        }

        componentImageXZ = new ViewJComponentTriSliceImage( this, imageA, LUTa, imageBufferA_XZ, imageB, LUTb,
                imageBufferB_XZ, extents, ViewJComponentBase.CORONAL );
        componentImageXZ.setBuffers( imageBufferA_XZ, imageBufferB_XZ );

        // ZY -- X plane
        imageBufferA_ZY = new float[bufferFactor * imageA.getExtents()[2] * imageA.getExtents()[1]];
        if ( imageB != null ) {
            imageBufferB_ZY = new float[bufferFactor * imageB.getExtents()[2] * imageB.getExtents()[1]];
        }

        componentImageZY = new ViewJComponentTriSliceImage( this, imageA, LUTa, imageBufferA_ZY, imageB, LUTb,
                imageBufferB_ZY, extents, ViewJComponentBase.SAGITTAL );
        componentImageZY.setBuffers( imageBufferA_ZY, imageBufferB_ZY );

        Preferences.debug( "Preferred graphics configuration: " + config + "\n" );
        canvas = new VolumeCanvas3D( config );
        Preferences.debug( "Canvas: " + canvas.queryProperties() + "\n" );

        componentImageXY.show( slicePanel.tSlice, slicePanel.zSlice, null, null, true, null, null );
        componentImageXZ.show( slicePanel.tSlice, slicePanel.ySlice, null, null, true, null, null );
        componentImageZY.show( slicePanel.tSlice, slicePanel.xSlice, null, null, true, null, null );
    }

    /**
     *	Constructs main frame structures for 3 image planes.  Makes the LUT if necessary,
     *	then sets up the buffer arrays appropriately and calls the constructors for the
     *	three image planes.
     */
    public void configureVolumeFrame() {

        /** Extents of image A, same as xDim, yDim, etc. */
        int[] extents;

        if ( imageA == null ) {
            return;
        }

        if ( componentVolImage == null ) {
            if ( canvas.supportsTexture3D() ) {
                // build 3D texture
                texture = new VolumeTexture( imageA );
                if ( isViewTextureAligned ) {
                    volRenderNode = new NodeAlignedVolumeTextureRender( imageA, texture );
                } else {
                    volRenderNode = new NodeVolumeTextureRender( imageA, texture );
                }
            }

            if ( ( ( imageA.getNDims() == 3 ) && ( imageB == null ) )
                    || ( ( imageA.getNDims() == 3 ) && ( imageB != null ) && ( imageB.getNDims() == 3 ) ) ) {
                extents = new int[3];
                extents[0] = xDim;
                extents[1] = yDim;
                extents[2] = zDim;
            } else { // imageA.getNDims() == 4 or imageB.getNDims() == 4
                extents = new int[4];
                extents[0] = xDim;
                extents[1] = yDim;
                extents[2] = zDim;
                if ( imageA.getNDims() == 4 ) {
                    extents[3] = tDim;
                } else {
                    extents[3] = tDim;
                }
            }

            imageVolBufferA = new float[bufferFactor * xDim * yDim];
            if ( imageB != null ) {
                imageVolBufferB = new float[bufferFactor * imageB.getSliceSize()];
            }

            componentVolImage = new ViewJComponentSurfaceVolume( (RenderViewBase) this, imageA, this.LUTa,
                    imageVolBufferA, imageB, this.LUTb, imageVolBufferB, texture, extents );
            // if this is a color image, then update the RGB info in the component
            if ( imageA.isColorImage() ) {
                if ( componentVolImage.getRGBTA() == null ) {
                    int[] RGBExtents = new int[2];

                    RGBExtents[0] = 4;
                    RGBExtents[1] = 256;

                    ModelRGB rgb = new ModelRGB( RGBExtents );

                    componentVolImage.setRGBTA( rgb );
                }
            } // end if image is an RGB type
            componentVolImage.show( 0, null, null, true );

            volRenderBG = new BranchGroup();
            volRenderBG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
            volRenderBG.setCapability( Group.ALLOW_CHILDREN_READ );
            volRenderBG.setCapability( Group.ALLOW_CHILDREN_WRITE );
            volRenderBG.setCapability( BranchGroup.ALLOW_DETACH );
            volRenderBG.setPickable( false );
            volRenderBG.addChild( volRenderNode );
            volRenderOG.insertChild( volRenderBG, 0 );
            updateTextureVolumeRender();
        }
    }

    /**
     *	Constructs main frame structures for 3 image planes.  Makes the LUT if necessary,
     *	then sets up the buffer arrays appropriately and calls the constructors for the
     *	three image planes.  Used by the switch between aligned volume texture rendering
     *     and the volume texture rendering.
     *  Changed to public to allow updating after the volume is sculpted.
     */
    public void reConfigureVolumeFrame() {

        /** Extents of image A, same as xDim, yDim, etc. */
        int[] extents;

        if ( imageA == null ) {
            return;
        }

        if ( canvas.supportsTexture3D() ) {
            // build 3D texture
            texture = new VolumeTexture( imageA );
            if ( isViewTextureAligned ) {
                volRenderNode = new NodeAlignedVolumeTextureRender( imageA, texture );
            } else {
                volRenderNode = new NodeVolumeTextureRender( imageA, texture );
            }
        }

        if ( ( ( imageA.getNDims() == 3 ) && ( imageB == null ) )
                || ( ( imageA.getNDims() == 3 ) && ( imageB != null ) && ( imageB.getNDims() == 3 ) ) ) {
            extents = new int[3];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
        } else { // imageA.getNDims() == 4 or imageB.getNDims() == 4
            extents = new int[4];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
            if ( imageA.getNDims() == 4 ) {
                extents[3] = tDim;
            } else {
                extents[3] = tDim;
            }
        }

        imageVolBufferA = new float[bufferFactor * xDim * yDim];
        if ( imageB != null ) {
            imageVolBufferB = new float[bufferFactor * imageB.getSliceSize()];
        }

        componentVolImage = new ViewJComponentSurfaceVolume( this, imageA, this.LUTa, imageVolBufferA, imageB, this.LUTb,
                imageVolBufferB, texture, extents );

        // if this is a color image, then update the RGB info in the component
        if ( imageA.isColorImage() ) {
            if ( componentVolImage.getRGBTA() == null ) {
                int[] RGBExtents = new int[2];

                RGBExtents[0] = 4;
                RGBExtents[1] = 256;

                ModelRGB rgb = new ModelRGB( RGBExtents );

                componentVolImage.setRGBTA( rgb );
            }
        } // end if image is an RGB type
        synchronized ( this ) {

            volRenderBG.detach();
            volRenderBG.removeAllChildren();
            componentVolImage.show( 0, null, null, true );
            volRenderBG.addChild( volRenderNode );
            volRenderOG.insertChild( volRenderBG, 0 );

            updateTextureVolumeRender();
        }
    }

    /**
     * Determine the current viewing transformation and pass it to the
     * texture volume renderer so that it can update itself.
     */
    private void updateTextureVolumeRender() {
        if ( volRenderNode == null ) {
            return;
        }

        // Update the lighting of the texture volume if lighting is
        // enabled and the mouse is not interactive.
        if ( m_bRenderModeLighting && !mouseRotateBehavior.isButtonPressed() ) {

            // Remove volume render node so that we can update its texture.
            volRenderOG.removeChild( volRenderOG.indexOfChild( volRenderBG ) );

            // Retrieve the composite image values from the 3D texture.
            int[] aiCompositeImageA = componentVolImage.getCompositeImageA();

            // Create a vertex property for software lighting.
            SoftwareVertexProperty kVertexProp = new SoftwareVertexProperty();

            kVertexProp.enableDiffuse( true );

            // Obtain the extents for image A.
            int[] aiExtents = imageA.getExtents();
            float fOffsetX = ( aiExtents[0] - 1 ) / 2.0f;
            float fOffsetY = ( aiExtents[1] - 1 ) / 2.0f;
            float fOffsetZ = ( aiExtents[2] - 1 ) / 2.0f;

            // Convert the eye point from world to model coordinates
            // and do the same for any world positioned lights.
            Matrix3f kMatrix3x3 = new Matrix3f();

            currentTransform.get( kMatrix3x3 );
            Point3f kWorldEye = new Point3f( myEyePoint );
            Point3f kModelEye = new Point3f();

            kMatrix3x3.transform( kWorldEye, kModelEye );
            kMatrix3x3.invert();
            Vector3f kV = new Vector3f();

            kMatrix3x3.getRow( 0, kV );
            kV.negate();
            kMatrix3x3.setRow( 0, kV );
            m_kSoftwareLightSet.applyWorldToModelTransform( kMatrix3x3 );

            // Loop through all of the voxels...
            int iVoxel = 0;

            for ( int iZ = 0; iZ < aiExtents[2]; iZ++ ) {
                float fZ = iZ - fOffsetZ;

                // Access a slice image values from the 3D texture for image A.
                int[] aiTextureSliceA = texture.getBufferedRaster( iZ );

                int iPixel = 0;

                for ( int iY = 0; iY < aiExtents[1]; iY++ ) {
                    float fY = iY - fOffsetY;

                    for ( int iX = 0; iX < aiExtents[0]; iX++ ) {
                        float fX = iX - fOffsetX;

                        // Set the cell position.
                        kVertexProp.setPosition( -fX, -fY, -fZ );

                        // Set the cell normal vector.
                        kVertexProp.setNormal( akNormalsImageA[iVoxel] );

                        // Set the cell color.
                        int iARGB = aiCompositeImageA[iVoxel];
                        float fR = ( ( iARGB >> 16 ) & 0x0ff ) / 255.0f;
                        float fG = ( ( iARGB >> 8 ) & 0x0ff ) / 255.0f;
                        float fB = ( ( iARGB ) & 0x0ff ) / 255.0f;

                        kVertexProp.setDiffuse( fR, fG, fB );

                        // Apply the lighting to determine the color
                        Color3f kColor = m_kSoftwareLightSet.getCellColor( m_kSoftwareMaterial, kVertexProp, kModelEye );

                        // Replace the composite RGB channel values in the
                        // texture image with that computed from the lighting.
                        aiTextureSliceA[iPixel++] = ( iARGB & 0xff000000 )
                                | ( ( (int) ( kColor.x * 255.0f ) & 0x0ff ) << 16 )
                                | ( ( (int) ( kColor.y * 255.0f ) & 0x0ff ) << 8 )
                                | ( ( (int) ( kColor.z * 255.0f ) & 0x0ff ) );
                        iVoxel++;
                    }
                }
                texture.setImageComponent( iZ );
            }

            // Return the volume render node to the scene graph.
            volRenderOG.insertChild( volRenderBG, 0 );
        }

        // Retrieve the current viewing transformation.
        TransformGroup kViewPlatformTransform = universe.getViewingPlatform().getViewPlatformTransform();
        Transform3D kViewTransform = new Transform3D();

        kViewPlatformTransform.getTransform( kViewTransform );

        if ( isViewTextureAligned == false ) {
            // Before projection, the viewing direction vector is (0,0,-1).
            // Transform this vector by the current viewing transform.
            Vector3d kViewDir = new Vector3d();

            kViewTransform.transform( new Vector3d( 0.0, 0.0, -1.0 ), kViewDir );

            // Allow the texture volume renderer to update itself based
            // on the current viewing transform.
            volRenderNode.updateSlicing( kViewDir );
            kViewPlatformTransform = null;
            kViewTransform = null;
            kViewDir = null;
        } else {
            volRenderNode.updateView( kViewTransform );
        }
    }

    /**
     *   Creates the scene graph, made up of a branch group parent, a transform group under that
     *	which applies mouse behavior and lights to the scene, and three branch groups under the
     *	transform group for each of the three image planes.  The surfaces that can be added would
     *	be children of the transform group.  Each image plane has a transform group associated with it
     *	and beneath that, a box shape where the texture maps are applied.  The shape is what is actually
     *	displayed.
     */
    private void createImageSceneGraph() {

        xBox = ( xDim - 1 ) * resols[0];
        yBox = ( yDim - 1 ) * resols[1];
        zBox = ( zDim - 1 ) * resols[2];
        maxBox = xBox;
        if ( yBox > maxBox ) {
            maxBox = yBox;
        }
        if ( zBox > maxBox ) {
            maxBox = zBox;
        }

        // Define vectors used for mapping each coordinate from its
        // real range X=[0,xdim-1], Y=[0,ydim-1], Z=[0,zdim-1] into
        // the (s,t,r) texture coordinates where each texture coordinate
        // is defined over the range [0,1].
        // Note that integral screen coordinates represent pixel centers
        // whereas integral texture coordinates represent texel boundaries.
        // Therefore, X maps to s=(X+0.5)/xdim, and correspondingly
        // for Y and Z mapping to t and r, respectively.
        m_kCoordMapX = new Vector4f( ( maxBox / xBox ) / 2.0f, 0.0f, 0.0f, 0.5f );
        m_kCoordMapY = new Vector4f( 0.0f, ( maxBox / yBox ) / 2.0f, 0.0f, 0.5f );
        m_kCoordMapZ = new Vector4f( 0.0f, 0.0f, -( xBox / zBox ) / 2.0f, 0.5f );
        m_kCoordMapZNeg = new Vector4f( 0.0f, 0.0f, ( xBox / zBox ) / 2.0f, 0.5f );

        // Normalize the size
        // xBox range between 0 - 1.
        xBox = xBox / maxBox;
        yBox = yBox / maxBox;
        zBox = zBox / maxBox;

        // Create a Transformgroup to scale all objects so they
        // appear in the scene.
        transformNode3d = new Transform3D();
        transformNode3d.setScale( 0.45f );

        BranchGroup pointBG = new BranchGroup();

        pointBG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        pointBG.setCapability( Group.ALLOW_CHILDREN_READ );
        pointBG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        pointBG.setCapability( BranchGroup.ALLOW_DETACH );
        Appearance appearance = new Appearance();

        appearance.setCapability( Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE );
        TransparencyAttributes tap = new TransparencyAttributes();

        tap.setCapability( TransparencyAttributes.ALLOW_VALUE_WRITE );
        tap.setTransparencyMode( TransparencyAttributes.BLENDED );
        tap.setTransparency( 1.0f );
        appearance.setTransparencyAttributes( tap );
        Shape3D pointShape = new Shape3D( new PointArray( 1, 1 ), appearance );

        pointBG.addChild( pointShape );

        volRenderOG = new OrderedGroup();
        volRenderOG.setCapability( Group.ALLOW_CHILDREN_READ );
        volRenderOG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        volRenderOG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        volRenderOG.setCapability( BranchGroup.ALLOW_DETACH );
        volRenderOG.addChild( pointBG );

        volBG = new BranchGroup();
        volBG.setCapability( Group.ALLOW_CHILDREN_READ );
        volBG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        volBG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        volBG.setCapability( BranchGroup.ALLOW_DETACH );
        volBG.addChild( volRenderOG );

        sceneRootTG.setTransform( transformNode3d );
        sceneRootTG.setCapability( Group.ALLOW_CHILDREN_READ );
        sceneRootTG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        sceneRootTG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        sceneRootTG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        sceneRootTG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        sceneRootTG.setCapability( TransformGroup.ENABLE_PICK_REPORTING );
        sceneRootTG.setCapability( Node.ALLOW_LOCAL_TO_VWORLD_READ );

        // here, we switch group between triplanar view and 3D texture volume view
        switchGroup = new Switch();
        switchGroup.setWhichChild( Switch.CHILD_NONE );
        switchGroup.setCapability( Switch.ALLOW_SWITCH_WRITE );
        switchGroup.setCapability( Switch.ALLOW_CHILDREN_WRITE );
        switchGroup.setCapability( Switch.ALLOW_CHILDREN_READ );

        triPlanarViewBG = new BranchGroup();
        triPlanarViewBG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        triPlanarViewBG.setCapability( Group.ALLOW_CHILDREN_READ );
        triPlanarViewBG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        triPlanarViewBG.setCapability( BranchGroup.ALLOW_DETACH );

        switchGroup.addChild( triPlanarViewBG );
        switchGroup.addChild( volBG );
        switchGroup.setWhichChild( 0 );

        sceneRootTG.addChild( switchGroup );

        objXYPlaneBG = new BranchGroup();
        objXYPlaneBG.setCapability( BranchGroup.ALLOW_DETACH );
        triPlanarViewBG.addChild( objXYPlaneBG );

        objTransXY_TG = new TransformGroup();
        objTransXY_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        objTransXY_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        objTransXY_TG.setCapability( TransformGroup.ENABLE_PICK_REPORTING );
        objXYPlaneBG.setPickable( false );

        /* Z-Probe Transform Group */
        m_kObjPlaneProbeZ_TG = new TransformGroup();
        m_kObjPlaneProbeZ_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        m_kObjPlaneProbeZ_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        m_kObjPlaneProbeZ_TG.setCapability( Group.ALLOW_CHILDREN_READ );
        m_kObjPlaneProbeZ_TG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        m_kObjPlaneProbeZ_TG.addChild( objTransXY_TG );
        objXYPlaneBG.addChild( m_kObjPlaneProbeZ_TG );

        objXZPlaneBG = new BranchGroup();
        objXZPlaneBG.setCapability( BranchGroup.ALLOW_DETACH );
        triPlanarViewBG.addChild( objXZPlaneBG );

        objTransXZ_TG = new TransformGroup();
        objTransXZ_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        objTransXZ_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        objTransXZ_TG.setCapability( TransformGroup.ENABLE_PICK_REPORTING );
        objXZPlaneBG.setPickable( false );

        /* Y-Probe Transform Group */
        m_kObjPlaneProbeY_TG = new TransformGroup();
        m_kObjPlaneProbeY_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        m_kObjPlaneProbeY_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        m_kObjPlaneProbeY_TG.setCapability( Group.ALLOW_CHILDREN_READ );
        m_kObjPlaneProbeY_TG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        m_kObjPlaneProbeY_TG.addChild( objTransXZ_TG );
        objXZPlaneBG.addChild( m_kObjPlaneProbeY_TG );

        objZYPlaneBG = new BranchGroup();
        objZYPlaneBG.setCapability( BranchGroup.ALLOW_DETACH );
        triPlanarViewBG.addChild( objZYPlaneBG );

        objTransZY_TG = new TransformGroup();
        objTransZY_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        objTransZY_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        objTransZY_TG.setCapability( TransformGroup.ENABLE_PICK_REPORTING );
        objZYPlaneBG.setPickable( false );

        /* X-Probe Transform Group */
        m_kObjPlaneProbeX_TG = new TransformGroup();
        m_kObjPlaneProbeX_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        m_kObjPlaneProbeX_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        m_kObjPlaneProbeX_TG.setCapability( Group.ALLOW_CHILDREN_READ );
        m_kObjPlaneProbeX_TG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        m_kObjPlaneProbeX_TG.addChild( objTransZY_TG );
        objZYPlaneBG.addChild( m_kObjPlaneProbeX_TG );

        objBoxSliceX_BG = new BranchGroup();
        objBoxSliceX_BG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        objBoxSliceX_BG.setCapability( Group.ALLOW_CHILDREN_READ );
        objBoxSliceX_BG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        objBoxSliceX_BG.setCapability( BranchGroup.ALLOW_DETACH );
        objBoxSliceX_BG.setPickable( false );

        objBoxSliceY_BG = new BranchGroup();
        objBoxSliceY_BG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        objBoxSliceY_BG.setCapability( Group.ALLOW_CHILDREN_READ );
        objBoxSliceY_BG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        objBoxSliceY_BG.setCapability( BranchGroup.ALLOW_DETACH );
        objBoxSliceY_BG.setPickable( false );

        objBoxSliceZ_BG = new BranchGroup();
        objBoxSliceZ_BG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        objBoxSliceZ_BG.setCapability( Group.ALLOW_CHILDREN_READ );
        objBoxSliceZ_BG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        objBoxSliceZ_BG.setCapability( BranchGroup.ALLOW_DETACH );
        objBoxSliceZ_BG.setPickable( false );

        TransformGroup objTransSliceX = new TransformGroup();

        objTransSliceX.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        objTransSliceX.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        objTransSliceX.setCapability( TransformGroup.ENABLE_PICK_REPORTING );
        objBoxSliceX_BG.addChild( objTransSliceX );

        TransformGroup objTransSliceY = new TransformGroup();

        objTransSliceY.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        objTransSliceY.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        objTransSliceY.setCapability( TransformGroup.ENABLE_PICK_REPORTING );
        objBoxSliceY_BG.addChild( objTransSliceY );

        TransformGroup objTransSliceZ = new TransformGroup();

        objTransSliceZ.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        objTransSliceZ.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        objTransSliceZ.setCapability( TransformGroup.ENABLE_PICK_REPORTING );
        objBoxSliceZ_BG.addChild( objTransSliceZ );

        m_kObjBoxSliceProbeX_TG = new TransformGroup();
        m_kObjBoxSliceProbeX_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        m_kObjBoxSliceProbeX_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        m_kObjBoxSliceProbeX_TG.setCapability( Group.ALLOW_CHILDREN_READ );
        m_kObjBoxSliceProbeX_TG.setCapability( Group.ALLOW_CHILDREN_WRITE );

        m_kObjBoxSliceProbeY_TG = new TransformGroup();
        m_kObjBoxSliceProbeY_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        m_kObjBoxSliceProbeY_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        m_kObjBoxSliceProbeY_TG.setCapability( Group.ALLOW_CHILDREN_READ );
        m_kObjBoxSliceProbeY_TG.setCapability( Group.ALLOW_CHILDREN_WRITE );

        m_kObjBoxSliceProbeZ_TG = new TransformGroup();
        m_kObjBoxSliceProbeZ_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        m_kObjBoxSliceProbeZ_TG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        m_kObjBoxSliceProbeZ_TG.setCapability( Group.ALLOW_CHILDREN_READ );
        m_kObjBoxSliceProbeZ_TG.setCapability( Group.ALLOW_CHILDREN_WRITE );

        buildCubicBranch();

        texXYImageComp2D = new ImageComponent2D( ImageComponent.FORMAT_RGBA, imageA.getExtents()[0],
                imageA.getExtents()[1] );
        texXYImageComp2D.setCapability( ImageComponent2D.ALLOW_IMAGE_WRITE );
        texXYImageComp2D.setCapability( ImageComponent2D.ALLOW_IMAGE_READ );
        texXYImageComp2D.setCapability( ImageComponent2D.ALLOW_SIZE_READ );
        texXYImageComp2D.setCapability( ImageComponent2D.ALLOW_FORMAT_READ );
        texXYImageComp2D.set( componentImageXY.getImage() );

        // Replace by Texture2D
        texXY = new Texture2D( Texture.BASE_LEVEL, Texture.RGBA, imageA.getExtents()[0], imageA.getExtents()[1] );
        texXY.setEnable( true );
        texXY.setMinFilter( Texture.BASE_LEVEL_LINEAR );
        texXY.setMagFilter( Texture.BASE_LEVEL_LINEAR );
        texXY.setBoundaryModeS( Texture.CLAMP_TO_EDGE );
        texXY.setBoundaryModeT( Texture.CLAMP_TO_EDGE );
        texXY.setImage( 0, texXYImageComp2D );
        texXY.setCapability( Texture2D.ALLOW_IMAGE_WRITE );
        texXY.setCapability( Texture2D.ALLOW_IMAGE_READ );
        texXY.setCapability( Texture2D.ALLOW_ENABLE_WRITE );

        texXZImageComp2D = new ImageComponent2D( ImageComponent.FORMAT_RGBA, imageA.getExtents()[0],
                imageA.getExtents()[2] );
        texXZImageComp2D.setCapability( ImageComponent2D.ALLOW_IMAGE_WRITE );
        texXZImageComp2D.setCapability( ImageComponent2D.ALLOW_IMAGE_READ );
        texXZImageComp2D.setCapability( ImageComponent2D.ALLOW_SIZE_READ );
        texXZImageComp2D.setCapability( ImageComponent2D.ALLOW_FORMAT_READ );
        texXZImageComp2D.set( componentImageXZ.getImage() );

        texXZ = new Texture2D( Texture.BASE_LEVEL, Texture.RGBA, imageA.getExtents()[0], imageA.getExtents()[2] );
        texXZ.setEnable( true );
        texXZ.setMinFilter( Texture.BASE_LEVEL_LINEAR );
        texXZ.setMagFilter( Texture.BASE_LEVEL_LINEAR );
        texXZ.setBoundaryModeS( Texture.CLAMP_TO_EDGE );
        texXZ.setBoundaryModeT( Texture.CLAMP_TO_EDGE );
        texXZ.setImage( 0, texXZImageComp2D );
        texXZ.setCapability( Texture2D.ALLOW_IMAGE_WRITE );
        texXZ.setCapability( Texture2D.ALLOW_IMAGE_READ );
        texXZ.setCapability( Texture2D.ALLOW_ENABLE_WRITE );

        texZYImageComp2D = new ImageComponent2D( ImageComponent.FORMAT_RGBA, imageA.getExtents()[2],
                imageA.getExtents()[1] );
        texZYImageComp2D.setCapability( ImageComponent2D.ALLOW_IMAGE_WRITE );
        texZYImageComp2D.setCapability( ImageComponent2D.ALLOW_IMAGE_READ );
        texZYImageComp2D.setCapability( ImageComponent2D.ALLOW_SIZE_READ );
        texZYImageComp2D.setCapability( ImageComponent2D.ALLOW_FORMAT_READ );
        texZYImageComp2D.set( componentImageZY.getImage() );

        texZY = new Texture2D( Texture.BASE_LEVEL, Texture.RGBA, imageA.getExtents()[2], imageA.getExtents()[1] );
        texZY.setEnable( true );
        texZY.setMinFilter( Texture.BASE_LEVEL_LINEAR );
        texZY.setMagFilter( Texture.BASE_LEVEL_LINEAR );
        texZY.setBoundaryModeS( Texture.CLAMP_TO_EDGE );
        texZY.setBoundaryModeT( Texture.CLAMP_TO_EDGE );
        texZY.setImage( 0, texZYImageComp2D );
        texZY.setCapability( Texture2D.ALLOW_IMAGE_WRITE );
        texZY.setCapability( Texture2D.ALLOW_IMAGE_READ );
        texZY.setCapability( Texture2D.ALLOW_ENABLE_WRITE );

        // ****************  Box for XY Plane
        Box imPlane1 = new Box( xBox, yBox, 0.0f, Box.GENERATE_TEXTURE_COORDS, null, 1 );

        taXY = new TransparencyAttributes();
        taXY.setCapability( TransparencyAttributes.ALLOW_VALUE_WRITE );
        taXY.setCapability( TransparencyAttributes.ALLOW_MODE_WRITE );
        taXY.setCapability( TransparencyAttributes.ALLOW_BLEND_FUNCTION_WRITE );
        taXY.setTransparencyMode( TransparencyAttributes.NONE );
        taXY.setTransparency( 0.0f ); // 0 = Opaque

        TextureAttributes ta = new TextureAttributes();

        ta.setTextureMode( TextureAttributes.MODULATE );

        Appearance appFXY = new Appearance();

        appFXY.setTransparencyAttributes( taXY );
        appFXY.setTextureAttributes( ta );

        appFXY.setTexture( texXY );
        appFXY.setTexCoordGeneration(
                new TexCoordGeneration( TexCoordGeneration.OBJECT_LINEAR, TexCoordGeneration.TEXTURE_COORDINATE_2,
                m_kCoordMapX, m_kCoordMapY ) );

        Shape3D shape = imPlane1.getShape( Box.FRONT );

        shape.setAppearance( appFXY );

        Appearance appBXY = new Appearance();

        appBXY.setTransparencyAttributes( taXY );
        appBXY.setTextureAttributes( ta );

        appBXY.setTexCoordGeneration(
                new TexCoordGeneration( TexCoordGeneration.OBJECT_LINEAR, TexCoordGeneration.TEXTURE_COORDINATE_2,
                m_kCoordMapX, m_kCoordMapY ) );
        appBXY.setTexture( texXY );

        shape = imPlane1.getShape( Box.BACK );
        shape.setAppearance( appBXY );

        objTransXY_TG.addChild( imPlane1 );

        // ****************  Box for XZ Plane
        Box imPlane2 = new Box( xBox, 0.0f, zBox, Box.GENERATE_TEXTURE_COORDS, null, 1 );

        objTransXZ_TG.addChild( imPlane2 );

        taXZ = new TransparencyAttributes();
        taXZ.setCapability( TransparencyAttributes.ALLOW_VALUE_WRITE );
        taXZ.setCapability( TransparencyAttributes.ALLOW_MODE_WRITE );
        taXZ.setCapability( TransparencyAttributes.ALLOW_BLEND_FUNCTION_WRITE );
        taXZ.setTransparencyMode( TransparencyAttributes.NONE );
        taXZ.setTransparency( 0.0f ); // 0 = Opaque

        shape = imPlane2.getShape( Box.TOP );

        Appearance appTXZ = new Appearance();

        appTXZ.setTransparencyAttributes( taXZ );
        appTXZ.setTextureAttributes( ta );
        appTXZ.setTexture( texXZ );
        appTXZ.setTexCoordGeneration(
                new TexCoordGeneration( TexCoordGeneration.OBJECT_LINEAR, TexCoordGeneration.TEXTURE_COORDINATE_2,
                m_kCoordMapX, m_kCoordMapZNeg ) );

        shape.setAppearance( appTXZ );

        shape = imPlane2.getShape( Box.BOTTOM );

        Appearance appBXZ = new Appearance();

        appBXZ.setTransparencyAttributes( taXZ );
        appBXZ.setTextureAttributes( ta );
        appBXZ.setTexture( texXZ );
        appBXZ.setTexCoordGeneration(
                new TexCoordGeneration( TexCoordGeneration.OBJECT_LINEAR, TexCoordGeneration.TEXTURE_COORDINATE_2,
                m_kCoordMapX, m_kCoordMapZNeg ) );

        shape.setAppearance( appBXZ );

        // *********************  Box for ZY Plane
        Box imPlane3 = new Box( 0.0f, yBox, zBox, Box.GENERATE_TEXTURE_COORDS, null, 1 );

        objTransZY_TG.addChild( imPlane3 );

        shape = imPlane3.getShape( Box.LEFT );
        TextureAttributes texAttrLZY = new TextureAttributes();

        texAttrLZY.setTextureMode( TextureAttributes.DECAL );
        texAttrLZY.setPerspectiveCorrectionMode( TextureAttributes.NICEST );
        Appearance appLZY = new Appearance();

        taZY = new TransparencyAttributes();
        taZY.setCapability( TransparencyAttributes.ALLOW_VALUE_WRITE );
        taZY.setCapability( TransparencyAttributes.ALLOW_MODE_WRITE );
        taZY.setCapability( TransparencyAttributes.ALLOW_BLEND_FUNCTION_WRITE );

        taZY.setTransparencyMode( TransparencyAttributes.NONE );
        taZY.setTransparency( 0.0f ); // 0 = Opaque
        appLZY.setTransparencyAttributes( taZY );
        appLZY.setTextureAttributes( ta );
        appLZY.setTexture( texZY );
        appLZY.setTexCoordGeneration(
                new TexCoordGeneration( TexCoordGeneration.OBJECT_LINEAR, TexCoordGeneration.TEXTURE_COORDINATE_2,
                m_kCoordMapZ, m_kCoordMapY ) );

        shape.setAppearance( appLZY );

        shape = imPlane3.getShape( Box.RIGHT );

        TextureAttributes texAttrRZY = new TextureAttributes();

        texAttrRZY.setTextureMode( TextureAttributes.DECAL );
        texAttrRZY.setPerspectiveCorrectionMode( TextureAttributes.NICEST );

        Appearance appRZY = new Appearance();

        appRZY.setTransparencyAttributes( taZY );
        appRZY.setTextureAttributes( ta );
        appRZY.setTexture( texZY );
        appRZY.setTexCoordGeneration(
                new TexCoordGeneration( TexCoordGeneration.OBJECT_LINEAR, TexCoordGeneration.TEXTURE_COORDINATE_2,
                m_kCoordMapZ, m_kCoordMapY ) );

        shape.setAppearance( appRZY );

        // Creates a bounding frame around the view
        createBoxFrame( xBox, yBox, zBox );

        // Setup X box frame
        switch ( getLabel( X, false ) ) {
        case X:
            boxSliceX = new ViewJComponentBoxSlice( 0, yBox, zBox, ViewJComponentBoxSlice.X_SLICE );
            break;

        case Y:
            boxSliceX = new ViewJComponentBoxSlice( xBox, 0, zBox, ViewJComponentBoxSlice.Y_SLICE );
            break;

        case Z:
            boxSliceX = new ViewJComponentBoxSlice( xBox, yBox, 0, ViewJComponentBoxSlice.Z_SLICE );
            break;
        }

        boxSliceX.setColor( Color.yellow );

        switch ( getLabel( Y, false ) ) {
        case X:
            boxSliceY = new ViewJComponentBoxSlice( 0, yBox, zBox, ViewJComponentBoxSlice.X_SLICE );
            break;

        case Y:
            boxSliceY = new ViewJComponentBoxSlice( xBox, 0, zBox, ViewJComponentBoxSlice.Y_SLICE );
            break;

        case Z:
            boxSliceY = new ViewJComponentBoxSlice( xBox, yBox, 0, ViewJComponentBoxSlice.Z_SLICE );
            break;
        }

        boxSliceY.setColor( Color.green );

        switch ( getLabel( Z, false ) ) {
        case X:
            boxSliceZ = new ViewJComponentBoxSlice( 0, yBox, zBox, ViewJComponentBoxSlice.X_SLICE );
            break;

        case Y:
            boxSliceZ = new ViewJComponentBoxSlice( xBox, 0, zBox, ViewJComponentBoxSlice.Y_SLICE );
            break;

        case Z:
            boxSliceZ = new ViewJComponentBoxSlice( xBox, yBox, 0, ViewJComponentBoxSlice.Z_SLICE );
            break;
        }
        boxSliceZ.setColor( Color.blue );

        /* Determine which boxSlice is the X_Slice, Y_Slice and Z_Slice, set
         * the m_kObjBoxVerts appropriately: */
        if ( boxSliceX.getMode() == ViewJComponentBoxSlice.X_SLICE ) {
            m_kObjBoxVertsX = boxSliceX.getVertices();
            objBoxSliceX_BG.addChild( m_kObjBoxSliceProbeX_TG );
        } else if ( boxSliceX.getMode() == ViewJComponentBoxSlice.Y_SLICE ) {
            m_kObjBoxVertsY = boxSliceX.getVertices();
            objBoxSliceY_BG.addChild( m_kObjBoxSliceProbeY_TG );
        } else if ( boxSliceX.getMode() == ViewJComponentBoxSlice.Z_SLICE ) {
            m_kObjBoxVertsZ = boxSliceX.getVertices();
            objBoxSliceZ_BG.addChild( m_kObjBoxSliceProbeZ_TG );
        }

        /* Determine which boxSlice is the X_Slice, Y_Slice and Z_Slice, set
         * the m_kObjBoxVerts appropriately: */
        if ( boxSliceY.getMode() == ViewJComponentBoxSlice.X_SLICE ) {
            m_kObjBoxVertsX = boxSliceY.getVertices();
            objBoxSliceX_BG.addChild( m_kObjBoxSliceProbeX_TG );
        } else if ( boxSliceY.getMode() == ViewJComponentBoxSlice.Y_SLICE ) {
            m_kObjBoxVertsY = boxSliceY.getVertices();
            objBoxSliceY_BG.addChild( m_kObjBoxSliceProbeY_TG );
        } else if ( boxSliceY.getMode() == ViewJComponentBoxSlice.Z_SLICE ) {
            m_kObjBoxVertsZ = boxSliceY.getVertices();
            objBoxSliceZ_BG.addChild( m_kObjBoxSliceProbeZ_TG );
        }

        /* Determine which boxSlice is the X_Slice, Y_Slice and Z_Slice, set
         * the m_kObjBoxVerts appropriately: */
        if ( boxSliceZ.getMode() == ViewJComponentBoxSlice.X_SLICE ) {
            m_kObjBoxVertsX = boxSliceZ.getVertices();
            objBoxSliceX_BG.addChild( m_kObjBoxSliceProbeX_TG );
        } else if ( boxSliceZ.getMode() == ViewJComponentBoxSlice.Y_SLICE ) {
            m_kObjBoxVertsY = boxSliceZ.getVertices();
            objBoxSliceY_BG.addChild( m_kObjBoxSliceProbeY_TG );
        } else if ( boxSliceZ.getMode() == ViewJComponentBoxSlice.Z_SLICE ) {
            m_kObjBoxVertsZ = boxSliceZ.getVertices();
            objBoxSliceZ_BG.addChild( m_kObjBoxSliceProbeZ_TG );
        }

        objBehaviorBG = new BranchGroup();
        objBehaviorBG.setCapability( BranchGroup.ALLOW_DETACH );
        objBehaviorBG.setCapability( Group.ALLOW_CHILDREN_READ );
        objBehaviorBG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        objBehaviorBG.setCapability( Group.ALLOW_CHILDREN_EXTEND );

        // Create the rotate behavior node
        mouseRotateBehavior = new MouseRotateExt( canvas, sceneRootTG );
        objBehaviorBG.addChild( mouseRotateBehavior );
        mouseRotateBehavior.setSchedulingBounds( bounds );
        mouseRotateBehavior.setupCallback( this );
        mouseRotateBehavior.setFactor( 0.005 );

        // Create the zoom behavior node
        mouseZoomBehavior = new MouseZoom( canvas, sceneRootTG );
        objBehaviorBG.addChild( mouseZoomBehavior );
        mouseZoomBehavior.setupCallback( this );
        mouseZoomBehavior.setSchedulingBounds( bounds );
        mouseZoomBehavior.setFactor( 0.005 );

        // Create the translate behavior node
        mouseTranslateBehavior = new MouseTranslate( canvas, sceneRootTG );
        objBehaviorBG.addChild( mouseTranslateBehavior );
        mouseTranslateBehavior.setupCallback( this );
        mouseTranslateBehavior.setSchedulingBounds( bounds );
        mouseTranslateBehavior.setFactor( 0.005 );
        sceneRootTG.addChild( objBehaviorBG );

    }

    /**
     *  Enable objBehavior branch group
     *  @param flag     <code>true</code> means turn on, <code>false</code>
     *                  means turn off.
     */
    public void enableObjBehavior( boolean flag ) {

        if ( flag ) {
            if ( !objBehaviorBG.isLive() ) {
                sceneRootTG.addChild( objBehaviorBG );
            }
            if ( cubicTG.indexOfChild( cubicBehaviorBG ) == -1 ) {
                cubicTG.addChild( cubicBehaviorBG );
            }
        } else {
            if ( objBehaviorBG.isLive() ) {
                objBehaviorBG.detach();
            }
            if ( cubicTG.indexOfChild( cubicBehaviorBG ) != -1 ) {
                cubicBehaviorBG.detach();
            }
        }
    }

    /**
     *  Accessor that returns the reference to image A.
     *  @return Image A.
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     *  Accessor that returns the reference to image B.
     *  @return Image B.
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     *  Accessor that sets the volumeDisplayMode flag. If true image is displayed
     *  as a volume. If false image is displayed as three orthogonal planes.
     *  @param volDisplayMode if true image is displayed as a volume
     *  ( 3D texture or 2D array Texture).
     */
    public void setVolumeDisplayMode( boolean volDisplayMode ) {
        volumeDisplayMode3D = volDisplayMode;
    }

    /**
     *   Build the cubic branch under the objRootBG
     */
    private void buildCubicBranch() {
        cubicBG = new BranchGroup();
        cubicBG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        cubicBG.setCapability( Group.ALLOW_CHILDREN_READ );
        cubicBG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        cubicBG.setCapability( BranchGroup.ALLOW_DETACH );
        cubicBG.setPickable( false );

        cubicTG = new TransformGroup();
        cubicTG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        cubicTG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        cubicTG.setCapability( Group.ALLOW_CHILDREN_READ );
        cubicTG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        cubicTG.setCapability( TransformGroup.ENABLE_PICK_REPORTING );

        transformCubic = new Transform3D();
        transformCubic.setScale( 0.1f );
        transformCubic.setRotation( new AxisAngle4f( 1, 1, 1, (float) .52 ) );
        transformCubic.setTranslation( new Vector3f( -0.7f, -0.7f, -0.7f ) );
        cubicTG.setTransform( transformCubic );

        cubicBG.addChild( cubicTG );
        cubicTG.addChild( buildCubicBox() );

        cubicBehaviorBG = new BranchGroup();
        cubicBehaviorBG.setCapability( BranchGroup.ALLOW_DETACH );
        cubicBehaviorBG.setCapability( Group.ALLOW_CHILDREN_READ );
        cubicBehaviorBG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        cubicBehaviorBG.setCapability( Group.ALLOW_CHILDREN_EXTEND );

        cubicRotate = new MouseRotate();
        cubicRotate.setupCallback( this );
        cubicRotate.setTransformGroup( cubicTG );
        cubicRotate.setSchedulingBounds( bounds );
        cubicBehaviorBG.addChild( cubicRotate );
        cubicTG.addChild( cubicBehaviorBG );
        cubicRotate.setFactor( 0.005 );

    }

    /**
     *   Attach cubic control branch group.
     */
    public void addCubicControl() {
        if ( !cubicBG.isLive() ) {
            objRootBG.addChild( cubicBG );
        }
    }

    /**
     *  Detach cubic control branch group.
     */
    public void removeCubicControl() {
        if ( cubicBG.isLive() ) {
            cubicBG.detach();
        }
    }

    /**
     *   Create the rotation control cubic box.
     */
    private Box buildCubicBox() {
        Appearance app = new Appearance();
        Box cubic = new Box( 1.0f, 1.0f, 1.0f, Box.GENERATE_TEXTURE_COORDS, app, 1 );
        int xOrient = -1, yOrient = -1, zOrient = -1;

        if ( imageA != null ) {
            xOrient = imageA.getFileInfo( 0 ).getAxisOrientation()[0];
            yOrient = imageA.getFileInfo( 0 ).getAxisOrientation()[1];
            zOrient = imageA.getFileInfo( 0 ).getAxisOrientation()[2];
        }

        TextureLoader textLoadFront = null;

        try {
            textLoadFront = new TextureLoader( MipavUtil.getIconImage( "a.gif" ), this );
        } catch ( FileNotFoundException error ) {
            Preferences.debug(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
            System.err.println(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
        }

        Shape3D shapeFront = cubic.getShape( Box.FRONT );
        TextureAttributes textAttrFront = new TextureAttributes();

        textAttrFront.setTextureMode( TextureAttributes.DECAL );
        TextureUnitState[] textUnitStateFront = new TextureUnitState[1];

        textUnitStateFront[0] = new TextureUnitState( textLoadFront.getTexture(), textAttrFront, null );
        textUnitStateFront[0].setCapability( TextureUnitState.ALLOW_STATE_WRITE );
        Appearance appFront = new Appearance();

        appFront.setTextureUnitState( textUnitStateFront );

        TextureLoader textLoadBack = null;

        try {
            textLoadBack = new TextureLoader( MipavUtil.getIconImage( "p.gif" ), this );
        } catch ( FileNotFoundException error ) {
            Preferences.debug(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
            System.err.println(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
        }

        Shape3D shapeBack = cubic.getShape( Box.BACK );
        TextureAttributes textAttrBack = new TextureAttributes();

        textAttrBack.setTextureMode( TextureAttributes.DECAL );
        TextureUnitState[] textUnitStateBack = new TextureUnitState[1];

        textUnitStateBack[0] = new TextureUnitState( textLoadBack.getTexture(), textAttrBack, null );
        textUnitStateBack[0].setCapability( TextureUnitState.ALLOW_STATE_WRITE );
        Appearance appBack = new Appearance();

        appBack.setTextureUnitState( textUnitStateBack );

        TextureLoader textLoadTop = null;

        try {
            textLoadTop = new TextureLoader( MipavUtil.getIconImage( "s.gif" ), this );
        } catch ( FileNotFoundException error ) {
            Preferences.debug(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
            System.err.println(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
        }

        Shape3D shapeTop = cubic.getShape( Box.TOP );
        TextureAttributes textAttrTop = new TextureAttributes();

        textAttrTop.setTextureMode( TextureAttributes.DECAL );
        TextureUnitState[] textUnitStateTop = new TextureUnitState[1];

        textUnitStateTop[0] = new TextureUnitState( textLoadTop.getTexture(), textAttrTop, null );
        textUnitStateTop[0].setCapability( TextureUnitState.ALLOW_STATE_WRITE );
        Appearance appTop = new Appearance();

        appTop.setTextureUnitState( textUnitStateTop );

        TextureLoader textLoadBottom = null;

        try {
            textLoadBottom = new TextureLoader( MipavUtil.getIconImage( "i.gif" ), this );
        } catch ( FileNotFoundException error ) {
            Preferences.debug(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
            System.err.println(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
        }

        Shape3D shapeBottom = cubic.getShape( Box.BOTTOM );
        TextureAttributes textAttrBottom = new TextureAttributes();

        textAttrBottom.setTextureMode( TextureAttributes.DECAL );
        TextureUnitState[] textUnitStateBottom = new TextureUnitState[1];

        textUnitStateBottom[0] = new TextureUnitState( textLoadBottom.getTexture(), textAttrBottom, null );
        textUnitStateBottom[0].setCapability( TextureUnitState.ALLOW_STATE_WRITE );
        Appearance appBottom = new Appearance();

        appBottom.setTextureUnitState( textUnitStateBottom );

        TextureLoader textLoadLeft = null;

        try {
            textLoadLeft = new TextureLoader( MipavUtil.getIconImage( "l.gif" ), this );
        } catch ( FileNotFoundException error ) {
            Preferences.debug(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
            System.err.println(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
        }

        Shape3D shapeLeft = cubic.getShape( Box.LEFT );
        TextureAttributes textAttrLeft = new TextureAttributes();

        textAttrLeft.setTextureMode( TextureAttributes.DECAL );
        TextureUnitState[] textUnitStateLeft = new TextureUnitState[1];

        textUnitStateLeft[0] = new TextureUnitState( textLoadLeft.getTexture(), textAttrLeft, null );
        textUnitStateLeft[0].setCapability( TextureUnitState.ALLOW_STATE_WRITE );
        Appearance appLeft = new Appearance();

        appLeft.setTextureUnitState( textUnitStateLeft );

        TextureLoader textLoadRight = null;

        try {
            textLoadRight = new TextureLoader( MipavUtil.getIconImage( "r.gif" ), this );
        } catch ( FileNotFoundException error ) {
            Preferences.debug(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
            System.err.println(
                    "Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n" );
        }

        Shape3D shapeRight = cubic.getShape( Box.RIGHT );
        TextureAttributes textAttrRight = new TextureAttributes();

        textAttrRight.setTextureMode( TextureAttributes.DECAL );
        TextureUnitState[] textUnitStateRight = new TextureUnitState[1];

        textUnitStateRight[0] = new TextureUnitState( textLoadRight.getTexture(), textAttrRight, null );
        textUnitStateRight[0].setCapability( TextureUnitState.ALLOW_STATE_WRITE );
        Appearance appRight = new Appearance();

        appRight.setTextureUnitState( textUnitStateRight );

        switch ( zOrient ) {
        case FileInfoBase.ORI_P2A_TYPE:
            shapeFront.setAppearance( appBack );
            shapeBack.setAppearance( appFront );
            break;

        case FileInfoBase.ORI_A2P_TYPE:
            shapeFront.setAppearance( appFront );
            shapeBack.setAppearance( appBack );
            break;

        case FileInfoBase.ORI_S2I_TYPE:
            shapeFront.setAppearance( appTop );
            shapeBack.setAppearance( appBottom );
            break;

        case FileInfoBase.ORI_I2S_TYPE:
            shapeFront.setAppearance( appBottom );
            shapeBack.setAppearance( appTop );
            break;

        case FileInfoBase.ORI_L2R_TYPE:
            shapeFront.setAppearance( appLeft );
            shapeBack.setAppearance( appRight );
            break;

        case FileInfoBase.ORI_R2L_TYPE:
            shapeFront.setAppearance( appRight );
            shapeBack.setAppearance( appLeft );
            break;

        default:
            break;
        }
        switch ( yOrient ) {
        case FileInfoBase.ORI_P2A_TYPE:
            shapeTop.setAppearance( appBack );
            shapeBottom.setAppearance( appFront );
            break;

        case FileInfoBase.ORI_A2P_TYPE:
            shapeTop.setAppearance( appFront );
            Vector4f planeSBack = new Vector4f( -0.5f, 0.0f, 0.0f, -0.5f );
            Vector4f planeTBack = new Vector4f( 0.0f, 0.0f, -0.5f, -0.5f );
            TexCoordGeneration texGenBack = new TexCoordGeneration();

            texGenBack.setPlaneS( planeSBack );
            texGenBack.setPlaneT( planeTBack );
            textUnitStateBack[0].setTexCoordGeneration( texGenBack );
            appBack.setTextureUnitState( textUnitStateBack );
            shapeBottom.setAppearance( appBack ); // rotate
            break;

        case FileInfoBase.ORI_S2I_TYPE:
            shapeTop.setAppearance( appTop );
            shapeBottom.setAppearance( appBottom );
            break;

        case FileInfoBase.ORI_I2S_TYPE:
            shapeTop.setAppearance( appBottom );
            shapeBottom.setAppearance( appTop );
            break;

        case FileInfoBase.ORI_L2R_TYPE:
            shapeTop.setAppearance( appLeft );
            shapeBottom.setAppearance( appRight );
            break;

        case FileInfoBase.ORI_R2L_TYPE:
            shapeTop.setAppearance( appRight );
            shapeBottom.setAppearance( appLeft );
            break;

        default:
            break;
        }
        switch ( xOrient ) {
        case FileInfoBase.ORI_P2A_TYPE:
            shapeLeft.setAppearance( appBack );
            shapeRight.setAppearance( appFront );
            break;

        case FileInfoBase.ORI_A2P_TYPE:
            shapeLeft.setAppearance( appFront );
            shapeRight.setAppearance( appBack );
            break;

        case FileInfoBase.ORI_S2I_TYPE:
            shapeLeft.setAppearance( appTop );
            shapeRight.setAppearance( appBottom );
            break;

        case FileInfoBase.ORI_I2S_TYPE:
            shapeLeft.setAppearance( appBottom );
            shapeRight.setAppearance( appTop );
            break;

        case FileInfoBase.ORI_L2R_TYPE:
            shapeLeft.setAppearance( appLeft );
            shapeRight.setAppearance( appRight );
            break;

        case FileInfoBase.ORI_R2L_TYPE:

            Vector4f planeS = new Vector4f( 0.0f, 0.5f, 0.0f, -0.5f );
            Vector4f planeT = new Vector4f( 0.0f, 0.0f, -0.5f, -0.5f );
            TexCoordGeneration texGen = new TexCoordGeneration();

            texGen.setPlaneS( planeS );
            texGen.setPlaneT( planeT );
            textUnitStateRight[0].setTexCoordGeneration( texGen );
            appRight.setTextureUnitState( textUnitStateRight );
            shapeLeft.setAppearance( appRight ); // rotate

            Vector4f planeSLeft = new Vector4f( 0.0f, -0.5f, 0.0f, 0.5f );
            Vector4f planeTLeft = new Vector4f( 0.0f, 0.0f, -0.5f, -0.5f );
            TexCoordGeneration texGenLeft = new TexCoordGeneration();

            texGenLeft.setPlaneS( planeSLeft );
            texGenLeft.setPlaneT( planeTLeft );
            textUnitStateLeft[0].setTexCoordGeneration( texGenLeft );
            appLeft.setTextureUnitState( textUnitStateLeft );
            shapeRight.setAppearance( appLeft ); // rotate
            break;

        default:
            break;
        }
        return cubic;
    }

    /**
     *  Accessor that sets the LUT for image A.
     *  @param LUT  The LUT.
     */
    public void setLUTa( ModelLUT LUT ) {
        componentImageXY.setLUTa( LUT );
        componentImageXZ.setLUTa( LUT );
        componentImageZY.setLUTa( LUT );
    }

    /**
     *  Accessor that sets the LUT for image B.
     *  @param LUT  The LUT
     */
    public void setLUTb( ModelLUT LUT ) {
        componentImageXY.setLUTb( LUT );
        componentImageXZ.setLUTb( LUT );
        componentImageZY.setLUTb( LUT );
    }

    /**
     *   Sets the alpha blending of parameter for two image displays.
     *   @param value Amount [0,100] that is the percentage of Image A to be displayed.
     */
    public void setAlphaBlend( int value ) {

        if ( volumeDisplayMode3D == false ) {
            if ( componentImageXY != null ) {
                componentImageXY.setAlphaBlend( value );
            }
            if ( componentImageXZ != null ) {
                componentImageXZ.setAlphaBlend( value );
            }
            if ( componentImageZY != null ) {
                componentImageZY.setAlphaBlend( value );
            }
        }
    }

    /**
     *	Sets the RGB table for image A.
     *	@param RGBT	New RGB table for image A.
     */
    public void setRGBTA( ModelRGB RGBT ) {
        if ( componentImageXY != null ) {
            componentImageXY.setRGBTA( RGBT );
        }
        if ( componentImageXZ != null ) {
            componentImageXZ.setRGBTA( RGBT );
        }
        if ( componentImageZY != null ) {
            componentImageZY.setRGBTA( RGBT );
        }
        if ( componentVolImage != null ) {
            componentVolImage.setRGBTA( RGBT );
        }
    }

    /**
     *	Sets the RGB table for image B.
     *	@param RGBT	New RGB table for image B.
     */
    public void setRGBTB( ModelRGB RGBT ) {
        if ( componentImageXY != null ) {
            componentImageXY.setRGBTB( RGBT );
        }

        if ( componentImageXZ != null ) {
            componentImageXZ.setRGBTB( RGBT );
        }

        if ( componentImageZY != null ) {
            componentImageZY.setRGBTB( RGBT );
        }
        if ( componentVolImage != null ) {
            componentVolImage.setRGBTB( RGBT );
        }
    }

    /**
     *  Sets the time slice to be displayed and changes the image planes displayed.
     *  @param slice Indicates image time-slice (4th dimension) to be displayed.
     */
    public void setTimeSlice( int slice ) {

        if ( imageA.getNDims() == 4 ) {
            if ( slicePanel.tSlice < imageA.getExtents()[3] ) {
                slicePanel.tSlice = slice;
                updateImages( null, null, true, -1 );
            }
        } else if ( ( imageB != null ) && ( imageB.getNDims() == 4 ) ) {
            if ( slicePanel.tSlice < imageB.getExtents()[3] ) {
                slicePanel.tSlice = slice;
                updateImages( null, null, true, -1 );
            }
        } else {
            return;
        }
    }

    /**
     *   This methods calls the componentImage's update method
     *   to redraw the screen - fastest of the three update methods.
     *   @return	Confirms successful update.
     */
    public boolean updateImages() {

        if ( componentImageXY == null || componentImageZY == null || componentImageXZ == null ) {
            return false;
        }
        updateVolume( null, null, false );
        return true;
    }

    /**
     *   This methods calls the componentImage's update method
     *   to redraw the screen. Without LUT changes.
     *   @param flag       forces show to re import image and calc. java image
     *   @return           boolean confirming successful update
     */
    public boolean updateImages( boolean flag ) {
        if ( update3DTriplanar( null, null, flag ) == false ) {
            return false;
        }

        if ( updateVolume( null, null, flag ) == false ) {
            return false;
        }
        return true;
    }

    /**
     *   This methods calls the componentImage's update method  to redraw the screen. Without LUT changes.
     *   @param forceShow  Forces show to reimport image and calculate java image.
     *   @return           Confirms successful update.
     */
    public boolean updateImages( ModelLUT LUTa, ModelLUT LUTb, boolean forceShow, int interpMode ) {
        boolean success = false;

        if ( getDisplayMode3D() == true ) {
            success = updateVolume( LUTa, LUTb, forceShow );
        } else {
            success = update3DTriplanar( LUTa, LUTb, forceShow );
        }

        return success;
    }

    /**
     *   This methods calls the componentImage's update method to redraw the screen.
     *   @param LUTa       LUT used to update imageA.
     *   @param LUTb       LUT used to update imageB.
     *   @param forceShow  Forces show to reimport image and calculate java image.
     *   @param interpMode Image interpolation method (Nearest or Smooth).
     *   @return           Confirms successful update.
     */
    public boolean update3DTriplanar( ModelLUT LUTa, ModelLUT LUTb, boolean forceShow ) {

        if ( componentImageXY == null || componentImageXZ == null || componentImageZY == null ) {
            return false;
        }
        if ( getDisplayMode3D() ) {
            return false;
        }
        if ( componentImageXY.show( slicePanel.tSlice, slicePanel.zSlice, LUTa, LUTb, forceShow, m_kProbeTransform,
                m_kObjBoxVertsZ )
                == true ) {
            texXYImageComp2D.set( componentImageXY.getImage() );

            Transform3D t3d_XY_t = new Transform3D();

            t3d_XY_t.setTranslation(
                    new Vector3f( 0, 0.0f, zBox - 2 * ( (float) slicePanel.zSlice / ( zDim - 1 ) ) * zBox ) );
            objTransXY_TG.setTransform( t3d_XY_t );
            t3d_XY_t = null;
        } else {
            return false;
        }

        if ( componentImageXZ.show( slicePanel.tSlice, slicePanel.ySlice, LUTa, LUTb, forceShow, m_kProbeTransform,
                m_kObjBoxVertsY )
                == true ) {
            texXZImageComp2D.set( componentImageXZ.getImage() );

            Transform3D t3d_XZ_t = new Transform3D();

            t3d_XZ_t.setTranslation(
                    new Vector3f( 0.0f, yBox - 2 * ( (float) slicePanel.ySlice / ( yDim - 1 ) ) * yBox, 0.0f ) );
            objTransXZ_TG.setTransform( t3d_XZ_t );
            t3d_XZ_t = null;
        } else {
            return false;
        }

        if ( componentImageZY.show( slicePanel.tSlice, slicePanel.xSlice, LUTa, LUTb, forceShow, m_kProbeTransform,
                m_kObjBoxVertsX )
                == true ) {
            texZYImageComp2D.set( componentImageZY.getImage() );

            Transform3D t3d_ZY_t = new Transform3D();

            t3d_ZY_t.setTranslation(
                    new Vector3f( -xBox + 2 * ( (float) slicePanel.xSlice / ( xDim - 1 ) ) * xBox, 0.0f, 0.0f ) );
            objTransZY_TG.setTransform( t3d_ZY_t );
            t3d_ZY_t = null;
        } else {
            return false;
        }
        return true;
    }

    /**
     *   This methods calls the componentImage's update method to redraw the screen.
     *   @param LUTa       LUT used to update imageA.
     *   @param LUTb       LUT used to update imageB.
     *   @param forceShow  Forces show to reimport image and calculate java image.
     *   @param interpMode Image interpolation method (Nearest or Smooth).
     *   @return           Confirms successful update.
     */
    public boolean updateVolume( ModelLUT LUTa, ModelLUT LUTb, boolean forceShow ) {
        volRenderOG.removeChild( volRenderOG.indexOfChild( volRenderBG ) );
        componentVolImage.show( slicePanel.tSlice, LUTa, LUTb, forceShow );
        volRenderOG.insertChild( volRenderBG, 0 );
        updateTextureVolumeRender();
        return true;
    }

    /**
     * Updates the slice textures based on the Probe position and rotation
     * angle. The ModelImage data is sampled along the diagonal, not
     * axis-aligned. bInterpolate indicates whether to use nearest-neighbors
     * or to interpolate the data.
     */
    public boolean updateProbe( boolean forceShow, boolean bInterpolate ) {
        if ( componentImageXY == null || componentImageXZ == null || componentImageZY == null ) {
            return false;
        }

        /* Set super-sample or nearest-neighbors sampling: */
        componentImageXY.Interpolate( bInterpolate );
        componentImageXZ.Interpolate( bInterpolate );
        componentImageZY.Interpolate( bInterpolate );

        if ( componentImageXY.show( slicePanel.tSlice, slicePanel.zSlice, null, null, forceShow, m_kProbeTransform,
                m_kObjBoxVertsZ )
                == true ) {
            texXYImageComp2D.set( componentImageXY.getImage() );
        }
        if ( componentImageXZ.show( slicePanel.tSlice, slicePanel.ySlice, null, null, forceShow, m_kProbeTransform,
                m_kObjBoxVertsY )
                == true ) {
            texXZImageComp2D.set( componentImageXZ.getImage() );
        }
        if ( componentImageZY.show( slicePanel.tSlice, slicePanel.xSlice, null, null, forceShow, m_kProbeTransform,
                m_kObjBoxVertsX )
                == true ) {
            texZYImageComp2D.set( componentImageZY.getImage() );
        }
        return false;
    }

    /**
     *   This methods calls the componentImage's REPAINT method
     *   to redraw the screen. The extents on this image have changed, so
     *   the extents need to be read in again and menus, panes and slide
     *   bars adjusted accordingly.
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     *	Calls various methods depending on the action.
     *	<ul>
     *	<li>Surface - opens the surface dialog</li>
     *	<li>View - opens the view control dialog</li>
     *	<li>Mouse - opens the mouse recorder dialog</li>
     *	<li>About - displays a message about this renderer</li>
     *	<li>Exit - sets variables to null and disposes of this frame</li>
     *	<li>X, Y, Z checkboxes - toggles the appropriate image planes on or off</li>
     *	</ul>
     *	@param event Event that triggered function.
     */
    public void actionPerformed( ActionEvent event ) {
        String command = event.getActionCommand();

        if ( command.equals( "MouseControl" ) ) {
            cleanMouseRecorder();
        } else if ( command.equals( "SlicesControl" ) ) {
            switchToSliceView( true );
        } else if ( command.equals( "textureRenderControl" ) ) {
            switchToVolView( true );
        } else if ( command.equals( "Capture" ) ) {
            writeImageAuto();
        } else if ( command.equals( "CaptureControl" ) ) {
            if ( captureFrame != null ) {
                disableCamera();
            }
        }
    }

    /**
     * Clean mouse recorder for any existing event items.
     */
    public void cleanMouseRecorder() {
        if ( mousePanel != null && !mousePanel.listModel.isEmpty() ) {
            if ( mousePanel.isRecording() ) {
                mousePanel.removeAllItems();
            }
        }
    }

    /**
     * Perform some actions required when switching to the slice renderer.
     * @param firstTime  perform special actions required when switching for the first time
     */
    public void switchToSliceView( boolean firstTime ) {
        switchGroup.setWhichChild( 0 );
        volumeDisplayMode3D = false;

        if ( surfacePanel != null ) {

            if ( surfacePanel.getSurfaceClipCB().isSelected() && clipPanel != null ) {
                clipPanel.removeClipSlice();
                surfacePanel.getSurfaceClipCB().setSelected( false );
            }
            surfacePanel.getSurfaceClipCB().setEnabled( false );
        }
        if ( clipPanel != null ) {
            clipPanel.disableClipPlanes();
            clipPanel.disableClipPlanesArbi();
            clipPanel.setVisible( false );
        }
        if ( volOpacityPanel != null ) {
            volOpacityPanel.setVisible( false );
        }

        if ( firstTime && mousePanel != null && !mousePanel.listModel.isEmpty() ) {
            if ( mousePanel.isRecording() ) {
                mousePanel.removeAllItems();
            }
        }
        if ( boxPanel != null ) {
            boxPanel.setEnable( false );
        }

    }

    /**
     * Perform some actions required when switching to the volume renderer.
     * @param firstTime  perform special actions required when switching for the first time
     */
    public void switchToVolView( boolean firstTime ) {
        if ( surfacePanel != null ) {
            surfacePanel.getSurfaceClipCB().setEnabled( true );
        }
        switchGroup.setWhichChild( 1 );
        volumeDisplayMode3D = true;

        if ( firstTime && mousePanel != null && !mousePanel.listModel.isEmpty() ) {
            if ( mousePanel.isRecording() ) {
                mousePanel.removeAllItems();
            }
        }

        if ( volRenderNode == null ) {
            configureVolumeFrame();
        }

        if ( clipPanel != null ) {
            if ( clipPanel.isFirstTimeBuildTree() ) {
                clipPanel.buildClipPlanesTree();
            }
        }

        if ( boxPanel != null ) {
            boxPanel.setEnable( true );
        }

    }

    /**
     * Hack. Set the update 3D texture volume win-level flag.
     * @param flag true update 3D texture volume with win-level, false not update.
     */
    public void setDisplayMode3D( boolean flag ) {
    	volumeDisplayMode3D = flag;
    }

    /**
     *   Return the current volume display mode.
     *   @return volumeDisplayMode3D   volume 3D diaplay mode.
     */
    public boolean getDisplayMode3D() {
        return volumeDisplayMode3D;
    }

    /**
     *    Sets how the image plane should be displayed depending on value of slider.
     *    @param e  Event that triggered this function.
     */
    public void stateChanged( ChangeEvent e ) {}

    /**
     * Return the X box color frame
     * @return objBoxSliceX_BG  Called be the JDialogSliceBox
     */
    public BranchGroup getObjBoxSliceX_BG() {
        return objBoxSliceX_BG;
    }

    /**
     * Return the Y box color frame
     * @return objBoxSliceY_BG  Called be the JDialogSliceBox
     */
    public BranchGroup getObjBoxSliceY_BG() {
        return objBoxSliceY_BG;
    }

    /**
     * Return the Z box color frame
     * @return objBoxSliceZ_BG  Called be the JDialogSliceBox
     */
    public BranchGroup getObjBoxSliceZ_BG() {
        return objBoxSliceZ_BG;
    }

    /**
     *   Gets the objXYPlaneBG branch group
     *   @return objXYPlaneBG   called by JDialogSliceBox
     */
    public BranchGroup getObjXYPlaneBG() {
        return objXYPlaneBG;
    }

    /**
     *   Gets the objZYPlaneBG branch group
     *   @return objZYPlaneBG   called by JDialogSliceBox
     */
    public BranchGroup getObjZYPlaneBG() {
        return objZYPlaneBG;
    }

    /**
     *   Gets the objXZPlaneBG branch group
     *   @return objXZPlaneBG   called by JDialogSliceBox
     */
    public BranchGroup getObjXZPlaneBG() {
        return objXZPlaneBG;
    }

    /* Sets the Transform3D for the slices based on the Probe Transform. To
     * allow rotation about the probe position, and not just the center of the
     * slices, the probe translation and rotation protions of the matrix are
     * separated. First the center of the slice is translated from the probe
     * position to the origin, then rotated, then translated back to the probe
     * position. The three transformations are concatenated into one
     * Transform3D, which is used to display the slices, boxes, and to sample
     * the ModelImage on the diagonal: */
    public void setProbeTG( Transform3D kTransform, boolean bTwist ) {

        /* Get the translation and inverse translation transforms: */
        Vector3f kTranslateVector = new Vector3f();

        kTransform.get( kTranslateVector );
        Transform3D kTranslate = new Transform3D();

        kTranslate.setTranslation( kTranslateVector );

        Transform3D kTranslateInv = new Transform3D();

        kTranslateInv.setTranslation( new Vector3f( -kTranslateVector.x, -kTranslateVector.y, -kTranslateVector.z ) );

        /* Get the rotation transform: */
        Matrix3f kRotationMatrix = new Matrix3f();

        kTransform.getRotationScale( kRotationMatrix );
        Transform3D kRotate = new Transform3D();

        kRotate.setRotationScale( kRotationMatrix );

        /* Concatenate the three transformations to rotate about the probe
         * position: */
        Transform3D kTransformTotal = new Transform3D();

        kTransformTotal.mul( kTranslate );
        kTransformTotal.mul( kRotate );
        kTransformTotal.mul( kTranslateInv );

        /* Set the transform for sampling the ModelImage on the diagonal: */
        if ( m_kProbeTransform == null ) {
            m_kProbeTransform = new Transform3D( kTransformTotal );
        } else if ( bTwist ) {

            m_kProbeTransform.mul( kTransformTotal, m_kProbeTransform );
        } else {
            m_kProbeTransform.set( kTransformTotal );
        }

        /* Set the transforms for displaying the slices and boxes: */
        m_kObjBoxSliceProbeX_TG.setTransform( m_kProbeTransform );
        m_kObjPlaneProbeX_TG.setTransform( m_kProbeTransform );

        m_kObjBoxSliceProbeY_TG.setTransform( m_kProbeTransform );
        m_kObjPlaneProbeY_TG.setTransform( m_kProbeTransform );

        m_kObjBoxSliceProbeZ_TG.setTransform( m_kProbeTransform );
        m_kObjPlaneProbeZ_TG.setTransform( m_kProbeTransform );

    }

    /**
     * Called from the parent class when the Probe Entry Point is selected in
     * the PlaneRender object. The input kPoint has coordinates (0,1), these
     * are scaled to fit in the probe coordinates before sending the probe
     * object.
     */
    public void drawRFAPoint( Point3f kPoint ) {

        /* Scale coordinates for the probe: */
        kPoint.x = -xBox + 2 * kPoint.x * xBox;
        kPoint.y = -( yBox - 2 * kPoint.y * yBox );
        kPoint.z = -( zBox - 2 * kPoint.z * zBox );

        /* update the probe position: */
        probePanel.getProbeBase().updatePosition( kPoint );
    }

    /**
     * Update the X, Y, Z box frame positions.
     */
    public void updateBoxSlicePos() {
        updateBoxSliceXPos();
        updateBoxSliceYPos();
        updateBoxSliceZPos();
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).setAbsPositionLabels();
            }
        }
    }

    /**
     * Get the x slider position.
     * @return int  x slice value.
     */
    public int getXPosition() {
        int xPos = 0;

        switch ( getLabel( X, false ) ) {
        case X:
            xPos = slicePanel.xSlice;
            break;

        case Y:
            xPos = slicePanel.ySlice;
            break;

        case Z:
            xPos = slicePanel.zSlice;
            break;
        }
        return xPos;
    }

    /**
     * Get the y slider position.
     * @return int y slice value.
     */
    public int getYPosition() {
        int yPos = 0;

        switch ( getLabel( Y, false ) ) {
        case X:
            yPos = slicePanel.xSlice;
            break;

        case Y:
            yPos = slicePanel.ySlice;
            break;

        case Z:
            yPos = slicePanel.zSlice;
            break;
        }
        return yPos;
    }

    /**
     * Get the y slider position.
     * @return int y slice value.
     */
    public int getZPosition() {
        int zPos = 0;

        switch ( getLabel( Z, false ) ) {
        case X:
            zPos = slicePanel.xSlice;
            break;

        case Y:
            zPos = slicePanel.ySlice;
            break;

        case Z:
            zPos = slicePanel.zSlice;
            break;
        }
        return zPos;
    }

    /**
     *   update slice x the new position when x bounding checkbox isn't selected.
     */
    public void updateBoxSliceXPos() {
        switch ( getLabel( X, false ) ) {
        case X:
            boxSliceX.setSlices( -xBox + 2 * ( (float) slicePanel.xSlice / ( xDim - 1 ) ) * xBox, yBox, zBox,
                    ViewJComponentBoxSlice.X_SLICE );
            break;

        case Y:
            boxSliceX.setSlices( xBox, yBox - 2 * ( (float) slicePanel.ySlice / ( yDim - 1 ) ) * yBox, zBox,
                    ViewJComponentBoxSlice.Y_SLICE );
            break;

        case Z:
            boxSliceX.setSlices( xBox, yBox, zBox - 2 * ( (float) slicePanel.zSlice / ( zDim - 1 ) ) * zBox,
                    ViewJComponentBoxSlice.Z_SLICE );
            break;
        }
    }

    /**
     *   update slice y the new position when y bounding checkbox isn't selected.
     */
    public void updateBoxSliceYPos() {
        switch ( getLabel( Y, false ) ) {
        case X:
            boxSliceY.setSlices( -xBox + 2 * ( (float) slicePanel.xSlice / ( xDim - 1 ) ) * xBox, yBox, zBox,
                    ViewJComponentBoxSlice.X_SLICE );
            break;

        case Y:
            boxSliceY.setSlices( xBox, yBox - 2 * ( (float) slicePanel.ySlice / ( yDim - 1 ) ) * yBox, zBox,
                    ViewJComponentBoxSlice.Y_SLICE );
            break;

        case Z:
            boxSliceY.setSlices( xBox, yBox, zBox - 2 * ( (float) slicePanel.zSlice / ( zDim - 1 ) ) * zBox,
                    ViewJComponentBoxSlice.Z_SLICE );
            break;
        }
    }

    /**
     *   update slice z the new position when x bounding checkbox isn't selected.
     */
    public void updateBoxSliceZPos() {
        switch ( getLabel( Z, false ) ) {
        case X:
            boxSliceZ.setSlices( -xBox + 2 * ( (float) slicePanel.xSlice / ( xDim - 1 ) ) * xBox, yBox, zBox,
                    ViewJComponentBoxSlice.X_SLICE );
            break;

        case Y:
            boxSliceZ.setSlices( xBox, yBox - 2 * ( (float) slicePanel.ySlice / ( yDim - 1 ) ) * yBox, zBox,
                    ViewJComponentBoxSlice.Y_SLICE );
            break;

        case Z:
            boxSliceZ.setSlices( xBox, yBox, zBox - 2 * ( (float) slicePanel.zSlice / ( zDim - 1 ) ) * zBox,
                    ViewJComponentBoxSlice.Z_SLICE );
            break;
        }
    }

    /**
     *   Sets new frame around slice x based on the new position.
     */
    public void updateBoxSliceX() {
        removeBoxSliceX();
        addBoxSliceX();
    }

    /**
     *   Sets new frame around slice y based on the new position.
     */
    public void updateBoxSliceY() {
        removeBoxSliceY();
        addBoxSliceY();
    }

    /**
     *   Sets new frame around slice z based on the new position.
     */
    public void updateBoxSliceZ() {
        removeBoxSliceZ();
        addBoxSliceZ();
    }

    /**
     *   Detaches the slice frame on slice x.
     */
    public void removeBoxSliceX() {
        objBoxSliceX_BG.detach();
    }

    /**
     *   Detaches the slice frame on slice y.
     */
    public void removeBoxSliceY() {
        objBoxSliceY_BG.detach();
    }

    /**
     *   Detaches the slice frame on slice z.
     */
    public void removeBoxSliceZ() {
        objBoxSliceZ_BG.detach();
    }

    /**
     * Return mousePanel from parent frame
     * @return mousePanel   Mouse Dialog box.
     */
    public JPanelMouse getMouseDialog() {
        return mousePanel;
    }

    /**
     * Return surfacePanel from parent frame
     * @return surfacePanel   Surface Dialog box.
     */
    public JPanelSurface getSurfaceDialog() {
        return surfacePanel;
    }

    /**
     * Return probePanel from parent frame
     * @return probePanel   Probe Dialog box.
     */
    public JPanelProbe getProbeDialog() {
        return probePanel;
    }

    /**
     * Return the viewPanel from parent frame
     * @return viewPanel  View Dialog box.
     */
    public JPanelView getViewDialog() {
        return viewPanel;
    }

    /**
     * Return the diaplay dialog
     * @return boxPanel   Display dialog.
     */
    public JPanelDisplay getDisplayDialog() {
        return boxPanel;
    }

    /**
     * Return clipPanel from parent frame
     * @return clipPanel   Clip Dialog box.
     */
    public JPanelClip getClipDialog() {
        return clipPanel;
    }

    /**
     * Enable the six clipping planes at a time.
     */
    public void invokeClipping() {
        clipPanel.invokeClippingPlanes();
    }

    /**
     * Diable all the six clipping planes at a time.
     */
    public void diableClipping() {
       clipPanel.disable6Planes();
    }

    /**
     * Crop the region of the clipped volume.
     */
    public void cropClipVolume() {
        clipPanel.cropVolume();
    }

    /**
     * Undo crop region with the clipped volume.
     */
    public void undoCropVolume() {
        clipPanel.undoCrop();
    }

    /**
    * Save crop region with the clipped volume.
    */
    public void saveCropVolume() {
       clipPanel.saveCropImage();
    }


    /**
     * Return volume opacity control Dialog from parent frame
     * @return volOpacityPanel   volume opacity dialog box.
     */
    public JPanelVolOpacityBase getVolOpacityPanel() {
        return volOpacityPanel;
    }

    /**
     *   Gets the mouse pointer mode - standard or fly - from the
     *   view dialog.
     *   @return The mouse pointer mode.
     */
    public int getMouseMode() {
        if ( viewPanel == null ) {
            return JPanelView.STD_MODE;
        }
        return viewPanel.getMouseMode();
    }

    /**
     *   Sets the mouse pointer mode - standard or fly - in the
     *   view dialog.
     *   @param mode Mode to set to.
     */
    public void setMouseMode( int mode ) {
        viewPanel.setMouseMode( mode );
    }

    /**
     *   Makes the box frame visible.
     */
    public void showBoxFrame() {
        sceneRootTG.addChild( objBoxFrameBG );
    }

    /**
     *   Makes the box frame invisible.
     */
    public void hideBoxFrame() {
        objBoxFrameBG.detach();
    }

    /**
     *   Adds the slice frame for slice x back into the scene graph.
     *   It is necessary to make a new branch group and transform group
     *   because otherwise there is a RestrictedAccessException.
     */
    public void addBoxSliceX() {
        objBoxSliceX_BG = new BranchGroup();
        objBoxSliceX_BG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        objBoxSliceX_BG.setCapability( Group.ALLOW_CHILDREN_READ );
        objBoxSliceX_BG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        objBoxSliceX_BG.setCapability( BranchGroup.ALLOW_DETACH );
        objBoxSliceX_BG.setPickable( false );

        TransformGroup temp = new TransformGroup();

        temp.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        temp.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        temp.setCapability( Group.ALLOW_CHILDREN_READ );
        temp.setCapability( Group.ALLOW_CHILDREN_WRITE );
        objBoxSliceX_BG.addChild( temp );

        updateBoxSliceXPos();

        Shape3D shape = new Shape3D( boxSliceX, null );

        shape.setCapability( Shape3D.ALLOW_APPEARANCE_READ );
        shape.setCapability( Shape3D.ALLOW_APPEARANCE_WRITE );
        temp.addChild( shape );
        triPlanarViewBG.addChild( objBoxSliceX_BG );
    }

    /**
     *   Adds the slice frame for slice y back into the scene graph.
     *   It is necessary to make a new branch group and transform group
     *   because otherwise there is a RestrictedAccessException.
     */
    public void addBoxSliceY() {
        // I have to make a new one because otherwise I get a RestrictedAccessException
        objBoxSliceY_BG = new BranchGroup();
        objBoxSliceY_BG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        objBoxSliceY_BG.setCapability( Group.ALLOW_CHILDREN_READ );
        objBoxSliceY_BG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        objBoxSliceY_BG.setCapability( BranchGroup.ALLOW_DETACH );
        objBoxSliceY_BG.setPickable( false );

        TransformGroup temp = new TransformGroup();

        temp.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        temp.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        temp.setCapability( Group.ALLOW_CHILDREN_READ );
        temp.setCapability( Group.ALLOW_CHILDREN_WRITE );
        objBoxSliceY_BG.addChild( temp );

        updateBoxSliceYPos();

        Shape3D shape = new Shape3D( boxSliceY, null );

        shape.setCapability( Shape3D.ALLOW_APPEARANCE_READ );
        shape.setCapability( Shape3D.ALLOW_APPEARANCE_WRITE );
        shape.setCapability( Shape3D.ENABLE_PICK_REPORTING );
        temp.addChild( shape );
        triPlanarViewBG.addChild( objBoxSliceY_BG );
    }

    /**
     *   Adds the slice frame for slice z back into the scene graph.
     *   It is necessary to make a new branch group and transform group
     *   because otherwise there is a RestrictedAccessException.
     */
    public void addBoxSliceZ() {
        objBoxSliceZ_BG = new BranchGroup();
        objBoxSliceZ_BG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        objBoxSliceZ_BG.setCapability( Group.ALLOW_CHILDREN_READ );
        objBoxSliceZ_BG.setCapability( Group.ALLOW_CHILDREN_WRITE );
        objBoxSliceZ_BG.setCapability( BranchGroup.ALLOW_DETACH );
        objBoxSliceZ_BG.setPickable( false );

        TransformGroup temp = new TransformGroup();

        temp.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        temp.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        temp.setCapability( Group.ALLOW_CHILDREN_READ );
        temp.setCapability( Group.ALLOW_CHILDREN_WRITE );
        temp.setCapability( TransformGroup.ENABLE_PICK_REPORTING );
        objBoxSliceZ_BG.addChild( temp );

        updateBoxSliceZPos();

        Shape3D shape = new Shape3D( boxSliceZ, null );

        shape.setCapability( Shape3D.ALLOW_APPEARANCE_READ );
        shape.setCapability( Shape3D.ALLOW_APPEARANCE_WRITE );
        shape.setCapability( Shape3D.ENABLE_PICK_REPORTING );
        temp.addChild( shape );
        triPlanarViewBG.addChild( objBoxSliceZ_BG );
    }

    /**
     *   Makes the box slice X visible.
     */
    public void showBoxSliceX() {
        if ( slicePanel.xVisible == true ) {
            if ( xChanged == true ) {
                updateBoxSliceX();
                xChanged = false;
            } else {
                if ( !objBoxSliceX_BG.isLive() ) {
                    triPlanarViewBG.addChild( objBoxSliceX_BG );
                }
            }
        }
    }

    /**
     *   Makes the box slice Y visible.
     */
    public void showBoxSliceY() {
        if ( slicePanel.yVisible == true ) {
            if ( yChanged == true ) {
                updateBoxSliceY();
                yChanged = false;
            } else {
                if ( !objBoxSliceY_BG.isLive() ) {
                    triPlanarViewBG.addChild( objBoxSliceY_BG );
                }
            }
        }
    }

    /**
     *   Makes the box slice Z visible.
     */
    public void showBoxSliceZ() {
        if ( slicePanel.zVisible == true ) {
            if ( zChanged == true ) {
                updateBoxSliceZ();
                zChanged = false;
            } else {
                if ( !objBoxSliceZ_BG.isLive() ) {
                    triPlanarViewBG.addChild( objBoxSliceZ_BG );
                }
            }
        }
    }

    /* Called when the underlying data has changed, due to sculpting. Calls
     * the ViewJFrameVolumeView updateSliceData function to update the data
     * in the slices. */
    public void updateData() {
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).updateSliceData();
            }
        }
    }

    /**
     *   Sets the color of the x slice frame.
     *   @param color    Color to set to.
     */
    public void setSliceXColor( Color color ) {
        boxSliceX.setColor( color );
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).setXSliceHairColor( color );
            }
        }
    }

    /**
     *   Sets the color of the y slice frame.
     *   @param color    Color to set to.
     */
    public void setSliceYColor( Color color ) {
        boxSliceY.setColor( color );
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).setYSliceHairColor( color );
            }
        }
    }

    /**
     *   Sets the color of the z slice frame.
     *   @param color    Color to set to.
     */
    public void setSliceZColor( Color color ) {
        boxSliceZ.setColor( color );
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).setZSliceHairColor( color );
            }
        }
    }

    /* Returns the current color of the X Slice.
     * @return Color3f, XSlice color
     */
    public Color3f getXSliceColor() {
        return boxSliceX.getColor();
    }

    /* Returns the current color of the Y Slice.
     * @return Color3f, YSlice color
     */
    public Color3f getYSliceColor() {
        return boxSliceY.getColor();
    }

    /* Returns the current color of the Z Slice.
     * @return Color3f, ZSlice color
     */
    public Color3f getZSliceColor() {
        return boxSliceZ.getColor();
    }

    /* Updates the ViewJFrameVolumeView slider positions. Sliders are matched
     * based on the slider color -- which must match the zColor of the
     * PlaneRender that it maps to, passed in as kColor: */
    public void updateTriPlanar( float fValue, Color kColor ) {
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).setCoord( fValue, kColor );
            }
        }
    }

    /**
     * Switch between the volume texture mode and the regular volume texture mode
     * @param flag  texture aligned view enabled or not
     */
    public void setViewTextureAligned( boolean flag ) {
        isViewTextureAligned = flag;
        reConfigureVolumeFrame();
    }

    /**
     * Enable perspective projection rendering; otherwise use orthographic
     * projection.
     * @param bEnable true to enable perspective projection
     */
    public void setRenderPerspective( boolean bEnable ) {
        // The projection policy is stored in the view.
        // Note that the window resize policy is PHYSICAL_WORLD.

        View kView = universe.getViewer().getView();

        if ( bEnable ) {
            kView.setProjectionPolicy( View.PERSPECTIVE_PROJECTION );
            kView.setScreenScalePolicy( View.SCALE_SCREEN_SIZE );
        } else {
            // The View's screen scale is always updated based
            // on the difference in z-coordinates of the eyepoint
            // and the center of the bounding sphere around the volume.
            setupEye();
            updateViewScreenScale( parallelScaleT3D );
            kView.setProjectionPolicy( View.PARALLEL_PROJECTION );
            kView.setScreenScalePolicy( View.SCALE_EXPLICIT );
        }
        if ( clipPanel == null || ( clipPanel != null && !clipPanel.isClipArbiPicked() ) ) {
            updateCubicTransform( transformNode3d );
        }
    }

    /**
     * Build a image with the current rotational transformation matrix.
     * Step.1 convert the java3D transform matrix into a quaternion component.
     * Step.2 convert the quaternion into image( our world ) coordinate system.
     * Step.3 convert the quaternion into a rotatin matrix.
     *        Quaternion q ( w, x, y, z ): rotation of w around the vector ( x, y, z );
     *        Convert the quaternion into a rotation matrix.
     *        /                                                     \
     *        |    1-2*y^2-2*z^2     2*x*y-2*w*z      2*x*z+2*w*y   |
     *        |     2*xy+2*w*z       1-2*x^2-2*z^2    2*y*z-2*w*x   |
     *        |     2*x*z-2*w*y      2*y*z+2*w*x      1-2*x^2-2*y^2 |
     *        \                                                     /
     * Step.4 Calling the transform algorithm to extract the image.
     */
    public void updateImageFromRotation() {
        int interp = 0;
        double w, x, y, z;
        double[][] result = new double[4][4];

        Matrix4d mtx = new Matrix4d();
        Quat4d quat = new Quat4d();

        // Step.1
        currentTransform.get( mtx );
        mtx.get( quat );
        // Step.2
        w = quat.w;
        x = quat.x;
        y = -quat.y;
        z = -quat.z;
        // Step.3
        TransMatrix transMtx = new TransMatrix( 4 );

        transMtx.set( 0, 0, 1 - 2 * ( y * y ) - 2 * ( z * z ) );
        transMtx.set( 0, 1, 2 * ( x * y - w * z ) );
        transMtx.set( 0, 2, 2 * ( x * z + w * y ) );
        transMtx.set( 0, 3, 0 );
        transMtx.set( 1, 0, 2 * ( x * y + w * z ) );
        transMtx.set( 1, 1, 1 - 2 * ( x * x ) - 2 * ( z * z ) );
        transMtx.set( 1, 2, 2 * ( y * z - w * x ) );
        transMtx.set( 1, 3, 0 );
        transMtx.set( 2, 0, 2 * ( x * z - w * y ) );
        transMtx.set( 2, 1, 2 * ( y * z + w * x ) );
        transMtx.set( 2, 2, 1 - 2 * ( x * x ) - 2 * ( y * y ) );
        transMtx.set( 2, 3, 1 );
        transMtx.set( 3, 0, 0 );
        transMtx.set( 3, 1, 0 );
        transMtx.set( 3, 2, 0 );
        transMtx.set( 3, 3, 1 );

        TransMatrix xfrm = new TransMatrix( 4 );

        xfrm.identity();
        Point3Df center = imageA.getImageCentermm();

        xfrm.setTranslate( center.x, center.y, center.z );
        xfrm.multMatrix( xfrm.getMatrix(), transMtx.getMatrix(), result );
        xfrm.setMatrix( result );
        xfrm.setTranslate( -center.x, -center.y, -center.z );
        // Step.4
        AlgorithmTransform algoTrans = new AlgorithmTransform( imageA, xfrm, interp, resols[0], resols[1], resols[2],
                xDim, yDim, zDim, false, false, false );

        algoTrans.setUpdateOriginFlag( false );
        algoTrans.setProgressBarVisible( false );
        algoTrans.run();

        ModelImage resultImage1 = algoTrans.getTransformedImage();

        if ( ( algoTrans.isCompleted() == true ) && ( resultImage1 != null ) ) {
            resultImage1.calcMinMax();

            // The algorithm has completed and produced a new image to be displayed.
            try {
                new ViewJFrameImage( resultImage1, null, new Dimension( 610, 200 ) );
            } catch ( OutOfMemoryError error ) {
                MipavUtil.displayError( "Out of memory: unable to open new frame" );
            }
        }
    }

    /**
     *	Tells the mouse dialog that the transform has changed.  Does
     *	not use the type parameter.
     *	@param type			Type of transform.
     *	@param transform	Transform that was changed to.
     */
    public void transformChanged( int type, Transform3D transform ) {
        mousePanel.transformChanged( type, transform );
        currentTransformType = type;
        currentTransform = transform;
        if ( MouseBehaviorCallback.ZOOM == type ) {
            updateViewScreenScale( transform );
        }
        if ( MouseBehaviorCallback.ROTATE == type ) {
            if ( clipPanel != null ) {
                clipPanel.displaySClipPlanePts();
            }
            updateTextureVolumeRender();
            if ( clipPanel == null || ( clipPanel != null && !clipPanel.isClipArbiPicked() ) ) {
                updateCubicTransform( transform );
            }
        }
    }

    /**
     * The raybased renderer invokes this method when a mouse release event occurs.
     *	@param type			Type of transform.
     *	@param transform	Transform that was changed to.
     */
    public void transformUpdate( int type, Transform3D transform ) {
        if ( MouseBehaviorCallback.ROTATE == type && parallelRotation && transform != null ) {
            currentTransformType = type;
            currentTransform.set( transform );
            currentTransform.setScale( 0.75 );
            sceneRootTG.setTransform( currentTransform );
            updateTextureVolumeRender();
            if ( clipPanel == null || ( clipPanel != null && !clipPanel.isClipArbiPicked() ) ) {
                updateCubicTransform( currentTransform );
            }
        }
    }

    /**
     * Set the parallel rotation flag from the viewJFrameVolumeView.
     * @param flag  <code>true</code> set all the renderer to parallel rotation,
     *              <code>false</code> parallel rotation mode off.
     */
    public void setParallelRotation( boolean flag ) {
        parallelRotation = flag;
    }

    /**
     * Update the raycast based renderer.  JPanelSurface mouse release event invokes this
     * method call.
     */
    public void updateRaycastRender() {
        if ( rayBasedRender != null ) {
            rayBasedRender.transformUpdate( currentTransformType, currentTransform );
        }
    }

    /**
     *   Update the transform3D for the cubic.
     *   @param transform   Transform3D
     */
    public void updateCubicTransform( Transform3D transform ) {
        View kView = universe.getViewer().getView();
        Matrix4f matrix = new Matrix4f();
        AxisAngle4f axis = new AxisAngle4f();

        transformNode3d = transform;
        transform.get( matrix );
        axis.set( matrix );
        transformCubic.setRotation( axis );
        transformCubic.setScale( 0.1f );
        if ( kView.getProjectionPolicy() == View.PERSPECTIVE_PROJECTION ) {
            transformCubic.setTranslation( new Vector3f( -0.7f, -0.7f, -0.7f ) );
        } else {
            transformCubic.setTranslation( new Vector3f( -0.8f, -0.8f, -0.8f ) );
        }
        cubicTG.setTransform( transformCubic );
    }

    /**
     *	Tells the mouse dialog that the transform has changed.  Does
     *	not use the type parameter.
     *	@param type			Type of transform.
     *	@param transform	Transform that was changed to.
     */
    public void updateTransform( Transform3D transform ) {
        updateTextureVolumeRender();

    }

    /**
     *  This function calculates the scale factor for zooming in parallel
     *  projection.   The scenario is to calculate the distance between
     *  the origin boxframe center and tranformed boxframe center.   This
     *  distance is used to compute the screen scale factor.
     *  @param kTransform     The tranformation matrix from tranformChanged().
     */
    private void updateViewScreenScale( Transform3D kTransform ) {
        parallelScaleT3D = kTransform;
        // The boxframe center distance is acutally the bounding sphere
        // center distance.
        Shape3D shape;

        shape = new Shape3D( boxFrame, null );
        BoundingSphere kBounds = new BoundingSphere( shape.getBounds() );

        kBounds.transform( kBounds, kTransform );

        Point3d kVolumeCenterPoint = new Point3d();

        kBounds.getCenter( kVolumeCenterPoint );
        Vector3d kViewVector = new Vector3d();

        kViewVector.sub( myEyePoint, kVolumeCenterPoint );
        double dDist = Math.abs( kViewVector.z );

        View kView = universe.getViewer().getView();
        double dFieldOfView = kView.getFieldOfView();
        double dViewWidth = 15.0f * dDist * Math.tan( dFieldOfView / 15.0f );

        Screen3D kScreen = canvas.getScreen3D();

        kView.setScreenScale( kScreen.getPhysicalScreenWidth() / dViewWidth );

        /* Calculate and store the original screenscale: */
        dViewWidth = 15.0f * Math.tan( dFieldOfView / 15.0f );
        m_dOriginalScreenScale = kScreen.getPhysicalScreenWidth() / dViewWidth;
    }

    /**
     *   Set up the center point of the red line boxframe.
     */
    private void setupEye() {
        Shape3D shape;

        shape = new Shape3D( boxFrame, null );
        BoundingSphere kBounds = new BoundingSphere( shape.getBounds() );

        Point3d kVolumeCenterPoint = new Point3d();

        kBounds.getCenter( kVolumeCenterPoint );

        myEyePoint.x = kVolumeCenterPoint.x;
        myEyePoint.y = kVolumeCenterPoint.y;
        myEyePoint.z = kVolumeCenterPoint.z + kBounds.getRadius();
    }

    /**
     *	Updates the 3 orthogonal 2D texture maps' opacities.
     *   @param opX the opacity of the x plane, range [0,100] and -1 if no updating is required
     *   @param opX the opacity of the y plane, range [0,100] and -1 if no updating is required
     *   @param opX the opacity of the z plane, range [0,100] and -1 if no updating is required
     */
    public void updateOpacityOfOthrogPlanes( int opX, int opY, int opZ ) {
        if ( opX != -1 ) {
            if ( ( 1 - opX / 100.0f ) == 0 ) {
                taZY.setTransparencyMode( TransparencyAttributes.NONE );
            } else {
                taZY.setTransparencyMode( TransparencyAttributes.BLENDED );
            }
            taZY.setSrcBlendFunction( TransparencyAttributes.BLEND_SRC_ALPHA );
            taZY.setDstBlendFunction( TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA );
            taZY.setTransparency( 1 - opX / 100.0f );
        } else if ( opY != -1 ) {
            if ( ( 1 - opY / 100.0f ) == 0 ) {
                taXZ.setTransparencyMode( TransparencyAttributes.NONE );
            } else {
                taXZ.setTransparencyMode( TransparencyAttributes.BLENDED );
            }
            taXZ.setSrcBlendFunction( TransparencyAttributes.BLEND_SRC_ALPHA );
            taXZ.setDstBlendFunction( TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA );
            taXZ.setTransparency( 1 - opY / 100.0f );
        } else if ( opZ != -1 ) {
            if ( ( 1 - opZ / 100.0f ) == 0 ) {
                taXY.setTransparencyMode( TransparencyAttributes.NONE );
            } else {
                taXY.setTransparencyMode( TransparencyAttributes.BLENDED );
            }
            taXY.setSrcBlendFunction( TransparencyAttributes.BLEND_SRC_ALPHA );
            taXY.setDstBlendFunction( TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA );
            taXY.setTransparency( 1 - opZ / 100.0f );
        }
    }

    /**
     * Return the parent frame.
     * @return ViewJFrameVolumeView  parent frame
     */
    public ViewJFrameVolumeView getParentFrame() {
        return (ViewJFrameVolumeView) parent;
    }

    /**
     * Get the rotation control dialog box
     * @return rotationControlDialog   rotation control dialog
     */
    public JPanelCamera getCameraControl() {
        return rotationControlPanel;
    }

    /**
     * Get the geodesic control panel.
     * @return JPanelGeodesic  geodesic control panel.
     */
    public JPanelGeodesic getGeodesicPanel() {
        return geodesicPanel;
    }

    /**
     * Return the sculptor panel.
     * @return JPanelSculptor   sculptor control panel.
     */
    public JPanelSculptor getSculptorPanel() {
        return sculptorPanel;
    }

    /**
     *  return the current active slicePanel interface
     *  @return slicePanel    called by JDialogMouseRecorder
     */
    public JPanelSlices getSlicePanel() {
        return slicePanel;
    }

    /**
     *  Detach volume render branch group.
     */
    public void detachVolRender() {
        if ( volRenderBG.isLive() ) {
            volRenderOG.removeChild( volRenderOG.indexOfChild( volRenderBG ) );
        }
        volumeDisplayMode3D = false;
    }

    /* Return texture, the VolumeTexture member variable
     * @return texture
     */
    public VolumeTexture getVolumeTexture() {
        return texture;
    }

    /**
     *   Return volume render Order Group.
     *   @return volRenderOG   volume render order group.
     */
    public OrderedGroup getVolRenderOG() {
        return volRenderOG;
    }

    /**
     * Returns the left of right eye copy of the NodeVolumeTextureRender:
     * @param iView: determines which view to return, 0 for left and 1 for right:
     * @return the NodeVolumeTextureRender for displaying the volume texture
     * in the stereo window.
     */
    public Group getVolumeTextureCopy( int iView )
    {
        NodeVolumeTextureRender kReturn =
            new NodeVolumeTextureRender( imageA, texture,
                                         volRenderNode.getAxisSlice(),
                                         volRenderNode.getIncreasingOrder() );
        return (Group)kReturn;
    }


    /**
     *   Return volume render Branch Group.
     *   @return volRenderBG   volume render branch group.
     */
    public BranchGroup getVolRenderBG() {
        return volRenderBG;
    }

    /**
     * Retrieve the current "fine" spacing between slices.
     * @return float Current "fine" spacing between slices.
     */
    public float getSliceSpacingFine() {
        if ( volRenderNode != null && ( volRenderNode instanceof NodeAlignedVolumeTextureRender ) ) {
            return ( (NodeAlignedVolumeTextureRender) volRenderNode ).getSliceSpacingFine();
        }
        return 1.0f;
    }

    /**
     * Retrieve the current "coarse" spacing between slices.
     * @return float Current "coarse" spacing between slices.
     */
    public float getSliceSpacingCoarse() {
        if ( volRenderNode != null && ( volRenderNode instanceof NodeAlignedVolumeTextureRender ) ) {
            return ( (NodeAlignedVolumeTextureRender) volRenderNode ).getSliceSpacingCoarse();
        }
        return 1.50f;
    }

    /**
     * Set the desired "fine" spacing between slices.
     * @param dSpacing float Desired "fine" spacing between slices.
     */
    public void setSliceSpacingFine( float fSpacing ) {
        if ( volRenderNode != null && ( volRenderNode instanceof NodeAlignedVolumeTextureRender ) ) {
            ( (NodeAlignedVolumeTextureRender) volRenderNode ).setSliceSpacingFine( fSpacing );
        }
    }

    /**
     * Set the desired "coarse" spacing between slices.
     * @param dSpacing float Desired "coarse" spacing between slices.
     */
    public void setSliceSpacingCoarse( float fSpacing ) {
        if ( volRenderNode != null && ( volRenderNode instanceof NodeAlignedVolumeTextureRender ) ) {
            ( (NodeAlignedVolumeTextureRender) volRenderNode ).setSliceSpacingCoarse( fSpacing );
        }
    }

    /**
     * Select the "coarse" slice sampling for rendering.
     */
    public void useSliceSpacingCoarse() {
        if ( volRenderNode != null && ( volRenderNode instanceof NodeAlignedVolumeTextureRender ) ) {
            ( (NodeAlignedVolumeTextureRender) volRenderNode ).useSliceSpacingCoarse();
        }
    }

    /**
     * Retrieves whether the "fine" slice sampling is currently selected.
     * @return boolean Returns true if "fine" slice sampling is currently
     * selected.
     */
    public void useSliceSpacingFine() {
        if ( volRenderNode != null && ( volRenderNode instanceof NodeAlignedVolumeTextureRender ) ) {
            ( (NodeAlignedVolumeTextureRender) volRenderNode ).useSliceSpacingFine();
        }
    }

    /**
     *   Closes the image
     */
    public void close() {
        disposeLocal( true );
    }

    /**
     *  Calls disposeLocal
     */
    protected void finalize()
        throws Throwable {
        this.disposeLocal( false );
        super.finalize();
    }

    /**
     *  Sets all variables to null, disposes, and garbage collects
     */
    public void disposeLocal( boolean flag ) {
        parent = null;
        if ( volRenderNode != null && volRenderNode.isLive() ) {
            if ( volBG.isLive() ) {
                volBG.detach();
            }
            volRenderNode.dispose();
            volRenderNode = null;
        }
        if ( componentImageXY != null ) {
            componentImageXY.disposeLocal();
            componentImageXY = null;
        }
        if ( componentImageZY != null ) {
            componentImageZY.disposeLocal();
            componentImageZY = null;
        }
        if ( componentImageXZ != null ) {
            componentImageXZ.disposeLocal();
            componentImageXZ = null;
        }
        if ( componentVolImage != null ) {
            componentVolImage.disposeLocal();
            componentVolImage = null;
        }

        imageVolBufferA = null;
        imageVolBufferB = null;

        if ( texture != null ) {
            texture.disposeLocal();
            texture = null;
        }
        if ( volOpacityPanel != null ) {
            volOpacityPanel.disposeLocal();
            volOpacityPanel = null;
        }
        if ( universe != null ) {
            universe.removeAllLocales();
            universe = null;
        }

        volOpacityPanel = null;
        imageBufferA_XY = null;
        imageBufferA_XZ = null;
        imageBufferA_ZY = null;
        imageBufferB_XY = null;
        imageBufferB_ZY = null;
        imageBufferB_XZ = null;
        imageBufferB_XZ = null;

        taXY = null;
        taXZ = null;
        taZY = null;
        imageA = null;
        imageB = null;

        if ( boxPanel != null ) {
            boxPanel.dispose();
            boxPanel = null;
        }
        if ( slicePanel != null ) {
            slicePanel.dispose();
            slicePanel = null;
        }
        if ( mousePanel != null ) {
            mousePanel.dispose();
            mousePanel = null;
        }
        if ( surfacePanel != null ) {
            surfacePanel.dispose();
            surfacePanel = null;
        }
        if ( clipPanel != null ) {
            clipPanel.dispose();
            clipPanel = null;
        }
        if ( probePanel != null ) {
            probePanel.dispose();
            probePanel = null;
        }

        if ( rotationControlPanel != null ) {
            rotationControlPanel.disposeLocal( false );
            rotationControlPanel = null;
        }

        m_kSoftwareLightSet = null;
        m_kSoftwareMaterial = null;

        if ( geodesicPanel != null ) {
            geodesicPanel.disposeLocal( false );
            geodesicPanel = null;
        }

        if ( sculptorPanel != null ) {
            sculptorPanel.disposeLocal( false );
            sculptorPanel = null;
        }

        if ( flag == true ) {
            super.disposeLocal();
        }
    }

    /**
     *  Gets the current scene state, in terms of what numbers the slices are
     *  on and whether or not they are visible.
     *  @return A SceneState object with the variables set appropriately.
     */
    public Object getSceneState() {

        int comp = 0;
        TransferFunction opacTransFunct = null;
        boolean isVolOpacityChanged = false;

        if ( volOpacityPanel != null && !imageA.isColorImage() ) {
            comp = volOpacityPanel.getTabbedPane().getSelectedIndex();

            if ( volOpacityPanel.getSelectedComponent( comp ).getOpacityTransferFunction().getFunction() != null ) {
                opacTransFunct = volOpacityPanel.getSelectedComponent( comp ).getOpacityTransferFunction();
            }
        }
        return new SceneState( getSlicePanel().getXSlice(), getSlicePanel().getYSlice(), getSlicePanel().getZSlice(),
                getSlicePanel().getXOpacitySlice(), getSlicePanel().getYOpacitySlice(),
                getSlicePanel().getZOpacitySlice(), getSlicePanel().getXVisible(), getSlicePanel().getYVisible(),
                getSlicePanel().getZVisible(), getSurfaceDialog().getSurfaceOpacity(), getClipDialog().getSliceA(),
                getClipDialog().getSliceX(), getClipDialog().getSliceY(), getClipDialog().getSliceZ(),
                getClipDialog().getSliceXInv(), getClipDialog().getSliceYInv(), getClipDialog().getSliceZInv(),
                getClipDialog().getAVisible(), getClipDialog().getXVisible(), getClipDialog().getYVisible(),
                getClipDialog().getZVisible(), getClipDialog().getXVisibleInv(), getClipDialog().getYVisibleInv(),
                getClipDialog().getZVisibleInv(), comp, opacTransFunct, isVolOpacityChanged, getDisplayMode3D(),
                getClipDialog().is6PlaneClipping(), getClipDialog().getAxisX(), getClipDialog().getAxisY(),
                getClipDialog().getAxisZ(), getClipDialog().getAxisAngle(), getClipDialog().isClipArbiPicked() );
    }

    /**
     *  Sets the GUI components to their proper state before the action is dispatched
     *  from the player.
     *  @param scene    The state of the scene.
     */
    public void setGUI( Object scene ) {
        if ( ( (SceneState) scene ).isVolumeDisplayMode3D ) {
            slicePanel.disableSlices();
            if ( ( (SceneState) scene ).is6PlaneClipping ) {

                if ( ( (SceneState) scene ).xClipVisible || ( (SceneState) scene ).yClipVisible
                        || ( (SceneState) scene ).zClipVisible || ( (SceneState) scene ).xNegClipVisible
                        || ( (SceneState) scene ).yNegClipVisible || ( (SceneState) scene ).zNegClipVisible ) {
                    getClipDialog().disableClipA();
                }

                if ( first6ClipPlane ) {
                    getClipDialog().swapModelClipBG( true );
                    first6ClipPlane = false;
                    firstClipArbi = true;
                }
                getClipDialog().setCheckBoxX( ( (SceneState) scene ).xClipVisible );
                getClipDialog().setXSliderEnabled( ( (SceneState) scene ).xClipVisible );
                if ( ( (SceneState) scene ).xClipVisible ) {
                    getClipDialog().getSliderX().setValue( ( (SceneState) scene ).clipSliceX );
                } else {
                    getClipDialog().initClipSliceX();
                }

                getClipDialog().setCheckBoxY( ( (SceneState) scene ).yClipVisible );
                getClipDialog().setYSliderEnabled( ( (SceneState) scene ).yClipVisible );
                if ( ( (SceneState) scene ).yClipVisible ) {
                    getClipDialog().getSliderY().setValue( ( (SceneState) scene ).clipSliceY );
                } else {
                    getClipDialog().initClipSliceY();
                }

                getClipDialog().setCheckBoxZ( ( (SceneState) scene ).zClipVisible );
                getClipDialog().setZSliderEnabled( ( (SceneState) scene ).zClipVisible );
                if ( ( (SceneState) scene ).zClipVisible ) {
                    getClipDialog().getSliderZ().setValue( ( (SceneState) scene ).clipSliceZ );
                } else {
                    getClipDialog().initClipSliceZ();
                }

                getClipDialog().setCheckBoxXInv( ( (SceneState) scene ).xNegClipVisible );
                getClipDialog().setXSliderInvEnabled( ( (SceneState) scene ).xNegClipVisible );
                if ( ( (SceneState) scene ).xNegClipVisible ) {
                    getClipDialog().getSliderXInv().setValue( ( (SceneState) scene ).clipSliceXNeg );
                } else {
                    getClipDialog().initClipSliceXInv();
                }

                getClipDialog().setCheckBoxYInv( ( (SceneState) scene ).yNegClipVisible );
                getClipDialog().setYSliderInvEnabled( ( (SceneState) scene ).yNegClipVisible );
                if ( ( (SceneState) scene ).yNegClipVisible ) {
                    getClipDialog().getSliderYInv().setValue( ( (SceneState) scene ).clipSliceYNeg );
                } else {
                    getClipDialog().initClipSliceYInv();
                }

                getClipDialog().setCheckBoxZInv( ( (SceneState) scene ).zNegClipVisible );
                getClipDialog().setZSliderInvEnabled( ( (SceneState) scene ).zNegClipVisible );
                if ( ( (SceneState) scene ).zNegClipVisible ) {
                    getClipDialog().getSliderZInv().setValue( ( (SceneState) scene ).clipSliceZNeg );
                } else {
                    getClipDialog().initClipSliceZInv();
                }
            } else { // arbitrary clipping

                if ( ( (SceneState) scene ).aClipVisible ) {
                    getClipDialog().disable6Planes();
                }

                if ( firstClipArbi ) {
                    getClipDialog().swapModelClipBG( false );
                    firstClipArbi = false;
                    first6ClipPlane = true;
                }
                getClipDialog().setCheckBoxA( ( (SceneState) scene ).aClipVisible );
                getClipDialog().setASliderEnabled( ( (SceneState) scene ).aClipVisible );
                if ( ( (SceneState) scene ).aClipVisible ) {
                    if ( ( (SceneState) scene ).isClipArbiPicked ) {
                        getClipDialog().setArbiPlanePickable( true );
                        getClipDialog().setClipSliceAwithRotate( ( (SceneState) scene ).axisX,
                                ( (SceneState) scene ).axisY, ( (SceneState) scene ).axisZ,
                                ( (SceneState) scene ).axisAngle );
                    } else {
                        getClipDialog().setArbiPlanePickable( false );
                    }
                    getClipDialog().getSliderA().setValue( ( (SceneState) scene ).clipSliceA );
                } else {
                    getClipDialog().setArbiPlanePickable( false );
                    getClipDialog().initClipSliceA();
                }
            }

            if ( ( (SceneState) scene ).isVolOpacityChanged ) {
                if ( volOpacityPanel != null ) {
                    volOpacityPanel.getSelectedComponent( ( (SceneState) scene ).whichComp ).updateTransFunc(
                            ( (SceneState) scene ).transformFunc );
                }
            }
        } else {
            slicePanel.enableSlices();
            getSlicePanel().getBoxX().setSelected( ( (SceneState) scene ).xVisible );
            getSlicePanel().setXSliderEnabled( ( (SceneState) scene ).xVisible );
            getSlicePanel().getBoxY().setSelected( ( (SceneState) scene ).yVisible );
            getSlicePanel().setYSliderEnabled( ( (SceneState) scene ).yVisible );
            getSlicePanel().getBoxZ().setSelected( ( (SceneState) scene ).zVisible );
            getSlicePanel().setZSliderEnabled( ( (SceneState) scene ).zVisible );
            getSlicePanel().getSliderX().setValue( ( (SceneState) scene ).x + 1 );
            getSlicePanel().getSliderY().setValue( ( (SceneState) scene ).y + 1 );
            getSlicePanel().getSliderZ().setValue( ( (SceneState) scene ).z + 1 );
            getSlicePanel().setOpacitySliderXEnabled( ( (SceneState) scene ).xVisible );
            getSlicePanel().getOpacitySliderX().setValue( ( (SceneState) scene ).xOpacity );
            getSlicePanel().setOpacitySliderYEnabled( ( (SceneState) scene ).yVisible );
            getSlicePanel().getOpacitySliderY().setValue( ( (SceneState) scene ).yOpacity );
            getSlicePanel().setOpacitySliderZEnabled( ( (SceneState) scene ).zVisible );
            getSlicePanel().getOpacitySliderZ().setValue( ( (SceneState) scene ).zOpacity );
            getSurfaceDialog().getOpacitySlider().setValue( ( (SceneState) scene ).surfaceOpacity );
        }
    }

    /**
     * Overrides the parent autoCapture method.  Camera use this method to
     * take snapshot automatically.
     */
    public void autoCapture() {
        super.autoCapture();
        while ( rotationTimes > 0 ) {
            // This rotation is along the world x, y, z axis, not along the local x, y, z axis
            Matrix4f m = new Matrix4f();

            m.setIdentity();
            Vector3f rotAxis = new Vector3f();
            Vector3f point = new Vector3f();

            transRotation.get( m );
            point.x = m.m03;
            point.y = m.m13;
            point.z = m.m23;
            if ( rotationAxis == ViewJFrameRenderCamera.X_AXIS ) {
                rotAxis.x = 1;
                rotAxis.y = 0;
                rotAxis.z = 0;
            } else if ( rotationAxis == ViewJFrameRenderCamera.Y_AXIS ) {
                rotAxis.x = 0;
                rotAxis.y = 1;
                rotAxis.z = 0;
            } else if ( rotationAxis == ViewJFrameRenderCamera.Z_AXIS ) {
                rotAxis.x = 0;
                rotAxis.y = 0;
                rotAxis.z = 1;
            } else if ( rotationAxis == ViewJFrameRenderCamera.NO_AXIS ) {
                return;
            }

            Matrix me = new Matrix();

            me.element[0][0] = m.m00;
            me.element[0][1] = m.m01;
            me.element[0][2] = m.m02;
            me.element[0][3] = m.m03;
            me.element[1][0] = m.m10;
            me.element[1][1] = m.m11;
            me.element[1][2] = m.m12;
            me.element[1][3] = m.m13;
            me.element[2][0] = m.m20;
            me.element[2][1] = m.m21;
            me.element[2][2] = m.m22;
            me.element[2][3] = m.m23;
            me.element[3][0] = m.m30;
            me.element[3][1] = m.m31;
            me.element[3][2] = m.m32;
            me.element[3][3] = m.m33;

            me.rotate( point, rotAxis, rotationAngle );

            m.m00 = me.element[0][0];
            m.m01 = me.element[0][1];
            m.m02 = me.element[0][2];
            m.m03 = me.element[0][3];
            m.m10 = me.element[1][0];
            m.m11 = me.element[1][1];
            m.m12 = me.element[1][2];
            m.m13 = me.element[1][3];
            m.m20 = me.element[2][0];
            m.m21 = me.element[2][1];
            m.m22 = me.element[2][2];
            m.m23 = me.element[2][3];
            m.m30 = me.element[3][0];
            m.m31 = me.element[3][1];
            m.m32 = me.element[3][2];
            m.m33 = me.element[3][3];
            transRotation.set( m );

            synchronized ( this ) {
                transformChanged( MouseBehaviorCallback.ROTATE, transRotation );
                sceneRootTG.setTransform( transRotation );
                writeImage();
            }
            rotationTimes--;
        }
        writeImage();
    }

    /**
     * Overrides the parent resetImage method.   This method reset
     * the surface volume to the original position.
     */
    public void resetImage() {
        super.resetImage();
        transformChanged( MouseBehaviorCallback.ROTATE, transRotation );
    }

    /**
     * Overrides the parent resetImage method.   This method reset
     * the surface volume to the original position.
     */
    public void resetAxisX() {
        super.resetAxisX();
        transformChanged( MouseBehaviorCallback.ROTATE, transRotation );
    }

    /**
     * Overrides the parent resetImage method.   This method reset
     * the surface volume to the original position.
     */
    public void resetAxisY() {
        super.resetAxisY();
        transformChanged( MouseBehaviorCallback.ROTATE, transRotation );
    }

    /**
     * Overrides the rotateImage to rotate the surface volume.
     */
    public void rotateImage() {
        // Hack way to fix one bug from theading. */
        if ( rotationCount >= 2 ) {
            rotationCount = 1;
            return;
        } else {
            rotationCount++;
            super.rotateImage();
        }
    }

    /**
     *   Dispatches event to appropriate object.
     *   @param event    Event to dispatch.
     */
    public void dispatchSavedEvent( EventObject event ) {
        if ( event instanceof ActionEvent ) {
            actionPerformed( (ActionEvent) event );
        } else if ( event instanceof MouseEvent ) {
            canvas.dispatchEvent( (MouseEvent) event );
        }
    }

    /**
     * Not used now. Might be used later on.
     */
    public void updateProbePos() {
        float x, y, z;
        float xRatio, yRatio, zRatio;
        int xSlice, ySlice, zSlice;

        xSlice = slicePanel.getXProbePos();
        ySlice = slicePanel.getYProbePos();
        zSlice = slicePanel.getZProbePos();
        xRatio = (float) xSlice / (float) ( xDim - 1 );
        yRatio = (float) ySlice / (float) ( yDim - 1 );
        zRatio = (float) zSlice / (float) ( zDim - 1 );
        x = -xBox + xRatio * xBox * 2;
        y = -yBox + yRatio * yBox * 2;
        z = -zBox + zRatio * zBox * 2;

        probePanel.updateProbePos( x, y, z );
    }

    /**
     * Check the given voxel belong to which segmentation.
     * @param x float   given voxel x coordinate
     * @param y float   given voxel y coordinate
     * @param z float   given voxel z coordinate
     * @return int      segmentation component type
     */
    public int whichTissue( float x, float y, float z ) {
        float xRatio, yRatio, zRatio;
        int voxelX, voxelY, voxelZ;
        float value;

        if ( x > xBox || x < -xBox ) {
            return -1;
        }
        if ( y > yBox || y < -yBox ) {
            return -1;
        }
        if ( z > zBox || z < -zBox ) {
            return -1;
        }

        // calculate the voxel's x, y, z coordinates in the volume
        xRatio = ( x + xBox ) / ( xBox * 2 );
        yRatio = ( y + yBox ) / ( yBox * 2 );
        zRatio = ( z + zBox ) / ( zBox * 2 );

        voxelX = MipavMath.round( xRatio * (float) ( xDim - 1 ) );
        voxelY = MipavMath.round( yRatio * (float) ( yDim - 1 ) );
        voxelZ = MipavMath.round( zRatio * (float) ( zDim - 1 ) );
        voxelY = yDim - 1 - voxelY;
        voxelZ = zDim - 1 - voxelZ;

        // get the image intensity value
        value = imageA.getFloat( voxelX, voxelY, voxelZ );

        if ( !isEntryPoint && value >= -75 ) {
            isEntryPoint = true;
            return ENTRY_POINT;
        }

        if ( value >= 200 ) {
            isEntryPoint = true;
            return BONE_SEG;
        }

        if ( segmentationImage != null ) {
            // get image intensity value
            value = segmentationImage.getFloat( voxelX, voxelY, voxelZ );
            if ( value == ARTERIAL_SEG ) {
                return ARTERIAL_SEG;
            } else if ( value == VEINOUS_SEG ) {
                return VEINOUS_SEG;
            } else if ( value == VASCULATURE_SEG ) {
                isEntryPoint = true;
                return VASCULATURE_SEG;
            } else if ( value == TUMOR_SEG ) {
                return TUMOR_SEG;
            }
        }
        return -1;
    }

    /**
     * Set the texture renderer to render in composite mode.
     */
    public void setCompositeMode() {
        m_bRenderModeLighting = false;
        updateImages( null, null, false, -1 );
    }

    /**
     * Set the texture render to render in lighting mode.
     */
    public void setLightingMode() {

        m_bRenderModeLighting = true;

        // Setup state based on properties set in the display options dialog.
        setRenderPerspective( boxPanel.radioButtonPerspective.isSelected() );

        updateLighting();
        updateTextureVolumeRender();
    }

    /**
     * This method is normally called by JPanelLights when a light bulb
     * property changes.  It is up to this instance to decide how to
     * update the rendering.
     */
    public void updateLighting() {

        // Update the software model/world lights.
        JPanelLights kLightsPanel = surfacePanel.getLightDialog();

        m_kSoftwareLightSet.setModelLights( kLightsPanel.getSoftwareLightsModel() );
        m_kSoftwareLightSet.setWorldLights( kLightsPanel.getSoftwareLightsWorld() );
    }

    /**
     * Update the shininess from the JPanelLights.
     * @param value  shininess value.
     */
    public void updateShininess(float value) {
        if ( parent instanceof ViewJFrameVolumeView ) {
              if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                 ( (ViewJFrameVolumeView) parent ).setMaterialShininess(value);
              }
        }
    }

    /**
     * Set the image which we can check to see if the probe is hitting anything important (such as vessels, etc).
     * @param img  segmentation image
     */
    public void setSegmentationImage( ModelImage img ) {
        segmentationImage = img;
    }

    /**
     * Return the segmentation region map image which contains info on where the vascualture, etc are located.
     * @return  (vessel, etc) segmentation image
     */
    public ModelImage getSegmentationImage() {
        return segmentationImage;
    }

    /**
     *	Does nothing but must instantiate for this to implements ViewImageUpdateInterface.
     */
    public void setSlice( int slice ) {}

    /**
     * Set entry point rotation flag.
     * @param flag boolean
     */
    public void enableEntryPoint( boolean flag ) {
        isEntryPoint = flag;
    }

    /**
     * Set the reference to ray based renderer, raycast renderer or shear warp renderer.
     * This method set the clipping dialog to control the both the 3D texture renderer
     * and raycast based renderer.
     * @param _rayBasedRender VolumeRenderer reference
     */
    public void setRayBasedRender( VolumeRenderer _rayBasedRender ) {
        rayBasedRender = _rayBasedRender;
        clipPanel.setRayBasedRender( _rayBasedRender );
        volOpacityPanel.setRayBasedRender( _rayBasedRender );
        surfacePanel.getLightDialog().setRayBasedRender( _rayBasedRender );
        sculptorPanel.setVolumeSculptor( _rayBasedRender );
    }

    /* Return the original ScreenScale for orthographic projection: */
    public double getOriginalScreenScale() {
        return m_dOriginalScreenScale;
    }

    /* Allow disabling or enabling mouse rotation: */
    public void setMouseRotateEnable( boolean bEnable ) {
        mouseRotateBehavior.setEnable( bEnable );
    }

    /* surfaceAdded: tells the parent frame ViewJFrameVolumeView that a new
     * triangle mesh surface has been added to the scene. */
    public void surfaceAdded() {
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).surfaceAdded();
            }
        }
    }

    /**
     * Tell the surfaceRenderer to add the triangle mesh surface.
     */
    public void branchSurfaceAdded() {
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).branchSurfaceAdded();
            }
        }
    }

    /* surfaceAdded: tells the parent frame ViewJFrameVolumeView that a
     * triangle mesh surface has been removed from the scene. */
    public void surfaceRemoved( int iIndex ) {
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).surfaceRemoved( iIndex );
            }
        }
    }

    /**
     * Tell the surfaceRenderer to remove the triangle mesh surface.
     */
    public void branchSurfaceRemoved() {
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).branchSurfaceRemoved();
            }
        }
    }

    /**
     * Set the surface color from the JPanelSurface color control.
     * @param iIndex  surface index.
     * @param kColor  color
     */
    public void setColor( int iIndex, Color4f kColor ) {
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).setColor( iIndex, kColor );
            }
        }
    }

    /**
     * Set the surface color in the tri-planer view.
     * @param kColor color
     */
    public void branchSetColor( Color4f kColor ) {
        if ( parent instanceof ViewJFrameVolumeView ) {
            if ( ( (ViewJFrameVolumeView) parent ) != null ) {
                ( (ViewJFrameVolumeView) parent ).branchSetColor( kColor );
            }
        }
    }

    /**
     * Get the actual X, Y, Z label from the current orientation
     * @param label  desired X, Y, Z label
     * @param isTag  is label tag or not, if label tag, get the specific orientation.
     * @return int
     */
    public int getLabel( int label, boolean isTag ) {
        if ( label == X ) {
            if ( axisXOrient == FileInfoBase.ORI_L2R_TYPE || axisXOrient == FileInfoBase.ORI_R2L_TYPE ) {
                return X;
            } else if ( axisXOrient == FileInfoBase.ORI_A2P_TYPE || axisXOrient == FileInfoBase.ORI_P2A_TYPE ) {
                if ( isTag ) {
                    if ( axisZOrient == FileInfoBase.ORI_R2L_TYPE || axisZOrient == FileInfoBase.ORI_L2R_TYPE
                            || axisZOrient == FileInfoBase.ORI_I2S_TYPE || axisZOrient == FileInfoBase.ORI_S2I_TYPE ) {
                        return Y;
                    }

                } else {
                    if ( axisZOrient == FileInfoBase.ORI_R2L_TYPE || axisZOrient == FileInfoBase.ORI_L2R_TYPE ) {
                        return Z;
                    } else if ( axisZOrient == FileInfoBase.ORI_I2S_TYPE || axisZOrient == FileInfoBase.ORI_S2I_TYPE ) {
                        return Y;
                    }
                }
            } else if ( axisXOrient == FileInfoBase.ORI_I2S_TYPE || axisXOrient == FileInfoBase.ORI_S2I_TYPE ) {
                if ( isTag ) {
                    if ( axisZOrient == FileInfoBase.ORI_R2L_TYPE || axisZOrient == FileInfoBase.ORI_L2R_TYPE
                            || axisZOrient == FileInfoBase.ORI_P2A_TYPE || axisZOrient == FileInfoBase.ORI_A2P_TYPE ) {
                        return Z;
                    }

                } else {
                    if ( axisZOrient == FileInfoBase.ORI_R2L_TYPE || axisZOrient == FileInfoBase.ORI_L2R_TYPE ) {
                        return Z;
                    } else if ( axisZOrient == FileInfoBase.ORI_P2A_TYPE || axisZOrient == FileInfoBase.ORI_A2P_TYPE ) {
                        return Y;
                    }
                }
            } else { // X axis UNKNOWN
                return X;
            }
        } else if ( label == Y ) {
            // Setup Y box frame
            if ( ( axisXOrient == FileInfoBase.ORI_A2P_TYPE || axisXOrient == FileInfoBase.ORI_P2A_TYPE )
                    && ( axisZOrient == FileInfoBase.ORI_R2L_TYPE || axisZOrient == FileInfoBase.ORI_L2R_TYPE ) ) {
                if ( isTag ) {
                    return Z;
                } else {
                    return X;
                }
            } else if ( ( axisZOrient == FileInfoBase.ORI_A2P_TYPE || axisZOrient == FileInfoBase.ORI_P2A_TYPE )
                    && ( axisXOrient == FileInfoBase.ORI_I2S_TYPE || axisXOrient == FileInfoBase.ORI_S2I_TYPE ) ) {
                if ( isTag ) {
                    return X;
                } else {
                    return Z;
                }
            } else if ( axisYOrient == FileInfoBase.ORI_L2R_TYPE || axisYOrient == FileInfoBase.ORI_R2L_TYPE ) {
                return X;
            } else if ( axisYOrient == FileInfoBase.ORI_P2A_TYPE || axisYOrient == FileInfoBase.ORI_A2P_TYPE ) {
                return Y;
            } else if ( axisYOrient == FileInfoBase.ORI_I2S_TYPE || axisYOrient == FileInfoBase.ORI_S2I_TYPE ) {
                return Z;
            } else { // Y axis UNKNOWN
                return Y;
            }
        } else if ( label == Z ) {
            // Setup Z box frame
            if ( axisZOrient == FileInfoBase.ORI_I2S_TYPE || axisZOrient == FileInfoBase.ORI_S2I_TYPE ) {
                return Z;
            } else if ( axisZOrient == FileInfoBase.ORI_L2R_TYPE || axisZOrient == FileInfoBase.ORI_R2L_TYPE ) {
                if ( isTag ) {
                    if ( axisXOrient == FileInfoBase.ORI_A2P_TYPE || axisXOrient == FileInfoBase.ORI_P2A_TYPE
                            || axisXOrient == FileInfoBase.ORI_I2S_TYPE || axisXOrient == FileInfoBase.ORI_S2I_TYPE ) {
                        return X;
                    }

                } else {
                    if ( axisXOrient == FileInfoBase.ORI_A2P_TYPE || axisXOrient == FileInfoBase.ORI_P2A_TYPE ) {
                        return Y;
                    } else if ( axisXOrient == FileInfoBase.ORI_I2S_TYPE || axisXOrient == FileInfoBase.ORI_S2I_TYPE ) {
                        return X;
                    }
                }
            } else if ( axisZOrient == FileInfoBase.ORI_A2P_TYPE || axisZOrient == FileInfoBase.ORI_P2A_TYPE ) {
                if ( isTag ) {
                    if ( axisXOrient == FileInfoBase.ORI_R2L_TYPE || axisXOrient == FileInfoBase.ORI_L2R_TYPE
                            || axisXOrient == FileInfoBase.ORI_I2S_TYPE || axisXOrient == FileInfoBase.ORI_S2I_TYPE ) {
                        return Y;
                    }

                } else {
                    if ( axisXOrient == FileInfoBase.ORI_R2L_TYPE || axisXOrient == FileInfoBase.ORI_L2R_TYPE ) {
                        return Y;
                    } else if ( axisXOrient == FileInfoBase.ORI_I2S_TYPE || axisXOrient == FileInfoBase.ORI_S2I_TYPE ) {
                        return X;
                    }
                }
            } else { // Z axis UNKNOWN
                return Z;
            }
        }
        return -1;

    }

    /**
     * Set the texture material shininess value.
     * @param value   shininess value.
     */
    public void setMaterialShininess(float value) {
        m_kSoftwareMaterial.shininess = value;
    }

}

