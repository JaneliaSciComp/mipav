package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.SceneState;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview.JPanelProbe;
import gov.nih.mipav.view.renderer.J3D.volumeview.VolumeRenderer;

import java.awt.*;
import java.awt.event.*;
import java.io.FileNotFoundException;
import java.util.EventObject;

import javax.media.j3d.*;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.vecmath.*;

import com.sun.j3d.utils.behaviors.mouse.*;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.image.TextureLoader;
import com.sun.j3d.utils.universe.SimpleUniverse;


/**
 * Frame that holds the surface renderer. This frame is only possible to activate if a three dimensional image is
 * loaded. The image shows up in the frame as the three planes, with an axial view, a coronal view, and a sagittal view.
 * The user can slide these planes up and down and can turn them on and off. The user can also load in surfaces created
 * from the original image. These 3D surfaces will appear in the proper place within the three image planes. There are
 * many options for viewing the surfaces. Additionally, the user can change the view mode, so that the mouse causes the
 * view to "fly". The user can then record the different mouse actions and play them back.
 * 
 * @see ViewJComponentSurfaceImage
 * @see JDialogSurface
 * @see JDialogView
 * @see JDialogMouseRecorder
 */
public class SurfaceView extends SurfaceRender {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 608959413602233595L;

    /** Value which indicates a voxel that is part of the arterial vasculature tree. */
    public static final int ARTERIAL_SEG = 1;

    /** Value which indicates a voxel in the segmenation image that is part of the veinous vasculature tree. */
    public static final int VEINOUS_SEG = 2;

    /** Value which indicates a voxel in the segmenation image that is part of the vasculature (arterial or veinous). */
    public static final int VASCULATURE_SEG = 3;

    /** Value which indicates a voxel in the segmenation image that is part of the tumor. */
    public static final int TUMOR_SEG = 4;

    /** Value which indicates a bone in the image. */
    public static final int BONE_SEG = 5;

    /** Value which indicates the probe entry point color. */
    public static final int ENTRY_POINT = 6;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------
    /** Dialog to turn bounding box of surface renderer on and off, and to change the color of the frame. */
    private JPanelDisplay boxPanel;

    /** The frame around the AXIAL, CORONAL, SAGITTAL slices: */
    private final ViewJComponentBoxSlice[] boxSlices = new ViewJComponentBoxSlice[3];

    /**
     * The transformed boxSlices frames used to sample the volume data along diagonal slices:
     */
    private Vector3f[][] boxSliceVertices;

    /** Buffer factor, 1 usually, 4 for color images. */
    private int bufferFactor = 1;

    /** Dialog to turn the clipping palne box on and off. */
    private JPanelClip clipPanel;

    /** The cubic roational behavior branch group. */
    private BranchGroup cubicBehaviorBG;

    /** Cubic branch group. */
    private BranchGroup cubicBG;

    /** Cubic mouse rotation behavior. */
    private MouseRotate cubicRotate;

    /** Cubic transform group. */
    private TransformGroup cubicTG;

    /** Current transform matrix. */
    private Transform3D currentTransform = new Transform3D();

    /** Current transform changes type. */
    private int currentTransformType;

    /** flag indicate the first time 6 clipping plane branch switch. */
    private boolean first6ClipPlane = true;

    /** flag indicate the first time arbitrary clipping plane branch switch. */
    private boolean firstClipArbi = true;

    /** Geodesic panel. */
    private JPanelGeodesic geodesicPanel;

    /** Current image A. */
    private ModelImage imageA;

    /** Current image B. */
    private ModelImage imageB;

    /** Flag indicates that probe rotate around the entry point or not. */
    private boolean isEntryPoint = false;

    /** Flag set when texture volume is being rendered using lights. */
    private boolean m_bRenderModeLighting = false;

    /**
     * Zoomfactor, zooming in screen scale space, and is relative to original position, this stores that original
     * position:.
     */
    private double m_dOriginalScreenScale = 1.0;

    /** TransformGroup[] used to rotate the AXIAL, CORONAL, SAGITTAL slice BOXES: */
    private final TransformGroup[] m_kObjBoxSliceProbe_TG = new TransformGroup[3]; /* Rotates the X Box */

    /** TransformGroup[] used to rotate the AXIAL, CORONAL, SAGITTAL slice PLANES: */
    private final TransformGroup[] m_kObjPlaneProbe_TG = new TransformGroup[3];

    /**
     * The Transform3D used to position the planes based on the Probe position and angle. The planes are rotates about
     * the intersection between the probe and the plane, which may or may not be in the center of the plane. This
     * transform allows non-origin rotations by conactenating the translation to the origin, rotation, and translation
     * matricies into one transform, which is used to set the Transforms for the TransformGroups above and also to
     * sample the ModelImage data on the diagonal slice:
     */
    private Transform3D m_kProbeTransform = new Transform3D();

    /** Used to compute software lighting of composite texture volume. */
    private SoftwareLightSet m_kSoftwareLightSet = new SoftwareLightSet();

    /** DOCUMENT ME! */
    private SoftwareMaterial m_kSoftwareMaterial = new SoftwareMaterial();

    /** Dialog for recording and playing back mouse events. */
    private JPanelMouse mousePanel;

    /** Mouse Rotate behavior. */
    private MouseRotateExt mouseRotateBehavior;

    /** Mouse Translate behavior. */
    private MouseTranslate mouseTranslateBehavior;

    /** Mouse Zoom behavior. */
    private MouseZoom mouseZoomBehavior;

    /** Initial center viewing point of the image. */
    private final Point3d myEyePoint = new Point3d();

    /** Group dictating how the XY plane is translated. */
    private final TransformGroup[] objTransSlices_TG = new TransformGroup[3];

    /** BranchGroup[] containing the image planes. */
    private final BranchGroup[] objPlane_BG = new BranchGroup[3];

    /** Parallel rotation flag. */
    private boolean parallelRotation = true;

    /** Transformation matrix. */
    private Transform3D parallelScaleT3D;

    /** Controls for the image. ViewJFrameVolumeView */
    private ViewJFrameBase parent;

    /** Dialog for loading and displaying probes. */
    private JPanelProbe probePanel;

    /** Resolutions of image A. */
    private float[] resols = new float[4];

    /** fix one bug from threading. */
    private int rotationCount = 1;

    /** Image with areas marked with values indicating that they are special tissue types (vessels, etc). */
    private ModelImage segmentationImage;

    /** Dialog to turn the slices control box on and off. */
    private JPanelSlices slicePanel;

    /** Dialog for loading and displaying surfaces. */
    private JPanelSurface surfacePanel;

    /** Panel for displaying the SurfaceTexture interface: */
    private JPanelSurfaceTexture surfaceTexturePanel;

    /** ****************************************************************************** */

    /** Tri planar view and the 3D texture volume view switch group. */
    private Switch switchGroup;

    /** Texture mapping for the RFA probe rotation: */
    Vector4f[][] m_kRFACoordMaps = new Vector4f[3][2];

    /** Cubic tansform3D. */
    private Transform3D transformCubic;

    /** Setup the initial position of the image, added to ScenerootTG transform group. */
    private Transform3D transformNode3d;

    /** View object in rendering a three dimensional scene from one viewpoint. */
    private View view;

    /** Volume branch group. */
    private BranchGroup volBG;

    /** Volume render branch group. */
    private BranchGroup volRenderBG;

    /** Volume render order group, specify the rendering order. */
    private OrderedGroup volRenderOG;

    /**
     * If true display 3D volume as a 3D texture map or 2D array of texture maps. If false display 3 orthogonal image
     * planes.
     */
    private boolean volumeDisplayMode3D = false;

    /**
     * Numbers dicatating the sizes of the planes based on the extents and resolutions of the image.
     */
    private float xBox, yBox, zBox, maxBox;

    /** Dimensions of image A. */
    private int xDim, yDim, zDim, tDim;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor called by the VirtualEndoscopyView. Used for instantiation.
     * 
     * @param _imageA ModelImage imageA
     * @param _imageB ModelImage imageB always null
     * @param _config GraphicsConfiguration graphics configuration
     */
    public SurfaceView(final ModelImage _imageA, final ModelImage _imageB, final GraphicsConfiguration _config) {
        super(_imageA, _imageB, _config);
        this.imageA = _imageA;
        this.imageB = _imageB;
    }

    /**
     * Makes a frame and puts the three image planes into it. Creates the scene graph which dictates the behavior of the
     * image planes and surfaces. Initializes the surface dialog and the mouse recorder dialog, so that this original
     * view is saved. When the user opens these dialogs, they have already been created; they are just set to visible.
     * 
     * @param _imageA First image to display, cannot be null.
     * @param _LUTa LUT of the imageA (if null grayscale LUT is constructed).
     * @param _imageB Second loaded image, may be null.
     * @param _LUTb LUT of the imageB, may be null.
     * @param _parent Frame base reference.
     * @param _config configuration reference.
     * @param _pBar progress bar
     */
    public SurfaceView(final ModelImage _imageA, final ModelLUT _LUTa, final ModelImage _imageB, final ModelLUT _LUTb,
            final ViewJFrameBase _parent, final GraphicsConfiguration _config, final ViewJProgressBar _pBar) {
        super(_imageA, _imageB, _config);
        this.imageA = _imageA;
        this.imageB = _imageB;
        this.LUTa = _LUTa;
        this.LUTb = _LUTb;
        this.parent = _parent;

        imageA.setImageOrder(ModelImage.IMAGE_A);

        if (imageB != null) {
            imageB.setImageOrder(ModelImage.IMAGE_B);
        }

        bufferFactor = 1;

        if (imageA.isColorImage()) {
            bufferFactor = 4;
        }

        background.setColor(new Color3f(Color.black));

        final int[] dimExtents = imageA.getExtents();
        xDim = dimExtents[0];
        yDim = dimExtents[1];
        zDim = dimExtents[2];
        resols = imageA.getResolutions(0);
        for (int i = 0; i < 3; i++) {
            resols[i] = Math.abs(resols[i]);
        }

        if ( (resols[0] == 0.0f) || (resols[1] == 0.0f) || (resols[2] == 0.0f)) {
            resols[0] = 1.0f;
            resols[1] = 1.0f;
            resols[2] = 1.0f;
        }
        parallelScaleT3D = new Transform3D();
        boxPanel = new JPanelDisplay(this);
        slicePanel = new JPanelSlices(this);
        mousePanel = new JPanelMouse(this);

        configureSliceFrame();
        createImageSceneGraph();
        _pBar.updateValueImmed(5);
        universe = new SimpleUniverse(canvas);
        // This will move the ViewPlatform back a bit so the
        // objects in the scene can be viewed.

        universe.getViewingPlatform().setNominalViewingTransform();

        view = universe.getViewer().getView();

        // Make sure that front and back clip planes are specified as a
        // virtual distance from the eye
        view.setFrontClipPolicy(View.VIRTUAL_EYE);
        view.setBackClipPolicy(View.VIRTUAL_EYE);

        final double backClipDistance = view.getBackClipDistance();

        view.setBackClipDistance(backClipDistance);

        // (back / front) must be < 3000 to avoid loosing z-buffer resolution.
        // The larger the number, the more the z-buffer space will be used
        // for objects in the front of the scene. If you loose z-buffer
        // resolution for objects at the back of your scene, decrease this
        // number. If your objects are getting front clipped, adjust both
        // the front and back clip distances to keep this ratio.
        view.setFrontClipDistance(backClipDistance / 600.0);

        surfacePanel = new JPanelSurface(this, canvas, sceneRootTG, xBox, yBox, zBox);
        probePanel = new JPanelProbe(this, parent, xBox, yBox, zBox);
        _pBar.updateValueImmed(10);
        clipPanel = new JPanelClip(this, xBox, yBox, zBox);
        _pBar.updateValueImmed(15);
        viewPanel = new JPanelView(this);
        _pBar.updateValueImmed(20);

        rotationControlPanel = new JPanelCamera(this);
        geodesicPanel = new JPanelGeodesic(this);
        surfaceTexturePanel = new JPanelSurfaceTexture(this, imageA);

        rotationControlPanel.setVisible(false);
        _pBar.updateValueImmed(30);
        mousePanel.setup();
        objRootBG.compile();
        universe.addBranchGraph(objRootBG);

        transformBoxSlices(new Transform3D());

    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Calls various methods depending on the action.
     * 
     * <ul>
     * <li>Surface - opens the surface dialog</li>
     * <li>View - opens the view control dialog</li>
     * <li>Mouse - opens the mouse recorder dialog</li>
     * <li>About - displays a message about this renderer</li>
     * <li>Exit - sets variables to null and disposes of this frame</li>
     * <li>X, Y, Z checkboxes - toggles the appropriate image planes on or off</li>
     * </ul>
     * 
     * @param event Event that triggered function.
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("MouseControl")) {
            cleanMouseRecorder();
        } else if (command.equals("SlicesControl")) {
            switchToSliceView(true);
        } else if (command.equals("textureRenderControl")) {
            switchToVolView(true);
        } else if (command.equals("Capture")) {
            writeImageAuto();
        } else if (command.equals("CaptureControl")) {

            if (captureFrame != null) {
                disableCamera();
            }
        }
    }

    /**
     * Attach cubic control branch group.
     */
    public void addCubicControl() {

        if ( !cubicBG.isLive()) {
            objRootBG.addChild(cubicBG);
        }
    }

    /**
     * Overrides the parent autoCapture method. Camera use this method to take snapshot automatically.
     */
    public void autoCapture() {
        super.autoCapture();

        while (rotationTimes > 0) {

            // This rotation is along the world x, y, z axis, not along the local x, y, z axis
            final Matrix4f m = new Matrix4f();

            m.setIdentity();

            final Vector3f rotAxis = new Vector3f();
            final Vector3f point = new Vector3f();

            transRotation.get(m);
            point.x = m.m03;
            point.y = m.m13;
            point.z = m.m23;

            if (rotationAxis == ViewJFrameRenderCamera.X_AXIS) {
                rotAxis.x = 1;
                rotAxis.y = 0;
                rotAxis.z = 0;
            } else if (rotationAxis == ViewJFrameRenderCamera.Y_AXIS) {
                rotAxis.x = 0;
                rotAxis.y = 1;
                rotAxis.z = 0;
            } else if (rotationAxis == ViewJFrameRenderCamera.Z_AXIS) {
                rotAxis.x = 0;
                rotAxis.y = 0;
                rotAxis.z = 1;
            } else if (rotationAxis == ViewJFrameRenderCamera.NO_AXIS) {
                return;
            }

            final Matrix me = new Matrix();

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

            me.rotate(point, rotAxis, rotationAngle);

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
            transRotation.set(m);

            synchronized (this) {
                transformChanged(MouseBehaviorCallback.ROTATE, transRotation);
                sceneRootTG.setTransform(transRotation);
                writeImage();
            }

            rotationTimes--;
        }

        writeImage();
    }

    /**
     * Clean mouse recorder for any existing event items.
     */
    public void cleanMouseRecorder() {

        if ( (mousePanel != null) && !mousePanel.listModel.isEmpty()) {

            if (mousePanel.isRecording()) {
                mousePanel.removeAllItems();
            }
        }
    }

    /**
     * Closes the image.
     */
    public void close() {
        disposeLocal(true);
    }

    /**
     * Constructs main frame structures for 3 image planes. Makes the LUT if necessary, then sets up the buffer arrays
     * appropriately and calls the constructors for the three image planes.
     */
    public void configureVolumeFrame() {

        /** Extents of image A, same as xDim, yDim, etc. */
        final int[] extents;
        volRenderBG = new BranchGroup();
        volRenderBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        volRenderBG.setCapability(Group.ALLOW_CHILDREN_READ);
        volRenderBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        volRenderBG.setCapability(BranchGroup.ALLOW_DETACH);
        volRenderBG.setPickable(false);

        volRenderOG.insertChild(volRenderBG, 0);

        if (imageA == null) {
            return;
        }
        switchToVolView(true);

    }

    /**
     * Crop the region of the clipped volume.
     */
    public void cropClipVolume() {
        clipPanel.cropVolume();
    }

    /**
     * Detach volume render branch group.
     */
    public void detachVolRender() {

        if (volRenderBG.isLive()) {
            volRenderOG.removeChild(volRenderOG.indexOfChild(volRenderBG));
        }

        volumeDisplayMode3D = false;
    }

    /**
     * Diable all the six clipping planes at a time.
     */
    public void disableClipping() {
        clipPanel.disable6Planes();
    }

    /**
     * Dispatches event to appropriate object.
     * 
     * @param event Event to dispatch.
     */
    public void dispatchSavedEvent(final EventObject event) {

        if (event instanceof ActionEvent) {
            actionPerformed((ActionEvent) event);
        } else if (event instanceof MouseEvent) {
            canvas.dispatchEvent((MouseEvent) event);
        }
    }

    /**
     * Sets all variables to null, disposes, and garbage collects.
     * 
     * @param flag DOCUMENT ME!
     */
    public void disposeLocal(final boolean flag) {
        parent = null;

        imageA = null;
        imageB = null;

        if (boxPanel != null) {
            boxPanel.dispose();
            boxPanel = null;
        }

        if (slicePanel != null) {
            slicePanel.dispose();
            slicePanel = null;
        }

        if (mousePanel != null) {
            mousePanel.dispose();
            mousePanel = null;
        }

        if (surfacePanel != null) {
            surfacePanel.dispose();
            surfacePanel = null;
        }

        if (surfaceTexturePanel != null) {
            surfaceTexturePanel.dispose();
            surfaceTexturePanel = null;
        }

        if (clipPanel != null) {
            clipPanel.dispose();
            clipPanel = null;
        }

        if (probePanel != null) {
            probePanel.dispose();
            probePanel = null;
        }

        if (rotationControlPanel != null) {
            rotationControlPanel.disposeLocal(false);
            rotationControlPanel = null;
        }

        m_kSoftwareLightSet = null;
        m_kSoftwareMaterial = null;

        if (geodesicPanel != null) {
            geodesicPanel.disposeLocal(false);
            geodesicPanel = null;
        }

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * Called from the parent class when the Probe Entry Point is selected in the PlaneRender object.
     * 
     * @param kPoint probe entry point in ModelStorageBase Coordinates
     */
    public void drawRFAPoint(final Point3f kPoint) {
        /* Scale coordinates for the probe: */
        kPoint.x /= xDim;
        kPoint.y /= yDim;
        kPoint.z /= zDim;

        kPoint.x = -xBox + (2 * kPoint.x * xBox);
        kPoint.y = - (yBox - (2 * kPoint.y * yBox));
        kPoint.z = - (zBox - (2 * kPoint.z * zBox));

        /* update the probe position: */
        probePanel.getProbeBase().updatePosition(kPoint);
    }

    /**
     * Set entry point rotation flag.
     * 
     * @param flag boolean
     */
    public void enableEntryPoint(final boolean flag) {
        isEntryPoint = flag;
    }

    /**
     * Enable objBehavior branch group.
     * 
     * @param flag <code>true</code> means turn on, <code>false</code> means turn off.
     */
    public void enableObjBehavior(final boolean flag) {

        if (flag) {

            if ( !objBehaviorBG.isLive()) {
                sceneRootTG.addChild(objBehaviorBG);
            }

            if (cubicTG.indexOfChild(cubicBehaviorBG) == -1) {
                cubicTG.addChild(cubicBehaviorBG);
            }
        } else {

            if (objBehaviorBG.isLive()) {
                objBehaviorBG.detach();
            }

            if (cubicTG.indexOfChild(cubicBehaviorBG) != -1) {
                cubicBehaviorBG.detach();
            }
        }
    }

    /**
     * Get the rotation control dialog box.
     * 
     * @return rotationControlDialog rotation control dialog
     */
    public JPanelCamera getCameraControl() {
        return rotationControlPanel;
    }

    /**
     * Return clipPanel from parent frame.
     * 
     * @return clipPanel Clip Dialog box.
     */
    public JPanelClip getClipDialog() {
        return clipPanel;
    }

    /**
     * Return the diaplay dialog.
     * 
     * @return boxPanel Display dialog.
     */
    public JPanelDisplay getDisplayDialog() {
        return boxPanel;
    }

    /**
     * Return the current volume display mode.
     * 
     * @return volumeDisplayMode3D volume 3D diaplay mode.
     */
    public boolean getDisplayMode3D() {
        return volumeDisplayMode3D;
    }

    /**
     * Get the geodesic control panel.
     * 
     * @return JPanelGeodesic geodesic control panel.
     */
    public JPanelGeodesic getGeodesicPanel() {
        return geodesicPanel;
    }

    /**
     * Accessor that returns the reference to image A.
     * 
     * @return Image A.
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Accessor that returns the reference to image B.
     * 
     * @return Image B.
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Return mousePanel from parent frame.
     * 
     * @return mousePanel Mouse Dialog box.
     */
    public JPanelMouse getMouseDialog() {
        return mousePanel;
    }

    /**
     * Gets the mouse pointer mode - standard or fly - from the view dialog.
     * 
     * @return The mouse pointer mode.
     */
    public int getMouseMode() {

        if (viewPanel == null) {
            return JPanelView.STD_MODE;
        }

        return viewPanel.getMouseMode();
    }

    /**
     * Gets the objXYPlaneBG branch group.
     * 
     * @return objXYPlaneBG called by JDialogSliceBox
     */
    public BranchGroup getObjPlane_BG(final int orientation) {
        return objPlane_BG[orientation];
    }

    /**
     * Return the original ScreenScale for orthographic projection:
     * 
     * @return DOCUMENT ME!
     */
    public double getOriginalScreenScale() {
        return m_dOriginalScreenScale;
    }

    /**
     * Return the parent frame.
     * 
     * @return ViewJFrameVolumeView parent frame
     */
    public ViewJFrameVolumeView getParentFrame() {
        return (ViewJFrameVolumeView) parent;
    }

    /**
     * Return probePanel from parent frame.
     * 
     * @return probePanel Probe Dialog box.
     */
    public JPanelProbe getProbeDialog() {
        return probePanel;
    }

    /**
     * Gets the current scene state, in terms of what numbers the slices are on and whether or not they are visible.
     * 
     * @return A SceneState object with the variables set appropriately.
     */
    public Object getSceneState() {

        final int comp = 0;
        final TransferFunction opacTransFunct = null;
        final boolean isVolOpacityChanged = false;

        return new SceneState(getSlicePanel().getXSlice(), getSlicePanel().getYSlice(), getSlicePanel().getZSlice(),
                getSlicePanel().getXOpacitySlice(), getSlicePanel().getYOpacitySlice(), getSlicePanel()
                        .getZOpacitySlice(), getSlicePanel().getVisible(0), getSlicePanel().getVisible(1),
                getSlicePanel().getVisible(2), getSurfaceDialog().getSurfaceOpacity(), getClipDialog().getSliceA(),
                getClipDialog().getSliceX(), getClipDialog().getSliceY(), getClipDialog().getSliceZ(), getClipDialog()
                        .getSliceXInv(), getClipDialog().getSliceYInv(), getClipDialog().getSliceZInv(),
                getClipDialog().getAVisible(), getClipDialog().getXVisible(), getClipDialog().getYVisible(),
                getClipDialog().getZVisible(), getClipDialog().getXVisibleInv(), getClipDialog().getYVisibleInv(),
                getClipDialog().getZVisibleInv(), comp, opacTransFunct, isVolOpacityChanged, getDisplayMode3D(),
                getClipDialog().is6PlaneClipping(), getClipDialog().getAxisX(), getClipDialog().getAxisY(),
                getClipDialog().getAxisZ(), getClipDialog().getAxisAngle(), getClipDialog().isClipArbiPicked());
    }

    /**
     * Return the segmentation region map image which contains info on where the vascualture, etc are located.
     * 
     * @return (vessel, etc) segmentation image
     */
    public ModelImage getSegmentationImage() {
        return segmentationImage;
    }

    /**
     * return the current active slicePanel interface.
     * 
     * @return slicePanel called by JDialogMouseRecorder
     */
    public JPanelSlices getSlicePanel() {
        return slicePanel;
    }

    /**
     * Return surfacePanel from parent frame.
     * 
     * @return surfacePanel Surface Dialog box.
     */
    public JPanelSurface getSurfaceDialog() {
        return surfacePanel;
    }

    /**
     * Returns the surface texture panel interface.
     * 
     * @return surface texture panel.
     */
    public JPanelSurfaceTexture getSurfaceTexture() {
        return surfaceTexturePanel;
    }

    /**
     * Returns the surface texture panel mainPanel interface.
     * 
     * @return surface texture mainPanel.
     */
    public JPanel getSurfaceTexturePanel() {
        return surfaceTexturePanel.getMainPanel();
    }

    /**
     * Return the viewPanel from parent frame.
     * 
     * @return viewPanel View Dialog box.
     */
    public JPanelView getViewDialog() {
        return viewPanel;
    }

    /**
     * Return volume opacity control Dialog from parent frame.
     * 
     * @return volOpacityPanel volume opacity dialog box.
     */
    public JPanelVolOpacityBase getVolOpacityPanel() {
        return null;
    }

    /**
     * Return volume render Branch Group.
     * 
     * @return volRenderBG volume render branch group.
     */
    public BranchGroup getVolRenderBG() {
        return volRenderBG;
    }

    /**
     * Return volume render Order Group.
     * 
     * @return volRenderOG volume render order group.
     */
    public OrderedGroup getVolRenderOG() {
        return volRenderOG;
    }

    /**
     * Returns the ModelImage resolutions.
     * 
     * @return ModelImage resolutions
     */
    public float[] getResolutions() {
        return resols;
    }

    /**
     * Makes the box frame invisible.
     */
    public void hideBoxFrame() {
        objBoxFrameBG.detach();
    }

    /**
     * Enable the six clipping planes at a time.
     */
    public void invokeClipping() {
        clipPanel.invokeClippingPlanes();
    }

    /**
     * Constructs main frame structures for 3 image planes. Makes the LUT if necessary, then sets up the buffer arrays
     * appropriately and calls the constructors for the three image planes. Used by the switch between aligned volume
     * texture rendering and the volume texture rendering. Changed to public to allow updating after the volume is
     * sculpted.
     */
    public void reConfigureVolumeFrame() {

        /** Extents of image A, same as xDim, yDim, etc. */
        int[] extents;

        if (imageA == null) {
            return;
        }

        if ( ( (imageA.getNDims() == 3) && (imageB == null))
                || ( (imageA.getNDims() == 3) && (imageB != null) && (imageB.getNDims() == 3))) {
            extents = new int[3];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
        } else { // imageA.getNDims() == 4 or imageB.getNDims() == 4
            extents = new int[4];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;

            if (imageA.getNDims() == 4) {
                extents[3] = tDim;
            } else {
                extents[3] = tDim;
            }
        }

    }

    /**
     * Detach cubic control branch group.
     */
    public void removeCubicControl() {

        if (cubicBG.isLive()) {
            cubicBG.detach();
        }
    }

    /**
     * Overrides the parent resetImage method. This method reset the surface volume to the original position.
     */
    public void resetAxisX() {
        super.resetAxisX();
        transformChanged(MouseBehaviorCallback.ROTATE, transRotation);
    }

    /**
     * Overrides the parent resetImage method. This method reset the surface volume to the original position.
     */
    public void resetAxisY() {
        super.resetAxisY();
        transformChanged(MouseBehaviorCallback.ROTATE, transRotation);
    }

    /**
     * Overrides the parent resetImage method. This method reset the surface volume to the original position.
     */
    public void resetImage() {
        super.resetImage();
        transformChanged(MouseBehaviorCallback.ROTATE, transRotation);
    }

    /**
     * Overrides the rotateImage to rotate the surface volume.
     */
    public void rotateImage() {

        // Hack way to fix one bug from theading. */
        if (rotationCount >= 2) {
            rotationCount = 1;

            return;
        } else {
            rotationCount++;
            super.rotateImage();
        }
    }

    /**
     * Save crop region with the clipped volume.
     */
    public void saveCropVolume() {
        clipPanel.saveCropImage();
    }

    /**
     * Sets the alpha blending of parameter for two image displays.
     * 
     * @param value Amount [0,100] that is the percentage of Image A to be displayed.
     */
    public void setAlphaBlend(final int value) {

    }

    /**
     * Set the texture renderer to render in composite mode.
     */
    public void setCompositeMode() {
        m_bRenderModeLighting = false;
        updateImages(null, null, false, -1);
    }

    /**
     * Hack. Set the update 3D texture volume win-level flag.
     * 
     * @param flag true update 3D texture volume with win-level, false not update.
     */
    public void setDisplayMode3D(final boolean flag) {
        volumeDisplayMode3D = flag;
    }

    /**
     * Sets the GUI components to their proper state before the action is dispatched from the player.
     * 
     * @param scene The state of the scene.
     */
    public void setGUI(final Object scene) {

        if ( ((SceneState) scene).isVolumeDisplayMode3D) {
            slicePanel.disableSlices();

            if ( ((SceneState) scene).is6PlaneClipping) {

                if ( ((SceneState) scene).xClipVisible || ((SceneState) scene).yClipVisible
                        || ((SceneState) scene).zClipVisible || ((SceneState) scene).xNegClipVisible
                        || ((SceneState) scene).yNegClipVisible || ((SceneState) scene).zNegClipVisible) {
                    getClipDialog().disableClipA();
                }

                if (first6ClipPlane) {
                    getClipDialog().swapModelClipBG(true);
                    first6ClipPlane = false;
                    firstClipArbi = true;
                }

                getClipDialog().setCheckBoxX( ((SceneState) scene).xClipVisible);
                getClipDialog().setXSliderEnabled( ((SceneState) scene).xClipVisible);

                if ( ((SceneState) scene).xClipVisible) {
                    getClipDialog().getSliderX().setValue( ((SceneState) scene).clipSliceX);
                } else {
                    getClipDialog().initClipSliceX();
                }

                getClipDialog().setCheckBoxY( ((SceneState) scene).yClipVisible);
                getClipDialog().setYSliderEnabled( ((SceneState) scene).yClipVisible);

                if ( ((SceneState) scene).yClipVisible) {
                    getClipDialog().getSliderY().setValue( ((SceneState) scene).clipSliceY);
                } else {
                    getClipDialog().initClipSliceY();
                }

                getClipDialog().setCheckBoxZ( ((SceneState) scene).zClipVisible);
                getClipDialog().setZSliderEnabled( ((SceneState) scene).zClipVisible);

                if ( ((SceneState) scene).zClipVisible) {
                    getClipDialog().getSliderZ().setValue( ((SceneState) scene).clipSliceZ);
                } else {
                    getClipDialog().initClipSliceZ();
                }

                getClipDialog().setCheckBoxXInv( ((SceneState) scene).xNegClipVisible);
                getClipDialog().setXSliderInvEnabled( ((SceneState) scene).xNegClipVisible);

                if ( ((SceneState) scene).xNegClipVisible) {
                    getClipDialog().getSliderXInv().setValue( ((SceneState) scene).clipSliceXNeg);
                } else {
                    getClipDialog().initClipSliceXInv();
                }

                getClipDialog().setCheckBoxYInv( ((SceneState) scene).yNegClipVisible);
                getClipDialog().setYSliderInvEnabled( ((SceneState) scene).yNegClipVisible);

                if ( ((SceneState) scene).yNegClipVisible) {
                    getClipDialog().getSliderYInv().setValue( ((SceneState) scene).clipSliceYNeg);
                } else {
                    getClipDialog().initClipSliceYInv();
                }

                getClipDialog().setCheckBoxZInv( ((SceneState) scene).zNegClipVisible);
                getClipDialog().setZSliderInvEnabled( ((SceneState) scene).zNegClipVisible);

                if ( ((SceneState) scene).zNegClipVisible) {
                    getClipDialog().getSliderZInv().setValue( ((SceneState) scene).clipSliceZNeg);
                } else {
                    getClipDialog().initClipSliceZInv();
                }
            } else { // arbitrary clipping

                if ( ((SceneState) scene).aClipVisible) {
                    getClipDialog().disable6Planes();
                }

                if (firstClipArbi) {
                    getClipDialog().swapModelClipBG(false);
                    firstClipArbi = false;
                    first6ClipPlane = true;
                }

                getClipDialog().setCheckBoxA( ((SceneState) scene).aClipVisible);
                getClipDialog().setASliderEnabled( ((SceneState) scene).aClipVisible);

                if ( ((SceneState) scene).aClipVisible) {

                    if ( ((SceneState) scene).isClipArbiPicked) {
                        getClipDialog().setArbiPlanePickable(true);
                        getClipDialog().setClipSliceAwithRotate( ((SceneState) scene).axisX,
                                ((SceneState) scene).axisY, ((SceneState) scene).axisZ, ((SceneState) scene).axisAngle);
                    } else {
                        getClipDialog().setArbiPlanePickable(false);
                    }

                    getClipDialog().getSliderA().setValue( ((SceneState) scene).clipSliceA);
                } else {
                    getClipDialog().setArbiPlanePickable(false);
                    getClipDialog().initClipSliceA();
                }
            }

        } else {
            if (slicePanel != null) {
                slicePanel.enableSlices();
                getSlicePanel().getBoxX().setSelected( ((SceneState) scene).xVisible);
                getSlicePanel().setXSliderEnabled( ((SceneState) scene).xVisible);
                getSlicePanel().getBoxY().setSelected( ((SceneState) scene).yVisible);
                getSlicePanel().setYSliderEnabled( ((SceneState) scene).yVisible);
                getSlicePanel().getBoxZ().setSelected( ((SceneState) scene).zVisible);
                getSlicePanel().setZSliderEnabled( ((SceneState) scene).zVisible);
                getSlicePanel().getSliderX().setValue( ((SceneState) scene).x + 1);
                getSlicePanel().getSliderY().setValue( ((SceneState) scene).y + 1);
                getSlicePanel().getSliderZ().setValue( ((SceneState) scene).z + 1);
                getSlicePanel().setOpacitySliderXEnabled( ((SceneState) scene).xVisible);
                getSlicePanel().getOpacitySliderX().setValue( ((SceneState) scene).xOpacity);
                getSlicePanel().setOpacitySliderYEnabled( ((SceneState) scene).yVisible);
                getSlicePanel().getOpacitySliderY().setValue( ((SceneState) scene).yOpacity);
                getSlicePanel().setOpacitySliderZEnabled( ((SceneState) scene).zVisible);
                getSlicePanel().getOpacitySliderZ().setValue( ((SceneState) scene).zOpacity);
                getSurfaceDialog().getOpacitySlider().setValue( ((SceneState) scene).surfaceOpacity);
            }
        }
    }

    /**
     * Set the texture render to render in lighting mode.
     */
    public void setLightingMode() {

        m_bRenderModeLighting = true;

        // Setup state based on properties set in the display options dialog.
        setRenderPerspective(boxPanel.radioButtonPerspective.isSelected());

        updateLighting();
        updateTextureVolumeRender();
    }

    /**
     * Accessor that sets the LUT for image A.
     * 
     * @param LUT The LUT.
     */
    public void setLUTa(final ModelLUT LUT) {

    }

    /**
     * Accessor that sets the LUT for image B.
     * 
     * @param LUT The LUT
     */
    public void setLUTb(final ModelLUT LUT) {

    }

    /**
     * Set the texture material shininess value.
     * 
     * @param value shininess value.
     */
    public void setMaterialShininess(final float value) {
        m_kSoftwareMaterial.shininess = value;
    }

    /**
     * Sets the mouse pointer mode - standard or fly - in the view dialog.
     * 
     * @param mode Mode to set to.
     */
    public void setMouseMode(final int mode) {
        viewPanel.setMouseMode(mode);
    }

    /**
     * Allow disabling or enabling mouse rotation:
     * 
     * @param bEnable DOCUMENT ME!
     */
    public void setMouseRotateEnable(final boolean bEnable) {
        mouseRotateBehavior.setEnable(bEnable);
    }

    /**
     * Set the parallel rotation flag from the viewJFrameVolumeView.
     * 
     * @param flag <code>true</code> set all the renderer to parallel rotation, <code>false</code> parallel rotation
     *            mode off.
     */
    public void setParallelRotation(final boolean flag) {
        parallelRotation = flag;
    }

    /**
     * Sets the Transform3D for the slices based on the Probe Transform. To allow rotation about the probe position, and
     * not just the center of the slices, the probe translation and rotation protions of the matrix are separated. First
     * the center of the slice is translated from the probe position to the origin, then rotated, then translated back
     * to the probe position. The three transformations are concatenated into one Transform3D, which is used to display
     * the slices, boxes, and to sample the ModelImage on the diagonal:
     * 
     * @param kTransform current probe transform
     * @param bTwist true if there is twist about the probe axis
     */
    public void setProbeTG(final Transform3D kTransform, final boolean bTwist) {

        /* Get the translation and inverse translation transforms: */
        final Vector3f kTranslateVector = new Vector3f();
        kTransform.get(kTranslateVector);

        final Transform3D kTranslate = new Transform3D();
        kTranslate.setTranslation(kTranslateVector);

        final Transform3D kTranslateInv = new Transform3D();
        kTranslateInv.setTranslation(new Vector3f( -kTranslateVector.x, -kTranslateVector.y, -kTranslateVector.z));

        /* Get the rotation transform: */
        final Matrix3f kRotationMatrix = new Matrix3f();
        kTransform.getRotationScale(kRotationMatrix);

        final Transform3D kRotate = new Transform3D();
        kRotate.setRotationScale(kRotationMatrix);

        /*
         * Concatenate the three transformations to rotate about the probe position:
         */
        final Transform3D kTransformTotal = new Transform3D();
        kTransformTotal.mul(kTranslate);
        kTransformTotal.mul(kRotate);
        kTransformTotal.mul(kTranslateInv);

        /* Set the transform for sampling the ModelImage on the diagonal: */
        if (m_kProbeTransform == null) {
            m_kProbeTransform = new Transform3D(kTransformTotal);
        } else if (bTwist) {
            m_kProbeTransform.mul(kTransformTotal, m_kProbeTransform);
        } else {
            m_kProbeTransform.set(kTransformTotal);
        }

        transformBoxSlices(m_kProbeTransform);

        /* Set the transforms for displaying the slices and boxes: */
        for (int i = 0; i < 3; i++) {
            if (m_kObjBoxSliceProbe_TG[i] != null) {
                m_kObjBoxSliceProbe_TG[i].setTransform(m_kProbeTransform);
            }
            m_kObjPlaneProbe_TG[i].setTransform(m_kProbeTransform);
        }
    }

    /**
     * Set the reference to ray based renderer, raycast renderer or shear warp renderer. This method set the clipping
     * dialog to control the both the 3D texture renderer and raycast based renderer.
     * 
     * @param _rayBasedRender VolumeRenderer reference
     */
    public void setRayBasedRender(final VolumeRenderer _rayBasedRender) {
        clipPanel.setRayBasedRender(_rayBasedRender);
        surfacePanel.getLightDialog().setRayBasedRender(_rayBasedRender);
    }

    /**
     * Enable perspective projection rendering; otherwise use orthographic projection.
     * 
     * @param bEnable true to enable perspective projection
     */
    public void setRenderPerspective(final boolean bEnable) {
        // The projection policy is stored in the view.
        // Note that the window resize policy is PHYSICAL_WORLD.

        final View kView = universe.getViewer().getView();

        if (bEnable) {
            kView.setProjectionPolicy(View.PERSPECTIVE_PROJECTION);
            kView.setScreenScalePolicy(View.SCALE_SCREEN_SIZE);
        } else {

            // The View's screen scale is always updated based
            // on the difference in z-coordinates of the eyepoint
            // and the center of the bounding sphere around the volume.
            setupEye();
            updateViewScreenScale(parallelScaleT3D);
            kView.setProjectionPolicy(View.PARALLEL_PROJECTION);
            kView.setScreenScalePolicy(View.SCALE_EXPLICIT);
        }

        if ( (clipPanel == null) || ( (clipPanel != null) && !clipPanel.isClipArbiPicked())) {
            updateCubicTransform(transformNode3d);
        }
        getParentFrame().setRenderPerspective(bEnable);
    }

    /**
     * Sets the RGB table for image A.
     * 
     * @param RGBT New RGB table for image A.
     */
    public void setRGBTA(final ModelRGB RGBT) {}

    /**
     * Sets the RGB table for image B.
     * 
     * @param RGBT New RGB table for image B.
     */
    public void setRGBTB(final ModelRGB RGBT) {}

    /**
     * Set the image which we can check to see if the probe is hitting anything important (such as vessels, etc).
     * 
     * @param img segmentation image
     */
    public void setSegmentationImage(final ModelImage img) {
        segmentationImage = img;
    }

    /**
     * Sets the values for JPanelSlices, based on the positions in the PlaneRender class for the AXIAL, CORONAL, and
     * SAGITTAL views.
     * 
     * @param center the three slider values in File Coordinates
     */
    public void setCenter(final Vector3f center) {

        /* update the sliders: */
        slicePanel.setCenter((int) center.x, (int) center.y, (int) center.z);
        /* re-render: */
        updateBoxSlicePos(false);

    }

    /**
     * Does nothing but must instantiate for this to implements ViewImageUpdateInterface.
     * 
     * @param slice DOCUMENT ME!
     */
    public void setSlice(final int slice) {}

    /**
     * Sets the color of the x slice frame.
     * 
     * @param color Color to set to.
     */
    public void setSliceColor(final Color color, final int orientation) {
        boxSlices[orientation].setColor(color);

        if (parent instanceof ViewJFrameVolumeView) {
            if ( ((ViewJFrameVolumeView) parent) != null) {
                ((ViewJFrameVolumeView) parent).setSliceHairColor(orientation, color);
            }
        }
    }

    /**
     * Sets the time slice to be displayed and changes the image planes displayed.
     * 
     * @param slice Indicates image time-slice (4th dimension) to be displayed.
     */
    public void setTimeSlice(final int slice) {

        if (imageA.getNDims() == 4) {

            if (slicePanel.tSlice < imageA.getExtents()[3]) {
                slicePanel.tSlice = slice;
                updateImages(null, null, true, -1);
            }
        } else if ( (imageB != null) && (imageB.getNDims() == 4)) {

            if (slicePanel.tSlice < imageB.getExtents()[3]) {
                slicePanel.tSlice = slice;
                updateImages(null, null, true, -1);
            }
        } else {
            return;
        }
    }

    /**
     * Accessor that sets the volumeDisplayMode flag. If true image is displayed as a volume. If false image is
     * displayed as three orthogonal planes.
     * 
     * @param volDisplayMode if true image is displayed as a volume ( 3D texture or 2D array Texture).
     */
    public void setVolumeDisplayMode(final boolean volDisplayMode) {
        volumeDisplayMode3D = volDisplayMode;
    }

    /**
     * Makes the box frame visible.
     */
    public void showBoxFrame() {
        sceneRootTG.addChild(objBoxFrameBG);
    }

    /**
     * Sets how the image plane should be displayed depending on value of slider.
     * 
     * @param e Event that triggered this function.
     */
    public void stateChanged(final ChangeEvent e) {}

    /**
     * Perform some actions required when switching to the slice renderer.
     * 
     * @param firstTime perform special actions required when switching for the first time
     */
    public void switchToSliceView(final boolean firstTime) {
        switchGroup.setWhichChild(0);
        volumeDisplayMode3D = false;

        if (surfacePanel != null) {

            if (surfacePanel.getSurfaceClipCB().isSelected() && (clipPanel != null)) {
                clipPanel.removeClipSlice();
                surfacePanel.getSurfaceClipCB().setSelected(false);
            }

            surfacePanel.getSurfaceClipCB().setEnabled(false);
        }

        if (clipPanel != null) {
            clipPanel.disableClipPlanes();
            clipPanel.disableClipPlanesArbi();
            clipPanel.setVisible(false);
        }

        if (firstTime && (mousePanel != null) && !mousePanel.listModel.isEmpty()) {

            if (mousePanel.isRecording()) {
                mousePanel.removeAllItems();
            }
        }

        if (boxPanel != null) {
            boxPanel.setEnable(false);
        }

    }

    /**
     * Perform some actions required when switching to the volume renderer.
     * 
     * @param firstTime perform special actions required when switching for the first time
     */
    public void switchToVolView(final boolean firstTime) {

        if (surfacePanel != null) {
            surfacePanel.getSurfaceClipCB().setEnabled(true);
        }

        switchGroup.setWhichChild(1);
        volumeDisplayMode3D = true;

        if (firstTime && (mousePanel != null) && !mousePanel.listModel.isEmpty()) {

            if (mousePanel.isRecording()) {
                mousePanel.removeAllItems();
            }
        }

        if (clipPanel != null) {

            if (clipPanel.isFirstTimeBuildTree()) {
                clipPanel.buildClipPlanesTree();
            }
        }

        if (boxPanel != null) {
            boxPanel.setEnable(true);
        }

    }

    /**
     * Tells the mouse dialog that the transform has changed. Does not use the type parameter.
     * 
     * @param type Type of transform.
     * @param transform Transform that was changed to.
     */
    public void transformChanged(final int type, final Transform3D transform) {
        mousePanel.transformChanged(type, transform);
        currentTransformType = type;
        currentTransform = transform;

        if (MouseBehaviorCallback.ZOOM == type) {
            updateViewScreenScale(transform);
        }

        if (MouseBehaviorCallback.ROTATE == type) {

            if (clipPanel != null) {
                clipPanel.displaySClipPlanePts();
            }
            updateRaycastRender();
            updateTextureVolumeRender();

            if ( (clipPanel == null) || ( (clipPanel != null) && !clipPanel.isClipArbiPicked())) {
                updateCubicTransform(transform);
            }
        }
    }

    /**
     * The raybased renderer invokes this method when a mouse release event occurs.
     * 
     * @param type Type of transform.
     * @param transform Transform that was changed to.
     */
    public void transformUpdate(final int type, final Transform3D transform) {

        if ( (MouseBehaviorCallback.ROTATE == type) && parallelRotation && (transform != null)) {
            currentTransformType = type;
            currentTransform.set(transform);
            currentTransform.setScale(0.75);
            sceneRootTG.setTransform(currentTransform);
            updateTextureVolumeRender();

            if ( (clipPanel == null) || ( (clipPanel != null) && !clipPanel.isClipArbiPicked())) {
                updateCubicTransform(currentTransform);
            }
        }
    }

    /**
     * Undo crop region with the clipped volume.
     */
    public void undoCropVolume() {
        clipPanel.undoCrop();
    }

    /**
     * transform the points used to render the triSliceImages textures.
     * 
     * @param kTransform the Transform3D used to rotate the boxes.
     */
    private void transformBoxSlices(Transform3D kTransform) {
        if (kTransform == null) {
            kTransform = new Transform3D();
            kTransform.setIdentity();
        }
        final Point3f[][] inVertices = new Point3f[3][];
        final Point3f[][] outVertices = new Point3f[3][];

        for (int j = 0; j < 3; j++) {

            inVertices[j] = boxSlices[j].getVertices();
            outVertices[j] = new Point3f[4];
            if (boxSliceVertices[j] == null) {
                boxSliceVertices[j] = new Vector3f[4];
            }
            for (int i = 0; i < 4; i++) {
                outVertices[j][i] = new Point3f();
                if (boxSliceVertices[j][i] == null) {
                    boxSliceVertices[j][i] = new Vector3f();
                }

                /* Rotate the points in the bounding box: */
                kTransform.transform(inVertices[j][i], outVertices[j][i]);
                /* Convert the points to ModelImage space: */
                ScreenToModel(new Vector3f(outVertices[j][i].x, outVertices[j][i].y, outVertices[j][i].z),
                        boxSliceVertices[j][i]);
            }
        }

    }

    /**
     * Translate from normalized plane coordinates to Model coordinates:
     * 
     * @param screen the input point to be transformed from normalized plane coordinates
     * @param model the output point in Model coordinates
     */
    private void ScreenToModel(final Vector3f screen, final Vector3f model) {
        model.x = Math.round( ( (screen.x + xBox) * ((float) xDim - 1)) / (2.0f * xBox));
        model.y = Math.round( ( (screen.y - yBox) * ((float) yDim - 1)) / ( -2.0f * yBox));
        model.z = Math.round( ( (screen.z - zBox) * ((float) zDim - 1)) / ( -2.0f * zBox));
    }

    /**
     * Translate from Model coordinates to normalized plane coordinates:
     * 
     * @param model the input point in Model coordinates
     * @param screen the output point in normalized plane coordinates
     */
    public void ModelToScreen(final Vector3f model, final Vector3f screen) {
        screen.x = (2.0f * xBox) * (model.x / ((float) xDim - 1)) - xBox;
        screen.y = ( -2.0f * yBox) * (model.y / ((float) yDim - 1)) + yBox;
        screen.z = ( -2.0f * zBox) * (model.z / ((float) zDim - 1)) + zBox;
    }

    /**
     * sets up the values for the ViewJComponentBoxSlice array. Reorders the three boxSlice (x,y,z) coordinates and mode
     * value into local ModelCoordinates.
     * 
     * @param Vector3f[] the reordered (x,y,z) coordinates for each boxSlice remapped into ModelCoordinates.
     * @param boxSliceConstants the reordered mode value (X_SLICE, Y_SLICE, Z_SLICE) remapped into ModelCoordinates
     */
    private void getBoxSliceInScreen(final Vector3f[] screenBoxPoints, final int[] boxSliceConstants) {
        final WildMagic.LibFoundation.Mathematics.Vector3f modelIndex = new WildMagic.LibFoundation.Mathematics.Vector3f();
        MipavCoordinateSystems.fileToModel(new WildMagic.LibFoundation.Mathematics.Vector3f(0, 1, 2), modelIndex,
                imageA);
        boxSliceConstants[0] = (int) modelIndex.X;
        boxSliceConstants[1] = (int) modelIndex.Y;
        boxSliceConstants[2] = (int) modelIndex.Z;

        final float[][] boxPoints = new float[3][];
        for (int i = 0; i < 3; i++) {
            boxPoints[i] = new float[3];
            boxPoints[i][0] = xDim - 1;
            boxPoints[i][1] = yDim - 1;
            boxPoints[i][2] = zDim - 1;

            boxPoints[i][boxSliceConstants[i]] = slicePanel.getSlice(i);
        }

        for (int i = 0; i < 3; i++) {
            this.ModelToScreen(new Vector3f(boxPoints[i][0], boxPoints[i][1], boxPoints[i][2]), screenBoxPoints[i]);
        }
    }

    /**
     * Update the transform3D for the cubic.
     * 
     * @param transform Transform3D
     */
    public void updateCubicTransform(final Transform3D transform) {
        final View kView = universe.getViewer().getView();
        final Matrix4f matrix = new Matrix4f();
        final AxisAngle4f axis = new AxisAngle4f();

        transformNode3d = transform;
        transform.get(matrix);
        axis.set(matrix);
        transformCubic.setRotation(axis);
        transformCubic.setScale(0.1f);

        if (kView.getProjectionPolicy() == View.PERSPECTIVE_PROJECTION) {
            transformCubic.setTranslation(new Vector3f( -0.7f, -0.7f, -0.7f));
        } else {
            transformCubic.setTranslation(new Vector3f( -0.8f, -0.8f, -0.8f));
        }

        cubicTG.setTransform(transformCubic);
    }

    /**
     * Called when the underlying data has changed, due to sculpting. Calls the ViewJFrameVolumeView updateSliceData
     * function to update the data in the slices.
     */
    public void updateData() {

        if (parent instanceof ViewJFrameVolumeView) {

            if ( ((ViewJFrameVolumeView) parent) != null) {
                ((ViewJFrameVolumeView) parent).updateSliceData();
            }
        }
    }

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. The extents on this image have
     * changed, so the extents need to be read in again and menus, panes and slide bars adjusted accordingly.
     * 
     * @return DOCUMENT ME!
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * Build a image with the current rotational transformation matrix. Step.1 convert the java3D transform matrix into
     * a quaternion component. Step.2 convert the quaternion into image( our world ) coordinate system. Step.3 convert
     * the quaternion into a rotatin matrix. Quaternion q ( w, x, y, z ): rotation of w around the vector ( x, y, z );
     * Convert the quaternion into a rotation matrix. / \ | 1-2*y^2-2*z^2 2*x*y-2*w*z 2*x*z+2*w*y | | 2*xy+2*w*z
     * 1-2*x^2-2*z^2 2*y*z-2*w*x | | 2*x*z-2*w*y 2*y*z+2*w*x 1-2*x^2-2*y^2 | \ / Step.4 Calling the transform algorithm
     * to extract the image.
     */
    public void updateImageFromRotation() {
        final int interp = 0;
        double w, x, y, z;
        final double[][] result = new double[4][4];

        final Matrix4d mtx = new Matrix4d();
        final Quat4d quat = new Quat4d();

        // Step.1
        currentTransform.get(mtx);
        mtx.get(quat);

        // Step.2
        w = quat.w;
        x = quat.x;
        y = -quat.y;
        z = -quat.z;

        // Step.3
        final TransMatrix transMtx = new TransMatrix(4);

        transMtx.set(0, 0, 1 - (2 * (y * y)) - (2 * (z * z)));
        transMtx.set(0, 1, 2 * ( (x * y) - (w * z)));
        transMtx.set(0, 2, 2 * ( (x * z) + (w * y)));
        transMtx.set(0, 3, 0);
        transMtx.set(1, 0, 2 * ( (x * y) + (w * z)));
        transMtx.set(1, 1, 1 - (2 * (x * x)) - (2 * (z * z)));
        transMtx.set(1, 2, 2 * ( (y * z) - (w * x)));
        transMtx.set(1, 3, 0);
        transMtx.set(2, 0, 2 * ( (x * z) - (w * y)));
        transMtx.set(2, 1, 2 * ( (y * z) + (w * x)));
        transMtx.set(2, 2, 1 - (2 * (x * x)) - (2 * (y * y)));
        transMtx.set(2, 3, 1);
        transMtx.set(3, 0, 0);
        transMtx.set(3, 1, 0);
        transMtx.set(3, 2, 0);
        transMtx.set(3, 3, 1);

        final TransMatrix xfrm = new TransMatrix(4);

        final WildMagic.LibFoundation.Mathematics.Vector3f center = imageA.getImageCentermm(false);

        xfrm.setTranslate(center.X, center.Y, center.Z);
        xfrm.Mult(transMtx);
        xfrm.setTranslate( -center.X, -center.Y, -center.Z);

        // Step.4
        final AlgorithmTransform algoTrans = new AlgorithmTransform(imageA, xfrm, interp, resols[0], resols[1],
                resols[2], xDim, yDim, zDim, false, false, false);

        algoTrans.setUpdateOriginFlag(false);
        algoTrans.run();

        final ModelImage resultImage1 = algoTrans.getTransformedImage();

        if ( (algoTrans.isCompleted() == true) && (resultImage1 != null)) {
            resultImage1.calcMinMax();

            // The algorithm has completed and produced a new image to be displayed.
            try {
                new ViewJFrameImage(resultImage1, null, new Dimension(610, 200));
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
        }
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen - fastest of the three update methods.
     * 
     * @return Confirms successful update.
     */
    public boolean updateImages() {

        updateVolume(null, null, false);

        return true;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes.
     * 
     * @param flag forces show to re import image and calc. java image
     * 
     * @return boolean confirming successful update
     */
    public boolean updateImages(final boolean flag) {

        if (updateVolume(null, null, flag) == false) {
            return false;
        }

        return true;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes.
     * 
     * @param LUTa DOCUMENT ME!
     * @param LUTb DOCUMENT ME!
     * @param forceShow Forces show to reimport image and calculate java image.
     * @param interpMode DOCUMENT ME!
     * 
     * @return Confirms successful update.
     */
    public boolean updateImages(final ModelLUT LUTa, final ModelLUT LUTb, final boolean forceShow, final int interpMode) {
        boolean success = false;

        if (getDisplayMode3D() == true) {
            success = updateVolume(LUTa, LUTb, forceShow);
        }

        return success;
    }

    /**
     * This method is normally called by JPanelLights when a light bulb property changes. It is up to this instance to
     * decide how to update the rendering.
     */
    public void updateLighting() {

        // Update the software model/world lights.
        final JPanelLights kLightsPanel = surfacePanel.getLightDialog();

        m_kSoftwareLightSet.setModelLights(kLightsPanel.getSoftwareLightsModel());
        m_kSoftwareLightSet.setWorldLights(kLightsPanel.getSoftwareLightsWorld());
    }

    /**
     * Updates the slice textures based on the Probe position and rotation angle. The ModelImage data is sampled along
     * the diagonal, not axis-aligned. bInterpolate indicates whether to use nearest-neighbors or to interpolate the
     * data.
     * 
     * @param forceShow Forces triSliceImages[i].show to re-calculate the image texture.
     * @param bInterpolate uses nearest-neighbors when false, interpolates data when true
     * @param bReset when true indicates that the probe navigation has been reset and the slice bounding boxes should be
     *            displayed, when false the probe is no axis-aligned and the slice bounding boxes are not displayed
     */
    public void updateProbe(final boolean forceShow, final boolean bInterpolate, final boolean bReset) {

        for (int i = 0; i < 3; i++) {

            /* Turn slice bounding boxes on: */
            if (bReset) {
                showBoxSlice(i);
            }
            /* Turn slice bounding boxes off: */
            else {
                removeBoxSlice(i);
            }
        }
        return;
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
        xRatio = (float) xSlice / (float) (xDim - 1);
        yRatio = (float) ySlice / (float) (yDim - 1);
        zRatio = (float) zSlice / (float) (zDim - 1);
        x = -xBox + (xRatio * xBox * 2);
        y = -yBox + (yRatio * yBox * 2);
        z = -zBox + (zRatio * zBox * 2);

        probePanel.updateProbePos(x, y, z);
    }

    /**
     * Update the raycast based renderer. JPanelSurface mouse release event invokes this method call.
     */
    public void updateRaycastRender() {

    }

    /**
     * Update the shininess from the JPanelLights.
     * 
     * @param value shininess value.
     */
    public void updateShininess(final float value) {

        if (parent instanceof ViewJFrameVolumeView) {

            if ( ((ViewJFrameVolumeView) parent) != null) {
                ((ViewJFrameVolumeView) parent).setMaterialShininess(value);
            }
        }
    }

    /**
     * Tells the mouse dialog that the transform has changed. Does not use the type parameter.
     * 
     * @param transform Transform that was changed to.
     */
    public void updateTransform(final Transform3D transform) {
        updateTextureVolumeRender();

    }

    /**
     * This methods calls the componentImage's update method to redraw the screen.
     * 
     * @param LUTa LUT used to update imageA.
     * @param LUTb LUT used to update imageB.
     * @param forceShow Forces show to reimport image and calculate java image.
     * 
     * @return Confirms successful update.
     */
    public boolean updateVolume(final ModelLUT LUTa, final ModelLUT LUTb, final boolean forceShow) {
        return true;
    }

    /**
     * Check the given voxel belong to which segmentation.
     * 
     * @param x float given voxel x coordinate
     * @param y float given voxel y coordinate
     * @param z float given voxel z coordinate
     * 
     * @return int segmentation component type
     */
    public int whichTissue(final float x, final float y, final float z) {
        float xRatio, yRatio, zRatio;
        int voxelX, voxelY, voxelZ;
        float value;

        if ( (x > xBox) || (x < -xBox)) {
            return -1;
        }

        if ( (y > yBox) || (y < -yBox)) {
            return -1;
        }

        if ( (z > zBox) || (z < -zBox)) {
            return -1;
        }

        // calculate the voxel's x, y, z coordinates in the volume
        xRatio = (x + xBox) / (xBox * 2);
        yRatio = (y + yBox) / (yBox * 2);
        zRatio = (z + zBox) / (zBox * 2);

        voxelX = MipavMath.round(xRatio * (xDim - 1));
        voxelY = MipavMath.round(yRatio * (yDim - 1));
        voxelZ = MipavMath.round(zRatio * (zDim - 1));
        voxelY = yDim - 1 - voxelY;
        voxelZ = zDim - 1 - voxelZ;

        // get the image intensity value
        value = imageA.getFloat(voxelX, voxelY, voxelZ);

        if ( !isEntryPoint && (value >= -75)) {
            isEntryPoint = true;

            return SurfaceView.ENTRY_POINT;
        }

        if (value >= 200) {
            isEntryPoint = true;

            return SurfaceView.BONE_SEG;
        }

        if (segmentationImage != null) {

            // get image intensity value
            value = segmentationImage.getFloat(voxelX, voxelY, voxelZ);

            if (value == SurfaceView.ARTERIAL_SEG) {
                return SurfaceView.ARTERIAL_SEG;
            } else if (value == SurfaceView.VEINOUS_SEG) {
                return SurfaceView.VEINOUS_SEG;
            } else if (value == SurfaceView.VASCULATURE_SEG) {
                isEntryPoint = true;

                return SurfaceView.VASCULATURE_SEG;
            } else if (value == SurfaceView.TUMOR_SEG) {
                return SurfaceView.TUMOR_SEG;
            }
        }

        return -1;
    }

    /**
     * Calls disposeLocal.
     * 
     * @throws Throwable DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        this.disposeLocal(false);
        super.finalize();
    }

    /**
     * Create the rotation control cubic box.
     * 
     * @return A cube representing the image orientation, with labels painted on the cube faces showing which axis
     *         corresponds to which axis in patient coordinates.
     */
    private Box buildCubicBox() {

        /* Read the axis strings from the FileInfo data structure: */
        final String[] akAxisLabels = new String[3];
        final String[][] aakAxisFiles = new String[][] { {"u", "u"}, {"u", "u"}, {"u", "u"}};

        final int[] axisOrientation = MipavCoordinateSystems.getAxisOrientation(imageA);
        if (axisOrientation != null) {
            for (int i = 0; i < 3; i++) {
                akAxisLabels[i] = FileInfoBase.getAxisOrientationStr(axisOrientation[i]).toLowerCase();
                // System.out.println(akAxisLabels[i]);
                /*
                 * The file name correspond to the axis strings, read the file names from the axis strings:
                 */
                aakAxisFiles[i][0] = new String(String.valueOf(akAxisLabels[i].charAt(0)));
                aakAxisFiles[i][1] = new String(String.valueOf(akAxisLabels[i]
                        .charAt(akAxisLabels[i].lastIndexOf(" ") + 1)));
            }
        }

        /*
         * Get the sides of the faces that correspond to the axis labels. Axis labels were read left-right, top-bottom,
         * back-front, get the cube faces in that order so the appropriate axis texture can be applied to the cube face:
         */
        Appearance app = new Appearance();
        final Box cubic = new Box(1.0f, 1.0f, 1.0f, Primitive.GENERATE_TEXTURE_COORDS, app, 1);
        final Shape3D[][] shapeCubic = new Shape3D[3][2];
        /* Box.LEFT and Box.RIGHT map to the x-axis orientation strings: */
        shapeCubic[0][0] = cubic.getShape(Box.LEFT);
        shapeCubic[0][1] = cubic.getShape(Box.RIGHT);
        /* Box.TOP and Box.BOTTOM map to the y-axis orientation strings: */
        shapeCubic[1][0] = cubic.getShape(Box.TOP);
        shapeCubic[1][1] = cubic.getShape(Box.BOTTOM);
        /* Box.BACK and Box.FRONT map to the z-axis orientation strings: */
        shapeCubic[2][1] = cubic.getShape(Box.BACK);
        shapeCubic[2][0] = cubic.getShape(Box.FRONT);

        /*
         * Loop over the sides of the cube, read the corresponding texture, and apply the texture to the cube face:
         */
        /* For each axis: */
        for (int i = 0; i < 3; i++) {
            /* For cube face along the current axis: */
            for (int j = 0; j < 2; j++) {
                /* If the orientation isn't unknown (represented by the "u" ): */
                if ( !aakAxisFiles[i][j].equals("u")) {
                    /* Load the texture: */
                    TextureLoader textLoad = null;
                    try {
                        textLoad = new TextureLoader(MipavUtil.getIconImage(aakAxisFiles[i][j] + ".gif"), this);
                    } catch (final FileNotFoundException error) {
                        Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                                + ">.  Check that this file is available.\n");
                        System.err.println("Exception ocurred while getting <" + error.getMessage()
                                + ">.  Check that this file is available.\n");
                    }
                    /* Apply the texture to the current cube face: */
                    final TextureAttributes textAttr = new TextureAttributes();
                    textAttr.setTextureMode(TextureAttributes.DECAL);
                    final TextureUnitState[] textUnitState = new TextureUnitState[1];
                    textUnitState[0] = new TextureUnitState(textLoad.getTexture(), textAttr, null);
                    textUnitState[0].setCapability(TextureUnitState.ALLOW_STATE_WRITE);
                    app = new Appearance();
                    app.setTextureUnitState(textUnitState);
                    shapeCubic[i][j].setAppearance(app);
                }
            }
        }
        /* Return the texture-mapped cube: */
        return cubic;
    }

    /**
     * Build the cubic branch under the objRootBG.
     */
    private void buildCubicBranch() {
        cubicBG = new BranchGroup();
        cubicBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        cubicBG.setCapability(Group.ALLOW_CHILDREN_READ);
        cubicBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        cubicBG.setCapability(BranchGroup.ALLOW_DETACH);
        cubicBG.setPickable(false);

        cubicTG = new TransformGroup();
        cubicTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cubicTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cubicTG.setCapability(Group.ALLOW_CHILDREN_READ);
        cubicTG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        cubicTG.setCapability(Node.ENABLE_PICK_REPORTING);

        transformCubic = new Transform3D();
        transformCubic.setScale(0.1f);
        transformCubic.setRotation(new AxisAngle4f(1, 1, 1, (float) .52));
        transformCubic.setTranslation(new Vector3f( -0.7f, -0.7f, -0.7f));
        cubicTG.setTransform(transformCubic);

        cubicBG.addChild(cubicTG);
        cubicTG.addChild(buildCubicBox());

        cubicBehaviorBG = new BranchGroup();
        cubicBehaviorBG.setCapability(BranchGroup.ALLOW_DETACH);
        cubicBehaviorBG.setCapability(Group.ALLOW_CHILDREN_READ);
        cubicBehaviorBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        cubicBehaviorBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);

        cubicRotate = new MouseRotate();
        cubicRotate.setupCallback(this);
        cubicRotate.setTransformGroup(cubicTG);
        cubicRotate.setSchedulingBounds(bounds);
        cubicBehaviorBG.addChild(cubicRotate);
        cubicTG.addChild(cubicBehaviorBG);
        cubicRotate.setFactor(0.005);

    }

    /**
     * Constructs main frame structures for 3 image planes. Makes the LUT if necessary, then sets up the buffer arrays
     * appropriately and calls the constructors for the three image planes.
     */
    private void configureSliceFrame() {

        /** Extents of image A, same as xDim, yDim, etc. */
        int[] extents;

        if (imageA == null) {
            return;
        }

        if ( ( (imageA.getNDims() == 3) && (imageB == null))
                || ( (imageA.getNDims() == 3) && (imageB != null) && (imageB.getNDims() == 3))) {
            extents = new int[3];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
        } else {
            extents = new int[4];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;

            if (imageA.getNDims() == 4) {
                extents[3] = tDim;
            } else {
                extents[3] = tDim;
            }
        }

        // if not a color image and LUTa is null then make a LUT
        if (imageA.isColorImage() == false) {
            final int[] dimExtentsLUT = new int[2];

            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            if (LUTa == null) {
                LUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

                float min, max;

                if (imageA.getType() == ModelStorageBase.UBYTE) {
                    min = 0;
                    max = 255;
                } else if (imageA.getType() == ModelStorageBase.BYTE) {
                    min = -128;
                    max = 127;
                } else {
                    min = (float) imageA.getMin();
                    max = (float) imageA.getMax();
                }

                final float imgMin = (float) imageA.getMin();
                final float imgMax = (float) imageA.getMax();

                LUTa.resetTransferLine(min, imgMin, max, imgMax);
            }

            if ( (imageB != null) && (LUTb == null)) {
                LUTb = new ModelLUT(ModelLUT.HOTMETAL, 256, dimExtentsLUT);

                float min, max;

                if (imageB.getType() == ModelStorageBase.UBYTE) {
                    min = 0;
                    max = 255;
                } else if (imageB.getType() == ModelStorageBase.BYTE) {
                    min = -128;
                    max = 127;
                } else {
                    min = (float) imageB.getMin();
                    max = (float) imageB.getMax();
                }

                final float imgMin = (float) imageB.getMin();
                final float imgMax = (float) imageB.getMax();

                LUTb.resetTransferLine(min, imgMin, max, imgMax);
            }
        }

        Preferences.debug("Preferred graphics configuration: " + config + "\n");
        canvas = new VolumeCanvas3D(config);
        Preferences.debug("Canvas: " + canvas.queryProperties() + "\n");

        boxSliceVertices = new Vector3f[3][];
        boxSliceVertices[0] = null;
        boxSliceVertices[1] = null;
        boxSliceVertices[2] = null;
    }

    /**
     * Generates the texture coord map vectors for the three orthogonal slices.
     */
    private void generateRFACoordMaps() {

        // Define vectors used for mapping each coordinate from its
        // real range X=[0,xdim-1], Y=[0,ydim-1], Z=[0,zdim-1] into
        // the (s,t,r) texture coordinates where each texture coordinate
        // is defined over the range [0,1].
        // Note that integral screen coordinates represent pixel centers
        // whereas integral texture coordinates represent texel boundaries.
        // Therefore, X maps to s=(X+0.5)/xdim, and correspondingly
        // for Y and Z mapping to t and r, respectively.
        for (int i = 0; i < 2; i++) {
            m_kRFACoordMaps[0][i] = generateRFACoordMaps(FileInfoBase.AXIAL, i);
            m_kRFACoordMaps[1][i] = generateRFACoordMaps(FileInfoBase.CORONAL, i);
            m_kRFACoordMaps[2][i] = generateRFACoordMaps(FileInfoBase.SAGITTAL, i);
        }

        /*
         * The following logic is to match TextureCoordinate Maps to the ViewJComponentBoxSlice Vertex Array:
         */
        final int[] axialOrder = MipavCoordinateSystems.getAxisOrder(imageA, FileInfoBase.AXIAL);
        final int[] coronalOrder = MipavCoordinateSystems.getAxisOrder(imageA, FileInfoBase.CORONAL);
        final int[] sagittalOrder = MipavCoordinateSystems.getAxisOrder(imageA, FileInfoBase.SAGITTAL);

        if ( (axialOrder[0] > axialOrder[1]) && (axialOrder[2] != 0)) {
            final Vector4f temp = m_kRFACoordMaps[0][0];
            m_kRFACoordMaps[0][0] = m_kRFACoordMaps[0][1];
            m_kRFACoordMaps[0][1] = temp;
        }
        if ( (coronalOrder[0] > coronalOrder[1]) && (coronalOrder[2] != 0)) {
            final Vector4f temp = m_kRFACoordMaps[1][0];
            m_kRFACoordMaps[1][0] = m_kRFACoordMaps[1][1];
            m_kRFACoordMaps[1][1] = temp;
        }
        if ( (sagittalOrder[0] > sagittalOrder[1]) && (sagittalOrder[2] != 0)) {
            final Vector4f temp = m_kRFACoordMaps[2][0];
            m_kRFACoordMaps[2][0] = m_kRFACoordMaps[2][1];
            m_kRFACoordMaps[2][1] = temp;
        }

        if ( (axialOrder[0] < axialOrder[1]) && (axialOrder[2] == 0)) {
            final Vector4f temp = m_kRFACoordMaps[0][0];
            m_kRFACoordMaps[0][0] = m_kRFACoordMaps[0][1];
            m_kRFACoordMaps[0][1] = temp;
        }
        if ( (coronalOrder[0] < coronalOrder[1]) && (coronalOrder[2] == 0)) {
            final Vector4f temp = m_kRFACoordMaps[1][0];
            m_kRFACoordMaps[1][0] = m_kRFACoordMaps[1][1];
            m_kRFACoordMaps[1][1] = temp;
        }
        if ( (sagittalOrder[0] < sagittalOrder[1]) && (sagittalOrder[2] == 0)) {
            final Vector4f temp = m_kRFACoordMaps[2][0];
            m_kRFACoordMaps[2][0] = m_kRFACoordMaps[2][1];
            m_kRFACoordMaps[2][1] = temp;
        }
    }

    /**
     * create texture coordmaps for the texture-mapped planes displaying the triSliceImages.
     * 
     * @param orientation the orientation of the plane for which the texture coordinates are generated (AXIAL, CORONAL,
     *            or SAGITTAL)
     * @param texCoord the dimension along which the texture coordinate varies
     */
    private Vector4f generateRFACoordMaps(final int orientation, final int texCoord) {
        final int[] axisOrder = MipavCoordinateSystems.getAxisOrder(imageA, orientation);
        final float[] coordMapArray = new float[3];
        coordMapArray[0] = (maxBox / xBox) / 2.0f;
        coordMapArray[1] = - (maxBox / yBox) / 2.0f;
        coordMapArray[2] = - (maxBox / zBox) / 2.0f;
        if (axisOrder[2] == 0) {
            coordMapArray[2] = (maxBox / zBox) / 2.0f;
        }
        for (int i = 0; i < 3; i++) {
            if (i != axisOrder[texCoord]) {
                coordMapArray[i] = 0f;
            }
        }

        final Vector4f coordMap = new Vector4f();
        coordMap.x = coordMapArray[0];
        coordMap.y = coordMapArray[1];
        coordMap.z = coordMapArray[2];
        coordMap.w = 0.5f;

        return coordMap;
    }

    /**
     * Creates the scene graph, made up of a branch group parent, a transform group under that which applies mouse
     * behavior and lights to the scene, and three branch groups under the transform group for each of the three image
     * planes. The surfaces that can be added would be children of the transform group. Each image plane has a transform
     * group associated with it and beneath that, a box shape where the texture maps are applied. The shape is what is
     * actually displayed.
     */
    private void createImageSceneGraph() {

        // Create a Transformgroup to scale all objects so they
        // appear in the scene.
        transformNode3d = new Transform3D();
        transformNode3d.setScale(0.45f);

        final BranchGroup pointBG = new BranchGroup();

        pointBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        pointBG.setCapability(Group.ALLOW_CHILDREN_READ);
        pointBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        pointBG.setCapability(BranchGroup.ALLOW_DETACH);

        final Appearance appearance = new Appearance();

        appearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);

        final TransparencyAttributes tap = new TransparencyAttributes();

        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
        tap.setTransparencyMode(TransparencyAttributes.BLENDED);
        tap.setTransparency(1.0f);
        appearance.setTransparencyAttributes(tap);

        final Shape3D pointShape = new Shape3D(new PointArray(1, 1), appearance);

        pointBG.addChild(pointShape);

        volRenderOG = new OrderedGroup();
        volRenderOG.setCapability(Group.ALLOW_CHILDREN_READ);
        volRenderOG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        volRenderOG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        volRenderOG.setCapability(BranchGroup.ALLOW_DETACH);
        volRenderOG.addChild(pointBG);

        volBG = new BranchGroup();
        volBG.setCapability(Group.ALLOW_CHILDREN_READ);
        volBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        volBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        volBG.setCapability(BranchGroup.ALLOW_DETACH);
        volBG.addChild(volRenderOG);

        sceneRootTG.setTransform(transformNode3d);
        sceneRootTG.setCapability(Group.ALLOW_CHILDREN_READ);
        sceneRootTG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        sceneRootTG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        sceneRootTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        sceneRootTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        sceneRootTG.setCapability(Node.ENABLE_PICK_REPORTING);
        sceneRootTG.setCapability(Node.ALLOW_LOCAL_TO_VWORLD_READ);

        // here, we switch group between triplanar view and 3D texture volume view
        switchGroup = new Switch();
        switchGroup.setWhichChild(Switch.CHILD_NONE);
        switchGroup.setCapability(Switch.ALLOW_SWITCH_WRITE);
        switchGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
        switchGroup.setCapability(Group.ALLOW_CHILDREN_READ);

        triPlanarViewBG = new BranchGroup();
        triPlanarViewBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        triPlanarViewBG.setCapability(Group.ALLOW_CHILDREN_READ);
        triPlanarViewBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        triPlanarViewBG.setCapability(BranchGroup.ALLOW_DETACH);

        switchGroup.addChild(triPlanarViewBG);
        switchGroup.addChild(volBG);
        switchGroup.setWhichChild(0);

        sceneRootTG.addChild(switchGroup);

        buildCubicBranch();

        xBox = (xDim - 1) * resols[0];
        yBox = (yDim - 1) * resols[1];
        zBox = (zDim - 1) * resols[2];
        maxBox = xBox;

        if (yBox > maxBox) {
            maxBox = yBox;
        }

        if (zBox > maxBox) {
            maxBox = zBox;
        }

        // must be called before xBox, yBox, and zBox are normalized:
        generateRFACoordMaps();

        // Normalize the size
        // xBox range between 0 - 1.
        xBox = xBox / maxBox;
        yBox = yBox / maxBox;
        zBox = zBox / maxBox;

        final int[] boxSliceConstants = new int[3];
        final Vector3f[] screenBoxPoints = new Vector3f[3];
        for (int i = 0; i < 3; i++) {
            screenBoxPoints[i] = new Vector3f();
        }
        getBoxSliceInScreen(screenBoxPoints, boxSliceConstants);

        final Color[] sliceColors = new Color[3];
        sliceColors[0] = Color.red;
        sliceColors[1] = Color.green;
        sliceColors[2] = Color.yellow;

        for (int i = 0; i < 3; i++) {

            boxSlices[i] = new ViewJComponentBoxSlice(screenBoxPoints[i].x, screenBoxPoints[i].y, screenBoxPoints[i].z,
                    boxSliceConstants[i]);
            boxSlices[i].setColor(sliceColors[i]);

            objPlane_BG[i] = new BranchGroup();
            objPlane_BG[i].setCapability(BranchGroup.ALLOW_DETACH);
            triPlanarViewBG.addChild(objPlane_BG[i]);

            objTransSlices_TG[i] = new TransformGroup();
            objTransSlices_TG[i].setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
            objTransSlices_TG[i].setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
            objTransSlices_TG[i].setCapability(Node.ENABLE_PICK_REPORTING);

            objPlane_BG[i].setPickable(false);

            m_kObjPlaneProbe_TG[i] = new TransformGroup();
            m_kObjPlaneProbe_TG[i].setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
            m_kObjPlaneProbe_TG[i].setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
            m_kObjPlaneProbe_TG[i].setCapability(Group.ALLOW_CHILDREN_READ);
            m_kObjPlaneProbe_TG[i].setCapability(Group.ALLOW_CHILDREN_WRITE);
            m_kObjPlaneProbe_TG[i].addChild(objTransSlices_TG[i]);
            objPlane_BG[i].addChild(m_kObjPlaneProbe_TG[i]);

        }

        // Creates a bounding frame around the view
        createBoxFrame(xBox, yBox, zBox);

        objBehaviorBG = new BranchGroup();
        objBehaviorBG.setCapability(BranchGroup.ALLOW_DETACH);
        objBehaviorBG.setCapability(Group.ALLOW_CHILDREN_READ);
        objBehaviorBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objBehaviorBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);

        // Create the rotate behavior node
        mouseRotateBehavior = new MouseRotateExt(canvas, sceneRootTG);
        objBehaviorBG.addChild(mouseRotateBehavior);
        mouseRotateBehavior.setSchedulingBounds(bounds);
        mouseRotateBehavior.setupCallback(this);
        mouseRotateBehavior.setFactor(0.005);

        // Create the zoom behavior node
        mouseZoomBehavior = new MouseZoom(canvas, sceneRootTG);
        objBehaviorBG.addChild(mouseZoomBehavior);
        mouseZoomBehavior.setupCallback(this);
        mouseZoomBehavior.setSchedulingBounds(bounds);
        mouseZoomBehavior.setFactor(0.005);

        // Create the translate behavior node
        mouseTranslateBehavior = new MouseTranslate(canvas, sceneRootTG);
        objBehaviorBG.addChild(mouseTranslateBehavior);
        mouseTranslateBehavior.setupCallback(this);
        mouseTranslateBehavior.setSchedulingBounds(bounds);
        mouseTranslateBehavior.setFactor(0.005);
        sceneRootTG.addChild(objBehaviorBG);

    }

    /**
     * Set up the center point of the red line boxframe.
     */
    private void setupEye() {
        Shape3D shape;

        shape = new Shape3D(boxFrame, null);

        final BoundingSphere kBounds = new BoundingSphere(shape.getBounds());

        final Point3d kVolumeCenterPoint = new Point3d();

        kBounds.getCenter(kVolumeCenterPoint);

        myEyePoint.x = kVolumeCenterPoint.x;
        myEyePoint.y = kVolumeCenterPoint.y;
        myEyePoint.z = kVolumeCenterPoint.z + kBounds.getRadius();
    }

    /**
     * Determine the current viewing transformation and pass it to the texture volume renderer so that it can update
     * itself.
     */
    private void updateTextureVolumeRender() {

    }

    /**
     * This function calculates the scale factor for zooming in parallel projection. The scenario is to calculate the
     * distance between the origin boxframe center and tranformed boxframe center. This distance is used to compute the
     * screen scale factor.
     * 
     * @param kTransform The tranformation matrix from tranformChanged().
     */
    private void updateViewScreenScale(final Transform3D kTransform) {
        parallelScaleT3D = kTransform;

        // The boxframe center distance is acutally the bounding sphere
        // center distance.
        Shape3D shape;

        shape = new Shape3D(boxFrame, null);

        final BoundingSphere kBounds = new BoundingSphere(shape.getBounds());

        kBounds.transform(kBounds, kTransform);

        final Point3d kVolumeCenterPoint = new Point3d();

        kBounds.getCenter(kVolumeCenterPoint);

        final Vector3d kViewVector = new Vector3d();

        kViewVector.sub(myEyePoint, kVolumeCenterPoint);

        final double dDist = Math.abs(kViewVector.z);

        final View kView = universe.getViewer().getView();
        final double dFieldOfView = kView.getFieldOfView();
        double dViewWidth = 15.0f * dDist * Math.tan(dFieldOfView / 15.0f);

        final Screen3D kScreen = canvas.getScreen3D();

        kView.setScreenScale(kScreen.getPhysicalScreenWidth() / dViewWidth);

        /* Calculate and store the original screenscale: */
        dViewWidth = 15.0f * Math.tan(dFieldOfView / 15.0f);
        m_dOriginalScreenScale = kScreen.getPhysicalScreenWidth() / dViewWidth;
    }

    public void updateParent() {
        getParentFrame().updateImages(true);
    }

}
