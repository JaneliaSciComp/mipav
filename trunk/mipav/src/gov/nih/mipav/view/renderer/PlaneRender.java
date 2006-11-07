package gov.nih.mipav.view.renderer;


import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.surfaceview.rfaview.mouse.*;

import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.universe.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import java.io.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Class PlaneRender: renders a single dimension of the ModelImage data as a
 * texture-mapped polygon. The PlaneRender class keeps track of whether it is
 * rendering the Axial, Sagital, or Coronal view of the data.
 *
 * Surfaces are displayed as the intersection of the ModelTriangleMesh with
 * the rendered z-slice. 
 */
public class PlaneRender extends VolumeCanvas3D implements MouseMotionListener, MouseListener, MouseBehaviorCallback {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2025132936439496099L;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    /** The image dimensions in x,y,z:. */
    private int[] m_aiLocalImageExtents;

    /** Image component 2D. Used to store the ModelImage data as textures. */
    private ImageComponent2D[] m_akImageComponent;

    /** Set of colors used to draw the X and Y Bars and the Z box:. */
    private Color3f[] m_akColors = { new Color3f(1, 0, 0), new Color3f(0, 1, 0), new Color3f(1, 1, 0) };

    /** when true, the axis labels (P-> L-> S->) will be drawn */
    private boolean m_bDrawAxes = true;

    /** Turns on drawing of the X,Y bars and the Axis labels:. */
    private boolean m_bDrawXHairs = true;

    /** Boolean to turn on/off the RFA probe entry point selection with mouse:. */
    private boolean m_bEntryPointSelect = false;

    /** Change the mouse cursor with the first mouseDrag event */
    private boolean m_bFirstDrag = true;

    /** True when the left mouse has been pressed, set to false when the left
     * mouse button is released. */
    private boolean m_bLeftMousePressed = false;

    /** Whether to store all the data in ImageComponent2D array or not:. */
    private boolean m_bMemoryUsage;

    /** Actual image orietation. */
    private boolean m_bPatientOrientation = true;

    /** Flag indicating if the right mouse button is currently pressed
     * down: */
    private boolean m_bRightMousePressed = false;

    /** The center of the X,Y bar cross hairs, in plane coordinates:. */
    private float m_fCenterX, m_fCenterY;

    /** The current active lookup table: */
    private ModelStorageBase m_kActiveLookupTable;

    /** lower x-bound of the texture-mapped polygon: */
    private float m_fX0;

    /** upper x-bound of the texture-mapped polygon: */
    private float m_fX1;

    /** Numbers dicatating the size of the plane based on the extents and
     * resolutions of the image. */
    private float m_fXBox, m_fYBox, m_fMaxBox;

    /** Width of the texture-mapped polygon: */
    private float m_fXRange;

    /** X direction mouse translation. */
    private float m_fXTranslate = 0.0f;

    /** lower y-bound of the texture-mapped polygon: */
    private float m_fY0;

    /** upper y-bound of the texture-mapped polygon: */
    private float m_fY1;

    /** Height of the texture-mapped polygon: */
    private float m_fYRange;

    /** Y direction mouse translatioin. */
    private float m_fYTranslate = 0.0f;

    /** Image scaling from Zoom:. */
    private float m_fZoomScale = 1.0f;

    /** Which dimension of the ModelImage to render. */
    private int m_iPlaneOrientation = 0;

    /** Which slice is currently displayed in the XY plane. */
    private int m_iSlice;

    /** Current active image for manipulating the LUT by dragging with the
     * right-mouse down. */
    private ModelImage m_kActiveImage;

    /** The Canvas3D object on which the plane is drawn. */
    private VolumeCanvas3D m_kCanvas;

    /** current zoom transformation */
    private Transform3D m_kCurrentTransform;

    /** The current displayed texture, based on the value of m_iSlice. */
    private ImageComponent2D m_kDisplayedImage;

    /** Current image A. */
    private ModelImage m_kImageA;

    /** Current image B. */
    private ModelImage m_kImageB;

    /** x-axis label: */
    private String m_kLabelX = new String("X");

    /** y-axis label: */
    private String m_kLabelY = new String("Y");

    /** Mouse translate behavior. */
    private MouseTranslation m_kMouseTranslate;

    /** Mouse zoom behavior. */
    private MouseZoomBehavior m_kMouseZoom;

    /**
     * The BranchGroup root of the scene managed by the simple universe. The
     * root has a single child, a TransformGroup, that manages all of the
     * actual objects.
     */
    private BranchGroup m_kObjRootBG;

    /** Root order group of the image scene graph. */
    private OrderedGroup m_kOrderedGroup;

    /** Data members for the PlaneRender class:. */

    /** Reference to the parent frame:. */
    private ViewJFrameVolumeView m_kParent;

    /** Branch group for the RFA indicator point. */
    private BranchGroup m_kRFA_BranchGroup = null;

    /** Branch group to hold the texture image. */
    private BranchGroup m_kTextBranchGroup;

    /** Transform group to hold the texture images. */
    private TransformGroup m_kTextTransformGroup;

    /** The 2D texture for the texture-mapped polygon. */
    private Texture2D m_kTexture;

    /** Group dictating how the plane is translated. */
    private TransformGroup m_kTranslationTG;

    /** The SimpleUniverse object which is the parent of everything else in the scene. */
    private SimpleUniverse m_kUniverse;

    /** PatientSlice contains all the Patient Coordinate System view-specific
     * data for rendering this component: */
    private PatientSlice m_kPatientSlice;

    private WindowLevel m_kWinLevel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new PlaneRender object.
     *
     * @param  kParent  ViewJFrameVolumeView - reference to parent frame.
     * @param  kImageA  First image to display, cannot be null.
     * @param  kLUTa    LUT of the imageA (if null grayscale LUT is constructed).
     * @param  kImageB  Second loaded image, may be null.
     * @param  kLUTb    LUT of the imageB, may be null.
     * @param  kConfig  GraphicsConfiguration
     * @param  iPlane   Image dimension to be displayed.
     * @param bMemory when true store all the data in memory, when false,
     * write textues as the slices change
     */
    public PlaneRender(ViewJFrameVolumeView kParent,
                       ModelImage kImageA, ModelLUT kLUTa,
                       ModelImage kImageB, ModelLUT kLUTb,
                       GraphicsConfiguration kConfig, int iPlane, boolean bMemory)
    {
        super(kConfig);
        m_kParent = kParent;
        m_iPlaneOrientation = iPlane;
        m_bMemoryUsage = bMemory;

        m_kImageA = kImageA;
        m_kImageB = kImageB;
        m_kImageA.setImageOrder(ModelImage.IMAGE_A);

        if (m_kImageB != null) {
            m_kImageB.setImageOrder(ModelImage.IMAGE_B);
        }

        setOrientation();

        /* Set up the canvas: */
        m_kCanvas = new VolumeCanvas3D(kConfig);
        m_kCanvas.addMouseMotionListener(this);
        m_kCanvas.addMouseListener(this);
        m_kCanvas.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        m_kCanvas.setBackground(Color.white);

        m_kPatientSlice = new PatientSlice( m_kImageA, kLUTa,
                                            m_kImageB, kLUTb,
                                            m_iPlaneOrientation );

        m_kWinLevel = new WindowLevel();

        /* Create the scene graph and initialize the rendering */
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes the frame.
     */
    public void close() {
        disposeLocal();
    }

    /**
     * Clean memory.
     */
    public void disposeLocal() {
        m_kImageA = null;
        m_kImageB = null;

        for (int i = 0; i < 3; i++) {
            m_akColors[i] = null;
        }

        m_kLabelX = null;
        m_kLabelY = null;

        m_kCanvas = null;

        if (m_kUniverse != null) {
            m_kUniverse.removeAllLocales();
            m_kUniverse = null;
        }

        m_kCurrentTransform = null;
        m_kObjRootBG = null;
        m_kOrderedGroup = null;

        m_kTranslationTG = null;

        if (m_bMemoryUsage) {

            for (int iZ = 0; iZ < m_aiLocalImageExtents[2]; iZ++) {
                m_akImageComponent[iZ] = null;
            }

            m_akImageComponent = null;
        }

        m_kDisplayedImage = null;
        m_kTexture = null;

        m_kMouseZoom = null;
        m_kMouseTranslate = null;

        m_kTextBranchGroup = null;
        m_kTextTransformGroup = null;

        if (m_kRFA_BranchGroup != null) {
            m_kRFA_BranchGroup = null;
        }
    }

    /**
     * Given a point in FileCoordinates, draw it in local PatientCoordinates
     * with a red sphere:
     *
     * @param  kPoint  RFA indicator point coordinate
     */
    public void drawRFAPoint(Point3f kPoint) {

        Point3Df patientPt = new Point3Df();
        MipavCoordinateSystems.FileToPatient( new Point3Df( kPoint.x,
                                                            kPoint.y,
                                                            kPoint.z ),
                                              patientPt, m_kImageA,
                                              m_iPlaneOrientation );
        Point3Df quadPt = new Point3Df();
        this.PatientToQuad( patientPt, quadPt );

        /* If this is the first time drawing, create the BranchGroup to hold
         * the sphere representation: */
        if (m_kRFA_BranchGroup == null) {
            Shape3D kSphere = new Sphere(0.025f).getShape();

            kSphere.getAppearance().
                getMaterial().setEmissiveColor(new Color3f(1f, 0f, 0f));
            kSphere.setPickable(false);

            Transform3D kTransform = new Transform3D();

            kTransform.set(new Vector3f(quadPt.x, quadPt.y, 0.05f));

            TransformGroup kTransformGroup = new TransformGroup();

            kTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
            kTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
            kTransformGroup.setTransform(kTransform);
            kTransformGroup.addChild(kSphere.cloneTree());
            m_kRFA_BranchGroup = new BranchGroup();
            m_kRFA_BranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
            m_kRFA_BranchGroup.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
            m_kRFA_BranchGroup.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
            m_kRFA_BranchGroup.addChild(kTransformGroup);
            m_kRFA_BranchGroup.compile();
            m_kOrderedGroup.addChild(m_kRFA_BranchGroup);
        } /* Otherwise, update the position of the existing sphere: */
        else {
            Transform3D kTransform = new Transform3D();

            kTransform.set(new Vector3f(quadPt.x, quadPt.y, quadPt.z));

            TransformGroup kTransformGroup =
                (TransformGroup) (m_kRFA_BranchGroup.getChild(0));

            kTransformGroup.setTransform(kTransform);
        }
    }

    /**
     * Enable or disable target point for the RFA probe from within the plane
     * renderer:
     *
     * @param  bEnable  true enable target point, false not.
     */
    public void enableTargetPointPicking(boolean bEnable) {

        if (m_bEntryPointSelect == bEnable) {
            return;
        }

        m_bEntryPointSelect = bEnable;

        /* If point selection is disabled, then detach the display group: */
        if (!bEnable && (m_kRFA_BranchGroup != null)) {
            m_kOrderedGroup.removeChild(m_kRFA_BranchGroup);
        } /* If point selection is enabled, then draw the display group: */
        else if (bEnable && (m_kRFA_BranchGroup != null)) {
            m_kOrderedGroup.addChild(m_kRFA_BranchGroup);
        }
    }

    /**
     * Returns the VolumeCanvas3D object.
     *
     * @return  the volume canvas
     */
    public VolumeCanvas3D getCanvas() {
        return m_kCanvas;
    }

    /**
     * Accessor that returns the reference to image A.
     *
     * @return  Image A.
     */
    public ModelImage getImageA() {
        return m_kImageA;
    }

    /**
     * Accessor that returns the reference to image B.
     *
     * @return  Image B.
     */
    public ModelImage getImageB() {
        return m_kImageB;
    }


    /**
     * One of the overrides necessary to be a MouseListener. This function is
     * called when there is a double-click event.
     *
     * @param  kEvent  the mouse event generated by a mouse clicked
     */
    public void mouseClicked(MouseEvent kEvent) {}

    /**
     * One of the overrides necessary to be a MouseListener. This function is
     * invoked when a mouse button is held down and the mouse is dragged in
     * the active window area.
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    public void mouseDragged(MouseEvent kEvent) {

        /* If the right mouse button is pressed and
         * dragged. processRightMouseDrag updates the HistoLUT window and
         * level (contrast and brightness) */
        if (m_bRightMousePressed && !kEvent.isShiftDown()) {
            processRightMouseDrag(kEvent);
        }

        /* Dragging the mouse with the left-mouse button held down changes the
         * positions of the X and Y cross bars, and therefor the ZSlice positions of the associated PlaneRender objects
         * and the TriPlanar Surface. The new positions are calculated and passed onto the parent frame.
         */
        if (m_bLeftMousePressed && !kEvent.isShiftDown()) {
            processLeftMouseDrag(kEvent);
        }
    }

    /**
     * One of the overrides necessary to be a MouseListener. This function is
     * called when the mouse enters the active area.
     *
     * @param  kEvent  the mouse event generated by a mouse entered
     */
    public void mouseEntered(MouseEvent kEvent) {}

    /**
     * One of the overrides necessary to be a mouselistener. This function is
     * called when the mouse leaves the active area.
     *
     * @param  kEvent  the mouse event generated by a mouse exit
     */
    public void mouseExited(MouseEvent kEvent) {}

    /**
     * One of the overrides necessary to be a MouseMotionListener. This
     * function is called when the mouse is moved (without holding any buttons
     * down).
     *
     * @param  kEvent  the event generated by a mouse movement
     */
    public void mouseMoved(MouseEvent kEvent) {}

    /**
     * One of the overrides necessary to be a MouseListener.
     *
     * <p>If the left mouse button is pressed, the function sets the
     * m_bLeftMousePressed to be true, and records the current canvas width
     * and height.</p>
     *
     * @param  kEvent  the mouse event generated by a mouse press
     */
    public void mousePressed(MouseEvent kEvent) {

        /* If the button pressed is the left mouse button: */
        if ((kEvent.getButton() == MouseEvent.BUTTON1) && !kEvent.isShiftDown()) {
            m_bLeftMousePressed = true;
        }

        if ((kEvent.getButton() == MouseEvent.BUTTON3) && !kEvent.isShiftDown()) {
            m_bRightMousePressed = true;
        }
    }

    /**
     * One of the overrides necessary to be a MouseListener.
     *
     * @param  kEvent  the mouse event generated by a mouse release
     */
    public void mouseReleased(MouseEvent kEvent) {

        /* If the button pressed is the left mouse button turn off the
         * m_bLeftMousePressed flag: */
        if ((kEvent.getButton() == MouseEvent.BUTTON3) && !kEvent.isShiftDown()) {
            if ( m_kActiveImage.isColorImage() )
            {
                m_kActiveImage.
                    notifyImageDisplayListeners(false, 0,
                                                (ModelRGB)m_kActiveLookupTable);
            }
            else
            {
                m_kActiveImage.
                    notifyImageDisplayListeners((ModelLUT)m_kActiveLookupTable, false);
            }
            m_bRightMousePressed = false;
        }

        if ((kEvent.getButton() == MouseEvent.BUTTON1) && !kEvent.isShiftDown()) {
            processLeftMouseDrag( kEvent );
            m_bLeftMousePressed = false;

            /* If the RFA probe point is being set by the mouse, then
             * calculate the mouse position in FileCoordinates and pass the
             * information to the parent class: */
            if (m_bEntryPointSelect) {

                /* Calculate the center of the mouse in QUAD coordineates, taking
                 * into account zoom and translate: */
                Point3Df quadPt = new Point3Df();
                this.MouseToQuad(kEvent.getX(), kEvent.getY(), quadPt, false);
                Point3Df patientPt = new Point3Df();
                this.QuadToPatient( quadPt, patientPt );
                Point3Df kRFAPoint = new Point3Df();
                MipavCoordinateSystems.PatientToFile(patientPt, kRFAPoint, m_kImageA,
                                                     m_iPlaneOrientation );
                /* Tell the parent to draw the RFA point: */
                m_kParent.
                    drawRFAPoint( new Point3f( kRFAPoint.x, kRFAPoint.y, kRFAPoint.z ) );
            }
        }
        m_bFirstDrag = true;
        m_kCanvas.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
    }

    /**
     * Sets the background color for the frame and rendered image.
     *
     * @param  color  RGBA color to use as the background color.
     */
    public void setBackgroundColor(Color color) {
        m_kCanvas.setBackground(color);

        return;
    }

    /**
     * Accessor that sets the LUT for image A.
     *
     * @param  LUT  The LUT.
     */
    public void setLUTa(ModelLUT LUT)
    {
        m_kPatientSlice.setLUTa( LUT );
        writeTexture( true );
    }

    /**
     * Accessor that sets the LUT for image B.
     *
     * @param  LUT  The LUT.
     */
    public void setLUTb(ModelLUT LUT)
    {
        m_kPatientSlice.setLUTb( LUT );
        writeTexture( true );
    }

    /**
     * Accessor that sets the RGB lookup table for image A.
     *
     * @param  RGBT  the new ModelRGB for PatientSlice.imageA
     */
    public void setRGBTA(ModelRGB RGBT)
    {
        m_kPatientSlice.setRGBTA( RGBT );
        writeTexture( true );
    }

    /**
     * Accessor that sets the RGB lookup table for image B.
     *
     * @param  RGBT  the new ModelRGB for PatientSlice.imageB
     */
    public void setRGBTB(ModelRGB RGBT)
    {
        m_kPatientSlice.setRGBTB( RGBT );
        writeTexture( true );
    }

    /**
     * Changes the displayed texture based on the new value for m_iSlice.
     *
     * @param fSlice The relative position along the actual
     * m_aiLocalImageExtents[2] dimension of the new slice.
     */
    public void setSlice(float fSlice) {
        int iSlice = (int)fSlice;

        /* Check bounds: */
        if (iSlice > (m_aiLocalImageExtents[2] - 1)) {
            iSlice = m_aiLocalImageExtents[2] - 1;
        }

        if (iSlice < 0) {
            iSlice = 0;
        }

        if (iSlice != m_iSlice) {
            m_iSlice = iSlice;
            /* Set the new texture image: */
            if (m_bMemoryUsage) {
                m_kDisplayedImage.set(m_akImageComponent[m_iSlice].getImage());
            } else {
                writeTexture( true );
            }
        }
    }

    /**
     * Sets the new location of the XBar.
     *
     * @param  fSlice  The new position of the XBar in plane coordinates:
     */
    private void setXBar(float fSlice) {
        fSlice = fSlice / (m_aiLocalImageExtents[0] - 1);
        m_fCenterX = (fSlice * (m_fX1 - m_fX0)) + m_fX0;
    }

    /**
     * Sets the default color for the SliceHairColor.
     *
     * @param  kColor  set the hair color to this color
     */
    public void setSliceHairColor(int iView, Color3f kColor) {
        m_akColors[iView] = kColor;
        initAxes();
        drawLabels();
    }

    /**
     * Sets the new location of the YBar.
     *
     * @param  fSlice  The new position of the YBar in plane coordinates:
     */
    private void setYBar(float fSlice) {
        fSlice = fSlice / (m_aiLocalImageExtents[1] - 1);
        m_fCenterY = (fSlice * (m_fY1 - m_fY0)) + m_fY0;
    }


    /**
     * Turns displaying the Axis labes on or off:
     *
     * @param bShow when true display the axis lables, when false hide the
     * axis labels
     */
    public void showAxes(boolean bShow) {
        if (m_bDrawAxes != bShow) {
            m_bDrawAxes = bShow;
            initAxes();
        }
    }

    /**
     * Turns displaying the X and Y bars on or off:
     *
     * @param  bShow when true show the cross-hairs when false hide the cross-hairs
     */
    public void showXHairs(boolean bShow) {
        m_bDrawXHairs = bShow;
    }

    /**
     * Tells the mouse dialog that the transform has changed.
     *
     * @param  type       Type of transform.
     * @param  transform  Transform that was changed to.
     */
    public void transformChanged(int type, Transform3D transform) {

        /* The View is set to parallel, so zooming changes the
         * View.ScreenScale value, calculated by calling
         * updateViewScreenScale: */
        if (MouseBehaviorCallback.ZOOM == type) {
            updateViewScreenScale(transform);

            /* Undo zoom for the Axes so the remain in place: */
            float fXTrans = m_fX0 * 0.85f / m_fZoomScale;
            float fYTrans = m_fY1 * 0.85f / m_fZoomScale;
            if (m_bPatientOrientation &&
                (m_iPlaneOrientation != FileInfoBase.AXIAL)) {
                fYTrans = -fYTrans;
            }

            Transform3D kTextTransform = new Transform3D();
            kTextTransform.setTranslation(new Vector3f(fXTrans, fYTrans, -2.5f));
            kTextTransform.setScale(0.01f / m_fZoomScale);
            m_kTextTransformGroup.setTransform(kTextTransform);
        }

        /* Translation: */
        if (MouseBehaviorCallback.TRANSLATE == type) {

            /* Keep track of the x,y translation for mouse movements and for
             * when a surface is added to the scene: */
            Matrix4d kMatrixZoomTranslate = new Matrix4d();

            transform.get(kMatrixZoomTranslate);
            m_fXTranslate = (float) (kMatrixZoomTranslate.m03);
            m_fYTranslate = (float) (kMatrixZoomTranslate.m13);
        }
    }

    /**
     * Causes the labels to be redrawn:.
     */
    public void update() {
        initAxes();
        drawLabels();
        m_kCanvas.paint(m_kCanvas.getGraphics());
    }

    /**
     * Causes new data to be loaded from the ModelImage into the textures and
     * redisplayed on the texture-mapped polygon:
     */
    public void updateData() {
        writeTexture( true );
        update();
    }

    /**
     * Causes the data to be redrawn with new LUT values:
     *
     * @param  LUTa  imageA LUT
     * @param  LUTb  imageB LUT
     */
    public void updateLut(ModelLUT LUTa, ModelLUT LUTb) {
        m_kPatientSlice.setLUTa( LUTa );
        m_kPatientSlice.setLUTb( LUTb );
        writeTexture( true );
    }

    /**
     * Causes the data to be redrawn with new RGBTA values:
     *
     * @param  RGBT  imageA ModelRGB
     */
    public void updateRGBTA(ModelRGB RGBT) {
        m_kPatientSlice.setRGBTA( RGBT );
        writeTexture( true );
    }

    /**
     * Causes the data to be redrawn with new RGBTA values:
     *
     * @param  RGBT  imageB ModelRGB
     */
    public void updateRGBTB(ModelRGB RGBT) {
        m_kPatientSlice.setRGBTB( RGBT );
        writeTexture( true );
    }

    /**
     * Cleans memory.
     *
     * @throws  Throwable  if there is a problem encountered during memory clean-up
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Creates the scene graph, made up of a branch group parent, a transform
     * group under that which applies mouse behaviors to the scene, and a
     * branch groups under the transform group for the texture-mapped polygon.
     */
    private void createImageSceneGraph() {

        m_kCurrentTransform = new Transform3D();
        m_kCurrentTransform.setTranslation(new Vector3f(0f, 0f, -2.5f));

        /* Initialize the Root Branch Group*/
        m_kObjRootBG = new BranchGroup();
        m_kObjRootBG.setCapability(BranchGroup.ALLOW_DETACH);
        m_kObjRootBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        m_kObjRootBG.setCapability(Group.ALLOW_CHILDREN_READ);
        m_kObjRootBG.setCapability(Group.ALLOW_CHILDREN_WRITE);

        m_kOrderedGroup = new OrderedGroup();
        m_kOrderedGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        m_kOrderedGroup.setCapability(Group.ALLOW_CHILDREN_READ);
        m_kOrderedGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);

        m_kObjRootBG.addChild(m_kOrderedGroup);

        Background kBackground = new Background(new Color3f(Color.black));
        BoundingSphere kBounds = new BoundingSphere(new Point3d(0.0f, 0.0f, 0.0f), 100.0f);

        kBackground.setApplicationBounds(kBounds);
        m_kOrderedGroup.addChild(kBackground);

        /* Initialize the Zoom/Translate Transform Group: */
        m_kTranslationTG = new TransformGroup();
        m_kTranslationTG.setTransform(m_kCurrentTransform);
        m_kTranslationTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        m_kTranslationTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        m_kTranslationTG.setCapability(TransformGroup.ALLOW_CHILDREN_WRITE);
        m_kTranslationTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        m_kTranslationTG.setCapability(TransformGroup.ALLOW_CHILDREN_EXTEND);
        m_kOrderedGroup.addChild(m_kTranslationTG);

        /* Setup appearance attributes common for the texture mapped polygon. */
        Appearance kDefaultAppearance = new Appearance();
        RenderingAttributes kRenderingAttributes = new RenderingAttributes();

        kRenderingAttributes.setAlphaTestValue(0.0f);
        kRenderingAttributes.setAlphaTestFunction(RenderingAttributes.GREATER);
        kDefaultAppearance.setRenderingAttributes(kRenderingAttributes);

        // Default appearance: (PolygonAttributes)
        PolygonAttributes kPolygonAttributes = new PolygonAttributes();

        kPolygonAttributes.setCapability(PolygonAttributes.ALLOW_OFFSET_WRITE);
        kDefaultAppearance.setPolygonAttributes(kPolygonAttributes);

        // Default appearance: (Material)
        // Disable lighting so that the color information comes from
        // the texture maps.
        Material kMaterial = new Material();

        kMaterial.setLightingEnable(false);
        kDefaultAppearance.setMaterial(kMaterial);

        // Default appearance: (TransparencyAttributes)
        // Use blended transparency mode which has the default blending
        // operation set to alpha_src*src + (1-alpha_src)*dst.  This works
        // only for back to front ordering.
        TransparencyAttributes kTransparencyAttr = new TransparencyAttributes();

        kDefaultAppearance.setTransparencyAttributes(kTransparencyAttr);
        kTransparencyAttr.setTransparency(1.0f);
        kTransparencyAttr.setTransparencyMode(TransparencyAttributes.BLENDED);

        // Default appearance: (TextureAttributes)
        // Use Replace mode because we don't want to have to worry about
        // what color the slice is rendered as before the texture is
        // applied to it.
        TextureAttributes kTextureAttr = new TextureAttributes();

        kDefaultAppearance.setTextureAttributes(kTextureAttr);
        kTextureAttr.setTextureMode(TextureAttributes.REPLACE);

        // Default appearance: (Texture)
        // Allow the Texture attribute to be modified.
        kDefaultAppearance.setCapability(Appearance.ALLOW_TEXTURE_WRITE);

        // Default appearance: (TexCoordGeneration)
        // Allow the TexCoordGeneration attribute to be modified.
        kDefaultAppearance.setCapability(Appearance.ALLOW_TEXGEN_WRITE);

        QuadArray kGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.TEXTURE_COORDINATE_2);

        /* Setup the normalized polygon coordinates and texture coordinates:*/

        m_fX0 = -m_fXBox / m_fMaxBox;
        m_fX1 = m_fXBox / m_fMaxBox;
        m_fY0 = -m_fYBox / m_fMaxBox;
        m_fY1 = m_fYBox / m_fMaxBox;

        m_fXRange = m_fX1 - m_fX0;
        m_fYRange = m_fY1 - m_fY0;

        kGeometry.setCoordinate(0, new Point3d(m_fX0, m_fY0, 0));
        kGeometry.setCoordinate(1, new Point3d(m_fX1, m_fY0, 0));
        kGeometry.setCoordinate(2, new Point3d(m_fX1, m_fY1, 0));
        kGeometry.setCoordinate(3, new Point3d(m_fX0, m_fY1, 0));

        Vector4f kCoordMapX = new Vector4f((m_fMaxBox / m_fXBox) / 2.0f, 0.0f, 0.0f, 0.5f);
        Vector4f kCoordMapY = new Vector4f(0.0f, (m_fMaxBox / m_fYBox) / 2.0f, 0.0f, 0.5f);

        TexCoordGeneration kTexCoordGeneration = new TexCoordGeneration(TexCoordGeneration.OBJECT_LINEAR,
                                                                        TexCoordGeneration.TEXTURE_COORDINATE_2,
                                                                        kCoordMapX, kCoordMapY);

        /* The initial display is the middle of the range: */
        m_iSlice = (m_aiLocalImageExtents[2] - 1) / 2;
        m_kDisplayedImage = new ImageComponent2D(ImageComponent.FORMAT_RGBA, m_aiLocalImageExtents[0], m_aiLocalImageExtents[1]);
        m_kDisplayedImage.setCapability(ImageComponent2D.ALLOW_IMAGE_WRITE);
        m_kDisplayedImage.setCapability(ImageComponent2D.ALLOW_IMAGE_READ);
        m_kDisplayedImage.setCapability(ImageComponent2D.ALLOW_SIZE_READ);
        m_kDisplayedImage.setCapability(ImageComponent2D.ALLOW_FORMAT_READ);

        writeTexture( true );

        m_kTexture = new Texture2D(Texture.BASE_LEVEL, Texture.RGBA, m_aiLocalImageExtents[0], m_aiLocalImageExtents[1]);
        m_kTexture.setEnable(true);
        m_kTexture.setMinFilter(Texture.BASE_LEVEL_LINEAR);
        m_kTexture.setMagFilter(Texture.BASE_LEVEL_LINEAR);
        m_kTexture.setBoundaryModeS(Texture.CLAMP_TO_EDGE);
        m_kTexture.setBoundaryModeT(Texture.CLAMP_TO_EDGE);
        m_kTexture.setImage(0, m_kDisplayedImage);
        m_kTexture.setCapability(Texture2D.ALLOW_IMAGE_WRITE);
        m_kTexture.setCapability(Texture2D.ALLOW_IMAGE_READ);
        m_kTexture.setCapability(Texture2D.ALLOW_ENABLE_WRITE);

        kDefaultAppearance.setTexture(m_kTexture);
        kDefaultAppearance.setTexCoordGeneration(kTexCoordGeneration);

        // Create the shape (geometry+appearance) for this slice.
        // Allow the appearance to be read.
        Shape3D kShape = new Shape3D(kGeometry, kDefaultAppearance);

        m_kTranslationTG.addChild(kShape);

        /* Setup the MouseZoom behavior: */
        m_kMouseZoom = new MouseZoomBehavior(m_kTranslationTG);
        m_kMouseZoom.setupCallback(this);
        m_kMouseZoom.setSchedulingBounds(kBounds);
        m_kMouseZoom.setFactor(0.005);
        m_kTranslationTG.addChild(m_kMouseZoom);

        /* Setup the MouseTranslate behavior: */
        m_kMouseTranslate = new MouseTranslation(m_kTranslationTG);
        m_kMouseTranslate.setupCallback(this);
        m_kMouseTranslate.setSchedulingBounds(kBounds);
        m_kMouseTranslate.setFactor(0.005);
        m_kTranslationTG.addChild(m_kMouseTranslate);

        /* Center and draw the X and Y bars: */
        m_fCenterX = (m_fX0 + m_fX1) / 2.0f;
        m_fCenterY = (m_fY0 + m_fY1) / 2.0f;
        drawLabels();
    }

    /**
     * Draws the Z box, the X bar and the Y bar:.
     */
    private void drawLabels() {

        for (int iNode = 3; iNode < m_kTranslationTG.numChildren(); iNode++) {
            m_kTranslationTG.removeChild(iNode);
        }

        LineArray kBox = new LineArray(8, GeometryArray.COORDINATES | GeometryArray.COLOR_3);

        kBox.setCoordinate(0, new Point3d(m_fX0, m_fY0, 0.1));
        kBox.setColor(0, m_akColors[ m_iPlaneOrientation ] );
        kBox.setCoordinate(1, new Point3d(m_fX1, m_fY0, 0.1));
        kBox.setColor(1, m_akColors[ m_iPlaneOrientation ] );
        kBox.setCoordinate(2, new Point3d(m_fX1, m_fY0, 0.1));
        kBox.setColor(2, m_akColors[ m_iPlaneOrientation ] );
        kBox.setCoordinate(3, new Point3d(m_fX1, m_fY1, 0.1));
        kBox.setColor(3, m_akColors[ m_iPlaneOrientation ] );
        kBox.setCoordinate(4, new Point3d(m_fX1, m_fY1, 0.1));
        kBox.setColor(4, m_akColors[ m_iPlaneOrientation ] );
        kBox.setCoordinate(5, new Point3d(m_fX0, m_fY1, 0.1));
        kBox.setColor(5, m_akColors[ m_iPlaneOrientation ] );
        kBox.setCoordinate(6, new Point3d(m_fX0, m_fY1, 0.1));
        kBox.setColor(6, m_akColors[ m_iPlaneOrientation ] );
        kBox.setCoordinate(7, new Point3d(m_fX0, m_fY0, 0.1));
        kBox.setColor(7, m_akColors[ m_iPlaneOrientation ] );

        Shape3D kBoxShape = new Shape3D();

        kBoxShape.addGeometry(kBox);
        kBoxShape.setPickable(false);

        TransformGroup kBoxTransformGroup = new TransformGroup(new Transform3D());

        kBoxTransformGroup.addChild(kBoxShape);

        BranchGroup kBoxBranchGroup = new BranchGroup();

        kBoxBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
        kBoxBranchGroup.addChild(kBoxTransformGroup);
        kBoxBranchGroup.compile();

        m_kTranslationTG.addChild(kBoxBranchGroup);

        TransformGroup kTransformGroup = new TransformGroup(new Transform3D());

        if (m_bDrawXHairs) {
            Color3f kXSliceHairColor = m_akColors[2];
            Color3f kYSliceHairColor = m_akColors[0];
            if ( m_iPlaneOrientation == FileInfoBase.AXIAL )
            {
                kXSliceHairColor = m_akColors[2];
                kYSliceHairColor = m_akColors[1];
            }
            else if ( m_iPlaneOrientation == FileInfoBase.SAGITTAL )
            {
                kXSliceHairColor = m_akColors[1];
                kYSliceHairColor = m_akColors[0];
            }

            LineArray kYBar = new LineArray(4, GeometryArray.COORDINATES | GeometryArray.COLOR_3);

            float fScreenX = (float) m_fCenterX;
            float fScreenY = (float) -m_fCenterY;
            kYBar.setCoordinate(0, new Point3d(m_fX0, fScreenY, 0.1));
            kYBar.setColor(0, kYSliceHairColor);
            kYBar.setCoordinate(1, new Point3d(fScreenX - .10, fScreenY, 0.1));
            kYBar.setColor(1, kYSliceHairColor);

            kYBar.setCoordinate(2, new Point3d(fScreenX + .10, fScreenY, 0.1));
            kYBar.setColor(2, kYSliceHairColor);
            kYBar.setCoordinate(3, new Point3d(m_fX1, fScreenY, 0.1));
            kYBar.setColor(3, kYSliceHairColor);

            /* Create the Shape3D object to contain the LineArray: */
            Shape3D kYBarShape = new Shape3D();

            kYBarShape.addGeometry(kYBar);
            kYBarShape.setPickable(false);

            LineArray kXBar = new LineArray(4, GeometryArray.COORDINATES | GeometryArray.COLOR_3);

            kXBar.setCoordinate(0, new Point3d(fScreenX, m_fY0, 0.1));
            kXBar.setColor(0, kXSliceHairColor);
            kXBar.setCoordinate(1, new Point3d(fScreenX, fScreenY - 0.1, 0.1));
            kXBar.setColor(1, kXSliceHairColor);

            kXBar.setCoordinate(2, new Point3d(fScreenX, fScreenY + .10, 0.1));
            kXBar.setColor(2, kXSliceHairColor);
            kXBar.setCoordinate(3, new Point3d(fScreenX, m_fY1, 0.1));
            kXBar.setColor(3, kXSliceHairColor);

            /* Create the Shape3D object to contain the LineArray: */
            Shape3D kXBarShape = new Shape3D();

            kXBarShape.addGeometry(kXBar);
            kXBarShape.setPickable(false);

            kTransformGroup.addChild(kXBarShape);
            kTransformGroup.addChild(kYBarShape);
        }

        BranchGroup kBranchGroup = new BranchGroup();

        kBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
        kBranchGroup.addChild(kTransformGroup);
        kBranchGroup.compile();

        m_kTranslationTG.addChild(kBranchGroup);
    }

    /**
     * LocalToScreen converts a point from local coordinates to screen coordinates.
     * @param patient, 
     * @param screen, the transfromed patient point, with scale and teanslation
     */
    private void LocalToScreen( Point2Df patient, Point2Df screen )
    {
//         fCenterX = (fCenterX - m_fX0) / m_fXRange;
//         fCenterY = (fCenterY - m_fY0) / m_fYRange;


        int iCanvasWidth = m_kCanvas.getWidth();
        int iCanvasHeight = m_kCanvas.getHeight();

        //afCenter[1] = (float) ((m_iCanvasHeight - 1) - iY - ((m_iCanvasHeight - 1) / 2.0f)) / m_fDivY;
        screen.x = patient.x;
        screen.y = patient.y;

        screen.y += m_fYTranslate;
        screen.y *= m_fZoomScale;

        screen.y *= (float) iCanvasWidth / 2.0f;
        screen.y += ((iCanvasHeight - 1) / 2.0f);
        screen.y = (float) ((iCanvasHeight - 1) - screen.y - ((iCanvasHeight - 1) / 2.0f)) / ((float) iCanvasWidth / 2.0f);

        screen.y /= m_fZoomScale;
        screen.y -= m_fYTranslate;

        if (screen.y < m_fY0) {
            screen.y = m_fY0;
        }

        if (screen.y > m_fY1) {
            screen.y = m_fY1;
        }
    }


    /**
     * Calculate the center of the mouse in the QUAD coordinates, taking into
     * account zoom and translate:
     *
     * @param  iX        mouse x coordinate value
     * @param  iY        mouse y coordinate value
     * @param  afCenter  center in QUAD coordinates
     */
    private void MouseToQuad(int iX, int iY, Point3Df kQuad, boolean bSetCenter )
    {
        int iCanvasWidth = m_kCanvas.getWidth();
        int iCanvasHeight = m_kCanvas.getHeight();
        iX = (int)Math.min( iCanvasWidth,  Math.max( 0, iX ) );
        iY = (int)Math.min( iCanvasHeight, Math.max( 0, iY ) );
        float fHalfWidth = ((float) iCanvasWidth-1) / 2.0f;
        float fHalfHeight = ((float) iCanvasHeight-1) / 2.0f;

        kQuad.x = ((float) (iX - fHalfWidth)) / fHalfWidth;
        kQuad.y = ((float) (iY - fHalfHeight)) / fHalfWidth;

        kQuad.x /= m_fZoomScale;
        kQuad.y /= m_fZoomScale;

        kQuad.x -= m_fXTranslate;
        kQuad.y -= m_fYTranslate;

        /* Bounds checking: */
        kQuad.x = Math.min( Math.max( kQuad.x, m_fX0 ), m_fX1 );
        kQuad.y = Math.min( Math.max( kQuad.y, m_fY0 ), m_fY1 );

        if ( bSetCenter )
        {
            m_fCenterX = kQuad.x;
            m_fCenterY = kQuad.y;
        }

        /* Normalize: */
        kQuad.x = (kQuad.x - m_fX0) / m_fXRange;
        kQuad.y = (kQuad.y - m_fY0) / m_fYRange;
        kQuad.z = m_iSlice / (float)(m_aiLocalImageExtents[2] - 1);
    }

    /* Convert the position in Quad coordinates into PatientCoordinates:
     * @param quadPt, the current point in QuadCoordinates
     * @param patientPt transformed quadPt in PatientCoordinates
     */
    private void QuadToPatient( Point3Df quadPt, Point3Df patientPt )
    {
        patientPt.x = quadPt.x * (m_aiLocalImageExtents[0] - 1);
        patientPt.y = quadPt.y * (m_aiLocalImageExtents[1] - 1);
        patientPt.z = quadPt.z * (m_aiLocalImageExtents[2] - 1);
    }

        
    private void PatientToQuad( Point3Df patientPt, Point3Df quadPt )
    {
        quadPt.x = patientPt.x / (float)(m_aiLocalImageExtents[0] - 1);
        quadPt.y = patientPt.y / (float)(m_aiLocalImageExtents[1] - 1);
        quadPt.z = patientPt.z / (float)(m_aiLocalImageExtents[2] - 1);

        quadPt.x = (quadPt.x * m_fXRange) + m_fX0;
        quadPt.y = (quadPt.y * m_fYRange) + m_fY0;
        quadPt.z = 1.0f;
    }


    /**
     * Creates the scene graph and initializes the SimpleUniverse and the Viewing transformations.
     */
    private void init() {
        createImageSceneGraph();

        m_kUniverse = new SimpleUniverse(m_kCanvas);

        View kView = m_kUniverse.getViewer().getView();

        kView.setProjectionPolicy(View.PARALLEL_PROJECTION);
        kView.setScreenScalePolicy(View.SCALE_EXPLICIT);

        m_kObjRootBG.compile();
        m_kUniverse.addBranchGraph(m_kObjRootBG);
        setVisible(true);
        m_kCanvas.update(m_kCanvas.getGraphics());

        /* set the ViewScreenScale based on the transformation: */
        updateViewScreenScale(m_kCurrentTransform);
        initAxes();
    }

    /**
     * Initializes the Axis labels based on the ModelImage orientation. Axes are displayed with 3D text objects and
     * arrows drawn as polygons. They are colored and labeled to match the axes they represent.
     */
    private void initAxes() {

        if (m_kTextBranchGroup != null) {
            m_kOrderedGroup.removeChild(m_kTextBranchGroup);
            m_kTextBranchGroup = null;
            m_kTextTransformGroup = null;
        }
        if ( !m_bDrawAxes )
        {
            return;
        }

        String kLabelX = new String( m_kLabelX );
        if ( !m_kImageA.getRadiologicalView() && (m_iPlaneOrientation != FileInfoBase.SAGITTAL) )
        {
            if ( !m_bPatientOrientation )
            {
                kLabelX = new String( "-X" );
            }
            else
            {
                kLabelX = new String( "R" );
            }
        }
        Text3D kXText = new Text3D(new Font3D(MipavUtil.courier12B, new FontExtrusion()), kLabelX,
                                   new Point3f(25f, 1f, 0f));

        Text3D kYText = new Text3D(new Font3D(MipavUtil.courier12B, new FontExtrusion()), m_kLabelY,
                                   new Point3f(-2f, -25f, 0f));

        if ( m_bPatientOrientation &&
            (m_iPlaneOrientation != FileInfoBase.AXIAL) ) {
            kYText.setPosition(new Point3f(-2f, 31f, 0f));
        }

        QuadArray kXGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.COLOR_3);

        Color3f kXSliceHairColor = m_akColors[2];
        Color3f kYSliceHairColor = m_akColors[0];
        if ( m_iPlaneOrientation == FileInfoBase.AXIAL )
        {
            kXSliceHairColor = m_akColors[2];
            kYSliceHairColor = m_akColors[1];
        }
        else if ( m_iPlaneOrientation == FileInfoBase.SAGITTAL )
        {
            kXSliceHairColor = m_akColors[1];
            kYSliceHairColor = m_akColors[0];
        }

        kXGeometry.setColor(0, kXSliceHairColor);
        kXGeometry.setColor(1, kXSliceHairColor);
        kXGeometry.setColor(2, kXSliceHairColor);
        kXGeometry.setColor(3, kXSliceHairColor);
        kXGeometry.setCoordinate(0, new Point3d(2f, 4f, 0.5f));
        kXGeometry.setCoordinate(1, new Point3d(18f, 4f, 0.5f));
        kXGeometry.setCoordinate(2, new Point3d(18f, 6f, 0.5f));
        kXGeometry.setCoordinate(3, new Point3d(2f, 6f, 0.5f));

        TriangleArray kXTri = new TriangleArray(3, TriangleArray.COORDINATES | TriangleArray.COLOR_3);

        kXTri.setColor(0, kXSliceHairColor);
        kXTri.setColor(1, kXSliceHairColor);
        kXTri.setColor(2, kXSliceHairColor);
        kXTri.setCoordinate(0, new Point3d(18f, 9f, 0.5f));
        kXTri.setCoordinate(1, new Point3d(18f, 1f, 0.5f));
        kXTri.setCoordinate(2, new Point3d(23f, 5f, 0.5f));

        QuadArray kYGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.COLOR_3);

        kYGeometry.setColor(0, kYSliceHairColor);
        kYGeometry.setColor(1, kYSliceHairColor);
        kYGeometry.setColor(2, kYSliceHairColor);
        kYGeometry.setColor(3, kYSliceHairColor);

        if (m_bPatientOrientation &&
            (m_iPlaneOrientation != FileInfoBase.AXIAL)) {
            kYGeometry.setCoordinate(0, new Point3d(2f, 4f, 0.5f));
            kYGeometry.setCoordinate(1, new Point3d(0f, 4f, 0.5f));
            kYGeometry.setCoordinate(2, new Point3d(0f, 22f, 0.5f));
            kYGeometry.setCoordinate(3, new Point3d(2f, 22f, 0.5f));
        } else {
            kYGeometry.setCoordinate(0, new Point3d(2f, 6f, 0.5f));
            kYGeometry.setCoordinate(1, new Point3d(0f, 6f, 0.5f));
            kYGeometry.setCoordinate(2, new Point3d(0f, -12f, 0.5f));
            kYGeometry.setCoordinate(3, new Point3d(2f, -12f, 0.5f));
        }

        TriangleArray kYTri = new TriangleArray(3, TriangleArray.COORDINATES | TriangleArray.COLOR_3);

        kYTri.setColor(0, kYSliceHairColor);
        kYTri.setColor(1, kYSliceHairColor);
        kYTri.setColor(2, kYSliceHairColor);

        if (m_bPatientOrientation &&
            (m_iPlaneOrientation != FileInfoBase.AXIAL)) {
            kYTri.setCoordinate(0, new Point3d(5f, 22f, 0.5f));
            kYTri.setCoordinate(1, new Point3d(-4f, 22f, 0.5f));
            kYTri.setCoordinate(2, new Point3d(1f, 27f, 0.5f));
        } else {
            kYTri.setCoordinate(0, new Point3d(5f, -12f, 0.5f));
            kYTri.setCoordinate(1, new Point3d(-4f, -12f, 0.5f));
            kYTri.setCoordinate(2, new Point3d(1f, -17f, 0.5f));
        }

        PolygonAttributes kPolygonAttributes = new PolygonAttributes();

        kPolygonAttributes.setCullFace(PolygonAttributes.CULL_NONE);

        Material kXMaterial = new Material( kXSliceHairColor, kXSliceHairColor, kXSliceHairColor,
                                            kXSliceHairColor, 1.0f);
        Material kYMaterial = new Material( kYSliceHairColor, kYSliceHairColor, kYSliceHairColor,
                                            kYSliceHairColor, 1.0f);

        Appearance kXAppearance = new Appearance();

        kXAppearance.setMaterial(kXMaterial);
        kXAppearance.setPolygonAttributes(kPolygonAttributes);

        Appearance kYAppearance = new Appearance();

        kYAppearance.setMaterial(kYMaterial);
        kYAppearance.setPolygonAttributes(kPolygonAttributes);

        Shape3D kXBox = new Shape3D(kXGeometry, kXAppearance);
        Shape3D kXArrows = new Shape3D(kXTri, kXAppearance);
        Shape3D kYBox = new Shape3D(kYGeometry, kYAppearance);
        Shape3D kYArrows = new Shape3D(kYTri, kYAppearance);
        Shape3D kXAxisLabel = new Shape3D(kXText, kXAppearance);
        Shape3D kYAxisLabel = new Shape3D(kYText, kYAppearance);

        float fXTrans = m_fX0 * 0.85f / m_fZoomScale;
        float fYTrans = m_fY1 * 0.85f / m_fZoomScale;

        if (m_bPatientOrientation &&
            (m_iPlaneOrientation != FileInfoBase.AXIAL)) {
            fYTrans = -fYTrans;
        }

        Transform3D kTextTransform = new Transform3D();
        kTextTransform.setScale(0.01f / m_fZoomScale);
        kTextTransform.setTranslation(new Vector3f(fXTrans, fYTrans, -2.5f));
        m_kTextTransformGroup = new TransformGroup();
        m_kTextTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        m_kTextTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        m_kTextTransformGroup.setCapability(TransformGroup.ALLOW_CHILDREN_WRITE);
        m_kTextTransformGroup.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        m_kTextTransformGroup.setCapability(TransformGroup.ALLOW_CHILDREN_EXTEND);
        m_kTextTransformGroup.setTransform(kTextTransform);

        m_kTextTransformGroup.addChild(kXBox);
        m_kTextTransformGroup.addChild(kXArrows);
        m_kTextTransformGroup.addChild(kYBox);
        m_kTextTransformGroup.addChild(kYArrows);
        m_kTextTransformGroup.addChild(kXAxisLabel);
        m_kTextTransformGroup.addChild(kYAxisLabel);

        m_kTextBranchGroup = new BranchGroup();
        m_kTextBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
        m_kTextBranchGroup.addChild(m_kTextTransformGroup);
        m_kTextBranchGroup.compile();

        m_kOrderedGroup.addChild(m_kTextBranchGroup);
    }

    /**
     * Dragging the mouse with the left-mouse button held down changes the
     * positions of the X and Y cross bars, and therefor the ZSlice positions
     * of the associated PlaneRender objects and the TriPlanar Surface. The
     * new positions are calculated and passed onto the parent frame.
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    private void processLeftMouseDrag(MouseEvent kEvent) {

        /* If the RFA point is enabled, then the mouse is used to select
         * the Probe point, not to move the slice positions: */
        if (m_bEntryPointSelect) {
            return;
        }

        /* Calculate the center of the mouse in quad coordineates, taking into
         * account zoom and translate: */
        Point3Df quadPt = new Point3Df();
        this.MouseToQuad(kEvent.getX(), kEvent.getY(), quadPt, true);

        drawLabels();

        /* Tell the ViewJFrameVolumeView parent to update the other
         * PlaneRenders and the SurfaceRender with the changed Z position
         * of the planes with color matching the moved bar: */
        Point3Df patientPt = new Point3Df();
        this.QuadToPatient( quadPt, patientPt );
        Point3Df volumePt = new Point3Df();
        MipavCoordinateSystems.PatientToFile( patientPt, volumePt, m_kImageA, m_iPlaneOrientation );
        m_kParent.setSliceFromPlane( volumePt );
    }

    /**
     * setCenter sets the cursor and slice position for this PlaneRender
     * object, based on the 3D location of the three intersecting ModelImage
     * planes.
     * @param center, the 3D center in FileCoordinates of the three
     * intersecting ModelImage planes.
     */
    public void setCenter( Point3Df center )
    {
        /* update PatientSlice first: */
        m_kPatientSlice.setCenter( (int)center.x, (int)center.y, (int)center.z );

        Point3Df patientPt = new Point3Df();
        MipavCoordinateSystems.FileToPatient( center, patientPt, m_kImageA, m_iPlaneOrientation );
        setXBar( patientPt.x );
        setYBar( patientPt.y );
        setSlice( patientPt.z );
        drawLabels();
    }

    /**
     * Returns the current center point of the 3 intersecting ModelImage
     * planes in FileCoordinates.
     * @return the current volume center point in FileCoordinates
     */
    public Point3Df getCenter()
    {
        return m_kPatientSlice.getCenter();
    }

    /**
     * If the right mouse button is pressed and dragged. processRightMouseDrag
     * updates the HistoLUT window and level (contrast and brightness)
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    private void processRightMouseDrag(MouseEvent kEvent) {

        /* Get the coordinates of the mouse position in local coordinates: */
        Point3Df quadPt = new Point3Df();
        this.MouseToQuad(kEvent.getX(), kEvent.getY(), quadPt, false);
        m_kActiveLookupTable = null;

        /* Get which image is active, either m_kImageA or m_kImageB: */
        m_kActiveImage = m_kParent.getHistoLUTActiveImage();

        if (m_kActiveImage == null) {
            m_kActiveImage = m_kParent.getHistoRGBActiveImage();
        }
        if (m_kActiveImage == null) {
            m_kActiveImage = m_kImageA;
        }
        m_kPatientSlice.setActiveImage( m_kActiveImage );
        m_kActiveLookupTable = m_kPatientSlice.getActiveLookupTable();

        if ( m_kWinLevel.updateWinLevel( quadPt.x, quadPt.y, m_bFirstDrag, m_kActiveLookupTable, m_kActiveImage ) )
        {
            if ( m_kActiveImage == m_kImageA )
            {
                if ( m_kImageA.isColorImage() )
                {
                    m_kParent.getRGBDialog().setRGBTA((ModelRGB)m_kActiveLookupTable);
                    m_kParent.getRGBDialog().update( );
                }
                else
                {
                    m_kParent.getLUTDialog().setLUTA((ModelLUT)m_kActiveLookupTable);
                }
            }
            else if ( m_kActiveImage == m_kImageB )
            {
                if ( m_kImageB.isColorImage() )
                {
                    m_kParent.getRGBDialog().setRGBTB((ModelRGB)m_kActiveLookupTable);
                    m_kParent.getRGBDialog().update( );
                }
                else
                {
                    m_kParent.getLUTDialog().setLUTB((ModelLUT)m_kActiveLookupTable);
                }
            }
        }
        if (m_bFirstDrag) {
            try {
                Image kImg = MipavUtil.getIconImage("qkwinlevel.gif");
                Cursor kWinLevelCursor = Toolkit.getDefaultToolkit().createCustomCursor(kImg, new Point(12, 12),
                                                                                        "WinLevel");
                /* Set the cursor icon: */
                m_kCanvas.setCursor(kWinLevelCursor);
            } catch (FileNotFoundException error) { }
            m_bFirstDrag = false;
        }
    }

    /**
     * Based on the orientaion of the ModelImage, sets up the index
     * parameters, m_aiLocalImageExtents[0], m_aiLocalImageExtents[1], and
     * m_aiLocalImageExtents[2], the drawing colors for the z box, x and y
     * bars, and the invert flags.
     *
     * <p>Once setup everything is rendered into an x,y plane where x,y may be
     * any of the original x,y, or z dimensions in the original
     * ModelImage.</p>
     */
    private void setOrientation() {
        m_aiLocalImageExtents = m_kImageA.getExtents( m_iPlaneOrientation );

        float[] afResolutions = m_kImageA.getResolutions( 0, m_iPlaneOrientation );

        if ((afResolutions[0] == 0.0f) || (afResolutions[1] == 0.0f) || (afResolutions[2] == 0.0f)) {
            afResolutions[0] = 1.0f;
            afResolutions[1] = 1.0f;
            afResolutions[2] = 1.0f;
        }

        m_fXBox = (float) (m_aiLocalImageExtents[0] - 1) * afResolutions[0];
        m_fYBox = (float) (m_aiLocalImageExtents[1] - 1) * afResolutions[1];

        m_fMaxBox = m_fXBox;

        if (m_fYBox > m_fMaxBox) {
            m_fMaxBox = m_fYBox;
        }

        if ( m_kImageA.getImageOrientation() != FileInfoBase.UNKNOWN_ORIENT )
        {
            if ((m_iPlaneOrientation == FileInfoBase.AXIAL) ||
                (m_iPlaneOrientation == FileInfoBase.CORONAL)) {
                m_kLabelX = new String("L");
            } else {
                m_kLabelX = new String("P");
            }

            if ((m_iPlaneOrientation == FileInfoBase.SAGITTAL) ||
                (m_iPlaneOrientation == FileInfoBase.CORONAL)) {
                m_kLabelY = new String("S");
            } else {
                m_kLabelY = new String("P");
            }
        }
        else
        {
            m_bPatientOrientation = false;
            if ( m_iPlaneOrientation == FileInfoBase.SAGITTAL )
            {
                m_kLabelX = new String("Z");
                m_kLabelY = new String("Y");
            }
            else if ( m_iPlaneOrientation == FileInfoBase.CORONAL )
            {
                m_kLabelX = new String("X");
                m_kLabelY = new String("Z");
            }
        }
    }

    /**
     * This function calculates the scale factor for zooming in parallel
     * projection. The scenario is to calculate the distance between the
     * origin boxframe center and tranformed boxframe center. This distance is
     * used to compute the screen scale factor.
     *
     * @param  kTransform  The tranformation matrix from tranformChanged().
     */
    private void updateViewScreenScale(Transform3D kTransform) {
        float dRadius = m_fX1 - m_fX0;

        if ((m_fY1 - m_fY0) > dRadius) {
            dRadius = m_fY1 - m_fY0;
        }

        BoundingSphere kBounds = new BoundingSphere(new Point3d(0, 0, 0), dRadius);
        kBounds.transform(kBounds, kTransform);
        Point3d kVolumeCenterPoint = new Point3d();
        kBounds.getCenter(kVolumeCenterPoint);
        float dDist = Math.abs((float) kVolumeCenterPoint.z);

        View kView = m_kUniverse.getViewer().getView();
        float dFieldOfView = (float) kView.getFieldOfView();
        float dViewWidth = 2.0f * dDist * (float) Math.tan(dFieldOfView /2.0f);

        Screen3D kScreen = m_kCanvas.getScreen3D();
        // screen width is in meters.
        float dNewScreenScale = (float) kScreen.getPhysicalScreenWidth() / dViewWidth;
        float dOldScreenScale = (float) kScreen.getPhysicalScreenWidth() / 2.0f;

        kView.setScreenScale(dNewScreenScale);

        m_fZoomScale = (float) (dNewScreenScale / dOldScreenScale);
    }

    /**
     * Stores the ModelImage data as an array of texture maps, with LUT or
     * RGBA color lookup:.
     */
    private void writeTexture( boolean bForceShow )
    {

        if ( m_kImageA == null )
        {
            return;
        }
        int buffFactor = m_kImageA.isColorImage() ? 4 : 1;
        int[] iImageBufferA = new int[ m_aiLocalImageExtents[0] * m_aiLocalImageExtents[1] * buffFactor ];
        int[] iImageBufferB = null;
        if ( m_kImageB != null )
        {
            buffFactor = m_kImageB.isColorImage() ? 4 : 1;
            iImageBufferB = new int[ m_aiLocalImageExtents[0] * m_aiLocalImageExtents[1] * buffFactor ];
        }

        int iNumberSlices = m_aiLocalImageExtents[2];
        if ( !m_bMemoryUsage )
        {
            iNumberSlices = 1;
        }
        m_akImageComponent = new ImageComponent2D[iNumberSlices];

        float alphaBlend = 0.5f;
//         if (frame.getVolOpacityPanel() != null)
//         {
//             alphaBlend = (100.0f - (float) frame.getVolOpacityPanel().getAlphaBlendSliderValue()) / 100.0f;
//         }

        for (int iZ = 0; iZ < iNumberSlices; iZ++) {
            if ( !m_bMemoryUsage )
            {
                m_kPatientSlice.updateSlice( m_iSlice );
            }
            else
            {
                m_kPatientSlice.updateSlice( iZ );
            }
            if ( m_kPatientSlice.showUsingOrientation( 0, iImageBufferA, iImageBufferB, bForceShow, true, alphaBlend, true ) )
            {
                BufferedImage kBuff = new BufferedImage( m_aiLocalImageExtents[0], m_aiLocalImageExtents[1], BufferedImage.TYPE_INT_ARGB );
                kBuff.setRGB( 0, 0, m_aiLocalImageExtents[0], m_aiLocalImageExtents[1], iImageBufferA, 0, m_aiLocalImageExtents[0] );

                m_akImageComponent[iZ] = new ImageComponent2D(ImageComponent.FORMAT_RGBA, m_aiLocalImageExtents[0], m_aiLocalImageExtents[1]);
                m_akImageComponent[iZ].setCapability(ImageComponent2D.ALLOW_IMAGE_WRITE);
                m_akImageComponent[iZ].setCapability(ImageComponent2D.ALLOW_IMAGE_READ);
                m_akImageComponent[iZ].setCapability(ImageComponent2D.ALLOW_SIZE_READ);
                m_akImageComponent[iZ].setCapability(ImageComponent2D.ALLOW_FORMAT_READ);
                m_akImageComponent[iZ].set( kBuff );

                /* Setup the texture map: */
                if ( (iZ == m_iSlice) || !m_bMemoryUsage )
                {
                    m_kDisplayedImage.set(m_akImageComponent[iZ].getImage());
                }
            }
        }
    }
}
