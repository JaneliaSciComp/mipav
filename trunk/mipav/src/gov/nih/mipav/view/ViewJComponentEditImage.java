package gov.nih.mipav.view;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.AlgorithmRegionGrow;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmMask;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;

import gov.nih.mipav.model.structures.*;


import gov.nih.mipav.view.Preferences.ComplexDisplay;
import gov.nih.mipav.view.Preferences.DefaultDisplay;
import gov.nih.mipav.view.Preferences.InterpolateDisplay;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.dialogs.JDialogCheckerBoard.Animate;
import gov.nih.mipav.view.icons.PlaceHolder;
import gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManager;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.AffineTransform;
import java.awt.image.*;
import java.io.*;
import java.net.URL;
import java.text.*;
import java.util.*;

import javax.imageio.ImageIO;
import javax.swing.JOptionPane;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Basic displayable image object in MIPAV. Contains the viewable objects such as VOIs and carries listeners to most any
 * action that can happen in a window.
 * 
 * @version 0.1 Nov 18, 1997
 * @author Matthew J. McAuliffe, Ph.D.
 */
public class ViewJComponentEditImage extends ViewJComponentBase implements MouseMotionListener, MouseWheelListener,
MouseListener, PaintGrowListener, ScreenCoordinateListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3799092616373463766L;

    /**
     * Color used in the rectangle indicating the image is active. Images which are not active will not have the
     * recangle drawn.
     */
    public static Color ACTIVE_IMAGE_COLOR = Color.cyan;

    /*
     * * In painting COMPLEX images the COMPLEX image is assumed to have been a result of transforming real data.
     * x(n1,n2) real <-> X(k1,k2) = X*(-k1,-k2) for 2D and x(n1,n2,n3) real <-> X(k1,k2,k3) = X*(-k1,-k2,-k3).
     * Therefore, when one pixel is painted in a COMPLEX image, the above defined complementary pixel is also painted.
     * The zero point for k1, k2, k3 occur such that there is one more pixel below the zero point than there is above
     * it. For example, if the FFT is 2048 by 2048 by 2048, the center occurs at (1024,1024,1024) so that (0,0,0) is
     * further away than (2047,2047,2047)
     */

    /** Used to indicte geometric zoom steps (2x, 4x, 8x, 16x ...). */
    public static final int GEOMETRIC_ZOOM = 0;

    /** Used to indicte linear zoom steps (2x, 3x, 4x, 5x ...). */
    public static final int LINEAR_ZOOM = 1;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /**
     * Dialog used to set properties of the checkerboard display:(Image A displayed in alternating squares with Image B.
     */
    public JDialogCheckerBoard checkerDialog = null;

    /** Dialog used to control region growing of the paint tool. */
    public RegionGrowDialog growDialog = null;

    /** Used in paint to indicate intensity painted into the image. */
    public float intensityDropper = 1.0f;

    /** Default size of magnifier window = 120 pixels. */
    public int MAGR_HEIGHT = 120;

    /** Magnification value of magnifier window -- default = 4.0. */
    public float MAGR_MAG = 4f;

    /** default = 120 pixels. */
    public int MAGR_WIDTH = 120;

    /** Set to true when all contours of a VOI are active. */
    protected boolean allActive = false;

    /** Value used to control the display when compositing two images. Usually = 1 - alphaPrime. */
    protected float alphaBlend = 0.5f;

    /**
     * Value used to control the display when compositing two images. It is the amount of image A to display and is has
     * a range of [0,1]
     */
    protected float alphaPrime = 0.5f;

    /** String representing RGB components used in applied paint */
    protected String rgbString = null;

    /** Buffer used to store ARGB images of the image presently being displayed. */
    protected int[] cleanImageBufferA = null;

    /** Buffer used to store ARGB images of the image presently being displayed. */
    protected int[] cleanImageBufferB = null;

    

	/** Crosshair cursor that can be changed per user preference (in Mipav Options). */
    protected Cursor crosshairCursor = MipavUtil.crosshairCursor;

    /** Used to describe the cursor mode. */
    protected int cursorMode;

    /** DOCUMENT ME! */
    protected boolean displayFuzzy = false;

    /** Flag used  to prohibit VOI drawing. */
    protected boolean drawVOIs = true;

    /** Frame where the component image is displayed. */
    protected ViewJFrameBase frame;

    /** Controls having color and opacity. */
    protected ViewControlsImage frameControls = null;

    /**
     * Region grow parameters fuzzyConnectedness is not used if fuzzyThreshold is less than 0. When fuzzyConnectedness
     * is used, fuzzyThreshold ranges from 0 to 1.
     */
    protected float fuzzyThreshold = -1;

    /** Model for image A. */
    protected ModelImage imageA;

    /** Active image. */
    protected ModelImage imageActive = null;

    /** Model for image B. */
    protected ModelImage imageB;

    /** Buffer for image A, raw intensity. */
    protected float[] imageBufferA = null;

    /** Buffer for active image. */
    protected float[] imageBufferActive = null;

    /** Buffer for image B, raw intensity. */
    protected float[] imageBufferB = null;

    /** The dimensionality of the image. */
    protected int[] imageExtents;

    /** DOCUMENT ME! */
    protected JDialogVOIStatistics imageStatList;

    /** DOCUMENT ME! */
    protected float less = 10;

    /** DOCUMENT ME! */
    protected float lessB = 10;

    /** DOCUMENT ME! */
    protected float lessG = 10;

    /** DOCUMENT ME! */
    protected float lessR = 10;

    /** Flag for displaying logMag. */
    protected boolean logMagDisplay = false;

    /** Lookup table for image A. */
    protected ModelLUT LUTa;

    /** Lookup table for image B. */
    protected ModelLUT LUTb;

    /** PatientSlice contains all the Patient Coordinate system view-specific data for rendering this component:. */
    protected PatientSlice m_kPatientSlice;

    /** Dialog used to control the magnification of the image. */
    protected JDialogMagnificationControls magSettings;

    /** DOCUMENT ME! */
    protected float maxDistance = -1;

    /** DOCUMENT ME! */
    protected int[] maxExtents = new int[2];

    /** Used to "lock" display when an algorithm is in the calculation process. */
    protected boolean modifyFlag = true;

    /** DOCUMENT ME! */
    protected float more = 10;

    /** DOCUMENT ME! */
    protected float moreB = 10;

    /** DOCUMENT ME! */
    protected float moreG = 10;

    /** DOCUMENT ME! */
    protected float moreR = 10;

    /** Keep track of state of shift for mouse Pressed events. */
    protected boolean mousePressIsShiftDown = false;

    /** The number columns when two images are displayed in the checker board mode. */
    protected int nColumnCheckers = -1;

    /** Checkerboard display parameters. The number of rows to display. */
    protected int nRowCheckers = -1; // a negative value indicates no checkerboarding

    /** for the use of the user-notifier. */
    protected boolean onTop = false;

    /** orientation of the slice, may be AXIAL, CORONAL, SAGITTAL, or UNKNOWN_ORIENT. */
    protected int orientation = FileInfoBase.UNKNOWN_ORIENT;

    /** Buffer used to indicate if the pixel location is painted (true) or unpainted (false). */
    protected BitSet paintBitmap;

    /** Backup of paint buffer for undo. */
    protected BitSet paintBitmapBU;

    /** if true do not getMask on a setActiveImage command so as to keep the mask from the old active image. */
    protected boolean paintBitmapSwitch = false;

    /** BitSet used for painting (brushes on/off). */
    protected BitSet paintBrush = null;

    /** Dimension of the paint. */
    protected Dimension paintBrushDim = null;

    /** DOCUMENT ME! */
    protected Dimension paintBrushDimPrevious = null;

    /** DOCUMENT ME! */
    protected BitSet paintBrushPrevious = null;

    /** Paint brush size. */
    protected int paintBrushSize;

    /** buffered image that is transparent to show the paintbrush cursor. */
    protected BufferedImage paintImage = null;

    /** DOCUMENT ME! */
    protected BufferedImage paintImagePrevious = null;

    /** The buffer used to store the ARGB image of the image presently being displayed. */
    protected int[] pixBuffer = null;

    /** Buffer used to store ARGB image of the windowed imageB. */
    protected int[] pixBufferB = null;

    /**
     * The previous paint brush we were using when the user temporarily changes the paint brush size using keyboard
     * shortcuts (by pressing 1, 2, 3, 4).
     */
    protected int previousPaintBrush = -1;

    /** DOCUMENT ME! */
    protected ModelRGB RGBTA;

    /** DOCUMENT ME! */
    protected ModelRGB RGBTB;

    /** Used with commitMask(int imagesDone), to save the value. */
    protected float saveValue;

    /** DOCUMENT ME! */
    protected float saveValueB;

    /** DOCUMENT ME! */
    protected float saveValueG;

    /** DOCUMENT ME! */
    protected float saveValueR;

    /** Used with commitMask(int imagesDone), to save positions. */
    protected short saveX, saveY, saveZ;

    /** Buffer used to remember the area painted around the last seed point for region growing. */
    protected BitSet seedPaintBitmap;

    /** Seed value to start the region grow. */
    protected float seedVal;

    /** DOCUMENT ME! */
    protected float seedValB;

    /** DOCUMENT ME! */
    protected float seedValG;

    /** DOCUMENT ME! */
    protected float seedValR;

    /** DOCUMENT ME! */
    protected int sizeLimit = -1;

    /** DOCUMENT ME! */
    protected int slice = -99;

    /** String used to override the slice number in the lower left corner. */
    protected String stringOverride = null;

    /** DOCUMENT ME! */
    protected long time;

    /** DOCUMENT ME! */
    protected int timeSlice = 0;

    /** DOCUMENT ME! */
    protected Color toppedColor = ViewJComponentEditImage.ACTIVE_IMAGE_COLOR;

    /**
     * This flag is used by the fuzzy connectedness to indicate that a VOI should be used to calculate certain values to
     * initialize the process.
     */
    protected boolean useVOI = false;

    /** DOCUMENT ME! */
    protected boolean variableThresholds = false;

    /** Visible rectangle to draw topped. */
    protected Rectangle visRect;

    /** User invokes window and level adjustment with right mouse drag in DEFAULT mode. */
    protected boolean winLevelSet = false;

    /** X coord of seed point. */
    protected short xPG;

    /** Y coord of seed point. */
    protected short yPG;

    /** Z coord of seed point. */
    protected short zPG;

    /** DOCUMENT ME! */
    private Image cleanImageB = null;

    /** If true then the x,y location and pixel/voxel intensity should be displayed (x,y: intensity). */
    private boolean intensityLabel = false;

    /**
     * Last slice the image was at when win region was ON. its necessary because otherwise, a new cleanImageB would have
     * to be created upon every repaint. should be initialized to a negative number
     */
    private int lastWinRegionSlice = -1;

    /** window / level mouse control:. */
    protected WindowLevel m_kWinLevel;

    /** DOCUMENT ME! */
    private Image offscreenImage = null;

    /** This image indicates which images have been painted. */
    private int[] paintImageBuffer = null;

    /** Flag used to indicate if the shift key is depressed. True indicates the shift key is depressed. */
    private boolean shiftDown = false;

    /**
     * Flag used to indicate that the intensity values of the image should be displayed in the magnified portion of the
     * image. The intensity values can only be shown if the magnification is high enough to fit the text representation
     * of the value in the magnified voxel/pixel.
     */
    private boolean showMagIntensity = false;

    /** Boolean to determine if the mouse had been dragging before mouse release (for moving and setting active). */
    private boolean wasDragging = false;

    /**
     * The window region size of used when two images are displayed in the same frame. The size of the window indicates
     * how much of image B is displayed.
     */
    private int windowedRegionSize = 100;

    protected boolean useRComp = true;

    protected boolean useGComp = true;

    protected boolean useBComp = true;

    /** flag indicating whether there is 0 to 1 LUT Adjustment * */
    private boolean zeroToOneLUTAdj = false;

    protected int[] paintBuffer;



    /** color of grid. */
    protected Color gridColor = Color.lightGray;

    /** if number/lettering should be displayed for grid boxes. */
    protected boolean gridLabelingOn = false;

    /**
     * boolean to determine the orientation: true is x-axis numbered false is
     * x-axis lettered.
     */
    protected boolean gridLabelOrientation = true;

    /** Flag to indicate if NEI grid overlay should be displayed. */
    protected boolean gridOverlayOn = false;

    /** spacing of the grid (horizontal) in terms of resolution. */
    protected float gridSpacingX = 20f;

    /** spacing of the grid (vertical) in terms of resolution. */
    protected float gridSpacingY = 20f;

    /** Flag to indicate if DICOM overlay should be displayed. */
    protected boolean overlayOn = false;

    protected VOIManager voiManager;

	private long lastMouseEvent = 0;
	
	
	//following methods are primarily used in the creation of checkerboards and animting it
	private boolean doCheckerboardAnimate = false;
	private int xSep = 0;
	private int ySep = 0;
	int xMod = 0;
	int yMod = 0;
	int[] xStart;
    int[] yStart;
	private boolean makingCheckerboard = false;
	private int[] bandSpacing;
	
	
	
	

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     * 
     * @param _frame frame where image(s) will be displayed
     * @param _imageA Model of the image that will be displayed
     * @param _LUTa LUT used to display imageA
     * @param imgBufferA storage buffer used to display image A
     * @param _imageB Model of the image that will be displayed
     * @param _LUTb LUT used to display imageB
     * @param imgBufferB storage buffer used to display image B
     * @param pixelBuffer storage buffer used to build a displayable image
     * @param zoom initial magnification of image
     * @param extents initial display dimensions of the image
     * @param logMagDisplay display log magnitude of image
     * @param _orientation orientation of the image
     */
    public ViewJComponentEditImage(final ViewJFrameBase _frame, final ModelImage _imageA, final ModelLUT _LUTa,
            float[] imgBufferA, final ModelImage _imageB, final ModelLUT _LUTb, float[] imgBufferB, int[] pixelBuffer,
            final float zoom, final int[] extents, final boolean logMagDisplay, final int _orientation) {
        super(_imageA.getWidth(_orientation), _imageA.getHeight(_orientation), _imageA);

        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;
        imageExtents = extents;

        orientation = _orientation;

        LUTa = _LUTa;
        LUTb = _LUTb;

        showSliceNumber = (imageA.getNDims() > 2) && ! (this instanceof ViewJComponentTriImage);

        if (imageA.isDicomImage()) {
            setOverlay(Preferences.is(Preferences.PREF_SHOW_DICOM_OVERLAYS));
        } else {
            setOverlay(Preferences.is(Preferences.PREF_SHOW_IMAGE_OVERLAYS));
        }
        
        // active image color: get preset
        if (Preferences.getProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR) == null) {
            Preferences.setProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR, MipavUtil
                    .makeColorString(ViewJComponentEditImage.ACTIVE_IMAGE_COLOR));
        } else {
            this
            .setHighlightColor(MipavUtil.extractColor(Preferences
                    .getProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR)));
        }

        // Custom crosshair cursors
        if (Preferences.getProperty(Preferences.PREF_CROSSHAIR_CURSOR) == null) {
            Preferences.setProperty(Preferences.PREF_CROSSHAIR_CURSOR, "default");
        }

        final String crosshairName = Preferences.getProperty(Preferences.PREF_CROSSHAIR_CURSOR);

        if (crosshairName.equalsIgnoreCase("default")) {
            this.setCrosshairCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        } else {

            try {
                final Toolkit toolkit = Toolkit.getDefaultToolkit();

                this.setCrosshairCursor(toolkit.createCustomCursor(MipavUtil.getIcon(crosshairName).getImage(),
                        new Point(15, 15), crosshairName));
            } catch (final NullPointerException noIcon) {

                // specfied icon cannot be found. Instead, we load default:
                this.setCrosshairCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
                Preferences.debug("ViewJComponentEditImage: Crosshair icon \"" + crosshairName
                        + "\" cannot be found.  " + "Instead, using default crosshair pointer.\n", 2);
            }
        }

        cursorMode = ViewJComponentBase.DEFAULT;

        if (imgBufferA == null) {
            final int bufferFactor = (imageA.isColorImage() ? 4 : imageA.isComplexImage() ? Preferences.getComplexDisplay().getNumParts() : 1);
            imgBufferA = new float[bufferFactor * imageDim.width * imageDim.height];
        }

        if ( (imgBufferB == null) && (imageB != null)) {
            final int bufferFactor = (imageB.isColorImage() ? 4 : imageB.isComplexImage() ? Preferences.getComplexDisplay().getNumParts() : 1);
            imgBufferB = new float[bufferFactor * imageDim.width * imageDim.height];
        }

        if (pixelBuffer == null) {
            pixelBuffer = new int[imageDim.width * imageDim.height];
        }

        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
        pixBuffer = pixelBuffer;

        paintBitmap = imageA.getMask();
        paintBitmapBU = imageA.getMaskBU();

        imageBufferActive = imageBufferA;
        this.logMagDisplay = logMagDisplay;

        super.setZoom(zoom, zoom);

        addMouseMotionListener(this);
        addMouseListener(this);
        setVisible(true);

        addMouseWheelListener(this);

        if (frame != null) {
            magSettings = new JDialogMagnificationControls(frame, this, getZoomX(), imageA.getImageName());
        }

        maxExtents[0] = imageDim.width;
        maxExtents[1] = imageDim.height;

        /* create the slice renderer for this orientation: */
        m_kPatientSlice = new PatientSlice(imageA, LUTa, imageB, LUTb, orientation);
        m_kPatientSlice.setBuffers(imageBufferA, imageBufferB);

        /* create the WindowLevel controller: */
        m_kWinLevel = new WindowLevel();

        if ( ! (_frame instanceof ViewJFrameLightBox)) {
            loadPaintBrush(Preferences.getProperty(Preferences.PREF_LAST_PAINT_BRUSH), false);

            rgbString = Preferences.getProperty(Preferences.PREF_RGB_PAINT_COMPONENTS);
            if (rgbString == null) {
                Preferences.setProperty(Preferences.PREF_RGB_PAINT_COMPONENTS, "RGB");
                rgbString = "RGB";
            } else {
                useRComp = rgbString.indexOf("R") != -1;
                useGComp = rgbString.indexOf("G") != -1;
                useBComp = rgbString.indexOf("B") != -1;
            }

        }
        final int imageSize = extents[0] * extents[1];
        paintBuffer = new int[imageSize];
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Gets position data to display in message bar - for DICOM and MINC images, gives patient position as well. The
     * image's associated transformation must be FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL, or the orientations must be
     * set up correctly, or else the function returns null.
     * 
     * @param image The image the point lies within.
     * @param position (x,y,z(slice)) position in FileCoordinates
     * 
     * @return An array of strings that represent patient position.
     */
    public static final String[] getScannerPositionLabels(final ModelImage image, final Vector3f position) {
        final DecimalFormat nf = new DecimalFormat("#####0.0##");
        final Vector3f kOut = new Vector3f(position);
        MipavCoordinateSystems.fileToScanner(position, kOut, image);

        final float[] tCoord = new float[3];
        tCoord[0] = kOut.X;
        tCoord[1] = kOut.Y;
        tCoord[2] = kOut.Z;

        final String[] labels = {"R-L: ", "A-P: ", "I-S: "};

        if ( !image.getRadiologicalView()) {
            labels[0] = new String("L-R: ");
        }

        final String[] strs = new String[3];

        if (image.getRadiologicalView()) {

            if ( (tCoord[0] < 0)) {
                strs[0] = new String(labels[0] + labels[0].charAt(0) + ": " + String.valueOf(nf.format(tCoord[0])));
            } else {
                strs[0] = new String(labels[0] + labels[0].charAt(2) + ": " + String.valueOf(nf.format(tCoord[0])));
            }
        } else {

            if ( (tCoord[0] < 0)) {
                strs[0] = new String(labels[0] + labels[0].charAt(2) + ": " + String.valueOf(nf.format(tCoord[0])));
            } else {
                strs[0] = new String(labels[0] + labels[0].charAt(0) + ": " + String.valueOf(nf.format(tCoord[0])));
            }
        }

        for (int i = 1; i < 3; i++) {

            if ( (tCoord[i] < 0)) {
                strs[i] = new String(labels[i] + labels[i].charAt(0) + ": " + String.valueOf(nf.format(tCoord[i])));
            } else {
                strs[i] = new String(labels[i] + labels[i].charAt(2) + ": " + String.valueOf(nf.format(tCoord[i])));
            }
        }

        return strs;
    }
    
    
    
    
    /**
     * Gets position data to display in message bar - for DICOM and MINC images, gives patient position as well. The
     * image's associated transformation must be FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL, or the orientations must be
     * set up correctly, or else the function returns null.
     * 
     * @param image The image the point lies within.
     * @param position (x,y,z(slice)) position in FileCoordinates
     * 
     * @return An array of strings that represent patient position.
     */
    public static final String[] getRASScannerPositionLabels(final ModelImage image, final Vector3f position) {
        final DecimalFormat nf = new DecimalFormat("#####0.0##");
        final Vector3f kOut = new Vector3f(position);
        if (image.getNDims() >= 3) {
            MipavCoordinateSystems.fileToScanner(position, kOut, image);
        }

        final float[] tCoord = new float[3];
        tCoord[0] = -kOut.X;
        tCoord[1] = -kOut.Y;
        tCoord[2] = kOut.Z;

        final String[] labels = {"L-R: ", "P-A: ", "I-S: "};

        if ( !image.getRadiologicalView()) {
            labels[0] = new String("R-L: ");
        }

        final String[] strs = new String[3];

        if (image.getRadiologicalView()) {

            if ( (tCoord[0] < 0)) {
                strs[0] = new String(labels[0] + labels[0].charAt(0) + ": " + String.valueOf(nf.format(tCoord[0])));
            } else {
                strs[0] = new String(labels[0] + labels[0].charAt(2) + ": " + String.valueOf(nf.format(tCoord[0])));
            }
        } else {

            if ( (tCoord[0] < 0)) {
                strs[0] = new String(labels[0] + labels[0].charAt(2) + ": " + String.valueOf(nf.format(tCoord[0])));
            } else {
                strs[0] = new String(labels[0] + labels[0].charAt(0) + ": " + String.valueOf(nf.format(tCoord[0])));
            }
        }

        for (int i = 1; i < 3; i++) {

            if ( (tCoord[i] < 0)) {
                strs[i] = new String(labels[i] + labels[i].charAt(0) + ": " + String.valueOf(nf.format(tCoord[i])));
            } else {
                strs[i] = new String(labels[i] + labels[i].charAt(2) + ": " + String.valueOf(nf.format(tCoord[i])));
            }
        }

        return strs;
    }

    /**
     * Calculates the volume of the painted voxels.
     * 
     * @param str string in showRegionGrow {@link #showRegionGrow(int, String)}
     */
    public void calcPaintedVolume(final String str) {
        int count = 0;
        int zEnd;
        int tEnd;
        int z;
        int t;
        int sliceSize;
        int imgLength;
        int volSize = 1;
        int cf;
        float buffer[];
        int offset;
        int i;
        float total[] = null;
        float mean[] = null;
        float stdDev[] = null;
        float diff;
        final int paintSize = paintBitmap.size();

        zEnd = 1;
        tEnd = 1;
        if (imageA.getNDims() >= 4) {
            tEnd = imageA.getExtents()[3];
        }
        if (imageA.getNDims() >= 3) {
            zEnd = imageA.getExtents()[2];
        }
        if (imageA.isColorImage()) {
            cf = 4;
        } else if (imageA.isComplexImage()) {
            cf = 2;
        } else {
            cf = 1;
        }

        if (cf == 2) {
            for (i = 0; i < paintSize; i++) {
                if (paintBitmap.get(i)) {
                    count++;
                }
            }
            showRegionInfo(count, str);
            return;
        }

        total = new float[cf];
        mean = new float[cf];
        stdDev = new float[cf];
        sliceSize = imageA.getExtents()[0] * imageA.getExtents()[1];
        imgLength = cf * sliceSize;
        if (imageA.getNDims() >= 3) {
            volSize = sliceSize * imageA.getExtents()[2];
        }
        buffer = new float[imgLength];
        for (t = 0; t < tEnd; t++) {
            for (z = 0; z < zEnd; z++) {
                offset = t * volSize + z * sliceSize;
                try {
                    imageA.exportData(cf * offset, imgLength, buffer); // locks and releases
                    // lock
                } catch (final IOException error) {
                    MipavUtil.displayError("ViewJComponentImage: Image(s) locked");

                    return;
                }
                if (cf == 4) {
                    for (i = 0; i < sliceSize; i++) {
                        if (paintBitmap.get(offset + i)) {
                            count++;
                            total[0] += buffer[4 * i + 1];
                            total[1] += buffer[4 * i + 2];
                            total[2] += buffer[4 * i + 3];
                        } // if (paintBitmap.get(offset + i))
                    } // for (i = 0; i < sliceSize; i++)
                } // if (cf == 4)
                else {
                    for (i = 0; i < sliceSize; i++) {
                        if (paintBitmap.get(offset + i)) {
                            count++;
                            total[0] += buffer[i];
                        } // if (paintBitmap.get(offset + i))
                    } // for (i = 0; i < sliceSize; i++)
                }
            } // for (z = 0; z < zEnd; z++)
        } // for (t = 0; t < tEnd; t++)

        mean[0] = total[0] / count;
        if (cf == 4) {
            mean[1] = total[1] / count;
            mean[2] = total[2] / count;
        }

        for (t = 0; t < tEnd; t++) {
            for (z = 0; z < zEnd; z++) {
                offset = t * volSize + z * sliceSize;
                try {
                    imageA.exportData(cf * offset, imgLength, buffer); // locks and releases
                    // lock
                } catch (final IOException error) {
                    MipavUtil.displayError("ViewJComponentImage: Image(s) locked");

                    return;
                }
                if (cf == 4) {
                    for (i = 0; i < sliceSize; i++) {
                        if (paintBitmap.get(offset + i)) {
                            diff = buffer[4 * i + 1] - mean[0];
                            stdDev[0] += diff * diff;
                            diff = buffer[4 * i + 2] - mean[1];
                            stdDev[1] += diff * diff;
                            diff = buffer[4 * i + 3] - mean[2];
                            stdDev[2] += diff * diff;
                        } // if (paintBitmap.get(offset + i))
                    } // for (i = 0; i < sliceSize; i++)
                } // if (cf == 4)
                else {
                    for (i = 0; i < sliceSize; i++) {
                        if (paintBitmap.get(offset + i)) {
                            diff = buffer[i] - mean[0];
                            stdDev[0] += diff * diff;
                        } // if (paintBitmap.get(offset + i))
                    } // for (i = 0; i < sliceSize; i++)
                }
            } // for (z = 0; z < zEnd; z++)
        } // for (t = 0; t < tEnd; t++)
        stdDev[0] = (float) Math.sqrt(stdDev[0] / count);
        if (cf == 4) {
            stdDev[1] = (float) Math.sqrt(stdDev[1] / count);
            stdDev[2] = (float) Math.sqrt(stdDev[2] / count);
        }

        showRegionInfo(count, total, mean, stdDev, str);
    }

    /**
     * Loops through the images and displays them.
     * 
     * @param ms how long to wait between each image
     */
    public synchronized void cine(final int ms) {
        int i;
        int zDim;
        long localTime = System.currentTimeMillis();
        long waitTime;

        if (ms < time) {
            waitTime = 1;
        } else {
            waitTime = ms - time + 1;
        }

        if (imageA.getNDims() > 2) {
            zDim = imageExtents[2];
        } else {
            return;
        }

        for (i = 0; i < zDim; i++) {
            show(timeSlice, i, null, null, true, -1);

            try {
                wait(waitTime);
            } catch (final InterruptedException error) {
                Preferences.debug("ViewJComponentEditImage.cine:wait.\n");
            }
        }

        localTime = (System.currentTimeMillis() - localTime) / zDim;
        Preferences.debug("\n ***** " + (1 / ((float) localTime / 1000)) + " fps.\n\n");
    }

    /**
     * Replace intensities in the image using painted mask.
     * 
     * @param imagesDone IMAGE_A, IMAGE_B, or BseedVOTH
     * @param clearPaintMask if true clear paint mask
     * @param polarity DOCUMENT ME!
     */
    public void commitMask(final int imagesDone, final boolean clearPaintMask, final boolean polarity,
            final boolean commitMasksAs4D) {
        commitMask(imagesDone, clearPaintMask, polarity, null, commitMasksAs4D);
    }

    /**
     * Replace intensities in the image using painted mask.
     * 
     * @param imagesDone IMAGE_A, IMAGE_B, or BseedVOTH
     * @param clearPaintMask if true clear paint mask
     * @param polarity DOCUMENT ME!
     * @param intensityLockVector Vector containing Integers values which are indexed to the locked intensity values in
     *            the image
     */
    public void commitMask(final int imagesDone, final boolean clearPaintMask, final boolean polarity,
            final Vector<Integer> intensityLockVector, final boolean commitMasksAs4D) {
        commitMask(imagesDone, clearPaintMask, polarity, intensityLockVector, true, commitMasksAs4D);
    }

    /**
     * Replace intensities in the image using painted mask.
     * 
     * @param affectedImage IMAGE_A, IMAGE_B, or BseedVOTH
     * @param clearPaintMask if true clear paint mask
     * @param polarity DOCUMENT ME!
     * @param intensityLockVector Vector containing Integers values which are indexed to the locked intensity values in
     *            the image
     * @param showProgressBar if true, shows the progress bar for this algorithm
     */
    public void commitMask(final ModelImage affectedImage, final boolean clearPaintMask, final boolean polarity,
            final Vector<Integer> intensityLockVector, final boolean showProgressBar, final boolean commitMasksAs4D) {

        if (affectedImage == imageA) {
            commitMask(ViewJComponentBase.IMAGE_A, clearPaintMask, polarity, intensityLockVector, showProgressBar,
                    commitMasksAs4D);
        }

        if ( (imageB != null) && (affectedImage == imageB)) {
            commitMask(ViewJComponentBase.IMAGE_B, clearPaintMask, polarity, intensityLockVector, showProgressBar,
                    commitMasksAs4D);
        }
    }

    /**
     * Replace intensities in the image using painted mask.
     * 
     * @param imagesDone IMAGE_A, IMAGE_B, or BseedVOTH
     * @param clearPaintMask if true clear paint mask
     * @param polarity DOCUMENT ME!
     * @param intensityLockVector Vector containing Integers values which are indexed to the locked intensity values in
     *            the image
     * @param showProgressBar if true, shows the progress bar for this algorithm
     */
    public void commitMask(final int imagesDone, final boolean clearPaintMask, final boolean polarity,
            final Vector<Integer> intensityLockVector, final boolean showProgressBar, final boolean commitMasksAs4D) {

        float min, max;
        Color fillColor = new Color(128, 0, 0);
        final int[] slice = new int[1];
        slice[0] = 0;

        AlgorithmMask maskAlgo = null;

        if (imageA.isColorImage() == true) {

            if (frame.getControls() != null) {
                fillColor = frame.getControls().getTools().getPaintColor();
            } else if (frameControls != null) {
                fillColor = frameControls.getTools().getPaintColor();
            } else {
                fillColor = new Color(128, 0, 0);
            }
        }

        if ( (imagesDone == ViewJComponentBase.IMAGE_A) || (imagesDone == ViewJComponentBase.BOTH)) {
            final float imgMinOrig = (float) imageA.getMin();
            final float imgMaxOrig = (float) imageA.getMax();

            if (imageA.isColorImage() == true) {
                maskAlgo = new AlgorithmMask(imageA, fillColor, polarity, false);
                maskAlgo.setRunningInSeparateThread(false);
                maskAlgo.calcInPlace25DC(paintBitmap, fillColor, timeSlice, rgbString, intensityLockVector);
            } else {
                if (imageA.getNDims() == 4) {
                    if (commitMasksAs4D) {
                        for (int i = 0; i < imageA.getExtents()[3]; i++) {
                            maskAlgo = new AlgorithmMask(imageA, intensityDropper, polarity, false);
                            maskAlgo.setRunningInSeparateThread(false);
                            maskAlgo.calcInPlace25D(paintBitmap, intensityDropper, i, intensityLockVector);
                        }
                    } else {
                        maskAlgo = new AlgorithmMask(imageA, intensityDropper, polarity, false);
                        maskAlgo.setRunningInSeparateThread(false);
                        maskAlgo.calcInPlace25D(paintBitmap, intensityDropper, timeSlice, intensityLockVector);
                    }
                } else {
                    maskAlgo = new AlgorithmMask(imageA, intensityDropper, polarity, false);
                    maskAlgo.setRunningInSeparateThread(false);
                    maskAlgo.calcInPlace25D(paintBitmap, intensityDropper, timeSlice, intensityLockVector);
                }

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

                if ( (intensityDropper < imgMinOrig) || (intensityDropper > imgMaxOrig)) {
                    LUTa.resetTransferLine(min, imgMin, max, imgMax);

                    if (imageA.getHistogramFrame() != null) {
                        imageA.getHistogramFrame().redrawFrames();
                    }
                }

            }
        }

        maskAlgo = null;

        if ( ( (imagesDone == ViewJComponentBase.IMAGE_B) || (imagesDone == ViewJComponentBase.BOTH))
                && (imageB != null)) {

            if (imageB.isColorImage() == true) {
                maskAlgo = new AlgorithmMask(imageB, fillColor, polarity, false);
                maskAlgo.setRunningInSeparateThread(false);
                maskAlgo.calcInPlace25DC(paintBitmap, fillColor, timeSlice, rgbString, intensityLockVector);
            } else {
                if (imageA.getNDims() == 4) {
                    if (commitMasksAs4D) {
                        for (int i = 0; i < imageA.getExtents()[3]; i++) {
                            maskAlgo = new AlgorithmMask(imageB, intensityDropper, polarity, false);
                            maskAlgo.setRunningInSeparateThread(false);
                            maskAlgo.calcInPlace25D(paintBitmap, intensityDropper, i, intensityLockVector);
                        }
                    } else {
                        maskAlgo = new AlgorithmMask(imageB, intensityDropper, polarity, false);
                        maskAlgo.setRunningInSeparateThread(false);
                        maskAlgo.calcInPlace25D(paintBitmap, intensityDropper, timeSlice, intensityLockVector);
                    }
                } else {
                    maskAlgo = new AlgorithmMask(imageB, intensityDropper, polarity, false);
                    maskAlgo.setRunningInSeparateThread(false);
                    maskAlgo.calcInPlace25D(paintBitmap, intensityDropper, timeSlice, intensityLockVector);
                }

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

                LUTb.resetTransferLine(min, max);
            }
        }

        if (clearPaintMask == true) {
            paintBitmap.clear();
        }
    }

    /**
     * Creates a new ubyte image from the paint mask.
     * 
     * @return the name of the new ubyte image
     */
    public String commitPaintToUbyteMask() {
        AlgorithmMask maskAlgo = null;
        Color fillColor = null;
        ModelImage imageACopy = null, imageBCopy = null;
        int length;
        int colorFactor;
        double[] buffer;
        double[] bufferI;
        int lengthUbyte;
        byte[] bufferUbyte;
        byte red, green, blue;
        int i;
        int end;

        final ViewJProgressBar progressBar = new ViewJProgressBar(imageActive.getImageName(), "Masking ...", 0, 100,
                true);
        progressBar.setSeparateThread(false);

        imageACopy = (ModelImage) imageA.clone();

        if (imageA.getNDims() == 2) {
            end = 1;
        } else if (imageA.getNDims() == 3) {
            end = imageA.getExtents()[2];
        } else {
            end = imageA.getExtents()[2] * imageA.getExtents()[3];
        }

        for (i = 0; i < end; i++) {
            (imageACopy.getFileInfo(i)).setModality(FileInfoBase.OTHER);
        }

        if (imageB != null) {
            imageBCopy = (ModelImage) imageB.clone();

            if (imageB.getNDims() == 2) {
                end = 1;
            } else if (imageB.getNDims() == 3) {
                end = imageB.getExtents()[2];
            } else {
                end = imageB.getExtents()[2] * imageB.getExtents()[3];
            }

            for (i = 0; i < end; i++) {
                (imageBCopy.getFileInfo(i)).setModality(FileInfoBase.OTHER);
            }
        }

        if (imageA.isColorImage() == true) {

            if (frame.getControls() != null) {
                fillColor = frame.getControls().getTools().getPaintColor();
            } else if (frameControls != null) {
                fillColor = frameControls.getTools().getPaintColor();
            } else {
                fillColor = new Color(128, 0, 0);
            }

            if (imageB == null) {
                maskAlgo = new AlgorithmMask(imageACopy, fillColor, true, false);
            } else {

                if (imageActive == imageA) {

                    // if( commitMode == IMAGE_A) {
                    maskAlgo = new AlgorithmMask(imageACopy, fillColor, true, false);
                } else {
                    maskAlgo = new AlgorithmMask(imageBCopy, fillColor, true, false);
                }
            }

            maskAlgo.setRunningInSeparateThread(false);
            maskAlgo.addProgressChangeListener(progressBar);
            progressBar.setVisible(ViewUserInterface.getReference().isAppFrameVisible());
            maskAlgo.calcInPlace25DCMask((BitSet) paintBitmap.clone(), fillColor, timeSlice);

        } else { // not color

            if (imageB == null) {
                maskAlgo = new AlgorithmMask(imageACopy, intensityDropper, true, false);
            } else {

                if (imageActive == imageA) {

                    // if( commitMode == IMAGE_A) {
                    maskAlgo = new AlgorithmMask(imageACopy, intensityDropper, true, false);
                } else {
                    maskAlgo = new AlgorithmMask(imageBCopy, intensityDropper, true, false);
                }
            }

            maskAlgo.setRunningInSeparateThread(false);
            maskAlgo.addProgressChangeListener(progressBar);
            progressBar.setVisible(ViewUserInterface.getReference().isAppFrameVisible());
            maskAlgo.calcInPlace25DMask((BitSet) paintBitmap.clone(), intensityDropper, timeSlice);
        } // not color

        if (imageACopy != null) {

            if (imageACopy.getType() != ModelStorageBase.UBYTE) {

                try {

                    if (imageACopy.isColorImage()) {
                        colorFactor = 4;
                    } else {
                        colorFactor = 1;
                    }

                    length = imageACopy.getSliceSize() * colorFactor;

                    if (imageACopy.getNDims() >= 3) {
                        length = length * imageACopy.getExtents()[2];
                    }

                    if (imageACopy.getNDims() == 4) {
                        length = length * imageACopy.getExtents()[3];
                    }

                    buffer = new double[length];

                    if (imageACopy.isComplexImage()) {
                        bufferI = new double[length];
                        imageACopy.exportDComplexData(0, length, buffer, bufferI);
                        imageACopy.reallocate(ModelStorageBase.UBYTE);

                        for (i = 0; i < length; i++) {
                            buffer[i] = Math.round(Math.sqrt( (buffer[i] * buffer[i]) + (bufferI[i] * bufferI[i])));
                        }

                        imageACopy.importData(0, buffer, true);
                    } else if (imageA.isColorImage()) {
                        imageACopy.exportData(0, length, buffer); // locks and releases lock
                        imageACopy.reallocate(ModelStorageBase.UBYTE);
                        lengthUbyte = length / 4;
                        bufferUbyte = new byte[lengthUbyte];
                        red = (byte) Math.round(fillColor.getRed());
                        green = (byte) Math.round(fillColor.getGreen());
                        blue = (byte) Math.round(fillColor.getBlue());

                        for (i = 0; i < lengthUbyte; i++) {

                            if ( (Math.round((byte) buffer[ (4 * i) + 1]) == red)
                                    && (Math.round((byte) buffer[ (4 * i) + 2]) == green)
                                    && (Math.round((byte) buffer[ (4 * i) + 3]) == blue)) {
                                bufferUbyte[i] = 1;
                            }
                        }

                        imageACopy.importData(0, bufferUbyte, true);
                    } else {
                        imageACopy.exportData(0, length, buffer); // locks and releases lock
                        imageACopy.reallocate(ModelStorageBase.UBYTE);
                        imageACopy.importData(0, buffer, true);
                    }
                } catch (final IOException error) {
                    buffer = null;
                    MipavUtil.displayError("IO Exception");

                    if (imageACopy != null) {
                        imageACopy.disposeLocal();
                        imageACopy = null;
                    }

                    return null;
                } catch (final OutOfMemoryError e) {
                    buffer = null;
                    MipavUtil.displayError("Out of memory error");

                    if (imageACopy != null) {
                        imageACopy.disposeLocal();
                        imageACopy = null;
                    }

                    return null;
                }
            } // if (imageACopy.getType != ModelStorageBase.UBYTE)
        } else {

            if (imageBCopy.getType() != ModelStorageBase.UBYTE) {

                try {

                    if (imageBCopy.isColorImage()) {
                        colorFactor = 4;
                    } else {
                        colorFactor = 1;
                    }

                    length = imageBCopy.getSliceSize() * colorFactor;

                    if (imageBCopy.getNDims() >= 3) {
                        length = length * imageBCopy.getExtents()[2];
                    }

                    if (imageBCopy.getNDims() == 4) {
                        length = length * imageBCopy.getExtents()[3];
                    }

                    buffer = new double[length];

                    if ( (imageBCopy.getType() == ModelStorageBase.COMPLEX)
                            || (imageBCopy.getType() == ModelStorageBase.DCOMPLEX)) {
                        bufferI = new double[length];
                        imageBCopy.exportDComplexData(0, length, buffer, bufferI);
                        imageBCopy.reallocate(ModelStorageBase.UBYTE);
                        imageBCopy.importData(0, buffer, true);
                    } else if (imageB.isColorImage()) {
                        imageBCopy.exportData(0, length, buffer); // locks and releases lock
                        imageBCopy.reallocate(ModelStorageBase.UBYTE);
                        lengthUbyte = length / 4;
                        bufferUbyte = new byte[lengthUbyte];
                        red = (byte) Math.round(fillColor.getRed());
                        green = (byte) Math.round(fillColor.getGreen());
                        blue = (byte) Math.round(fillColor.getBlue());

                        for (i = 0; i < lengthUbyte; i++) {

                            if ( (Math.round((byte) buffer[ (4 * i) + 1]) == red)
                                    && (Math.round((byte) buffer[ (4 * i) + 2]) == green)
                                    && (Math.round((byte) buffer[ (4 * i) + 3]) == blue)) {
                                bufferUbyte[i] = 1;
                            }
                        }

                        imageBCopy.importData(0, bufferUbyte, true);
                    } else {
                        imageBCopy.exportData(0, length, buffer); // locks and releases lock
                        imageBCopy.reallocate(ModelStorageBase.UBYTE);
                        imageBCopy.importData(0, buffer, true);
                    }
                } catch (final IOException error) {
                    buffer = null;
                    MipavUtil.displayError("IO Exception");

                    if (imageBCopy != null) {
                        imageBCopy.disposeLocal();
                        imageBCopy = null;
                    }

                    return null;
                } catch (final OutOfMemoryError e) {
                    buffer = null;
                    MipavUtil.displayError("Out of memory error");

                    if (imageBCopy != null) {
                        imageBCopy.disposeLocal();
                        imageBCopy = null;
                    }

                    return null;
                }
            } // if (imageBCopy.getType != ModelStorageBase.UBYTE)
        }

        try {

        	if (imageActive == imageA) {
        	
        		if (imageACopy != null) {
                    imageACopy.setImageName("Mask image");
                    imageACopy.clearMask();
                    new ViewJFrameImage(imageACopy, null, new Dimension(610, 200), false);
                }
            } else {

                if (imageBCopy != null) {
                    imageBCopy.setImageName("Mask image");
                    imageBCopy.clearMask();
                    new ViewJFrameImage(imageBCopy, null, new Dimension(610, 200), false);
                }
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: unable to open new frame");

            if (imageACopy != null) {
                imageACopy.disposeLocal();
                imageACopy = null;
            }

            if (imageBCopy != null) {
                imageBCopy.disposeLocal();
                imageBCopy = null;
            }

            return null;
        }

        if (imageActive == imageA && imageACopy != null) {
            return imageACopy.getImageName();
        } else if (imageBCopy != null) {
            return imageBCopy.getImageName();
        } else {
        	return "";
        }
    }

    /**
     * Creates a new short image from the paint mask.
     * 
     * @return the name of the new short image
     */
    public String commitPaintToMask() {
        AlgorithmMask maskAlgo = null;
        Color fillColor = null;
        ModelImage imageACopy = null, imageBCopy = null;
        int length;
        int colorFactor;
        double[] buffer;
        double[] bufferI;
        int lengthShort;
        short[] bufferShort;
        byte red, green, blue;
        int i;
        int end;

        final ViewJProgressBar progressBar = new ViewJProgressBar(imageActive.getImageName(), "Masking ...", 0, 100,
                true);
        progressBar.setSeparateThread(false);

        imageACopy = (ModelImage) imageA.clone();

        if (imageA.getNDims() == 2) {
            end = 1;
        } else if (imageA.getNDims() == 3) {
            end = imageA.getExtents()[2];
        } else {
            end = imageA.getExtents()[2] * imageA.getExtents()[3];
        }

        for (i = 0; i < end; i++) {
            (imageACopy.getFileInfo(i)).setModality(FileInfoBase.OTHER);
        }

        if (imageB != null) {
            imageBCopy = (ModelImage) imageB.clone();

            if (imageB.getNDims() == 2) {
                end = 1;
            } else if (imageB.getNDims() == 3) {
                end = imageB.getExtents()[2];
            } else {
                end = imageB.getExtents()[2] * imageB.getExtents()[3];
            }

            for (i = 0; i < end; i++) {
                (imageBCopy.getFileInfo(i)).setModality(FileInfoBase.OTHER);
            }
        }

        if (imageA.isColorImage() == true) {

            if (frame.getControls() != null) {
                fillColor = frame.getControls().getTools().getPaintColor();
            } else if (frameControls != null) {
                fillColor = frameControls.getTools().getPaintColor();
            } else {
                fillColor = new Color(128, 0, 0);
            }

            if (imageB == null) {
                maskAlgo = new AlgorithmMask(imageACopy, fillColor, true, false);
            } else {

                if (imageActive == imageA) {

                    // if( commitMode == IMAGE_A) {
                    maskAlgo = new AlgorithmMask(imageACopy, fillColor, true, false);
                } else {
                    maskAlgo = new AlgorithmMask(imageBCopy, fillColor, true, false);
                }
            }

            maskAlgo.setRunningInSeparateThread(false);
            maskAlgo.addProgressChangeListener(progressBar);
            progressBar.setVisible(ViewUserInterface.getReference().isAppFrameVisible());
            maskAlgo.calcInPlace25DCMask((BitSet) paintBitmap.clone(), fillColor, timeSlice);

        } else { // not color

            if (imageB == null) {
                maskAlgo = new AlgorithmMask(imageACopy, intensityDropper, true, false);
            } else {

                if (imageActive == imageA) {

                    // if( commitMode == IMAGE_A) {
                    maskAlgo = new AlgorithmMask(imageACopy, intensityDropper, true, false);
                } else {
                    maskAlgo = new AlgorithmMask(imageBCopy, intensityDropper, true, false);
                }
            }

            maskAlgo.setRunningInSeparateThread(false);
            maskAlgo.addProgressChangeListener(progressBar);
            progressBar.setVisible(ViewUserInterface.getReference().isAppFrameVisible());
            maskAlgo.calcInPlace25DMask((BitSet) paintBitmap.clone(), intensityDropper, timeSlice);
        } // not color

        if (imageACopy != null) {

            if (imageACopy.getType() != ModelStorageBase.SHORT) {

                try {

                    if (imageACopy.isColorImage()) {
                        colorFactor = 4;
                    } else {
                        colorFactor = 1;
                    }

                    length = imageACopy.getSliceSize() * colorFactor;

                    if (imageACopy.getNDims() >= 3) {
                        length = length * imageACopy.getExtents()[2];
                    }

                    if (imageACopy.getNDims() == 4) {
                        length = length * imageACopy.getExtents()[3];
                    }

                    buffer = new double[length];

                    if ( imageACopy.isComplexImage()) {
                        bufferI = new double[length];
                        imageACopy.exportDComplexData(0, length, buffer, bufferI);
                        imageACopy.reallocate(ModelStorageBase.SHORT);

                        for (i = 0; i < length; i++) {
                            buffer[i] = Math.round(Math.sqrt( (buffer[i] * buffer[i]) + (bufferI[i] * bufferI[i])));
                        }

                        imageACopy.importData(0, buffer, true);
                    } else if (imageA.isColorImage()) {
                        imageACopy.exportData(0, length, buffer); // locks and releases lock
                        imageACopy.reallocate(ModelStorageBase.SHORT);
                        lengthShort = length / 4;
                        bufferShort = new short[lengthShort];
                        red = (byte) Math.round(fillColor.getRed());
                        green = (byte) Math.round(fillColor.getGreen());
                        blue = (byte) Math.round(fillColor.getBlue());

                        for (i = 0; i < lengthShort; i++) {

                            if ( (Math.round((byte) buffer[ (4 * i) + 1]) == red)
                                    && (Math.round((byte) buffer[ (4 * i) + 2]) == green)
                                    && (Math.round((byte) buffer[ (4 * i) + 3]) == blue)) {
                                bufferShort[i] = 1;
                            }
                        }

                        imageACopy.importData(0, bufferShort, true);
                    } else {
                        imageACopy.exportData(0, length, buffer); // locks and releases lock
                        imageACopy.reallocate(ModelStorageBase.SHORT);
                        imageACopy.importData(0, buffer, true);
                    }
                } catch (final IOException error) {
                    buffer = null;
                    MipavUtil.displayError("IO Exception");

                    if (imageACopy != null) {
                        imageACopy.disposeLocal();
                        imageACopy = null;
                    }

                    return null;
                } catch (final OutOfMemoryError e) {
                    buffer = null;
                    MipavUtil.displayError("Out of memory error");

                    if (imageACopy != null) {
                        imageACopy.disposeLocal();
                        imageACopy = null;
                    }

                    return null;
                }
            } // if (imageACopy.getType != ModelStorageBase.SHORT)
        } else {

            if (imageBCopy.getType() != ModelStorageBase.SHORT) {

                try {

                    if (imageBCopy.isColorImage()) {
                        colorFactor = 4;
                    } else {
                        colorFactor = 1;
                    }

                    length = imageBCopy.getSliceSize() * colorFactor;

                    if (imageBCopy.getNDims() >= 3) {
                        length = length * imageBCopy.getExtents()[2];
                    }

                    if (imageBCopy.getNDims() == 4) {
                        length = length * imageBCopy.getExtents()[3];
                    }

                    buffer = new double[length];

                    if ( (imageBCopy.getType() == ModelStorageBase.COMPLEX)
                            || (imageBCopy.getType() == ModelStorageBase.DCOMPLEX)) {
                        bufferI = new double[length];
                        imageBCopy.exportDComplexData(0, length, buffer, bufferI);
                        imageBCopy.reallocate(ModelStorageBase.SHORT);
                        imageBCopy.importData(0, buffer, true);
                    } else if (imageB.isColorImage()) {
                        imageBCopy.exportData(0, length, buffer); // locks and releases lock
                        imageBCopy.reallocate(ModelStorageBase.SHORT);
                        lengthShort = length / 4;
                        bufferShort = new short[lengthShort];
                        red = (byte) Math.round(fillColor.getRed());
                        green = (byte) Math.round(fillColor.getGreen());
                        blue = (byte) Math.round(fillColor.getBlue());

                        for (i = 0; i < lengthShort; i++) {

                            if ( (Math.round((byte) buffer[ (4 * i) + 1]) == red)
                                    && (Math.round((byte) buffer[ (4 * i) + 2]) == green)
                                    && (Math.round((byte) buffer[ (4 * i) + 3]) == blue)) {
                                bufferShort[i] = 1;
                            }
                        }

                        imageBCopy.importData(0, bufferShort, true);
                    } else {
                        imageBCopy.exportData(0, length, buffer); // locks and releases lock
                        imageBCopy.reallocate(ModelStorageBase.SHORT);
                        imageBCopy.importData(0, buffer, true);
                    }
                } catch (final IOException error) {
                    buffer = null;
                    MipavUtil.displayError("IO Exception");

                    if (imageBCopy != null) {
                        imageBCopy.disposeLocal();
                        imageBCopy = null;
                    }

                    return null;
                } catch (final OutOfMemoryError e) {
                    buffer = null;
                    MipavUtil.displayError("Out of memory error");

                    if (imageBCopy != null) {
                        imageBCopy.disposeLocal();
                        imageBCopy = null;
                    }

                    return null;
                }
            } // if (imageBCopy.getType != ModelStorageBase.SHORT)
        }

        try {

            if (imageActive == imageA) {

                if (imageACopy != null) {
                    imageACopy.setImageName("Mask image");
                    imageACopy.clearMask();
                    new ViewJFrameImage(imageACopy, null, new Dimension(610, 200), false);
                }
            } else {

                if (imageBCopy != null) {
                    imageBCopy.setImageName("Mask image");
                    imageBCopy.clearMask();
                    new ViewJFrameImage(imageBCopy, null, new Dimension(610, 200), false);
                }
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: unable to open new frame");

            if (imageACopy != null) {
                imageACopy.disposeLocal();
                imageACopy = null;
            }

            if (imageBCopy != null) {
                imageBCopy.disposeLocal();
                imageBCopy = null;
            }

            return null;
        }

        if (imageActive == imageA && imageACopy != null) {
            return imageACopy.getImageName();
        } else if (imageBCopy != null) {
            return imageBCopy.getImageName();
        } else {
        	return "";
        }
    }

    /**
     * Deletes the selected contour of an VOI.

    public void deleteSelectedContours() {
        this.deleteSelectedContours( -1, false);
    }
     */
    /**
     * Deletes the selected contour of an VOI.
     * 
     * @param centerPtLocation DOCUMENT ME!
     * @param btest DOCUMENT ME!

    public void deleteSelectedContours(final int centerPtLocation, boolean btest) {
        int i, s, nVOI;

        final ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = 0; i < nVOI; i++) {

            if ( (VOIs.VOIAt(i).isActive() == true) && ( !btest || (i != centerPtLocation))) {

                break;
            } // Set i
        }

        if (i == nVOI) {
            MipavUtil.displayError("VOI must be selected.");

            return; // No VOI to delete
        }

        if (imageActive.getNDims() == 2) {
            voiHandler.deleteContour(VOIs.VOIAt(i), 0);
        } else if (imageActive.getNDims() >= 3) {

            for (s = 0; s < imageActive.getExtents()[2]; s++) {
                voiHandler.deleteContour(VOIs.VOIAt(i), s);
            }
        }

        // System.err.println("We are always going through this for deletion");

        if (VOIs.VOIAt(i).isEmpty() == true) {
            imageActive.unregisterVOI(VOIs.VOIAt(i));

            final int id = (getActiveImage().getVOIs().size() > 0) ? ( ( (getActiveImage().getVOIs().lastElement()))
                    .getID() + 1) : 0;
            final int lastUID = (getActiveImage().getVOIs().size() > 0) ? ( ( (getActiveImage().getVOIs().lastElement()))
                    .getUID() + 1)
                    : -1;

            voiHandler.updateVOIColor(id, lastUID);

            System.err.println( "ViewJComponentEditImage.deleteSelectedContours voiHandler" );
            voiHandler.setVOI_ID( -1);
        }

        imageActive.notifyImageDisplayListeners(null, true);
    }     */

    /**
     * Sets all variables to null, disposes, and garbage collects.
     * 
     * @param flag if true garbage collector should be called.
     */
    public void disposeLocal(final boolean flag) {
        imageBufferA = null;
        imageBufferB = null;
        pixBuffer = null;
        pixBufferB = null;
        paintBitmap = null;
        paintBitmapBU = null;
        imageActive = null;
        imageBufferActive = null;
        frame = null;
        frameControls = null;
        imageA = null;
        imageB = null;

        cleanImageBufferA = null;
        cleanImageBufferB = null;

        if (paintImage != null) {
            paintImage.flush();
            paintImage = null;
        }

        if (paintImagePrevious != null) {
            paintImagePrevious.flush();
            paintImagePrevious = null;
        }

        if (imageStatList != null) {

            // removeVOIUpdateListener(imageStatList);
            imageStatList.dispose();
            imageStatList = null;
        }

        if (magSettings != null) {
            magSettings.dispose();
            magSettings = null;
        }

        //if (voiHandler != null) {
        //    voiHandler.disposeLocal(flag);
        //}

        //voiHandler = null;

        if (growDialog != null) {

            if (growDialog instanceof JDialogBase) {
                ((JDialogBase) growDialog).dispose();
            }

            growDialog = null;
        }

        if (checkerDialog != null) {
            checkerDialog.dispose();
            checkerDialog = null;
        }

        LUTa = null;
        LUTb = null;

        if (flag == true) {
            super.disposeLocal();
        }

    }

    /**
     * Draws the gradicules (the half-image lines which demarcate lengths in the image).
     * 
     * @param g The graphics object which will do the painting.
     * @param xRes The resolutions in the <i>x</i>-direction.
     * @param yRes The resolutions in the <i>y</i>-direction.
     */
    public void drawGradicules(final Graphics g, final float xRes, final float yRes) {
        final Insets insets = frame.getInsets();
        final int rightOffset = getBounds().width - insets.left;
        final int bottomOffset = getBounds().height - insets.bottom - 15;

        // Draw gradicules
        if ( (getZoomX() >= 1.0) && (getZoomY() >= 1.0)) {
            final float XCMPerPix = (float) (xRes / 10.0 / getZoomX());
            final float YCMPerPix = (float) (yRes / 10.0 / getZoomY());

            final int XGradcmLen = (int) ( (getBounds().width / 3.0) * (XCMPerPix));
            final int YGradcmLen = (int) ( (getBounds().height / 3.0) * (YCMPerPix));

            final int XGradpixLen = (int) (XGradcmLen / XCMPerPix);
            final int YGradpixLen = (int) (YGradcmLen / XCMPerPix);

            g.setColor(Color.white);
            g.drawLine(XGradpixLen, bottomOffset - 30, XGradpixLen, bottomOffset - 43);
            g.drawLine(XGradpixLen + (int) (XGradcmLen / XCMPerPix), bottomOffset - 30, XGradpixLen
                    + (int) (XGradcmLen / XCMPerPix), bottomOffset - 43);
            g.drawLine(rightOffset - 22, YGradpixLen, rightOffset - 35, YGradpixLen);
            g.drawLine(rightOffset - 22, YGradpixLen + (int) (YGradcmLen / YCMPerPix), rightOffset - 35, YGradpixLen
                    + (int) (YGradcmLen / YCMPerPix));

            g.setColor(Color.black);
            g.drawLine(XGradpixLen + 1, bottomOffset - 30, XGradpixLen + 1, bottomOffset - 43);
            g.drawLine(XGradpixLen - 1, bottomOffset - 30, XGradpixLen - 1, bottomOffset - 43);
            g.drawLine(XGradpixLen + (int) (XGradcmLen / XCMPerPix) + 1, bottomOffset - 30, XGradpixLen
                    + (int) (XGradcmLen / XCMPerPix) + 1, bottomOffset - 43);
            g.drawLine(XGradpixLen + (int) (XGradcmLen / XCMPerPix) - 1, bottomOffset - 30, XGradpixLen
                    + (int) (XGradcmLen / XCMPerPix) - 1, bottomOffset - 43);
            g.drawLine(rightOffset - 22, YGradpixLen + 1, rightOffset - 35, YGradpixLen + 1);
            g.drawLine(rightOffset - 22, YGradpixLen - 1, rightOffset - 35, YGradpixLen - 1);
            g.drawLine(rightOffset - 22, YGradpixLen + (int) (YGradcmLen / YCMPerPix) + 1, rightOffset - 35,
                    YGradpixLen + (int) (YGradcmLen / YCMPerPix) + 1);
            g.drawLine(rightOffset - 22, YGradpixLen + (int) (YGradcmLen / YCMPerPix) - 1, rightOffset - 35,
                    YGradpixLen + (int) (YGradcmLen / YCMPerPix) - 1);

            for (int i = 1; i < XGradcmLen; i++) {
                g.setColor(Color.white);
                g.drawLine(XGradpixLen + (int) (i / XCMPerPix), bottomOffset - 30, XGradpixLen + (int) (i / XCMPerPix),
                        bottomOffset - 35);
                g.setColor(Color.black);
                g.drawLine(XGradpixLen + (int) (i / XCMPerPix) + 1, bottomOffset - 30, XGradpixLen
                        + (int) (i / XCMPerPix) + 1, bottomOffset - 35);
                g.drawLine(XGradpixLen + (int) (i / XCMPerPix) - 1, bottomOffset - 30, XGradpixLen
                        + (int) (i / XCMPerPix) - 1, bottomOffset - 35);
            }

            for (int i = 1; i < YGradcmLen; i++) {
                g.setColor(Color.white);
                g.drawLine(rightOffset - 22, YGradpixLen + (int) (i / YCMPerPix), rightOffset - 27, YGradpixLen
                        + (int) (i / YCMPerPix));
                g.setColor(Color.black);
                g.drawLine(rightOffset - 22, YGradpixLen + (int) (i / YCMPerPix) + 1, rightOffset - 27, YGradpixLen
                        + (int) (i / YCMPerPix) + 1);
                g.drawLine(rightOffset - 22, YGradpixLen + (int) (i / YCMPerPix) - 1, rightOffset - 27, YGradpixLen
                        + (int) (i / YCMPerPix) - 1);
            }

            g.setColor(Color.black);
            g.drawLine(rightOffset - 20, YGradpixLen, rightOffset - 20, YGradpixLen * 2);
            g.setColor(Color.white);
            g.drawLine(rightOffset - 21, YGradpixLen, rightOffset - 21, YGradpixLen * 2);
            g.setColor(Color.black);
            g.drawLine(rightOffset - 22, YGradpixLen, rightOffset - 22, YGradpixLen * 2);

            g.drawLine(XGradpixLen, bottomOffset - 28, XGradpixLen * 2, bottomOffset - 28);
            g.setColor(Color.white);
            g.drawLine(XGradpixLen, bottomOffset - 29, XGradpixLen * 2, bottomOffset - 29);
            g.setColor(Color.black);
            g.drawLine(XGradpixLen, bottomOffset - 30, XGradpixLen * 2, bottomOffset - 30);
        }
    }

    /**
     * Draws a white character on a black surround.
     * 
     * @param str string to be displayed
     * @param g graphics contexts (where to draw the string)
     * @param x x coordinate of where the string is to be drawn
     * @param y y coordinate of where the string is to be drawn
     */
    public final void drawStringBW(final String str, final Graphics g, final int x, final int y) {

        if (str == null) {
            return;
        }

        g.setColor(Color.black);
        g.drawString(str, x, y + 1);
        g.drawString(str, x, y - 1);
        g.drawString(str, x + 1, y);
        g.drawString(str, x - 1, y);
        g.setColor(Color.white);
        g.drawString(str, x, y);
    }

    /**
     * Erases all paint.
     * 
     * @param onlyCurrent DOCUMENT ME!
     */
    public void eraseAllPaint(final boolean onlyCurrent) {

        if (onlyCurrent) {

            // find start point
            final int currentFrame = imageA.getParentFrame().getViewableSlice();
            final int sliceSize = imageA.getSliceSize();
            final int startPt = currentFrame * sliceSize;
            final int endPt = startPt + sliceSize;

            for (int i = startPt; i < endPt; i++) {
                paintBitmap.clear(i);
            }
        } else {
            paintBitmap.clear();
        }

        if (seedPaintBitmap != null) {
            seedPaintBitmap.clear();
            seedVal = (float) imageActive.getMin();
            this.saveValue = seedVal;
            saveX = saveY = saveZ = xPG = yPG = -1;
        }

        if (growDialog != null) {
            growDialog.notifyPaintListeners(false, false, paintBitmap);
        } else {
            imageActive.notifyImageDisplayListeners(null, true);
        }
    }

    /**
     * Returns the active image.
     * 
     * @return active image
     */
    public ModelImage getActiveImage() {
        return imageActive;
    }

    /**
     * Returns the active image buffer.
     * 
     * @return active image buffer
     */
    public float[] getActiveImageBuffer() {
        return imageBufferActive;
    }

    /**
     * Returns the active image buffer for this slice.
     * 
     * @return active image buffer for this slice
     */
    public float[] getActiveImageSliceBuffer() {
        int cFactor = 1;

        if (imageActive.isColorImage() == true) {
            cFactor = 4;
        }

        final float[] sliceBuffer = new float[cFactor * imageExtents[0] * imageExtents[1]];

        if (imageActive.isColorImage() == true) {

            try {
                imageActive.exportData(0, sliceBuffer.length, sliceBuffer);
            } catch (final IOException error) {
                MipavUtil.displayError("Error while trying to retrieve RGB data.");

                return null;
            }
        } else {

            for (int i = 0; i < sliceBuffer.length; i++) {
                sliceBuffer[i] = imageBufferActive[i];
            }
        }

        return sliceBuffer;
    }

    /**
     * Returns the active image.
     * 
     * @return active image
     */
    public ModelLUT getActiveLUT() {
        return (ModelLUT) m_kPatientSlice.getActiveLookupTable();
    }

    /**
     * Returns the active image.
     * 
     * @return active image
     */
    public ModelRGB getActiveRGB() {
        return (ModelRGB) m_kPatientSlice.getActiveLookupTable();
    }

    /**
     * Returns float alphaBlend. The value used to blend two images displayed in the same frame.
     * 
     * @return alphaBlend
     */
    public float getAlphaBlend() {
        return alphaBlend;
    }

    /**
     * Returns the VOI mode.
     * 
     * @return drawing mode for the VOI tools (i.e. ELLIPSE, LINE ...)
     */
    public int getCursorMode() {
        return cursorMode;
    }

    /**
     * Returns the frame.
     * 
     * @return frame
     */
    public ViewJFrameBase getFrame() {
        return frame;
    }

    /**
     * Returns the imageA.
     * 
     * @return imageA
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Returns the imageB.
     * 
     * @return imageB
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Gets imageA's image buffer of intensities.
     * 
     * @return float[] imageA's intensity buffer
     */
    public float[] getImageBufferA() {
        return imageBufferA;
    }

    /**
     * Gets imageB's image buffer of intensities.
     * 
     * @return float[] imageB's intensity buffer
     */

    public float[] getImageBufferB() {
        return imageBufferB;
    }

    /**
     * Gets the intensityDropper intensity.
     * 
     * @return the intensity dropper
     */
    public float getIntensityDropper() {
        return intensityDropper;
    }

    /**
     * Returns the model lut for the imageA.
     * 
     * @return the model LUT for imageA
     */
    public ModelLUT getLUTa() {
        return LUTa;
    }

    /**
     * Returns the model lut for the imageB.
     * 
     * @return the model LUT for imageB
     */
    public ModelLUT getLUTb() {
        return LUTb;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return boolean
     */
    public boolean getModifyFlag() {
        return modifyFlag;
    }

    /**
     * Finds the number of points in the active VOI contour.
     * 
     * @return the number of points in the selected VOI
     */
    public int getNumPoints() {
        final ViewVOIVector VOIs = imageActive.getVOIs();

        if (VOIs.size() == 0) {
            return 0;
        }
        return VOIs.elementAt(0).getCurves().size();
    }

    /**
     * Returns the number of VOIs in the active image.
     * 
     * @return int number of VOIs
     */
    public int getnVOI() {
        final ViewVOIVector VOIs = imageActive.getVOIs();

        return (VOIs.size());
    }

    /**
     * Returns float opacity.
     * 
     * @return opacity
     */
    public float getOpacity() {
        return frame.getControls().getTools().getOpacity();
    }

    /**
     * Returns the image's orientation.
     * 
     * @return image orientation
     */
    public int getOrientation() {
        return orientation;
    }

    /**
     * Returns BitSet paintBitmap.
     * 
     * @return paintBitmap
     */
    public BitSet getPaintBitmap() {
        return paintBitmap;
    }

    /**
     * Gets the paint buffer.
     * 
     * @return int[] paint buffer
     */
    public int[] getPaintBuffer() {
        return paintImageBuffer;
    }

    /**
     * Gets the paint mask.
     * 
     * @return the current paint mask
     */
    public BitSet getPaintMask() {
        return paintBitmap;
    }

    /**
     * Gets the buffer used to store ARGB images of the image presently being displayed.
     * 
     * @return int[] buffer of ARGB image A
     */
    public int[] getPixBuffer() {
        return pixBuffer;
    }

    /**
     * Gets the buffer used to store ARGB images of the image presently being displayed.
     * 
     * @return int[] buffer of ARGB image A
     */
    public int[] getPixBufferB() {
        return pixBufferB;
    }

    /**
     * Returns the ModelRGB RGBTA for imageA.
     * 
     * @return RGBTA for imageA
     */
    public ModelRGB getRGBTA() {
        return RGBTA;
    }

    /**
     * Returns the ModelRGB for imageB.
     * 
     * @return RGBTB for imageB
     */
    public ModelRGB getRGBTB() {
        return RGBTB;
    }

    /**
     * Determines whether to enable the showIntensity checkbox for magnification box.
     * 
     * @return whether to enable showIntensity checkbox
     */
    public boolean getShowMagIntensityEnabled(int width, int height, float mag) {
        final Graphics g = getGraphics();

        if (g == null) {
            return false;
        }

        g.setFont(MipavUtil.font10);

        return super.getShowMagIntensityEnabled(g, width, height, mag, imageActive.getType(),
                getActiveImage().getMin(), getActiveImage().getMax());

    }

    /**
     * Returns the slice of the image.
     * 
     * @return the slice
     */
    public int getSlice() {
        return slice;
    }

    /**
     * Gets the time slice of the image.
     * 
     * @return the current time slice
     */
    public int getTimeSlice() {
        return timeSlice;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public VOIHandlerInterface getVOIHandler() {
        return this.voiManager.getParent();
    }

    /**
     * Tells whether or not the image is being displayed in checkerboard mode.
     * 
     * @return boolean
     */
    public boolean isCheckerboarded() {
        return ( (nRowCheckers > 0) && (nColumnCheckers > 0));
    }

    // ************************************************************************
    // **************************** Key Events *****************************
    // ************************************************************************

    /**
     * Reads in an image icon from either the set of mipav icons or the user's $HOME/mipav/brushes/ directory.
     * 
     * @param imageName The
     * @return
     * @throws IOException
     */
    private BufferedImage loadImageFile(String imageName) throws IOException {
        URL imageURL = null;

        if (imageName == null) {
            imageName = "square 8x8.gif";
        }

        imageURL = PlaceHolder.class.getResource(imageName);

        if (imageURL == null) {
            final File imageFile = new File(System.getProperty("user.home") + File.separator + "mipav" + File.separator
                    + "brushes" + File.separator + imageName);
            if (imageFile.exists() && imageFile.canRead()) {
                imageURL = imageFile.toURI().toURL();
            }
        }

        return ImageIO.read(imageURL);
    }

    /**
     * Loads built-in (.gif) or custom (.png) paint brushes based on the last-used paint brush in preferences.
     * 
     * @param paintName the name of the brush to load
     * @param isQuick DOCUMENT ME!
     */
    public void loadPaintBrush(final String paintName, final boolean isQuick) {

        if (isQuick) {
            quickSwitchBrush();
        }

        BufferedImage tempImage = null;
        try {
            tempImage = loadImageFile(paintName);
        } catch (final IOException e) {
            MipavUtil.displayError("Could not load paint brush image: " + paintName);
            e.printStackTrace();
            return;
        }

        if (tempImage == null) {
            MipavUtil.displayError("Could not load paint brush image: " + paintName);
            return;
        }
        
        if (!(this instanceof ViewJComponentTriImage)) {
	        int tempImageWidth = tempImage.getWidth();
	        int tempImageHeight = tempImage.getHeight();
	        int tempImageType = tempImage.getType();
	        float res0 = imageA.getFileInfo()[0].getResolutions()[0];
	        float res1 = imageA.getFileInfo()[0].getResolutions()[1];
	        if ((res0 > res1) && (res0/res1 <= 10.0f)) {
	        	tempImageWidth = (int)Math.round(tempImageWidth * res0/res1);
	        	tempImage = null;
	        	tempImage = new BufferedImage(tempImageWidth, tempImageHeight, tempImageType);
	        }
	        if ((res1 > res0) && (res1/res0 <= 10.0f)) {
	        	tempImageHeight = (int)Math.round(tempImageHeight * res1/res0);
	        	tempImage = null;
	        	tempImage = new BufferedImage(tempImageWidth, tempImageHeight, tempImageType);
	        }
        } // if ((!(this instanceof ViewJComponentTriImage))

        // create the bitset and the brush dimensions
        paintBrushDim = new Dimension(tempImage.getWidth(), tempImage.getHeight());

        paintBrush = new BitSet(paintBrushDim.width * paintBrushDim.height);

        final int numBands = tempImage.getData().getNumBands();
        final int[] pixel = new int[numBands];
        for (int y = 0, pix = 0; y < paintBrushDim.height; y++) {
            for (int x = 0; x < paintBrushDim.width; x++, pix++) {
                // calling getPixel is required to get this to work for both .png and .gif images.
                tempImage.getData().getPixel(x, y, pixel);

                boolean doSetPixel = true;
                // if any of the pixel bands has a non-zero value, then the pixel shouldn't be used for painting
                for (int b = 0; b < numBands; b++) {
                    if (pixel[b] != 0) {
                        doSetPixel = false;
                    }
                }

                if (doSetPixel) {
                    paintBrush.set(pix);
                }
            }
        }

        paintBuffer = null;
        tempImage.flush();

        if (paintImage != null) {
            paintImage.flush();
            paintImage = null;
        }
        // create a buffered image that will be drawn in place of a cursor (with red transparent pixels)
        paintImage = new BufferedImage(paintBrushDim.width, paintBrushDim.height, BufferedImage.TYPE_INT_ARGB);

        updatePaintBrushCursor();
    }

    // ************************************************************************
    // ***************************** Mouse Events *****************************
    // ************************************************************************

    /**
     * A mouse event. When the mouse is clicked in the image, several different things may happen. If a Volume of
     * Interest (VOI) is selected and the click count is 2, a VOI dialog should pop up. If the click count is 1 and the
     * mouse is in an VOI, it should select the VOI. In all other cases, a click within the image but not in an VOI
     * should deselect all VOIs.
     * 
     * @param mouseEvent event that triggers function; contains click count
     */
    public void mouseClicked(final MouseEvent mouseEvent) {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        if (frame instanceof ViewJFrameLightBox) {

            // on a double click, or a single click from the
            // right mouse button (indicated by the META modifier)
            // .. set the current image slice
            // on a single click from the left mouse button, select
            // the individual lightbox pane
            if (mouseEvent.getClickCount() == 2) {
                ((ViewJFrameLightBox) frame).updateImageSlice(slice, timeSlice);

                return;
            } else if (mouseEvent.getClickCount() == 1) {

                // check to see if this event was consumed -- just in case
                // the double click is also showing a single click
                if (mouseEvent.isConsumed()) {
                    return;
                }

                if (mouseEvent.isMetaDown()) { // right mouse click
                    ((ViewJFrameLightBox) frame).updateImageSlice(slice, timeSlice);
                } else {
                    ((ViewJFrameLightBox) frame).updateImageSelection(slice, mouseEvent.isShiftDown());
                }

                return;
            }

        }
    }

    // ************************************************************************
    // ************************** Mouse Motion Events *************************
    // ************************************************************************

    /**
     * A mouse event. Drags a VOI real time by calling a series of translations and draws. Once the mouse is released,
     * the positions are reset permenantly. Also rubberbands points if the cursor indicates it by calling rubberbandVOI.
     * 
     * @param mouseEvent event that triggered function
     */
    public void mouseDragged(final MouseEvent mouseEvent) {

        final int mouseMods = mouseEvent.getModifiers();

        int xS, yS;
        Color dropperColor;
        int diffX = Math.round((lastMouseX - mouseEvent.getX()) * getZoomX());
        int diffY = Math.round((lastMouseY - mouseEvent.getY()) * getZoomY());
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        if ( (pixBuffer == null) || (imageBufferActive == null) || (modifyFlag == false)) {
            return;
        }

        xS = getScaledX(mouseEvent.getX()); // zoomed x. Used as cursor
        yS = getScaledY(mouseEvent.getY()); // zoomed y. Used as cursor

        final int xDim = imageActive.getExtents()[0];
        final int yDim = imageActive.getExtents()[1];

        if ( (xS < 0) || (xS >= xDim) || (yS < 0) || (yS >= yDim)) {
            return;
        }

        processDefaultMouseDrag(mouseEvent, xS, yS);
        if ( cursorMode == ViewJComponentBase.DEFAULT && ((mouseEvent.getModifiers() & InputEvent.BUTTON3_MASK) != 0) )
        {
        	return;
        }
        
        int scrollX = 0;
    	int scrollY = 0;
    	if (frame instanceof ViewJFrameImage) {
    		scrollX = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().width;
    		scrollY = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().height;
    	}

        
    	float imageX = imageActive.getExtents()[0]*getZoomX();
    	float imageY = imageActive.getExtents()[1]*getZoomY();

        if (cursorMode == ViewJComponentBase.DROPPER_PAINT) {
            if (imageActive.isColorImage() == true) {
                dropperColor = new Color(
                        (int) imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 1],
                        (int) imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 2],
                        (int) imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 3]);
                frame.getControls().getTools().setPaintColor(dropperColor);
            } else if(imageActive.isComplexImage()) {
                int loc = ((yS * imageActive.getExtents()[0]) + xS)*2;
                intensityDropper = (float) Math.sqrt(imageBufferActive[loc]*imageBufferActive[loc] + imageBufferActive[loc+1]*imageBufferActive[loc+1]);
                frame.getControls().getTools().setIntensityPaintName(String.valueOf((int) (intensityDropper)));
            } else {
                intensityDropper = imageBufferActive[ (yS * imageActive.getExtents()[0]) + xS];
                frame.getControls().getTools().setIntensityPaintName(String.valueOf((int) (intensityDropper)));
            }
        } else if (cursorMode == ViewJComponentBase.ERASER_PAINT) {
            performPaint(mouseEvent, true);
            if (imageActive.getTriImageFrame() == null) {
                imageActive.notifyImageDisplayListeners();
            } else {
                imageActive.notifyImageDisplayListeners_notTriFrame();
            }
        } else if (cursorMode == ViewJComponentBase.PAINT_VOI) {
            performPaint(mouseEvent, mouseMods == InputEvent.BUTTON3_MASK);
            if (imageActive.getTriImageFrame() == null) {
                imageActive.notifyImageDisplayListeners();
            } else {
                imageActive.notifyImageDisplayListeners_notTriFrame();
            }
        } else if(scrollX<imageX || scrollY<imageY) {
        	if(cursorMode == ViewJComponentBase.DEFAULT && mouseEvent.isShiftDown()) {
        		setCursor(new Cursor(Cursor.HAND_CURSOR));
        		int currentScrollX = ((ViewJFrameImage) frame).getScrollPane().getHorizontalScrollBar().getValue();
        		int currentScrollY = ((ViewJFrameImage) frame).getScrollPane().getVerticalScrollBar().getValue();

        		if(mouseEvent.getWhen() - lastMouseEvent > 100) {
	        		lastMouseEvent = mouseEvent.getWhen();
	        		 ((ViewJFrameImage) frame).getScrollPane().getHorizontalScrollBar().setValue(currentScrollX + diffX);
	        		 ((ViewJFrameImage) frame).getScrollPane().getVerticalScrollBar().setValue(currentScrollY + diffY);
        		} 
 
        	}
        }
    }

    /**
     * Store the lastMouse position.
     * 
     * @param mouseEvent event
     */
    public void mouseEntered(final MouseEvent mouseEvent) {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();
    }

    /**
     * Resets the level set stack.
     * 
     * @param mouseEvent event that triggered function
     */
    public void mouseExited(final MouseEvent mouseEvent) {
        lastMouseX = OUT_OF_BOUNDS;
        lastMouseY = OUT_OF_BOUNDS;

        if ( (cursorMode == ViewJComponentBase.MAG_REGION) || (cursorMode == ViewJComponentBase.PAINT_VOI)
                || (cursorMode == ViewJComponentBase.ERASER_PAINT) || (cursorMode == ViewJComponentBase.WIN_REGION)) {

            // repaint();
            paintComponent(getGraphics());
        }

        //if (cursorMode == ViewJComponentBase.DEFAULT) {
            intensityLabel = false;
            paintComponent(getGraphics());
        //}
    }

    /**
     * A mouse event. If the mode is level set, draws level sets as user moves mouse. Otherwise, changes the cursor
     * depending on where the mouse is in relation to the VOI.
     * 
     * @param mouseEvent event that triggered the function
     */
    public void mouseMoved(final MouseEvent mouseEvent) {
        int xS, yS;

        final Graphics g = getGraphics();
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();
        shiftDown = mouseEvent.isShiftDown();

        if ( (cursorMode == ViewJComponentBase.ZOOMING_IN) || (cursorMode == ViewJComponentBase.ZOOMING_OUT)) {

            // if we are in zoom mode, we don't care about any of the other things
            // that are happening here, in fact, zoom breaks if we don't return
            return;
        } 
                
        xS = getScaledX(mouseEvent.getX()); // zoomed x. Used as cursor
        yS = getScaledY(mouseEvent.getY()); // zoomed y. Used as cursor

        if ( (cursorMode == ViewJComponentBase.PAINT_VOI) && mouseEvent.isShiftDown()) {
            performPaint(mouseEvent, false);
            imageActive.notifyImageDisplayListeners(null, true);

            return;
        }

        // the user can erase by holding down shift while in eraser mode
        // or by holding down control while in paint mode
        if ( ( (cursorMode == ViewJComponentBase.ERASER_PAINT) && mouseEvent.isShiftDown())
                || ( (cursorMode == ViewJComponentBase.PAINT_VOI) && mouseEvent.isControlDown())) {
            performPaint(mouseEvent, true);
            imageActive.notifyImageDisplayListeners(null, true);

            return;
        }

        if ( (g == null) || (modifyFlag == false) || (slice == -99) || (imageActive == null)) {
            return;
        }

        if ( (pixBuffer == null) || (imageBufferActive == null)) {
            g.dispose();

            return;
        }

        if ( imageActive == null )
        {
            setActiveImage(ViewJComponentBase.IMAGE_A);
        }
        if ( (xS < 0) || (xS >= imageActive.getExtents()[0]) || // Check to ensure point is within
                (yS < 0) || (yS >= imageActive.getExtents()[1])) { // the image bounds
            g.dispose();

            return;
        }

        if (cursorMode == ViewJComponentBase.MAG_REGION) {
            repaint();

            return;
        } else if (cursorMode == ViewJComponentBase.WIN_REGION) {
            repaint();

            return;
        } else if ( (cursorMode == ViewJComponentBase.PAINT_VOI) || (cursorMode == ViewJComponentBase.ERASER_PAINT)) {

            // repaint();
            paintComponent(getGraphics());

            return;
        } else if ( (cursorMode == ViewJComponentBase.PAINT_CAN) || (cursorMode == ViewJComponentBase.PAINT_VASC)) {

            if (growDialog != null) {

                if (imageActive.isColorImage()) {
                    growDialog.setPositionText("  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS) + "  R:  "
                            + String.valueOf(imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 1])
                            + "  G:  "
                            + String.valueOf(imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 2])
                            + "  B:  "
                            + String.valueOf(imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 3]));
                } else if(imageActive.isComplexImage()) { 
                    int loc = ((yS * imageActive.getExtents()[0]) + xS)*2;
                    float magValue = (float) Math.sqrt(imageBufferActive[loc]*imageBufferActive[loc] + imageBufferActive[loc+1]*imageBufferActive[loc+1]);
                    growDialog.setPositionText("  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS)
                            + "  Intensity:  "+ String.valueOf(magValue));
                } else {
                    growDialog.setPositionText("  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS)
                            + "  Intensity:  "
                            + String.valueOf(imageBufferActive[ (yS * imageActive.getExtents()[0]) + xS]));
                }
            }

            g.dispose();

            return;
        }
        if ((cursorMode != ViewJComponentBase.VOI_3D) && (cursorMode != ViewJComponentBase.DROPPER_PAINT))
        {
            setCursorMode(ViewJComponentBase.DEFAULT);
        }
    } // end mouseMoved

    /**
     * A mouse event. Sets the mode of the program depending on the cursor mode. If the mode is move, activates the
     * contour or line and enables the delete button.
     * 
     * @param mouseEvent event that triggered function
     */
    public void mousePressed(final MouseEvent mouseEvent) {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        final int xS = getScaledX(mouseEvent.getX());
        final int yS = getScaledY(mouseEvent.getY());

        if (modifyFlag == false) {
            return;
        }

        // save the state of the shift button
        mousePressIsShiftDown = mouseEvent.isShiftDown();

        // shows intsnsity label upon mouse press
        if (cursorMode == ViewJComponentBase.DEFAULT || cursorMode == ViewJComponentBase.VOI_3D) {
            setPixelInformationAtLocation(xS, yS, mouseEvent.isControlDown());

            if (mouseEvent.getButton() == MouseEvent.BUTTON1) {
                intensityLabel = Preferences.is(Preferences.PREF_SHOW_INTENSITY_ON_LEFT_CLICK);
                paintComponent(getGraphics());
            }
        }

        if ( (cursorMode == ViewJComponentBase.DEFAULT) && mouseEvent.isControlDown()) { // center the image around
            // cursor (no zooming)

            final int centerX = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().width / 2;
            final int centerY = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().height / 2;

            ((ViewJFrameImage) frame).getScrollPane().getHorizontalScrollBar().setValue(mouseEvent.getX() - centerX);
            ((ViewJFrameImage) frame).getScrollPane().getVerticalScrollBar().setValue(mouseEvent.getY() - centerY);

        }

        if ( (cursorMode == ViewJComponentBase.ZOOMING_IN) || (cursorMode == ViewJComponentBase.ZOOMING_OUT)) {
            // int xS = getScaledX(mouseEvent.getX()); // zoomed x. Used as cursor int yS =
            // getScaledY(mouseEvent.getY()); // zoomed y. Used as cursor
        	getActiveImage().updateVOIs();
            ((ViewJFrameImage) frame).updateFrame(getZoomMagnitudeX(mouseEvent.getButton() == MouseEvent.BUTTON3),
                    getZoomMagnitudeY(mouseEvent.getButton() == MouseEvent.BUTTON3), xS, yS);

            if (mouseEvent.isShiftDown() == false) {
                cursorMode = ViewJComponentBase.DEFAULT;
                setCursor(MipavUtil.defaultCursor);
            }

            return;
        }

        try {
            mousePressedPaint(mouseEvent);

            if ( (cursorMode == ViewJComponentBase.WIN_REGION)
                    && (mouseEvent.getModifiers() == InputEvent.BUTTON3_MASK)) {
                final String newValue = JOptionPane.showInputDialog(frame, "Enter new size for windowed region:",
                        String.valueOf(windowedRegionSize));

                try {

                    if (newValue != null) {
                        windowedRegionSize = Integer.parseInt(newValue);
                    }
                } catch (final NumberFormatException nfe) {
                    MipavUtil.displayError("Invalid size entered for windowed region.");
                }
            }
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.mousePressed");
            setCursorMode(ViewJComponentBase.DEFAULT);

            return;
        }
    }

    /**
     * A mouse event. This function sets up and draws the VOI according to the mode.
     * 
     * @param mouseEvent event that triggered function
     */
    public void mouseReleased(final MouseEvent mouseEvent) {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();
        
      //updates winlevel if right mouse button was pressed and user's preferences indicate this should occur
        if ( ((mouseEvent.getModifiers() & InputEvent.BUTTON3_MASK) != 0) && 
                Preferences.is(Preferences.PREF_SHOW_WINLEV_ON_RIGHT_CLICK)) {
            if (winLevelSet) {
                winLevelSet = false;
            }
            
        }

        if (wasDragging) {
            wasDragging = false;
            return;
        }

        if (modifyFlag == false) {
            return;
        }

        int xS = getScaledX(mouseEvent.getX()); // zoomed x. Used as cursor
        int yS = getScaledY(mouseEvent.getY()); // zoomed y. Used as cursor

        if ( (xS < 0) || (xS >= imageActive.getExtents()[0]) || (yS < 0) || (yS >= imageActive.getExtents()[1])) {
            return;
        }

        setPixelInformationAtLocation(xS, yS, false);
        
        // clicking with the right mouse button in a regular image frame updates the image's
        // tri-image frame (if one is open) to show that point in all of the components
        if ( (mouseEvent.getModifiers() & InputEvent.BUTTON2_MASK) != 0) {
            final ViewJFrameTriImage triFrame = imageActive.getTriImageFrame();

            if (triFrame != null) {
                triFrame.setSlicesFromFrame(xS, yS, slice);
            }
        }

        if (cursorMode == ViewJComponentBase.PAINT_CAN) {
            xPG = (short) xS;
            yPG = (short) yS;
            zPG = (short) slice;

            if (imageActive.isColorImage()) {
                final int index = 4 * (yS + imageActive.getExtents()[0] + xS);
                seedValR = imageBufferActive[index + 1];
                seedValG = imageBufferActive[index + 2];
                seedValB = imageBufferActive[index + 3];
                regionGrow((short) xS, (short) yS, (short) slice, seedValR, seedValG, seedValB, null, true);
            } else if(imageActive.isComplexImage()) { 
                int loc = ((yS * imageActive.getExtents()[0]) + xS)*2;
                seedVal = (float) Math.sqrt(imageBufferActive[loc]*imageBufferActive[loc] + imageBufferActive[loc+1]*imageBufferActive[loc+1]);
            } else {
                seedVal = imageBufferActive[ (yS * imageActive.getExtents()[0]) + xS];
                regionGrow((short) xS, (short) yS, (short) slice, seedVal, null, true);
            }

            imageActive.notifyImageDisplayListeners(null, true);

        } else if (cursorMode == ViewJComponentBase.PAINT_VASC) {
            final int index = xS + (yS * imageActive.getExtents()[0]);
            final int z = MipavMath.round( ((ViewJFramePaintVasculature) frame).getMIPZValue(index));
            final float value = ((ViewJFramePaintVasculature) frame).imageBuffer[index
                                                                                 + (z * imageActive.getSliceSize())];

            ((ViewJFrameImage) ((ViewJFramePaintVasculature) frame).parent).getComponentImage().regionGrow((short) xS,
                    (short) yS, (short) z, value, null, true);
            ((ViewJFrameImage) ((ViewJFramePaintVasculature) frame).parent).getComponentImage().setRegionGrowVars(
                    (short) xS, (short) yS, (short) z, value);
            imageActive.notifyImageDisplayListeners(null, true);
        }
        //else if (cursorMode == DEFAULT) {
            intensityLabel = false;
            paintComponent(getGraphics());
        //}

        // reset mousePressIsShiftDown for next mouse click
        mousePressIsShiftDown = false;

    } // end mouseReleased()

    /**
     * ************************************************************************ ************************** Mouse Wheel
     * Events *************************.************************************************************************
     * 
     * @param mouseWheelEvent DOCUMENT ME!
     */
    public void mouseWheelMoved(final MouseWheelEvent mouseWheelEvent) {
    	
    	
    	
    	
    	
        final int wheelRotation = mouseWheelEvent.getWheelRotation();

        if (frame instanceof ViewJFrameImage) {
        	
        	boolean isActiveFrame = frame.isActive();
        	
        	if(isActiveFrame) {
        		
        		
        		
        		
        		if (wheelRotation < 0) {

                    if (imageActive.getNDims() > 2) {

                        if (mouseWheelEvent.isShiftDown()) {

                            if ( ((ViewJFrameImage) frame).getImageA().getNDims() == 3) {
                                ((ViewJFrameImage) frame).setShiftDown(true);
                            }
                        }else {
                        	if ( ((ViewJFrameImage) frame).getImageA().getNDims() == 3) {
                                ((ViewJFrameImage) frame).setShiftDown(false);
                            }
                        }

                        ((ViewJFrameImage) frame).incSlice();
                    } else {
                    	int xCoord = mouseWheelEvent.getX();
                    	int yCoord = mouseWheelEvent.getY();
                    	
                        ((ViewJFrameImage) frame).updateFrame(getZoomX() * 2.0f, getZoomY() * 2.0f, xCoord, yCoord);
                    }
                } else {

                    if (imageActive.getNDims() > 2) {

                        if (mouseWheelEvent.isShiftDown()) {

                            if ( ((ViewJFrameImage) frame).getImageA().getNDims() == 3) {
                                ((ViewJFrameImage) frame).setShiftDown(true);
                            }
                        }else {
                        	if ( ((ViewJFrameImage) frame).getImageA().getNDims() == 3) {
                                ((ViewJFrameImage) frame).setShiftDown(false);
                            }
                        }

                        ((ViewJFrameImage) frame).decSlice();
                    } else {
                    	int xCoord = mouseWheelEvent.getX();
                    	int yCoord = mouseWheelEvent.getY();
                    	
                        ((ViewJFrameImage) frame).updateFrame(getZoomX() / 2.0f, getZoomY() / 2.0f, xCoord, yCoord);
                    }
                }
        		
        		
        		
        		restartCheckerboardAnimateThread();
        		
        		
        		
        	}

            
        }
    }
    
    
    
    public void restartCheckerboardAnimateThread() {

    	if(checkerDialog != null) {
			if(checkerDialog.isAnimating()) {
				
				//first stop currrent thread
				checkerDialog.setAnimating(false);
				setCheckerboardAnimate(false);

				while(!checkerDialog.isThreadStopped()) {
					//do nothing
				}
				
				setMakingCheckerboard(true);
				//now reset checkerboard
				//checkerDialog.setCc(0);
				
				checkerDialog.refreshCc();
				paintComponent(getGraphics());
				
				setMakingCheckerboard(false);
				
				//now restart thread
				checkerDialog.setAnimating(true);
				setCheckerboardAnimate(true);
				
				checkerDialog.animateThread = checkerDialog.new Animate();
    	    	try {
    	    		checkerDialog.animateThread.start();
    	    	}catch (Exception e) {
    				e.printStackTrace();
    				return;
    			}
			}else {
				//just reset checkerboard
				if(checkerDialog.isCheckerboardApplied()) {
					setMakingCheckerboard(true);
					//now reset checkerboard
					//checkerDialog.setCc(0);
					checkerDialog.refreshCc();
					checkerDialog.setPressedStart(0);
					paintComponent(getGraphics());
					
					setMakingCheckerboard(false);
				}
				
				
			}
			
		}
    }
    
    

    /**
     * Open the user defined LUT table and transfer function.
     */
    public void openUDLUTandTransferFunct() {
        final String fName = "userdefine.lut";
        final String dName = Preferences.getPreferencesDir();

        readLUTandTFunctFrom(fName, dName);

        try {

            if (imageA.isColorImage() == false) {

                imageA.notifyImageDisplayListeners(null, true);

                if (imageB != null) {
                    imageB.notifyImageDisplayListeners(null, true);
                }

            } else { // RGB image

                imageA.notifyImageDisplayListeners(true, 1, RGBTA);
                if (imageB != null) {
                    imageB.notifyImageDisplayListeners(true, 1, RGBTB);
                }
            }

        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.openUDLUT");
        }

    }

    public String getRGBPaintComponents() {
        return this.rgbString;
    }

    public void setRGBPaintComponents(final String rgb) {
        this.rgbString = rgb;
    }

 
    

    
    /**
     * Paints the image and calls drawSelf for all VOIs.
     * 
     * @param graphics graphics
     */
    public void paintComponent(final Graphics graphics) {

        try {
            if (modifyFlag == false) {
                return;
            }

            if (graphics == null) {
                Preferences.debug("ComponentEditImage.paintComponent: graphics = null");

                return;
            }
            Graphics2D offscreenGraphics2d = null;

            if (isDisplayable()) { // a component is displayable when it is connected to a screen resource
                final int width = Math.round(zoomX * imageDim.width * resolutionX);
                final int height = Math.round(zoomY * imageDim.height * resolutionY);

                if ( (offscreenImage == null) || (offscreenImage.getWidth(null) != width)
                        || (offscreenImage.getHeight(null) != height)) {
                    try {
                        offscreenImage = createImage(width, height);
                    } catch (final OutOfMemoryError error) {
                        offscreenImage = null;
                        System.gc();

                        if (Preferences.is(Preferences.PREF_ZOOM_LINEAR)) {
                            if (zoomX <= 1.0f) {
                                zoomX = 0.5f * zoomX;
                            } else {
                                zoomX = zoomX - 1.0f;
                            }

                            if (zoomY <= 1.0f) {
                                zoomY = 0.5f * zoomY;
                            } else {
                                zoomY = zoomY - 1.0f;
                            }

                        } else { // zoomMode == EXPONENTIAL

                            zoomX = 0.5f * zoomX;
                            zoomY = 0.5f * zoomY;
                        }

                        final int xS = getScaledX(imageDim.width >> 2);
                        final int yS = getScaledY(imageDim.height >> 2);
                        cursorMode = ViewJComponentBase.DEFAULT;
                        setCursor(MipavUtil.defaultCursor);
                        ((ViewJFrameImage) frame).updateFrame(zoomX, zoomY, xS, yS);
                        System.err.println("Reduced zoom because createImage in "
                                + "ComponentEditImage.paintComponent yielded OutOfMemoryError");

                        return;
                    } // catch (OutOfMemoryError error)
                }

                offscreenGraphics2d = (Graphics2D) offscreenImage.getGraphics();

                if (offscreenGraphics2d == null) {
                    Preferences.debug("Can't get memory image context in ViewJComponentEditImage:paintComponent()");

                    return;
                }
            } else {
                return;
            }

            offscreenGraphics2d.setColor(Color.black);
            offscreenGraphics2d.fillRect(0, 0, getSize().width, getSize().height);

            if ( (paintImageBuffer == null) || (paintImageBuffer.length != (imageDim.width * imageDim.height))) {
                paintImageBuffer = new int[imageDim.width * imageDim.height]; // make the buffer that will hold the
                // paint
            } else {
                Arrays.fill(paintImageBuffer, 0); // ensure erasure of old image, otherwise ghosting occurs
            }

            // clip graphics to visible area on screen - this saves rendering time
            final Rectangle visibleRect = getVisibleRect();
            offscreenGraphics2d.setClip(visibleRect);

            // build the paint image that will be blended on-screen
            makePaintImage(paintImageBuffer, paintBitmap, slice, frame, (imageExtents.length < 3));

            if (Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)) {
                makePaintBitmapBorder(paintImageBuffer, paintBitmap, slice, frame);
            }

            if ( ! (this instanceof ViewJComponentRegistration)) {
                //draw3DVOIs(offscreenGraphics2d, true);
            }

            if (memImageA == null) { // create imageA if it hasn't already been created
                memImageA = new MemoryImageSource(imageDim.width, imageDim.height, pixBuffer, 0, imageDim.width);
                img = createImage(memImageA);
            } else {
                memImageA.newPixels(pixBuffer, ColorModel.getRGBdefault(), 0, imageDim.width);
                img.flush();
            }

            final int zoomedWidth = Math.round(zoomX * img.getWidth(this) * resolutionX);
            final int zoomedHeight = Math.round(zoomY * img.getHeight(this) * resolutionY);

            if ( (interpMode == ViewJComponentBase.INTERPOLATE_A)
                    || (interpMode == ViewJComponentBase.INTERPOLATE_BOTH)) {
                offscreenGraphics2d.setRenderingHints(Preferences.getInterpolateDisplay().getRenderingHint());
            }

            // draw image A
            offscreenGraphics2d.drawImage(img, 0, 0, zoomedWidth, zoomedHeight, 0, 0, img.getWidth(this), img
                    .getHeight(this), null);

            if (imageB != null) {

                if ( (interpMode == ViewJComponentBase.INTERPOLATE_B)
                        || (interpMode == ViewJComponentBase.INTERPOLATE_BOTH)) {
                    offscreenGraphics2d.setRenderingHints(Preferences.getInterpolateDisplay().getRenderingHint());
                } else {
                    offscreenGraphics2d.setRenderingHints(InterpolateDisplay.NEAREST.getRenderingHint());
                }

                // if checkerboarding is OFF, this means blending should be enabled
                if ( ! (frame instanceof ViewJFrameLightBox)) {
                	if(frame instanceof ViewJFrameTriImage) {
                		cleanBuffer(ViewJComponentBase.BOTH);
                	}else {
                		cleanBuffer(ViewJComponentBase.IMAGE_A);
                	}
                }

                if ( !isCheckerboarded()) {
                    adjustOpacityFor000Color();
                    offscreenGraphics2d.setComposite(AlphaComposite
                            .getInstance(AlphaComposite.SRC_OVER, 1 - alphaBlend));
                    
                } else {

                	if(!isCheckerboardAnimate() && isMakingCheckerboard()) {
                		makeCheckerboard();
                	}
                	
                	
                }

                if (memImageB == null) {
                    memImageB = new MemoryImageSource(imageDim.width, imageDim.height, pixBufferB, 0, imageDim.width);
                    imgB = createImage(memImageB);
                } else {
                    memImageB.newPixels(pixBufferB, ColorModel.getRGBdefault(), 0, imageDim.width);
                    imgB.flush();
                }

                // draw image B
                offscreenGraphics2d.drawImage(imgB, 0, 0, zoomedWidth, zoomedHeight, 0, 0, imgB.getWidth(this), imgB
                        .getHeight(this), null);

            }

            memImageA.newPixels(paintImageBuffer, ColorModel.getRGBdefault(), 0, imageDim.width);

            Image paintImage = createImage(memImageA); // the image representing the paint mask

            // change rendering hint back from preference set interpolation mode to nearest neighbor so that
            // all other painting will not be in interpolated mode
            offscreenGraphics2d.setRenderingHints(InterpolateDisplay.NEAREST.getRenderingHint());

            offscreenGraphics2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f));

            offscreenGraphics2d.drawImage(paintImage, 0, 0, zoomedWidth, zoomedHeight, 0, 0, img.getWidth(this), img
                    .getHeight(this), null);

            // remove the local buffer memory allocation
            paintImage.flush();
            paintImage = null;

            if ( (cursorMode == ViewJComponentBase.PAINT_VOI)
                    || ( (cursorMode == ViewJComponentBase.ERASER_PAINT) && ( (lastMouseX != OUT_OF_BOUNDS) || (lastMouseY != OUT_OF_BOUNDS)))) {

                // this method repaints the paint brush cursor without repainting the entire image
                repaintPaintBrushCursorFast(offscreenGraphics2d);
            }
            if(drawVOIs) {
            	draw3DVOIs(offscreenGraphics2d, false);
            }
            if ( ! (this instanceof ViewJComponentRegistration)) {
                //voiHandler.drawVOIs(offscreenGraphics2d); // draw all VOI regions


                if (overlayOn) {
                    showOverlay(offscreenGraphics2d);
                }
                if (gridOverlayOn) {
                    showGridOverlay(offscreenGraphics2d);
                }
            }

            drawImageText(offscreenGraphics2d); // draw image text, i.e. slice number

            if ( (cursorMode == ViewJComponentBase.WIN_REGION)
                    && ( (lastMouseX != OUT_OF_BOUNDS) || (lastMouseY != OUT_OF_BOUNDS)) && (shiftDown == false)) {

                if ( (interpMode == ViewJComponentBase.INTERPOLATE_B)
                        || (interpMode == ViewJComponentBase.INTERPOLATE_BOTH)) {
                    offscreenGraphics2d.setRenderingHints(Preferences.getInterpolateDisplay().getRenderingHint());
                }

                if ( (lastWinRegionSlice != slice) || (cleanImageB == null)) {
                    cleanBuffer(ViewJComponentBase.IMAGE_B);

                    final MemoryImageSource memImageSource = new MemoryImageSource(imageDim.width, imageDim.height,
                            cleanImageBufferB, 0, imageDim.width);

                    cleanImageB = createImage(memImageSource);
                }

                super.paintWindowComponent(offscreenGraphics2d, lastMouseX, lastMouseY, windowedRegionSize,
                        windowedRegionSize, getZoomX(), cleanImageB);

                lastWinRegionSlice = slice;
            } else if ( (cursorMode == ViewJComponentBase.MAG_REGION)
                    && ( (lastMouseX != OUT_OF_BOUNDS) || (lastMouseY != OUT_OF_BOUNDS))) {
                paintMagComponent(offscreenGraphics2d);
            } else if (cursorMode == ViewJComponentBase.DEFAULT || cursorMode == ViewJComponentBase.VOI_3D) {

                if (intensityLabel) {
                    if ( ! (this instanceof ViewJComponentSingleRegistration) && ! (frame instanceof ViewJFrameLightBox)) {
                        if (frame instanceof ViewJFrameImage) {
                            // display intensity values on screen
                            repaintImageIntensityLabelFast(offscreenGraphics2d);
                        }
                    }
                }
            }

            if (onTop) {

                // paint the on-top notifier for the user when this component is on the top of the user-interface
                offscreenGraphics2d.setColor(toppedColor);
                String preferredSize = Preferences.getProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR_BORDERSIZE);
                int prefSize = Integer.valueOf(preferredSize);
                if(prefSize == 5) {
                	offscreenGraphics2d.drawRect(visibleRect.x, visibleRect.y, visibleRect.width - 1, visibleRect.height - 1);
                    offscreenGraphics2d.drawRect(visibleRect.x + 1, visibleRect.y + 1, visibleRect.width - 3, visibleRect.height - 3);
                    offscreenGraphics2d.drawRect(visibleRect.x + 2, visibleRect.y + 2, visibleRect.width - 5, visibleRect.height - 5);
                    offscreenGraphics2d.drawRect(visibleRect.x + 3, visibleRect.y + 3, visibleRect.width - 7, visibleRect.height - 7);
                    offscreenGraphics2d.drawRect(visibleRect.x + 4, visibleRect.y + 4, visibleRect.width - 9, visibleRect.height - 9);
                }else if(prefSize == 4) {
                	offscreenGraphics2d.drawRect(visibleRect.x, visibleRect.y, visibleRect.width - 1, visibleRect.height - 1);
                    offscreenGraphics2d.drawRect(visibleRect.x + 1, visibleRect.y + 1, visibleRect.width - 3, visibleRect.height - 3);
                    offscreenGraphics2d.drawRect(visibleRect.x + 2, visibleRect.y + 2, visibleRect.width - 5, visibleRect.height - 5);
                    offscreenGraphics2d.drawRect(visibleRect.x + 3, visibleRect.y + 3, visibleRect.width - 7, visibleRect.height - 7);
                }else if(prefSize == 3) {
                	offscreenGraphics2d.drawRect(visibleRect.x, visibleRect.y, visibleRect.width - 1, visibleRect.height - 1);
                    offscreenGraphics2d.drawRect(visibleRect.x + 1, visibleRect.y + 1, visibleRect.width - 3, visibleRect.height - 3);
                    offscreenGraphics2d.drawRect(visibleRect.x + 2, visibleRect.y + 2, visibleRect.width - 5, visibleRect.height - 5);
                }else if(prefSize == 2) {
                	offscreenGraphics2d.drawRect(visibleRect.x, visibleRect.y, visibleRect.width - 1, visibleRect.height - 1);
                    offscreenGraphics2d.drawRect(visibleRect.x + 1, visibleRect.y + 1, visibleRect.width - 3, visibleRect.height - 3);
                }else {
                	offscreenGraphics2d.drawRect(visibleRect.x, visibleRect.y, visibleRect.width - 1, visibleRect.height - 1);
                }
                

            }

            graphics.drawImage(offscreenImage, 0, 0, null);

            if (offscreenGraphics2d != null) {
                offscreenGraphics2d.dispose();
                offscreenGraphics2d = null;
            }

            // TODO: hack to avoid running out of memory on some systems. this slows down the painting a lot
            // try to reclaim memory if getting close to the limit
            /*
             * double memoryInUse = ( (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) /
             * 1048576); double totalMemory = (Runtime.getRuntime().totalMemory() / 1048576); if ( (memoryInUse /
             * totalMemory) > 0.8) { System.err.println("repaint gc() called:\t" + memoryInUse + "\t" + totalMemory);
             * System.gc(); }
             */
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImge.paintComponent.");
        } catch (final Throwable t) {
            t.printStackTrace();
        }
    }

    public void setDrawVOIs(boolean drawVOIs) {
		this.drawVOIs = drawVOIs;
	}

	/**
     * Paints the image and calls drawSelf for all VOIs and also resizes the image if it is too big for printer.
     * 
     * @param tx x translation
     * @param ty y translation
     * @param g graphics
     */
    public void paintComponentForPrinter(final int tx, final int ty, final Graphics g) {

        if (g == null) {
            MipavUtil.displayError("ComponentEditImage.paintComponent: graphics = null");

            return;
        }

        g.translate(tx, ty);

        super.paintComponent(g);

        draw3DVOIs(g, false);

        if (getOverlayOn()) {
            showOverlay(g);
        }
    }
    /*
     * 
    public void paintComponentForPrinter(int tx, int ty, Graphics g) {
        ViewVOIVector VOIs = imageActive.getVOIs();

        if (g == null) {
            MipavUtil.displayError("ComponentEditImage.paintComponent: graphics = null");

            return;
        }

        g.translate(tx, ty);

        super.paintComponent(g);

        int nVOI = VOIs.size();

        if (slice != -99) {

            for (int i = nVOI - 1; i >= 0; i--) {
                VOIs.VOIAt(i).drawSelf(getZoomX(), getZoomY(), resolutionX, resolutionY, 0, 0,
                        imageActive.getFileInfo(0).getResolutions(), imageActive.getFileInfo(0).getUnitsOfMeasure(),
                        slice, orientation, g);
            }
        }

        if (cursorMode == ViewJComponentBase.LEVELSET) {
            g.setColor(Color.yellow);
            System.err.println( "ViewJComponentEditImage.paintComponentForPrinter voiHandler" );
            g.drawPolygon(voiHandler.getZoomedLevelSetPolygon());
            // g.drawPolygon(zoomPolygon(rbLevelSet.getLevelSetPolygon(), getZoomX(), getZoomY()));
        }

        if (getOverlayOn()) {
            showOverlay(g);
        }
    }
     */
    /**
     * Paints a magnified window over the image centered about the cursor.
     * 
     * @param graphics2d graphics component
     */
    public void paintMagComponent(final Graphics2D graphics2d) {
        int xNew = lastMouseX;
        int yNew = lastMouseY;
        int width = MAGR_WIDTH;
        int height = MAGR_HEIGHT;
        final float mag = ( (shiftDown == false) ? MAGR_MAG : getZoomX());
        final int imageType = imageActive.getType();
        final int imageXDim = imageActive.getExtents()[0];
        final double minIntensity = getActiveImage().getMin();
        final double maxIntensity = getActiveImage().getMax();

        int xNewO, yNewO;
        int x1, y1, xw1, yh1;
        int x2, y2;

        RenderingHints hintImageA = InterpolateDisplay.NEAREST.getRenderingHint();
        RenderingHints hintImageB = InterpolateDisplay.NEAREST.getRenderingHint();
        if(magSettings != null) {
            hintImageA = magSettings.getInterpType().getRenderingHint();
            hintImageB = magSettings.getInterpType().getRenderingHint();
        } else if ( (interpMode == ViewJComponentBase.INTERPOLATE_A)) {
            hintImageA = Preferences.getInterpolateDisplay().getRenderingHint();
        } else if ( (interpMode == ViewJComponentBase.INTERPOLATE_B)) {
            hintImageB = Preferences.getInterpolateDisplay().getRenderingHint();
        } else if ( (interpMode == ViewJComponentBase.INTERPOLATE_BOTH)) {
            hintImageA = Preferences.getInterpolateDisplay().getRenderingHint();
            hintImageB = Preferences.getInterpolateDisplay().getRenderingHint();
        }
        
        if (zoomX >= 2) {

            while ( ( (Math.round(width / zoomX) - (width / zoomX)) != 0)
                    || ( (Math.round(width / zoomX / 2.0f) - (width / zoomX / 2.0f)) != 0)) {
                width++;
            }
        }

        xNew = (int) ( ((int) (xNew / zoomX) * zoomX) + 0.5);
        yNew = (int) ( ((int) (yNew / zoomY) * zoomY) + 0.5);

        final int sIWidth = (int) (width / mag);
        final int sIHeight = (int) (height / mag);

        if (img != null) {
            xNewO = xNew - (int) (0.5f * width);
            yNewO = yNew - (int) (0.5f * height);

            final int sX = (int) (xNew / zoomX);
            final int sY = (int) (yNew / zoomY);

            if ( (sX - (sIWidth / 2)) < 0) {
                return;
            }

            if ( (sY - (sIHeight / 2)) < 0) {
                return;
            }

            // Draw zoomed portion of window
            x2 = sX - (sIWidth / 2);
            x1 = xNewO;
            xw1 = width + xNewO;
            y2 = sY - (sIHeight / 2);
            y1 = yNewO;
            yh1 = height + yNewO;

            graphics2d.setRenderingHints(hintImageA);

            graphics2d.drawImage(img, x1, y1, xw1, yh1, x2, y2, sX + (sIWidth / 2), sY + (sIHeight / 2), this);

            if ( (imageB != null) && (imgB != null)) {

                graphics2d.setRenderingHints(hintImageB);

                if ( !isCheckerboarded()) {
                    graphics2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1 - alphaBlend));
                }

                graphics2d.drawImage(imgB, x1, y1, xw1, yh1, x2, y2, sX + (sIWidth / 2), sY + (sIHeight / 2), this);

                graphics2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f));
            }

            graphics2d.setRenderingHints(InterpolateDisplay.NEAREST.getRenderingHint());

            graphics2d.setColor(Color.red.darker());
            graphics2d.drawRect(xNewO, yNewO, width - 1, height - 1);

            graphics2d.setColor(textColor);
            graphics2d.setFont(MipavUtil.font10);

            if (zoomX >= 1.0) {

                if ( ( (imageDim.height - 10) > 0) && (sliceString != null)) {
                    graphics2d.drawString(Float.toString(mag) + "x", xNewO + 5, yNewO + height - 5);
                }
            }

            float offsetY = 0;
            final int startX = sX - (sIWidth / 2);
            final int startY = sY - (sIHeight / 2);
            final int endX = sX + (sIWidth / 2);
            final int endY = sY + (sIHeight / 2);
            int pix;
            int osX, osY;

            final float xwidth = (float) width / (endX - startX);
            final float yheight = (float) height / (endX - startX);

            final int fontHeight = graphics2d.getFontMetrics(graphics2d.getFont()).getHeight();
            final int minStrWidth = graphics2d.getFontMetrics(graphics2d.getFont()).stringWidth(
                    Integer.toString((int) minIntensity));
            int maxStrWidth = graphics2d.getFontMetrics(graphics2d.getFont()).stringWidth(
                    Integer.toString((int) maxIntensity));

            if (minStrWidth > maxStrWidth) {
                maxStrWidth = minStrWidth;
            }

            final int maxCharWidth = graphics2d.getFontMetrics(graphics2d.getFont()).charWidth('8');
            final int maxFracDigs = ( ((int) (xwidth) - maxStrWidth) / maxCharWidth) - 2;
            final NumberFormat nf = NumberFormat.getNumberInstance();

            if (maxFracDigs > 1) {
                nf.setMaximumFractionDigits(maxFracDigs);
            } else {
                nf.setMaximumFractionDigits(1);
            }

            if (showMagIntensity) {
            
                if ( ( ( (imageType == ModelStorageBase.FLOAT) || (imageType == ModelStorageBase.DOUBLE) || (imageType == ModelStorageBase.DCOMPLEX) 
                        || (imageType == ModelStorageBase.COMPLEX) || (imageType == ModelStorageBase.ARGB) 
                        || (imageType == ModelStorageBase.ARGB_FLOAT) || (imageType == ModelStorageBase.ARGB_USHORT)) && ( (maxStrWidth < (xwidth - 1 - (2 * maxCharWidth))) && (fontHeight < (yheight - 1))))
                            || ( ( (imageType != ModelStorageBase.FLOAT) && (imageType != ModelStorageBase.DOUBLE) && (imageType != ModelStorageBase.DCOMPLEX)
                                && (imageType != ModelStorageBase.COMPLEX) && (imageType != ModelStorageBase.ARGB) && (imageType != ModelStorageBase.ARGB_FLOAT) 
                                && (imageType != ModelStorageBase.ARGB_USHORT)) && ( (maxStrWidth < (xwidth - 1)) && (fontHeight < (yheight - 1))))) {
                    
                    for (int y = startY; y < endY; y++) {
                        float offsetX = 0;

                        for (int x = startX; x < endX; x++) {

                            pix = (y * imageXDim) + x;

                            if ( (pix >= 0) && (pix < imageBufferActive.length)) {

                                if ( (imageType == ModelStorageBase.FLOAT) || (imageType == ModelStorageBase.DOUBLE)
                                        || (imageType == ModelStorageBase.ARGB)
                                        || (imageType == ModelStorageBase.ARGB_FLOAT)
                                        || (imageType == ModelStorageBase.ARGB_USHORT)) {
                                    sliceString = nf.format(imageBufferActive[pix]);
                                } else if((imageType == ModelStorageBase.COMPLEX)
                                        || (imageType == ModelStorageBase.DCOMPLEX)) {
                                    sliceString = nf.format(Math.sqrt(imageBufferActive[pix*2]*imageBufferActive[pix*2] + imageBufferActive[pix*2+1]*imageBufferActive[pix*2+1]));
                                } else {
                                    sliceString = Integer.toString((int) imageBufferActive[pix]);
                                }

                                graphics2d.setColor(Color.black);
                                osX = MipavMath.round(offsetX);
                                osY = MipavMath.round(offsetY);
                                graphics2d.drawString(sliceString, xNewO + osX + 5, yNewO + osY + 14);
                                graphics2d.drawString(sliceString, xNewO + osX + 5, yNewO + osY + 16);
                                graphics2d.drawString(sliceString, xNewO + osX + 6, yNewO + osY + 15);
                                graphics2d.drawString(sliceString, xNewO + osX + 4, yNewO + osY + 15);
                                graphics2d.setColor(Color.white);
                                graphics2d.drawString(sliceString, xNewO + osX + 5, yNewO + osY + 15);

                            }

                            offsetX += (float) width / (endX - startX);
                        }

                        offsetY += (float) height / (endX - startX);
                    }
                }
            }
        }
    }
    

    /**
     * Backups up ( and swaps if not null) the current and previously used paintBrush.
     */
    public void quickSwitchBrush() {

        // swap previous with current

        if (paintBrushPrevious != null) {
            final BitSet tempSet = (BitSet) paintBrushPrevious.clone();
            final Dimension tempDim = (Dimension) paintBrushDimPrevious.clone();

            final int prevWidth = paintImagePrevious.getWidth();
            final int prevHeight = paintImagePrevious.getHeight();

            final BufferedImage tempBImage = new BufferedImage(prevWidth, prevHeight, paintImagePrevious.getType());

            for (int i = 0; i < prevWidth; i++) {

                for (int j = 0; j < prevHeight; j++) {
                    tempBImage.setRGB(i, j, paintImagePrevious.getRGB(i, j));
                }
            }

            final int curWidth = paintImage.getWidth();
            final int curHeight = paintImage.getHeight();
            final BufferedImage tempBImageCurrent = new BufferedImage(curWidth, curHeight, paintImage.getType());

            for (int i = 0; i < curWidth; i++) {

                for (int j = 0; j < curHeight; j++) {
                    tempBImageCurrent.setRGB(i, j, paintImage.getRGB(i, j));
                }
            }

            paintBrushPrevious = (BitSet) paintBrush.clone();
            paintBrush = tempSet;

            paintBrushDimPrevious = (Dimension) paintBrushDim.clone();
            paintBrushDim = tempDim;

            paintImagePrevious.flush();
            paintImagePrevious = null;

            paintImage.flush();
            paintImage = null;

            // switch them
            paintImage = tempBImage;
            paintImagePrevious = tempBImageCurrent;
        } else {
            paintBrushPrevious = (BitSet) paintBrush.clone();
            paintBrushDimPrevious = (Dimension) paintBrushDim.clone();

            final int width = paintImage.getWidth();
            final int height = paintImage.getHeight();

            paintImagePrevious = new BufferedImage(width, height, paintImage.getType());

            for (int i = 0; i < width; i++) {

                for (int j = 0; j < height; j++) {
                    paintImagePrevious.setRGB(i, j, paintImage.getRGB(i, j));
                }
            }
        }

    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between previously supplied bounds.
     * 
     * @param str the string to prepend to message containing region growth statistics
     */
    public void regionGrow(final String str) {

        if (seedPaintBitmap != null) {
            regionGrow(xPG, yPG, zPG, seedVal, str, false);
        }
    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between the the bounds which are also supplied. Used on black and white images.
     * 
     * <p>
     * when click <code>false</code>, adds points in the newly grown region which weren't in the old one remove
     * points which were in the old region but aren't in the new one (and which weren't in the region painted before the
     * last click), otherwise, the regions are simply added into the new set.
     * </p>
     * 
     * @param x x coordinate of the seed point
     * @param y y coordinate of the seed point
     * @param z z coordinate of the seed point
     * @param value Intensity value at the seed point
     * @param str String to start line with
     * @param click whether this region grow was initiated by a click on the image
     */
    public void regionGrow(final short x, final short y, final short z, final float value, final String str,
            final boolean click) {
        this.regionGrow(x, y, z, value, imageActive, str, click);
    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between the the bounds which are also supplied. Used for black and white images.
     * 
     * <p>
     * When click is <code>false</code>, adds points in the newly grown region which weren't in the old one remove
     * points which were in the old region but aren't in the new one (and which weren't in the region painted before the
     * last click), otherwise, the regions are simply added into the new set.
     * </p>
     * 
     * @param x x coordinate of the seed point
     * @param y y coordinate of the seed point
     * @param z z coordinate of the seed point
     * @param value Intensity value at the seed point
     * @param image the image to perform the region grow in
     * @param leadString the string to append to the region grow output
     * @param click whether this region grow was initiated by a click on the image
     */
    public void regionGrow(final short x, final short y, final short z, final float value, final ModelImage image,
            final String leadString, boolean click) {
        final Cursor cursor = getCursor();

        setCursor(MipavUtil.waitCursor);

        int count;

        BitSet tempBitmap = null;

        if (click) {

            // backup the current paint mask
            backupPaintBitmap();

            if (seedPaintBitmap == null) {
                seedPaintBitmap = new BitSet();
            } else {
                seedPaintBitmap.clear();
            }
        } else {
            tempBitmap = (BitSet) seedPaintBitmap.clone();
            seedPaintBitmap.clear();
        }

        if (x != -1) {
            saveX = x;
            saveY = y;
            saveZ = z;
            saveValue = value;
        } else {
            return;
            // eraseAllPaint();
        }

        if (growDialog != null) {
            fuzzyThreshold = growDialog.getFuzzyThreshold();
            useVOI = growDialog.getUseVOI();
            displayFuzzy = growDialog.getDisplayFuzzy();
            sizeLimit = growDialog.getMaxSize();
            maxDistance = growDialog.getMaxDistance();
            less = growDialog.getLowerBound();
            more = growDialog.getUpperBound();
            variableThresholds = growDialog.getVariableThresholds();
        }

        if ( (fuzzyThreshold == -2.0f) || (sizeLimit == -2) || (maxDistance == -2)) {
            return;
        }

        try {
            final AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(image, 1.0f, 1.0f);

            regionGrowAlgo.setRunningInSeparateThread(false);

            if (image.getType() == ModelStorageBase.BOOLEAN) {
                less = 0;
                more = 0;
            }

            if (image.getNDims() == 2) {
                count = regionGrowAlgo.regionGrow2D(seedPaintBitmap, new Point(saveX, saveY), fuzzyThreshold, useVOI,
                        displayFuzzy, growDialog, saveValue - less, saveValue + more, sizeLimit, maxDistance,
                        variableThresholds);
                showRegionInfo(count, leadString);
            } else if ( (image.getNDims() == 3) || (image.getNDims() == 4)) {
                CubeBounds regionGrowBounds;

                if ( ((JDialogPaintGrow) growDialog).boundsConstrained()) {

                    // constrain bounds to cropping volume
                    regionGrowBounds = ((ViewJFrameTriImage) frame).getBoundedVolume();
                } else {

                    // bounds are not constrained by cropping volume, use image extents as bounds
                    regionGrowBounds = new CubeBounds(imageExtents[0], 0, imageExtents[1], 0, imageExtents[2], 0);
                }

                count = regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3D(saveX, saveY, saveZ), fuzzyThreshold,
                        useVOI, displayFuzzy, growDialog, saveValue - less, saveValue + more, sizeLimit, maxDistance,
                        variableThresholds, timeSlice, regionGrowBounds);
                showRegionInfo(count, leadString);
            }
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.regionGrow");
        }

        if ( !click) {

            // add points in the newly grown region which weren't in the old one
            BitSet diff = (BitSet) seedPaintBitmap.clone();

            diff.andNot(tempBitmap);
            paintBitmap.or(diff);

            // remove points which were in the old region but aren't in the new one
            // (and which weren't in the region painted before the last click)
            diff = (BitSet) seedPaintBitmap.clone();
            tempBitmap.andNot(diff);
            tempBitmap.andNot(paintBitmapBU);
            paintBitmap.xor(tempBitmap);
        } else {
            paintBitmap.or(seedPaintBitmap);
        }
        if ( imageActive != null )
        {
            imageActive.setMask(paintBitmap);
        }

        if (growDialog != null) {
            growDialog.notifyPaintListeners(true, false, paintBitmap);
        }

        image.notifyImageDisplayListeners(null, true);
        setCursor(cursor);
    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between the the bounds which are also supplied. Used on color images.
     * 
     * <p>
     * when click <code>false</code>, adds points in the newly grown region which weren't in the old one remove
     * points which were in the old region but aren't in the new one (and which weren't in the region painted before the
     * last click), otherwise, the regions are simply added into the new set.
     * </p>
     * 
     * @param x x coordinate of the seed point
     * @param y y coordinate of the seed point
     * @param z z coordinate of the seed point
     * @param valueR Red intensity value at the seed point
     * @param valueG Green intensity value at the seed point
     * @param valueB Blue intensity value at the seed point
     * @param str String to start line with
     * @param click whether this region grow was initiated by a click on the image
     */
    public void regionGrow(final short x, final short y, final short z, final float valueR, final float valueG,
            final float valueB, final String str, final boolean click) {
        this.regionGrow(x, y, z, valueR, valueG, valueB, imageActive, str, click);
    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between the the bounds which are also supplied. Used for color images.
     * 
     * <p>
     * When click is <code>false</code>, adds points in the newly grown region which weren't in the old one remove
     * points which were in the old region but aren't in the new one (and which weren't in the region painted before the
     * last click), otherwise, the regions are simply added into the new set.
     * </p>
     * 
     * @param x x coordinate of the seed point
     * @param y y coordinate of the seed point
     * @param z z coordinate of the seed point
     * @param valueR Red value at the seed point
     * @param valueG Green value at the seed point
     * @param valueB Blue value at the seed point
     * @param image the image to perform the region grow in
     * @param leadString the string to append to the region grow output
     * @param click whether this region grow was initiated by a click on the image
     */
    public void regionGrow(final short x, final short y, final short z, final float valueR, final float valueG,
            final float valueB, final ModelImage image, final String leadString, boolean click) {
    	
        final Cursor cursor = getCursor();

        setCursor(MipavUtil.waitCursor);

        int count;

        BitSet tempBitmap = null;

        if (click) {

            // backup the current paint mask
            backupPaintBitmap();

            if (seedPaintBitmap == null) {
                seedPaintBitmap = new BitSet();
            } else {
                seedPaintBitmap.clear();
            }
        } else {
            tempBitmap = (BitSet) seedPaintBitmap.clone();
            seedPaintBitmap.clear();
        }

        if (x != -1) {
            saveX = x;
            saveY = y;
            saveZ = z;
            saveValueR = valueR;
            saveValueG = valueG;
            saveValueB = valueB;
        } else {
            return;
        }

        if (growDialog != null) {
            fuzzyThreshold = growDialog.getFuzzyThreshold();
            useVOI = growDialog.getUseVOI();
            displayFuzzy = growDialog.getDisplayFuzzy();
            sizeLimit = growDialog.getMaxSize();
            maxDistance = growDialog.getMaxDistance();
            lessR = growDialog.getLowerBoundR();
            moreR = growDialog.getUpperBoundR();
            lessG = growDialog.getLowerBoundG();
            moreG = growDialog.getUpperBoundG();
            lessB = growDialog.getLowerBoundB();
            moreB = growDialog.getUpperBoundB();
        }

        if ( (fuzzyThreshold == -2.0f) || (sizeLimit == -2) || (maxDistance == -2)) {
            return;
        }

        try {
            final AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(image, 1.0f, 1.0f);

            regionGrowAlgo.setRunningInSeparateThread(false);

            if (image.getNDims() == 2) {
                count = regionGrowAlgo.regionGrow2D(seedPaintBitmap, new Point(saveX, saveY), fuzzyThreshold, useVOI,
                        displayFuzzy, growDialog, saveValueR - lessR, saveValueR + moreR, saveValueG - lessG,
                        saveValueG + moreG, saveValueB - lessB, saveValueB + moreB, sizeLimit, maxDistance);
                showRegionInfo(count, leadString);
            } else if ( (image.getNDims() == 3) || (image.getNDims() == 4)) {
                CubeBounds regionGrowBounds;

                if ( ((JDialogPaintGrow) growDialog).boundsConstrained()) {

                    // constrain bounds to cropping volume
                    regionGrowBounds = ((ViewJFrameTriImage) frame).getBoundedVolume();
                } else {

                    // bounds are not constrained by cropping volume, use image extents as bounds
                    regionGrowBounds = new CubeBounds(imageExtents[0], 0, imageExtents[1], 0, imageExtents[2], 0);
                }

                count = regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3D(saveX, saveY, saveZ), fuzzyThreshold,
                        useVOI, displayFuzzy, growDialog, saveValueR - lessR, saveValueR + moreR, saveValueG - lessG,
                        saveValueG + moreG, saveValueB - lessB, saveValueB + moreB, sizeLimit, maxDistance, timeSlice,
                        regionGrowBounds);
                showRegionInfo(count, leadString);
            }
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.regionGrow");
        }

        if ( !click) {

            // add points in the newly grown region which weren't in the old one
            BitSet diff = (BitSet) seedPaintBitmap.clone();

            diff.andNot(tempBitmap);
            paintBitmap.or(diff);

            // remove points which were in the old region but aren't in the new one
            // (and which weren't in the region painted before the last click)
            diff = (BitSet) seedPaintBitmap.clone();
            tempBitmap.andNot(diff);
            tempBitmap.andNot(paintBitmapBU);
            paintBitmap.xor(tempBitmap);
        } else {
            paintBitmap.or(seedPaintBitmap);
        }

        if (growDialog != null) {
            growDialog.notifyPaintListeners(true, false, paintBitmap);
        }

        image.notifyImageDisplayListeners(null, true);
        setCursor(cursor);
    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between previously supplied bounds.
     * 
     * @param str the string to prepend to message containing region growth statistics
     */
    public void regionGrowColor(final String str) {

        if (seedPaintBitmap != null) {
            regionGrow(xPG, yPG, zPG, seedValR, seedValG, seedValB, str, false);
        }
    }

    /**
     * Remembers the current paint brush size so that it can be reset later.
     */
    public void rememberPaintBrushSize() {

        // only remember if we're not remembering another brush size
        if (previousPaintBrush == -1) {
            previousPaintBrush = paintBrushSize;
        }
    }

    /**
     * Resets the LUTs.
     */
    public void resetLUTs() {

        try {

            if (imageA.isColorImage() == false) {

                if (imageA == imageActive) {
                    this.resetLUT(LUTa, imageA);
                } else if ( (imageB != null) && (imageB == imageActive)) {
                    this.resetLUT(LUTb, imageB);
                }
                imageA.notifyImageDisplayListeners(null, false);

                if (imageB != null) {
                    imageB.notifyImageDisplayListeners(null, false);
                }

            } else { // RGB image

                if (imageA == imageActive) {
                    this.resetRGB(RGBTA);
                } else if ( (imageB != null) && (imageB == imageActive)) {
                    this.resetRGB(RGBTB);
                }
                imageA.notifyImageDisplayListeners(true, 1, RGBTA);
                if (imageB != null) {
                    imageB.notifyImageDisplayListeners(true, 1, RGBTB);
                }
            }

        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.resetLUT");
        }
    }

    /**
     * Resets the paint brush size to the remembered value.
     */
    public void resetPaintBrushSize() {

        // don't reset unless we've remembered a brush size
        if (previousPaintBrush != -1) {
            setPaintBrushSize(previousPaintBrush);
        }

        previousPaintBrush = -1;
    }

    /**
     * This method saves the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are saved. If this is a color image, then only the functions are saved.
     * 
     * @param filename filename to save LUT as
     * @param dirName directory to save LUT to
     */
    public void saveLUTandTransferFunction(final String filename, final String dirName) {
        ModelRGB rgb;
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (imageActive == imageA) {
            img = this.getImageA();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = this.getRGBTA();
                lut = null;
            } else {
                useLUT = true;
                rgb = null;
                lut = this.getLUTa();
            }
        } else {
            img = this.getImageB();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = this.getRGBTB();
                lut = null;
            } else {
                useLUT = true;
                rgb = null;
                lut = this.getLUTb();
            }
        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);
            }

            fileHistoLUT.writeLUTandTransferFunction();
            Preferences.setDefaultDisplay(DefaultDisplay.LUT);

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
        }

    } // end saveLUTAs()

    /**
     * This method reads a LUT and Transfer function from a file diretory specified.
     * 
     * @param filename filename of LUT
     * @param dirName directory of LUT
     */
    public void readLUTandTFunctFrom(final String filename, final String dirName) {
        ModelRGB rgb;
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (imageActive == imageA) {
            img = this.getImageA();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = this.getRGBTA();
                lut = null;
            } else {
                useLUT = true;
                rgb = null;
                lut = this.getLUTa();
            }
        } else {
            img = this.getImageB();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = this.getRGBTB();
                lut = null;
            } else {
                useLUT = true;
                rgb = null;
                lut = this.getLUTb();
            }
        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);
            }

            fileHistoLUT.readLUTandTransferFunction(false);

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
        }

    } // end saveLUTAs()

    /**
     * Save user defined LUT table.
     */
    public void saveUDLUT() {

        // save both the LUT and the transfer functions
        final String fName = "userdefine.lut";
        final String dName = Preferences.getPreferencesDir();

        saveLUTandTransferFunction(fName, dName);

    }

    /**
     * Sets the active image for drawing VOIs.
     * 
     * @param active IMAGE_A or IMAGE_B
     */
    public void setActiveImage(final int active) {
        winLevelSet = false;
        //voiHandler.setActiveVOI_ID(active);

        if ( (active == ViewJComponentBase.IMAGE_A) || (imageB == null)) {
            imageActive = imageA;
            imageBufferActive = imageBufferA;

            if ( !paintBitmapSwitch) {
                paintBitmap = imageA.getMask();
            }
        } else if (active == ViewJComponentBase.IMAGE_B) {
            imageActive = imageB;
            imageBufferActive = imageBufferB;
        }

        m_kPatientSlice.setActiveImage(imageActive);
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     * 
     * @param value amount [0,100] that is the percentage of Image A to be displayed
     */
    public void setAlphaBlend(final int value) {

        if ( (value >= 0) && (value <= 100)) {
            alphaBlend = value / 100.0f;
            alphaPrime = 1 - alphaBlend;
        }
    }

    /**
     * The frame in which the image(s) is displayed, allocates the memory and uses this method to pass the references to
     * the buffers.
     * 
     * @param imgBufferA storage buffer used to display image A
     * @param imgBufferB storage buffer used to display image B
     * @param pixBuff storage buffer used to build a displayable image
     * @param pixBuffB storage buffer used to build a displayable imageB for the window
     */
    public void setBuffers(final float[] imgBufferA, final float[] imgBufferB, final int[] pixBuff, final int[] pixBuffB) {
        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
        cleanImageBufferA = pixBuff;
        cleanImageBufferB = pixBuffB;
        imageBufferActive = imageBufferA;
        m_kPatientSlice.setBuffers(imgBufferA, imgBufferB);
    }

    /**
     * Sets the image to display in "Checkerboard" mode with the given numbers of rows and columns.
     * 
     * @param rowCheckers int # of rows
     * @param columnCheckers int # of columns
     */
    public void setCheckerboard(final int rowCheckers, final int columnCheckers) {
    	

    	cleanBuffer(ViewJComponentBase.IMAGE_B);
    	
        this.nRowCheckers = rowCheckers;
        this.nColumnCheckers = columnCheckers;

        final ViewJFrameBase vjfb = (this).getFrame();
        final ViewControlsImage vci = vjfb.getControls();

        if (vci != null) {

            if ( (rowCheckers < 1) || (columnCheckers < 1)) {
                vci.setAlphaSliderEnabled(true);
            } else {
                vci.setAlphaSliderEnabled(false);
            }
        }
    }

    /**
     * Changes the Crosshair cursor to be either the default crosshair or a pre-existing gif.
     * 
     * @param curs the new crosshair cursor
     */
    public void setCrosshairCursor(final Cursor curs) {
        this.crosshairCursor = curs;
    }

    /**
     * Switches modes based on the variable mode. Sets voiHandler.getRubberband() activity and the cursor.
     * 
     * @param mode the integer mode
     */
    public void setCursorMode(final int mode) {
        this.cursorMode = mode;
        boolean isImageFrame = frame instanceof ViewJFrameImage;

        switch (mode) {

        case ViewJComponentBase.DEFAULT:
            setCursor(MipavUtil.smallPointerCursor);
            if (isImageFrame) {
                getFrame().getControls().getTools()
                .setPointerSelected();
            }

            break;

        case ViewJComponentBase.PROBE:
            setCursor(MipavUtil.probeCursor);
            if (isImageFrame) {
                getFrame().getControls().getTools()
                .setPointerSelected();
            }

            break;

        case ViewJComponentBase.WAND:
            setCursor(MipavUtil.wandCursor); // Hand cursor
            break;

        case ViewJComponentBase.PAINT_VOI:
            setCursor(MipavUtil.blankCursor);
            getFrame().requestFocus();
            break;

        case ViewJComponentEditImage.PAINT_CAN:
            setCursor(crosshairCursor);
            break;

        case ViewJComponentEditImage.PAINT_VASC:
            setCursor(crosshairCursor);
            break;

        case ViewJComponentEditImage.DROPPER_PAINT:
            setCursor(crosshairCursor);
            break;

        case ViewJComponentEditImage.ERASER_PAINT:
            setCursor(MipavUtil.blankCursor);
            getFrame().requestFocus();
            break;

        case ViewJComponentEditImage.MAG_REGION:
            setCursor(MipavUtil.magRegionCursor);
            break;

        case ViewJComponentEditImage.WIN_REGION:
            setCursor(MipavUtil.magRegionCursor);
            break;

        case ViewJComponentEditImage.ZOOMING_IN:

            Toolkit toolkit = Toolkit.getDefaultToolkit();
            Cursor magnifyCursor = toolkit.createCustomCursor(MipavUtil
                    .getIcon("zoomin.gif").getImage(), new Point(10, 10),
            "zoomin");

            setCursor(magnifyCursor);
            break;

        case ViewJComponentEditImage.ZOOMING_OUT:
            toolkit = Toolkit.getDefaultToolkit();

            Cursor unmagnifyCursor = toolkit.createCustomCursor(MipavUtil
                    .getIcon("zoomout.gif").getImage(), new Point(10, 10),
            "zoomout");

            setCursor(unmagnifyCursor);
            break;

        }
    }

    /**
     * Sets whether a fuzzy image is displayed.
     * 
     * @param val whether to show the fuzzy connectedness image
     */
    public void setDisplayFuzzy(final boolean val) {
        this.displayFuzzy = val;
    }

    /**
     * Enables or disables the component for modification.
     * 
     * @param flag true = modify, and false = locked
     */
    public void setEnabled(final boolean flag) {
        modifyFlag = flag;
        //voiHandler.getRubberband().setActive(flag);
    }

    /**
     * Sets the frameControls.
     * 
     * @param controls the controls with color and opacity information
     */
    public void setFrameControls(final ViewControlsImage controls) {
        frameControls = controls;
    }

    // When the apply button is pressed, JDialogPaintGrow sets the following 7 parameters used in regionGrow.
    /**
     * Sets whether fuzzy connectedness is used and the fuzzy threshold.
     * 
     * @param val the fuzzy connectedness threshold value
     */
    public void setFuzzyThreshold(final float val) {
        this.fuzzyThreshold = val;
    }

    /**
     * Sets the RegionGrowDialog for this class (usually used to set it to null).
     * 
     * @param dialog the paint grow dialog
     */
    public void setGrowDialog(final RegionGrowDialog dialog) {
        growDialog = dialog;
    }

    /**
     * Sets the hasThreshold1 for setPaintBuffers.
     * 
     * @param hasThreshold1 whether the paint buffer has a threshold1
     */
    public void setHasThreshold1(final boolean hasThreshold1) {
        m_kPatientSlice.setHasThreshold1(hasThreshold1);
    }

    /**
     * Sets the hasThreshold2 for setPaintBuffers.
     * 
     * @param hasThreshold2 whether the paint buffer has a threshold2
     */
    public void setHasThreshold2(final boolean hasThreshold2) {
        m_kPatientSlice.setHasThreshold2(hasThreshold2);
    }

    /**
     * Changes the color used to highlight the currently on-top image.
     * 
     * @param c the new color to use.
     */
    public void setHighlightColor(final Color c) {
        toppedColor = c;
    }

    /**
     * Sets component's ImageA. assumes dimensionality same as image B's for now.
     * 
     * @param image imageA
     */
    public void setImageA(final ModelImage image) {
        imageA = image;
        m_kPatientSlice.setImageA(image);
        setZoom(1, 1); // sets zoom
    }

    /**
     * Sets component's ImageB. !!!!!! assumes dimensionality same as image A's for now will fix soon.
     * 
     * @param image imageB
     */
    public void setImageB(final ModelImage image) {
        imageB = image;
        m_kPatientSlice.setImageB(image);

        if (imageB == null) {

            // remove checker boarding
            nRowCheckers = -1;
            nColumnCheckers = -1;
        }
    }

    /**
     * Sets component's ImageB buffer.
     * 
     * @param buffer image buffer to put in the buffer for Image B
     */
    public void setImageBufferB(final float[] buffer) {
        imageBufferB = buffer;
    }

    /**
     * Sets the colocalize image.
     * 
     * @param imageColocalize the colocalization image
     */
    public void setImageColocalize(final ModelImage imageColocalize) {
        m_kPatientSlice.setImageColocalize(imageColocalize);
    }

    /* ********************************************************************** */
    /* ****************************** Accessors ***************************** */
    /* ********************************************************************** */

    /**
     * DOCUMENT ME!
     * 
     * @param imageExtents int[]
     */
    public void setImageExtents(final int[] imageExtents) {
        this.imageExtents = imageExtents;
        if (m_kPatientSlice != null) {
            m_kPatientSlice.setImageExtents();
        }
    }

    /**
     * Sets the intensityDropper intensity.
     * 
     * @param intensityDropper the dropper intensity
     */
    public void setIntensityDropper(final float intensityDropper) {
        this.intensityDropper = intensityDropper;
    }

    /**
     * Sets less for regionGrow.
     * 
     * @param val lower region grow delta
     */
    public void setLess(final float val) {
        this.less = val;
    }

    /**
     * Sets lessB for regionGrow.
     * 
     * @param val lower region grow delta
     */
    public void setLessB(final float val) {
        this.lessB = val;
    }

    /**
     * Sets lessG for regionGrow.
     * 
     * @param val lower region grow delta
     */
    public void setLessG(final float val) {
        this.lessG = val;
    }

    /**
     * Sets lessR for regionGrow.
     * 
     * @param val lower region grow delta
     */
    public void setLessR(final float val) {
        this.lessR = val;
    }

    /**
     * Sets the log magnitude display flag.
     * 
     * @param flag if true display log of the Magnitude of the complex image
     */
    public void setLogMagDisplay(final boolean flag) {
        logMagDisplay = flag;
    }

    /**
     * accessor that sets the model LUT for the imageA.
     * 
     * @param LUT the model LUT
     */
    public void setLUTa(final ModelLUT LUT) {
        LUTa = LUT;
        m_kPatientSlice.setLUTa(LUT);
    }

    /**
     * Sets the model LUTb for the imageB.
     * 
     * @param LUT the model LUT
     */
    public void setLUTb(final ModelLUT LUT) {
        LUTb = LUT;
        m_kPatientSlice.setLUTb(LUT);
    }

    /**
     * Sets maxDistance for regionGrow.
     * 
     * @param val the maximum region grow distance
     */
    public void setMaxDistance(final int val) {
        this.maxDistance = val;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param flag DOCUMENT ME!
     */
    public void setModifyFlag(final boolean flag) {
        modifyFlag = flag;
    }

    /**
     * Sets more for regionGrow.
     * 
     * @param val upper region grow delta
     */
    public void setMore(final float val) {
        this.more = val;
    }

    /**
     * Sets moreB for regionGrow.
     * 
     * @param val upper region grow delta
     */
    public void setMoreB(final float val) {
        this.moreB = val;
    }

    /**
     * Sets moreG for regionGrow.
     * 
     * @param val upper region grow delta
     */
    public void setMoreG(final float val) {
        this.moreG = val;
    }

    /**
     * Sets moreR for regionGrow.
     * 
     * @param val upper region grow delta
     */
    public void setMoreR(final float val) {
        this.moreR = val;
    }

    /**
     * If true do not getMask on a setActiveImage command so as to keep the mask from the old active image.
     * 
     * @param paintBitmapSwitch if true do not getMask on a setActiveImage command
     */
    public void setPaintBitmapSwitch(final boolean paintBitmapSwitch) {
        this.paintBitmapSwitch = paintBitmapSwitch;
    }

    /**
     * Switches modes based on the variable mode. Sets the number of pixels to be drawn when painting.
     * 
     * @param paintBrushSize the integer mode
     */
    public void setPaintBrushSize(final int paintBrushSize) {
        this.paintBrushSize = paintBrushSize;
    }

    /**
     * Sets the paint mask.
     * 
     * @param mask the new paint mask
     */
    public void setPaintMask(final BitSet mask) {
        paintBitmap = mask;
        if ( imageActive != null )
        {
            imageActive.setMask(paintBitmap);
        }
    }

    /**
     * Prints ModelImage information at the mouse location.
     * 
     * @param xS mouse x location
     * @param yS mouse y location
     */
    public void setPixelInformationAtLocation(final int xS, final int yS, boolean isControlDown) {

        try {
            StringBuilder str = new StringBuilder();

            str.append("  X: ").append(xS).append(" Y: ").append(yS);
            
            if (imageActive.isColorImage()) {
                str.append(
                        "  R:  ").append(imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 1]).append(
                        "  G:  ").append(imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 2]).append(
                        "  B:  ").append(imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 3]);
            } else if(imageActive.isComplexImage()) { 
                switch(Preferences.getComplexDisplay()) {
                case APLUSBI:
                    str.append("  Intensity:  ").append(imageBufferActive[2*((yS * imageActive.getExtents()[0]) + xS)]).append(" + ").append(
                            imageBufferActive[2*((yS * imageActive.getExtents()[0]) + xS) + 1]).append("*i");
                    break;
                case REITHETA:
                    double a = imageBufferActive[2*((yS * imageActive.getExtents()[0]) + xS)];
                    double b = imageBufferActive[2*((yS * imageActive.getExtents()[0]) + xS) + 1];
                    double r = Math.sqrt(a*a + b*b);
                    double theta = Math.atan(b/a);
                    str.append("  Intensity:  ").append(r).append(" * e^(i*");
                    if(a > 0) {
                        ; //no action necessary
                    } else if(a < 0) {
                        if(b >= 0) {
                            str.append(theta+Math.PI);
                        } else {
                            str.append(theta-Math.PI);
                        }
                    } else if(a == 0) {
                        if(b > 0) {
                            str.append(Math.PI/2);
                        } else if(b < 0) {
                            str.append(-Math.PI/2);
                        } else {
                            str.append(0);
                        }
                    }
                    str.append(")");
                    break;
                case MAGNITUDE: //TODO: this displays different values depending on display preferences at time of image loading
                default:
                    str.append("  Intensity:  ").append(String.valueOf(imageBufferActive[ (yS * imageActive.getExtents()[0]) + xS]));
                }
            } else {
                str.append("  Intensity:  ").append(String.valueOf(imageBufferActive[ (yS * imageActive.getExtents()[0]) + xS]));
            }
            
            if ( (imageActive.getOrigin()[0] != 0) || (imageActive.getOrigin()[1] != 0)
                    || ( (imageActive.getNDims() > 2) && (imageActive.getOrigin()[2] != 0))) {
                final String[] values = ViewJComponentEditImage.getScannerPositionLabels(imageActive, new Vector3f(xS,
                        yS, slice));
                
                if (values != null) {
                    str.append(" Position: ").append(values[0]).append(" ").append(values[1]).append(" ").append(values[2]);
                }
            } 
            
            frame.setMessageText(str.toString());
            if(isControlDown) {
            	Preferences.data(str.toString() + "\n");
            }
        } catch (final ArrayIndexOutOfBoundsException error) {
            frame.setMessageText("  X: " + String.valueOf( (xS)) + " Y: " + String.valueOf( (yS)));
        }
    }

    
    
    public synchronized boolean isCheckerboardAnimate() {
		return doCheckerboardAnimate;
	}

	public synchronized void setCheckerboardAnimate(boolean doCheckerboardAnimate) {
		this.doCheckerboardAnimate = doCheckerboardAnimate;
	}
	
	public synchronized boolean isMakingCheckerboard() {
		return makingCheckerboard;
	}

	public synchronized void setMakingCheckerboard(boolean makingCheckerboard) {
		this.makingCheckerboard = makingCheckerboard;
	}

	public synchronized int[] getCleanImageBufferB() {
		return cleanImageBufferB;
	}
	

	public synchronized int[] getMaxExtents() {
		return maxExtents;
	}

	public synchronized int getxSep() {
		return xSep;
	}

	public synchronized void setxSep(int xSep) {
		this.xSep = xSep;
	}

	public synchronized int getySep() {
		return ySep;
	}

	public synchronized void setySep(int ySep) {
		this.ySep = ySep;
	}

	/*public synchronized int getCheckerboardCounter() {
		//notify();
		return checkerboardCounter;
	}

	public synchronized void setCheckerboardCounter(int checkerboardCounter) {
		this.checkerboardCounter = checkerboardCounter;
		//notify();
	}
*/
	/**
     * Sets the variables used to remember the point where the last region grow was started from.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param val intensity at the point (x,y,z)
     */
    public void setRegionGrowVars(final short x, final short y, final short z, final float val) {
        xPG = x;
        yPG = y;
        zPG = z;
        seedVal = val;
    }

    // The following 2 functions set the RGB tables for ARGB images A and B.
    /**
     * Sets the RGB table for ARGB image A.
     * 
     * @param RGBT RGB table
     */
    public void setRGBTA(final ModelRGB RGBT) {
        RGBTA = RGBT;
        m_kPatientSlice.setRGBTA(RGBT);
    }

    /**
     * Sets the RGB table for ARGB image B.
     * 
     * @param RGBT RGB table
     */
    public void setRGBTB(final ModelRGB RGBT) {
        RGBTB = RGBT;
        m_kPatientSlice.setRGBTB(RGBT);
    }

    /**
     * Sets whether to show intensity in mag. box
     * 
     * @param flag whether to show intensity in mag. box
     */
    public void setShowMagIntensity(final boolean flag) {
        showMagIntensity = flag;
    }

    /**
     * Sets whether the slice number will be shown in the lower left hand corner of the image.
     * 
     * @param flag whether to display the slice number
     */
    public void setShowSliceNum(final boolean flag) {
        showSliceNumber = flag;
    }

    /**
     * Sets sizeLimit for regionGrow.
     * 
     * @param val the maximum region grow size
     */
    public void setSizeLimit(final int val) {
        this.sizeLimit = val;
    }

    /**
     * Sets the slice of the image.
     * 
     * @param _slice image slice to be displayed
     */
    public void setSlice(final int _slice) {
        slice = _slice;
        m_kPatientSlice.updateSlice(slice);
        //if ( voiManager != null )
        //{
        //    voiManager.setSlice(slice);
        //}
    }

    /**
     * Sets the string painted on the lower left.
     * 
     * @param str str that is painted on the lower left of image
     */
    public void setStringOverride(final String str) {
        stringOverride = str;
    }

    /**
     * Sets the booleans for using thresholds in setColorPaintBuffers.
     * 
     * @param useRedThreshold whether to threshold the red paint buffer
     * @param useGreenThreshold whether to threshold the green paint buffer
     * @param useBlueThreshold whether to threshold the blue paint buffer
     */
    public void setThresholdColors(final boolean useRedThreshold, final boolean useGreenThreshold,
            final boolean useBlueThreshold) {
        m_kPatientSlice.setThresholdColors(useRedThreshold, useGreenThreshold, useBlueThreshold);
    }

    /**
     * Sets the thresholds.
     * 
     * @param threshold1 the first threshold
     * @param threshold2 the second threshold
     */
    public void setThresholds(final float threshold1, final float threshold2) {
        m_kPatientSlice.setThresholds(threshold1, threshold2);
    }

    /**
     * Sets the time slice of the image.
     * 
     * @param _slice the time slice to be displayed
     */
    public void setTimeSlice(final int _slice) {
        timeSlice = _slice;
    }

    /**
     * Sets whether a selected VOI is used to calculate the initial variance.
     * 
     * @param val whether to use the selected VOI to get the initial variance for the fuzzy regionGrow
     */
    public void setUseVOI(final boolean val) {
        this.useVOI = val;
    }

    /**
     * If true varies thresholds with region growth.
     * 
     * @param variableThresholds boolean
     */
    public void setVariableThresholds(final boolean variableThresholds) {
        this.variableThresholds = variableThresholds;
    }

    /**
     * For generating the display of 1 or 2 images.
     * 
     * @param tSlice t (time) slice to show
     * @param zSlice z slice to show
     * @param forceShow forces this method to import image and recalculate java image
     * 
     * @return boolean to indicate if the show was successful
     */
    public boolean show(final int tSlice, final int zSlice, final boolean forceShow) {
        return show(tSlice, zSlice, null, null, forceShow, interpMode);
    }

    /**
     * Shows the image and the VOI(s).
     * 
     * @param tSlice t (time) slice to show
     * @param zSlice z slice to show
     * @param _LUTa LUTa - to change to new LUT for imageA else null
     * @param _LUTb LUTb - to change to new LUT for imageB else null
     * @param forceShow forces this method to import image and recalculate java image
     * 
     * @return boolean to indicate if the show was successful
     */
    public boolean show(final int tSlice, final int zSlice, final ModelLUT _LUTa, final ModelLUT _LUTb,
            final boolean forceShow) {
        return show(tSlice, zSlice, _LUTa, _LUTb, forceShow, interpMode);
    }

    /**
     * Shows the image and the VOI(s).
     * 
     * @param tSlice t (time) slice to show
     * @param zSlice z slice to show
     * @param _LUTa LUTa - to change to new LUT for imageA else null
     * @param _LUTb LUTb - to change to new LUT for imageB else null
     * @param forceShow forces this method to import image and recalculate java image
     * @param interpMode image interpolation method (Nearest or Smooth)
     * 
     * @return boolean to indicate if the show was successful
     */
    public boolean show(final int tSlice, final int zSlice, final ModelLUT _LUTa, final ModelLUT _LUTb,
            final boolean forceShow, final int interpMode) {

        if (interpMode > -1) {
            setInterpolationMode(interpMode);
        }

        m_kPatientSlice.setLUTa(_LUTa);
        m_kPatientSlice.setLUTb(_LUTb);
        m_kPatientSlice.updateSlice(zSlice);

        if (cleanImageBufferB == null) {
            cleanImageBufferB = new int[imageExtents[0] * imageExtents[1]];
        }

        if (m_kPatientSlice.showUsingOrientation(tSlice, cleanImageBufferA, cleanImageBufferB, forceShow, false)) {
            cleanImageB = null;
            cleanBuffer(ViewJComponentBase.IMAGE_A);
            cleanBuffer(ViewJComponentBase.IMAGE_B);

            slice = zSlice;
            setSliceString(String.valueOf(slice));
            paintComponent(getGraphics());

            return true;
        } else {
            return false;
        }
    } // end of show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow)

    /**
     * Resets the buffer to 0s and displays a blank image.
     * 
     * @return boolean to indicate that the show was successful
     */
    public boolean showBlank() {

        for (int i = 0; i < pixBuffer.length; i++) {
            pixBuffer[i] = 0;
        }

        importImage(pixBuffer); // Method in parent class to import the image
        importImageB(pixBuffer);

        setSliceString("");
        slice = -99;

        return true;
    }

    /**
     * Display statistics about the grown region.
     * 
     * @param count Number of pixels (voxels)
     * @param leadString the string to prepend to message containing region growth statistics
     */
    public void showRegionInfo(final int count, final String leadString) {
        float volume;
        float area;

        try {
            String str = new String();

            if (imageActive.getNDims() == 2) {
                area = count * imageActive.getResolutions(0)[0] * imageActive.getResolutions(0)[1];
                str = imageActive.getFileInfo(0).getAreaUnitsOfMeasureStr();

                if (leadString != null) {
                    frame.getUserInterface().setDataText(
                            leadString + " region grow: pixels = " + count + "\t  area = " + area + str + "\n");
                } else {
                    frame.getUserInterface().setDataText(
                            "statistics pixels = " + count + "\t  area = " + area + str + "\n");
                }

            } else {
                volume = count * imageActive.getResolutions(0)[0] * imageActive.getResolutions(0)[1]
                                                                                                  * imageActive.getResolutions(0)[2];

                str = imageActive.getFileInfo(0).getVolumeUnitsOfMeasureStr();

                if (leadString != null) {
                    frame.getUserInterface().setDataText(
                            leadString + " region grow: pixels = " + count + "\t  volume = " + volume + str + "\n");
                } else {
                    frame.getUserInterface().setDataText(
                            "statistics pixels = " + count + "\t  volume = " + volume + str + "\n");
                }
            }
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.showRegionInfo");
        }
    }

    /**
     * Display statistics about the grown region.
     * 
     * @param count Number of pixels (voxels)
     * @param total Sum of pixel intensities
     * @param mean Average pixel intensity
     * @param stdDev Standard deviation of pixel intensities
     * @param leadString the string to prepend to message containing region growth statistics
     */
    public void showRegionInfo(final int count, final float total[], final float mean[], final float stdDev[],
            String leadString) {
        float volume;
        float area;
        int pad;
        int i;
        String areaString;
        String volumeString;

        if (leadString.length() < 25) {
            pad = 25 - leadString.length();
            for (i = 0; i < pad; i++) {
                leadString = leadString.concat(" ");
            }
        }

        try {
            String str = new String();
            if (imageActive.getNDims() == 2) {
                area = count * imageActive.getResolutions(0)[0] * imageActive.getResolutions(0)[1];
                str = imageActive.getFileInfo(0).getAreaUnitsOfMeasureStr();
                areaString = String.valueOf(area) + str;
                if (areaString.length() < 20) {
                    pad = 20 - areaString.length();
                    for (i = 0; i < pad; i++) {
                        areaString = areaString.concat(" ");
                    }
                }

                if (leadString != null) {
                    frame.getUserInterface().setDataText("\n" + leadString + "\tpixels" + "\t\tarea");
                } else {
                    frame.getUserInterface().setDataText("\nstatistics              " + "\tpixels" + "\t\tarea");
                }
                if (total.length == 1) {
                    frame.getUserInterface().setDataText(
                    "\t\ttotal intensity\t\tmean intensity\t\tstandard deviation\n");
                    frame.getUserInterface().setDataText("\n\t\t" + count + "\t\t" + areaString);
                } else {
                    frame.getUserInterface().setDataText("\n\t\t" + count + "\t\t" + areaString + "\n");
                }

            } else {
                volume = count * imageActive.getResolutions(0)[0] * imageActive.getResolutions(0)[1]
                                                                                                  * imageActive.getResolutions(0)[2];

                str = imageActive.getFileInfo(0).getVolumeUnitsOfMeasureStr();
                volumeString = String.valueOf(volume) + str;
                if (volumeString.length() < 20) {
                    pad = 20 - volumeString.length();
                    for (i = 0; i < pad; i++) {
                        volumeString = volumeString.concat(" ");
                    }
                }

                if (leadString != null) {
                    frame.getUserInterface().setDataText("\n" + leadString + "\tpixels" + "\t\tvolume");
                } else {
                    frame.getUserInterface().setDataText("\nstatistics              " + "\tpixels" + "\t\tvolume");
                }
                if (total.length == 1) {
                    frame.getUserInterface().setDataText(
                    "\t\ttotal intensity\t\tmean intensity\t\tstandard deviation\n");
                    frame.getUserInterface().setDataText("\n\t\t" + count + "\t\t" + volumeString);
                } else {
                    frame.getUserInterface().setDataText("\n\t\t" + count + "\t\t" + volumeString + "\n");
                }
            }

            if (total.length == 1) {
                frame.getUserInterface().setDataText("\t" + total[0] + "\t\t" + mean[0] + "\t\t" + stdDev[0] + "\n");
            } else {
                frame.getUserInterface().setDataText("\t\ttotal intensity\t\tmean intensity\t\tstandard deviation\n");
                frame.getUserInterface().setDataText(
                        "red\t\t" + total[0] + "\t\t" + mean[0] + "\t\t" + stdDev[0] + "\n");
                frame.getUserInterface().setDataText(
                        "green\t\t" + total[1] + "\t\t" + mean[1] + "\t\t" + stdDev[1] + "\n");
                frame.getUserInterface().setDataText(
                        "blue\t\t" + total[2] + "\t\t" + mean[2] + "\t\t" + stdDev[2] + "\n");
            }
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.showRegionInfo");
        }
    }

    /**
     * Opens a JDialogStatistics to allow computation ofROI statistics.
     */
    public void showStatisticsCalculator() {

        if (imageStatList == null) {

            if ( (imageActive.getVOIs() != null) && (imageActive.getVOIs().size() != 0)) {
                imageStatList = new JDialogVOIStatistics(imageActive.getVOIs());
                imageStatList.setVisible(true);
                // addVOIUpdateListener(imageStatList); // i'd rather not do it this way...
            } else {
                MipavUtil.displayError("A VOI must be present to use the statistics calculator");
            }
        } else {
            imageStatList.refreshVOIList(imageActive.getVOIs());
            imageStatList.setVisible(true);
        }
    }

    /**
     * Undoes the last paint.
     */
    public void undoLastPaint() {
        paintBitmap.clear();
        for (int i = paintBitmapBU.nextSetBit(0); i >= 0; i = paintBitmapBU.nextSetBit(i + 1)) {
            paintBitmap.set(i);
        }

        if (growDialog != null) {
            growDialog.notifyPaintListeners( (growDialog instanceof JDialogPaintGrow), false, paintBitmap);
        } else {
            imageActive.notifyImageDisplayListeners(null, true);
        }
    }

    /**
     * Causes the image to update its paint bit mask and redisplay itself.
     * 
     * @param region new paint region bit set
     * @param backup whether to save the previous paint mask to allow the update to be un-done
     * @param isGrower whether this paint listener is the one that did the region grow
     */
    public void updatePaint(final BitSet region, final boolean backup, final boolean isGrower) {

        if (isGrower) {

            if (backup) {
                paintBitmapBU.clear();
                for (int i = paintBitmap.nextSetBit(0); i >= 0; i = paintBitmap.nextSetBit(i + 1)) {
                    paintBitmapBU.set(i);
                }
            }

            paintBitmap = region;
        } else {

            // should only get method called if this component image is not the paint grow listener
            // who performed the region grow (ie - in the ViewJFramePaintVasculature)
            int z;
            int index;

            paintBitmap.clear();

            for (int i = 0; i < region.length(); i++) {

                if (frame instanceof ViewJFramePaintVasculature) {
                    z = MipavMath.round( ((ViewJFramePaintVasculature) frame).getMIPZValue(i));
                    index = (z * imageExtents[0] * imageExtents[1]) + i;
                } else {

                    // not in the mip image, so i is iterating over all the image slices already
                    index = i;
                }

                if (region.get(index)) {
                    paintBitmap.set(i);
                }
            }
        }

        imageActive.notifyImageDisplayListeners(null, true);
    }

    /**
     * Updates the Paint Cursor's BufferedImage with the correct color/opacity.
     */
    public void updatePaintBrushCursor() {
        int opacity = MipavMath.round(255 * .3);
        Color paintColor = Color.red;
        try {
            opacity = (int) (frame.getControls().getTools().getOpacity() * 255);
            paintColor = frame.getControls().getTools().getPaintColor();
        } catch (final Exception e) {}

        final Color brushColor = new Color(paintColor.getRed(), paintColor.getGreen(), paintColor.getBlue(), opacity);
        int counter = 0;

        for (int y = 0; y < paintBrushDim.height; y++) {

            for (int x = 0; x < paintBrushDim.width; x++, counter++) {

                if (paintBrush.get(counter)) {
                    paintImage.setRGB(x, y, brushColor.getRGB());
                }
            }
        }
    }

    /**
     * Sets this component to paint the "on top" high-light. It is not required to use the coloured rectangle to
     * high-light the on-top component, but the component is not aware of its position in the user-interface, and thus,
     * that the user-interface controls point to it.
     * 
     * @param hilite <code>true</code> will set this component to paint the 'on-top' high-light the next time it is
     *            redrawn. <code>false</code>, of course, will not let the component paint the coloured rectangle
     *            when repainted.
     */
    public void useHighlight(final boolean hilite) {
        onTop = hilite;
    }


    protected void draw3DVOIs(Graphics offscreenGraphics2d, boolean bBlend)
    {
        if ( imageA.getVOIs() != null )
        {
            ViewVOIVector VOIs = imageA.getVOIs();
            if (VOIs != null && voiManager != null) {
                int nVOI = VOIs.size();
                for (int i = nVOI - 1; i >= 0; i--) {    
                    VOI kVOI = VOIs.get(i);
                    Vector<VOIBase> kCurves = kVOI.getCurves();
                    if(kCurves != null) {
                        for ( int k = 0; k < kCurves.size(); k++ )
                        {
                            VOIBase kVOI3D = kCurves.get(k);
                            offscreenGraphics2d.setColor( kVOI.getColor() );
                            voiManager.draw( kVOI3D, 
                                        imageA.getResolutions(0), 
                                        imageA.getUnitsOfMeasure(), slice, 
                                        offscreenGraphics2d, (orientation == FileInfoBase.UNKNOWN_ORIENT) );
                        }
                    }
                }
            }
        }
        if ( (imageB != null) && (imageB.getVOIs() != null) )
        {
            ViewVOIVector VOIs = imageB.getVOIs();
            if (VOIs != null && voiManager != null) {
                int nVOI = VOIs.size();

                for (int i = nVOI - 1; i >= 0; i--) {    
                    VOI kVOI = VOIs.get(i);
                    Vector<VOIBase> kCurves = kVOI.getCurves();
                    if(kCurves != null) {
                        for ( int k = 0; k < kCurves.size(); k++ )
                        {
                            VOIBase kVOI3D = kCurves.get(k);
                            voiManager.draw( kVOI3D, 
                                        imageB.getResolutions(0), 
                                        imageB.getUnitsOfMeasure(), slice, 
                                        offscreenGraphics2d, (orientation == FileInfoBase.UNKNOWN_ORIENT) );
                        }
                    }
                }
            }
        }
    }

    /**
     * Calls dispose to dump this instance.
     * 
     * @throws Throwable DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected int getBrushSize() {

        switch (paintBrushSize) {

        case 0:
            return 1;

        case 1:
            return 4;

        case 2:
            return 8;

        case 3:
            return 16;

        default:
            return 0;
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected int getHBrushSize() {
        return getBrushSize() / 2;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param reverse DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected float getZoomMagnitudeX(final boolean reverse) {
        return getZoomMagnitude(getZoomX(), reverse);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param reverse DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected float getZoomMagnitudeY(final boolean reverse) {
        return getZoomMagnitude(getZoomY(), reverse);
    }

    /**
     * When a mousePressed event is triggered and the mode is DROPPER_PAINT, ERASER_PAINT, PAINT_VOI, or MAG_REGION this
     * function is called. It is shared with the derived classes.
     * 
     * @param mouseEvent the mouseEvent that triggered this function call.
     */
    protected void mousePressedPaint(final MouseEvent mouseEvent) {
        int xS = getScaledX(mouseEvent.getX()); // zoomed x. Used as cursor
        int yS = getScaledY(mouseEvent.getY()); // zoomed y. Used as cursor

        if ( (xS < 0) || (xS >= imageActive.getExtents()[0]) || (yS < 0) || (yS >= imageActive.getExtents()[1])) {
            return;
        }

        if (cursorMode == ViewJComponentBase.DROPPER_PAINT) {

            if (imageActive.isColorImage()) {
                final Color dropperColor = new Color(
                        (int) imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 1],
                        (int) imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 2],
                        (int) imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 3]);
                frame.getControls().getTools().setPaintColor(dropperColor);
            } else if(imageActive.isComplexImage()) { 
                int loc = ((yS * imageActive.getExtents()[0]) + xS)*2;
                intensityDropper = (float) Math.sqrt(imageBufferActive[loc]*imageBufferActive[loc] + imageBufferActive[loc+1]*imageBufferActive[loc+1]);
                frame.getControls().getTools().setIntensityPaintName(String.valueOf((int) (intensityDropper)));
            } else {
                intensityDropper = imageBufferActive[ (yS * imageActive.getExtents()[0]) + xS];
                frame.getControls().getTools().setIntensityPaintName(String.valueOf((int) (intensityDropper)));
            }
        }

        if (cursorMode == ViewJComponentBase.ERASER_PAINT) {
            performPaint(mouseEvent, true);
            imageActive.notifyImageDisplayListeners();
        } else if (cursorMode == ViewJComponentBase.PAINT_VOI) {
            // backup paintBitmap to paintBitmapBU
            backupPaintBitmap();

            xS = getScaledX(mouseEvent.getX()); // zoomed x. Used as cursor
            yS = getScaledY(mouseEvent.getY()); // zoomed y. Used as cursor

            performPaint(mouseEvent, mouseEvent.getModifiers() == InputEvent.BUTTON3_MASK);
            // imageActive.notifyImageDisplayListeners(); 8/12/2008-nish Commented out b/c we want update on mouse
            // release
        }

        if ( (cursorMode == ViewJComponentBase.MAG_REGION) && (mouseEvent.getModifiers() == InputEvent.BUTTON3_MASK)) {

            if ( (magSettings != null) && !magSettings.isVisible()) {
                magSettings.setWidthText((int) (frame.getSize().width * 0.25));
                magSettings.setHeightText((int) (frame.getSize().height * 0.25));
                magSettings.setVisible(true);
            }
        }
    }

    /**
     * Creates the Java image to be displayed from the model image. Makes it from the appropriate slice.
     * 
     * @param slice Slice of image to create java image from.
     * 
     * @return Flag indicating success or failure.
     */
    public boolean createImg(final int slice) {
        m_kPatientSlice.updateSlice(slice);

        if (m_kPatientSlice.showUsingOrientation(0, paintBuffer, null, true, false)) {
            importImage(paintBuffer);
        }
        return true;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    protected void performPaint(final MouseEvent event) {
        performPaint(event, false);
    }

    /**
     * This method will set or clear the paint bitmap as the user paints on-screen.
     * 
     * @param mouseEvent MouseEvent the mouseEvent associated with this paint action
     * @param erase boolean if true, paintBitmap is cleared (erased), otherwise paintBitmap is set (painted)
     */
    protected void performPaint(final MouseEvent mouseEvent, final boolean erase) {

        final int xS = (int) ( (mouseEvent.getX() / (zoomX * resolutionX)) + 0.5);

        final int yS = (int) ( (mouseEvent.getY() / (zoomY * resolutionY)) + 0.5);

        final int brushSize = getBrushSize();
        final int hBrushSize = getHBrushSize();

        if (paintBrush != null) {

            int brushXDim = paintBrushDim.width;
            int brushYDim = paintBrushDim.height;
            if (!(this instanceof ViewJComponentTriImage)) {
	            float res0 = imageA.getFileInfo()[0].getResolutions()[0];
	            float res1 = imageA.getFileInfo()[0].getResolutions()[1];
	            if ((res0 > res1) && (res0/res1 <= 10.0f)) {
	            	brushXDim = (int)Math.round(brushXDim * res1 / res0);
	            }
	            if ((res1 > res0) && (res1/res0 <= 10.0f)) {
	            	brushYDim = (int)Math.round(brushYDim * res0 / res1);
	            }
            } // if (!(this instanceof ViewJComponentTriImage))

            int counter = 0;
            final int offset = imageActive.getSliceSize() * slice;

            final int xDim = imageActive.getExtents()[0];
            final int yDim = imageActive.getExtents()[1];

            for (int height = 0; height < brushYDim; height++) {

                for (int width = 0; width < brushXDim; width++, counter++) {

                    if (paintBrush.get(counter)) {

                        if ( ( (xS + width) < xDim) && ( (yS + height) < yDim)) {
                            final int st = ( (yS + height) * imageActive.getExtents()[0]) + (xS + width);

                            if (erase == true) {
                                paintBitmap.clear(offset + st);
                            } else {
                                paintBitmap.set(offset + st);
                            }
                        }
                    }

                }
            }

        } else {

            int jMin = Math.max(yS - hBrushSize, 0);
            int jMax = Math.min(yS - hBrushSize + brushSize - 1, imageActive.getExtents()[1] - 1);
            int iMin = Math.max(xS - hBrushSize, 0);
            int iMax = Math.min(xS - hBrushSize + brushSize - 1, imageActive.getExtents()[0] - 1);

            int offset = imageActive.getSliceSize() * slice;

            for (int j = jMin; j <= jMax; j++) {

                for (int i = iMin; i <= iMax; i++) {
                    final int st = (j * imageActive.getExtents()[0]) + i;

                    if (erase == true) {
                        paintBitmap.clear(offset + st);
                    } else {
                        paintBitmap.set(offset + st);
                    }
                }
            }

            if (imageActive.isComplexImage()) {
                int temp = iMin;

                iMin = imageActive.getExtents()[0] - Math.max(iMax, 1);
                iMax = imageActive.getExtents()[0] - Math.max(temp, 1);
                temp = jMin;
                jMin = imageActive.getExtents()[1] - Math.max(jMax, 1);
                jMax = imageActive.getExtents()[1] - Math.max(temp, 1);

                if (imageActive.getNDims() == 3) {
                    offset = imageActive.getSliceSize() * (imageActive.getExtents()[2] - Math.max(slice, 1));
                }

                for (int j = jMin; j <= jMax; j++) {

                    for (int i = iMin; i <= iMax; i++) {
                        final int st = (j * imageActive.getExtents()[0]) + i;

                        if (erase == true) {
                            paintBitmap.clear(offset + st);
                        } else {
                            paintBitmap.set(offset + st);
                        }
                    }
                }
            } // if (imageActive.getType() == ModelStorageBase.COMPLEX)

        }
        // changethis

        if ( imageActive != null )
        {
            imageActive.setMask(paintBitmap);
        }
        // imageActive.notifyImageDisplayListeners();
    }

    /**
     * processDefaultMouseDrag performs the mouseDrag operations when in DEFAULT mode. The default operation when the
     * right mouse button is held down is the window-level image adjustment, and setting the ModelImage information at
     * the pixel location. This function in shared with the deried classes.
     * 
     * @param mouseEvent the mouse event
     * @param xS the mouse x location on screen
     * @param yS the mouse y location on screen
     */
    protected void processDefaultMouseDrag(final MouseEvent mouseEvent, final int xS, final int yS) {
        final int xDim = imageActive.getExtents()[0];
        final int yDim = imageActive.getExtents()[1];
        String str;

        try {
            if (cursorMode == ViewJComponentBase.DEFAULT || cursorMode == ViewJComponentBase.VOI_3D ) {
            	
            	//updates winlevel if right mouse button was pressed and user's preferences indicate this should occur
                if ( ((mouseEvent.getModifiers() & InputEvent.BUTTON3_MASK) != 0) && 
                		Preferences.is(Preferences.PREF_SHOW_WINLEV_ON_RIGHT_CLICK)) {

                    // Dragging the mouse with the right mouse button pressed
                    // increases the window when going from left to right.
                    // Dragging the mouse with the right mouse button pressed
                    // increases the level when going from up to down.
                    m_kWinLevel.setAlpha(alphaBlend);

                    final float fX = xS / (float) xDim;
                    final float fY = yS / (float) yDim;
                    //System.out.println("xs is " + xS);
                    //System.out.println("ys is " + yS);
                     m_kWinLevel.updateWinLevel(fX, fY, !winLevelSet, m_kPatientSlice.getActiveLookupTable(),
                            imageActive);
                    setCursor(MipavUtil.winLevelCursor);
                    if ( !winLevelSet) {
                        //setCursor(MipavUtil.winLevelCursor);
                        winLevelSet = true;
                    }
                } // if ((mouseEvent.getModifiers() & MouseEvent.BUTTON3_MASK) != 0)

                //if left mouse button was pressed, the intensity values should be displayed based on the user's preferences
                if ( (mouseEvent.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK) == InputEvent.BUTTON1_DOWN_MASK) {
                    intensityLabel = Preferences.is(Preferences.PREF_SHOW_INTENSITY_ON_LEFT_CLICK);
                    paintComponent(getGraphics());
                }

                setPixelInformationAtLocation(xS, yS, mouseEvent.isControlDown());
            } // if (mode == DEFAULT))

        } catch (final ArrayIndexOutOfBoundsException error) {
            str = "  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS);
            frame.setMessageText(str);

            if ( (mouseEvent.getModifiers() & InputEvent.BUTTON2_MASK) != 0) {
                frame.getUserInterface().setDataText("\n" + str);
            }
        }
    }

    /**
     * >>>>>>> .r533 The purpose of this method is to examine both LUTs to determine if they are zero-based, that is, if
     * they map values of zero to the color R=0, G=0, B=0. If the LUT does, in fact, do this, then this method ensures
     * that those pixels are completely transparent so that no blending takes place for those pixels.
     */
    private void adjustOpacityFor000Color() {
        if (pixBufferB != null) { // no need to execute if there is no image B

            final ViewJFrameBase vjfb = frame;

            ModelLUT modelLUT = vjfb.getLUTa();

            if (modelLUT != null) {
                Color zeroIndexColor; // get the color at index 0 of LUT a

                // test to see if the color is R == 0, G == 0, B == 0
                boolean zeroIndexColorIs000;

                modelLUT = vjfb.getLUTb();

                if (modelLUT != null) {
                    zeroIndexColor = modelLUT.getColor(0);

                    // test to see if the color is R == 0, G == 0, B == 0
                    zeroIndexColorIs000 = ( (zeroIndexColor.getRed() == 0) && (zeroIndexColor.getGreen() == 0) && (zeroIndexColor
                            .getBlue() == 0));

                    if (zeroIndexColorIs000) {

                        for (int i = 0; i < pixBufferB.length; i++) {
                            int temp = pixBufferB[i];
                            temp = temp & 0x00ffffff; // apply mask. temp will equal zero if the pixel should be
                            // transparent

                            if (temp == 0) {
                                pixBufferB[i] = pixBufferB[i] & 0x00ffffff; // make pixel transparent
                            }
                        }
                    }
                }
            }

            if (imageA.isColorImage() && imageB != null) {
                int temp2, temp3;
                for (int i = 0; i < pixBufferB.length; i++) {
                    final int temp = pixBufferB[i];
                    if ( !zeroToOneLUTAdj) {
                        temp2 = temp & 0x00ffffff; // apply mask. temp will equal zero if the pixel should be
                        // transparent
                        if (temp2 == 0) {
                            pixBufferB[i] = pixBufferB[i] & 0x00ffffff; // make pixel transparent
                        }
                    } else {
                        temp3 = temp & 0xffffffff;

                        if (temp3 == 0) {
                            pixBufferB[i] = pixBufferB[i] & 0xffffffff;
                        }
                    }

                }
            }
        }
    }

    /**
     * Backs up the paintBitmap into the paintBitmapBU variable.
     */
    private void backupPaintBitmap() {
        paintBitmapBU.clear();

        for (int i = paintBitmap.nextSetBit(0); i >= 0; i = paintBitmap.nextSetBit(i + 1)) {
            paintBitmapBU.set(i);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param buffer DOCUMENT ME!
     */
    private void cleanBuffer(final int buffer) {

        if (buffer == ViewJComponentBase.IMAGE_A) {

            if ( (pixBuffer == null) || (pixBuffer.length != cleanImageBufferA.length)) {
                pixBuffer = new int[cleanImageBufferA.length];
            }

            System.arraycopy(cleanImageBufferA, 0, pixBuffer, 0, cleanImageBufferA.length);

        } else if (buffer == ViewJComponentBase.IMAGE_B) {

            if (cleanImageBufferB != null) {

                if ( (pixBufferB == null) || (pixBufferB.length != cleanImageBufferB.length)) {
                    pixBufferB = new int[cleanImageBufferB.length];
                }
                
                System.arraycopy(cleanImageBufferB, 0, pixBufferB, 0, cleanImageBufferB.length);
            }
        } else if (buffer == ViewJComponentBase.BOTH) {

            if ( (pixBuffer == null) || (pixBuffer.length != cleanImageBufferA.length)) {
                pixBuffer = new int[cleanImageBufferA.length];
            }

            if ( (pixBufferB == null) || (pixBufferB.length != cleanImageBufferB.length)) {
                pixBufferB = new int[cleanImageBufferB.length];
            }

            for (int i = 0; i < cleanImageBufferA.length; i++) {
                pixBuffer[i] = cleanImageBufferA[i];
                pixBufferB[i] = cleanImageBufferB[i];
            }
        }
    }

    /**
     * Draws text onto the image, such as the slice number.
     * 
     * @param offscreenGraphics2d Graphics2D graphics context to draw in
     */
    private void drawImageText(final Graphics2D offscreenGraphics2d) {

        if ( ( ((int) ( (zoomX * imageDim.width) + 0.5) - 40) > 0) && (sliceString != null)
                && (showSliceNumber == true)) {
            offscreenGraphics2d.setFont(MipavUtil.font12);
            offscreenGraphics2d.setColor(Color.black);
            offscreenGraphics2d.drawString(sliceString, 5, (int) ( (zoomY * resolutionY * imageDim.height) + 0.5f) - 5);
            offscreenGraphics2d.drawString(sliceString, 5, (int) ( (zoomY * resolutionY * imageDim.height) + 0.5f) - 6);
            offscreenGraphics2d.drawString(sliceString, 5, (int) ( (zoomY * resolutionY * imageDim.height) + 0.5f) - 4);
            offscreenGraphics2d.drawString(sliceString, 6, (int) ( (zoomY * resolutionY * imageDim.height) + 0.5f) - 5);
            offscreenGraphics2d.drawString(sliceString, 4, (int) ( (zoomY * resolutionY * imageDim.height) + 0.5f) - 5);
            offscreenGraphics2d.setColor(Color.white);
            offscreenGraphics2d.drawString(sliceString, 5, (int) ( (zoomY * resolutionY * imageDim.height) + 0.5f) - 5);
        }
    }

    /**
     * Returns the magnitude zoom depending on the zoom mode.
     * 
     * @param zoom the zoom factor (x or y)
     * @param reverse for reverse zoom
     * 
     * @return the calculated zoom factor
     */
    private float getZoomMagnitude(final float zoom, final boolean reverse) {

        if (Preferences.is(Preferences.PREF_ZOOM_LINEAR)) {

            if (cursorMode == ViewJComponentBase.ZOOMING_IN) {

                if (reverse) {

                    if (zoom <= 1.0f) {
                        return zoom * 0.5f; // to prevent zooming to 0
                    }

                    return zoom - 1.0f;
                } else {
                    if (zoom <= 1.0f) {
                        return zoom * 2.0f;
                    } else {
                        return zoom + 1.0f;
                    }
                }
            } else { // mode == ZOOMING_OUT

                if (reverse) {
                    if (zoom <= 1.0f) {
                        return zoom * 2.0f;
                    } else {
                        return zoom + 1.0f;
                    }
                } else {

                    if (zoom <= 1.0f) {
                        return zoom * 0.5f; // to prevent zooming to 0
                    }

                    return zoom - 1.0f;
                }
            }
        } else { // zoomMode == EXPONENTIAL

            if (cursorMode == ViewJComponentBase.ZOOMING_IN) {

                if (reverse) {
                    return zoom * 0.5f;
                } else {
                    return zoom * 2.0f;
                }
            } else { // mode == ZOOMING_OUT

                if (reverse) {
                    return zoom * 2.0f;
                } else {
                    return zoom * 0.5f;
                }
            }
        }
    }


    // When the apply or close button is pressed, JDialogCheckerBoard sets the following 2 parameters used.
    /**
     * Sets the number of checkers in a row and a column.
     */
    private void makeCheckerboard() {

    	cleanBuffer(ViewJComponentBase.IMAGE_B);
    	
        int xDim, yDim;
        int xIndex, yIndex;
        int x, y;
        
        boolean doA;

        if ( (imageB == null) || (pixBufferB == null)) {
            return;
        }

        final ViewJFrameBase vjfb = (this).getFrame();
        final ViewControlsImage vci = vjfb.getControls();

        if (vci != null) {
            vci.setAlphaSliderEnabled(false);
        }

        xDim = maxExtents[0];
        yDim = maxExtents[1];

        xSep = xDim / nColumnCheckers;
        xMod = xDim % nColumnCheckers;
        ySep = yDim / nRowCheckers;
        yMod = yDim % nRowCheckers;
        xStart = new int[nColumnCheckers + 1];
        yStart = new int[nRowCheckers + 1];
        xStart[0] = 0;

        for (x = 1; x <= nColumnCheckers; x++) {
            xStart[x] = xStart[x - 1] + xSep;

            if (x <= xMod) {
                xStart[x]++;
            }
        } // for (x = 1; x < columnCheckers; x++)

        yStart[0] = 0;

        for (y = 1; y <= nRowCheckers; y++) {
            yStart[y] = yStart[y - 1] + ySep;

            if (y <= yMod) {
                yStart[y]++;
            }
        } // for (y = 1; y < rowCheckers; y++)

        // Do checkered rows starting with doA = true;
        doA = true;
        xIndex = 1;
        yIndex = 1;

        for (y = 0; y < yDim;) {

            for (x = 0; x < xDim;) {

                if (doA) {
                    pixBufferB[x + (y * xDim)] = 0;
                }

                x++;

                if (x == xDim) {
                    doA = true;
                    xIndex = 1;
                } else if (x == xStart[xIndex]) {
                    doA = !doA;
                    xIndex++;
                }
            } // for (x = 0; x < xDim;)

            y++;

            if (y == yDim) {} else if (y == yStart[yIndex]) {
                yIndex++;
                y = yStart[yIndex];
                yIndex++;
            }
        } // for (y = 0; y < yDim;)

        // Do checkered rows starting with doA = false;
        doA = false;
        xIndex = 1;
        yIndex = 2;

        for (y = yStart[1]; y < yDim;) {

            for (x = 0; x < xDim;) {

                if (doA) {
                    pixBufferB[x + (y * xDim)] = 0;
                }

                x++;

                if (x == xDim) {
                    doA = false;
                    xIndex = 1;
                } else if (x == xStart[xIndex]) {
                    doA = !doA;
                    xIndex++;
                }
            } // for (x = 0; x < xDim;)

            y++;

            if (y == yDim) {} else if (y == yStart[yIndex]) {
                yIndex++;
                y = yStart[yIndex];
                yIndex++;
            }
        } // for (y = yStart[1]; y < yDim;)
        
        
        
        if(nColumnCheckers == 1 || nRowCheckers == 1) {
        	if(nColumnCheckers == 1) {
        		bandSpacing = new int[nRowCheckers];
        		for(int i=0;i<bandSpacing.length;i++) {
        			bandSpacing[i] = ySep;
        		}
        		
        		for(int i=0;i<yMod;i++) {
    				bandSpacing[i] = bandSpacing[i] + 1;
    			}
        		
        		
        	}else {
        		bandSpacing = new int[nColumnCheckers];
        		
        		for(int i=0;i<bandSpacing.length;i++) {
        			bandSpacing[i] = xSep;
        		}
        		
        		for(int i=0;i<xMod;i++) {
    				bandSpacing[i] = bandSpacing[i] + 1;
    			}
        	}
        	

    		if(checkerDialog != null) {
    				checkerDialog.setBandSpacingCounter(0);
    		}
    				
    				

    		
        }
        
		
		
		
    }


    public void loopBandSpacing() {
		int temp = bandSpacing[bandSpacing.length-1];
		for(int i=bandSpacing.length-1;i>=0;i--) {
			if(i==0) {
				bandSpacing[0] = temp;
			}else {
				bandSpacing[i] = bandSpacing[i-1];
			}
		}

	}
    
    
    
    public void loopBandSpacingReverse() {
		int temp = bandSpacing[0];
		for(int i=0;i<bandSpacing.length;i++) {
			if(i==bandSpacing.length-1) {
				bandSpacing[bandSpacing.length-1] = temp;
			}else {
				bandSpacing[i] = bandSpacing[i+1];
			}
		}

	}
    
    
    

    public synchronized int[] getBandSpacing() {
		return bandSpacing;
	}

	public synchronized void setBandSpacing(int[] bandSpacing) {
		this.bandSpacing = bandSpacing;
	}

	public synchronized int[] getxStart() {
		return xStart;
	}

	public synchronized int[] getyStart() {
		return yStart;
	}

	public synchronized int getyMod() {
		return yMod;
	}
	
	public synchronized int getxMod() {
		return xMod;
	}

	/**
     * Repaints the image intensity label.
     * 
     * @param graphics2d Graphics2D the graphics context to draw in
     */
    private void repaintImageIntensityLabelFast(final Graphics2D graphics2d) {

        if ( (graphics2d == null) || (lastMouseX == OUT_OF_BOUNDS) || (lastMouseY == OUT_OF_BOUNDS)) {
            return;
        }

        final int xS = getScaledX(lastMouseX); // zoomed x. Used as cursor
        final int yS = getScaledY(lastMouseY); // zoomed y. Used as cursor

        // position where label should go
        final int x = (MipavMath.round(xS * zoomX) + 15);
        final int y = (MipavMath.round(yS * zoomY) + 35);

        // positions needed to determine when label should be flipped over when getting too close to edge
        final int wC = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().width;
        final int hC = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().height;
        final int xC = MipavMath.round(xS * zoomX);
        final int yC = MipavMath.round(yS * zoomY);

        Color textColor = null;
        Color backgroundColor = null;

        // set color of label text
        if (Preferences.getProperty(Preferences.PREF_INTENSITY_LABEL_COLOR) != null) {
            final String prefColor = Preferences.getProperty(Preferences.PREF_INTENSITY_LABEL_COLOR);
            textColor = MipavUtil.extractColor(prefColor);
        } else {
            textColor = Color.yellow;
        }

        if (Preferences.getProperty(Preferences.PREF_INTENSITY_LABEL_BACKGROUND_COLOR) != null) {
            final String prefColor = Preferences.getProperty(Preferences.PREF_INTENSITY_LABEL_BACKGROUND_COLOR);
            backgroundColor = MipavUtil.extractColor(prefColor);
        } else {
            backgroundColor = Color.black;
        }

        // we will only display up to 3 decimal places
        final DecimalFormat df = new DecimalFormat("0.0##");

        graphics2d.setColor(backgroundColor);

        if (imageActive.isColorImage()) {
            final String red = df.format(new Float(
                    imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 1]).doubleValue());
            final String green = df.format(new Float(
                    imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 2]).doubleValue());
            final String blue = df.format(new Float(
                    imageBufferActive[ (4 * ( (yS * imageActive.getExtents()[0]) + xS)) + 3]).doubleValue());

            if ( ( (wC - xC) > 170) && ( (hC - yC) > 40)) {
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x + 1, y);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 1, y);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x, y - 1);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x, y + 1);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x, y);
            } else if ( ( (wC - xC) <= 170) && ( (hC - yC) > 40)) {
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 159, y);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 161, y);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 160, y - 1);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 160, y + 1);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 160, y);
            } else if ( ( (wC - xC) <= 170) && ( (hC - yC) <= 40)) {
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 159, y - 40);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 161, y - 40);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 160, y - 41);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 160, y - 39);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 160, y - 40);
            } else if ( ( (wC - xC) > 170) && ( (hC - yC) <= 40)) {
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x + 1, y - 40);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x - 1, y - 40);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x, y - 41);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x, y - 39);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + red + "," + green + ","
                        + blue, x, y - 40);
            }
        } else {
            final String intensity;
            if(imageActive.isComplexImage() && Preferences.getComplexDisplay() != ComplexDisplay.MAGNITUDE) {
                int loc = ((yS * imageActive.getExtents()[0]) + xS)*2;
                intensity = df.format(new Double(Math.sqrt(imageBufferActive[loc]*imageBufferActive[loc] + 
                                                            imageBufferActive[loc+1]*imageBufferActive[loc+1])).doubleValue());
            } else {
                intensity = df.format(new Float(imageBufferActive[ (yS * imageActive.getExtents()[0]) + xS])
                .doubleValue());
            }
            
            if ( ( (wC - xC) > 100) && ( (hC - yC) > 50)) {
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x + 1, y);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 1, y);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x, y - 1);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x, y + 1);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x, y);
            } else if ( ( (wC - xC) <= 100) && ( (hC - yC) > 50)) {
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 79, y);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 81, y);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 80, y - 1);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 80, y + 1);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 80, y);
            } else if ( ( (wC - xC) <= 100) && ( (hC - yC) <= 50)) {
                graphics2d
                .drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 79, y - 40);
                graphics2d
                .drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 81, y - 40);
                graphics2d
                .drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 80, y - 41);
                graphics2d
                .drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 80, y - 39);

                graphics2d.setColor(textColor);
                graphics2d
                .drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x - 80, y - 40);
            } else if ( ( (wC - xC) > 100) && ( (hC - yC) <= 50)) {
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x, y - 40);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x, y - 40);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x, y - 41);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x, y - 39);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS) + "," + String.valueOf(yS) + ":  " + intensity, x, y - 40);
            }
        }
    }

    /**
     * Repaints the paint brush cursor without repainting the entire image.
     * 
     * @param graphics2d Graphics2D the graphics context to draw in
     */
    private void repaintPaintBrushCursorFast(final Graphics2D graphics2d) {

        if ( (graphics2d == null) || (lastMouseX == OUT_OF_BOUNDS) || (lastMouseY == OUT_OF_BOUNDS)) {
            return;
        }

        final int xS = lastMouseX;
        final int yS = lastMouseY;

        // yx, xz
        float factor = 1f;
        float factor2 = 1f;

        factor = resolutionX / resolutionY;

        if (factor < 1) {
            factor2 = 1f / factor;
            factor = 1f;
        }

        graphics2d.drawImage(paintImage.getScaledInstance(MipavMath.round(paintImage.getWidth() * zoomX * factor),
                MipavMath.round(paintImage.getHeight() * zoomY * factor2), 0), new AffineTransform(1.0f / factor, 0f,
                        0f, 1.0f / factor2, xS, yS), null);

    }

    /**
     * DOCUMENT ME!
     * 
     * @param LUT DOCUMENT ME!
     * @param image DOCUMENT ME!
     */
    public void resetLUT(final ModelLUT LUT, final ModelImage image) {
        float min, max;
        final float[] x = new float[4];
        final float[] y = new float[4];       
        final Dimension dim = new Dimension(256, 256);

        // Set LUT min max values;
        if (image.getType() == ModelStorageBase.UBYTE) {
            min = 0;
            max = 255;
        } else if (image.getType() == ModelStorageBase.BYTE) {
            min = -128;
            max = 127;
        } else {
            min = (float) image.getMin();
            max = (float) image.getMax();
        }

        x[0] = min;
        y[0] = dim.height - 1;

        x[1] = (min + ( (max - min) / 3.0f));
        y[1] = (dim.height - 1) - ( (dim.height - 1) / 3.0f);

        x[2] = (min + ( (max - min) * 0.67f));
        y[2] = (dim.height - 1) - ( (dim.height - 1) * 0.67f);

        x[3] = max;
        y[3] = 0;
        LUT.getTransferFunction().importArrays(x, y, 4);

    }

    /**
     * DOCUMENT ME!
     * 
     * @param RGBT DOCUMENT ME!
     */
    private void resetRGB(ModelRGB RGBT) {
        final float[] x = new float[4];
        final float[] y = new float[4];
        final Dimension dim = new Dimension(256, 256);

        // Set LUT min max values;
        x[0] = 0;
        y[0] = dim.height - 1;

        x[1] = 255 * 0.333f;
        y[1] = (dim.height - 1) - ( (dim.height - 1) / 3.0f);

        x[2] = 255 * 0.667f;
        y[2] = (dim.height - 1) - ( (dim.height - 1) * 0.67f);

        x[3] = 255;
        y[3] = 0;

        if (RGBT == null) {
            final int[] RGBExtents = new int[2];
            RGBExtents[0] = 4;
            RGBExtents[1] = 256;
            RGBT = new ModelRGB(RGBExtents);
        }

        RGBT.getRedFunction().importArrays(x, y, 4);
        RGBT.getGreenFunction().importArrays(x, y, 4);
        RGBT.getBlueFunction().importArrays(x, y, 4);
        RGBT.makeRGB( -1);
    }

    /**
     * 
     * @param zeroToOneLUTAdj
     */
    public void setZeroToOneLUTAdj(final boolean zeroToOneLUTAdj) {
        this.zeroToOneLUTAdj = zeroToOneLUTAdj;
    }

    /**
     * 
     * @return
     */
    public boolean isZeroToOneLUTAdj() {
        return zeroToOneLUTAdj;
    }



    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener#fileToScreen(WildMagic.LibFoundation.Mathematics.Vector3f)
     */
    public Vector3f fileToScreen(Vector3f kFile) {
        Vector3f patientPt = new Vector3f(kFile);
        MipavCoordinateSystems.fileToPatient( kFile, patientPt, imageA, orientation );
        Vector3f screenPt = new Vector3f();
        super.LocalToScreen( patientPt, screenPt );
        return screenPt;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener#patientToScreen(WildMagic.LibFoundation.Mathematics.Vector3f)
     */
    public Vector3f patientToScreen(Vector3f kPt) {
        Vector3f screenPt = new Vector3f();
        super.LocalToScreen( kPt, screenPt );
        return screenPt;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener#screenToFile(int, int, int, WildMagic.LibFoundation.Mathematics.Vector3f)
     */
    public boolean screenToFile(int iX, int iY, int iZ, Vector3f kVolumePt) {
        boolean bClipped = false;
        if ( (iX < 0 ) || (iX > getWidth()) || (iY < 0 ) || (iY > getHeight()) )
        {
            bClipped = true;
        }
        Vector3f screenPt = new Vector3f(iX, iY, iZ);
        Vector3f patientPt = new Vector3f();
        super.ScreenToLocal( screenPt, patientPt );
        final int[] extents = getActiveImage().getExtents(orientation);
        if ( (patientPt.X < 0) || (patientPt.X > extents[0]-1) ||
                (patientPt.Y < 0) || (patientPt.Y > extents[1]-1) )
        {
            bClipped = true;
        }

        MipavCoordinateSystems.patientToFile( patientPt, kVolumePt, imageA, orientation );
        return bClipped;
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener#screenToFile(WildMagic.LibFoundation.Mathematics.Vector3f, WildMagic.LibFoundation.Mathematics.Vector3f)
     */
    public boolean screenToFile( Vector3f kScreen, Vector3f kFile ) {
        boolean bClipped = false;
        if ( (kScreen.X < 0 ) || (kScreen.X > getWidth()) || (kScreen.Y < 0 ) || (kScreen.Y > getHeight()) )
        {
            bClipped = true;
        }
        Vector3f patientPt = new Vector3f();
        super.ScreenToLocal( kScreen, patientPt );
        final int[] extents = getActiveImage().getExtents(orientation);
        if ( (patientPt.X < 0) || (patientPt.X > extents[0]-1) ||
                (patientPt.Y < 0) || (patientPt.Y > extents[1]-1) )
        {
            bClipped = true;
        }
        MipavCoordinateSystems.patientToFile( patientPt, kFile, imageA, orientation );
        return bClipped;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener#screenToFile(WildMagic.LibFoundation.Mathematics.Vector3f, WildMagic.LibFoundation.Mathematics.Vector3f)
     */
    public Vector3f screenToFile( Vector3f kScreen ) {
        Vector3f patientPt = new Vector3f();
        super.ScreenToLocal( kScreen, patientPt );
        Vector3f kFile = new Vector3f();
        MipavCoordinateSystems.patientToFile( patientPt, kFile, imageA, orientation );
        return kFile;
    }


    /**
     * Increments the current slice in local coordinates and returns 
     * the new position in the view-independent file coordinates.
     * @return
     */
    public Vector3f upSlice()
    {
        if ( slice + 1 < imageExtents[2] )
        {
            setSlice(slice+1);
            return m_kPatientSlice.getCenter();
        }
        return null;
    }

    /**
     * Decrements the current slice in local coordinates and returns 
     * the new position in the view-independent file coordinates.
     * @return
     */
    public Vector3f downSlice()
    {
        if ( slice - 1 >= 0 )
        {
            setSlice(slice-1);
            return m_kPatientSlice.getCenter();
        }
        return null;
    } 




    /**
     * get the color of the grid.
     * 
     * @return Color grid color
     */

    public Color getGridColor() {
        return this.gridColor;
    }

    /**
     * Whether or not labels should be drawn on the grid overlay.
     * 
     * @return boolean
     */
    public boolean getGridLabeling() {
        return gridLabelingOn;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean getGridLabelOrientation() {
        return gridLabelOrientation;
    }

    /**
     * returns whether grid overlay is being displayed.
     * 
     * @return boolean is grid overlay on?
     */
    public boolean getGridOverlay() {
        return gridOverlayOn;
    }

    /**
     * returns the grid spacing in terms of resolution.
     * 
     * @return float grid spacing
     */
    public float getGridSpacingX() {
        return gridSpacingX;
    }

    /**
     * returns the grid spacing in terms of resolution.
     * 
     * @return float grid spacing
     */
    public float getGridSpacingY() {
        return gridSpacingY;
    }

    /**
     * set the color of the grid.
     * 
     * @param color
     *            Color
     */
    public void setGridColor(Color color) {
        this.gridColor = color;
    }

    /**
     * Tells the grid overlay (if on) to show abc/123 labeling.
     * 
     * @param doLabel
     *            boolean
     */
    public void setGridLabelingOn(boolean doLabel) {
        this.gridLabelingOn = doLabel;
    }

    /**
     * Sets the axis orientation of abc and 123 labeling of the grid overlay.
     * 
     * @param or
     *            boolean true = x-axis numbered, false = x-axis lettered
     */
    public void setGridLabelOrientation(boolean or) {
        this.gridLabelOrientation = or;
    }

    /**
     * Sets whether or not to show the NEI grid overlay.
     * 
     * @param flag
     *            boolean show grid overlay (or not!)
     */
    public void setGridOverlay(boolean flag) {
        gridOverlayOn = flag;
    }

    /**
     * sets the grid spacing (horizontal) in terms of resolution.
     * 
     * @param spacing
     *            float new grid spacing
     */
    public void setGridSpacingX(float spacing) {
        this.gridSpacingX = spacing;
    }

    /**
     * sets the grid spacing (horizontal) in terms of resolution.
     * 
     * @param spacing
     *            float new grid spacing
     */
    public void setGridSpacingY(float spacing) {
        this.gridSpacingY = spacing;
    }


    public VOIManager getVOIManager( )
    {
        return voiManager;
    }

    public void setVOIManager( VOIManager kManager )
    {
        voiManager = kManager;
    }

    /**
     * Returns if image/dicom overlay should be shown.
     * 
     * @return boolean is the overlay shown
     */
    public boolean getOverlayOn() {
        return this.overlayOn;
    }    


    /**
     * Sets whether or not to show the overlay.
     * 
     * @param flag
     *            boolean that tells whether or not to show the overlay
     */
    public void setOverlay(boolean flag) {
        overlayOn = flag;
    }
    /**
     * Draws a grid on top of the image according to the gridSpacingX and
     * gridSpacingY.
     * 
     * @param g
     *            Graphics the graphics used to draw
     */
    protected void showGridOverlay(Graphics g) {
        g.setColor(gridColor);

        Insets insets = getFrame().getInsets();
        int rightOffset = getBounds().width - insets.left;
        int bottomOffset = getBounds().height - insets.bottom;
        float offset = 0;

        int xDim = getActiveImage().getExtents()[0];
        int yDim = getActiveImage().getExtents()[0];

        float resX = getActiveImage().getResolutions(0)[0];
        float resY = getActiveImage().getResolutions(0)[1];

        float numVertical = (xDim * resX) / gridSpacingX;
        float numHorizontal = (yDim * resY) / gridSpacingY;

        float verticalSpacing = (xDim / numVertical) * getZoomX();
        float horizontalSpacing = (yDim / numHorizontal) * getZoomY();

        for (int i = 0; i < numVertical; i++, offset += verticalSpacing) {
            g.drawLine(MipavMath.round(offset), 0, MipavMath.round(offset),
                    bottomOffset);
        }

        offset = 0;

        for (int i = 0; i < numHorizontal; i++, offset += horizontalSpacing) {
            g.drawLine(0, MipavMath.round(offset), rightOffset, MipavMath
                    .round(offset));
        }

        if (gridLabelingOn) {
            int i, j;
            float xOffset = 0;
            float yOffset = 0;
            String gridLabel = "";

            float xPadding = 2 + (2 * getZoomX());
            float yPadding = 15 + (2 * getZoomY());

            char alphaLabel;

            for (i = 0; i < numHorizontal; i++, xOffset += horizontalSpacing) {

                for (j = 0, yOffset = 0; j < numVertical; j++, yOffset += verticalSpacing) {

                    if ((i == 0) || (j == 0)) {

                        if (gridLabelOrientation) {

                            // x-axis is 1, 2, 3, 4... y-axis is a, b, c, d
                            alphaLabel = (char) (97 + j);

                            gridLabel = Integer.toString((i + 1)) + alphaLabel;

                        } else {
                            alphaLabel = (char) (97 + i);
                            gridLabel = alphaLabel + Integer.toString((j + 1));
                        }

                        g.drawString(gridLabel, MipavMath.round(yOffset
                                + xPadding), MipavMath
                                .round(xOffset + yPadding));

                    }
                }

            }
        }

    }

    /**
     * Displays Image overlays (DICOM or image attributes).
     * 
     * @param g
     *            Graphics object used to paint and display the strings.
     */
    public void showOverlay(Graphics g) {
        String[] overlays = new String[16];
        String[] overlayNames = new String[16];

        if (getActiveImage().getFileInfo(0) instanceof FileInfoDicom) {
            FileInfoDicom fileInfo;

            if (getSlice() >= 0) {
                fileInfo = (FileInfoDicom) (getActiveImage()
                        .getFileInfo())[getSlice()];
            } else {
                fileInfo = (FileInfoDicom) (getActiveImage()
                        .getFileInfo())[0];
            }

            String[] dicomKeys = Preferences.getOverlays(true);
            overlayNames = Preferences.getOverlayNames(true);

            for (int i = 0; i < 16; i++) {

                if ((dicomKeys[i] != null) && !dicomKeys[i].equals("-")) {
                    overlays[i] = buildOverlayStrings(fileInfo,
                            overlayNames[i], dicomKeys[i]);

                    if (overlays[i] != null) {
                        // System.err.println(i + ": OVERLAY NAME DICOM: " +
                        // overlayNames[i] + ", key: " +
                        // dicomKeys[i] + ", overlay: " + overlays[i]);
                    }
                }
            }

            Insets insets = getFrame().getInsets();
            int rightOffset = getBounds().width - insets.left;
            int bottomOffset = getBounds().height - insets.bottom
            - 15;

            int len;

            for (int i = 0; i < 16; i++) {

                if (overlays[i] != null) {
                    len = g.getFontMetrics(g.getFont())
                    .stringWidth(overlays[i]);

                    if (i < 4) {
                        drawStringBW(overlays[i], g, 5,
                                (15 * (i + 1)));
                    } else if ((i > 3) && (i < 8)) {
                        drawStringBW(overlays[i], g, rightOffset
                                - len, (15 * ((i % 4) + 1)));
                    } else if ((i > 7) && (i < 12)) {
                        drawStringBW(overlays[i], g, 5, bottomOffset
                                - 45 + (i % 4 * 15));
                    } else if (i > 11) {
                        drawStringBW(overlays[i], g, rightOffset
                                - len, bottomOffset - 45 + (i % 4 * 15));
                    }
                    // System.err.println(i +
                    // ": should have drawn a string somewhere: " +
                    // overlays[i]);
                }
            }

            drawGradicules(g, fileInfo.getResolutions()[0], fileInfo
                    .getResolutions()[1]);

            /*
             * float reconDiameter;
             * 
             * try { reconDiameter = Float.valueOf((String)
             * (fileInfo.getTag("0018,1100").getValue(true))).floatValue(); }
             * catch (Exception ex) { reconDiameter =
             * compImage.getActiveImage().getExtents()[0] *
             * fileInfo.getResolutions()[0]; }
             * 
             * String[] values =
             * compImage.setOverlayValues(compImage.getActiveImage
             * ().getImageOrientation(), MipavMath.round(fileInfo.XLocation),
             * MipavMath.round(fileInfo.yLocation),
             * MipavMath.round(fileInfo.zLocation),
             * MipavMath.round(reconDiameter));
             * 
             * int index = values[0].length() / 2;
             * 
             * for (int i = 0; i < values[0].length(); i++) {
             * compImage.drawStringBW(String.valueOf(values[0].charAt(i)), g, 5,
             * (getHeight() / 2) - ((index - i) * 15)); }
             * 
             * index = values[1].length() / 2;
             * 
             * for (int i = 0; i < values[1].length(); i++) {
             * compImage.drawStringBW(String.valueOf(values[1].charAt(i)), g,
             * rightOffset - 10, (getHeight() / 2) - ((index - i) * 15)); }
             * 
             * len = g.getFontMetrics(g.getFont()).stringWidth(values[2]);
             * compImage.drawStringBW(values[2], g, (getWidth() / 2) - (len /
             * 2), 15); len =
             * g.getFontMetrics(g.getFont()).stringWidth(values[3]);
             * compImage.drawStringBW(values[3], g, (getWidth() / 2) - (len /
             * 2), bottomOffset); compImage.drawStringBW(values[4], g, 5, 75);
             */
        } else {
            FileInfoBase fileInfo;

            if (getSlice() >= 0) {
                fileInfo = getActiveImage().getFileInfo()[Math.min(getSlice(), getActiveImage().getFileInfo().length-1)];
            } else {
                fileInfo = getActiveImage().getFileInfo()[0];
            }

            String[] attribs = Preferences.getOverlays(false);

            overlayNames = Preferences.getOverlayNames(false);

            for (int i = 0; i < 16; i++) {

                if ((attribs[i] != null) && !attribs[i].equals("-")) {
                    overlays[i] = buildOverlayStrings(fileInfo,
                            overlayNames[i], attribs[i]);
                }
            }

            Insets insets = getFrame().getInsets();
            int rightOffset = getBounds().width - insets.left;
            int bottomOffset = getBounds().height - insets.bottom
            - 15;

            int len;

            for (int i = 0; i < 16; i++) {

                if (overlays[i] != null) {
                    len = g.getFontMetrics(g.getFont())
                    .stringWidth(overlays[i]);

                    if (i < 4) {
                        drawStringBW(overlays[i], g, 5,
                                (15 * (i + 1)));
                    } else if ((i > 3) && (i < 8)) {
                        drawStringBW(overlays[i], g, rightOffset
                                - len, (15 * ((i % 4) + 1)));
                    } else if ((i > 7) && (i < 12)) {
                        drawStringBW(overlays[i], g, 5, bottomOffset
                                - 45 + (i % 4 * 15));
                    } else if (i > 11) {
                        drawStringBW(overlays[i], g, rightOffset
                                - len, bottomOffset - 45 + (i % 4 * 15));
                    }
                }
            }
        }
    }


    /**
     * Builds the overlay Strings from the tag's value. Concatenates the output
     * strings from the tags and ensures that any properly read-in string has
     * usable (if empty) values.
     * 
     * @param inf
     *            The FileInfo with DICOM tags to display.
     * @param name
     *            DOCUMENT ME!
     * @param attribString
     *            Key of tag to display.
     * 
     * @return null when value is not a String or when the tag does not exist.
     */
    private String buildOverlayStrings(FileInfoBase inf, String name,
            String attribString) {

        if (inf instanceof FileInfoDicom) {

            try {

                if ((attribString == null) || (attribString == "")) {
                    return null;
                }

                Object val = ((FileInfoDicom) inf).getTagTable().getValue(
                        attribString);

                if (val == null) {
                    return null;
                } else if ((name != null) && !(name.trim().equals(""))) {
                    return (name + " - " + val);
                } else {
                    return val.toString();
                }
            } catch (IllegalArgumentException ex) {
                Preferences.debug("Illegal arg on: " + attribString);

                return null;
            } catch (ClassCastException notStr) {
                Preferences.debug("Creating strings for DICOM overlay for "
                        + attribString
                        + " but encountered a ClassCastException.\n", 4);

                return null;
            } catch (NullPointerException noTag) {
                Preferences.debug("Creating strings for DICOM overlay for "
                        + attribString
                        + " but encountered a NullPointerException.\n", 4);

                return null;
            }
        } else { // FileInfo is NOT DICOM

            if (attribString == null) {
                return null;
            }

            String resultStr = new String();

            if ((name != null) && (name != "")) {
                resultStr = name + " - ";
            }

            String[] atts = JDialogOverlay.attribStr;

            if (attribString.equals(atts[0])) {
                return resultStr + Integer.toString(inf.getExtents()[0]);
            } else if (attribString.equals(atts[1])) {
                return resultStr + Integer.toString(inf.getExtents()[1]);
            } else if (attribString.equals(atts[2])) {

                if (inf.getExtents().length > 2) {
                    return resultStr + Integer.toString(inf.getExtents()[2]);
                }
            } else if (attribString.equals(atts[3])) {

                if (inf.getExtents().length > 3) {
                    return resultStr + Integer.toString(inf.getExtents()[3]);
                }
            } else if (attribString.equals(atts[4])) {
                return resultStr + getActiveImage().getTypeString();
            } else if (attribString.equals(atts[5])) {
                return resultStr
                + Double.toString(getActiveImage().getMin());
            } else if (attribString.equals(atts[6])) {
                return resultStr
                + Double.toString(getActiveImage().getMax());
            } else if (attribString.equals(atts[7])) {
                return resultStr
                + FileInfoBase.getImageOrientationStr(inf
                        .getImageOrientation());
            } else if (attribString.equals(atts[8])) {
                return resultStr
                + FileInfoBase.getAxisOrientationStr(inf
                        .getAxisOrientation(0));
            } else if (attribString.equals(atts[9])) {
                return resultStr
                + FileInfoBase.getAxisOrientationStr(inf
                        .getAxisOrientation(1));
            } else if (attribString.equals(atts[10])) {

                if (inf.getExtents().length > 2) {
                    return resultStr
                    + FileInfoBase.getAxisOrientationStr(inf
                            .getAxisOrientation(2));
                }
            } else if (attribString.equals(atts[11])) {
                return new String(resultStr
                        + inf.getResolutions()[0]
                                               + " "
                                               + (Unit.getUnitFromLegacyNum(inf
                                                       .getUnitsOfMeasure()[0])).getAbbrev());
            } else if (attribString.equals(atts[12])) {
                return new String(resultStr
                        + inf.getResolutions()[1]
                                               + " "
                                               + (Unit.getUnitFromLegacyNum(inf
                                                       .getUnitsOfMeasure()[1])).getAbbrev());
            } else if (attribString.equals(atts[13])) {

                if (inf.getExtents().length > 2) {
                    return new String(resultStr
                            + inf.getResolutions()[2]
                                                   + " "
                                                   + (Unit.getUnitFromLegacyNum(inf
                                                           .getUnitsOfMeasure()[2])).getAbbrev());
                }
            } else if (attribString.equals(atts[14])) {

                if (inf.getExtents().length > 3) {
                    return new String(resultStr
                            + inf.getResolutions()[3]
                                                   + " "
                                                   + (Unit.getUnitFromLegacyNum(inf
                                                           .getUnitsOfMeasure()[3])).getAbbrev());
                }
            } else if (attribString.equals(atts[15])) {
                return resultStr + Float.toString(inf.getSliceThickness());
            } else if (attribString.equals(atts[16])) {
                return resultStr + Float.toString(inf.getOrigin()[0]);
            } else if (attribString.equals(atts[17])) {
                return resultStr + Float.toString(inf.getOrigin()[1]);
            } else if (attribString.equals(atts[18])) {

                if (inf.getExtents().length > 2) {
                    return resultStr + Float.toString(inf.getOrigin()[2]);
                }
            } else if (attribString.equals(atts[19])) {

                if (inf.getExtents().length > 3) {
                    return resultStr + Float.toString(inf.getOrigin()[3]);
                }
            } else if (attribString.equals(atts[20])) {

                if (inf.getEndianess()) {
                    return resultStr + "big endian";
                } else {
                    return resultStr + "little endian";
                }
            }

            return null;

        }
    }

    @Override
    public Vector3f fileToScreenVOI(Vector3f kFile) {
        // axisFlip represents whether to invert the axes after they are reordered
        final boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(getActiveImage(), orientation);
        Vector3f patientPt = new Vector3f(kFile);
        MipavCoordinateSystems.fileToPatient( kFile, patientPt, imageA, orientation );
        if ( axisFlip[0] ) patientPt.X += 1;
        if ( axisFlip[1] ) patientPt.Y += 1;
        //if ( axisFlip[2] ) patientPt.Z += 1;
        Vector3f screenPt = new Vector3f();
        super.LocalToScreen( patientPt, screenPt );
        return screenPt;
    }

    @Override
    public Vector3f patientToScreenVOI(Vector3f kPt) {
        return patientToScreen(kPt);
    }

    @Override
    public boolean screenToFileVOI(int iX, int iY, int iZ, Vector3f kVolumePt) {
        boolean bClipped = false;
        if ( (iX < 0 ) || (iX > getWidth()) || (iY < 0 ) || (iY > getHeight()) )
        {
            bClipped = true;
        }
        Vector3f screenPt = new Vector3f(iX, iY, iZ);
        Vector3f patientPt = new Vector3f();
        super.ScreenToLocal( screenPt, patientPt );

        // axisFlip represents whether to invert the axes after they are reordered
        final boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(getActiveImage(), orientation);
        final int[] extents = getActiveImage().getExtents(orientation);
        if ( axisFlip[0] ) patientPt.X -= 1;
        if ( axisFlip[1] ) patientPt.Y -= 1;
        //if ( axisFlip[2] ) patientPt.Z -= 1;
        
        if ( (patientPt.X < 0) || (patientPt.X > extents[0]) ||
                (patientPt.Y < 0) || (patientPt.Y > extents[1]) )
        {
            bClipped = true;
        }
        
        //System.err.println( patientPt + " " + kVolumePt + " " + bClipped + " " + imageExtents[0] + " " + imageExtents[1] + " " + imageExtents[2]);

        MipavCoordinateSystems.patientToFile( patientPt, kVolumePt, imageA, orientation );
        return bClipped;
    }

    @Override
    public Vector3f screenToFileVOI(Vector3f kScreen) {
        Vector3f patientPt = new Vector3f();
        super.ScreenToLocal( kScreen, patientPt );
        // axisFlip represents whether to invert the axes after they are reordered
        //final boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(getActiveImage(), orientation);
        //if ( axisFlip[0] ) patientPt.X -= 1;
        //if ( axisFlip[1] ) patientPt.Y -= 1;
        //if ( axisFlip[2] ) patientPt.Z -= 1;
        Vector3f kFile = new Vector3f();
        MipavCoordinateSystems.patientToFile( patientPt, kFile, imageA, orientation );
        return kFile;
    }

    @Override
    public boolean screenToFileVOI(Vector3f kScreen, Vector3f kFile) {
        boolean bClipped = false;
        if ( (kScreen.X < 0 ) || (kScreen.X > getWidth()) || (kScreen.Y < 0 ) || (kScreen.Y > getHeight()) )
        {
            bClipped = true;
        }
        Vector3f patientPt = new Vector3f();
        super.ScreenToLocal( kScreen, patientPt );
        // axisFlip represents whether to invert the axes after they are reordered
        final boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(getActiveImage(), orientation);
        final int[] extents = getActiveImage().getExtents(orientation);
        if ( axisFlip[0] ) patientPt.X -= 1;
        if ( axisFlip[1] ) patientPt.Y -= 1;
        //if ( axisFlip[2] ) patientPt.Z -= 1;
        
        if ( (patientPt.X < 0) || (patientPt.X > extents[0]) ||
                (patientPt.Y < 0) || (patientPt.Y > extents[1]) )
        {
            bClipped = true;
        }
        MipavCoordinateSystems.patientToFile( patientPt, kFile, imageA, orientation );
        return bClipped;
    }

    @Override
    public void setCenter(Vector3f center) {
    	Vector3f patientPt = new Vector3f();
    	MipavCoordinateSystems.fileToPatient( center, patientPt, imageA, orientation );
    	setSlice( (int) patientPt.Z );
	}
}
