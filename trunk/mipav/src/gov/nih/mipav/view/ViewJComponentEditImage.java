package gov.nih.mipav.view;


import gov.nih.mipav.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.icons.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import java.awt.image.*;

import java.io.*;

import java.text.*;

import java.util.*;

import javax.swing.*;


/**
 * Basic displayable image object in MIPAV. Contains the viewable objects such as VOIs and carries listeners to most any
 * action that can happen in a window.
 *
 * @version  0.1 Nov 18, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class ViewJComponentEditImage extends ViewJComponentBase
        implements MouseMotionListener, MouseWheelListener, MouseListener, KeyListener, PaintGrowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3799092616373463766L;

    /**
     * Color used in the rectangle indicating the image is active. Images which are not active will not have the
     * recangle drawn.
     */
    public static Color ACTIVE_IMAGE_COLOR = Color.cyan;

    /* * In painting COMPLEX images the COMPLEX image is assumed to have been a result of
     * transforming real data.  x(n1,n2) real <-> X(k1,k2) = X*(-k1,-k2) for 2D and x(n1,n2,n3) real <-> X(k1,k2,k3) =
     * X*(-k1,-k2,-k3).  Therefore, when one pixel is painted in a COMPLEX image, the above defined complementary pixel
     * is also painted. The zero point for k1, k2, k3 occur such that there is one more pixel below the zero point than
     * there is above it.  For example, if the FFT is 2048 by 2048 by 2048, the center occurs at (1024,1024,1024) so
     * that (0,0,0) is further away than (2047,2047,2047) */

    /** Used to indicte geometric zoom steps (2x, 4x, 8x, 16x ...). */
    public static final int GEOMETRIC_ZOOM = 0;

    /** Used to indicte linear zoom steps (2x, 3x, 4x, 5x ...). */
    public static final int LINEAR_ZOOM = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

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

    /** String representing RGB components used in applied paint*/
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

    /** Flag used ONLY by ViewJComponentRegistration to prohibit VOI drawing. */
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
    protected Color toppedColor = ACTIVE_IMAGE_COLOR;

    /**
     * This flag is used by the fuzzy connectedness to indicate that a VOI should be used to calculate certain values to
     * initialize the process.
     */
    protected boolean useVOI = false;

    /** DOCUMENT ME! */
    protected boolean variableThresholds = false;

    /** Visible rectangle to draw topped. */
    protected Rectangle visRect;

    /** Handles all aspects of VOIs, including mouse responses, movements, VOI graphing etc. */
    protected VOIHandler voiHandler = null;

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
    private WindowLevel m_kWinLevel;

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

    private boolean useRComp = true;
    private boolean useGComp = true;
    private boolean useBComp = true;
    
    
    /** flag indicating whethere there is 0 to 1 LUT Adjustment **/
    private boolean zeroToOneLUTAdj = false;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  _frame         frame where image(s) will be displayed
     * @param  _imageA        Model of the image that will be displayed
     * @param  _LUTa          LUT used to display imageA
     * @param  imgBufferA     storage buffer used to display image A
     * @param  _imageB        Model of the image that will be displayed
     * @param  _LUTb          LUT used to display imageB
     * @param  imgBufferB     storage buffer used to display image B
     * @param  pixelBuffer    storage buffer used to build a displayable image
     * @param  zoom           initial magnification of image
     * @param  extents        initial display dimensions of the image
     * @param  logMagDisplay  display log magnitude of image
     * @param  _orientation   orientation of the image
     */
    public ViewJComponentEditImage(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
                                   ModelImage _imageB, ModelLUT _LUTb, float[] imgBufferB, int[] pixelBuffer,
                                   float zoom, int[] extents, boolean logMagDisplay, int _orientation) {
        super(_imageA.getWidth(_orientation), _imageA.getHeight(_orientation), _imageA);

        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;
        imageExtents = extents;

        orientation = _orientation;

        LUTa = _LUTa;
        LUTb = _LUTb;

        showSliceNumber = (imageA.getNDims() > 2) && !(this instanceof ViewJComponentTriImage);

        // active image color: get preset
        if (Preferences.getProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR) == null) {
            Preferences.setProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR, MipavUtil.makeColorString(ACTIVE_IMAGE_COLOR));
        } else {
            this.setHighlightColor(MipavUtil.extractColor(Preferences.getProperty(Preferences.PREF_ACTIVE_IMAGE_COLOR)));
        }

        // Custom crosshair cursors
        if (Preferences.getProperty(Preferences.PREF_CROSSHAIR_CURSOR) == null) {
            Preferences.setProperty(Preferences.PREF_CROSSHAIR_CURSOR, "default");
        }

        String crosshairName = Preferences.getProperty(Preferences.PREF_CROSSHAIR_CURSOR);

        if (crosshairName.equalsIgnoreCase("default")) {
            this.setCrosshairCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        } else {

            try {
                Toolkit toolkit = Toolkit.getDefaultToolkit();

                this.setCrosshairCursor(toolkit.createCustomCursor(MipavUtil.getIcon(crosshairName).getImage(),
                                                                   new Point(15, 15), crosshairName));
            } catch (NullPointerException noIcon) {

                // specfied icon cannot be found.  Instead, we load default:
                this.setCrosshairCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
                Preferences.debug("ViewJComponentEditImage: Crosshair icon \"" + crosshairName +
                                  "\" cannot be found.  " + "Instead, using default crosshair pointer.\n", 2);
            }
        }

        cursorMode = DEFAULT;

        if (imgBufferA == null) {
            int bufferFactor = (imageA.isColorImage() ? 4 : 1);
            imgBufferA = new float[bufferFactor * imageDim.width * imageDim.height];
        }

        if ((imgBufferB == null) && (imageB != null)) {
            int bufferFactor = (imageB.isColorImage() ? 4 : 1);
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


        if (this instanceof ViewJComponentSingleRegistration) {
            voiHandler = new VOIRegistrationHandler(this);
        } else {
            voiHandler = new VOIHandler(this);
        }

        if (imageA.isDicomImage()) {
            voiHandler.setOverlay(Preferences.is(Preferences.PREF_SHOW_DICOM_OVERLAYS));
        } else {
            voiHandler.setOverlay(Preferences.is(Preferences.PREF_SHOW_IMAGE_OVERLAYS));
        }

        if (!(this instanceof ViewJComponentTriImage)) {
            addMouseListener(voiHandler);
            addMouseMotionListener(voiHandler);
        }

        addMouseMotionListener(this);
        addMouseListener(this);
        setVisible(true);

        addKeyListener(this);
        addMouseWheelListener(this);


        if (frame != null) {
            magSettings = new JDialogMagnificationControls((Frame) frame, this, getZoomX(), imageA.getImageName());
        }

        maxExtents[0] = imageDim.width;
        maxExtents[1] = imageDim.height;

        /* create the slice renderer for this orientation: */
        m_kPatientSlice = new PatientSlice(imageA, LUTa, imageB, LUTb, orientation);
        m_kPatientSlice.setBuffers(imageBufferA, imageBufferB);

        /* create the WindowLevel controller: */
        m_kWinLevel = new WindowLevel();

        if (!(_frame instanceof ViewJFrameLightBox)) {
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
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Gets position data to display in message bar - for DICOM and MINC images, gives patient position as well. The
     * image's associated transformation must be FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL, or the orientations must be
     * set up correctly, or else the function returns null.
     *
     * @param   image     The image the point lies within.
     * @param   position  (x,y,z(slice)) position in FileCoordinates
     *
     * @return  An array of strings that represent patient position.
     */
    public static final String[] getScannerPositionLabels(ModelImage image, Point3Df position) {
        DecimalFormat nf = new DecimalFormat("#####0.0##");
        Point3Df kOut = new Point3Df();
        if (image.getNDims() < 3) {
       // 	return null;
        }
        MipavCoordinateSystems.fileToScanner(position, kOut, image);

        float[] tCoord = new float[3];
        tCoord[0] = kOut.x;
        tCoord[1] = kOut.y;
        tCoord[2] = kOut.z;

        String[] labels = { "R-L: ", "A-P: ", "I-S: " };

        if (!image.getRadiologicalView()) {
            labels[0] = new String("L-R: ");
        }

        String[] strs = new String[3];

        if (image.getRadiologicalView()) {

            if ((tCoord[0] < 0)) {
                strs[0] = new String(labels[0] + labels[0].charAt(0) + ": " + String.valueOf(nf.format(tCoord[0])));
            } else {
                strs[0] = new String(labels[0] + labels[0].charAt(2) + ": " + String.valueOf(nf.format(tCoord[0])));
            }
        } else {

            if ((tCoord[0] < 0)) {
                strs[0] = new String(labels[0] + labels[0].charAt(2) + ": " + String.valueOf(nf.format(tCoord[0])));
            } else {
                strs[0] = new String(labels[0] + labels[0].charAt(0) + ": " + String.valueOf(nf.format(tCoord[0])));
            }
        }

        for (int i = 1; i < 3; i++) {

            if ((tCoord[i] < 0)) {
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
     * @param  str  string in showRegionGrow {@link #showRegionGrow(int, String)}
     */
    public void calcPaintedVolume(String str) {
        int count = 0;
        int end = paintBitmap.size();

        for (int i = 0; i < end; i++) {

            if (paintBitmap.get(i) == true) {
                count++;
            }
        }

        showRegionInfo(count, str);
    }

    /**
     * Loops through the images and displays them.
     *
     * @param  ms  how long to wait between each image
     */
    public synchronized void cine(int ms) {
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
            } catch (InterruptedException error) {
                Preferences.debug("ViewJComponentEditImage.cine:wait.\n");
            }
        }

        localTime = (System.currentTimeMillis() - localTime) / zDim;
        Preferences.debug("\n ***** " + (1 / ((float) localTime / 1000)) + " fps.\n\n");
    }

    /**
     * Replace intensities in the image using painted mask.
     *
     * @param  imagesDone      IMAGE_A, IMAGE_B, or BseedVOTH
     * @param  clearPaintMask  if true clear paint mask
     * @param  polarity        DOCUMENT ME!
     */
    public void commitMask(int imagesDone, boolean clearPaintMask, boolean polarity) {
        commitMask(imagesDone, clearPaintMask, polarity, null);
    }

    /**
     * Replace intensities in the image using painted mask.
     *
     * @param  imagesDone           IMAGE_A, IMAGE_B, or BseedVOTH
     * @param  clearPaintMask       if true clear paint mask
     * @param  polarity             DOCUMENT ME!
     * @param  intensityLockVector  Vector containing Integers values which are indexed to the locked intensity values
     *                              in the image
     */
    public void commitMask(int imagesDone, boolean clearPaintMask, boolean polarity, Vector intensityLockVector) {
        commitMask(imagesDone, clearPaintMask, polarity, intensityLockVector, true);
    }

    /**
     * Replace intensities in the image using painted mask.
     *
     * @param  affectedImage        IMAGE_A, IMAGE_B, or BseedVOTH
     * @param  clearPaintMask       if true clear paint mask
     * @param  polarity             DOCUMENT ME!
     * @param  intensityLockVector  Vector containing Integers values which are indexed to the locked intensity values
     *                              in the image
     * @param  showProgressBar      if true, shows the progress bar for this algorithm
     */
    public void commitMask(ModelImage affectedImage, boolean clearPaintMask, boolean polarity,
                           Vector intensityLockVector, boolean showProgressBar) {

        if (affectedImage == imageA) {
            commitMask(IMAGE_A, clearPaintMask, polarity, intensityLockVector, showProgressBar);
        }

        if ((imageB != null) && (affectedImage == imageB)) {
            commitMask(IMAGE_B, clearPaintMask, polarity, intensityLockVector, showProgressBar);
        }
    }


    /**
     * Replace intensities in the image using painted mask.
     *
     * @param  imagesDone           IMAGE_A, IMAGE_B, or BseedVOTH
     * @param  clearPaintMask       if true clear paint mask
     * @param  polarity             DOCUMENT ME!
     * @param  intensityLockVector  Vector containing Integers values which are indexed to the locked intensity values
     *                              in the image
     * @param  showProgressBar      if true, shows the progress bar for this algorithm
     */
    public void commitMask(int imagesDone, boolean clearPaintMask, boolean polarity, Vector intensityLockVector,
                           boolean showProgressBar) {

        float min, max;
        Color fillColor = new Color(128, 0, 0);
        int[] slice = new int[1];
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

        if ((imagesDone == IMAGE_A) || (imagesDone == BOTH)) {
            float imgMinOrig = (float) imageA.getMin();
            float imgMaxOrig = (float) imageA.getMax();

            if (imageA.isColorImage() == true) {
                maskAlgo = new AlgorithmMask(imageA, fillColor, polarity, false);
                maskAlgo.setRunningInSeparateThread(false);
                maskAlgo.calcInPlace25DC(paintBitmap, fillColor, timeSlice, rgbString);
            } else {

                if (imageA.getNDims() == 4) {

                    // Build dialog 3D or 4D
                    JDialogMask3D4D dialog3D4D = new JDialogMask3D4D(frame, slice);

                    if (slice[0] == -1) {
                        timeSlice = -1;
                    }
                }

                if (slice[0] <= 0) {
                    maskAlgo = new AlgorithmMask(imageA, intensityDropper, polarity, false);
                    maskAlgo.setRunningInSeparateThread(false);
                    maskAlgo.calcInPlace25D(paintBitmap, intensityDropper, timeSlice, intensityLockVector);

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

                    float imgMin = (float) imageA.getMin();
                    float imgMax = (float) imageA.getMax();

                    if ((intensityDropper < imgMinOrig) || (intensityDropper > imgMaxOrig)) {
                        LUTa.resetTransferLine(min, imgMin, max, imgMax);

                        if (imageA.getHistoLUTFrame() != null) {
                            imageA.getHistoLUTFrame().update();
                        }
                    }
                }
            }
        }

        maskAlgo = null;

        if (((imagesDone == IMAGE_B) || (imagesDone == BOTH)) && (imageB != null)) {

            if (imageB.isColorImage() == true) {
                maskAlgo = new AlgorithmMask(imageB, fillColor, polarity, false);
                maskAlgo.setRunningInSeparateThread(false);
                maskAlgo.calcInPlace25DC(paintBitmap, fillColor, timeSlice, rgbString);
            } else {

                if (imageA.getNDims() == 4) {

                    // Build dialog 3D or 4D
                    JDialogMask3D4D dialog3D4D = new JDialogMask3D4D(frame, slice);

                    if (slice[0] == -1) {
                        timeSlice = -1;
                    }
                }

                maskAlgo = new AlgorithmMask(imageB, intensityDropper, polarity, false);
                maskAlgo.setRunningInSeparateThread(false);
                maskAlgo.calcInPlace25D(paintBitmap, intensityDropper, timeSlice, intensityLockVector);

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
     * Creates a new short image from the paint mask.
     *
     * @return  the name of the new short image
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

        ViewJProgressBar progressBar = new ViewJProgressBar(imageActive.getImageName(), "Masking ...", 0, 100, true);
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
            maskAlgo.calcInPlace25DCShortMask((BitSet) paintBitmap.clone(), fillColor, timeSlice);

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
            maskAlgo.calcInPlace25DShortMask((BitSet) paintBitmap.clone(), intensityDropper, timeSlice);
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

                    if ((imageACopy.getType() == ModelStorageBase.COMPLEX) ||
                            (imageACopy.getType() == ModelStorageBase.DCOMPLEX)) {
                        bufferI = new double[length];
                        imageACopy.exportDComplexData(0, length, buffer, bufferI);
                        imageACopy.reallocate(ModelStorageBase.SHORT);

                        for (i = 0; i < length; i++) {
                            buffer[i] = Math.round(Math.sqrt((buffer[i] * buffer[i]) + (bufferI[i] * bufferI[i])));
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

                            if ((Math.round((byte) buffer[(4 * i) + 1]) == red) &&
                                    (Math.round((byte) buffer[(4 * i) + 2]) == green) &&
                                    (Math.round((byte) buffer[(4 * i) + 3]) == blue)) {
                                bufferShort[i] = 1;
                            }
                        }

                        imageACopy.importData(0, bufferShort, true);
                    } else {
                        imageACopy.exportData(0, length, buffer); // locks and releases lock
                        imageACopy.reallocate(ModelStorageBase.SHORT);
                        imageACopy.importData(0, buffer, true);
                    }
                } catch (IOException error) {
                    buffer = null;
                    MipavUtil.displayError("IO Exception");

                    if (imageACopy != null) {
                        imageACopy.disposeLocal();
                        imageACopy = null;
                    }

                    return null;
                } catch (OutOfMemoryError e) {
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

                    if ((imageBCopy.getType() == ModelStorageBase.COMPLEX) ||
                            (imageBCopy.getType() == ModelStorageBase.DCOMPLEX)) {
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

                            if ((Math.round((byte) buffer[(4 * i) + 1]) == red) &&
                                    (Math.round((byte) buffer[(4 * i) + 2]) == green) &&
                                    (Math.round((byte) buffer[(4 * i) + 3]) == blue)) {
                                bufferShort[i] = 1;
                            }
                        }

                        imageBCopy.importData(0, bufferShort, true);
                    } else {
                        imageBCopy.exportData(0, length, buffer); // locks and releases lock
                        imageBCopy.reallocate(ModelStorageBase.SHORT);
                        imageBCopy.importData(0, buffer, true);
                    }
                } catch (IOException error) {
                    buffer = null;
                    MipavUtil.displayError("IO Exception");

                    if (imageBCopy != null) {
                        imageBCopy.disposeLocal();
                        imageBCopy = null;
                    }

                    return null;
                } catch (OutOfMemoryError e) {
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

            if (imageBCopy == null) {

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
        } catch (OutOfMemoryError error) {
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

        if (imageACopy != null) {
            return imageACopy.getImageName();
        } else {
            return imageBCopy.getImageName();
        }
    }

    /**
     * Deletes the selected contour of an VOI.
     */
    public void deleteSelectedContours() {
        this.deleteSelectedContours(-1, false);
    }

    /**
     * Deletes the selected contour of an VOI.
     *
     * @param  centerPtLocation  DOCUMENT ME!
     * @param  btest             DOCUMENT ME!
     */
    public void deleteSelectedContours(int centerPtLocation, boolean btest) {
        int i, s, nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = 0; i < nVOI; i++) {

            if ((VOIs.VOIAt(i).isActive() == true) && (!btest || (i != centerPtLocation))) {

                break;
            } // Set i
        }

        if (i == nVOI) {
            MipavUtil.displayError("VOI must be selected.");

            return; // No VOI to delete
        }

        if (imageActive.getNDims() == 2) {
            getVOIHandler().deleteContour(VOIs.VOIAt(i), 0);
        } else if (imageActive.getNDims() >= 3) {

            for (s = 0; s < imageActive.getExtents()[2]; s++) {
                getVOIHandler().deleteContour(VOIs.VOIAt(i), s);
            }
        }

        // System.err.println("We are always going through this for deletion");

        if (VOIs.VOIAt(i).isEmpty() == true) {
            imageActive.unregisterVOI(VOIs.VOIAt(i));

            int id = (getActiveImage().getVOIs().size() > 0)
                     ? (((VOI) (getActiveImage().getVOIs().lastElement())).getID() + 1) : 0;
            int lastUID = (getActiveImage().getVOIs().size() > 0)
                          ? (((VOI) (getActiveImage().getVOIs().lastElement())).getUID() + 1) : -1;

            getVOIHandler().updateVOIColor(id, lastUID);
            voiHandler.setVOI_ID(-1);
        }

        imageActive.notifyImageDisplayListeners(null, true);
    }

    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  flag  if true garbage collector should be called.
     */
    public void disposeLocal(boolean flag) {
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

        if (voiHandler != null) {
            voiHandler.disposeLocal(flag);
        }

        voiHandler = null;

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
     * @param  g     The graphics object which will do the painting.
     * @param  xRes  The resolutions in the <i>x</i>-direction.
     * @param  yRes  The resolutions in the <i>y</i>-direction.
     */
    public void drawGradicules(Graphics g, float xRes, float yRes) {
        Insets insets = frame.getInsets();
        int rightOffset = getBounds().width - insets.left;
        int bottomOffset = getBounds().height - insets.bottom - 15;

        // Draw gradicules
        if ((getZoomX() >= 1.0) && (getZoomY() >= 1.0)) {
            float XCMPerPix = (float) (xRes / 10.0 / getZoomX());
            float YCMPerPix = (float) (yRes / 10.0 / getZoomY());

            int XGradcmLen = (int) ((getBounds().width / 3.0) * (XCMPerPix));
            int YGradcmLen = (int) ((getBounds().height / 3.0) * (YCMPerPix));

            int XGradpixLen = (int) (XGradcmLen / XCMPerPix);
            int YGradpixLen = (int) (YGradcmLen / XCMPerPix);

            g.setColor(Color.white);
            g.drawLine(XGradpixLen, bottomOffset - 30, XGradpixLen, bottomOffset - 43);
            g.drawLine(XGradpixLen + (int) (XGradcmLen / XCMPerPix), bottomOffset - 30,
                       XGradpixLen + (int) (XGradcmLen / XCMPerPix), bottomOffset - 43);
            g.drawLine(rightOffset - 22, YGradpixLen, rightOffset - 35, YGradpixLen);
            g.drawLine(rightOffset - 22, YGradpixLen + (int) (YGradcmLen / YCMPerPix), rightOffset - 35,
                       YGradpixLen + (int) (YGradcmLen / YCMPerPix));

            g.setColor(Color.black);
            g.drawLine(XGradpixLen + 1, bottomOffset - 30, XGradpixLen + 1, bottomOffset - 43);
            g.drawLine(XGradpixLen - 1, bottomOffset - 30, XGradpixLen - 1, bottomOffset - 43);
            g.drawLine(XGradpixLen + (int) (XGradcmLen / XCMPerPix) + 1, bottomOffset - 30,
                       XGradpixLen + (int) (XGradcmLen / XCMPerPix) + 1, bottomOffset - 43);
            g.drawLine(XGradpixLen + (int) (XGradcmLen / XCMPerPix) - 1, bottomOffset - 30,
                       XGradpixLen + (int) (XGradcmLen / XCMPerPix) - 1, bottomOffset - 43);
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
                g.drawLine(XGradpixLen + (int) (i / XCMPerPix) + 1, bottomOffset - 30,
                           XGradpixLen + (int) (i / XCMPerPix) + 1, bottomOffset - 35);
                g.drawLine(XGradpixLen + (int) (i / XCMPerPix) - 1, bottomOffset - 30,
                           XGradpixLen + (int) (i / XCMPerPix) - 1, bottomOffset - 35);
            }

            for (int i = 1; i < YGradcmLen; i++) {
                g.setColor(Color.white);
                g.drawLine(rightOffset - 22, YGradpixLen + (int) (i / YCMPerPix), rightOffset - 27,
                           YGradpixLen + (int) (i / YCMPerPix));
                g.setColor(Color.black);
                g.drawLine(rightOffset - 22, YGradpixLen + (int) (i / YCMPerPix) + 1, rightOffset - 27,
                           YGradpixLen + (int) (i / YCMPerPix) + 1);
                g.drawLine(rightOffset - 22, YGradpixLen + (int) (i / YCMPerPix) - 1, rightOffset - 27,
                           YGradpixLen + (int) (i / YCMPerPix) - 1);
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
     * @param  str  string to be displayed
     * @param  g    graphics contexts (where to draw the string)
     * @param  x    x coordinate of where the string is to be drawn
     * @param  y    y coordinate of where the string is to be drawn
     */
    public final void drawStringBW(String str, Graphics g, int x, int y) {

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
     * @param  onlyCurrent  DOCUMENT ME!
     */
    public void eraseAllPaint(boolean onlyCurrent) {

        if (onlyCurrent) {

            // find start point
            int currentFrame = imageA.getParentFrame().getViewableSlice();
            int sliceSize = imageA.getSliceSize();
            int startPt = currentFrame * sliceSize;
            int endPt = startPt + sliceSize;

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
     * @return  active image
     */
    public ModelImage getActiveImage() {
        return imageActive;
    }

    /**
     * Returns the active image buffer.
     *
     * @return  active image buffer
     */
    public float[] getActiveImageBuffer() {
        return imageBufferActive;
    }

    /**
     * Returns the active image buffer for this slice.
     *
     * @return  active image buffer for this slice
     */
    public float[] getActiveImageSliceBuffer() {
        int cFactor = 1;

        if (imageActive.isColorImage() == true) {
            cFactor = 4;
        }

        float[] sliceBuffer = new float[cFactor * imageExtents[0] * imageExtents[1]];

        if (imageActive.isColorImage() == true) {

            try {
                imageActive.exportData(0, sliceBuffer.length, sliceBuffer);
            } catch (IOException error) {
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
     * @return  active image
     */
    public ModelLUT getActiveLUT() {
        return (ModelLUT) m_kPatientSlice.getActiveLookupTable();
    }

    /**
     * Returns float alphaBlend. The value used to blend two images displayed in the same frame.
     *
     * @return  alphaBlend
     */
    public float getAlphaBlend() {
        return alphaBlend;
    }

    /**
     * Returns the VOI mode.
     *
     * @return  drawing mode for the VOI tools (i.e. ELLIPSE, LINE ...)
     */
    public int getCursorMode() {
        return cursorMode;
    }

    /**
     * Returns the frame.
     *
     * @return  frame
     */
    public ViewJFrameBase getFrame() {
        return frame;
    }

    /**
     * Returns the imageA.
     *
     * @return  imageA
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Returns the imageB.
     *
     * @return  imageB
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Gets imageA's image buffer of intensities.
     *
     * @return  float[] imageA's intensity buffer
     */
    public float[] getImageBufferA() {
        return imageBufferA;
    }

    /**
     * Gets imageB's image buffer of intensities.
     *
     * @return  float[] imageB's intensity buffer
     */

    public float[] getImageBufferB() {
        return imageBufferB;
    }

    /**
     * Gets the intensityDropper intensity.
     *
     * @return  the intensity dropper
     */
    public float getIntensityDropper() {
        return intensityDropper;
    }

    /**
     * Returns the model lut for the imageA.
     *
     * @return  the model LUT for imageA
     */
    public ModelLUT getLUTa() {
        return LUTa;
    }

    /**
     * Returns the model lut for the imageB.
     *
     * @return  the model LUT for imageB
     */
    public ModelLUT getLUTb() {
        return LUTb;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  boolean
     */
    public boolean getModifyFlag() {
        return modifyFlag;
    }

    /**
     * Finds the number of points in the active VOI contour.
     *
     * @return  the number of points in the selected VOI
     */
    public int getNumPoints() {
        ViewVOIVector VOIs = imageActive.getVOIs();

        if (VOIs.size() == 0) {
            return 0;
        } else {
            return ((VOI) VOIs.elementAt(0)).getCurves()[0].size();
        }
    }

    /**
     * Returns the number of VOIs in the active image.
     *
     * @return  int number of VOIs
     */
    public int getnVOI() {
        ViewVOIVector VOIs = imageActive.getVOIs();

        return (VOIs.size());
    }

    /**
     * Returns float opacity.
     *
     * @return  opacity
     */
    public float getOpacity() {
        return frame.getControls().getTools().getOpacity();
    }

    /**
     * Returns the image's orientation.
     *
     * @return  image orientation
     */
    public int getOrientation() {
        return orientation;
    }

    /**
     * Returns BitSet paintBitmap.
     *
     * @return  paintBitmap
     */
    public BitSet getPaintBitmap() {
        return paintBitmap;
    }

    /**
     * Gets the paint buffer.
     *
     * @return  int[] paint buffer
     */
    public int[] getPaintBuffer() {
        return paintImageBuffer;
    }

    /**
     * Gets the paint mask.
     *
     * @return  the current paint mask
     */
    public BitSet getPaintMask() {
        return paintBitmap;
    }

    /**
     * Gets the buffer used to store ARGB images of the image presently being displayed.
     *
     * @return  int[] buffer of ARGB image A
     */
    public int[] getPixBuffer() {
        return pixBuffer;
    }

    /**
     * Gets the buffer used to store ARGB images of the image presently being displayed.
     *
     * @return  int[] buffer of ARGB image A
     */
    public int[] getPixBufferB() {
        return pixBufferB;
    }


    /**
     * Returns the ModelRGB RGBTA for imageA.
     *
     * @return  RGBTA for imageA
     */
    public ModelRGB getRGBTA() {
        return RGBTA;
    }

    /**
     * Returns the ModelRGB for imageB.
     *
     * @return  RGBTB for imageB
     */
    public ModelRGB getRGBTB() {
        return RGBTB;
    }


    /**
     * Determines whether to enable the showIntensity checkbox for magnification box.
     *
     * @return  whether to enable showIntensity checkbox
     */
    public boolean getShowMagIntensityEnabled() {
        Graphics g = getGraphics();

        if (g == null) {
            return false;
        }

        g.setFont(MipavUtil.font10);

        return super.getShowMagIntensityEnabled(g, MAGR_WIDTH, MAGR_HEIGHT, MAGR_MAG, imageActive.getType(),
                                                getActiveImage().getMin(), getActiveImage().getMax());

    }

    /**
     * Returns the slice of the image.
     *
     * @return  the slice
     */
    public int getSlice() {
        return slice;
    }

    /**
     * Gets the time slice of the image.
     *
     * @return  the current time slice
     */
    public int getTimeSlice() {
        return timeSlice;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public VOIHandler getVOIHandler() {
        return this.voiHandler;
    }

    /**
     * Tells whether or not the image is being displayed in checkerboard mode.
     *
     * @return  boolean
     */
    public boolean isCheckerboarded() {
        return ((nRowCheckers > 1) && (nColumnCheckers > 1));
    }


    // ************************************************************************
    // **************************** Key Events *****************************
    // ************************************************************************

    /**
     * Does nothing.
     *
     * @param  e  event
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * If the shift key is pressed, updates the graphics.
     *
     * @param  e  the key released event
     */
    public void keyReleased(KeyEvent e) {
        int keyCode = e.getKeyCode();

        if (cursorMode == WIN_REGION) {

            switch (keyCode) {

                case KeyEvent.VK_SHIFT:
                    update(getGraphics());
            }
        }
    }

    /**
     * Does nothing.cent.
     *
     * @param  e  event
     */
    public void keyTyped(KeyEvent e) { }

    /**
     * This method loads the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are loaded. If this is a color image, then only the functions are loaded.
     *
     * @param  loadAll    boolean boolean indicating that both lut and transfer functions should be loaded. If false,
     *                    then only transfer functions are loaded.
     * @param  filename   String filename to save LUT as
     * @param  dirName    String directory to save LUT to
     * @param  quietMode  boolean if true indicates that warnings should not be displayed.
     */
    public void loadOnlyLUTFrom(boolean loadAll, String filename, String dirName, boolean quietMode) {
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

        // if not using a lut (i.e. rgb only), then you
        // can't loadAll.... there are only functions, so
        // reset the loadAll variable
        if (!useLUT) {
            loadAll = false;
        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);

                if (loadAll) {
                    fileHistoLUT.readOnlyLUT(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }

                if (imageActive == imageA) {
                    this.setLUTa(lut);
                } else {
                    this.setLUTb(lut);
                }
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);

                if (loadAll) {
                    fileHistoLUT.readOnlyLUT(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }

                if (imageActive == imageA) {
                    this.setRGBTA(rgb);
                } else {
                    this.setRGBTB(rgb);
                }
            }

            img.notifyImageDisplayListeners(lut, true);

        } catch (IOException error) {

            if (!quietMode) {
                MipavUtil.displayError("Error reading LUT: \n" + error.getMessage());
            }
        }
    } // end loadLUTFrom()

    /**
     * Loads built-in (.gif) or custom (.png) paint brushes based on the last-used paint brush in preferences.
     *
     * @param  paintName  the name of the brush to load
     */
    public void loadPaintBrush(String paintName) {
        String fullPath = null;

        if (paintName == null) {
            fullPath = PlaceHolder.class.getResource("square 8x8.gif").getPath();
        } else {

            try {
                fullPath = PlaceHolder.class.getResource(paintName).getPath();
            } catch (Exception e) {
                fullPath = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "brushes" +
                           File.separator + paintName;

                if (!(new File(fullPath)).exists()) {
                    fullPath = PlaceHolder.class.getResource("square 8x8.gif").getPath();
                }

            }
        }

        // fix the URL nonsense with spaces
        fullPath = fullPath.replaceAll("%20", " ");

        File paintFile = new File(fullPath);
        FileIO fileIO = new FileIO();

        // read in the .gif or .png as a model image to create the BitSet
        ModelImage brushImage = fileIO.readImage(paintFile.getPath());

        if (brushImage == null) {
            return;
        }

        int[] brushExtents = brushImage.getExtents();

        // create the bitset and the brush dimensions
        paintBrushDim = new Dimension(brushExtents[0], brushExtents[1]);
        paintBrush = new BitSet(brushExtents[0] * brushExtents[1]);

        int counter = 0;

        int[] buffer = new int[brushExtents[0] * brushExtents[1] * 4];

        try {
            brushImage.exportData(0, buffer.length, buffer);
        } catch (Exception e) {
            MipavUtil.displayError("Open brush failed.");
            brushImage.disposeLocal();

            return;
        }


        int length = buffer.length;

        if (paintImage != null) {
            paintImage.flush();
            paintImage = null;
        }

        // create a buffered image that will be drawn in place of a cursor (with red transparent pixels)
        paintImage = new BufferedImage(paintBrushDim.width, paintBrushDim.height, BufferedImage.TYPE_INT_ARGB);


        // set or clear the bitset based on the black pixels (black == on)
        for (int i = 0; i < length; i += 4, counter++) {

            if (buffer[i + 1] == 0) {
                paintBrush.set(counter);
            } else {
                paintBrush.clear(counter);
            }
        }

        updatePaintBrushCursor();

        // remove the image created as it is no longer needed
        brushImage.disposeLocal();

    }

    /**
     * Loads built-in (.gif) or custom (.png) paint brushes based on the last-used paint brush in preferences.
     *
     * @param  paintName  the name of the brush to load
     * @param  isQuick    DOCUMENT ME!
     */
    public void loadPaintBrush(String paintName, boolean isQuick) {

        if (isQuick) {
            quickSwitchBrush();
        }

        String fullPath = null;

        if (paintName == null) {
            fullPath = PlaceHolder.class.getResource("square 8x8.gif").getPath();
        } else {

            try {
                fullPath = PlaceHolder.class.getResource(paintName).getPath();
            } catch (Exception e) {
                fullPath = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "brushes" +
                           File.separator + paintName;

                if (!(new File(fullPath)).exists()) {
                    fullPath = PlaceHolder.class.getResource("square 8x8.gif").getPath();
                }

            }
        }

        // fix the URL nonsense with spaces
        fullPath = fullPath.replaceAll("%20", " ");

        File paintFile = new File(fullPath);
        FileIO fileIO = new FileIO();

        // read in the .gif or .png as a model image to create the BitSet
        ModelImage brushImage = fileIO.readImage(paintFile.getPath());

        if (brushImage == null) {
            return;
        }

        int[] brushExtents = brushImage.getExtents();

        // create the bitset and the brush dimensions
        paintBrushDim = new Dimension(brushExtents[0], brushExtents[1]);
        paintBrush = new BitSet(brushExtents[0] * brushExtents[1]);

        int counter = 0;

        int[] buffer = new int[brushExtents[0] * brushExtents[1] * 4];

        try {
            brushImage.exportData(0, buffer.length, buffer);
        } catch (Exception e) {
            MipavUtil.displayError("Open brush failed.");
            brushImage.disposeLocal();

            return;
        }


        int length = buffer.length;

        if (paintImage != null) {
            paintImage.flush();
            paintImage = null;
        }

        // create a buffered image that will be drawn in place of a cursor (with red transparent pixels)
        paintImage = new BufferedImage(paintBrushDim.width, paintBrushDim.height, BufferedImage.TYPE_INT_ARGB);


        // set or clear the bitset based on the black pixels (black == on)
        for (int i = 0; i < length; i += 4, counter++) {

            if (buffer[i + 1] == 0) {
                paintBrush.set(counter);
            } else {
                paintBrush.clear(counter);
            }
        }

        updatePaintBrushCursor();

        // remove the image created as it is no longer needed
        brushImage.disposeLocal();

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
     * @param  mouseEvent  event that triggers function; contains click count
     */
    public void mouseClicked(MouseEvent mouseEvent) {
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
     * @param  mouseEvent  event that triggered function
     */
    public void mouseDragged(MouseEvent mouseEvent) {
        int mouseMods = mouseEvent.getModifiers();

        int xS, yS;
        Color dropperColor;
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        if ((pixBuffer == null) || (imageBufferActive == null) || (modifyFlag == false)) {
            return;
        }

        xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        int xDim = imageActive.getExtents()[0];
        int yDim = imageActive.getExtents()[1];

        if ((xS < 0) || (xS >= xDim) || (yS < 0) || (yS >= yDim)) {
            return;
        }

        // if((mouseEvent.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK) == InputEvent.BUTTON1_DOWN_MASK) {
        processDefaultMouseDrag(mouseEvent, xS, yS);
        // }


        if (cursorMode == DROPPER_PAINT) {

            if (imageActive.isColorImage() == true) {
                dropperColor = new Color((int) imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 1],
                                         (int) imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 2],
                                         (int) imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 3]);
                frame.getControls().getTools().setPaintColor(dropperColor);
            } else {
                intensityDropper = imageBufferActive[(yS * imageActive.getExtents()[0]) + xS];
                frame.getControls().getTools().setIntensityPaintName(String.valueOf((int) (intensityDropper)));
            }
        } else if (cursorMode == ERASER_PAINT) {
            performPaint(mouseEvent, true);
            imageActive.notifyImageDisplayListeners();
        } else if (cursorMode == PAINT_VOI) {
            performPaint(mouseEvent, mouseMods == MouseEvent.BUTTON3_MASK);
            imageActive.notifyImageDisplayListeners();
        }
    }

    /**
     * Store the lastMouse position.
     *
     * @param  mouseEvent  event
     */
    public void mouseEntered(MouseEvent mouseEvent) {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();
    }

    /**
     * Resets the level set stack.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseExited(MouseEvent mouseEvent) {
        lastMouseX = OUT_OF_BOUNDS;
        lastMouseY = OUT_OF_BOUNDS;

        if ((cursorMode == MAG_REGION) || (cursorMode == PAINT_VOI) || (cursorMode == ERASER_PAINT) ||
                (cursorMode == WIN_REGION)) {

            // repaint();
            paintComponent(getGraphics());
        }

        if (cursorMode == DEFAULT) {
            intensityLabel = false;
            paintComponent(getGraphics());
        }
    }

    /**
     * A mouse event. If the mode is level set, draws level sets as user moves mouse. Otherwise, changes the cursor
     * depending on where the mouse is in relation to the VOI.
     *
     * @param  mouseEvent  event that triggered the function
     */
    public void mouseMoved(MouseEvent mouseEvent) {
        int xS, yS;

        Graphics g = getGraphics();
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();
        shiftDown = mouseEvent.isShiftDown();

        if ((cursorMode == ZOOMING_IN) || (cursorMode == ZOOMING_OUT)) {

            // if we are in zoom mode, we don't care about any of the other things
            // that are happening here, in fact, zoom breaks if we don't return
            return;
        } else if ((cursorMode == RECTANGLE) || (cursorMode == ELLIPSE) || (cursorMode == LINE) ||
                       (cursorMode == RECTANGLE3D) || (cursorMode == POINT_VOI) || (cursorMode == POLYLINE) ||
                       (cursorMode == LEVELSET) || (cursorMode == PAINT_VOI) || (cursorMode == DROPPER_PAINT) ||
                       (cursorMode == ERASER_PAINT) || (cursorMode == QUICK_LUT) || (cursorMode == PROTRACTOR) ||
                       (cursorMode == LIVEWIRE) || (cursorMode == ANNOTATION) || (cursorMode == POLYLINE_SLICE_VOI) ||
                       (cursorMode == MOVE) || (cursorMode == MOVE_POINT) || (cursorMode == NEW_POINT) ||
                       (cursorMode == RETRACE) || (cursorMode == DELETE_POINT) || (cursorMode == TRANSLATE)) {
            g.dispose();

            return;
        }

        xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        if ((cursorMode == PAINT_VOI) && mouseEvent.isShiftDown()) {
            performPaint(mouseEvent, false);
            imageActive.notifyImageDisplayListeners(null, true);

            return;
        }

        // the user can erase by holding down shift while in eraser mode
        // or by holding down control while in paint mode
        if (((cursorMode == ERASER_PAINT) && mouseEvent.isShiftDown()) ||
                ((cursorMode == PAINT_VOI) && mouseEvent.isControlDown())) {
            performPaint(mouseEvent, true);
            imageActive.notifyImageDisplayListeners(null, true);

            return;
        }

        if ((g == null) || (modifyFlag == false) || (slice == -99)) {
            return;
        }

        if ((pixBuffer == null) || (imageBufferActive == null)) {
            g.dispose();

            return;
        }

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || // Check to ensure point is within
                (yS < 0) || (yS >= imageActive.getExtents()[1])) { // the image bounds
            g.dispose();

            return;
        }

        if (cursorMode == MAG_REGION) {
            repaint();

            return;
        } else if (cursorMode == WIN_REGION) {
            repaint();

            return;
        } else if ((cursorMode == PAINT_VOI) || (cursorMode == ERASER_PAINT)) {

            // repaint();
            paintComponent(getGraphics());

            return;
        } else if ((cursorMode == PAINT_CAN) || (cursorMode == PAINT_VASC)) {

            if (growDialog != null) {

                if (imageActive.isColorImage()) {
                    growDialog.setPositionText("  X: " + String.valueOf((xS + 1)) + " Y: " + String.valueOf((yS + 1)) +
                                               "  R:  " +
                                               String.valueOf(imageBufferActive[(4 *
                                                                                     ((yS *
                                                                                           imageActive.getExtents()[0]) +
                                                                                          xS)) + 1]) + "  G:  " +
                                               String.valueOf(imageBufferActive[(4 *
                                                                                     ((yS *
                                                                                           imageActive.getExtents()[0]) +
                                                                                          xS)) + 2]) + "  B:  " +
                                               String.valueOf(imageBufferActive[(4 *
                                                                                     ((yS *
                                                                                           imageActive.getExtents()[0]) +
                                                                                          xS)) + 3]));
                } else {
                    growDialog.setPositionText("  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) +
                                               "  Intensity:  " +
                                               String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]));
                }
            }

            g.dispose();

            return;
        }

        // System.err.println("got to end...");

        setCursorMode(DEFAULT);
    } // end mouseMoved


    /**
     * A mouse event. Sets the mode of the program depending on the cursor mode. If the mode is move, activates the
     * contour or line and enables the delete button.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mousePressed(MouseEvent mouseEvent) {
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        int xS = getScaledX(mouseEvent.getX());
        int yS = getScaledY(mouseEvent.getY());

        if (modifyFlag == false) {
            return;
        }

        // save the state of the shift button
        mousePressIsShiftDown = mouseEvent.isShiftDown();

        // shows intsnsity label upon mouse press
        if (cursorMode == DEFAULT) {
            setPixelInformationAtLocation(xS, yS);

            if (mouseEvent.getButton() == MouseEvent.BUTTON1) {
                intensityLabel = true;
                paintComponent(getGraphics());
            }
        }


        if ((cursorMode == DEFAULT) && mouseEvent.isControlDown()) { // center the image around cursor (no zooming)

            int centerX = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().width / 2;
            int centerY = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().height / 2;

            ((ViewJFrameImage) frame).getScrollPane().getHorizontalScrollBar().setValue(mouseEvent.getX() - centerX);
            ((ViewJFrameImage) frame).getScrollPane().getVerticalScrollBar().setValue(mouseEvent.getY() - centerY);


        }

        if ((cursorMode == ZOOMING_IN) || (cursorMode == ZOOMING_OUT)) {
            // int xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor int yS =
            // getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

            ((ViewJFrameImage) frame).updateFrame(getZoomMagnitudeX(mouseEvent.getButton() == MouseEvent.BUTTON3),
                                                  getZoomMagnitudeY(mouseEvent.getButton() == MouseEvent.BUTTON3), xS,
                                                  yS);

            if (mouseEvent.isShiftDown() == false) {
                cursorMode = DEFAULT;
                setCursor(MipavUtil.defaultCursor);
            }

            return;
        }

        try {
            mousePressedPaint(mouseEvent);

            if ((cursorMode == WIN_REGION) && (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK)) {
                String newValue = JOptionPane.showInputDialog(frame, "Enter new size for windowed region:",
                                                              String.valueOf(windowedRegionSize));

                try {

                    if (newValue != null) {
                        windowedRegionSize = Integer.parseInt(newValue);
                    }
                } catch (NumberFormatException nfe) {
                    MipavUtil.displayError("Invalid size entered for windowed region.");
                }
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.mousePressed");
            setCursorMode(DEFAULT);

            return;
        }
    }


    /**
     * A mouse event. This function sets up and draws the VOI according to the mode.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
    	
    	//calling garbage collect here to clean up any memory used while getting the LPS coordinates
    	 System.gc();
    	
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        if (wasDragging) {
            wasDragging = false;
            return;
        }

        if (modifyFlag == false) {
            return;
        }

        int xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        int yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || (yS < 0) || (yS >= imageActive.getExtents()[1])) {
            return;
        }

        if (cursorMode != MOVE) {
            setPixelInformationAtLocation(xS, yS);
        }

       
        
        // clicking with the right mouse button in a regular image frame updates the image's
        // tri-image frame (if one is open) to show that point in all of the components
        if ((mouseEvent.getModifiers() & InputEvent.BUTTON2_MASK) != 0) {
            ViewJFrameTriImage triFrame = imageActive.getTriImageFrame();

            if (triFrame != null) {
                triFrame.setSlicesFromFrame(xS, yS, slice);
            }
        }

        if (cursorMode == POINT_VOI) { }
        else if (cursorMode == POLYLINE_SLICE_VOI) { }
        else if (cursorMode == ANNOTATION) { }
        else if (cursorMode == LEVELSET) { }
        else if (cursorMode == RECTANGLE) { }
        else if (cursorMode == RECTANGLE3D) { }
        else if (cursorMode == ELLIPSE) { }
        else if (cursorMode == LINE) { }
        else if (cursorMode == PROTRACTOR) { }
        else if (cursorMode == NEW_POINT) { }
        else if (cursorMode == DELETE_POINT) { }
        else if (cursorMode == PAINT_CAN) {
            xPG = (short) xS;
            yPG = (short) yS;
            zPG = (short) slice;

            if (imageActive.isColorImage()) {
                int index = 4 * (yS + imageActive.getExtents()[0] + xS);
                seedValR = imageBufferActive[index + 1];
                seedValG = imageBufferActive[index + 2];
                seedValB = imageBufferActive[index + 3];
                regionGrow((short) xS, (short) yS, (short) slice, seedValR, seedValG, seedValB, null, true);
            } else {
                seedVal = imageBufferActive[(yS * imageActive.getExtents()[0]) + xS];
                regionGrow((short) xS, (short) yS, (short) slice, seedVal, null, true);
            }

            imageActive.notifyImageDisplayListeners(null, true);

        } else if (cursorMode == PAINT_VASC) {
            int index = xS + (yS * imageActive.getExtents()[0]);
            int z = MipavMath.round(((ViewJFramePaintVasculature) frame).getMIPZValue(index));
            float value = ((ViewJFramePaintVasculature) frame).imageBuffer[index + (z * imageActive.getSliceSize())];

            ((ViewJFrameImage) ((ViewJFramePaintVasculature) frame).parent).getComponentImage().regionGrow((short) xS,
                                                                                                           (short) yS,
                                                                                                           (short) z,
                                                                                                           value, null,
                                                                                                           true);
            ((ViewJFrameImage) ((ViewJFramePaintVasculature) frame).parent).getComponentImage().setRegionGrowVars((short)
                                                                                                                  xS,
                                                                                                                  (short)
                                                                                                                  yS,
                                                                                                                  (short)
                                                                                                                  z,
                                                                                                                  value);
            imageActive.notifyImageDisplayListeners(null, true);
        } else if (cursorMode == QUICK_LUT) {
            int wS, hS;

            xS = MipavMath.round(voiHandler.getRubberband().getBounds().x / (getZoomX() * resolutionX));
            yS = MipavMath.round(voiHandler.getRubberband().getBounds().y / (getZoomY() * resolutionY));
            wS = MipavMath.round(voiHandler.getRubberband().getBounds().width / (getZoomX() * resolutionX));
            hS = MipavMath.round(voiHandler.getRubberband().getBounds().height / (getZoomY() * resolutionY));

            if (imageA.isColorImage() == false) {

                if (imageA == imageActive) {
                    this.quickLUT(xS, wS, yS, hS, imageBufferA, imageA, LUTa);
                    imageActive.notifyImageDisplayListeners(LUTa, true);
                } else if ((imageB != null) && (imageActive == imageB)) {
                    this.quickLUT(xS, wS, yS, hS, imageBufferB, imageB, LUTb);
                    imageActive.notifyImageDisplayListeners(LUTb, true);
                }
            } else { // RGB image

                if (imageA == imageActive) {
                    this.quickRGB(xS, wS, yS, hS, imageBufferA, imageA, RGBTA);
                    imageActive.notifyImageDisplayListeners(true, 1, RGBTA);
                } else if ((imageBufferB != null) && (imageB != null) && (imageB == imageActive)) {
                    this.quickRGB(xS, wS, yS, hS, imageBufferB, imageB, RGBTB);
                    imageActive.notifyImageDisplayListeners(true, 1, RGBTB);
                }
            }
            
            if ((getActiveImage().isColorImage()) && (getActiveImage().getHistoRGBFrame() != null)) {
                getActiveImage().getHistoRGBFrame().update();
            } else if (getActiveImage().getHistoLUTFrame() != null) {
                getActiveImage().getHistoLUTFrame().update();
            }
            
            if (!((mouseEvent.isShiftDown() == true) || Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                setCursorMode(DEFAULT);
            }
        } else if (cursorMode == DEFAULT) {
            intensityLabel = false;
            paintComponent(getGraphics());
        }

        // reset mousePressIsShiftDown for next mouse click
        mousePressIsShiftDown = false;

    } // end mouseReleased()


    /**
     * ************************************************************************ ************************** Mouse Wheel
     * Events *************************.************************************************************************
     *
     * @param  mouseWheelEvent  DOCUMENT ME!
     */
    public void mouseWheelMoved(MouseWheelEvent mouseWheelEvent) {
        int wheelRotation = mouseWheelEvent.getWheelRotation();

        if (frame instanceof ViewJFrameImage) {

            if (wheelRotation < 0) {

                if (imageActive.getNDims() > 2) {

                    if (mouseWheelEvent.isShiftDown()) {

                        if (((ViewJFrameImage) frame).getImageA().getNDims() == 3) {
                            ((ViewJFrameImage) frame).setShiftDown(true);
                        }
                    }

                    ((ViewJFrameImage) frame).incSlice();
                } else {
                    ((ViewJFrameImage) frame).updateFrame(getZoomX() * 2.0f, getZoomY() * 2.0f);
                }
            } else {

                if (imageActive.getNDims() > 2) {

                    if (mouseWheelEvent.isShiftDown()) {

                        if (((ViewJFrameImage) frame).getImageA().getNDims() == 3) {
                            ((ViewJFrameImage) frame).setShiftDown(true);
                        }
                    }

                    ((ViewJFrameImage) frame).decSlice();
                } else {
                    ((ViewJFrameImage) frame).updateFrame(getZoomX() / 2.0f, getZoomY() / 2.0f);
                }
            }
        }
    }

    /**
     * Open the user defined LUT table.
     */
    public void openUDLUT() {
        String fName = "userdefine.lut";
        String dName = Preferences.getPreferencesDir();

        loadOnlyLUTFrom(true, fName, dName, false);

        // load the transfer function
        String fName2 = "userdefineTF.fun";

        loadUDTransferFunction(fName2, dName);

        if (imageActive == imageA) {
            setLUTa(getLUTa());
        } else {
            setLUTb(getLUTb());
        }

    }

    public String getRGBPaintComponents() {
    	return this.rgbString;
    }
    
    public void setRGBPaintComponents(String rgb) {
    	this.rgbString = rgb;
    }
    
    /**
     * Paints the image and calls drawSelf for all VOIs.
     *
     * @param  graphics  graphics
     */
    public void paintComponent(Graphics graphics) {

        if (this instanceof ViewJComponentRegistration) {

            // System.err.println("Paint component ViewJComponent Registration in EditImage");
        }

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

                int width = Math.round(zoomX * imageDim.width * resolutionX);
                int height = Math.round(zoomY * imageDim.height * resolutionY);

                if ((offscreenImage == null) || (offscreenImage.getWidth(null) != width) ||
                        (offscreenImage.getHeight(null) != height)) {
                    offscreenImage = createImage(width, height);
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

            if ((paintImageBuffer == null) || (paintImageBuffer.length != (imageDim.width * imageDim.height))) {
                paintImageBuffer = new int[imageDim.width * imageDim.height]; // make the buffer that will hold the
                                                                              // paint
            } else {
                Arrays.fill(paintImageBuffer, 0); // ensure erasure of old image, otherwise ghosting occurs
            }

            Rectangle visibleRect = getVisibleRect();
            offscreenGraphics2d.setClip(visibleRect); // clip graphics to visible area on screen - this saves rendering
                                                      // time

            // build the paint image that will be blended on-screen
            makePaintImage(paintImageBuffer, paintBitmap, slice, frame, (imageExtents.length < 3));

            if (Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)) {
                makePaintBitmapBorder(paintImageBuffer, paintBitmap, slice, frame);
            }

            if (!(this instanceof ViewJComponentRegistration)) {
                voiHandler.paintSolidVOIinImage(offscreenGraphics2d);
            }


            if (memImageA == null) { // create imageA if it hasn't already been created
                memImageA = new MemoryImageSource(imageDim.width, imageDim.height, pixBuffer, 0, imageDim.width);
                img = createImage(memImageA);
            } else {
                memImageA.newPixels(pixBuffer, ColorModel.getRGBdefault(), 0, imageDim.width);
                img.flush();
            }

            int zoomedWidth = Math.round(zoomX * img.getWidth(this) * resolutionX);
            int zoomedHeight = Math.round(zoomY * img.getHeight(this) * resolutionY);

            if ((interpMode == INTERPOLATE_A) || (interpMode == INTERPOLATE_BOTH)) {
                offscreenGraphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                                         RenderingHints.VALUE_INTERPOLATION_BILINEAR));
            }

            // draw image A
            offscreenGraphics2d.drawImage(img, 0, 0, zoomedWidth, zoomedHeight, 0, 0, img.getWidth(this),
                                          img.getHeight(this), null);

            if (imageB != null) {

                if ((interpMode == INTERPOLATE_B) || (interpMode == INTERPOLATE_BOTH)) {
                    offscreenGraphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                                             RenderingHints.VALUE_INTERPOLATION_BILINEAR));
                } else {
                    offscreenGraphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                                             RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
                }

                // if checkerboarding is OFF, this means blending should be enabled
                if (!(frame instanceof ViewJFrameLightBox)) {
                    cleanBuffer(BOTH);
                }

                if (!isCheckerboarded()) {
                    adjustOpacityFor000Color();
                    offscreenGraphics2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                                                                                1 - alphaBlend));

                } else {
                    makeCheckerboard();
                }

                if (memImageB == null) {
                    memImageB = new MemoryImageSource(imageDim.width, imageDim.height, pixBufferB, 0, imageDim.width);
                    imgB = createImage(memImageB);
                } else {
                    memImageB.newPixels(pixBufferB, ColorModel.getRGBdefault(), 0, imageDim.width);
                    imgB.flush();
                }

                // draw image B
                offscreenGraphics2d.drawImage(imgB, 0, 0, zoomedWidth, zoomedHeight, 0, 0, imgB.getWidth(this),
                                              imgB.getHeight(this), null);

            }

            memImageA = new MemoryImageSource(imageDim.width, imageDim.height, paintImageBuffer, 0, imageDim.width);

            Image paintImage = createImage(memImageA); // the image representing the paint mask


            // change rendering hint back from BILINEAR to nearest neighbor so that
            // all other painting will not be in interpolated mode
            offscreenGraphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                                     RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));

            offscreenGraphics2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f));

            offscreenGraphics2d.drawImage(paintImage, 0, 0, zoomedWidth, zoomedHeight, 0, 0, img.getWidth(this),
                                          img.getHeight(this), null);

            if ((cursorMode == PAINT_VOI) ||
                    ((cursorMode == ERASER_PAINT) && ((lastMouseX != OUT_OF_BOUNDS) || (lastMouseY !=
                                                                                            OUT_OF_BOUNDS)))) {

                // this method repaints the paint brush cursor without repainting the entire image
                repaintPaintBrushCursorFast(offscreenGraphics2d);
            }

            if (!(this instanceof ViewJComponentRegistration)) {
                voiHandler.drawVOIs(offscreenGraphics2d); // draw all VOI regions
            }

            drawImageText(offscreenGraphics2d); // draw image text, i.e. slice number

            if ((cursorMode == WIN_REGION) && ((lastMouseX != OUT_OF_BOUNDS) || (lastMouseY != OUT_OF_BOUNDS)) &&
                    (shiftDown == false)) {

                if ((interpMode == INTERPOLATE_B) || (interpMode == INTERPOLATE_BOTH)) {
                    offscreenGraphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                                             RenderingHints.VALUE_INTERPOLATION_BILINEAR));
                }

                if ((lastWinRegionSlice != slice) || (cleanImageB == null)) {
                    cleanBuffer(IMAGE_B);

                    MemoryImageSource memImageSource = new MemoryImageSource(imageDim.width, imageDim.height,
                                                                             cleanImageBufferB, 0, imageDim.width);

                    cleanImageB = createImage(memImageSource);
                }

                super.paintWindowComponent(offscreenGraphics2d, lastMouseX, lastMouseY, windowedRegionSize,
                                           windowedRegionSize, getZoomX(), cleanImageB);

                lastWinRegionSlice = slice;
            } else if ((cursorMode == MAG_REGION) && ((lastMouseX != OUT_OF_BOUNDS) || (lastMouseY != OUT_OF_BOUNDS))) {
                paintMagComponent(offscreenGraphics2d);
            } else if (cursorMode == DEFAULT) {

                if (!(this instanceof ViewJComponentSingleRegistration) &&
                		!(frame instanceof ViewJFrameLightBox)) {

                    if (intensityLabel) {

                        // display intensity values on screen
                        repaintImageIntensityLabelFast(offscreenGraphics2d);
                    }
                }
            }

            if (onTop) {

                // paint the on-top notifier for the user when this component is on the top of the user-interface
                offscreenGraphics2d.setColor(toppedColor);
                offscreenGraphics2d.drawRect(visibleRect.x, visibleRect.y, visibleRect.width - 1,
                                             visibleRect.height - 1);
            }

            graphics.drawImage(offscreenImage, 0, 0, null);

            if (offscreenGraphics2d != null) {
                offscreenGraphics2d.dispose();
                offscreenGraphics2d = null;
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImge.paintComponent.");
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    /**
     * Paints the image and calls drawSelf for all VOIs and also resizes the image if it is too big for printer.
     *
     * @param  tx  x translation
     * @param  ty  y translation
     * @param  g   graphics
     */
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
                                       imageActive.getFileInfo(0).getResolutions(),
                                       imageActive.getFileInfo(0).getUnitsOfMeasure(), slice, orientation, g);
            }
        }

        if (cursorMode == LEVELSET) {
            g.setColor(Color.yellow);
            g.drawPolygon(voiHandler.getZoomedLevelSetPolygon());
            // g.drawPolygon(zoomPolygon(rbLevelSet.getLevelSetPolygon(), getZoomX(), getZoomY()));
        }

        if (voiHandler.getOverlayOn()) {
            voiHandler.showOverlay(g);
        }
    }

    /**
     * Paints a magnified window over the image centered about the cursor.
     *
     * @param  graphics2d  graphics component
     */
    public void paintMagComponent(Graphics2D graphics2d) {
        int xNew = lastMouseX;
        int yNew = lastMouseY;
        int width = MAGR_WIDTH;
        int height = MAGR_HEIGHT;
        float mag = ((shiftDown == false) ? MAGR_MAG : getZoomX());
        int imageType = imageActive.getType();
        int imageXDim = imageActive.getExtents()[0];
        double minIntensity = getActiveImage().getMin();
        double maxIntensity = getActiveImage().getMax();

        int xNewO, yNewO;
        int x1, y1, xw1, yh1;
        int x2, y2;

        if (zoomX >= 2) {

            while (((Math.round(width / zoomX) - (width / zoomX)) != 0) ||
                       ((Math.round(width / zoomX / 2.0f) - (width / zoomX / 2.0f)) != 0)) {
                width++;
            }
        }

        height = width;

        xNew = (int) (((int) (xNew / (float) zoomX) * zoomX) + 0.5);
        yNew = (int) (((int) (yNew / (float) zoomY) * zoomY) + 0.5);

        int sIWidth = (int) (width / mag);
        int sIHeight = (int) (height / mag);

        if (img != null) {
            xNewO = xNew - (int) (0.5f * width);
            yNewO = yNew - (int) (0.5f * height);

            int sX = (int) (xNew / zoomX);
            int sY = (int) (yNew / zoomY);

            if ((sX - (int) (sIWidth / 2)) < 0) {
                return;
            }

            if ((sY - (int) (sIHeight / 2)) < 0) {
                return;
            }

            // Draw zoomed portion of window
            x2 = sX - (int) (sIWidth / 2);
            x1 = xNewO;
            xw1 = width + xNewO;
            y2 = sY - (int) (sIHeight / 2);
            y1 = yNewO;
            yh1 = height + yNewO;

            if ((interpMode == INTERPOLATE_A) || (interpMode == INTERPOLATE_BOTH)) {
                graphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                                RenderingHints.VALUE_INTERPOLATION_BILINEAR));
            } else {
                graphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                                RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
            }

            graphics2d.drawImage(img, x1, y1, xw1, yh1, x2, y2, sX + (int) (sIWidth / 2), sY + (int) (sIHeight / 2),
                                 this);

            if ((imageB != null) && (imgB != null)) {

                if ((interpMode == INTERPOLATE_B) || (interpMode == INTERPOLATE_BOTH)) {
                    graphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                                    RenderingHints.VALUE_INTERPOLATION_BILINEAR));
                } else {
                    graphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                                    RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
                }

                if (!isCheckerboarded()) {
                    graphics2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1 - alphaBlend));
                }

                graphics2d.drawImage(imgB, x1, y1, xw1, yh1, x2, y2, sX + (int) (sIWidth / 2),
                                     sY + (int) (sIHeight / 2), this);

                graphics2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f));
            }

            graphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                            RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));

            graphics2d.setColor(Color.red.darker());
            graphics2d.drawRect(xNewO, yNewO, width - 1, height - 1);

            graphics2d.setColor(textColor);
            graphics2d.setFont(MipavUtil.font10);

            if (zoomX >= 1.0) {

                if (((imageDim.height - 10) > 0) && (sliceString != null)) {
                    graphics2d.drawString(Float.toString(mag) + "x", xNewO + 5, yNewO + height - 5);
                }
            }

            float offsetY = 0;
            int startX = sX - (int) (sIWidth / 2);
            int startY = sY - (int) (sIHeight / 2);
            int endX = sX + (int) (sIWidth / 2);
            int endY = sY + (int) (sIHeight / 2);
            int pix;
            int osX, osY;

            float xwidth = (float) width / (endX - startX);
            float yheight = (float) height / (endX - startX);

            int fontHeight = graphics2d.getFontMetrics(graphics2d.getFont()).getHeight();
            int minStrWidth = graphics2d.getFontMetrics(graphics2d.getFont()).stringWidth(Integer.toString((int)
                                                                                                           minIntensity));
            int maxStrWidth = graphics2d.getFontMetrics(graphics2d.getFont()).stringWidth(Integer.toString((int)
                                                                                                           maxIntensity));

            if (minStrWidth > maxStrWidth) {
                maxStrWidth = minStrWidth;
            }

            int maxCharWidth = graphics2d.getFontMetrics(graphics2d.getFont()).charWidth('8');
            int maxFracDigs = (((int) (xwidth) - maxStrWidth) / maxCharWidth) - 2;
            NumberFormat nf = NumberFormat.getNumberInstance();

            if (maxFracDigs > 1) {
                nf.setMaximumFractionDigits(maxFracDigs);
            } else {
                nf.setMaximumFractionDigits(1);
            }

            if ((((imageType == ModelImage.FLOAT) || (imageType == ModelImage.DOUBLE) ||
                      (imageType == ModelImage.COMPLEX) || (imageType == ModelImage.ARGB) ||
                      (imageType == ModelImage.ARGB_USHORT)) &&
                     ((maxStrWidth < (xwidth - 1 - (2 * maxCharWidth))) && (fontHeight < (yheight - 1)))) ||
                    (((imageType != ModelImage.FLOAT) && (imageType != ModelImage.DOUBLE) &&
                          (imageType != ModelImage.COMPLEX) && (imageType != ModelImage.ARGB) &&
                          (imageType != ModelImage.ARGB_USHORT)) &&
                         ((maxStrWidth < (xwidth - 1)) && (fontHeight < (yheight - 1))))) {

                if (showMagIntensity) {

                    for (int y = startY; y < endY; y++) {
                        float offsetX = 0;

                        for (int x = startX; x < endX; x++) {

                            pix = (y * imageXDim) + x;

                            if ((pix >= 0) && (pix < imageBufferActive.length)) {

                                if ((imageType == ModelImage.FLOAT) || (imageType == ModelImage.DOUBLE) ||
                                        (imageType == ModelImage.COMPLEX) || (imageType == ModelImage.ARGB) ||
                                        (imageType == ModelImage.ARGB_USHORT)) {
                                    sliceString = nf.format(imageBufferActive[pix]);
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
            BitSet tempSet = (BitSet) paintBrushPrevious.clone();
            Dimension tempDim = (Dimension) paintBrushDimPrevious.clone();

            int prevWidth = paintImagePrevious.getWidth();
            int prevHeight = paintImagePrevious.getHeight();


            BufferedImage tempBImage = new BufferedImage(prevWidth, prevHeight, paintImagePrevious.getType());

            for (int i = 0; i < prevWidth; i++) {

                for (int j = 0; j < prevHeight; j++) {
                    tempBImage.setRGB(i, j, paintImagePrevious.getRGB(i, j));
                }
            }

            int curWidth = paintImage.getWidth();
            int curHeight = paintImage.getHeight();
            BufferedImage tempBImageCurrent = new BufferedImage(curWidth, curHeight, paintImage.getType());

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

            int width = paintImage.getWidth();
            int height = paintImage.getHeight();

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
     * @param  str  the string to prepend to message containing region growth statistics
     */
    public void regionGrow(String str) {

        if (seedPaintBitmap != null) {
            regionGrow(xPG, yPG, zPG, seedVal, str, false);
        }
    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between the the bounds which are also supplied. Used on black and white images.
     *
     * <p>when click <code>false</code>, adds points in the newly grown region which weren't in the old one remove
     * points which were in the old region but aren't in the new one (and which weren't in the region painted before the
     * last click), otherwise, the regions are simply added into the new set.</p>
     *
     * @param  x      x coordinate of the seed point
     * @param  y      y coordinate of the seed point
     * @param  z      z coordinate of the seed point
     * @param  value  Intensity value at the seed point
     * @param  str    String to start line with
     * @param  click  whether this region grow was initiated by a click on the image
     */
    public void regionGrow(short x, short y, short z, float value, String str, boolean click) {
        this.regionGrow(x, y, z, value, imageActive, str, click);
    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between the the bounds which are also supplied. Used for black and white images.
     *
     * <p>When click is <code>false</code>, adds points in the newly grown region which weren't in the old one remove
     * points which were in the old region but aren't in the new one (and which weren't in the region painted before the
     * last click), otherwise, the regions are simply added into the new set.</p>
     *
     * @param  x           x coordinate of the seed point
     * @param  y           y coordinate of the seed point
     * @param  z           z coordinate of the seed point
     * @param  value       Intensity value at the seed point
     * @param  image       the image to perform the region grow in
     * @param  leadString  the string to append to the region grow output
     * @param  click       whether this region grow was initiated by a click on the image
     */
    public void regionGrow(short x, short y, short z, float value, ModelImage image, String leadString, boolean click) {
        Cursor cursor = getCursor();

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

        if ((fuzzyThreshold == -2.0f) || (sizeLimit == -2) || (maxDistance == -2)) {
            return;
        }

        try {
            AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(image, 1.0f, 1.0f);

            regionGrowAlgo.setRunningInSeparateThread(false);

            if (image.getType() == ModelStorageBase.BOOLEAN) {
                less = 0;
                more = 0;
            }

            if (image.getNDims() == 2) {
                count = regionGrowAlgo.regionGrow2D(seedPaintBitmap, new Point(saveX, saveY), fuzzyThreshold, useVOI,
                                                    displayFuzzy, growDialog, saveValue - less, saveValue + more,
                                                    sizeLimit, maxDistance, variableThresholds);
                showRegionInfo(count, leadString);
            } else if ((image.getNDims() == 3) || (image.getNDims() == 4)) {
                CubeBounds regionGrowBounds;

                if (((JDialogPaintGrow) growDialog).boundsConstrained()) {

                    // constrain bounds to cropping volume
                    regionGrowBounds = ((ViewJFrameTriImage) frame).getBoundedVolume();
                } else {

                    // bounds are not constrained by cropping volume, use image extents as bounds
                    regionGrowBounds = new CubeBounds(imageExtents[0], 0, imageExtents[1], 0, imageExtents[2], 0);
                }

                count = regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3Ds(saveX, saveY, saveZ), fuzzyThreshold,
                                                    useVOI, displayFuzzy, growDialog, saveValue - less,
                                                    saveValue + more, sizeLimit, maxDistance, variableThresholds,
                                                    timeSlice, regionGrowBounds);
                showRegionInfo(count, leadString);
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.regionGrow");
        }

        if (!click) {

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
     * between the the bounds which are also supplied. Used on color images.
     *
     * <p>when click <code>false</code>, adds points in the newly grown region which weren't in the old one remove
     * points which were in the old region but aren't in the new one (and which weren't in the region painted before the
     * last click), otherwise, the regions are simply added into the new set.</p>
     *
     * @param  x       x coordinate of the seed point
     * @param  y       y coordinate of the seed point
     * @param  z       z coordinate of the seed point
     * @param  valueR  Red intensity value at the seed point
     * @param  valueG  Green intensity value at the seed point
     * @param  valueB  Blue intensity value at the seed point
     * @param  str     String to start line with
     * @param  click   whether this region grow was initiated by a click on the image
     */
    public void regionGrow(short x, short y, short z, float valueR, float valueG, float valueB, String str,
                           boolean click) {
        this.regionGrow(x, y, z, valueR, valueG, valueB, imageActive, str, click);
    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between the the bounds which are also supplied. Used for color images.
     *
     * <p>When click is <code>false</code>, adds points in the newly grown region which weren't in the old one remove
     * points which were in the old region but aren't in the new one (and which weren't in the region painted before the
     * last click), otherwise, the regions are simply added into the new set.</p>
     *
     * @param  x           x coordinate of the seed point
     * @param  y           y coordinate of the seed point
     * @param  z           z coordinate of the seed point
     * @param  valueR      Red value at the seed point
     * @param  valueG      Green value at the seed point
     * @param  valueB      Blue value at the seed point
     * @param  image       the image to perform the region grow in
     * @param  leadString  the string to append to the region grow output
     * @param  click       whether this region grow was initiated by a click on the image
     */
    public void regionGrow(short x, short y, short z, float valueR, float valueG, float valueB, ModelImage image,
                           String leadString, boolean click) {
        Cursor cursor = getCursor();

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

        if ((fuzzyThreshold == -2.0f) || (sizeLimit == -2) || (maxDistance == -2)) {
            return;
        }

        try {
            AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(image, 1.0f, 1.0f);

            regionGrowAlgo.setRunningInSeparateThread(false);

            if (image.getNDims() == 2) {
                count = regionGrowAlgo.regionGrow2D(seedPaintBitmap, new Point(saveX, saveY), fuzzyThreshold, useVOI,
                                                    displayFuzzy, growDialog, saveValueR - lessR, saveValueR + moreR,
                                                    saveValueG - lessG, saveValueG + moreG, saveValueB - lessB,
                                                    saveValueB + moreB, sizeLimit, maxDistance);
                showRegionInfo(count, leadString);
            } else if ((image.getNDims() == 3) || (image.getNDims() == 4)) {
                CubeBounds regionGrowBounds;

                if (((JDialogPaintGrow) growDialog).boundsConstrained()) {

                    // constrain bounds to cropping volume
                    regionGrowBounds = ((ViewJFrameTriImage) frame).getBoundedVolume();
                } else {

                    // bounds are not constrained by cropping volume, use image extents as bounds
                    regionGrowBounds = new CubeBounds(imageExtents[0], 0, imageExtents[1], 0, imageExtents[2], 0);
                }

                count = regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3Ds(saveX, saveY, saveZ), fuzzyThreshold,
                                                    useVOI, displayFuzzy, growDialog, saveValueR - lessR,
                                                    saveValueR + moreR, saveValueG - lessG, saveValueG + moreG,
                                                    saveValueB - lessB, saveValueB + moreB, sizeLimit, maxDistance,
                                                    timeSlice, regionGrowBounds);
                showRegionInfo(count, leadString);
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.regionGrow");
        }

        if (!click) {

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
     * @param  str  the string to prepend to message containing region growth statistics
     */
    public void regionGrowColor(String str) {

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
                } else if ((imageB != null) && (imageB == imageActive)) {
                    this.resetLUT(LUTb, imageB);
                }
                imageA.notifyImageDisplayListeners(null, false);

                if (imageB != null) {
                    imageB.notifyImageDisplayListeners(null, false);
                }
                
            } else { // RGB image

                if (imageA == imageActive) {
                    this.resetRGB(RGBTA);
                } else if ((imageB != null) && (imageB == imageActive)) {
                    this.resetRGB(RGBTB);
                }
                imageA.notifyImageDisplayListeners(true, 1, RGBTA);
                if (imageB != null) {
                	imageB.notifyImageDisplayListeners(true, 1, RGBTB);
                }
            }

        } catch (OutOfMemoryError error) {
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
     * @param  filename  filename to save LUT as
     * @param  dirName   directory to save LUT to
     */
    public void saveOnlyLUTAs(String filename, String dirName) {
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

            fileHistoLUT.writeLUT();

        } catch (IOException error) {
            MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
        }

    } // end saveLUTAs()

    /**
     * Save user defined LUT table.
     */
    public void saveUDLUT() {

        // save both the LUT and the transfer functions
        String fName = "userdefine.lut";
        String dName = Preferences.getPreferencesDir();

        saveOnlyLUTAs(fName, dName);

        // save the transfer function
        String fName2 = "userdefineTF.fun";

        saveUDTransferFunction(fName2, dName);

        if (imageActive == imageA) {
            setLUTa(getLUTa());
        } else {
            setLUTb(getLUTb());
        }
    }

    /**
     * Sets the active image for drawing VOIs.
     *
     * @param  active  IMAGE_A or IMAGE_B
     */
    public void setActiveImage(int active) {
        winLevelSet = false;
        voiHandler.setActiveVOI_ID(active);

        if ((active == IMAGE_A) || (imageB == null)) {
            imageActive = imageA;
            imageBufferActive = imageBufferA;

            if (!paintBitmapSwitch) {
                paintBitmap = imageA.getMask();
            }
        } else if (active == IMAGE_B) {
            imageActive = imageB;
            imageBufferActive = imageBufferB;
        }

        m_kPatientSlice.setActiveImage(imageActive);
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed
     */
    public void setAlphaBlend(int value) {

        if ((value >= 0) && (value <= 100)) {
            alphaBlend = value / 100.0f;
            alphaPrime = 1 - alphaBlend;
        }
    }

    /**
     * The frame in which the image(s) is displayed, allocates the memory and uses this method to pass the references to
     * the buffers.
     *
     * @param  imgBufferA  storage buffer used to display image A
     * @param  imgBufferB  storage buffer used to display image B
     * @param  pixBuff     storage buffer used to build a displayable image
     * @param  pixBuffB    storage buffer used to build a displayable imageB for the window
     */
    public void setBuffers(float[] imgBufferA, float[] imgBufferB, int[] pixBuff, int[] pixBuffB) {
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
     * @param  rowCheckers     int # of rows
     * @param  columnCheckers  int # of columns
     */
    public void setCheckerboard(int rowCheckers, int columnCheckers) {
        this.nRowCheckers = rowCheckers;
        this.nColumnCheckers = columnCheckers;

        ViewJFrameBase vjfb = ((ViewJComponentEditImage) this).getFrame();
        ViewControlsImage vci = vjfb.getControls();

        if (vci != null) {

            if ((rowCheckers < 1) || (columnCheckers < 1)) {
                vci.setAlphaSliderEnabled(true);
            } else {
                vci.setAlphaSliderEnabled(false);
            }
        }
    }

    /**
     * Changes the Crosshair cursor to be either the default crosshair or a pre-existing gif.
     *
     * @param  curs  the new crosshair cursor
     */
    public void setCrosshairCursor(Cursor curs) {
        this.crosshairCursor = curs;
    }

    /**
     * Switches modes based on the variable mode. Sets voiHandler.getRubberband() activity and the cursor.
     *
     * @param  mode  the integer mode
     */
    public void setCursorMode(int mode) {
        this.cursorMode = mode;
        voiHandler.setMode(mode);
    }

    /**
     * Sets whether a fuzzy image is displayed.
     *
     * @param  val  whether to show the fuzzy connectedness image
     */
    public void setDisplayFuzzy(boolean val) {
        this.displayFuzzy = val;
    }

    /**
     * Enables or disables the component for modification.
     *
     * @param  flag  true = modify, and false = locked
     */
    public void setEnabled(boolean flag) {
        modifyFlag = flag;
        voiHandler.getRubberband().setActive(flag);
    }

    /**
     * Sets the frameControls.
     *
     * @param  controls  the controls with color and opacity information
     */
    public void setFrameControls(ViewControlsImage controls) {
        frameControls = controls;
    }

    // When the apply button is pressed, JDialogPaintGrow sets the following 7 parameters used in regionGrow.
    /**
     * Sets whether fuzzy connectedness is used and the fuzzy threshold.
     *
     * @param  val  the fuzzy connectedness threshold value
     */
    public void setFuzzyThreshold(float val) {
        this.fuzzyThreshold = val;
    }

    /**
     * Sets the RegionGrowDialog for this class (usually used to set it to null).
     *
     * @param  dialog  the paint grow dialog
     */
    public void setGrowDialog(RegionGrowDialog dialog) {
        growDialog = dialog;
    }

    /**
     * Sets the hasThreshold1 for setPaintBuffers.
     *
     * @param  hasThreshold1  whether the paint buffer has a threshold1
     */
    public void setHasThreshold1(boolean hasThreshold1) {
        m_kPatientSlice.setHasThreshold1(hasThreshold1);
    }

    /**
     * Sets the hasThreshold2 for setPaintBuffers.
     *
     * @param  hasThreshold2  whether the paint buffer has a threshold2
     */
    public void setHasThreshold2(boolean hasThreshold2) {
        m_kPatientSlice.setHasThreshold2(hasThreshold2);
    }

    /**
     * Changes the color used to highlight the currently on-top image.
     *
     * @param  c  the new color to use.
     */
    public void setHighlightColor(Color c) {
        toppedColor = c;
    }

    /**
     * Sets component's ImageA. assumes dimensionality same as image B's for now.
     *
     * @param  image  imageA
     */
    public void setImageA(ModelImage image) {
        imageA = image;
        m_kPatientSlice.setImageA(image);
        setZoom(1, 1); // sets zoom
    }

    /**
     * Sets component's ImageB. !!!!!! assumes dimensionality same as image A's for now will fix soon.
     *
     * @param  image  imageB
     */
    public void setImageB(ModelImage image) {
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
     * @param  buffer  image buffer to put in the buffer for Image B
     */
    public void setImageBufferB(float[] buffer) {
        imageBufferB = buffer;
    }

    /**
     * Sets the colocalize image.
     *
     * @param  imageColocalize  the colocalization image
     */
    public void setImageColocalize(ModelImage imageColocalize) {
        m_kPatientSlice.setImageColocalize(imageColocalize);
    }

    /* ********************************************************************** */
    /* ****************************** Accessors ***************************** */
    /* ********************************************************************** */

    /**
     * DOCUMENT ME!
     *
     * @param  imageExtents  int[]
     */
    public void setImageExtents(int[] imageExtents) {
        this.imageExtents = imageExtents;
    }

    /**
     * Sets the intensityDropper intensity.
     *
     * @param  intensityDropper  the dropper intensity
     */
    public void setIntensityDropper(float intensityDropper) {
        this.intensityDropper = intensityDropper;
    }

    /**
     * Sets less for regionGrow.
     *
     * @param  val  lower region grow delta
     */
    public void setLess(float val) {
        this.less = val;
    }

    /**
     * Sets lessB for regionGrow.
     *
     * @param  val  lower region grow delta
     */
    public void setLessB(float val) {
        this.lessB = val;
    }

    /**
     * Sets lessG for regionGrow.
     *
     * @param  val  lower region grow delta
     */
    public void setLessG(float val) {
        this.lessG = val;
    }

    /**
     * Sets lessR for regionGrow.
     *
     * @param  val  lower region grow delta
     */
    public void setLessR(float val) {
        this.lessR = val;
    }

    /**
     * Sets the log magnitude display flag.
     *
     * @param  flag  if true display log of the Magnitude of the complex image
     */
    public void setLogMagDisplay(boolean flag) {
        logMagDisplay = flag;
    }

    /**
     * accessor that sets the model LUT for the imageA.
     *
     * @param  LUT  the model LUT
     */
    public void setLUTa(ModelLUT LUT) {
        LUTa = LUT;
        m_kPatientSlice.setLUTa(LUT);
    }

    /**
     * Sets the model LUTb for the imageB.
     *
     * @param  LUT  the model LUT
     */
    public void setLUTb(ModelLUT LUT) {
        LUTb = LUT;
        m_kPatientSlice.setLUTb(LUT);
    }

    /**
     * Sets maxDistance for regionGrow.
     *
     * @param  val  the maximum region grow distance
     */
    public void setMaxDistance(int val) {
        this.maxDistance = val;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setModifyFlag(boolean flag) {
        modifyFlag = flag;
    }

    /**
     * Sets more for regionGrow.
     *
     * @param  val  upper region grow delta
     */
    public void setMore(float val) {
        this.more = val;
    }

    /**
     * Sets moreB for regionGrow.
     *
     * @param  val  upper region grow delta
     */
    public void setMoreB(float val) {
        this.moreB = val;
    }

    /**
     * Sets moreG for regionGrow.
     *
     * @param  val  upper region grow delta
     */
    public void setMoreG(float val) {
        this.moreG = val;
    }

    /**
     * Sets moreR for regionGrow.
     *
     * @param  val  upper region grow delta
     */
    public void setMoreR(float val) {
        this.moreR = val;
    }

    /**
     * If true do not getMask on a setActiveImage command so as to keep the mask from the old active image.
     *
     * @param  paintBitmapSwitch  if true do not getMask on a setActiveImage command
     */
    public void setPaintBitmapSwitch(boolean paintBitmapSwitch) {
        this.paintBitmapSwitch = paintBitmapSwitch;
    }

    /**
     * Switches modes based on the variable mode. Sets the number of pixels to be drawn when painting.
     *
     * @param  paintBrushSize  the integer mode
     */
    public void setPaintBrushSize(int paintBrushSize) {
        this.paintBrushSize = paintBrushSize;
    }

    /**
     * Sets the paint mask.
     *
     * @param  mask  the new paint mask
     */
    public void setPaintMask(BitSet mask) {
        paintBitmap = mask;
    }


    /**
     * Prints ModelImage information at the mouse location.
     *
     * @param  xS  mouse x location
     * @param  yS  mouse y location
     */
    public void setPixelInformationAtLocation(int xS, int yS) {

        try {
            String str;

            if ((imageActive.getOrigin()[0] != 0) || (imageActive.getOrigin()[1] != 0) ||
                    ((imageActive.getNDims() > 2) && (imageActive.getOrigin()[2] != 0))) {
                String[] values = getScannerPositionLabels(imageActive, new Point3Df(xS, yS, slice));

                if (values != null) {

                    if (imageActive.isColorImage()) {
                        str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  R:  " +
                              String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 1]) +
                              "  G:  " +
                              String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 2]) +
                              "  B:  " +
                              String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 3]) +
                              " Position: " + values[0] + " " + values[1] + " " + values[2];
                    } else {
                        str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  Intensity:  " +
                              String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]) +
                              " Position: " + values[0] + " " + values[1] + " " + values[2];
                    }

                    frame.setMessageText(str);
                } else {

                    if (imageActive.isColorImage()) {
                        str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  R:  " +
                              String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 1]) +
                              "  G:  " +
                              String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 2]) +
                              "  B:  " +
                              String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 3]);
                    } else {
                        str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  Intensity:  " +
                              String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]);
                    }

                    frame.setMessageText(str);
                }
            } else {

                if (imageActive.isColorImage() == true) {
                    str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  R:  " +
                          String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 1]) +
                          "  G:  " +
                          String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 2]) +
                          "  B:  " +
                          String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 3]);
                    frame.setMessageText(str);
                } else {
                    str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  Intensity:  " +
                          String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]);
                    frame.setMessageText(str);
                }
            }
        } catch (ArrayIndexOutOfBoundsException error) {
            frame.setMessageText("  X: " + String.valueOf((xS + 1)) + " Y: " + String.valueOf((yS + 1)));
        }
    }

    /**
     * Sets the variables used to remember the point where the last region grow was started from.
     *
     * @param  x    x coordinate
     * @param  y    y coordinate
     * @param  z    z coordinate
     * @param  val  intensity at the point (x,y,z)
     */
    public void setRegionGrowVars(short x, short y, short z, float val) {
        xPG = x;
        yPG = y;
        zPG = z;
        seedVal = val;
    }

    // The following 2 functions set the RGB tables for ARGB images A and B.
    /**
     * Sets the RGB table for ARGB image A.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTA(ModelRGB RGBT) {
        RGBTA = RGBT;
        m_kPatientSlice.setRGBTA(RGBT);
    }

    /**
     * Sets the RGB table for ARGB image B.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTB(ModelRGB RGBT) {
        RGBTB = RGBT;
        m_kPatientSlice.setRGBTB(RGBT);
    }

    /**
     * Sets whether to show intensity in mag. box
     *
     * @param  flag  whether to show intensity in mag. box
     */
    public void setShowMagIntensity(boolean flag) {
        showMagIntensity = flag;
    }

    /**
     * Sets whether the slice number will be shown in the lower left hand corner of the image.
     *
     * @param  flag  whether to display the slice number
     */
    public void setShowSliceNum(boolean flag) {
        showSliceNumber = flag;
    }

    /**
     * Sets sizeLimit for regionGrow.
     *
     * @param  val  the maximum region grow size
     */
    public void setSizeLimit(int val) {
        this.sizeLimit = val;
    }

    /**
     * Sets the slice of the image.
     *
     * @param  _slice  image slice to be displayed
     */
    public void setSlice(int _slice) {
        slice = _slice;
        m_kPatientSlice.updateSlice(slice);
    }

    /**
     * Sets the string painted on the lower left.
     *
     * @param  str  str that is painted on the lower left of image
     */
    public void setStringOverride(String str) {
        stringOverride = str;
    }

    /**
     * Sets the booleans for using thresholds in setColorPaintBuffers.
     *
     * @param  useRedThreshold    whether to threshold the red paint buffer
     * @param  useGreenThreshold  whether to threshold the green paint buffer
     * @param  useBlueThreshold   whether to threshold the blue paint buffer
     */
    public void setThresholdColors(boolean useRedThreshold, boolean useGreenThreshold, boolean useBlueThreshold) {
        m_kPatientSlice.setThresholdColors(useRedThreshold, useGreenThreshold, useBlueThreshold);
    }

    /**
     * Sets the thresholds.
     *
     * @param  threshold1  the first threshold
     * @param  threshold2  the second threshold
     */
    public void setThresholds(float threshold1, float threshold2) {
        m_kPatientSlice.setThresholds(threshold1, threshold2);
    }

    /**
     * Sets the time slice of the image.
     *
     * @param  _slice  the time slice to be displayed
     */
    public void setTimeSlice(int _slice) {
        timeSlice = _slice;
    }

    /**
     * Sets whether a selected VOI is used to calculate the initial variance.
     *
     * @param  val  whether to use the selected VOI to get the initial variance for the fuzzy regionGrow
     */
    public void setUseVOI(boolean val) {
        this.useVOI = val;
    }

    /**
     * If true varies thresholds with region growth.
     *
     * @param  variableThresholds  boolean
     */
    public void setVariableThresholds(boolean variableThresholds) {
        this.variableThresholds = variableThresholds;
    }

    /**
     * For generating the display of 1 or 2 images.
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, boolean forceShow) {
        return show(tSlice, zSlice, null, null, forceShow, interpMode);
    }

    /**
     * Shows the image and the VOI(s).
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   _LUTa      LUTa - to change to new LUT for imageA else null
     * @param   _LUTb      LUTb - to change to new LUT for imageB else null
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow) {
        return show(tSlice, zSlice, _LUTa, _LUTb, forceShow, interpMode);
    }

    /**
     * Shows the image and the VOI(s).
     *
     * @param   tSlice      t (time) slice to show
     * @param   zSlice      z slice to show
     * @param   _LUTa       LUTa - to change to new LUT for imageA else null
     * @param   _LUTb       LUTb - to change to new LUT for imageB else null
     * @param   forceShow   forces this method to import image and recalculate java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow, int interpMode) {

        if (interpMode > -1) {
            setInterpolationMode(interpMode);
        }

        m_kPatientSlice.setLUTa(_LUTa);
        m_kPatientSlice.setLUTb(_LUTb);
        m_kPatientSlice.updateSlice(zSlice);

        if (cleanImageBufferB == null) {
            cleanImageBufferB = new int[imageExtents[0] * imageExtents[1]];
        }

        if (m_kPatientSlice.showUsingOrientation(tSlice, cleanImageBufferA, cleanImageBufferB, forceShow, false, 0,
                                                     false)) {
            cleanImageB = null;
            cleanBuffer(IMAGE_A);
            cleanBuffer(IMAGE_B);

            slice = zSlice;
            setSliceString(String.valueOf(slice + 1));
            paintComponent(getGraphics());

            return true;
        } else {
            return false;
        }
    } // end of show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow)

    /**
     * Resets the buffer to 0s and displays a blank image.
     *
     * @return  boolean to indicate that the show was successful
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
     * @param  count       Number of pixels (voxels)
     * @param  leadString  the string to prepend to message containing region growth statistics
     */
    public void showRegionInfo(int count, String leadString) {
        float volume;
        float area;
        int measure;

        try {
            String str = new String();

            FileInfoBase[] fileInfo = imageActive.getFileInfo();

            if (imageActive.getNDims() == 2) {
                area = count * imageActive.getResolutions(0)[0] * imageActive.getResolutions(0)[1];
                str = imageActive.getFileInfo(0).getAreaUnitsOfMeasureStr();

                if (leadString != null) {
                    frame.getUserInterface().setDataText(leadString + " region grow: pixels = " + count +
                                                         "\t  area = " + area + str + "\n");
                } else {
                    frame.getUserInterface().setDataText("Region grow: pixels = " + count + "\t  area = " + area + str +
                                                         "\n");
                }

            } else {
                volume = count * imageActive.getResolutions(0)[0] * imageActive.getResolutions(0)[1] *
                             imageActive.getResolutions(0)[2];

                str = imageActive.getFileInfo(0).getVolumeUnitsOfMeasureStr();

                if (leadString != null) {
                    frame.getUserInterface().setDataText(leadString + " region grow: pixels = " + count +
                                                         "\t  volume = " + volume + str + "\n");
                } else {
                    frame.getUserInterface().setDataText("Region grow: pixels = " + count + "\t  volume = " + volume +
                                                         str + "\n");
                }
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.showRegionInfo");
        }
    }

    /**
     * Opens a JDialogStatistics to allow computation ofROI statistics.
     */
    public void showStatisticsCalculator() {

        if (imageStatList == null) {

            if ((imageActive.getVOIs() != null) && (imageActive.getVOIs().size() != 0)) {
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
        int pEnd = paintBitmap.size();

        for (int p = 0; p < pEnd; p++) {

            if (paintBitmapBU.get(p)) {
                paintBitmap.set(p);
            } else {
                paintBitmap.clear(p);
            }
        }

        if (growDialog != null) {
            growDialog.notifyPaintListeners((growDialog instanceof JDialogPaintGrow), false, paintBitmap);
        } else {
            imageActive.notifyImageDisplayListeners(null, true);
        }
    }

    /**
     * Causes the image to update its paint bit mask and redisplay itself.
     *
     * @param  region    new paint region bit set
     * @param  backup    whether to save the previous paint mask to allow the update to be un-done
     * @param  isGrower  whether this paint listener is the one that did the region grow
     */
    public void updatePaint(BitSet region, boolean backup, boolean isGrower) {

        if (isGrower) {

            if (backup) {
                paintBitmapBU = (BitSet) paintBitmap.clone();
            }

            paintBitmap = region;
        } else {

            // should only get method called if this component image is not the paint grow listener
            // who performed the region grow (ie - in the ViewJFramePaintVasculature)
            int z;
            int index;

            paintBitmap.clear();

            for (int i = 0; i < imageBufferActive.length; i++) {

                if (frame instanceof ViewJFramePaintVasculature) {
                    z = MipavMath.round(((ViewJFramePaintVasculature) frame).getMIPZValue(i));
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
        } catch (Exception e) { }

        Color brushColor = new Color(paintColor.getRed(), paintColor.getGreen(), paintColor.getBlue(), opacity);
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
     * @param  hilite  <code>true</code> will set this component to paint the 'on-top' high-light the next time it is
     *                 redrawn. <code>false</code>, of course, will not let the component paint the coloured rectangle
     *                 when repainted.
     */
    public void useHighlight(boolean hilite) {
        onTop = hilite;
    }

    /**
     * Calls dispose to dump this instance.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
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
     * @return  DOCUMENT ME!
     */
    protected int getHBrushSize() {
        return getBrushSize() / 2;
    }


    /**
     * DOCUMENT ME!
     *
     * @param   reverse  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected float getZoomMagnitudeX(boolean reverse) {
        return getZoomMagnitude(getZoomX(), reverse);
    }

    /**
     * DOCUMENT ME!
     *
     * @param   reverse  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected float getZoomMagnitudeY(boolean reverse) {
        return getZoomMagnitude(getZoomY(), reverse);
    }

    /**
     * When a mousePressed event is triggered and the mode is DROPPER_PAINT, ERASER_PAINT, PAINT_VOI, or MAG_REGION this
     * function is called. It is shared with the derived classes.
     *
     * @param  mouseEvent  the mouseEvent that triggered this function call.
     */
    protected void mousePressedPaint(MouseEvent mouseEvent) {
        int xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        int yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        int x = mouseEvent.getX();
        int y = mouseEvent.getY();

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || (yS < 0) || (yS >= imageActive.getExtents()[1])) {
            return;
        }

        if (cursorMode == DROPPER_PAINT) {

            if (imageActive.isColorImage() == true) {
                Color dropperColor = new Color((int)
                                               imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 1],
                                               (int)
                                               imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 2],
                                               (int)
                                               imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 3]);
                frame.getControls().getTools().setPaintColor(dropperColor);
            } else {
                intensityDropper = imageBufferActive[(yS * imageActive.getExtents()[0]) + xS];
                frame.getControls().getTools().setIntensityPaintName(String.valueOf((int) (intensityDropper)));
            }
        }

        if (cursorMode == ERASER_PAINT) {
            performPaint(mouseEvent, true);
            imageActive.notifyImageDisplayListeners();
        } else if (cursorMode == PAINT_VOI) {

            // backup paintBitmap to paintBitmapBU
            backupPaintBitmap();

            xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
            yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

            performPaint(mouseEvent, mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK);
            imageActive.notifyImageDisplayListeners();
        }

        if ((cursorMode == MAG_REGION) && (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK)) {

            if ((magSettings != null) && !magSettings.isVisible()) {
                magSettings.setWidthText((int) (frame.getSize().width * 0.25));
                magSettings.setVisible(true);
            }
        }
    }


    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    protected void performPaint(MouseEvent event) {
        performPaint(event, false);
    }

    /**
     * This method will set or clear the paint bitmap as the user paints on-screen.
     *
     * @param  mouseEvent  MouseEvent the mouseEvent associated with this paint action
     * @param  erase       boolean if true, paintBitmap is cleared (erased), otherwise paintBitmap is set (painted)
     */
    protected void performPaint(MouseEvent mouseEvent, boolean erase) {
    	
    	int xS = (int)((mouseEvent.getX() / (zoomX * resolutionX)) + 0.5);
    	
        int yS = (int)((mouseEvent.getY() / (zoomY * resolutionY)) + 0.5);

        int brushSize = getBrushSize();
        int hBrushSize = getHBrushSize();

        if (paintBrush != null) {

            int brushXDim = paintBrushDim.width;
            int brushYDim = paintBrushDim.height;

            int counter = 0;
            int offset = imageActive.getSliceSize() * slice;

            int xDim = imageActive.getExtents()[0];
            int yDim = imageActive.getExtents()[1];

            for (int height = 0; height < brushYDim; height++) {

                for (int width = 0; width < brushXDim; width++, counter++) {

                    if (paintBrush.get((height * brushYDim) + width)) {

                        if (((xS + width) < xDim) && ((yS + height) < yDim)) {
                            int st = ((yS + height) * imageActive.getExtents()[0]) + (xS + width);

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
                    int st = (j * imageActive.getExtents()[0]) + i;

                    if (erase == true) {
                        paintBitmap.clear(offset + st);
                    } else {
                        paintBitmap.set(offset + st);
                    }
                }
            }

            if (imageActive.getType() == ModelStorageBase.COMPLEX) {
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
                        int st = (j * imageActive.getExtents()[0]) + i;

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


        // imageActive.notifyImageDisplayListeners();
    }

    /**
     * processDefaultMouseDrag performs the mouseDrag operations when in DEFAULT mode. The default operation when the
     * right mouse button is held down is the window-level image adjustment, and setting the ModelImage information at
     * the pixel location. This function in shared with the deried classes.
     *
     * @param  mouseEvent  the mouse event
     * @param  xS          the mouse x location on screen
     * @param  yS          the mouse y location on screen
     */
    protected void processDefaultMouseDrag(MouseEvent mouseEvent, int xS, int yS) {
        int xDim = imageActive.getExtents()[0];
        int yDim = imageActive.getExtents()[1];
        String str;

        try {

            if (cursorMode == DEFAULT) {

                if ((mouseEvent.getModifiers() & MouseEvent.BUTTON3_MASK) != 0) {

                    // Dragging the mouse with the right mouse button pressed
                    // increases the window when going from left to right.
                    // Dragging the mouse with the right mouse button pressed
                    // increases the level when going from down to up.
                    m_kWinLevel.setAlpha(alphaBlend);

                    float fX = xS / (float) xDim;
                    float fY = yS / (float) yDim;

                    m_kWinLevel.updateWinLevel(fX, fY, !winLevelSet, m_kPatientSlice.getActiveLookupTable(),
                                               imageActive);

                    if (!winLevelSet) {
                        setCursor(MipavUtil.winLevelCursor);
                        winLevelSet = true;
                    }
                } // if ((mouseEvent.getModifiers() & MouseEvent.BUTTON3_MASK) != 0)

                // check is left mouse button was pressed...if so...we need to show intensity values
                if ((mouseEvent.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK) == InputEvent.BUTTON1_DOWN_MASK) {
                    intensityLabel = true;
                    paintComponent(getGraphics());
                }


            } // if (mode == DEFAULT))

            setPixelInformationAtLocation(xS, yS);
        } catch (ArrayIndexOutOfBoundsException error) {
            str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1);
            frame.setMessageText(str);

            if ((mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                frame.getUserInterface().setDataText("\n" + str);
            }
        }
    }

    /**
     * Update the voi color.
     *
     * @param  voiColor  the new voi color
     * @param  voiUID    the last voi id
     */
    protected void updateVOIColor(Color voiColor, int voiUID) {

        if (frame instanceof ViewJFrameImage) {
            ((ViewJFrameImage) frame).getControls().setVOIColor(voiColor);
            ((ViewJFrameImage) frame).setLastVOI_UID(voiUID);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  id      DOCUMENT ME!
     * @param  voiUID  DOCUMENT ME!
     */
    protected void updateVOIColor(int id, int voiUID) {

        if (frame instanceof ViewJFrameImage) {
            ((ViewJFrameImage) frame).getControls().setVOIColor(id);
            ((ViewJFrameImage) frame).setLastVOI_UID(voiUID);
        }
    }

    /**
     * >>>>>>> .r533 The purpose of this method is to examine both LUTs to determine if they are zero-based, that is, if
     * they map values of zero to the color R=0, G=0, B=0. If the LUT does, in fact, do this, then this method ensures
     * that those pixels are completely transparent so that no blending takes place for those pixels.
     */
    private void adjustOpacityFor000Color() {

        if (pixBufferB != null) { // no need to execute if there is no image B

            ViewJFrameBase vjfb = (ViewJFrameBase) frame;

            ModelLUT modelLUT = vjfb.getLUTa();

            if (modelLUT != null) {
                Color zeroIndexColor = modelLUT.getColor(0); // get the color at index 0 of LUT a

                // test to see if the color is R == 0, G == 0, B == 0
                boolean zeroIndexColorIs000;

                modelLUT = vjfb.getLUTb();

                if (modelLUT != null) {
                    zeroIndexColor = modelLUT.getColor(0);

                    // test to see if the color is R == 0, G == 0, B == 0
                    zeroIndexColorIs000 = ((zeroIndexColor.getRed() == 0) && (zeroIndexColor.getGreen() == 0) &&
                                               (zeroIndexColor.getBlue() == 0));

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
            
            if(imageA.isColorImage() && imageB != null) {
            		int temp2, temp3;
                    for (int i = 0; i < pixBufferB.length; i++) {
                        int temp = pixBufferB[i];
                        if(!zeroToOneLUTAdj) {
                        	temp2 = temp & 0x00ffffff; // apply mask. temp will equal zero if the pixel should be
                                                  // transparent
                        	if (temp2 == 0) {
                        		pixBufferB[i] = pixBufferB[i] & 0x00ffffff; // make pixel transparent
                        	}
                        }
                        else {
                        	temp3 = temp & 0xffffffff;
                        	
                        	if(temp3 == 0) {
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

        int pEnd = paintBitmap.size();

        for (int p = 0; p < pEnd; p++) {

            if (paintBitmap.get(p)) {
                paintBitmapBU.set(p);
            } else {
                paintBitmapBU.clear(p);
            }
        }
    }


    /**
     * DOCUMENT ME!
     *
     * @param  buffer  DOCUMENT ME!
     */
    private void cleanBuffer(int buffer) {

        if (buffer == IMAGE_A) {

            if ((pixBuffer == null) || (pixBuffer.length != cleanImageBufferA.length)) {
                pixBuffer = new int[cleanImageBufferA.length];
            }

            System.arraycopy(cleanImageBufferA, 0, pixBuffer, 0, cleanImageBufferA.length);

        } else if (buffer == IMAGE_B) {

            if (cleanImageBufferB != null) {

                if ((pixBufferB == null) || (pixBufferB.length != cleanImageBufferB.length)) {
                    pixBufferB = new int[cleanImageBufferB.length];
                }

                System.arraycopy(cleanImageBufferB, 0, pixBufferB, 0, cleanImageBufferB.length);
            }
        } else if (buffer == BOTH) {

            if ((pixBuffer == null) || (pixBuffer.length != cleanImageBufferA.length)) {
                pixBuffer = new int[cleanImageBufferA.length];
            }

            if ((pixBufferB == null) || (pixBufferB.length != cleanImageBufferB.length)) {
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
     * @param  offscreenGraphics2d  Graphics2D graphics context to draw in
     */
    private void drawImageText(Graphics2D offscreenGraphics2d) {

        if ((((int) ((zoomX * imageDim.width) + 0.5) - 40) > 0) && (sliceString != null) && (showSliceNumber == true)) {
            offscreenGraphics2d.setFont(MipavUtil.font12);
            offscreenGraphics2d.setColor(Color.black);
            offscreenGraphics2d.drawString(sliceString, 5, (int) ((zoomY * resolutionY * imageDim.height) + 0.5f) - 5);
            offscreenGraphics2d.drawString(sliceString, 5, (int) ((zoomY * resolutionY * imageDim.height) + 0.5f) - 6);
            offscreenGraphics2d.drawString(sliceString, 5, (int) ((zoomY * resolutionY * imageDim.height) + 0.5f) - 4);
            offscreenGraphics2d.drawString(sliceString, 6, (int) ((zoomY * resolutionY * imageDim.height) + 0.5f) - 5);
            offscreenGraphics2d.drawString(sliceString, 4, (int) ((zoomY * resolutionY * imageDim.height) + 0.5f) - 5);
            offscreenGraphics2d.setColor(Color.white);
            offscreenGraphics2d.drawString(sliceString, 5, (int) ((zoomY * resolutionY * imageDim.height) + 0.5f) - 5);
        }
    }

    /**
     * Returns the magnitude zoom depending on the zoom mode.
     *
     * @param   zoom     the zoom factor (x or y)
     * @param   reverse  for reverse zoom
     *
     * @return  the calculated zoom factor
     */
    private float getZoomMagnitude(float zoom, boolean reverse) {

        if (Preferences.is(Preferences.PREF_ZOOM_LINEAR)) {

            if (cursorMode == ZOOMING_IN) {

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

            if (cursorMode == ZOOMING_IN) {

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


    /**
     * Loads the User Defined transfer function into the lut.
     *
     * @param  fName  String file name
     * @param  dName  String directory name
     */
    private void loadUDTransferFunction(String fName, String dName) {
        FileHistoLUT fileHistoLUT;
        ModelLUT lut = null;
        String temp = "temp.temp";

        if (imageActive == imageA) {
            lut = this.getLUTa();
        } else {
            lut = this.getLUTb();
        }

        try {
            fileHistoLUT = new FileHistoLUT(temp, dName, lut);
            fileHistoLUT.readUDTransferFunction(fName, dName);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    // When the apply or close button is pressed, JDialogCheckerBoard sets the following 2 parameters used.
    /**
     * Sets the number of checkers in a row and a column.
     */
    private void makeCheckerboard() {
        int xSep, ySep, xMod, yMod;
        int xDim, yDim;
        int xIndex, yIndex;
        int x, y;
        int[] xStart;
        int[] yStart;
        boolean doA;

        if ((imageB == null) || (pixBufferB == null)) {
            return;
        }

        ViewJFrameBase vjfb = ((ViewJComponentEditImage) this).getFrame();
        ViewControlsImage vci = vjfb.getControls();

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

            if (y == yDim) { }
            else if (y == yStart[yIndex]) {
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

            if (y == yDim) { }
            else if (y == yStart[yIndex]) {
                yIndex++;
                y = yStart[yIndex];
                yIndex++;
            }
        } // for (y = yStart[1]; y < yDim;)
    }

    /**
     * DOCUMENT ME!
     *
     * @param  xS           DOCUMENT ME!
     * @param  wS           DOCUMENT ME!
     * @param  yS           DOCUMENT ME!
     * @param  hS           DOCUMENT ME!
     * @param  imageBuffer  DOCUMENT ME!
     * @param  image        DOCUMENT ME!
     * @param  LUT          DOCUMENT ME!
     */
    private void quickLUT(int xS, int wS, int yS, int hS, float[] imageBuffer, ModelImage image, ModelLUT LUT) {
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];

        float min = Float.MAX_VALUE;
        float max = -100000000;
        float[] x = new float[4];
        float[] y = new float[4];
        float[] z = new float[4];
        Dimension dim = new Dimension(256, 256);
        float minImage, maxImage;

        for (int j = yS; j < (yS + hS); j++) {

            for (int i = xS; i < (xS + wS); i++) {

                if (imageBuffer[(j * xDim) + i] > max) {
                    max = imageBuffer[(j * xDim) + i];
                }

                if (imageBuffer[(j * xDim) + i] < min) {
                    min = imageBuffer[(j * xDim) + i];
                }
            }
        }

        if (image.getType() == ModelStorageBase.UBYTE) {
            minImage = 0;
            maxImage = 255;
        } else if (image.getType() == ModelStorageBase.BYTE) {
            minImage = -128;
            maxImage = 127;
        } else {
            minImage = (float) image.getMin();
            maxImage = (float) image.getMax();
        }

        // Set LUT min max values;
        x[0] = minImage;
        x[1] = min;
        x[2] = max;
        x[3] = maxImage;

        y[0] = dim.height - 1;
        y[1] = dim.height - 1;
        y[2] = 0;
        y[3] = 0;


        LUT.getTransferFunction().importArrays(x, y, 4);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  xS           DOCUMENT ME!
     * @param  wS           DOCUMENT ME!
     * @param  yS           DOCUMENT ME!
     * @param  hS           DOCUMENT ME!
     * @param  imageBuffer  DOCUMENT ME!
     * @param  image        DOCUMENT ME!
     * @param  RGB          DOCUMENT ME!
     */
    private void quickRGB(int xS, int wS, int yS, int hS, float[] imageBuffer, ModelImage image, ModelRGB RGB) {
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];

        float[] minC = { Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE };
        float[] maxC = { -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE };

        float min = Float.MAX_VALUE;
        float max = -100000000;
        float[][] x = new float[3][4];
        float[][] y = new float[3][4];
        float[][] z = new float[3][4];
        Dimension dim = new Dimension(256, 256);

        for (int j = yS; j < (yS + hS); j++) {

            for (int i = xS; i < (xS + wS); i++) {

                for (int c = 0; c < 3; c++) {

                    if (imageBuffer[(j * xDim * 4) + (i * 4) + c + 1] > maxC[c]) {
                        maxC[c] = imageBuffer[(j * xDim * 4) + (i * 4) + c + 1];
                    }

                    if (imageBuffer[(j * xDim * 4) + (i * 4) + c + 1] < minC[c]) {
                        minC[c] = imageBuffer[(j * xDim * 4) + (i * 4) + c + 1];
                    }
                }
            }
        }

        max = Math.max(maxC[0], maxC[1]);
        max = Math.max(maxC[2], max);

        for (int i = 0; i < 3; i++) {

            // Set LUT min max values;
            // if (imageA.isColorImage() == true) {
            if (image.getType() == ModelStorageBase.ARGB) {
                x[i][1] = minC[i];
                x[i][2] = maxC[i];
            } else {
                x[i][1] = minC[i] * 255 / max;
                x[i][2] = maxC[i] * 255 / max;
            }

            x[i][0] = 0;
            x[i][3] = 255;

            y[i][0] = dim.height - 1;
            y[i][1] = dim.height - 1;
            y[i][2] = 0;
            y[i][3] = 0;
        }

        RGB.getRedFunction().importArrays(x[0], y[0], 4);
        RGB.getGreenFunction().importArrays(x[1], y[1], 4);
        RGB.getBlueFunction().importArrays(x[2], y[2], 4);
        RGB.makeRGB(-1);
    }


    /**
     * Repaints the image intensity label.
     *
     * @param  graphics2d  Graphics2D the graphics context to draw in
     */
    private void repaintImageIntensityLabelFast(Graphics2D graphics2d) {

        if ((graphics2d == null) || (lastMouseX == OUT_OF_BOUNDS) || (lastMouseY == OUT_OF_BOUNDS)) {
            return;
        }

        int xS = getScaledX(lastMouseX); // zoomed x.  Used as cursor
        int yS = getScaledY(lastMouseY); // zoomed y.  Used as cursor

        // position where label should go
        int x = (MipavMath.round(xS * zoomX) + 15);
        int y = (MipavMath.round(yS * zoomY) + 35);


        // positions needed to determine when label should be flipped over when getting too close to edge
        int wC = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().width;
        int hC = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().height;
        int xC = MipavMath.round(xS * zoomX);
        int yC = MipavMath.round(yS * zoomY);

        Color textColor = null;
        Color backgroundColor = null;

        // set color of label text
        if (Preferences.getProperty(Preferences.PREF_INTENSITY_LABEL_COLOR) != null) {
            String prefColor = Preferences.getProperty(Preferences.PREF_INTENSITY_LABEL_COLOR);
            textColor = MipavUtil.extractColor(prefColor);
        } else {
            textColor = Color.yellow;
        }

        if (Preferences.getProperty(Preferences.PREF_INTENSITY_LABEL_BACKGROUND_COLOR) != null) {
            String prefColor = Preferences.getProperty(Preferences.PREF_INTENSITY_LABEL_BACKGROUND_COLOR);
            backgroundColor = MipavUtil.extractColor(prefColor);
        } else {
            backgroundColor = Color.black;
        }

        // we will only display up to 3 decimal places
        DecimalFormat df = new DecimalFormat("0.0##");

        graphics2d.setColor(backgroundColor);

        if (imageActive.isColorImage()) {
            String red = df.format(new Float(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 1]).doubleValue());
            String green = df.format(new Float(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 2]).doubleValue());
            String blue = df.format(new Float(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 3]).doubleValue());

            if (((wC - xC) > 170) && ((hC - yC) > 40)) {
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x + 1, y);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 1, y);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x, y - 1);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x, y + 1);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x, y);
            } else if (((wC - xC) <= 170) && ((hC - yC) > 40)) {
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 159, y);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 161, y);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 160, y - 1);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 160, y + 1);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 160, y);
            } else if (((wC - xC) <= 170) && ((hC - yC) <= 40)) {
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 159, y - 40);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 161, y - 40);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 160, y - 41);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 160, y - 39);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 160, y - 40);
            } else if (((wC - xC) > 170) && ((hC - yC) <= 40)) {
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x + 1, y - 40);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x - 1, y - 40);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x, y - 41);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x, y - 39);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + red + "," +
                                      green + "," + blue, x, y - 40);
            }
        } else {
            String intensity = df.format(new Float(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]).doubleValue());

            if (((wC - xC) > 100) && ((hC - yC) > 50)) {
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x + 1,
                                      y);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 1,
                                      y);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x,
                                      y - 1);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x,
                                      y + 1);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x, y);
            } else if (((wC - xC) <= 100) && ((hC - yC) > 50)) {
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 79,
                                      y);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 81,
                                      y);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 80,
                                      y - 1);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 80,
                                      y + 1);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 80,
                                      y);
            } else if (((wC - xC) <= 100) && ((hC - yC) <= 50)) {
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 79,
                                      y - 40);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 81,
                                      y - 40);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 80,
                                      y - 41);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 80,
                                      y - 39);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x - 80,
                                      y - 40);
            } else if (((wC - xC) > 100) && ((hC - yC) <= 50)) {
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x,
                                      y - 40);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x,
                                      y - 40);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x,
                                      y - 41);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x,
                                      y - 39);

                graphics2d.setColor(textColor);
                graphics2d.drawString(String.valueOf(xS + 1) + "," + String.valueOf(yS + 1) + ":  " + intensity, x,
                                      y - 40);
            }
        }
    }

    /**
     * Repaints the paint brush cursor without repainting the entire image.
     *
     * @param  graphics2d  Graphics2D the graphics context to draw in
     */
    private void repaintPaintBrushCursorFast(Graphics2D graphics2d) {

        if ((graphics2d == null) || (lastMouseX == OUT_OF_BOUNDS) || (lastMouseY == OUT_OF_BOUNDS)) {
            return;
        }

        int xS = lastMouseX;
        int yS = lastMouseY;

        // yx, xz
        float factor = 1f;
        float factor2 = 1f;

        factor = resolutionX / resolutionY;

        if (factor < 1) {
            factor2 = 1f / factor;
            factor = 1f;
        }

        graphics2d.drawImage(paintImage.getScaledInstance(MipavMath.round(paintImage.getWidth() * zoomX * factor),
                                                          MipavMath.round(paintImage.getHeight() * zoomY * factor2), 0),
                             new AffineTransform(1f, 0f, 0f, 1f, xS, yS), null);
    }


    /**
     * DOCUMENT ME!
     *
     * @param  LUT    DOCUMENT ME!
     * @param  image  DOCUMENT ME!
     */
    private void resetLUT(ModelLUT LUT, ModelImage image) {
        float min, max;
        float[] x = new float[4];
        float[] y = new float[4];
        float[] z = new float[4];
        Dimension dim = new Dimension(256, 256);

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

        x[1] = (min + ((max - min) / 3.0f));
        y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);

        x[2] = (min + ((max - min) * 0.67f));
        y[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);

        x[3] = max;
        y[3] = 0;
        LUT.getTransferFunction().importArrays(x, y, 4);

    }

    /**
     * DOCUMENT ME!
     *
     * @param  RGBT  DOCUMENT ME!
     */
    private void resetRGB(ModelRGB RGBT) {
        float[] x = new float[4];
        float[] y = new float[4];
        float[] z = new float[4];
        Dimension dim = new Dimension(256, 256);

        // Set LUT min max values;
        x[0] = 0;
        y[0] = dim.height - 1;

        x[1] = 255 * 0.333f;
        y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);

        x[2] = 255 * 0.667f;
        y[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);

        x[3] = 255;
        y[3] = 0;

        if (RGBT == null) {
            int[] RGBExtents = new int[2];
            RGBExtents[0] = 4;
            RGBExtents[1] = 256;
            RGBT = new ModelRGB(RGBExtents);
        }

        RGBT.getRedFunction().importArrays(x, y, 4);
        RGBT.getGreenFunction().importArrays(x, y, 4);
        RGBT.getBlueFunction().importArrays(x, y, 4);
        RGBT.makeRGB(-1);
    }

    /**
     * Saves the User Defined transfer function (remapped 0->1).
     *
     * @param  fName  String file name
     * @param  dName  String directory name
     */
    private void saveUDTransferFunction(String fName, String dName) {
        FileHistoLUT fileHistoLUT;
        ModelLUT lut = null;
        String temp = "temp.temp";

        if (imageActive == imageA) {
            lut = this.getLUTa();
        } else {
            lut = this.getLUTb();
        }

        try {
            fileHistoLUT = new FileHistoLUT(temp, dName, lut);
            fileHistoLUT.writeUDTransferFunction(fName, dName);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    
    /**
     * 
     * @param zeroToOneLUTAdj
     */
	public void setZeroToOneLUTAdj(boolean zeroToOneLUTAdj) {
		this.zeroToOneLUTAdj = zeroToOneLUTAdj;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isZeroToOneLUTAdj() {
		return zeroToOneLUTAdj;
	}


	
	
}
