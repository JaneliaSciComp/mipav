package gov.nih.mipav.view;


import gov.nih.mipav.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;

//import static gov.nih.mipav.view.MipavUtil.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
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

    /** Thin paint brush, 1 pixel wide. */
    public static final int thinPaint = 0;

    /** Medium paint brush, 4 pixel wide. */
    public static final int medPaint = 1;

    /** Thick paint brush, 8 pixel wide. */
    public static final int thickPaint = 2;

    /** The thickest paint brush, 16 pixels wide. */
    public static final int thickestPaint = 3;

    /** DOCUMENT ME! */
    public static final int EXPONENTIAL = 0;

    /** DOCUMENT ME! */
    public static final int LINEAR = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public JDialogCheckerBoard checkerDialog = null;

    /** DOCUMENT ME! */
    public RegionGrowDialog growDialog = null;

    /** used in paint to indicate intensity painted into the image. */
    public float intensityDropper = 1.0f;

    /** default = 120 pixels. */
    public int MAGR_HEIGHT = 120;

    /** magnification value of magnifier window -- default = 4.0. */
    public float MAGR_MAG = 4f;

    /** default = 120 pixels. */
    public int MAGR_WIDTH = 120;

    /** DOCUMENT ME! */
    public int zoomMode = EXPONENTIAL;

    /** Set to true when all contours of a VOI are active. */
    protected boolean allActive = false;

    /** alphaBlending values for compositing two images. */
    protected float alphaBlend = 0.5f;

    /** DOCUMENT ME! */
    protected float alphaPrime = 0.5f;

    /** Buffer used to store ARGB images of the image presently being displayed. */
    protected int[] cleanImageBufferA = null;

    /** DOCUMENT ME! */
    protected int[] cleanImageBufferB = null;

    /** DOCUMENT ME! */
    protected int columnCheckers = -1;

    /** Crosshair cursor that can be changed per user preference (in Mipav Options) */
    protected Cursor crosshairCursor = MipavUtil.crosshairCursor;

    /** DOCUMENT ME! */
    protected boolean displayFuzzy = false;

    /** Frame where the component image is displayed. */
    protected ViewJFrameBase frame;

    /** Controls having color and opacity. */
    protected ViewControlsImage frameControls = null;

    /**
     * regionGrow parameters fuzzyConnectedness is not used if fuzzyThreshold is less than 0. When fuzzyConnectedness is
     * used, fuzzyThreshold ranges from 0 to 1.
     */
    protected float fuzzyThreshold = -1;

    /** used when graphing a VOI. */
    //protected float[] graphImgBuff;

    /** color of grid. */
    protected Color gridColor = Color.lightGray;

    /** Flag to indicate if NEI grid overlay should be displayed. */
    protected boolean gridOverlayOn = false;

    /** spacing of the grid (horizontal) in terms of resolution. */
    protected float gridSpacingX = 20f;

    /** spacing of the grid (vertical) in terms of resolution. */
    protected float gridSpacingY = 20f;

    /** if number/lettering should be displayed for grid boxes */
    protected boolean gridLabelingOn = false;

    /** boolean to determine the orientation:
     * true is x-axis numbered
     * false is x-axis lettered
     */
    protected boolean gridLabelOrientation = true;


    /** true if image is known to be in patient orientation and is displayed in ViewJFrameTriImage. */
    protected boolean hasOrientation = false;

    /** in setPaintBuffers and setPaintBuffers zero out buffer values below threshold. */
    protected boolean hasThreshold1 = false;

    /** DOCUMENT ME! */
    protected boolean hasThreshold2 = false;

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

    /** DOCUMENT ME! */
    protected float[] imageBufferColocalize = null;

    /** DOCUMENT ME! */
    protected ModelImage imageColocalize = null;

    /** DOCUMENT ME! */
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

    /** DOCUMENT ME! */
    protected float level, window;

    /** Flag for displaying logMag. */
    protected boolean logMagDisplay = false;

    /** Lookup table for image A. */
    protected ModelLUT LUTa;

    /** Lookup table for image B. */
    protected ModelLUT LUTb;

    /** Remapped LUT buffer. */
    protected int[] lutBufferRemapped = null;

    /** DOCUMENT ME! */
    protected JDialogMagnificationControls magSettings;

    /** DOCUMENT ME! */
    protected float maxDistance = -1;

    /** DOCUMENT ME! */
    protected int[] maxExtents = new int[2];

    /** DOCUMENT ME! */
    protected float minImageWin, maxImageWin;

    /** DOCUMENT ME! */
    protected float minWin, maxWin;

    /** used to describe the cursor mode. */
    protected int mode;

    /** New Text VOI (annotation). */
    // private VOI newTextVOI = null;

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

    /** DOCUMENT ME! */
    protected int oldXS = 0;

    /** DOCUMENT ME! */
    protected int oldYS = 0;

    /** for the use of the user-notifier. */
    protected boolean onTop = false;

    /** DOCUMENT ME! */
    protected int[] orient; // Gives the orientation of each of 3 axes

    /** DOCUMENT ME! */
    protected int orientation = NA;

    /** DOCUMENT ME! */
    protected int OUT_OF_BOUNDS = -9999;

    /** DOCUMENT ME! */
    protected int lastMouseX = OUT_OF_BOUNDS; // used by the repaintPaintBrushCursorFast method

    /** DOCUMENT ME! */
    protected int lastMouseY = OUT_OF_BOUNDS; // used by the repaintPaintBrushCursorFast method

    /** Flag to indicate if DICOM overlay should be displayed. */
    protected boolean overlayOn = false;

    /** DOCUMENT ME! */
    protected BitSet paintBitmap;

    /** Backup of paint buffer for undo. */
    protected BitSet paintBitmapBU;

    /** if true do not getMask on a setActiveImage command so as to keep the mask from the old active image. */
    protected boolean paintBitmapSwitch = false;

    /** Paint brush size. */
    protected int paintBrushSize;

    /** DOCUMENT ME! */
    protected int[] pixBuffer = null;

    /** Buffer used to store ARGB image of the windowed imageB. */
    protected int[] pixBufferB = null;

    /**
     * The previous paint brush we were using when the user temporarily changes the paint brush size using keyboard
     * shortcuts (by pressing 1, 2, 3, 4).
     */
    protected int previousPaintBrush = -1;

    /** DOCUMENT ME! */
    protected float[] res = new float[2];

    /** DOCUMENT ME! */
    protected ModelRGB RGBTA;

    /** DOCUMENT ME! */
    protected ModelRGB RGBTB;

    /** Checkerboard display parameters. */
    protected int rowCheckers = -1; // a negative value indicates no checkerboarding

    /** Rubberband tool for VOIs. */
   // protected Rubberband rubberband;

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
    protected float threshold1;

    /** DOCUMENT ME! */
    protected float threshold2;

    /** DOCUMENT ME! */
    protected long time;

    /** DOCUMENT ME! */
    protected int timeSlice = 0;

    /** DOCUMENT ME! */
    protected int timeSliceA = 0;

    /** DOCUMENT ME! */
    protected int timeSliceB = 0;

    /** DOCUMENT ME! */
    protected Color toppedColor = ACTIVE_IMAGE_COLOR;



    /** DOCUMENT ME! */
    protected boolean useBlueThreshold = false;

    /** DOCUMENT ME! */
    protected boolean useGreenThreshold = false;

    /** DOCUMENT ME! */
    protected boolean useRedThreshold = false;

    /** DOCUMENT ME! */
    protected boolean useVOI = false;

    /** DOCUMENT ME! */
    protected boolean variableThresholds = false;

    /** Visible rectangle to draw topped. */
    protected Rectangle visRect;

    /** User invokes window and level adjustment with right mouse drag in DEFAULT mode. */
    protected boolean winLevelSet = false;

    /** X coord of seed point. */
    protected short xPG;

    /** DOCUMENT ME! */
    protected float[] xWin = new float[4];

    /** Y coord of seed point. */
    protected short yPG;

    /** DOCUMENT ME! */
    protected float[] yWin = new float[4];

    /** Z coord of seed point. */
    protected short zPG;

    /** DOCUMENT ME! */
    protected float[] zWin = new float[4];

    /** DOCUMENT ME! */
    private Image cleanImageB = null;

    /**
     * last slice the image was at when win region was ON. its necessary because otherwise, a new cleanImageB would have
     * to be created upon every repaint. should be initialized to a negative number
     */
    private int lastWinRegionSlice = -1;

    /** DOCUMENT ME! */
    private MemoryImageSource memImageA = null;

    /** DOCUMENT ME! */
    private MemoryImageSource memImageB = null;

    /** DOCUMENT ME! */
    private Image offscreenImage = null;

    /** DOCUMENT ME! */
    private int[] paintImageBuffer = null;

    /** DOCUMENT ME! */
    private boolean shiftDown = false;

    /** DOCUMENT ME! */
    private boolean showMagIntensity = false;

    /** boolean to determine if the mouse had been dragging before mouse release (for moving and setting active). */
    private boolean wasDragging = false;

    /** DOCUMENT ME! */
    private int windowedRegionSize = 100;


    /**
     * Handles all aspects of VOIs, including mouse responses, movements, VOI graphing etc
     */
    protected VOIHandler voiHandler = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  _frame           frame where image(s) will be displayed
     * @param  _imageA          Model of the image that will be displayed
     * @param  _LUTa            LUT used to display imageA
     * @param  imgBufferA       storage buffer used to display image A
     * @param  _imageB          Model of the image that will be displayed
     * @param  _LUTb            LUT used to display imageB
     * @param  imgBufferB       storage buffer used to display image B
     * @param  pixelBuffer      storage buffer used to build a displayable image
     * @param  zoom             initial magnification of image
     * @param  extents          initial display dimensions of the image
     * @param  logMagDisplay    display log magnitude of image
     * @param  _orientation     orientation of the image
     * @param  _hasOrientation  true if the image is known to be in axial orientation and is displayed in
     *                          ViewJFrameTriImage
     * @param  _orient          a 3 integer array containing the orientation of each axis
     */
    public ViewJComponentEditImage(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
                                   ModelImage _imageB, ModelLUT _LUTb, float[] imgBufferB, int[] pixelBuffer,
                                   float zoom, int[] extents, boolean logMagDisplay, int _orientation,
                                   boolean _hasOrientation, int[] _orient) {
        super(extents, _orientation, _hasOrientation, _orient);

        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;
        imageExtents = extents;

        orientation = _orientation;
        hasOrientation = _hasOrientation;
        orient = _orient;

        LUTa = _LUTa;
        LUTb = _LUTb;

        lutBufferRemapped = new int[1];

        if (imageA.isDicomImage()) {
            setOverlay(Preferences.is(Preferences.PREF_SHOW_DICOM_OVERLAYS));
        } else {
            setOverlay(Preferences.is(Preferences.PREF_SHOW_IMAGE_OVERLAYS));
        }

        showSliceNumber = (imageA.getNDims() > 2) && !(this instanceof ViewJComponentTriImage);

        // active image color: get preset
        if (Preferences.getProperty("ActiveImageColor") == null) {
            Preferences.setProperty("ActiveImageColor", MipavUtil.makeColorString(ACTIVE_IMAGE_COLOR));
        } else {
            this.setHighlightColor(MipavUtil.extractColor(Preferences.getProperty("ActiveImageColor")));
        }

        // Custom crosshair cursors
        if (Preferences.getProperty("CrosshairCursor") == null) {
            Preferences.setProperty("CrosshairCursor", "default");
        }

        String crosshairName = Preferences.getProperty("CrosshairCursor");

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

        mode = DEFAULT;
        paintBrushSize = thickestPaint;

        if (imgBufferA == null) {
            int bufferFactor = (imageA.isColorImage() ? 4 : 1);
            imgBufferA = new float[bufferFactor * imageA.getExtents()[axisOrder[0]] * imageA.getExtents()[axisOrder[1]]];
        }

        if ((imgBufferB == null) && (imageB != null)) {
            int bufferFactor = (imageB.isColorImage() ? 4 : 1);
            imgBufferB = new float[bufferFactor * imageB.getExtents()[axisOrder[0]] * imageB.getExtents()[axisOrder[1]]];
        }

        if (pixelBuffer == null) {
            pixelBuffer = new int[imageA.getExtents()[axisOrder[0]] * imageA.getExtents()[axisOrder[1]]];
        }

        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
        pixBuffer = pixelBuffer;

        paintBitmap = imageA.getMask();
        paintBitmapBU = imageA.getMaskBU();

        // borderDetector = new AlgorithmVOIExtractionPaint( imageActive );

        imageBufferActive = imageBufferA;
        this.logMagDisplay = logMagDisplay;

        setZoom(zoom, zoom);

        voiHandler = new VOIHandler(this);

        if ( ! (this instanceof ViewJComponentTriImage)) {
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

        if ((orientation == NA) || (orientation == AXIAL)) {
            res[0] = Math.abs(imageActive.getFileInfo(0).getResolutions()[axisOrder[0]]);
            res[1] = Math.abs(imageActive.getFileInfo(0).getResolutions()[axisOrder[1]]);

            if ((res[0] == 0.0f) || (res[1] == 0.0f)) {
                res[0] = 1.0f;
                res[1] = 1.0f;
            }

            maxExtents[0] = imageActive.getExtents()[axisOrder[0]];
            maxExtents[1] = imageActive.getExtents()[axisOrder[1]];
        } else if (orientation == CORONAL) {
            res[0] = Math.abs(imageActive.getFileInfo(0).getResolutions()[axisOrder[0]]);
            res[1] = Math.abs(imageActive.getFileInfo(0).getResolutions()[axisOrder[1]]);

            if ((res[0] == 0.0f) || (res[1] == 0.0f)) {
                res[0] = 1.0f;
                res[1] = 1.0f;
            }

            maxExtents[0] = imageActive.getExtents()[axisOrder[0]];
            maxExtents[1] = imageActive.getExtents()[axisOrder[1]];
        } else { // orientation == ZY
            res[0] = Math.abs(imageActive.getFileInfo(0).getResolutions()[axisOrder[0]]);
            res[1] = Math.abs(imageActive.getFileInfo(0).getResolutions()[axisOrder[1]]);

            if ((res[0] == 0.0f) || (res[1] == 0.0f)) {
                res[0] = 1.0f;
                res[1] = 1.0f;
            }

            maxExtents[0] = imageActive.getExtents()[axisOrder[0]];
            maxExtents[1] = imageActive.getExtents()[axisOrder[1]];
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------



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
        int slice[] = new int[1];
        slice[0]=0;

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
                maskAlgo.setActiveImage(false);
                maskAlgo.setProgressBarVisible(showProgressBar);
                maskAlgo.calcInPlace25DC(paintBitmap, fillColor, timeSlice);
            } else {
                if (imageA.getNDims() == 4){
                    // Build dialog 3D or 4D
                    JDialogMask3D4D dialog3D4D = new JDialogMask3D4D(frame, slice);
                    if (slice[0] == -1 ) timeSlice = -1;
                }

                if (slice[0] <= 0 ){
                    maskAlgo = new AlgorithmMask(imageA, intensityDropper, polarity, false);
                    maskAlgo.setActiveImage(false);
                    maskAlgo.setProgressBarVisible(showProgressBar);
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
                maskAlgo.setActiveImage(false);
                maskAlgo.setProgressBarVisible(showProgressBar);
                maskAlgo.calcInPlace25DC(paintBitmap, fillColor, timeSlice);
            } else {
                if (imageA.getNDims() == 4){
                    // Build dialog 3D or 4D
                    JDialogMask3D4D dialog3D4D = new JDialogMask3D4D(frame, slice);
                    if (slice[0] == -1 ) timeSlice = -1;
                }
                maskAlgo = new AlgorithmMask(imageB, intensityDropper, polarity, false);
                maskAlgo.setActiveImage(false);
                maskAlgo.setProgressBarVisible(showProgressBar);
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
        Color fillColor;
        ModelImage imageACopy = null, imageBCopy = null;
        int length;
        int colorFactor;
        double buffer[];
        double bufferI[];

        imageACopy = (ModelImage) imageA.clone();

        if (imageB != null) {
            imageBCopy = (ModelImage) imageB.clone();
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

            maskAlgo.setActiveImage(false);
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

            maskAlgo.setActiveImage(false);
            maskAlgo.calcInPlace25DShortMask((BitSet) paintBitmap.clone(), intensityDropper, timeSlice);
        } // not color

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
            if (imageACopy.getType() != ModelStorageBase.SHORT) {
                try {
                    if (imageACopy.isColorImage()) {
                        colorFactor = 4;
                    }
                    else {
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
                       imageACopy.importData(0, buffer, true);
                   }
                    else {
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
            return imageACopy.getImageName();
        } else {
            if (imageBCopy.getType() != ModelStorageBase.SHORT) {
                try {
                    if (imageBCopy.isColorImage()) {
                        colorFactor = 4;
                    }
                    else {
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
                   }
                    else {
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
            return imageBCopy.getImageName();
        }
    }







    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  flag  if true garbage collector should be called.
     */
    public void disposeLocal(boolean flag) {
        lutBufferRemapped = null;
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
       // anchorPt = null;
       // newPtVOI = null;

        if (flag == true) {
            super.disposeLocal();
        }
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
     * Returns float alphaBlend. The value used to blend two images displayed in the same frame.
     *
     * @return  alphaBlend
     */
    public float getAlphaBlend() {
        return alphaBlend;
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
     * get the color of the grid.
     *
     * @return  Color grid color
     */

    public Color getGridColor() {
        return this.gridColor;
    }

    /**
     * returns whether grid overlay is being displayed.
     *
     * @return  boolean is grid overlay on?
     */
    public boolean getGridOverlay() {
        return gridOverlayOn;
    }

    /**
     * returns the grid spacing in terms of resolution.
     *
     * @return  float grid spacing
     */
    public float getGridSpacingX() {
        return gridSpacingX;
    }

    /**
     * returns the grid spacing in terms of resolution.
     *
     * @return  float grid spacing
     */
    public float getGridSpacingY() {
        return gridSpacingY;
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
     * Returns the VOI mode.
     *
     * @return  drawing mode for the VOI tools (i.e. ELLIPSE, LINE ...)
     */
    public int getMode() {
        return mode;
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
     * Gets the color of the paint the user has selected from the paint toolbar.
     *
     * @return  int the color of the paint selected by the user, represented as a packed integer
     */
    public int getSelectedPaintColor() {

        try {
            ViewJFrameBase vjfb = ((ViewJComponentEditImage) this).getFrame();
            ViewControlsImage vci = vjfb.getControls();
            ViewToolBarBuilder vtbb = vci.getTools();
            Color rgbColorObj = vtbb.getPaintColor();

            return rgbColorObj.getRGB();
        } catch (Throwable t) {
            return 0xffff0000; // default to red if exception is thrown
        }
    }

    /**
     * Determines if a VOI is selected (active).
     *
     * @return  true if active VOI is found --- Must add to JDialog base - should return null if nothing selected else
     *          return VOI selected!!!!
     */
    public VOI getSelectedVOI() {
        int i;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return null;
        }

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive() == true) {
                break;
            }
        }

        if (i == nVOI) {
            return null;
        } // No VOI to delete

        return VOIs.VOIAt(i);

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
     * Gets whether the slice number should be displayed at the lower left hand corner of the image.
     *
     * @return  whether the slice number will be shown
     */
    public boolean getShowSliceNum() {
        return showSliceNumber;
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

    public VOIHandler getVOIHandler() {
        return this.voiHandler;
    }

    /**
     * Tells whether or not the image is being displayed in checkerboard mode.
     *
     * @return  boolean
     */
    public boolean isCheckerboarded() {
        return ((rowCheckers > 1) && (columnCheckers > 1));
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

        if (mode == WIN_REGION) {

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
        int xS, yS;
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

        int i, j, m;

        ViewVOIVector VOIs = imageActive.getVOIs();
        int nVOI = VOIs.size();
        FileInfoBase fileInfo;
        int xS, yS;
        int distX, distY;
        int xDim, yDim;
        int zDim = 1;
        Color dropperColor;
        float[] lineX = new float[2];
        float[] lineY = new float[2];
        float[] lineZ = new float[2];
        float[] position;
        float[] intensity;
        String str;
        int sliceNum;
        int windowChange, levelChange;
        int sliceSize;
        float minR;
        float maxR;
        float minG;
        float maxG;
        float minB;
        float maxB;
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        if (pixBuffer == null || imageBufferActive == null || modifyFlag == false) {
            return;
        }

        xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        xDim = imageActive.getExtents()[0];
        yDim = imageActive.getExtents()[1];
        sliceSize = xDim * yDim;
        if (imageActive.getNDims() >= 3) {
            zDim = imageActive.getExtents()[2];
        }
        if (xS < 0 || xS >= xDim || yS < 0 || yS >= yDim) {
            return;
        }

        try {
            if (mode == DEFAULT) {
                if ( (mouseEvent.getModifiers() & MouseEvent.BUTTON3_MASK) != 0) {
                    // Dragging the mouse with the right mouse button pressed
                    // increases the window when going from left to right.
                    // Dragging the mouse with the right mouse button pressed
                    // increases the level when going from down to up.
                    setCursor(MipavUtil.winLevelCursor);
                    if (!winLevelSet) {
                        winLevelSet = true;

                        if (imageActive.isColorImage()) {
                            if (imageActive.getType() == ModelStorageBase.ARGB) {
                                minImageWin = 0;
                                maxImageWin = 255;
                            }
                            else {
                                minImageWin = (float) imageActive.getMin();
                                maxImageWin = (float) imageActive.getMax();
                            }

                            // THIS IS (one of) THE CULPRIT(s).  ADJUST THE FOLLOWING TO
                            // SET THE TRANSFER FUNCTION CORRECTLY (to current state)!!
                            // Set LUT min max values;
                            minR = Float.MAX_VALUE;
                            maxR = -Float.MAX_VALUE;
                            minG = Float.MAX_VALUE;
                            maxG = -Float.MAX_VALUE;
                            minB = Float.MAX_VALUE;
                            maxB = -Float.MAX_VALUE;
                            for (i = 0; i < sliceSize; i++) {
                                if (imageBufferActive[4 * i + 1] > maxR) {
                                    maxR = imageBufferActive[4 * i + 1];
                                }
                                if (imageBufferActive[4 * i + 1] < minR) {
                                    minR = imageBufferActive[4 * i + 1];
                                }
                                if (imageBufferActive[4 * i + 2] > maxG) {
                                    maxG = imageBufferActive[4 * i + 2];
                                }
                                if (imageBufferActive[4 * i + 2] < minG) {
                                    minG = imageBufferActive[4 * i + 2];
                                }
                                if (imageBufferActive[4 * i + 3] > maxB) {
                                    maxB = imageBufferActive[4 * i + 3];
                                }
                                if (imageBufferActive[4 * i + 3] < minB) {
                                    minB = imageBufferActive[4 * i + 3];
                                }
                            } // for (i = 0; i < sliceSize; i++)
                            minWin = Math.min(minR, minG);
                            minWin = Math.min(minWin, minB);
                            maxWin = Math.max(maxR, maxG);
                            maxWin = Math.max(maxWin, maxB);
                            if (imageActive.getType() == ModelStorageBase.ARGB) {
                                xWin[1] = minWin;
                                xWin[2] = maxWin;
                            }
                            else {
                                xWin[1] = minWin * 255 / maxWin;
                                xWin[2] = 255;
                            }
                            xWin[0] = 0;
                            yWin[0] = 255;
                            zWin[0] = 0;
                            yWin[1] = 255;
                            zWin[1] = 0;
                            yWin[2] = 0;
                            zWin[2] = 0;
                            xWin[3] = 255;
                            yWin[3] = 0;
                            zWin[3] = 0;
                            if (imageA == imageActive) {
                                RGBTA.getRedFunction().importArrays(xWin, yWin, 4);
                                RGBTA.getGreenFunction().importArrays(xWin, yWin, 4);
                                RGBTA.getBlueFunction().importArrays(xWin, yWin, 4);
                                RGBTA.makeRGB( -1);
                                imageA.notifyImageDisplayListeners(false, (int) (alphaBlend * 100),
                                    RGBTA);
                            }
                            else {
                                RGBTB.getRedFunction().importArrays(xWin, yWin, 4);
                                RGBTB.getGreenFunction().importArrays(xWin, yWin, 4);
                                RGBTB.getBlueFunction().importArrays(xWin, yWin, 4);
                                RGBTB.makeRGB( -1);
                                imageB.notifyImageDisplayListeners(false, (int) (alphaBlend * 100),
                                    RGBTB);
                            }
                        } // if (imageActive.isColorImage())
                        else { // imageActive black and white
                            if (imageActive.getType() == ModelStorageBase.UBYTE) {
                                minImageWin = 0;
                                maxImageWin = 255;
                            }
                            else if (imageActive.getType() ==
                                     ModelStorageBase.BYTE) {
                                minImageWin = -128;
                                maxImageWin = 127;
                            }
                            else {
                                minImageWin = (float) imageActive.getMin();
                                maxImageWin = (float) imageActive.getMax();
                            }

                            minWin = Float.MAX_VALUE;
                            maxWin = -Float.MAX_VALUE;
                            for (i = 0; i < imageBufferActive.length; i++) {
                                if (imageBufferActive[i] > maxWin) {
                                    maxWin = imageBufferActive[i];
                                }
                                if (imageBufferActive[i] < minWin) {
                                    minWin = imageBufferActive[i];
                                }
                            }

                            // THIS IS (one of) THE CULPRIT(s).  ADJUST THE FOLLOWING TO
                            // SET THE TRANSFER FUNCTION CORRECTLY (to current state)!!
                            // Set LUT min max values;
                            xWin[0] = minImageWin;
                            yWin[0] = 255;
                            zWin[0] = 0;
                            xWin[1] = minWin;
                            yWin[1] = 255;
                            zWin[1] = 0;
                            xWin[2] = maxWin;
                            yWin[2] = 0;
                            zWin[2] = 0;
                            xWin[3] = maxImageWin;
                            yWin[3] = 0;
                            zWin[3] = 0;
                            if (imageA == imageActive) {
                                LUTa.getTransferFunction().importArrays(xWin, yWin, 4);
                                imageActive.notifyImageDisplayListeners(LUTa, false);
                            }
                            else {
                                LUTb.getTransferFunction().importArrays(xWin, yWin, 4);
                                imageActive.notifyImageDisplayListeners(LUTb, false);
                            }
                        } // else imageActive black and white
                        level = (xWin[1] + xWin[2]) / 2.0f;
                        window = xWin[2] - xWin[1];
                        oldXS = xS;
                        oldYS = yS;
                    } // if (!winLevelSet)
                    else if (winLevelSet && ( (xS != oldXS) || (yS != oldYS))) {

                        // update the transfer function so the on-screen image
                        // (modelImage/viewJFrameImage) updates for the user
                        if (imageActive.isColorImage()) {
                            windowChange = xS - oldXS;
                            window = window +
                                windowChange * 4 * 255.0f / (xDim - 1);
                            if (window > 2 * 255.0f) {
                                window = 2 * 255.0f;
                            }
                            else if (window < 1) {
                                window = 1;
                            }
                            levelChange = oldYS - yS;
                            level = level + levelChange * 2 * 255.0f / (yDim - 1);
                            if (level > 255.0f) {
                                level = 255.0f;
                            }
                            else if (level < 0.0f) {
                                level = 0.0f;
                            }

                            xWin[2] = level + window / 2;
                            if (xWin[2] > 255.0f) {
                                yWin[2] = 255.0f * (xWin[2] - 255.0f) / window;
                                xWin[2] = 255.0f;
                            }
                            else {
                                yWin[2] = 0.0f;
                            }

                            xWin[1] = level - window / 2;
                            if (xWin[1] < 0.0f) {
                                yWin[1] = 255.0f + 255.0f * xWin[1] / window;
                                xWin[1] = 0.0f;
                            }
                            else {
                                yWin[1] = 255.0f;
                            }

                            if (imageA == imageActive) {
                                RGBTA.getRedFunction().importArrays(xWin, yWin, 4);
                                RGBTA.getGreenFunction().importArrays(xWin, yWin, 4);
                                RGBTA.getBlueFunction().importArrays(xWin, yWin, 4);
                                RGBTA.makeRGB( -1);
                                imageA.notifyImageDisplayListeners(false, (int) (alphaBlend * 100),
                                    RGBTA);
                                if (imageA.getHistoRGBFrame() != null) {
                                    imageA.getHistoRGBFrame().update();
                                }
                            }
                            else {
                                RGBTB.getRedFunction().importArrays(xWin, yWin, 4);
                                RGBTB.getGreenFunction().importArrays(xWin, yWin, 4);
                                RGBTB.getBlueFunction().importArrays(xWin, yWin, 4);
                                RGBTB.makeRGB( -1);
                                imageB.notifyImageDisplayListeners(false, (int) (alphaBlend * 100),
                                    RGBTB);
                                if (imageB.getHistoRGBFrame() != null) {
                                    imageB.getHistoRGBFrame().update();
                                }
                            }
                        } //  if (imageActive.isColorImage())
                        else { // imageActive black and white
                            windowChange = xS - oldXS;
                            window = window +
                                windowChange * 4 * (maxImageWin - minImageWin) / (xDim - 1);
                            if (window > 2 * (maxImageWin - minImageWin)) {
                                window = 2 * (maxImageWin - minImageWin);
                            }
                            else if (window < 1) {
                                window = 1;
                            }
                            levelChange = oldYS - yS;
                            level = level +
                                levelChange * 2 * (maxImageWin - minImageWin) / (yDim - 1);
                            if (level > maxImageWin) {
                                level = maxImageWin;
                            }
                            else if (level < minImageWin) {
                                level = minImageWin;
                            }

                            xWin[2] = level + window / 2;
                            if (xWin[2] > maxImageWin) {
                                yWin[2] = 255.0f * (xWin[2] - maxImageWin) / window;
                                xWin[2] = maxImageWin;
                            }
                            else {
                                yWin[2] = 0.0f;
                            }

                            xWin[1] = level - window / 2;
                            if (xWin[1] < minImageWin) {
                                yWin[1] = 255.0f - 255.0f * (minImageWin - xWin[1]) / window;
                                xWin[1] = minImageWin;
                            }
                            else {
                                yWin[1] = 255.0f;
                            }

                            if (imageA == imageActive) {
                                LUTa.getTransferFunction().importArrays(xWin, yWin, 4);
                                imageActive.notifyImageDisplayListeners(LUTa, false);
                                if (imageA.getHistoLUTFrame() != null) {
                                    imageA.getHistoLUTFrame().update();
                                }
                            }
                            else {
                                LUTb.getTransferFunction().importArrays(xWin, yWin, 4);
                                imageActive.notifyImageDisplayListeners(LUTb, false);
                                if (imageB.getHistoLUTFrame() != null) {
                                    imageB.getHistoLUTFrame().update();
                                }
                            }
                        } // imageActive black and white
                        oldXS = xS;
                        oldYS = yS;
                    } // else if (winLevelSet && ((xS != oldXS) || (yS != oldYS)))
                } // if ((mouseEvent.getModifiers() & MouseEvent.BUTTON3_MASK) != 0)
            } // if (mode == DEFAULT))
            if (imageActive.getFileInfo(0).getOrigin()[0] != 0 || imageActive.getFileInfo(0).getOrigin()[1] != 0
                || imageActive.getFileInfo(0).getOrigin()[2] != 0) {

                fileInfo = imageActive.getFileInfo()[slice];
                String[] values = setScannerPosition(fileInfo, xS, yS, slice);

                if (values != null) {

                    if (imageActive.isColorImage()) {
                        str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  R:  "
                            + String.valueOf(imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 1])
                            + "  G:  "
                            + String.valueOf(imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 2])
                            + "  B:  "
                            + String.valueOf(imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 3])
                            + " Position: " + values[0] + " " + values[1] + " " + values[2];
                    }
                    else {
                        str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  Intensity:  "
                            + String.valueOf(imageBufferActive[yS * imageActive.getExtents()[0] + xS])
                            + " Position: " + values[0] + " " + values[1] + " " + values[2];
                    }

                    frame.setMessageText(str);
                    if ( (mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                        frame.getUserInterface().setDataText("\n" + str);
                    }
                }
                else {
                    str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  Intensity:  "
                        + String.valueOf(imageBufferActive[yS * imageActive.getExtents()[0] + xS]);
                    frame.setMessageText(str);
                    if ( (mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                        frame.getUserInterface().setDataText("\n" + str);
                    }
                }
            }
            else {
                if (imageActive.isColorImage() == true) {
                    str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  R:  "
                        + String.valueOf(imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 1])
                        + "  G:  "
                        + String.valueOf(imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 2])
                        + "  B:  "
                        + String.valueOf(imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 3]);
                    frame.setMessageText(str);
                    if ( (mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                        frame.getUserInterface().setDataText("\n" + str);
                    }
                }
                else {
                    str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  Intensity:  "
                        + String.valueOf(imageBufferActive[yS * imageActive.getExtents()[0] + xS]);
                    frame.setMessageText(str);
                    if ( (mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                        frame.getUserInterface().setDataText("\n" + str);
                    }
                }
            }
        }
        catch (ArrayIndexOutOfBoundsException error) {
            str = "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1);
            frame.setMessageText(str);
            if ( (mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                frame.getUserInterface().setDataText("\n" + str);
            }
        }


        if (mode == RETRACE) { // (mouseEvent.isControlDown() ) {
            Graphics g = getGraphics();
            for (i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).isActive()) {
                    ( (VOIContour) (VOIs.VOIAt(i).getActiveContour(slice))).retraceContour(getZoomX(),
                        getZoomY(), resolutionX, resolutionY, imageActive.getFileInfo(0).getResolutions(), xS, yS,
                        g);
                    setMode(RETRACE);
                    break;
                }
            }
            g.dispose();
            return;
        }
        else if (mode == DROPPER_PAINT) {
            if (imageActive.isColorImage() == true) {
                dropperColor = new Color( (int) imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 1],
                                         (int) imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 2],
                                         (int) imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 3]);
                frame.getControls().getTools().setPaintColor(dropperColor);
            }
            else {
                intensityDropper = imageBufferActive[yS * imageActive.getExtents()[0] + xS];
                frame.getControls().getTools().setIntensityPaintName(String.valueOf( (int) (
                    intensityDropper)));
            }
        }
        else if (mode == ERASER_PAINT) {
            performPaint(mouseEvent, true);
            imageActive.notifyImageDisplayListeners();
        }
        else if (mode == PAINT_VOI) {
            performPaint(mouseEvent, mouseMods == MouseEvent.BUTTON3_MASK);
            imageActive.notifyImageDisplayListeners();
        }
    }


    /**
     * Unchanged.
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

        if ((mode == MAG_REGION) || (mode == PAINT_VOI) || (mode == ERASER_PAINT) || (mode == WIN_REGION)) {

            // repaint();
            paintComponent(getGraphics());
        }
    }

    /**
     *  A mouse event.  If the mode is level set, draws
     *  level sets as user moves mouse.  Otherwise, changes
     *  the cursor depending on where the mouse is in relation
     *  to the VOI.
     *  @param mouseEvent   event that triggered the function
     */
    public void mouseMoved(MouseEvent mouseEvent) {
//System.err.println("vjcomp mouse moved, mode: " + mode);

        int xS, yS;

        Graphics g = getGraphics();
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();
        shiftDown = mouseEvent.isShiftDown();

        if (mode == ZOOMING_IN || mode == ZOOMING_OUT) {
            // if we are in zoom mode, we don't care about any of the other things
            // that are happening here, in fact, zoom breaks if we don't return
            return;
        } else if (mode == RECTANGLE || mode == ELLIPSE || mode == LINE || mode == RECTANGLE3D || mode == POINT_VOI
            || mode == POLYLINE || mode == LEVELSET || mode == PAINT_VOI || mode == DROPPER_PAINT
            || mode == ERASER_PAINT || mode == QUICK_LUT || mode == PROTRACTOR || mode == LIVEWIRE
            || mode == ANNOTATION || mode == POLYLINE_SLICE_VOI || mode == MOVE || mode == MOVE_POINT || mode == NEW_POINT) {
            g.dispose();
            return;
        }


        xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        if (mode == PAINT_VOI && mouseEvent.isShiftDown()) {
            performPaint(mouseEvent, false);
            imageActive.notifyImageDisplayListeners(null, true);
            return;
        }

        // the user can erase by holding down shift while in eraser mode
        // or by holding down control while in paint mode
        if ( (mode == ERASER_PAINT && mouseEvent.isShiftDown()) || (mode == PAINT_VOI && mouseEvent.isControlDown())) {
            performPaint(mouseEvent, true);
            imageActive.notifyImageDisplayListeners(null, true);
            return;
        }

        if (g == null || modifyFlag == false || slice == -99) {
            return;
        }
        if (pixBuffer == null || imageBufferActive == null) {
            g.dispose();
            return;
        }

        if (xS < 0 || xS >= imageActive.getExtents()[0] || // Check to ensure point is within
            yS < 0 || yS >= imageActive.getExtents()[1]) { // the image bounds
            g.dispose();
            return;
        }

        if (mode == MAG_REGION) {
            repaint();
            return;
        } else if (mode == WIN_REGION) {
            repaint();
            return;
        } else if (mode == PAINT_VOI || mode == ERASER_PAINT) {
            //repaint();
                paintComponent(getGraphics());
            return;
        } else if (mode == PAINT_CAN || mode == PAINT_VASC) {
            if (growDialog != null) {
                if (imageActive.isColorImage()) {
                    growDialog.setPositionText(
                        "  X: " + String.valueOf( (xS + 1)) + " Y: " + String.valueOf( (yS + 1)) + "  R:  "
                        + String.valueOf(imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 1])
                        + "  G:  "
                        + String.valueOf(imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 2])
                        + "  B:  "
                        + String.valueOf(imageBufferActive[4 * (yS * imageActive.getExtents()[0] + xS) + 3]));
                }
                else {
                    growDialog.setPositionText(
                        "  X: " + String.valueOf(xS + 1) + " Y: " + String.valueOf(yS + 1) + "  Intensity:  "
                        + String.valueOf(imageBufferActive[yS * imageActive.getExtents()[0] + xS]));
                }
            }
            g.dispose();
            return;
        }

        //System.err.println("got to end...");

        setMode(DEFAULT);
    } //end mouseMoved



    /**
     * A mouse event. Sets the mode of the program depending on the cursor mode. If the mode is move, activates the
     * contour or line and enables the delete button.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mousePressed(MouseEvent mouseEvent) {
        int xS, yS;
        int x, y;
        Color dropperColor;
        float[] lineX = null;
        float[] lineY = null;
        float[] lineZ = null;
        float[] position;
        float[] intensity;
        ViewJFrameGraph lineGraph;
        int i, j, m;
        int nVOI;
        ViewVOIVector VOIs;
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        if (modifyFlag == false) {
            return;
        }

        // save the state of the shift button
        mousePressIsShiftDown = mouseEvent.isShiftDown();

        if ((mode == DEFAULT) && mouseEvent.isControlDown()) { // center the image around cursor (no zooming)

            int centerX = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().width / 2;
            int centerY = ((ViewJFrameImage) frame).getScrollPane().getViewport().getExtentSize().height / 2;

            ((ViewJFrameImage) frame).getScrollPane().getHorizontalScrollBar().setValue(mouseEvent.getX() - centerX);
            ((ViewJFrameImage) frame).getScrollPane().getVerticalScrollBar().setValue(mouseEvent.getY() - centerY);
        }

        if ((mode == ZOOMING_IN) || (mode == ZOOMING_OUT)) {
            xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
            yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

            ((ViewJFrameImage) frame).updateFrame(getZoomMagnitudeX(mouseEvent.getButton() == MouseEvent.BUTTON3),
                                                  getZoomMagnitudeY(mouseEvent.getButton() == MouseEvent.BUTTON3), xS,
                                                  yS);

            if (mouseEvent.isShiftDown() == false) {
                mode = DEFAULT;
                setCursor(MipavUtil.defaultCursor);
            }

            return;
        }

        try {
            lineX = new float[2];
            lineY = new float[2];
            lineZ = new float[2];

            xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
            yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

            x = mouseEvent.getX();
            y = mouseEvent.getY();

            if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || (yS < 0) || (yS >= imageActive.getExtents()[1])) {
                return;
            }

            if (mode == DROPPER_PAINT) {

                if (imageActive.isColorImage() == true) {
                    dropperColor = new Color((int)
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

            if (mode == ERASER_PAINT) {
                performPaint(mouseEvent, true);
                imageActive.notifyImageDisplayListeners();
            } else if (mode == PAINT_VOI) {

                // backup paintBitmap to paintBitmapBU
                paintBitmapBU.clear();

                int length = paintBitmap.length();

                for (int b = 0; b < length; b++) {

                    if (paintBitmap.get(b)) {
                        paintBitmapBU.set(b);
                    }
                }

                xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
                yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

                performPaint(mouseEvent, mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK);
                imageActive.notifyImageDisplayListeners();
            }

            if ((mode == MAG_REGION) && (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK)) {

                if ((magSettings != null) && !magSettings.isVisible()) {
                    magSettings.setWidthText((int) (frame.getSize().width * 0.25));
                    magSettings.setVisible(true);
                }
            }

            if ((mode == WIN_REGION) && (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK)) {
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
            setMode(DEFAULT);

            return;
        }

    }

    /**
     *  A mouse event.  This function sets up and draws
     *  the VOI according to the mode.
     *  @param mouseEvent   event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        int i, j, k;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();
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
        int xR = mouseEvent.getX();
        int yR = mouseEvent.getY();

        int xDim = imageActive.getExtents()[0];
        int yDim = imageActive.getExtents()[1];

        if (xS < 0 || xS >= imageActive.getExtents()[0] || yS < 0 || yS >= imageActive.getExtents()[1]) {
            return;
        }

        if (mode != MOVE) {
            setPixelInformationAtLocation(xS, yS);
        }

        // clicking with the right mouse button in a regular image frame updates the image's
        // tri-image frame (if one is open) to show that point in all of the components
        if ( (mouseEvent.getModifiers() & mouseEvent.BUTTON2_MASK) != 0) {
            ViewJFrameTriImage triFrame = imageActive.getTriImageFrame();

            if (triFrame != null) {
                triFrame.setSlicesFromFrame(xS, yS, slice);
            }
        }

        if (mode == POINT_VOI) {

        } // end of else if (mode == POINT_VOI)

        else if (mode == POLYLINE_SLICE_VOI) {

        }
        else if (mode == ANNOTATION) {

        }
        else if (mode == LEVELSET) {}
        else if (mode == RECTANGLE) {}
        else if (mode == RECTANGLE3D) {}
        else if (mode
                 == ELLIPSE) {}
        else if (mode == LINE) {}
        else if (mode == PROTRACTOR) {}
        else if (mode == NEW_POINT) {
        }
        else if (mode == DELETE_POINT) { // impossible for LINE

        }
        else if (mode == PAINT_CAN) {
            xPG = (short) xS;
            yPG = (short) yS;
            zPG = (short) slice;
            if (imageActive.isColorImage()) {
                int index = 4 * (yS + imageActive.getExtents()[0] + xS);
                seedValR = imageBufferActive[index + 1];
                seedValG = imageBufferActive[index + 2];
                seedValB = imageBufferActive[index + 3];
                regionGrow( (short) xS, (short) yS, (short) slice, seedValR,
                           seedValG, seedValB, null, true);
            }
            else {
                seedVal = imageBufferActive[yS * imageActive.getExtents()[0] + xS];
                regionGrow( (short) xS, (short) yS, (short) slice, seedVal, null, true);
            }
            imageActive.notifyImageDisplayListeners(null, true);

        }
        else if (mode == PAINT_VASC) {
            int index = xS + yS * imageActive.getExtents()[0];
            int z = MipavMath.round( ( (ViewJFramePaintVasculature) frame).getMIPZValue(index));
            float value = ( (ViewJFramePaintVasculature) frame).imageBuffer[index + z * imageActive.getSliceSize()];

            ( (ViewJFrameImage) ( (ViewJFramePaintVasculature) frame).parent).getComponentImage().regionGrow(
                (short) xS, (short) yS, (short) z, value, null, true);
            ( (ViewJFrameImage) ( (ViewJFramePaintVasculature) frame).parent).getComponentImage().setRegionGrowVars(
                (short) xS, (short) yS, (short) z, value);
            imageActive.notifyImageDisplayListeners(null, true);
        }
        else if (mode == RETRACE) {
            nVOI = VOIs.size();
            for (i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).isActive()) {
                    ( (VOIContour) (VOIs.VOIAt(i).getActiveContour(slice))).trimPoints(Preferences.getTrim(),
                        true);
                    ( (VOIContour) (VOIs.VOIAt(i).getActiveContour(slice))).resetIndex();

                    break;
                }
            }
            imageActive.notifyImageDisplayListeners();
        }
        else if (mode == QUICK_LUT) {
            int wS, hS;
            float min = Float.MAX_VALUE;
            float max = -100000000;
            float minR = Float.MAX_VALUE;
            float maxR = -Float.MAX_VALUE;
            float minG = Float.MAX_VALUE;

            ;
            float maxG = -Float.MAX_VALUE;
            float minB = Float.MAX_VALUE;
            float maxB = -Float.MAX_VALUE;

            float[] x = new float[4];
            float[] y = new float[4];
            float[] z = new float[4];
            Dimension dim = new Dimension(256, 256);
            float minImage, maxImage;

            xS = MipavMath.round(voiHandler.getRubberband().getBounds().x / (getZoomX() * resolutionX));
            yS = MipavMath.round(voiHandler.getRubberband().getBounds().y / (getZoomY() * resolutionY));
            wS = MipavMath.round(voiHandler.getRubberband().getBounds().width / (getZoomX() * resolutionX));
            hS = MipavMath.round(voiHandler.getRubberband().getBounds().height / (getZoomY() * resolutionY));

            if (imageA.isColorImage() == false) {
                if (imageA == imageActive) {
                    for (j = yS; j < yS + hS; j++) {
                        for (i = xS; i < xS + wS; i++) {
                            if (imageBufferA[j * xDim + i] > max) {
                                max = imageBufferA[j * xDim + i];
                            }
                            if (imageBufferA[j * xDim + i] < min) {
                                min = imageBufferA[j * xDim + i];
                            }
                        }
                    }

                    if (imageA.getType() == ModelStorageBase.UBYTE) {
                        minImage = 0;
                        maxImage = 255;
                    }
                    else if (imageA.getType() == ModelStorageBase.BYTE) {
                        minImage = -128;
                        maxImage = 127;
                    }
                    else {
                        minImage = (float) imageA.getMin();
                        maxImage = (float) imageA.getMax();
                    }

                    // Set LUT min max values;
                    x[0] = minImage;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    x[1] = min;
                    y[1] = dim.height - 1;
                    z[1] = 0;
                    x[2] = max;
                    y[2] = 0;
                    z[2] = 0;
                    x[3] = maxImage;
                    y[3] = 0;
                    z[3] = 0;
                    LUTa.getTransferFunction().importArrays(x, y, 4);
                }
                else if (imageB != null && imageActive == imageB) {
                    if (imageB.getType() == ModelStorageBase.UBYTE) {
                        minImage = 0;
                        maxImage = 255;
                    }
                    else if (imageB.getType() == ModelStorageBase.BYTE) {
                        minImage = -128;
                        maxImage = 127;
                    }
                    else {
                        minImage = (float) imageB.getMin();
                        maxImage = (float) imageB.getMax();
                    }
                    if (imageBufferB != null) {
                        min = Float.MAX_VALUE;
                        max = -100000000;
                        for (j = yS; j < yS + hS; j++) {
                            for (i = xS; i < xS + wS; i++) {
                                if (imageBufferB[j * xDim + i] > max) {
                                    max = imageBufferB[j * xDim + i];
                                }
                                if (imageBufferB[j * xDim + i] < min) {
                                    min = imageBufferB[j * xDim + i];
                                }
                            }
                        }
                        x[0] = minImage;
                        y[0] = dim.height - 1;
                        z[0] = 0;
                        x[1] = min;
                        y[1] = dim.height - 1;
                        z[1] = 0;
                        x[2] = max;
                        y[2] = 0;
                        z[2] = 0;
                        x[3] = maxImage;
                        y[3] = 0;
                        z[3] = 0;
                        LUTb.getTransferFunction().importArrays(x, y, 4);
                    }
                }
            }
            else { // RGB image
                if (imageA == imageActive) {
                    for (j = yS; j < yS + hS; j++) {
                        for (i = xS; i < xS + wS; i++) {
                            if (imageBufferA[j * xDim * 4 + i * 4 + 1] > maxR) {
                                maxR = imageBufferA[j * xDim * 4 + i * 4 + 1];
                            }
                            if (imageBufferA[j * xDim * 4 + i * 4 + 1] < minR) {
                                minR = imageBufferA[j * xDim * 4 + i * 4 + 1];
                            }
                            if (imageBufferA[j * xDim * 4 + i * 4 + 2] > maxG) {
                                maxG = imageBufferA[j * xDim * 4 + i * 4 + 2];
                            }
                            if (imageBufferA[j * xDim * 4 + i * 4 + 2] < minG) {
                                minG = imageBufferA[j * xDim * 4 + i * 4 + 2];
                            }
                            if (imageBufferA[j * xDim * 4 + i * 4 + 3] > maxB) {
                                maxB = imageBufferA[j * xDim * 4 + i * 4 + 3];
                            }
                            if (imageBufferA[j * xDim * 4 + i * 4 + 3] < minB) {
                                minB = imageBufferA[j * xDim * 4 + i * 4 + 3];
                            }
                        }
                    }
                    max = Math.max(maxR, maxG);
                    max = Math.max(maxB, max);

                    // Set LUT min max values;
                    // if (imageA.isColorImage() == true) {
                    if (imageA.getType() == ModelStorageBase.ARGB) {
                        x[1] = minR;
                        x[2] = maxR;
                    }
                    else {
                        x[1] = minR * 255 / max;
                        x[2] = maxR * 255 / max;
                    }
                    x[0] = 0;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    y[1] = dim.height - 1;
                    z[1] = 0;
                    y[2] = 0;
                    z[2] = 0;
                    x[3] = 255;
                    y[3] = 0;
                    z[3] = 0;

                    RGBTA.getRedFunction().importArrays(x, y, 4);

                    if (imageA.getType() == ModelStorageBase.ARGB) {
                        x[1] = minG;
                        x[2] = maxG;
                    }
                    else {
                        x[1] = minG * 255 / max;
                        x[2] = maxG * 255 / max;
                    }
                    x[0] = 0;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    y[1] = dim.height - 1;
                    z[1] = 0;
                    y[2] = 0;
                    z[2] = 0;
                    x[3] = 255;
                    y[3] = 0;
                    z[3] = 0;

                    RGBTA.getGreenFunction().importArrays(x, y, 4);

                    if (imageA.getType() == ModelStorageBase.ARGB) {
                        x[1] = minB;
                        x[2] = maxB;
                    }
                    else {
                        x[1] = minB * 255 / max;
                        x[2] = maxB * 255 / max;
                    }
                    x[0] = 0;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    y[1] = dim.height - 1;
                    z[1] = 0;
                    y[2] = 0;
                    z[2] = 0;
                    x[3] = 255;
                    y[3] = 0;
                    z[3] = 0;

                    RGBTA.getBlueFunction().importArrays(x, y, 4);
                    RGBTA.makeRGB( -1);
                }
                else if (imageBufferB != null && imageB != null && imageB == imageActive) {
                    minR = Float.MAX_VALUE;
                    maxR = -Float.MAX_VALUE;
                    minG = Float.MAX_VALUE;
                    maxG = -Float.MAX_VALUE;
                    minB = Float.MAX_VALUE;
                    maxB = -Float.MAX_VALUE;
                    for (j = yS; j < yS + hS; j++) {
                        for (i = xS; i < xS + wS; i++) {
                            if (imageBufferB[j * xDim * 4 + i * 4 + 1] > maxR) {
                                maxR = imageBufferB[j * xDim * 4 + i * 4 + 1];
                            }
                            if (imageBufferB[j * xDim * 4 + i * 4 + 1] < minR) {
                                minR = imageBufferB[j * xDim * 4 + i * 4 + 1];
                            }
                            if (imageBufferB[j * xDim * 4 + i * 4 + 2] > maxG) {
                                maxG = imageBufferB[j * xDim * 4 + i * 4 + 2];
                            }
                            if (imageBufferB[j * xDim * 4 + i * 4 + 2] < minG) {
                                minG = imageBufferB[j * xDim * 4 + i * 4 + 2];
                            }
                            if (imageBufferB[j * xDim * 4 + i * 4 + 3] > maxB) {
                                maxB = imageBufferB[j * xDim * 4 + i * 4 + 3];
                            }
                            if (imageBufferB[j * xDim * 4 + i * 4 + 3] < minB) {
                                minB = imageBufferB[j * xDim * 4 + i * 4 + 3];
                            }
                        }
                    }
                    max = Math.max(maxR, maxG);
                    max = Math.max(maxB, max);

                    // Set LUT min max values;
                    if (imageB.getType() == ModelStorageBase.ARGB) {
                        x[1] = minR;
                        x[2] = maxR;
                    }
                    else {
                        x[1] = minR * 255 / max;
                        x[2] = maxR * 255 / max;
                    }
                    x[0] = 0;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    y[1] = dim.height - 1;
                    z[1] = 0;
                    y[2] = 0;
                    z[2] = 0;
                    x[3] = 255;
                    y[3] = 0;
                    z[3] = 0;

                    RGBTB.getRedFunction().importArrays(x, y, 4);
                    if (imageB.getType() == ModelStorageBase.ARGB) {
                        x[1] = minG;
                        x[2] = maxG;
                    }
                    else {
                        x[1] = minG * 255 / max;
                        x[2] = maxG * 255 / max;
                    }
                    x[0] = 0;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    y[1] = dim.height - 1;
                    z[1] = 0;
                    y[2] = 0;
                    z[2] = 0;
                    x[3] = 255;
                    y[3] = 0;
                    z[3] = 0;

                    RGBTB.getGreenFunction().importArrays(x, y, 4);
                    if (imageB.getType() == ModelStorageBase.ARGB) {
                        x[1] = minB;
                        x[2] = maxB;
                    }
                    else {
                        x[1] = minB * 255 / max;
                        x[2] = maxB * 255 / max;
                    }
                    x[0] = 0;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    y[1] = dim.height - 1;
                    z[1] = 0;
                    y[2] = 0;
                    z[2] = 0;
                    x[3] = 255;
                    y[3] = 0;
                    z[3] = 0;

                    RGBTB.getBlueFunction().importArrays(x, y, 4);
                    RGBTB.makeRGB( -1);
                }
            }

            if (!imageActive.isColorImage()) {
                imageA.notifyImageDisplayListeners(LUTa, false);
                if (imageB != null) {
                    imageB.notifyImageDisplayListeners(LUTb, false);
                }
            }
            else {
                imageA.notifyImageDisplayListeners(false, (int) (alphaBlend * 100), RGBTA);
                if (imageB != null) {
                    imageB.notifyImageDisplayListeners(false, (int) (alphaBlend * 100), RGBTB);
                }
            }

            if (imageB == null) {
                if (imageA.isColorImage()) {
                    if (imageA.getHistoRGBFrame() != null) {
                        imageA.getHistoRGBFrame().update();
                    }
                }
                else if (imageA.getHistoLUTFrame() != null) {
                    if (imageA.getHistoLUTFrame() != null) {
                        imageA.getHistoLUTFrame().update();
                    }
                }
            }
            else {
                if (imageB.isColorImage()) {
                    if (imageB.getHistoRGBFrame() != null) {
                        imageB.getHistoRGBFrame().update();
                    }
                }
                else if (imageB.getHistoLUTFrame() != null) {
                    if (imageB.getHistoLUTFrame() != null) {
                        imageB.getHistoLUTFrame().update();
                    }
                }
            }

            if (mouseEvent.isShiftDown() != true) {
                setMode(DEFAULT);
            }
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
                    ((ViewJFrameImage) frame).incSlice();
                } else {
                    ((ViewJFrameImage) frame).updateFrame(getZoomX() * 2.0f, getZoomY() * 2.0f);
                }
            } else {

                if (imageActive.getNDims() > 2) {
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

    /**
     * Paints the image and calls drawSelf for all VOIs.
     *
     * @param  graphics  graphics
     */
    public void paintComponent(Graphics graphics) {
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
            makePaintImage(paintImageBuffer);

            if (Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)) {
                makePaintBitmapBorder(paintImageBuffer);
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

            memImage = new MemoryImageSource(imageDim.width, imageDim.height, paintImageBuffer, 0, imageDim.width);

            Image paintImage = createImage(memImage); // the image representing the paint mask

            // change rendering hint back from BILINEAR to nearest neighbor so that
            // all other painting will not be in interpolated mode
            offscreenGraphics2d.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                                     RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));

            offscreenGraphics2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f));

            offscreenGraphics2d.drawImage(paintImage, 0, 0, zoomedWidth, zoomedHeight, 0, 0, img.getWidth(this),
                                          img.getHeight(this), null);

            if ((mode == PAINT_VOI) ||
                    ((mode == ERASER_PAINT) && ((lastMouseX != OUT_OF_BOUNDS) || (lastMouseY != OUT_OF_BOUNDS)))) {

                // this method repaints the paint brush cursor without repainting the entire image
                repaintPaintBrushCursorFast(offscreenGraphics2d);
            }

            voiHandler.drawVOIs(offscreenGraphics2d, overlayOn, gridOverlayOn); // draw all VOI regions

            drawImageText(offscreenGraphics2d); // draw image text, i.e. slice number

            if ((mode == WIN_REGION) && ((lastMouseX != OUT_OF_BOUNDS) || (lastMouseY != OUT_OF_BOUNDS)) &&
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
            } else if ((mode == MAG_REGION) && ((lastMouseX != OUT_OF_BOUNDS) || (lastMouseY != OUT_OF_BOUNDS))) {
                paintMagComponent(offscreenGraphics2d);
            }

            if (onTop) {

                // paint the on-top notifier for the user when this component is on the top of the user-interface
                offscreenGraphics2d.setColor(toppedColor);
                offscreenGraphics2d.drawRect(visibleRect.x, visibleRect.y, visibleRect.width - 1,
                                             visibleRect.height - 1);
            }

            graphics.drawImage(offscreenImage, 0, 0, null);

            offscreenImage.flush();
            paintImage.flush();

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

        if (mode == LEVELSET) {
            g.setColor(Color.yellow);
            g.drawPolygon(voiHandler.getZoomedLevelSetPolygon());
            //g.drawPolygon(zoomPolygon(rbLevelSet.getLevelSetPolygon(), getZoomX(), getZoomY()));
        }

        if (overlayOn) {
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
        Cursor cursor = getCursor();

        setCursor(MipavUtil.waitCursor);

        int count;
        BitSet tempBitmap = null;

        if (click) {

            // backup the current paint mask
            int pEnd = paintBitmap.size();

            for (int p = 0; p < pEnd; p++) {

                if (paintBitmap.get(p)) {
                    paintBitmapBU.set(p);
                } else {
                    paintBitmapBU.clear(p);
                }
            }

            // reset the seedPaintBitmask so that we can keep track of points added in the regionGrow for this click
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
            AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(imageActive, 1.0f, 1.0f);

            regionGrowAlgo.setActiveImage(false);

            if (imageActive.getType() == ModelStorageBase.BOOLEAN) {
                less = 0;
                more = 0;
                variableThresholds = false;
            }

            if (imageActive.getNDims() == 2) {
                count = regionGrowAlgo.regionGrow2D(seedPaintBitmap, new Point(saveX, saveY), fuzzyThreshold, useVOI,
                                                    displayFuzzy, growDialog, saveValue - less, saveValue + more,
                                                    sizeLimit, maxDistance, variableThresholds);
                showRegionInfo(count, str);
            } else if ((imageActive.getNDims() == 3) || (imageActive.getNDims() == 4)) {
                CubeBounds regionGrowBounds;

                if ((growDialog instanceof JDialogPaintGrow) && ((JDialogPaintGrow) growDialog).boundsConstrained()) {

                    // constrain bounds to cropping volume
                    regionGrowBounds = ((ViewJComponentTriImage) ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A))
                                           .getBoundedVolume();
                } else {

                    // bounds are not constrained by cropping volume, use image extents as bounds
                    regionGrowBounds = new CubeBounds(imageExtents[0], 0, imageExtents[1], 0, imageExtents[2], 0);
                }

                count = regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3Ds(saveX, saveY, saveZ), fuzzyThreshold,
                                                    useVOI, displayFuzzy, growDialog, saveValue - less,
                                                    saveValue + more, sizeLimit, maxDistance, variableThresholds,
                                                    timeSlice, regionGrowBounds);
                showRegionInfo(count, str);
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

        imageActive.notifyImageDisplayListeners(null, true);
        setCursor(cursor);
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
            int pEnd = paintBitmap.size();

            for (int p = 0; p < pEnd; p++) {

                if (paintBitmap.get(p)) {
                    paintBitmapBU.set(p);
                } else {
                    paintBitmapBU.clear(p);
                }
            }

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

            regionGrowAlgo.setActiveImage(false);

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
                    regionGrowBounds = ((ViewJComponentTriImage) ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A))
                                           .getBoundedVolume();
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
        Cursor cursor = getCursor();

        setCursor(MipavUtil.waitCursor);

        int count;
        BitSet tempBitmap = null;

        if (click) {

            // backup the current paint mask
            int pEnd = paintBitmap.size();

            for (int p = 0; p < pEnd; p++) {

                if (paintBitmap.get(p)) {
                    paintBitmapBU.set(p);
                } else {
                    paintBitmapBU.clear(p);
                }
            }

            // reset the seedPaintBitmask so that we can keep track of points added in the regionGrow for this click
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
            AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(imageActive, 1.0f, 1.0f);

            regionGrowAlgo.setActiveImage(false);

            if (imageActive.getNDims() == 2) {
                count = regionGrowAlgo.regionGrow2D(seedPaintBitmap, new Point(saveX, saveY), fuzzyThreshold, useVOI,
                                                    displayFuzzy, growDialog, saveValueR - lessR, saveValueR + moreR,
                                                    saveValueG - lessG, saveValueG + moreG, saveValueB - lessB,
                                                    saveValueB + moreB, sizeLimit, maxDistance);
                showRegionInfo(count, str);
            } else if ((imageActive.getNDims() == 3) || (imageActive.getNDims() == 4)) {
                CubeBounds regionGrowBounds;

                if ((growDialog instanceof JDialogPaintGrow) && ((JDialogPaintGrow) growDialog).boundsConstrained()) {

                    // constrain bounds to cropping volume
                    regionGrowBounds = ((ViewJComponentTriImage) ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A))
                                           .getBoundedVolume();
                } else {

                    // bounds are not constrained by cropping volume, use image extents as bounds
                    regionGrowBounds = new CubeBounds(imageExtents[0], 0, imageExtents[1], 0, imageExtents[2], 0);
                }

                count = regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3Ds(saveX, saveY, saveZ), fuzzyThreshold,
                                                    useVOI, displayFuzzy, growDialog, saveValueR - lessR,
                                                    saveValueR + moreR, saveValueG - lessG, saveValueG + moreG,
                                                    saveValueB - lessB, saveValueB + moreB, sizeLimit, maxDistance,
                                                    timeSlice, regionGrowBounds);
                showRegionInfo(count, str);
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

        imageActive.notifyImageDisplayListeners(null, true);
        setCursor(cursor);
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
            int pEnd = paintBitmap.size();

            for (int p = 0; p < pEnd; p++) {

                if (paintBitmap.get(p)) {
                    paintBitmapBU.set(p);
                } else {
                    paintBitmapBU.clear(p);
                }
            }

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

            regionGrowAlgo.setActiveImage(false);

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
                    regionGrowBounds = ((ViewJComponentTriImage) ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A))
                                           .getBoundedVolume();
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
            float min, max;
            float[] x = new float[4];
            float[] y = new float[4];
            float[] z = new float[4];

            float[] x2 = new float[4];
            float[] y2 = new float[4];
            float[] z2 = new float[4];

            float[] x3 = new float[4];
            float[] y3 = new float[4];
            float[] z3 = new float[4];
            Dimension dim = new Dimension(256, 256);

            if (imageA.isColorImage() == false) {

                if (imageA == imageActive) {

                    // Set LUT min max values;
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

                    x[0] = min;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    x[1] = (min + ((max - min) / 3.0f));
                    y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z[1] = 0;
                    x[2] = (min + ((max - min) * 0.67f));
                    y[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z[2] = 0;
                    x[3] = max;
                    y[3] = 0;
                    z[3] = 0;
                    LUTa.getTransferFunction().importArrays(x, y, 4);
                } else if ((imageB != null) && (imageB == imageActive)) {

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

                    x2[0] = min;
                    y2[0] = dim.height - 1;
                    z2[0] = 0;
                    x2[1] = (min + ((max - min) / 3.0f));
                    y2[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z2[1] = 0;
                    x2[2] = (min + ((max - min) * 0.67f));
                    y2[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z2[2] = 0;
                    x2[3] = max;
                    y2[3] = 0;
                    z2[3] = 0;
                    LUTb.getTransferFunction().importArrays(x2, y2, 4);
                }
            } else { // RGB image

                int[] RGBExtents = new int[2];

                RGBExtents[0] = 4;
                RGBExtents[1] = 256;

                if (imageA == imageActive) {

                    // Set LUT min max values;
                    x[0] = 0;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    x[1] = 255 * 0.333f;
                    y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z[1] = 0;
                    x[2] = 255 * 0.667f;
                    y[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z[2] = 0;
                    x[3] = 255;
                    y[3] = 0;
                    z[3] = 0;

                    if (RGBTA == null) {
                        RGBTA = new ModelRGB(RGBExtents);
                        // imageA.setRGBT(RGBTA);
                    }

                    RGBTA.getRedFunction().importArrays(x, y, 4);

                    x2[0] = 0;
                    y2[0] = dim.height - 1;
                    z2[0] = 0;
                    x2[1] = 255 * 0.333f;
                    y2[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z2[1] = 0;
                    x2[2] = 255 * 0.667f;
                    y2[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z2[2] = 0;
                    x2[3] = 255;
                    y2[3] = 0;
                    z2[3] = 0;
                    RGBTA.getGreenFunction().importArrays(x2, y2, 4);

                    x3[0] = 0;
                    y3[0] = dim.height - 1;
                    z3[0] = 0;
                    x3[1] = 255 * 0.333f;
                    y3[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z3[1] = 0;
                    x3[2] = 255 * 0.667f;
                    y3[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z3[2] = 0;
                    x3[3] = 255;
                    y3[3] = 0;
                    z3[3] = 0;
                    RGBTA.getBlueFunction().importArrays(x3, y3, 4);

                    RGBTA.makeRGB(-1);
                } else if ((imageBufferB != null) && (imageB != null) && (imageB == imageActive)) {

                    // Set LUT min max values;
                    x = new float[4];
                    y = new float[4];
                    z = new float[4];

                    x2 = new float[4];
                    y2 = new float[4];
                    z2 = new float[4];

                    x3 = new float[4];
                    y3 = new float[4];
                    z3 = new float[4];

                    x[0] = 0;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    x[1] = 255 * 0.333f;
                    y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z[1] = 0;
                    x[2] = 255 * 0.667f;
                    y[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z[2] = 0;
                    x[3] = 255;
                    y[3] = 0;
                    z[3] = 0;

                    if (RGBTB == null) {
                        RGBTB = new ModelRGB(RGBExtents);
                    }

                    RGBTB.getRedFunction().importArrays(x, y, 4);

                    x2[0] = 0;
                    y2[0] = dim.height - 1;
                    z2[0] = 0;
                    x2[1] = 255 * 0.333f;
                    y2[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z2[1] = 0;
                    x2[2] = 255 * 0.667f;
                    y2[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z2[2] = 0;
                    x2[3] = 255;
                    y2[3] = 0;
                    z2[3] = 0;
                    RGBTB.getGreenFunction().importArrays(x2, y2, 4);

                    x3[0] = 0;
                    y3[0] = dim.height - 1;
                    z3[0] = 0;
                    x3[1] = 255 * 0.333f;
                    y3[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z3[1] = 0;
                    x3[2] = 255 * 0.667f;
                    y3[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z3[2] = 0;
                    x3[3] = 255;
                    y3[3] = 0;
                    z3[3] = 0;
                    RGBTB.getBlueFunction().importArrays(x3, y3, 4);

                    RGBTB.makeRGB(-1);
                }
            }

            imageA.notifyImageDisplayListeners(null, false);

            if (imageB != null) {
                imageB.notifyImageDisplayListeners(null, false);
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
    }

    /**
     * Sets the image to display in "Checkerboard" mode with the given numbers of rows and columns.
     *
     * @param  rowCheckers     int # of rows
     * @param  columnCheckers  int # of columns
     */
    public void setCheckerboard(int rowCheckers, int columnCheckers) {
        this.rowCheckers = rowCheckers;
        this.columnCheckers = columnCheckers;

        ViewJFrameBase vjfb = ((ViewJComponentEditImage) this).getFrame();
        ViewControlsImage vci = vjfb.getControls();

        if ((rowCheckers < 1) || (columnCheckers < 1)) {
            vci.setAlphaSliderEnabled(true);
        } else {
            vci.setAlphaSliderEnabled(false);
        }
    }

    /**
     * For generating the display of 1 or 2 RGB images - sets the buffers and the java image.
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean setColorPaintBuffers(int tSlice, int zSlice, boolean forceShow) {
        // Note that alphaBlending is applied with 1 component taken as zero if both components are not present -for
        // example, if either imageA or imageB but not both has red, then the red component is alphaBlended with zero.

        int i, j;
        int bufferSize;
        int ind4, index;
        int Ra, Ga, Ba, Rb, Gb, Bb;
        int imageSize;
        int pixValue;
        float redMapped, greenMapped, blueMapped;
        int[] RGBIndexBufferA = null;
        int[] RGBIndexBufferB = null;
        int xDim, yDim, zDim;
        float maxColorA = 255;
        float maxColorB = 255;
        float normColorB = 1;
        float normColorA = 1;
        float offsetAR = 0.0f;
        float offsetAG = 0.0f;
        float offsetAB = 0.0f;
        float offsetBR = 0.0f;
        float offsetBG = 0.0f;
        float offsetBB = 0.0f;

        if ((orientation == AXIAL) || (orientation == NA)) {
            bufferSize = imageExtents[0] * imageExtents[1] * 4;
            imageSize = imageExtents[0] * imageExtents[1];
        } else if (orientation == CORONAL) {
            bufferSize = imageExtents[0] * imageExtents[2] * 4;
            imageSize = imageExtents[0] * imageExtents[2];
        } else { // orientation == ZY
            bufferSize = imageExtents[2] * imageExtents[1] * 4;
            imageSize = imageExtents[2] * imageExtents[1];
        }

        xDim = imageExtents[0];
        yDim = imageExtents[1];
        zDim = 1;

        if (imageA.getNDims() >= 3) {
            zDim = imageExtents[2];
        }

        if (imageA.getType() == ModelStorageBase.ARGB_USHORT) {
            maxColorA = (float) imageA.getMaxR();
            maxColorA = Math.max((float) imageA.getMaxG(), maxColorA);
            maxColorA = Math.max((float) imageA.getMaxB(), maxColorA);
        } else if (imageA.getType() == ModelStorageBase.ARGB_FLOAT) {

            if (imageA.getMinR() < 0.0) {
                maxColorA = (float) (imageA.getMaxR() - imageA.getMinR());
                offsetAR = (float) (-imageA.getMinR());
            } else {
                maxColorA = (float) imageA.getMaxR();
            }

            if (imageA.getMinG() < 0.0) {
                maxColorA = Math.max((float) (imageA.getMaxG() - imageA.getMinG()), maxColorA);
                offsetAG = (float) (-imageA.getMinG());
            } else {
                maxColorA = Math.max((float) imageA.getMaxG(), maxColorA);
            }

            if (imageA.getMinB() < 0.0) {
                maxColorA = Math.max((float) (imageA.getMaxB() - imageA.getMinB()), maxColorA);
                offsetAB = (float) (-imageA.getMinB());
            } else {
                maxColorA = Math.max((float) imageA.getMaxB(), maxColorA);
            }
        }

        normColorA = 255 / maxColorA;

        if ((imageB != null) && (imageB.getType() == ModelStorageBase.ARGB_USHORT)) {
            maxColorB = (float) imageB.getMaxR();
            maxColorB = Math.max((float) imageB.getMaxG(), maxColorB);
            maxColorB = Math.max((float) imageB.getMaxB(), maxColorB);
        }

        if ((imageB != null) && (imageB.getType() == ModelStorageBase.ARGB_FLOAT)) {

            if (imageB.getMinR() < 0.0) {
                maxColorB = (float) (imageB.getMaxR() - imageB.getMinR());
                offsetBR = (float) (-imageB.getMinR());
            } else {
                maxColorB = (float) imageB.getMaxR();
            }

            if (imageB.getMinG() < 0.0) {
                maxColorB = Math.max((float) (imageB.getMaxG() - imageB.getMinG()), maxColorB);
                offsetBG = (float) (-imageB.getMinG());
            } else {
                maxColorB = Math.max((float) imageB.getMaxG(), maxColorB);
            }

            if (imageB.getMinB() < 0.0) {
                maxColorB = Math.max((float) (imageB.getMaxB() - imageB.getMinB()), maxColorB);
                offsetBB = (float) (-imageB.getMinB());
            } else {
                maxColorB = Math.max((float) imageB.getMaxB(), maxColorB);
            }
        }

        normColorB = 255 / maxColorB;

        if (RGBTA != null) {
            RGBIndexBufferA = RGBTA.exportIndexedRGB();
        }

        if ((imageB != null) && (RGBTB != null)) {
            RGBIndexBufferB = RGBTB.exportIndexedRGB();
        }

        if ((orientation == AXIAL) || (orientation == NA)) {

            if ((slice != zSlice) || (timeSlice != tSlice) || (forceShow == true)) {
                slice = zSlice;
                timeSlice = tSlice;

                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                } else {
                    timeSliceA = timeSlice;
                }

                if ((imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                } else {
                    timeSliceB = timeSlice;
                }

                int zDimSlices = 0;

                if (imageA.getNDims() >= 3) {
                    zDimSlices = imageExtents[2];
                }

                try {
                    imageA.exportData((timeSliceA * zDimSlices * bufferSize) + (slice * bufferSize), bufferSize,
                                      imageBufferA);

                    if (imageB != null) {
                        imageB.exportData((timeSliceB * zDimSlices * bufferSize) + (slice * bufferSize), bufferSize,
                                          imageBufferB);
                    }
                } catch (IOException error) {
                    MipavUtil.displayError("" + error);

                    return false;
                }

                if (useRedThreshold && useGreenThreshold) {

                    for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                        if ((imageBufferA[index + 1] < threshold1) || (imageBufferA[index + 2] < threshold2)) {
                            imageBufferA[index + 1] = 0;
                            imageBufferA[index + 2] = 0;
                            imageBufferA[index + 3] = 0;
                        }
                    }
                } // if (useRedThreshold && useGreenThreshold)
                else if (useRedThreshold && useBlueThreshold) {

                    for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                        if ((imageBufferA[index + 1] < threshold1) || (imageBufferA[index + 3] < threshold2)) {
                            imageBufferA[index + 1] = 0;
                            imageBufferA[index + 2] = 0;
                            imageBufferA[index + 3] = 0;
                        }
                    }
                } // else if (useRedThreshold && useBlueThreshold)
                else if (useGreenThreshold && useBlueThreshold) {

                    for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                        if ((imageBufferA[index + 2] < threshold1) || (imageBufferA[index + 3] < threshold2)) {
                            imageBufferA[index + 1] = 0;
                            imageBufferA[index + 2] = 0;
                            imageBufferA[index + 3] = 0;
                        }
                    }
                } // else if (useGreenThreshold && useBlueThreshold)
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

            if (imageB == null) {

                for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                    if (RGBTA != null) {

                        if (RGBTA.getROn()) {
                            redMapped = (RGBIndexBufferA[(int) ((imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                             0x00ff0000) >> 16;
                        } else {
                            redMapped = 0;
                        }

                        if (RGBTA.getGOn()) {
                            greenMapped = (RGBIndexBufferA[(int) ((imageBufferA[index + 2] + offsetAG) * normColorA)] &
                                               0x0000ff00) >> 8;
                        } else {
                            greenMapped = 0;
                        }

                        if (RGBTA.getBOn()) {
                            blueMapped = (RGBIndexBufferA[(int) ((imageBufferA[index + 3] + offsetAB) * normColorA)] &
                                              0x000000ff);
                        } else {
                            blueMapped = 0;
                        }
                    } // end of if (RGBTA != null)
                    else {
                        redMapped = (imageBufferA[index + 1] + offsetAR) * normColorA;
                        greenMapped = (imageBufferA[index + 2] + offsetAG) * normColorA;
                        blueMapped = (imageBufferA[index + 3] + offsetAB) * normColorA;
                    }

                    pixValue = 0xff000000 |
                                   (((int) (redMapped) << 16) | (((int) (greenMapped) << 8) | ((int) (blueMapped))));
                    cleanImageBufferA[j] = pixValue;

                } // end of for (index=0, j=0; j < imageSize; index += 4, j++)
            } // end of if (imageB == null )
            else { // imageB != null

                for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                    if ((RGBTA != null) && (RGBTB != null)) {

                        if (RGBTA.getROn()) {
                            Ra = (RGBIndexBufferA[(int) ((imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                      0x00ff0000) >> 16;
                        } else {
                            Ra = 0;
                        }

                        if (RGBTA.getGOn()) {
                            Ga = (RGBIndexBufferA[(int) ((imageBufferA[index + 2] + offsetAG) * normColorA)] &
                                      0x0000ff00) >> 8;
                        } else {
                            Ga = 0;
                        }

                        if (RGBTA.getBOn()) {
                            Ba = (RGBIndexBufferA[(int) ((imageBufferA[index + 3] + offsetAB) * normColorA)] &
                                      0x000000ff);
                        } else {
                            Ba = 0;
                        }

                        if (RGBTB.getROn()) {
                            Rb = (RGBIndexBufferB[(int) ((imageBufferB[index + 1] + offsetBR) * normColorB)] &
                                      0x00ff0000) >> 16;
                        } else {
                            Rb = 0;
                        }

                        if (RGBTB.getGOn()) {
                            Gb = (RGBIndexBufferB[(int) ((imageBufferB[index + 2] + offsetBG) * normColorB)] &
                                      0x0000ff00) >> 8;
                        } else {
                            Gb = 0;
                        }

                        if (RGBTB.getBOn()) {
                            Bb = (RGBIndexBufferB[(int) ((imageBufferB[index + 3] + offsetBB) * normColorB)] &
                                      0x000000ff);
                        } else {
                            Bb = 0;
                        }
                    } else {
                        Ra = (int) ((imageBufferA[index + 1] + offsetAR) * normColorA);
                        Ga = (int) ((imageBufferA[index + 2] + offsetAG) * normColorA);
                        Ba = (int) ((imageBufferA[index + 3] + offsetAB) * normColorA);

                        Rb = (int) ((imageBufferB[index + 1] + offsetBR) * normColorB);
                        Gb = (int) ((imageBufferB[index + 2] + offsetBG) * normColorB);
                        Bb = (int) ((imageBufferB[index + 3] + offsetBB) * normColorB);
                    }

                    pixValue = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                    cleanImageBufferA[j] = pixValue;
                    cleanImageBufferB[j] = 0xff000000 | (Rb << 16) | (Gb << 8) | Bb;

                } // end of for (index=0, j=0; j < imageSize; index += 4, j++)
            } // end of else for imageB != null
        } // end of if ((orientation == XY) || (orientation == NA))
        else if (orientation == CORONAL) {

            if ((slice != zSlice) || (timeSlice != tSlice) || (forceShow == true)) {
                slice = zSlice;
                timeSlice = tSlice;

                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                } else {
                    timeSliceA = timeSlice;
                }

                if ((imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                } else {
                    timeSliceB = timeSlice;
                }

                try {
                    imageA.exportRGBSliceXZ(timeSliceA, slice, imageBufferA);

                    if (imageB != null) {
                        imageB.exportRGBSliceXZ(timeSliceB, slice, imageBufferB);
                    }
                } catch (IOException error) {
                    MipavUtil.displayError("" + error);

                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

            if (imageB == null) {

                for (j = 0; j < zDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        ind4 = i + (xDim * j);
                        index = 4 * ind4;

                        if (RGBTA != null) {

                            if (RGBTA.getROn()) {
                                redMapped = (RGBIndexBufferA[(int) ((imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                                 0x00ff0000) >> 16;
                            } else {
                                redMapped = 0;
                            }

                            if (RGBTA.getGOn()) {
                                greenMapped = (RGBIndexBufferA[(int) ((imageBufferA[index + 2] + offsetAG) * normColorA)] &
                                                   0x0000ff00) >> 8;
                            } else {
                                greenMapped = 0;
                            }

                            if (RGBTA.getBOn()) {
                                blueMapped = (RGBIndexBufferA[(int) ((imageBufferA[index + 3] + offsetAB) * normColorA)] &
                                                  0x000000ff);
                            } else {
                                blueMapped = 0;
                            }
                        } // end of if (RGBTA != null)
                        else {
                            redMapped = (imageBufferA[index + 1] + offsetAR) * normColorA;
                            greenMapped = (imageBufferA[index + 2] + offsetAG) * normColorA;
                            blueMapped = (imageBufferA[index + 3] + offsetAB) * normColorA;
                        }

                        pixValue = 0xff000000 |
                                       (((int) (redMapped) << 16) | (((int) (greenMapped) << 8) | ((int) (blueMapped))));
                        cleanImageBufferA[ind4] = pixValue;

                    } // end of for (i = 0; i < xDim; i++)
                } // end of for (j = 0; j < zDim; j++)
            } // end of if (imageB == null )
            else { // imageB != null

                for (j = 0; j < zDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        ind4 = i + (xDim * j);
                        index = 4 * ind4;

                        if ((RGBTA != null) && (RGBTB != null)) {

                            if (RGBTA.getROn()) {
                                Ra = (RGBIndexBufferA[(int) ((imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                          0x00ff0000) >> 16;
                            } else {
                                Ra = 0;
                            }

                            if (RGBTA.getGOn()) {
                                Ga = (RGBIndexBufferA[(int) ((imageBufferA[index + 2] + offsetAG) * normColorA)] &
                                          0x0000ff00) >> 8;
                            } else {
                                Ga = 0;
                            }

                            if (RGBTA.getBOn()) {
                                Ba = (RGBIndexBufferA[(int) ((imageBufferA[index + 3] + offsetAB) * normColorA)] &
                                          0x000000ff);
                            } else {
                                Ba = 0;
                            }

                            if (RGBTB.getROn()) {
                                Rb = (RGBIndexBufferB[(int) ((imageBufferB[index + 1] + offsetBR) * normColorB)] &
                                          0x00ff0000) >> 16;
                            } else {
                                Rb = 0;
                            }

                            if (RGBTB.getGOn()) {
                                Gb = (RGBIndexBufferB[(int) ((imageBufferB[index + 2] + offsetBG) * normColorB)] &
                                          0x0000ff00) >> 8;
                            } else {
                                Gb = 0;
                            }

                            if (RGBTB.getBOn()) {
                                Bb = (RGBIndexBufferB[(int) ((imageBufferB[index + 3] + offsetBB) * normColorB)] &
                                          0x000000ff);
                            } else {
                                Bb = 0;
                            }
                        } else {
                            Ra = (int) ((imageBufferA[index + 1] + offsetAR) * normColorA);
                            Ga = (int) ((imageBufferA[index + 2] + offsetAG) * normColorA);
                            Ba = (int) ((imageBufferA[index + 3] + offsetAB) * normColorA);

                            Rb = (int) ((imageBufferB[index + 1] + offsetBR) * normColorB);
                            Gb = (int) ((imageBufferB[index + 2] + offsetBG) * normColorB);
                            Bb = (int) ((imageBufferB[index + 3] + offsetBB) * normColorB);
                        }

                        pixValue = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                        cleanImageBufferA[ind4] = pixValue;
                        cleanImageBufferB[ind4] = 0xff000000 | (Rb << 16) | (Gb << 8) | Bb;

                    } // end of for (i = 0; i < xDim; i++)
                } // end of for (j = 0; j < zDim; j++)
            } // end of else for imageB != null
        } // end of else if (orientation == XZ)
        else { // for orientation == ZY

            if ((slice != zSlice) || (timeSlice != tSlice) || (forceShow == true)) {
                slice = zSlice;
                timeSlice = tSlice;

                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                } else {
                    timeSliceA = timeSlice;
                }

                if ((imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                } else {
                    timeSliceB = timeSlice;
                }

                try {
                    imageA.exportRGBSliceZY(timeSliceA, slice, imageBufferA);

                    if (imageB != null) {
                        imageB.exportRGBSliceZY(timeSliceB, slice, imageBufferB);
                    }
                } catch (IOException error) {
                    MipavUtil.displayError("" + error);

                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice !=   || forceShow == true)

            if (imageB == null) {

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < zDim; i++) {
                        ind4 = (j * zDim) + i;
                        index = 4 * ind4;

                        if (RGBTA != null) {

                            if (RGBTA.getROn()) {
                                redMapped = (RGBIndexBufferA[(int) ((imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                                 0x00ff0000) >> 16;
                            } else {
                                redMapped = 0;
                            }

                            if (RGBTA.getGOn()) {
                                greenMapped = (RGBIndexBufferA[(int) ((imageBufferA[index + 2] + offsetAG) * normColorA)] &
                                                   0x0000ff00) >> 8;
                            } else {
                                greenMapped = 0;
                            }

                            if (RGBTA.getBOn()) {
                                blueMapped = (RGBIndexBufferA[(int) ((imageBufferA[index + 3] + offsetAB) * normColorA)] &
                                                  0x000000ff);
                            } else {
                                blueMapped = 0;
                            }
                        } // end of if (RGBTA != null)
                        else {
                            redMapped = (imageBufferA[index + 1] + offsetAR) * normColorA;
                            greenMapped = (imageBufferA[index + 2] + offsetAG) * normColorA;
                            blueMapped = (imageBufferA[index + 3] + offsetAB) * normColorA;
                        }

                        pixValue = 0xff000000 |
                                       (((int) (redMapped) << 16) | (((int) (greenMapped) << 8) | ((int) (blueMapped))));

                        cleanImageBufferA[ind4] = pixValue;

                    } // end of for (i = 0; i < zDim; i++)
                } // end of for (j = 0; j < yDim; j++)
            } // end of if (imageB == null )
            else { // imageB != null

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < zDim; i++) {
                        ind4 = (j * zDim) + i;
                        index = 4 * ind4;

                        if ((RGBTA != null) && (RGBTB != null)) {

                            if (RGBTA.getROn()) {
                                Ra = (RGBIndexBufferA[(int) ((imageBufferA[index + 1] + offsetAR) * normColorA)] &
                                          0x00ff0000) >> 16;
                            } else {
                                Ra = 0;
                            }

                            if (RGBTA.getGOn()) {
                                Ga = (RGBIndexBufferA[(int) ((imageBufferA[index + 2] + offsetAG) * normColorA)] &
                                          0x0000ff00) >> 8;
                            } else {
                                Ga = 0;
                            }

                            if (RGBTA.getBOn()) {
                                Ba = (RGBIndexBufferA[(int) ((imageBufferA[index + 3] + offsetAB) * normColorA)] &
                                          0x000000ff);
                            } else {
                                Ba = 0;
                            }

                            if (RGBTB.getROn()) {
                                Rb = (RGBIndexBufferB[(int) ((imageBufferB[index + 1] + offsetBR) * normColorB)] &
                                          0x00ff0000) >> 16;
                            } else {
                                Rb = 0;
                            }

                            if (RGBTB.getGOn()) {
                                Gb = (RGBIndexBufferB[(int) ((imageBufferB[index + 2] + offsetBG) * normColorB)] &
                                          0x0000ff00) >> 8;
                            } else {
                                Gb = 0;
                            }

                            if (RGBTB.getBOn()) {
                                Bb = (RGBIndexBufferB[(int) ((imageBufferB[index + 3] + offsetBB) * normColorB)] &
                                          0x000000ff);
                            } else {
                                Bb = 0;
                            }
                        } else {
                            Ra = (int) ((imageBufferA[index + 1] + offsetAR) * normColorA);
                            Ga = (int) ((imageBufferA[index + 2] + offsetAG) * normColorA);
                            Ba = (int) ((imageBufferA[index + 3] + offsetAB) * normColorA);

                            Rb = (int) ((imageBufferB[index + 1] + offsetBR) * normColorB);
                            Gb = (int) ((imageBufferB[index + 2] + offsetBG) * normColorB);
                            Bb = (int) ((imageBufferB[index + 3] + offsetBB) * normColorB);
                        }

                        pixValue = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                        cleanImageBufferA[ind4] = pixValue;
                        cleanImageBufferB[ind4] = 0xff000000 | (Rb << 16) | (Gb << 8) | Bb;

                    } // end of for (i = 0; i < zDim; i++)
                } // end of for (j = 0; j < yDim; j++)
            } // end of else for imageB != null
        } // end of else for orientation == ZY

        // If these cleanBuffer are removed, 3D color image slices will not change
        // in the screen display as the slice slider moves
        // after a replace function has been performed on a color image.
        cleanBuffer(IMAGE_A);
        cleanBuffer(IMAGE_B);

        return true;
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
     * set the color of the grid.
     *
     * @param  color  Color
     */
    public void setGridColor(Color color) {
        this.gridColor = color;
    }

    /**
     * Tells the grid overlay (if on) to show abc/123 labeling
     * @param doLabel boolean
     */
    public void setGridLabelingOn(boolean doLabel) {
        this.gridLabelingOn = doLabel;
    }

    /**
     * Whether or not labels should be drawn on the grid overlay
     * @return boolean
     */
    public boolean getGridLabeling() {
        return gridLabelingOn;
    }

    /**
     * Sets the axis orientation of abc and 123 labeling of the grid overlay
     * @param or boolean true = x-axis numbered, false = x-axis lettered
     */
    public void setGridLabelOrientation(boolean or) {
        this.gridLabelOrientation = or;
    }

    public boolean getGridLabelOrientation() {
        return gridLabelOrientation;
    }


    /**
     * Sets whether or not to show the NEI grid overlay.
     *
     * @param  flag  boolean show grid overlay (or not!)
     */
    public void setGridOverlay(boolean flag) {
        gridOverlayOn = flag;
    }

    /**
     * sets the grid spacing (horizontal) in terms of resolution.
     *
     * @param  spacing  float new grid spacing
     */
    public void setGridSpacingX(float spacing) {
        this.gridSpacingX = spacing;
    }

    /**
     * sets the grid spacing (horizontal) in terms of resolution.
     *
     * @param  spacing  float new grid spacing
     */
    public void setGridSpacingY(float spacing) {
        this.gridSpacingY = spacing;
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
        this.hasThreshold1 = hasThreshold1;
    }

    /**
     * Sets the hasThreshold2 for setPaintBuffers.
     *
     * @param  hasThreshold2  whether the paint buffer has a threshold2
     */
    public void setHasThreshold2(boolean hasThreshold2) {
        this.hasThreshold2 = hasThreshold2;
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
        setZoom(1, 1); // sets zoom
    }

    /**
     * Sets component's ImageB. !!!!!! assumes dimensionality same as image A's for now will fix soon.
     *
     * @param  image  imageB
     */
    public void setImageB(ModelImage image) {
        imageB = image;

        if (imageB == null) {

            // remove checker boarding
            rowCheckers = -1;
            columnCheckers = -1;
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
        this.imageColocalize = imageColocalize;
        imageBufferColocalize = new float[imageBufferA.length];
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
    }

    /**
     * Sets the model LUTb for the imageB.
     *
     * @param  LUT  the model LUT
     */
    public void setLUTb(ModelLUT LUT) {
        LUTb = LUT;
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
     * Switches modes based on the variable mode. Sets voiHandler.getRubberband() activity and the cursor.
     *
     * @param  mode  the integer mode
     */
    public void setMode(int mode) {
        this.mode = mode;
        voiHandler.setMode(mode);

        switch (mode) {

            case DEFAULT:
            case PROBE:
            case SELECT:
            case POINT_VOI:
            case RECTANGLE:
            case ELLIPSE:
            case RECTANGLE3D:
            case NEW_VOI:
            case POLYLINE:
            case LIVEWIRE:
            case LEVELSET:
            case LINE:
            case PROTRACTOR:
            case MOVE:
            case MOVE_POINT:
            case NEW_POINT:
            case POLYLINE_SLICE_VOI:
            case DELETE_POINT:
            case WAND:
            case RETRACE:
            case PAINT_CAN:
            case PAINT_VASC:
            case DROPPER_PAINT:
            case ERASER_PAINT:
            case MAG_REGION:
            case WIN_REGION:
            case QUICK_LUT:
            case ANNOTATION:
            case ZOOMING_IN:
            case ZOOMING_OUT:
                break;

        }
    }



    /**
     *
     * @return boolean
     */
    public boolean getModifyFlag() {
        return modifyFlag;
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
     * Sets the orientation of the image.
     *
     * @param  _orientation  orientaiton of image slice to be displayed
     */
    public void setOrientation(int _orientation) {
        orientation = _orientation;
    }

    /**
     * Sets whether or not to show the overlay.
     *
     * @param  flag  boolean that tells whether or not to show the overlay
     */
    public void setOverlay(boolean flag) {
        overlayOn = flag;
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
     * Sets the paint buffers and the Java image to be displayed.
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   _LUTa      LUTa - to change to new LUT for imageA else null
     * @param   _LUTb      LUTb - to change to new LUT for imageB else null
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean setPaintBuffers(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow) {
        float imageMinA = 0;
        int xDim, yDim, zDim;
        int bufferSize;
        int lutHeightA = 0;
        int index = 0;
        float[][] RGB_LUTa = null, RGB_LUTb = null;
        int[][] iRGB_LUTa = null, iRGB_LUTb = null;
        int Ra, Ga, Ba;
        int indexA, indexB;
        int pix = 0;
        int i, j;

        if (imageA == null) {
            return false;
        }

        if ((LUTa == null) && (_LUTb == null)) {
            return false;
        }

        if (_LUTa != null) {
            LUTa = _LUTa;
        }

        if ((imageB != null) && (_LUTb != null)) {
            LUTb = _LUTb;
        }

        lutHeightA = LUTa.getExtents()[1];

        // System.err.println("SHOW IN VJCOMP\n" + LUTa.toString());

        xDim = imageExtents[0];
        yDim = imageExtents[1];
        zDim = 1;

        if (imageA.getNDims() >= 3) {
            zDim = imageExtents[2];
        }

        if (lutHeightA != lutBufferRemapped.length) {

            try {
                lutBufferRemapped = new int[lutHeightA];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentEditImage.show");

                return false;
            }
        }

        if (imageB == null) {
            LUTa.exportIndexedLUT(lutBufferRemapped);
            // lutBufferRemapped = LUTa.exportIndexedLUT_Remove();
        }

        if ((orientation == AXIAL) || (orientation == NA)) {
            bufferSize = xDim * yDim;
        } else if (orientation == CORONAL) {
            bufferSize = xDim * zDim;
        } else { // orientation == ZY
            bufferSize = zDim * yDim;
        }

        if (imageB != null) {
            RGB_LUTa = LUTa.exportRGB_LUT(true);
            RGB_LUTb = LUTb.exportRGB_LUT(true);
            iRGB_LUTa = new int[3][RGB_LUTa[0].length];
            iRGB_LUTb = new int[3][RGB_LUTb[0].length];

            for (int c = 0; c < RGB_LUTa[0].length; c++) {
                iRGB_LUTa[0][c] = (int) (RGB_LUTa[0][c] + 0.5f);
                iRGB_LUTb[0][c] = (int) (RGB_LUTb[0][c] + 0.5f);
                iRGB_LUTa[1][c] = (int) (RGB_LUTa[1][c] + 0.5f);
                iRGB_LUTb[1][c] = (int) (RGB_LUTb[1][c] + 0.5f);
                iRGB_LUTa[2][c] = (int) (RGB_LUTa[2][c] + 0.5f);
                iRGB_LUTb[2][c] = (int) (RGB_LUTb[2][c] + 0.5f);
            }
        }

        if ((orientation == AXIAL) || (orientation == NA)) {

            if ((slice != zSlice) || (timeSlice != tSlice) || (forceShow == true)) {
                slice = zSlice;
                timeSlice = tSlice;

                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                } else {
                    timeSliceA = timeSlice;
                }

                if ((imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                } else {
                    timeSliceB = timeSlice;
                }

                int zDimSlices = 0;

                if (imageA.getNDims() >= 3) {
                    zDimSlices = imageExtents[2];
                }

                try {

                    if (imageA.getType() == ModelStorageBase.COMPLEX) {
                        imageA.exportComplexSliceXY((timeSliceA * zDimSlices) + slice, imageBufferA,
                                                    imageA.getLogMagDisplay());
                    } else {
                        imageA.exportSliceXY((timeSliceA * zDimSlices) + slice, imageBufferA);
                    }

                    if (imageB != null) {

                        if (imageB.getType() == ModelStorageBase.COMPLEX) {
                            imageB.exportComplexSliceXY((timeSliceB * zDimSlices) + slice, imageBufferB,
                                                        imageA.getLogMagDisplay());
                        } else {
                            imageB.exportSliceXY((timeSliceB * zDimSlices) + slice, imageBufferB);
                        }
                    }

                    if (hasThreshold1 || hasThreshold2) {
                        imageColocalize.exportSliceXY((timeSliceA * zDimSlices) + slice, imageBufferColocalize);
                    }
                } catch (IOException error) {
                    Preferences.debug("IOException in ViewJComponentEditImage:setPaintBuffers " + error);

                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

            if (hasThreshold1) {

                for (i = 0; i < imageBufferA.length; i++) {

                    if ((imageBufferA[i] < threshold1) || (imageBufferColocalize[i] < threshold2)) {
                        imageBufferA[i] = imageMinA;
                    }
                }
            } // if (hasThreshold1)
            else if (hasThreshold2) {

                for (i = 0; i < imageBufferA.length; i++) {

                    if ((imageBufferColocalize[i] < threshold1) || (imageBufferA[i] < threshold2)) {
                        imageBufferA[i] = imageMinA;
                    }
                }
            } // else if (hasThreshold2)

            if (imageB == null) {
                pix = 0;

                TransferFunction tf_imgA = LUTa.getTransferFunction();

                for (index = 0; index < bufferSize; index++) {
                    pix = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                    cleanImageBufferA[index] = lutBufferRemapped[pix];
                } // end of for (index=0; index < bufferSize; index++)
            } // end of if (imageB == null)
            else { // imageB != null
                indexA = indexB = 0;

                TransferFunction tf_imgA = LUTa.getTransferFunction();
                TransferFunction tf_imgB = LUTb.getTransferFunction();

                for (index = 0; index < bufferSize; index++) {
                    indexA = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                    indexB = (int) (tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);

                    Ra = iRGB_LUTa[0][indexA];
                    Ga = iRGB_LUTa[1][indexA];
                    Ba = iRGB_LUTa[2][indexA];

                    pix = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                    cleanImageBufferA[index] = pix;
                    cleanImageBufferB[index] = (0xff000000) | ((int) (RGB_LUTb[0][indexB]) << 16) |
                                                   ((int) (RGB_LUTb[1][indexB]) << 8) | (int) (RGB_LUTb[2][indexB]);
                } // end of for (index=0; index < bufferSize; index++)
            } // end of else for imageB != null
        } // end of if ((orientation == XY) || (orientation == NA))
        else if (orientation == CORONAL) {

            if ((slice != zSlice) || (timeSlice != tSlice) || (forceShow == true)) {
                slice = zSlice;
                timeSlice = tSlice;

                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                } else {
                    timeSliceA = timeSlice;
                }

                if ((imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                } else {
                    timeSliceB = timeSlice;
                }

                try {

                    if (imageA.getType() == ModelStorageBase.COMPLEX) {
                        imageA.exportComplexSliceXZ(timeSliceA, slice, imageBufferA, imageA.getLogMagDisplay());
                    } else {
                        imageA.exportSliceXZ(timeSliceA, slice, imageBufferA);
                    }

                    if (imageB != null) {

                        if (imageB.getType() == ModelStorageBase.COMPLEX) {
                            imageB.exportComplexSliceXZ(timeSliceB, slice, imageBufferB, imageA.getLogMagDisplay());
                        } else {
                            imageB.exportSliceXZ(timeSliceB, slice, imageBufferB);
                        }
                    }
                } catch (IOException error) {
                    Preferences.debug("IOException in ViewJComponentEditImage:setPaintBuffers (else if orientation == XZ)"); // Need to fix this

                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

            if (imageB == null) {
                pix = 0;

                TransferFunction tf_imgA = LUTa.getTransferFunction();

                for (j = 0; j < zDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        index = i + (xDim * j);
                        pix = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                        cleanImageBufferA[index] = lutBufferRemapped[pix];
                    } // end of for (i = 0; i < xDim; i++)
                } // end of for (j = 0; j < zDim; j++)
            } // end of if (imageB == null)
            else { // imageB != null
                indexA = indexB = 0;

                TransferFunction tf_imgA = LUTa.getTransferFunction();
                TransferFunction tf_imgB = LUTb.getTransferFunction();

                try {

                    for (j = 0; j < zDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            index = i + (xDim * j);
                            indexA = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                            indexB = (int) (tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);

                            Ra = iRGB_LUTa[0][indexA];
                            Ga = iRGB_LUTa[1][indexA];
                            Ba = iRGB_LUTa[2][indexA];

                            pix = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;

                            cleanImageBufferA[index] = pix;
                            cleanImageBufferB[index] = (0xff000000) | ((int) (RGB_LUTb[0][indexB]) << 16) |
                                                           ((int) (RGB_LUTb[1][indexB]) << 8) |
                                                           (int) (RGB_LUTb[2][indexB]);
                        } // end of for (i = 0; i < xDim; i++)
                    } // end of for (j = 0; j < zDim; j++)
                } catch (Exception e) {
                    System.out.println("indexA == " + indexA);
                    System.out.println("index == " + index);
                    System.out.println("pix == " + pix);
                    e.printStackTrace();
                }
            } // end of else for imageB != null
        } // end of else if (orientation == XZ)
        else { // orientation == ZY

            if ((slice != zSlice) || (timeSlice != tSlice) || (forceShow == true)) {
                slice = zSlice;
                timeSlice = tSlice;

                if (imageA.getNDims() < 4) {
                    timeSliceA = 0;
                } else {
                    timeSliceA = timeSlice;
                }

                if ((imageB != null) && (imageB.getNDims() < 4)) {
                    timeSliceB = 0;
                } else {
                    timeSliceB = timeSlice;
                }

                try {

                    if (imageA.getType() == ModelStorageBase.COMPLEX) {
                        imageA.exportComplexSliceZY(timeSliceA, slice, imageBufferA, imageA.getLogMagDisplay());
                    } else {
                        imageA.exportSliceZY(timeSliceA, slice, imageBufferA);
                    }

                    if (imageB != null) {

                        if (imageB.getType() == ModelStorageBase.COMPLEX) {
                            imageB.exportComplexSliceZY(timeSliceB, slice, imageBufferB, imageA.getLogMagDisplay());
                        } else {
                            imageB.exportSliceZY(timeSliceB, slice, imageBufferB);
                        }
                    }
                } catch (IOException error) {
                    Preferences.debug("" + error); // Need to fix this

                    return false;
                }
            } // end of if ( slice != zSlice || timeSlice != tSlice || forceShow == true)

            if (imageB == null) {
                pix = 0;

                TransferFunction tf_imgA = LUTa.getTransferFunction();

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < zDim; i++) {
                        index = (j * zDim) + i;
                        pix = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                        cleanImageBufferA[index] = lutBufferRemapped[pix];
                    } // end of for (i = 0; i < zDim; i++)
                } // end of for (j = 0; j < yDim; j++)
            } // end of if (imageB == null)
            else { // imageB != null

                indexA = indexB = 0;

                TransferFunction tf_imgA = LUTa.getTransferFunction();
                TransferFunction tf_imgB = LUTb.getTransferFunction();

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < zDim; i++) {
                        index = (j * zDim) + i;

                        indexA = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                        indexB = (int) (tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);

                        Ra = iRGB_LUTa[0][indexA];
                        Ga = iRGB_LUTa[1][indexA];
                        Ba = iRGB_LUTa[2][indexA];

                        pix = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                        cleanImageBufferA[index] = pix;
                        cleanImageBufferB[index] = (0xff000000) | ((int) (RGB_LUTb[0][indexB]) << 16) |
                                                       ((int) (RGB_LUTb[1][indexB]) << 8) | (int) (RGB_LUTb[2][indexB]);
                    } // end of for (i = 0; i < zDim; i++)
                } // end of for (j = 0; j < yDim; j++)
            } // end of else for imageB != null
        } // end of else for orientation == ZY

        cleanBuffer(IMAGE_A);
        cleanBuffer(IMAGE_B);

        return true;
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
    }

    /**
     * Sets the RGB table for ARGB image B.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTB(ModelRGB RGBT) {
        RGBTB = RGBT;
    }

    /**
     * Sets position data to display in message bar - for DICOM and MINC images, gives patient position as well. The
     * image's associated transformation must be FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL or the function returns null.
     *
     * @param   fileInfo  File info object of image displayed.
     * @param   x         Event that triggered this call.
     * @param   y         DOCUMENT ME!
     * @param   zSlice    Index to slice in the Z-plane.
     *
     * @return  An array of strings that represent patient position.
     */
    public String[] setScannerPosition(FileInfoBase fileInfo, int x, int y, int zSlice) {

        DecimalFormat nf = new DecimalFormat("#####0.0##");
        float[] tCoord = new float[3];
        String[] strs = new String[3];

        // Get the voxel coordinate in from mouse events in image space
        int z = zSlice;

        imageActive.getScannerCoordLPS(x, y, z, tCoord);

        if (tCoord[0] < 0) {
            strs[0] = "R: " + String.valueOf(nf.format(-tCoord[0]));
        } else {
            strs[0] = "L: " + String.valueOf(nf.format(tCoord[0]));
        }

        if (tCoord[1] < 0) {
            strs[1] = "A: " + String.valueOf(nf.format(-tCoord[1]));
        } else {
            strs[1] = "P: " + String.valueOf(nf.format(tCoord[1]));
        }

        if (tCoord[2] < 0) {
            strs[2] = "I: " + String.valueOf(nf.format(-tCoord[2]));
        } else {
            strs[2] = "S: " + String.valueOf(nf.format(tCoord[2]));
        }

        return strs;
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
        this.useRedThreshold = useRedThreshold;
        this.useGreenThreshold = useGreenThreshold;
        this.useBlueThreshold = useBlueThreshold;
    }

    /**
     * Sets the thresholds.
     *
     * @param  threshold1  the first threshold
     * @param  threshold2  the second threshold
     */
    public void setThresholds(float threshold1, float threshold2) {
        this.threshold1 = threshold1;
        this.threshold2 = threshold2;
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
     * For generating the display of 1 or 2 RGB images.
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, boolean forceShow) {

        if (setColorPaintBuffers(tSlice, zSlice, forceShow)) {

            if (showSliceNumber) {

                if (stringOverride == null) {
                    setSliceString(String.valueOf(zSlice + 1));
                } else {
                    setSliceString(stringOverride);
                }
            } else {
                setSliceString("");
            }

            paintComponent(getGraphics());

            return true;
        } else {
            return false;
        }
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

        if (imageA.isColorImage()) {

            // call the show method for displaying RGB images
            return (show(tSlice, zSlice, forceShow));
        }

        if (setPaintBuffers(tSlice, zSlice, _LUTa, _LUTb, forceShow)) {
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
                area = count * fileInfo[0].getResolutions()[0] * fileInfo[0].getResolutions()[1];
                str = imageActive.getFileInfo(0).getAreaUnitsOfMeasureStr();

                if (leadString != null) {
                    frame.getUserInterface().setDataText(leadString + " region grow: pixels = " + count +
                                                         "\t  area = " + area + str + "\n");
                } else {
                    frame.getUserInterface().setDataText("Region grow: pixels = " + count + "\t  area = " + area + str +
                                                         "\n");
                }

            } else {
                volume = count * fileInfo[0].getResolutions()[0] * fileInfo[0].getResolutions()[1] *
                             fileInfo[0].getResolutions()[2];

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
                imageStatList = new JDialogVOIStatistics(this.getActiveImage().getUserInterface(),
                                                         imageActive.getVOIs());
                imageStatList.setVisible(true);
                // addVOIUpdateListener(imageStatList); // i'd rather not do it this way...
            } else {
                MipavUtil.displayError("A VOI must be present to use the statistics calculator");
            }
        } else {
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
     * Converts the screen cursor x-coordinate to scaled version using the image's pixel resolution and zoom.
     *
     * @param   x  the cursor's x-coordinate
     *
     * @return  x-coordinate in image space
     */
    public int getScaledX(int x) {
        return (int) (x / (getZoomX() * resolutionX));
    }

    /**
     * Converts the screen cursor y-coordinate to scaled version using the image's pixel resolution and zoom.
     *
     * @param   y  the cursor's y-coordinate
     *
     * @return  x-coordinate in image space
     */
    public int getScaledY(int y) {
        return (int) (y / (getZoomY() * resolutionY));
    }

    /**
     * DOCUMENT ME!
     *
     * @param   reverse  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected float getZoomMagnitudeX(boolean reverse) {

        if (zoomMode == LINEAR) {

            if (mode == ZOOMING_IN) {

                if (reverse) {

                    if (getZoomX() <= 1.0f) {
                        return getZoomX() * 0.5f; // to prevent zooming to 0
                    }

                    return getZoomX() - 1.0f;
                } else {
                    return getZoomX() + 1.0f;
                }
            } else { // mode == ZOOMING_OUT

                if (reverse) {
                    return getZoomX() + 1.0f;
                } else {

                    if (getZoomX() <= 1.0f) {
                        return getZoomX() * 0.5f; // to prevent zooming to 0
                    }

                    return getZoomX() - 1.0f;
                }

            }
        } else { // zoomMode == EXPONENTIAL

            if (mode == ZOOMING_IN) {

                if (reverse) {
                    return getZoomX() * 0.5f;
                } else {
                    return getZoomX() * 2.0f;
                }
            } else { // mode == ZOOMING_OUT

                if (reverse) {
                    return getZoomX() * 2.0f;
                } else {
                    return getZoomX() * 0.5f;
                }
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   reverse  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected float getZoomMagnitudeY(boolean reverse) {

        if (zoomMode == LINEAR) {

            if (mode == ZOOMING_IN) {

                if (reverse) {

                    if (getZoomY() <= 1.0f) {
                        return getZoomY() * 0.5f; // to prevent zooming to 0
                    }

                    return getZoomY() - 1.0f;
                } else {
                    return getZoomY() + 1.0f;
                }
            } else { // mode == ZOOMING_OUT

                if (reverse) {
                    return getZoomY() + 1.0f;
                } else {

                    if (getZoomY() <= 1.0f) {
                        return getZoomY() * 0.5f; // to prevent zooming to 0
                    }

                    return getZoomY() - 1.0f;
                }

            }
        } else { // zoomMode == EXPONENTIAL

            if (mode == ZOOMING_IN) {

                if (reverse) {
                    return getZoomY() * 0.5f;
                } else {
                    return getZoomY() * 2.0f;
                }
            } else { // mode == ZOOMING_OUT

                if (reverse) {
                    return getZoomY() * 2.0f;
                } else {
                    return getZoomY() * 0.5f;
                }
            }
        }
    }

    /**
     * This method adds an opaque border to the painted area on-screen.
     *
     * @param  paintImageBuffer  int[] the buffer the paint image will be created from
     */
    protected void makePaintBitmapBorder(int[] paintImageBuffer) {
        int i, j;
        int idx;
        int xDim = imageDim.width;
        int yDim = imageDim.height;

        // Top row of image
        for (i = 0; i < (xDim - 1); i++) {

            if (paintImageBuffer[i] != 0) {
                paintImageBuffer[i] = paintImageBuffer[i] | 0xff000000;
            }
        }

        // Bottom row of image
        for (i = (yDim - 1) * xDim; i < (xDim * yDim); i++) {

            if (paintImageBuffer[i] != 0) {
                paintImageBuffer[i] = paintImageBuffer[i] | 0xff000000;
            }
        }

        // Left column of image
        for (i = 0; i < (xDim * yDim); i += xDim) {

            if (paintImageBuffer[i] != 0) {
                paintImageBuffer[i] = paintImageBuffer[i] | 0xff000000;
            }
        }

        // Right column of image
        for (i = (xDim - 1); i < (xDim * yDim); i += xDim) {

            if (paintImageBuffer[i] != 0) {
                paintImageBuffer[i] = paintImageBuffer[i] | 0xff000000;
            }
        }

        for (j = 1; j < (imageDim.height - 1); j++) {

            for (i = 1; i < (imageDim.width - 1); i++) {

                idx = (j * imageDim.width) + i;

                int left = idx - 1;
                int right = idx + 1;
                int above = idx - imageDim.width;
                int below = idx + imageDim.width;
                int northwest = above - 1;
                int northeast = above + 1;
                int southeast = below + 1;
                int southwest = below - 1;

                if (paintImageBuffer[idx] != 0) {

                    if (paintImageBuffer[above] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[below] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[left] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[right] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[northwest] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[northeast] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[southeast] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }

                    if (paintImageBuffer[southwest] == 0) {
                        paintImageBuffer[idx] = paintImageBuffer[idx] | 0xff000000;

                        continue;
                    }
                }
            }
        }
    }


    /**
     * This method creates a buffer that will be used to make an image of the painted area on-screen.
     *
     * @param  paintImageBuffer  int[] the buffer to fill that will make the paint image
     */
    protected void makePaintImage(int[] paintImageBuffer) {

        // get the color of the paint the user has selected
        int color = getSelectedPaintColor();
        float opacity = 0.3f;

        try {
            opacity = frame.getControls().getTools().getOpacity();
        } catch (Exception e) {
            /* do nothing, opactiy defaults to 0.3f */
        } // should be changed later to a more elegant solution, since this always fails when 'frame' is a
          // ViewJFrameLightBox

        int opacityInt = (int) (opacity * 255);
        opacityInt = opacityInt << 24;

        // this loop converts the paint mask from a BitSet into an image
        // buffer that will be drawn on-screen
        if (imageExtents.length < 3) {

            // for 2D images
            for (int j = 0; j < paintBitmap.length(); j++) {

                if (paintBitmap.get(j) == true) {
                    color = color & 0x00ffffff;
                    paintImageBuffer[j] = color | opacityInt;
                }
            }
        } else if (slice >= 0) {

            // for 3D images
            int j = slice * imageDim.width * imageDim.height;
            int numPixelsInOneSlice = imageDim.width * imageDim.height;
            int offset = numPixelsInOneSlice * slice;
            int numIterations = numPixelsInOneSlice + offset;

            for (; j < numIterations; j++) {

                if (paintBitmap.get(j) == true) {
                    color = color & 0x00ffffff;
                    paintImageBuffer[j - offset] = color | opacityInt;
                }
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
        int xS = getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        int yS = getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        int brushSize = getBrushSize();
        int hBrushSize = getHBrushSize();

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

        // imageActive.notifyImageDisplayListeners();
    }



    /**
<<<<<<< .mine
=======
     * Displays Image overlays (DICOM or image attributes).
     *
     * @param  g  Graphics object used to paint and display the strings.
     */
    protected void showOverlay(Graphics g) {

        String[] overlays = new String[16];
        String[] overlayNames = new String[16];

        if (imageActive.getFileInfo(0) instanceof FileInfoDicom) {
            FileInfoDicom fileInfo;

            if (slice >= 0) {
                fileInfo = (FileInfoDicom) (imageActive.getFileInfo())[slice];
            } else {
                fileInfo = (FileInfoDicom) (imageActive.getFileInfo())[0];
            }

            String[] dicomKeys = Preferences.getOverlays(true);
            overlayNames = Preferences.getOverlayNames(true);

            for (int i = 0; i < 16; i++) {

                if ((dicomKeys[i] != null) && !dicomKeys[i].equals("-")) {
                    overlays[i] = buildOverlayStrings(fileInfo, overlayNames[i], dicomKeys[i]);
                }
            }

            Insets insets = frame.getInsets();
            int rightOffset = getBounds().width - insets.left;
            int bottomOffset = getBounds().height - insets.bottom - 15;

            int len;

            for (int i = 0; i < 16; i++) {

                if (overlays[i] != null) {
                    len = g.getFontMetrics(g.getFont()).stringWidth(overlays[i]);

                    if (i < 4) {
                        drawStringBW(overlays[i], g, 5, (15 * (i + 1)));
                    } else if ((i > 3) && (i < 8)) {
                        drawStringBW(overlays[i], g, rightOffset - len, (15 * ((i % 4) + 1)));
                    } else if ((i > 7) && (i < 12)) {
                        drawStringBW(overlays[i], g, 5, bottomOffset - 45 + (i % 4 * 15));
                    } else if (i > 11) {
                        drawStringBW(overlays[i], g, rightOffset - len, bottomOffset - 45 + (i % 4 * 15));
                    }
                }
            }

            drawGradicules(g, fileInfo.getResolutions()[0], fileInfo.getResolutions()[1]);

            // At the momment we are using the reconDimension - why not use
            // ~reconDim = imageActive.getExtents()[0]*imageActive.getResolutions()[0]; ??
            float reconDiameter;

            try {
                reconDiameter = Float.valueOf((String) (fileInfo.getTag("0018,1100").getValue(true))).floatValue();
            } catch (Exception ex) {
                reconDiameter = imageActive.getExtents()[0] * fileInfo.getResolutions()[0];
            }

            String[] values = setOverlayValues(imageActive.getImageOrientation(), MipavMath.round(fileInfo.xLocation),
                                               MipavMath.round(fileInfo.yLocation), MipavMath.round(fileInfo.zLocation),
                                               MipavMath.round(reconDiameter));

            int index = values[0].length() / 2;

            for (int i = 0; i < values[0].length(); i++) {
                drawStringBW(String.valueOf(values[0].charAt(i)), g, 5, (getHeight() / 2) - ((index - i) * 15));
            }

            index = values[1].length() / 2;

            for (int i = 0; i < values[1].length(); i++) {
                drawStringBW(String.valueOf(values[1].charAt(i)), g, rightOffset - 10,
                             (getHeight() / 2) - ((index - i) * 15));
            }

            len = g.getFontMetrics(g.getFont()).stringWidth(values[2]);
            drawStringBW(values[2], g, (getWidth() / 2) - (len / 2), 15);
            len = g.getFontMetrics(g.getFont()).stringWidth(values[3]);
            drawStringBW(values[3], g, (getWidth() / 2) - (len / 2), bottomOffset);
            drawStringBW(values[4], g, 5, 75);
        } else {
            FileInfoBase fileInfo;

            if (slice >= 0) {
                fileInfo = imageActive.getFileInfo()[slice];
            } else {
                fileInfo = imageActive.getFileInfo()[0];
            }

            String[] attribs = Preferences.getOverlays(false);

            overlayNames = Preferences.getOverlayNames(false);

            for (int i = 0; i < 16; i++) {

                if ((attribs[i] != null) && !attribs[i].equals("-")) {
                    overlays[i] = buildOverlayStrings(fileInfo, overlayNames[i], attribs[i]);
                }
            }

            Insets insets = frame.getInsets();
            int rightOffset = getBounds().width - insets.left;
            int bottomOffset = getBounds().height - insets.bottom - 15;

            int len;

            for (int i = 0; i < 16; i++) {

                if (overlays[i] != null) {
                    len = g.getFontMetrics(g.getFont()).stringWidth(overlays[i]);

                    if (i < 4) {
                        drawStringBW(overlays[i], g, 5, (15 * (i + 1)));
                    } else if ((i > 3) && (i < 8)) {
                        drawStringBW(overlays[i], g, rightOffset - len, (15 * ((i % 4) + 1)));
                    } else if ((i > 7) && (i < 12)) {
                        drawStringBW(overlays[i], g, 5, bottomOffset - 45 + (i % 4 * 15));
                    } else if (i > 11) {
                        drawStringBW(overlays[i], g, rightOffset - len, bottomOffset - 45 + (i % 4 * 15));
                    }
                }
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
>>>>>>> .r533
     * The purpose of this method is to examine both LUTs to determine if they are zero-based, that is, if they map
     * values of zero to the color R=0, G=0, B=0. If the LUT does, in fact, do this, then this method ensures that those
     * pixels are completely transparent so that no blending takes place for those pixels.
     */
    private void adjustOpacityFor000Color() {

        if (pixBufferB != null) { // no need to execute if there is no image B

            ViewJFrameBase vjfb = (ViewJFrameBase) frame;

            ModelLUT modelLUT = vjfb.getLUTa();

            if (modelLUT != null) {
                Color zeroIndexColor = modelLUT.getColor(0); // get the color at index 0 of LUT a

                // test to see if the color is R == 0, G == 0, B == 0
                boolean zeroIndexColorIs000 = ((zeroIndexColor.getRed() == 0) && (zeroIndexColor.getGreen() == 0) &&
                                                   (zeroIndexColor.getBlue() == 0));

                if (zeroIndexColorIs000) {

                    for (int i = 0; i < pixBuffer.length; i++) {
                        int temp = pixBuffer[i];
                        temp = temp & 0x00ffffff; // apply mask. temp will equal zero if the pixel should be transparent

                        if (temp == 0) {
                            pixBuffer[i] = pixBuffer[i] & 0x00ffffff; // make pixel transparent
                        }
                    }
                }

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
        }
    }

    /**
     * Builds the overlay Strings from the tag's value. Concatenates the output strings from the tags and ensures that
     * any properly read-in string has usable (if empty) values.
     *
     * @param    inf       The FileInfo with DICOM tags to display.
     * @param    name      DOCUMENT ME!
     * @param    dicomKey  Key of tag to display.
     *
     * @return  null when value is not a String or when the tag does not exist.
     */
    private String buildOverlayStrings(FileInfoDicom inf, String name, String dicomKey) {

        try {

            if ((dicomKey == null) || (dicomKey == "")) {
                return null;
            }

            Object val = inf.getTag(dicomKey).getValue(true);

            if (val == null) {
                return null;
            } else if ((name != null) && !(name.trim().equals(""))) {
                return (name + " - " + val);
            } else {
                return val.toString();
            }
        } catch (IllegalArgumentException ex) {
            Preferences.debug("Illegal arg on: " + dicomKey);

            return null;
        } catch (ClassCastException notStr) {
            Preferences.debug("Creating strings for DICOM overlay for " + dicomKey +
                              " but encountered a ClassCastException.\n", 4);

            return null;
        } catch (NullPointerException noTag) {
            Preferences.debug("Creating strings for DICOM overlay for " + dicomKey +
                              " but encountered a NullPointerException.\n", 4);

            return null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   inf           FileInfoBase
     * @param   name          String
     * @param   attribString  String
     *
     * @return  String
     */
    public String buildOverlayStrings(FileInfoBase inf, String name, String attribString) {

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
            return resultStr + imageActive.getTypeString();
        } else if (attribString.equals(atts[5])) {
            return resultStr + Double.toString(imageActive.getMin());
        } else if (attribString.equals(atts[6])) {
            return resultStr + Double.toString(imageActive.getMax());
        } else if (attribString.equals(atts[7])) {
            return resultStr + inf.getImageOrientationStr(inf.getImageOrientation());
        } else if (attribString.equals(atts[8])) {
            return resultStr + inf.getAxisOrientationStr(inf.getAxisOrientation(0));
        } else if (attribString.equals(atts[9])) {
            return resultStr + inf.getAxisOrientationStr(inf.getAxisOrientation(1));
        } else if (attribString.equals(atts[10])) {

            if (inf.getExtents().length > 2) {
                return resultStr + inf.getAxisOrientationStr(inf.getAxisOrientation(2));
            }
        } else if (attribString.equals(atts[11])) {
            return new String(resultStr + inf.getResolutions()[0] + " " +
                              inf.getUnitsOfMeasureAbbrevStr(inf.getUnitsOfMeasure()[0]));
        } else if (attribString.equals(atts[12])) {
            return new String(resultStr + inf.getResolutions()[1] + " " +
                              inf.getUnitsOfMeasureAbbrevStr(inf.getUnitsOfMeasure()[1]));
        } else if (attribString.equals(atts[13])) {

            if (inf.getExtents().length > 2) {
                return new String(resultStr + inf.getResolutions()[2] + " " +
                                  inf.getUnitsOfMeasureAbbrevStr(inf.getUnitsOfMeasure()[2]));
            }
        } else if (attribString.equals(atts[14])) {

            if (inf.getExtents().length > 3) {
                return new String(resultStr + inf.getResolutions()[3] + " " +
                                  inf.getUnitsOfMeasureAbbrevStr(inf.getUnitsOfMeasure()[3]));
            }
        } else if (attribString.equals(atts[15])) {
            return resultStr + Float.toString(inf.getSliceSpacing());
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
        } else if (attribString.equals(atts[21])) {
            return resultStr + inf.getTransformIDStr(inf.getTransformID());
        }

        return null;

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
        vci.setAlphaSliderEnabled(false);

        // maxExtents[0], maxExtents[1] are xy, xz, or zy depending on the orientation
        xDim = maxExtents[0];
        yDim = maxExtents[1];

        xSep = xDim / columnCheckers;
        xMod = xDim % columnCheckers;
        ySep = yDim / rowCheckers;
        yMod = yDim % rowCheckers;
        xStart = new int[columnCheckers + 1];
        yStart = new int[rowCheckers + 1];
        xStart[0] = 0;

        for (x = 1; x <= columnCheckers; x++) {
            xStart[x] = xStart[x - 1] + xSep;

            if (x <= xMod) {
                xStart[x]++;
            }
        } // for (x = 1; x < columnCheckers; x++)

        yStart[0] = 0;

        for (y = 1; y <= rowCheckers; y++) {
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
     * Repaints the paint brush cursor without repainting the entire image.
     *
     * @param  graphics2d  Graphics2D the graphics context to draw in
     */
    private void repaintPaintBrushCursorFast(Graphics2D graphics2d) {

        if ((graphics2d == null) || (lastMouseX == OUT_OF_BOUNDS) || (lastMouseY == OUT_OF_BOUNDS)) {
            return;
        }

        int xS = getScaledX(lastMouseX); // zoomed x.  Used as cursor
        int yS = getScaledY(lastMouseY); // zoomed y.  Used as cursor

        int brushSize = getBrushSize();
        int hBrushSize = getHBrushSize();

        int jMin = Math.max(yS - hBrushSize, 0);
        int iMin = Math.max(xS - hBrushSize, 0);

        iMin *= zoomX * resolutionX;
        jMin *= zoomY * resolutionY;

        int width = MipavMath.round(brushSize * resolutionX * zoomX);
        int height = MipavMath.round(brushSize * resolutionY * zoomY);

        graphics2d.setColor(Color.red.darker());
        graphics2d.drawRect(iMin, jMin, width - 1, height - 1);
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
     * Sets the overlay data based on data from the DICOM header.
     *
     * @param   orient  the image orientation
     * @param   x       x coordinate
     * @param   y       y coordinate
     * @param   z       z coordinate
     * @param   dim     reconstruction diameter
     *
     * @return  returns an array of strings that represent patient position
     */
    public String[] setOverlayValues(int orient, long x, long y, long z, int dim) {
        String up, down, right, left, stable;

        if (orient == FileInfoBase.SAGITTAL) {

            if (y > 0) {
                left = "A " + String.valueOf(dim - y);
                right = "P " + String.valueOf(y);
            } else {
                left = "A " + String.valueOf(-y);
                right = "P " + String.valueOf(dim + y);
            }

            if (z > 0) {
                up = "S " + String.valueOf(z);
                down = "I " + String.valueOf(dim - z);
            } else {
                up = "S " + String.valueOf(dim + z);
                down = "I " + String.valueOf(-z);
            }

            if (x > 0) {
                stable = "L " + String.valueOf(x);
            } else {
                stable = "R " + String.valueOf(-x);
            }
        } else if (orient == FileInfoBase.CORONAL) {

            if (x > 0) {
                right = "L " + String.valueOf(x);
                left = "R " + String.valueOf(dim - x);
            } else {
                right = "L " + String.valueOf(dim + x);
                left = "R " + String.valueOf(-x);
            }

            if (z > 0) {
                up = "S " + String.valueOf(z);
                down = "I " + String.valueOf(dim - z);
            } else {
                up = "S " + String.valueOf(dim + z);
                down = "I " + String.valueOf(-z);
            }

            if (y > 0) {
                stable = "P " + String.valueOf(y);
            } else {
                stable = "A " + String.valueOf(-y);
            }
        } else if (orient == FileInfoBase.AXIAL) {

            if (y > 0) {
                down = "P " + String.valueOf(y);
                up = "A " + String.valueOf(dim - y);
            } else {
                down = "P " + String.valueOf(dim + y);
                up = "A " + String.valueOf(-y);
            }

            if (x > 0) {
                right = "L " + String.valueOf(x);
                left = "R " + String.valueOf(dim - x);
            } else {
                right = "L " + String.valueOf(dim + x);
                left = "R " + String.valueOf(-x);
            }

            if (z > 0) {
                stable = "S " + String.valueOf(z);
            } else {
                stable = "I " + String.valueOf(-z);
            }
        } else {
            FileInfoDicom fileInfo2;

            if (slice >= 0) {
                fileInfo2 = (FileInfoDicom) (imageActive.getFileInfo())[slice - 1];
            } else {

                try {
                    fileInfo2 = (FileInfoDicom) (imageActive.getFileInfo())[slice + 1];
                } catch (ArrayIndexOutOfBoundsException e) {
                    return new String[] { "", "", "", "", "" };
                }
            }

            long x2 = MipavMath.round(fileInfo2.xLocation);
            long y2 = MipavMath.round(fileInfo2.yLocation);
            long z2 = MipavMath.round(fileInfo2.zLocation);

            if (x == x2) { // this is definitely correct

                if (x > 0) {
                    right = "L " + String.valueOf(x);
                    left = "R " + String.valueOf(dim - x);
                } else {
                    right = "L " + String.valueOf(dim + x);
                    left = "R " + String.valueOf(-x);
                }

                up = "AS";
                down = "PI";
                stable = "";
            } else if (y == y2) { // completely guessing at this

                if (y > 0) {
                    left = "A " + String.valueOf(dim - y);
                    right = "P " + String.valueOf(y);
                } else {
                    left = "A " + String.valueOf(-y);
                    right = "P " + String.valueOf(dim + y);
                }

                up = "RS";
                down = "LI";
                stable = "";
            } else if (z == z2) { // completely guessing at this

                if (z > 0) {
                    up = "S " + String.valueOf(dim - y);
                    down = "I " + String.valueOf(y);
                } else {
                    up = "S " + String.valueOf(-y);
                    down = "I " + String.valueOf(dim + y);
                }

                right = "LA";
                left = "RP";
                stable = "";
            } else {
                return new String[] { "", "", "", "", "" };
            }
        }

        return new String[] { left, right, up, down, stable };
    }

    /**
     * DOCUMENT ME!
     *
     * @param  xS  DOCUMENT ME!
     * @param  yS  DOCUMENT ME!
     */
    public void setPixelInformationAtLocation(int xS, int yS) {

        try {

            if ((imageActive.getFileInfo(0).getOrigin()[0] != 0) || (imageActive.getFileInfo(0).getOrigin()[1] != 0) ||
                    (imageActive.getFileInfo(0).getOrigin()[2] != 0)) {
                FileInfoBase fileInfo = imageActive.getFileInfo()[slice];
                String[] values = setScannerPosition(fileInfo, xS, yS, slice);

                if (values != null) {
                    frame.setMessageText("  X: " + String.valueOf((xS + 1)) + " Y: " + String.valueOf((yS) + 1) +
                                         "  Intensity:  " +
                                         String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]) +
                                         " Position: " + values[0] + " " + values[1] + " " + values[2]);
                } else {
                    frame.setMessageText("  X: " + String.valueOf((xS + 1)) + " Y: " + String.valueOf((yS + 1)) +
                                         "  Intensity:  " +
                                         String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]));
                }

            } else {

                if (imageActive.isColorImage() == true) {
                    frame.setMessageText("  X: " + String.valueOf((xS + 1)) + " Y: " + String.valueOf((yS + 1)) +
                                         "  R:  " +
                                         String.valueOf(imageBufferActive[(4 *
                                                                               ((yS * imageActive.getExtents()[0]) +
                                                                                    xS)) + 1]) + "  G:  " +
                                         String.valueOf(imageBufferActive[(4 *
                                                                               ((yS * imageActive.getExtents()[0]) +
                                                                                    xS)) + 2]) + "  B:  " +
                                         String.valueOf(imageBufferActive[(4 *
                                                                               ((yS * imageActive.getExtents()[0]) +
                                                                                    xS)) + 3]));
                } else {
                    frame.setMessageText("  X: " + String.valueOf((xS + 1)) + " Y: " + String.valueOf((yS + 1)) +
                                         "  Intensity:  " +
                                         String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]));
                }
            }
        } catch (ArrayIndexOutOfBoundsException error) {
            frame.setMessageText("  X: " + String.valueOf((xS + 1)) + " Y: " + String.valueOf((yS + 1)));
        }
    }


}
