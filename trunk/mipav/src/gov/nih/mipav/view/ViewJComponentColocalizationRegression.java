package gov.nih.mipav.view;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class ViewJComponentColocalizationRegression extends ViewJComponentBase
        implements MouseMotionListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2295519432253254184L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Opacity value used by the paint brush. value = 1.0 - opaque value = 0.25 - default (mostly see through) */
    public float OPACITY = 0.25f;

    /*  When not in freeRangeMode:
     *  The point VOI must be dragged along the least squares fit line. In mousePressed and mouseDragged the
     * intersection point of the perpindicular line from the mouse location to the least squares fit line is calculated.
     *  The endpoints of the least squares fit line are at (XArray[0],yArray[0]) and (xArray[1],yArray[1]).  The mouse
     * location is at (x,y).  The intersection of the perpindicular from (x,y) to the least squares fit line occurs at
     * (xIntersect,yIntersect). By the sum of the squares formula for a right triangle we have (xIntersect -
     * xArray[0])**2 + (yIntersect - yArray[0])**2 + (xIntersect - x)**2 + (yIntersect - y)**2 = (x - xArray[0])**2 + (y
     * - yArray[0])**2 Likewise: (xIntersect - xArray[1])**2 + (yIntersect - yArray[1])**2 + (xIntersect - x)**2 +
     * (yIntersect - y)**2 = (x - xArray[1])**2 + (y - yArray[1])**2 Subtracting the first equation from the second:
     * (xIntersect - xArray[1])**2 - (xIntersect - xArray[0])**2 + (yIntersect - yArray[1])**2 - (yIntersect -
     * yArray[0])**2 = (x - xArray[1])**2 - (x - xArray[0])**2 + (y - yArray[1])**2 - (y - yArray[0])**2 With algebraic
     * simplification: xIntersect*(xArray[1] - xArray[0]) + yIntersect*(yArray[1] - yArray[0]) = x*(xArray[1] -
     * xArray[0]) + y*(yArray[1] - yArray[0]) Replacing yIntersect with xIntersect*lineSlope + lineOffset, (xArray[1] -
     * xArray[0] with xdiff, and (yArray[1] - yArray[0]) with ydiff: xIntersect = (x*xdiff + (y -
     * lineOffset)*ydiff)/(xdiff + lineSlope*ydiff) When in free range mode, the point VOI can be dragged anywhere in
     * the histogram buffer.
     */
    /** frame - frame where the component image is displayed. */
    protected ViewJFrameBase frame;

    /** imageA - model for image A. */
    protected ModelImage imageA;

    /** imageB - model for image B. */
    protected ModelImage imageB;

    /** DOCUMENT ME! */
    protected int interpMode = SMOOTH;

    /** true if selected red, green, blue components present in RGB image. */
    protected ModelRGB RGBTA;

    /** DOCUMENT ME! */
    protected ModelRGB RGBTB;

    /** DOCUMENT ME! */
    private AlgorithmColocalizationRegression alg;

    /** DOCUMENT ME! */
    private int bin1;

    /** DOCUMENT ME! */
    private int bin2;

    /** DOCUMENT ME! */
    private int bottomPad;

    /** DOCUMENT ME! */
    private int brightness; // offset ranging from -255 to 255 add to each
                            // scaled red, green, and blue

    /** DOCUMENT ME! */
    private float[] colocIntensity1;

    /** DOCUMENT ME! */
    private float[] colocIntensity2;

    /** DOCUMENT ME! */
    private float[] colocSize;

    /** DOCUMENT ME! */
    private float contrast; // scale factor ranging from 0.1 to 10.0
                            // by which to multiply each red, green, and blue

    /** DOCUMENT ME! */
    private ModelImage destImage;

    /** DOCUMENT ME! */
    private boolean doSecondIteration;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage editImageFrameA = null;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage editImageFrameB = null;

    /** DOCUMENT ME! */
    private float firstIndex = 0.0f;

    /** DOCUMENT ME! */
    private float[] freeRangeColocIntensity1 = null;

    /** DOCUMENT ME! */
    private float[] freeRangeColocIntensity2 = null;

    /** DOCUMENT ME! */
    private float[] freeRangeColocSize = null;

    /** DOCUMENT ME! */
    private boolean freeRangeMode = false;

    /** DOCUMENT ME! */
    private float[] freeRangeRThreshold = null;

    /** DOCUMENT ME! */
    private boolean haveFiltered = false; // whether or not the brightness/
                                          // contrast filter has been invoked

    /** DOCUMENT ME! */
    private boolean[] haveFreeRangeThreshold = null;

    /** DOCUMENT ME! */
    private boolean[] haveThreshold;

    /** DOCUMENT ME! */
    private float[] imageBufferDest = null;

    /** DOCUMENT ME! */
    private Image imgDest;

    /** DOCUMENT ME! */
    private int leftPad;

    /** DOCUMENT ME! */
    private float linearCorrelation;

    /** DOCUMENT ME! */
    private int lineMin;

    /** DOCUMENT ME! */
    //private double lineMin1, lineMax1, lineMin2, lineMax2;

    /** DOCUMENT ME! */
    private double lineOffset;

    /** DOCUMENT ME! */
    private double lineSlope;

    /** DOCUMENT ME! */
    private VOI lineVOI;

    /** DOCUMENT ME! */
    private ViewJFrameColocalizationRegression localizeFrame;

    /** DOCUMENT ME! */
    private boolean logMagDisplay = false;


    /** DOCUMENT ME! */
    private int[] lutBufferRemappedDest = null;

    /** LUTa - lookup table for destImage. */
    private ModelLUT LUTdest;

    /** DOCUMENT ME! */
    private MemoryImageSource memImageDest;

    /** DOCUMENT ME! */
    private double min1, max1, min2, max2;

    /** DOCUMENT ME! */
    private double minimumx, maximumx;

    /** DOCUMENT ME! */
    private int nVOI; // number of vois

    /** DOCUMENT ME! */
    private float originalLineOffset;

    /** DOCUMENT ME! */
    private float originalLineSlope;

    /** Buffer used to indicate if the pixel location is painted (true) or unpainted (false). */
    private BitSet paintBitmapDest;

    /** Buffer that displays the combined paintBitmap and pixBuffer buffers. */
    private int[] paintBufferDest = null;

    /** Buffer used to store ARGB images of the image presently being displayed. */
    private int[] pixBufferDest = null;

    /** DOCUMENT ME! */
    private VOI pointVOI;

    /** DOCUMENT ME! */
    private int red, green, blue;

    /** DOCUMENT ME! */
    private boolean regionLinesDisplay = false;

    /** DOCUMENT ME! */
    private float resX, resY; // x and y resolutions

    /** DOCUMENT ME! */
    private int rightPad;

    /** DOCUMENT ME! */
    private float[] rThreshold;

    /** DOCUMENT ME! */
    private double scale1, scale2;

    /** DOCUMENT ME! */
    private float secondIndex = 0.0f;

    /** DOCUMENT ME! */
    private boolean thresholdOn1;

    /** DOCUMENT ME! */
    private int topPad;

    /** DOCUMENT ME! */
    private boolean useBlue;

    /** DOCUMENT ME! */
    private boolean useGreen;

    /** DOCUMENT ME! */
    private boolean useRed;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;

    /** DOCUMENT ME! */
    private float[] xArray = new float[2];

    /** DOCUMENT ME! */
    private double xdiff, ydiff;

    /** Note that xDim and yDim refer to destImage, while zDim and tDim refer to imageA. */
    private int xDim, yDim, zDim, tDim;

    /** DOCUMENT ME! */
    private float[] yArray = new float[2];

    /** DOCUMENT ME! */
    private float[] zArray = new float[2];

    /** DOCUMENT ME! */
    private float zoomX = 1; // magnification, here zoomX = zoomY

    /** DOCUMENT ME! */
    private float zoomY = 1; // and zoom is always a power of 2

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  alg                 AlgorithmColocalizationRegression parent
     * @param  _frame              frame where the controls are obtained
     * @param  localizeFrame       frame where 2D histogram is displayed
     * @param  _imageA             Model of the image that will be displayed
     * @param  _imageB             Model of the image that will be displayed
     * @param  destImage           Image with histogram information
     * @param  LUTdest             LUT used to display destImage
     * @param  imageBufferDest     storage buffer used to display destImage
     * @param  useRed              DOCUMENT ME!
     * @param  useGreen            DOCUMENT ME!
     * @param  useBlue             DOCUMENT ME!
     * @param  originalLineSlope   DOCUMENT ME!
     * @param  originalLineOffset  DOCUMENT ME!
     * @param  haveThreshold       DOCUMENT ME!
     * @param  rThreshold          DOCUMENT ME!
     * @param  colocSize           DOCUMENT ME!
     * @param  colocIntensity1     DOCUMENT ME!
     * @param  colocIntensity2     DOCUMENT ME!
     * @param  min1                DOCUMENT ME!
     * @param  max1                DOCUMENT ME!
     * @param  min2                DOCUMENT ME!
     * @param  max2                DOCUMENT ME!
     * @param  scale1              DOCUMENT ME!
     * @param  scale2              DOCUMENT ME!
     * @param  lineMin1            DOCUMENT ME!
     * @param  lineMax1            DOCUMENT ME!
     * @param  lineMin2            DOCUMENT ME!
     * @param  lineMax2            DOCUMENT ME!
     * @param  thresholdOn1        DOCUMENT ME!
     * @param  pixBufferDest       storage buffer used to build a displayable image
     * @param  paintBufferDest     DOCUMENT ME!
     * @param  zoom                initial magnification of image
     * @param  extents             initial display dimensions of the image
     * @param  logMagDisplay       display log magnitude of image
     * @param  regionLinesDisplay  DOCUMENT ME!
     * @param  leftPad             DOCUMENT ME!
     * @param  rightPad            DOCUMENT ME!
     * @param  bottomPad           DOCUMENT ME!
     * @param  topPad              DOCUMENT ME!
     * @param  doSecondIteration   DOCUMENT ME!
     * @param  linearCorrelation   DOCUMENT ME!
     * @param  userInterface       DOCUMENT ME!
     */
    public ViewJComponentColocalizationRegression(AlgorithmColocalizationRegression alg, ViewJFrameBase _frame,
                                                  ViewJFrameColocalizationRegression localizeFrame, ModelImage _imageA,
                                                  ModelImage _imageB, ModelImage destImage, ModelLUT LUTdest,
                                                  float[] imageBufferDest, boolean useRed, boolean useGreen,
                                                  boolean useBlue, float originalLineSlope, float originalLineOffset,
                                                  boolean[] haveThreshold, float[] rThreshold, float[] colocSize,
                                                  float[] colocIntensity1, float[] colocIntensity2, double min1,
                                                  double max1, double min2, double max2, double scale1, double scale2,
                                                  double lineMin1, double lineMax1, double lineMin2, double lineMax2,
                                                  boolean thresholdOn1, int[] pixBufferDest, int[] paintBufferDest,
                                                  float zoom, int[] extents, boolean logMagDisplay,
                                                  boolean regionLinesDisplay, int leftPad, int rightPad, int bottomPad,
                                                  int topPad, boolean doSecondIteration, float linearCorrelation,
                                                  ViewUserInterface userInterface) {

        super(new Dimension(destImage.getExtents()[0], destImage.getExtents()[1]));

        this.alg = alg;
        frame = _frame;
        this.localizeFrame = localizeFrame;
        imageA = _imageA;
        editImageFrameA = imageA.getParentFrame().getComponentImage();
        imageB = _imageB;

        if (imageB != null) {
            editImageFrameB = imageB.getParentFrame().getComponentImage();
        }

        this.destImage = destImage;
        this.useRed = useRed;
        this.useGreen = useGreen;
        this.useBlue = useBlue;

        if (imageA.isColorImage()) {
            editImageFrameA.setThresholdColors(useRed, useGreen, useBlue);
        } else {
            editImageFrameA.setImageColocalize(imageB);
            editImageFrameB.setImageColocalize(imageA);
            editImageFrameA.setHasThreshold1(true);
            editImageFrameB.setHasThreshold2(true);
        }

        this.originalLineSlope = originalLineSlope;
        this.originalLineOffset = originalLineOffset;
        this.haveThreshold = haveThreshold;
        this.rThreshold = rThreshold;
        this.colocSize = colocSize;
        this.colocIntensity1 = colocIntensity1;
        this.colocIntensity2 = colocIntensity2;
        this.min1 = min1;
        this.max1 = max1;
        this.min2 = min2;
        this.max2 = max2;
        this.scale1 = scale1;
        this.scale2 = scale2;
        this.userInterface = userInterface;
        this.thresholdOn1 = thresholdOn1;

        if (thresholdOn1) {
            lineMin = (int) Math.ceil(lineMin1);
        } else {
            lineMin = (int) Math.ceil(lineMin2);
        }

        minimumx = ((lineMin1 - min1) * scale1) + leftPad;
        maximumx = ((lineMax1 - min1) * scale1) + leftPad;
        this.leftPad = leftPad;
        this.rightPad = rightPad;
        this.bottomPad = bottomPad;
        this.topPad = topPad;
        bin1 = destImage.getExtents()[0] - leftPad - rightPad;
        bin2 = destImage.getExtents()[1] - bottomPad - topPad;
        VOIs = destImage.getVOIs();

        if (VOIs != null) {
            nVOI = VOIs.size();
        } else {
            nVOI = 0;
        }

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.LINE) {
                lineVOI = VOIs.VOIAt(i);
            } else if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                pointVOI = VOIs.VOIAt(i);
            }
        }

        if (lineVOI == null) {
            MipavUtil.displayError("Expected line VOI not found");

            return;
        }

        if (pointVOI == null) {
            MipavUtil.displayError("Expected point VOI not found");

            return;
        }

        ((VOILine) (lineVOI.getCurves().elementAt(0))).exportArrays(xArray, yArray, zArray);

        lineSlope = (yArray[1] - yArray[0]) / (xArray[1] - xArray[0]);
        lineOffset = yArray[0] - (xArray[0] * lineSlope);

        xdiff = xArray[1] - xArray[0];
        ydiff = yArray[1] - yArray[0];


        xDim = destImage.getExtents()[0];
        yDim = destImage.getExtents()[1];

        if (imageA.getNDims() >= 3) {
            zDim = imageA.getExtents()[2];
        } else {
            zDim = 1;
        }

        if (imageA.getNDims() >= 4) {
            tDim = imageA.getExtents()[3];
        } else {
            tDim = 1;
        }

        resX = 1.0f;
        resY = 1.0f;
        setSize(Math.round(imageDim.width * resX), Math.round(imageDim.height * resY));

        this.LUTdest = LUTdest;
        lutBufferRemappedDest = new int[256];

        this.imageBufferDest = imageBufferDest;
        this.pixBufferDest = pixBufferDest;
        this.paintBufferDest = paintBufferDest;
        paintBitmapDest = destImage.getMask();
        this.logMagDisplay = logMagDisplay;
        this.regionLinesDisplay = regionLinesDisplay;
        this.doSecondIteration = doSecondIteration;
        this.linearCorrelation = linearCorrelation;
        setZoom(zoom, zoom);
        addMouseMotionListener(this);
        addMouseListener(this);
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Shows the 2D histogram image.
     *
     * @param   _LUTdest   LUTdest - to change to new LUT for imageA else null
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean buildImageDestObject(ModelLUT _LUTdest, boolean forceShow) {

        float rangeA = 0;
        float imageMinA = 0, imageMaxA = 0;
        int xDim, yDim;
        int bufferSize;
        int lutHeightA = 0;
        int index;
        int Ra, Ga, Ba;
        int pix;
        float opacityPrime;
        int i;

        if (destImage == null) {
            return false;
        }

        if (LUTdest == null) {
            return false;
        }

        if (_LUTdest != null) {
            LUTdest = _LUTdest;
        }

        lutHeightA = LUTdest.getExtents()[1];

        xDim = destImage.getExtents()[0];
        yDim = destImage.getExtents()[1];

        if (lutHeightA != lutBufferRemappedDest.length) {

            try {
                lutBufferRemappedDest = new int[lutHeightA];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ViewJComponentColocalizationRegression.buildImageObject");

                return false;
            }
        }

        LUTdest.exportIndexedLUT(lutBufferRemappedDest);

        bufferSize = xDim * yDim;

        if (logMagDisplay) {
            imageMinA = (float) (0.4342944819 * Math.log(1 + destImage.getMin()));
            imageMaxA = (float) (0.4342944819 * Math.log(1 + destImage.getMax()));
        } else {
            imageMinA = (float) destImage.getMin();
            imageMaxA = (float) destImage.getMax();
        }


        if (((imageMaxA - imageMinA) < 256) && (LUTdest.getLUTType() == ModelLUT.STRIPED)) {
           
        } else {
            rangeA = imageMaxA - imageMinA;

            if (rangeA == 0) {
                rangeA = 1;
            }

            
        }

        if (frame.getControls() != null) {
            OPACITY = frame.getControls().getTools().getOpacity();
            int paintColorIndex = getPaintColorIndex(frame);
            red = Math.round(frame.getControls().getTools().getPaintColor()[paintColorIndex].getRed() * OPACITY);
            green = Math.round(frame.getControls().getTools().getPaintColor()[paintColorIndex].getGreen() * OPACITY);
            blue = Math.round(frame.getControls().getTools().getPaintColor()[paintColorIndex].getBlue() * OPACITY);
        } else {
            red = 128;
            green = 0;
            blue = 0;
        }

        opacityPrime = 1 - OPACITY;

        if (forceShow == true) {

            try {

                destImage.exportSliceXY(0, imageBufferDest);
            } catch (IOException error) {
                MipavUtil.displayError("" + error); // Need to fix this

                return false;
            }
        }


        if (logMagDisplay) {

            for (i = 0; i < imageBufferDest.length; i++) {
                imageBufferDest[i] = (float) (0.4342944819 * Math.log(1 + imageBufferDest[i]));
            }
        }

        int value;
        pix = 0;

        TransferFunction tf_imgA = LUTdest.getTransferFunction();

        for (index = 0; index < bufferSize; index++) {
            pix = (int) (tf_imgA.getRemappedValue(imageBufferDest[index], 256) + 0.5f);

            if (paintBitmapDest.get(index) == true) {
                value = lutBufferRemappedDest[pix];
                Ra = (value & 0x00ff0000) >> 16;
                Ga = (value & 0x0000ff00) >> 8;
                Ba = (value & 0x000000ff);
                value = 0xff000000 |
                            (((int) ((Ra * opacityPrime) + red) << 16) |
                                 (((int) ((Ga * opacityPrime) + green) << 8) | ((int) ((Ba * opacityPrime) + blue))));
                paintBufferDest[index] = value;
                pixBufferDest[index] = lutBufferRemappedDest[pix];
            } else {
                paintBufferDest[index] = pixBufferDest[index] = lutBufferRemappedDest[pix];
            }
        }

        importImageDest(paintBufferDest);

        return true;
    }

    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  gcFlag  if true garbage collector should be called.
     */
    public void dispose(boolean gcFlag) {

        lutBufferRemappedDest = null;
        pixBufferDest = null;
        paintBufferDest = null;
        paintBitmapDest = null;
        frame = null;
        imageBufferDest = null;
        img = null;
        imgDest = null;
        memImageDest = null;
        super.disposeLocal();

        if (gcFlag == true) {
            System.gc();
            System.runFinalization();
            System.gc();
        }
    }


    /**
     * Clean up some resources!
     */
    public void finalizeLocal() { }

    /**
     * Accessor that returns int blue.
     *
     * @return  blue
     */
    public int getblue() {
        return blue;
    }

    /**
     * Accessor that returns int green.
     *
     * @return  green
     */
    public int getgreen() {
        return green;
    }

    /**
     * Accessor that returns the imageA.
     *
     * @return  imageA
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Accessor that returns the imageB.
     *
     * @return  imageB
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Accessor that returns float OPACITY.
     *
     * @return  OPACITY
     */
    public float getOPACITY() {
        return OPACITY;
    }

    /**
     * Accessor that returns BitSet paintBitmap.
     *
     * @return  paintBitmap
     */
    public BitSet getpaintBitmapDest() {
        return paintBitmapDest;
    }

    /**
     * Size set to object size.
     *
     * @return  dimension with the size
     */
    public Dimension getPreferredSize() {

        try {
            return new Dimension(Math.round(zoomX * imageDim.width * resX), Math.round(zoomY * imageDim.height * resY));
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.getPreferredSize");

            return null;
        }
    }


    /**
     * Accessor that returns int red.
     *
     * @return  red
     */
    public int getred() {
        return red;
    }

    /**
     * Gets the size of the object taking into account the zoom.
     *
     * @param   wh  dimension
     *
     * @return  dimension with the size
     */
    public Dimension getSize(Dimension wh) {

        try {

            if (wh == null) {
                return new Dimension(Math.round(zoomX * imageDim.width * resX),
                                     Math.round(zoomY * imageDim.height * resY));
            } else {
                wh.setSize(Math.round(zoomX * imageDim.width * resX), Math.round(zoomY * imageDim.height * resY));

                return wh;
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.getSize");

            return null;
        }
    }


    /**
     * Magnification in the x - dimension.
     *
     * @return  magnificaiton in the x - dimension
     */
    public float getZoomX() {
        return zoomX;
    }

    /**
     * Magnification in the y - dimension.
     *
     * @return  magnificaiton in the y - dimension
     */
    public float getZoomY() {
        return zoomY;
    }


    /**
     * Creates a Image object form an array of ints that have been formatted (packed) properly (i.e. aRGB)
     *
     * @param  data  Data (image) to be displayed that has been formatted (packed) properly (i.e. aRGB)
     */
    public void importImageDest(int[] data) {
        // If the MemoryImageSource and createImage steps are separated, then animate displays only the last image.
        // createImage must be executed right after MemoryImageSource.

        if (data != null) {
            memImageDest = null;

            try {
                memImageDest = new MemoryImageSource(imageDim.width, imageDim.height, data, 0, imageDim.width);

                if (!haveFiltered) {
                    imgDest = createImage(memImageDest);
                } else {
                    imgDest = createImage(new FilteredImageSource(memImageDest,
                                                                  new ViewJFilterAnimate(brightness, contrast)));
                }

            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ViewJComponentColocalizationRegression.importImage.");
            }

        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent mouseEvent) { }

    /**
     * mouseDragged only updates the histogram display header when the mouse passes over a point for which the linear
     * correlation coefficient and colocalizations have already been calculated.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseDragged(MouseEvent mouseEvent) {
        int x, y;
        int xS, yS;
        int ySInvert;
        int xIntersect, yIntersect;
        double xIntersectd, yIntersectd;
        int index = 0;
        int index1 = 0;
        int index2 = 0;
        float index1f = 0.0f;
        float index2f = 0.0f;
        float threshold;
        String currentThreshold;
        float colocAreaPercent;
        String currentColoc;
        float colocIntensityPercent1;
        String currentIntensity1;
        float colocIntensityPercent2;
        String currentIntensity2;
        int ch1, ch2;

        x = mouseEvent.getX();
        y = mouseEvent.getY();
        xS = Math.round(x / (getZoomX() * resolutionX)); // zoomed x.  Used as cursor
        yS = Math.round(y / (getZoomY() * resolutionY)); // zoomed y.  Used as cursor

        if (freeRangeMode) {
            ch1 = xS - leftPad;

            if (ch1 < 0) {
                ch1 = 0;
            }

            if (ch1 >= bin1) {
                ch1 = bin1 - 1;
            }

            index1 = (int) Math.round((ch1 / scale1) + min1);
            ySInvert = (2 * topPad) + bin2 - 1 - yS;
            ch2 = ySInvert - topPad;

            if (ch2 < 0) {
                ch2 = 0;
            }

            if (ch2 >= bin2) {
                ch2 = bin2 - 1;
            }

            index2 = (int) Math.round((ch2 / scale2) + min2);
            index = ch1 + (bin1 * ch2);
            firstIndex = index1;
            secondIndex = index2;

            if (!haveFreeRangeThreshold[index]) {
                ((VOIPoint) (pointVOI.getCurves().elementAt(0))).locateVOIPoint(xS, yS, 0, xDim, yDim, 1);
                paintComponent(getGraphics());

                return;
            }

            threshold = freeRangeRThreshold[index];
            colocAreaPercent = freeRangeColocSize[index];
            colocIntensityPercent1 = freeRangeColocIntensity1[index];
            colocIntensityPercent2 = freeRangeColocIntensity2[index];

            if (imageA.getNDims() > 2) {
                currentColoc = "     % colocalization volume = " + colocAreaPercent;
            } else {
                currentColoc = "     % colocalization area = " + colocAreaPercent;
            }

            if (useRed && useGreen) {
                currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % green colocalization = " + colocIntensityPercent2;
            } else if (useRed && useBlue) {
                currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
            } else if (useGreen && useBlue) {
                currentIntensity1 = "     % green colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
            } else {
                currentIntensity1 = "     % " + imageA.getImageName() + " colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % " + imageB.getImageName() + " colocalization = " + colocIntensityPercent2;
            }

            if (Float.isNaN(threshold)) {
                currentThreshold = "     Linear correlation coefficient is undefined";
            } else {
                currentThreshold = "     Linear correlation coefficient = " + threshold;
            }

            if ((useRed) && (useGreen)) {
                currentThreshold += " for pixels with red < " + index1 + " or green < " + index2;
            } else if ((useRed) && (useBlue)) {
                currentThreshold += " for pixels with red < " + index1 + " or blue < " + index2;
            } else if ((useGreen) && (useBlue)) {
                currentThreshold += " for pixels with green < " + index1 + " or blue < " + index2;
            } else {
                currentThreshold += " for pixels with " + imageA.getImageName() + " < " + index1 + " or " +
                                    imageB.getImageName() + " < " + index2;
            }

            localizeFrame.setCurrentLabels(currentColoc, currentIntensity1, currentIntensity2, currentThreshold);

            ((VOIPoint) (pointVOI.getCurves().elementAt(0))).locateVOIPoint(xS, yS, 0, xDim, yDim, 1);
        } // if (freeRangeMode)
        else { // !freeRangeMode

            // Find the intersection of the perpindicular from the mouse to
            // the least squares fit line
            xIntersectd = ((xS * xdiff) + ((yS - lineOffset) * ydiff)) / (xdiff + (lineSlope * ydiff));

            if (xIntersectd < minimumx) {
                xIntersectd = minimumx;
            }

            if (xIntersectd > maximumx) {
                xIntersectd = maximumx;
            }

            xIntersect = (int) Math.round(xIntersectd);
            yIntersectd = (lineSlope * xIntersectd) + lineOffset;
            yIntersect = (int) Math.round(yIntersectd);

            if (thresholdOn1) {
                index1 = (int) Math.round(((xIntersectd - leftPad) / scale1) + min1);

                if (index1 < lineMin) {
                    index1 = lineMin;
                }

                if (index1 > (lineMin + rThreshold.length - 1)) {
                    index1 = lineMin + rThreshold.length - 1;
                }

                index2f = (index1 * originalLineSlope) + originalLineOffset;
                index2 = Math.round(index2f);
                firstIndex = index1;
                secondIndex = index2f;

                if (!haveThreshold[index1 - lineMin]) {
                    ((VOIPoint) (pointVOI.getCurves().elementAt(0))).locateVOIPoint(xIntersect, yIntersect, 0, xDim,
                                                                                       yDim, 1);
                    paintComponent(getGraphics());

                    return;
                }

                threshold = rThreshold[index1 - lineMin];
                colocAreaPercent = colocSize[index1 - lineMin];
                colocIntensityPercent1 = colocIntensity1[index1 - lineMin];
                colocIntensityPercent2 = colocIntensity2[index1 - lineMin];
            } else {
                yIntersectd = (2 * topPad) + bin2 - 1 - yIntersectd;
                index2 = (int) Math.round(((yIntersectd - topPad) / scale2) + min2);

                if (index2 < lineMin) {
                    index2 = lineMin;
                }

                if (index2 > (lineMin + rThreshold.length - 1)) {
                    index2 = lineMin + rThreshold.length - 1;
                }

                index1f = (index2 - originalLineOffset) / originalLineSlope;
                index1 = Math.round(index1f);
                threshold = rThreshold[index2 - lineMin];
                firstIndex = index1f;
                secondIndex = index2;

                if (!haveThreshold[index2 - lineMin]) {
                    ((VOIPoint) (pointVOI.getCurves().elementAt(0))).locateVOIPoint(xIntersect, yIntersect, 0, xDim,
                                                                                       yDim, 1);
                    paintComponent(getGraphics());

                    return;
                }

                colocAreaPercent = colocSize[index2 - lineMin];
                colocIntensityPercent1 = colocIntensity1[index2 - lineMin];
                colocIntensityPercent2 = colocIntensity2[index2 - lineMin];
            }

            if (imageA.getNDims() > 2) {
                currentColoc = "     % colocalization volume = " + colocAreaPercent;
            } else {
                currentColoc = "     % colocalization area = " + colocAreaPercent;
            }

            if (useRed && useGreen) {
                currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % green colocalization = " + colocIntensityPercent2;
            } else if (useRed && useBlue) {
                currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
            } else if (useGreen && useBlue) {
                currentIntensity1 = "     % green colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
            } else {
                currentIntensity1 = "     % " + imageA.getImageName() + " colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % " + imageB.getImageName() + " colocalization = " + colocIntensityPercent2;
            }

            if (Float.isNaN(threshold)) {
                currentThreshold = "     Linear correlation coefficient is undefined";
            } else {
                currentThreshold = "     Linear correlation coefficient = " + threshold;
            }

            if (thresholdOn1) {

                if ((useRed) && (useGreen)) {
                    currentThreshold += " for pixels with red < " + index1 + " or green < " + index2f;
                } else if ((useRed) && (useBlue)) {
                    currentThreshold += " for pixels with red < " + index1 + " or blue < " + index2f;
                } else if ((useGreen) && (useBlue)) {
                    currentThreshold += " for pixels with green < " + index1 + " or blue < " + index2f;
                } else {
                    currentThreshold += " for pixels with " + imageA.getImageName() + " < " + index1 + " or " +
                                        imageB.getImageName() + " < " + index2f;
                }
            } else {

                if ((useRed) && (useGreen)) {
                    currentThreshold += " for pixels with red < " + index1f + " or green < " + index2;
                } else if ((useRed) && (useBlue)) {
                    currentThreshold += " for pixels with red < " + index1f + " or blue < " + index2;
                } else if ((useGreen) && (useBlue)) {
                    currentThreshold += " for pixels with green < " + index1f + " or blue < " + index2;
                } else {
                    currentThreshold += " for pixels with " + imageA.getImageName() + " < " + index1f + " or " +
                                        imageB.getImageName() + " < " + index2;
                }
            }

            localizeFrame.setCurrentLabels(currentColoc, currentIntensity1, currentIntensity2, currentThreshold);

            ((VOIPoint) (pointVOI.getCurves().elementAt(0))).locateVOIPoint(xIntersect, yIntersect, 0, xDim, yDim,
                                                                               1);
        } // else !freeRangeMode

        paintComponent(getGraphics());

        if (imageA.getNDims() == 2) {
            editImageFrameA.setThresholds(firstIndex, secondIndex);

            if (editImageFrameB != null) {
                editImageFrameB.setThresholds(firstIndex, secondIndex);
            }

            // zInitial = imageA.getParentFrame().getComponentImage().getSlice();
            if (imageA.isColorImage()) {

                /*for (t = 0 ; t < tDim; t++) {
                 *  for (z = zInitial+1; z < zDim; z++) {     editImageFrameA.show(t,z,true); } for (z = 0; z <=
                 * zInitial; z++) {     editImageFrameA.show(t,z,true); }}*/
                editImageFrameA.show(0, 0, true);
            } // if (imageA.isColorImage())
            else { // black and white images

                /*for (t = 0; t < tDim; t++) {
                 *  for (z = zInitial+1; z < zDim; z++) {     editImageFrameA.show(t,z,null,null,true,interpMode);
                 * editImageFrameB.show(t,z,null,null,true,interpMode); } for (z = 0; z <= zInitial; z++) {
                 * editImageFrameA.show(t,z,null,null,true,interpMode);
                 * editImageFrameB.show(t,z,null,null,true,interpMode); }}*/
                editImageFrameA.show(0, 0, null, null, true, interpMode);
                editImageFrameB.show(0, 0, null, null, true, interpMode);
            } // black and white images
        } // if (imageA.getNDims() == 2)
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent mouseEvent) { }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseMoved(MouseEvent mouseEvent) { }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent mouseEvent) { }

    /**
     * If the correlation coefficient and colocalizations have not already been calculated at the mouse release point,
     * then a call to AlgorithmColocalizationRegression calculates them.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        int x, y;
        int xS, yS;
        int ySInvert;
        int xIntersect, yIntersect;
        double xIntersectd, yIntersectd;
        int index = 0;
        int index1 = 0;
        int index2 = 0;
        float index1f = 0.0f;
        float index2f = 0.0f;
        float threshold;
        String currentThreshold;
        float colocAreaPercent;
        String currentColoc;
        float colocIntensityPercent1;
        String currentIntensity1;
        float colocIntensityPercent2;
        String currentIntensity2;
        String dataLine1;
        String dataLine2;
        int t, z;
        int zInitial;
        int ch1, ch2;

        x = mouseEvent.getX();
        y = mouseEvent.getY();
        xS = Math.round(x / (getZoomX() * resolutionX)); // zoomed x.  Used as cursor
        yS = Math.round(y / (getZoomY() * resolutionY)); // zoomed y.  Used as cursor

        if (freeRangeMode) {
            ch1 = xS - leftPad;

            if (ch1 < 0) {
                ch1 = 0;
            }

            if (ch1 >= bin1) {
                ch1 = bin1 - 1;
            }

            index1 = (int) Math.round((ch1 / scale1) + min1);
            ySInvert = (2 * topPad) + bin2 - 1 - yS;
            ch2 = ySInvert - topPad;

            if (ch2 < 0) {
                ch2 = 0;
            }

            if (ch2 >= bin2) {
                ch2 = bin2 - 1;
            }

            index2 = (int) Math.round((ch2 / scale2) + min2);
            index = ch1 + (bin1 * ch2);

            if (!haveFreeRangeThreshold[index]) {
                alg.calculateFreeRangeThreshold(ch1, ch2);
            }

            threshold = freeRangeRThreshold[index];
            colocAreaPercent = freeRangeColocSize[index];
            colocIntensityPercent1 = freeRangeColocIntensity1[index];
            colocIntensityPercent2 = freeRangeColocIntensity2[index];
            firstIndex = index1;
            secondIndex = index2;

            if (imageA.getNDims() > 2) {
                currentColoc = "     % colocalization volume = " + colocAreaPercent;
                dataLine1 = "%coloc vol\t";
            } else {
                currentColoc = "     % colocalization area = " + colocAreaPercent;
                dataLine1 = "%coloc area\t";
            }

            dataLine2 = colocAreaPercent + "\t";

            if (useRed && useGreen) {
                currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % green colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% red coloc\t% green coloc\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            } else if (useRed && useBlue) {
                currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% red coloc\t% blue coloc\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            } else if (useGreen && useBlue) {
                currentIntensity1 = "     % green colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% green coloc\t% blue coloc\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            } else {
                currentIntensity1 = "     % " + imageA.getImageName() + " colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % " + imageB.getImageName() + " colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% coloc1\t% coloc2\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            }

            if (doSecondIteration) {
                dataLine1 = dataLine1 + "Linear correlation coefficient - 2 iters\t";
                dataLine2 = dataLine2 + linearCorrelation + "\n";
            } else {
                dataLine1 = dataLine1 + "Linear correlation coefficient - 1 iter\t";
                dataLine2 = dataLine2 + linearCorrelation + "\n";
            }

            if (Float.isNaN(threshold)) {
                currentThreshold = "     Linear correlation coefficient is undefined";
                dataLine1 += "Linear correlation coefficient is undefined";
            } else {
                currentThreshold = "     Linear correlation coefficient = " + threshold;
                dataLine1 += "Linear correlation coefficient = " + threshold;
            }

            if ((useRed) && (useGreen)) {
                currentThreshold += " for pixels with red < " + index1 + " or green < " + index2;
                dataLine1 += " for pixels with red < " + index1 + " or green < " + index2 + "\n";
            } else if ((useRed) && (useBlue)) {
                currentThreshold += " for pixels with red < " + index1 + " or blue < " + index2;
                dataLine1 += " for pixels with red < " + index1 + " or blue < " + index2 + "\n";
            } else if ((useGreen) && (useBlue)) {
                currentThreshold += " for pixels with green < " + index1 + " or blue < " + index2;
                dataLine1 += " for pixels with green < " + index1 + " or blue < " + index2 + "\n";
            } else {
                currentThreshold += " for pixels with " + imageA.getImageName() + " < " + index1 + " or " +
                                    imageB.getImageName() + " < " + index2;
                dataLine1 += " for pixels with " + imageA.getImageName() + " < " + index1 + " or " +
                             imageB.getImageName() + " < " + index2 + "\n";
            }

            localizeFrame.setCurrentLabels(currentColoc, currentIntensity1, currentIntensity2, currentThreshold);
            userInterface.setDataText(dataLine1);
            userInterface.setDataText(dataLine2);
            userInterface.setDataText("\n");

            ((VOIPoint) (pointVOI.getCurves().elementAt(0))).locateVOIPoint(xS, yS, 0, xDim, yDim, 1);
        } // if (freeRangeMode)
        else { // !freeRangeMode

            // Find the intersection of the perpindicular from the mouse to
            // the least squares fit line
            xIntersectd = ((xS * xdiff) + ((yS - lineOffset) * ydiff)) / (xdiff + (lineSlope * ydiff));

            if (xIntersectd < minimumx) {
                xIntersectd = minimumx;
            }

            if (xIntersectd > maximumx) {
                xIntersectd = maximumx;
            }

            xIntersect = (int) Math.round(xIntersectd);
            yIntersectd = (lineSlope * xIntersectd) + lineOffset;
            yIntersect = (int) Math.round(yIntersectd);

            if (thresholdOn1) {
                index1 = (int) Math.round(((xIntersectd - leftPad) / scale1) + min1);

                if (index1 < lineMin) {
                    index1 = lineMin;
                }

                if (index1 > (lineMin + rThreshold.length - 1)) {
                    index1 = lineMin + rThreshold.length - 1;
                }

                index2f = (index1 * originalLineSlope) + originalLineOffset;
                index2 = Math.round(index2f);

                if (!haveThreshold[index1 - lineMin]) {
                    alg.calculateThreshold(index1 - lineMin);
                }

                threshold = rThreshold[index1 - lineMin];
                colocAreaPercent = colocSize[index1 - lineMin];
                colocIntensityPercent1 = colocIntensity1[index1 - lineMin];
                colocIntensityPercent2 = colocIntensity2[index1 - lineMin];
                firstIndex = index1;
                secondIndex = index2f;
            } else {
                yIntersectd = (2 * topPad) + bin2 - 1 - yIntersectd;
                index2 = (int) Math.round(((yIntersectd - topPad) / scale2) + min2);

                if (index2 < lineMin) {
                    index2 = lineMin;
                }

                if (index2 > (lineMin + rThreshold.length - 1)) {
                    index2 = lineMin + rThreshold.length - 1;
                }

                index1f = (index2 - originalLineOffset) / originalLineSlope;
                index1 = Math.round(index1f);

                if (!haveThreshold[index2 - lineMin]) {
                    alg.calculateThreshold(index2 - lineMin);
                }

                threshold = rThreshold[index2 - lineMin];
                colocAreaPercent = colocSize[index2 - lineMin];
                colocIntensityPercent1 = colocIntensity1[index2 - lineMin];
                colocIntensityPercent2 = colocIntensity2[index2 - lineMin];
                firstIndex = index1f;
                secondIndex = index2;
            }

            if (imageA.getNDims() > 2) {
                currentColoc = "     % colocalization volume = " + colocAreaPercent;
                dataLine1 = "%coloc vol\t";
            } else {
                currentColoc = "     % colocalization area = " + colocAreaPercent;
                dataLine1 = "%coloc area\t";
            }

            dataLine2 = colocAreaPercent + "\t";

            if (useRed && useGreen) {
                currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % green colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% red coloc\t% green coloc\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            } else if (useRed && useBlue) {
                currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% red coloc\t% blue coloc\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            } else if (useGreen && useBlue) {
                currentIntensity1 = "     % green colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% green coloc\t% blue coloc\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            } else {
                currentIntensity1 = "     % " + imageA.getImageName() + " colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % " + imageB.getImageName() + " colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% coloc1\t% coloc2\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            }

            if (doSecondIteration) {
                dataLine1 = dataLine1 + "Linear correlation coefficient - 2 iters\t";
                dataLine2 = dataLine2 + linearCorrelation + "\n";
            } else {
                dataLine1 = dataLine1 + "Linear correlation coefficient - 1 iter\t";
                dataLine2 = dataLine2 + linearCorrelation + "\n";
            }

            if (Float.isNaN(threshold)) {
                currentThreshold = "     Linear correlation coefficient is undefined";
                dataLine1 += "Linear correlation coefficient is undefined";
            } else {
                currentThreshold = "     Linear correlation coefficient = " + threshold;
                dataLine1 += "Linear correlation coefficient = " + threshold;
            }

            if (thresholdOn1) {

                if ((useRed) && (useGreen)) {
                    currentThreshold += " for pixels with red < " + index1 + " or green < " + index2f;
                    dataLine1 += " for pixels with red < " + index1 + " or green < " + index2f + "\n";
                } else if ((useRed) && (useBlue)) {
                    currentThreshold += " for pixels with red < " + index1 + " or blue < " + index2f;
                    dataLine1 += " for pixels with red < " + index1 + " or blue < " + index2f + "\n";
                } else if ((useGreen) && (useBlue)) {
                    currentThreshold += " for pixels with green < " + index1 + " or blue < " + index2f;
                    dataLine1 += " for pixels with green < " + index1 + " or blue < " + index2f + "\n";
                } else {
                    currentThreshold += " for pixels with " + imageA.getImageName() + " < " + index1 + " or " +
                                        imageB.getImageName() + " < " + index2f;
                    dataLine1 += " for pixels with " + imageA.getImageName() + " < " + index1 + " or " +
                                 imageB.getImageName() + " < " + index2f + "\n";
                }
            } else {

                if ((useRed) && (useGreen)) {
                    currentThreshold += " for pixels with red < " + index1f + " or green < " + index2;
                    dataLine1 += " for pixels with red < " + index1f + " or green < " + index2 + "\n";
                } else if ((useRed) && (useBlue)) {
                    currentThreshold += " for pixels with red < " + index1f + " or blue < " + index2;
                    dataLine1 += " for pixels with red < " + index1f + " or blue < " + index2 + "\n";
                } else if ((useGreen) && (useBlue)) {
                    currentThreshold += " for pixels with green < " + index1f + " or blue < " + index2;
                    dataLine1 += " for pixels with green < " + index1f + " or blue < " + index2 + "\n";
                } else {
                    currentThreshold += " for pixels with " + imageA.getImageName() + " < " + index1f + " or " +
                                        imageB.getImageName() + " < " + index2;
                    dataLine1 += " for pixels with " + imageA.getImageName() + " < " + index1f + " or " +
                                 imageB.getImageName() + " < " + index2 + "\n";
                }
            }

            localizeFrame.setCurrentLabels(currentColoc, currentIntensity1, currentIntensity2, currentThreshold);
            userInterface.setDataText(dataLine1);
            userInterface.setDataText(dataLine2);
            userInterface.setDataText("\n");

            ((VOIPoint) (pointVOI.getCurves().elementAt(0))).locateVOIPoint(xIntersect, yIntersect, 0, xDim, yDim,
                                                                               1);
        } // !freeRangeMode

        paintComponent(getGraphics());
        editImageFrameA.setThresholds(firstIndex, secondIndex);

        if (editImageFrameB != null) {
            editImageFrameB.setThresholds(firstIndex, secondIndex);
        }

        zInitial = imageA.getParentFrame().getComponentImage().getSlice();

        if (imageA.isColorImage()) {

            for (t = 0; t < tDim; t++) {

                for (z = zInitial + 1; z < zDim; z++) {
                    editImageFrameA.show(t, z, true);
                }

                for (z = 0; z <= zInitial; z++) {
                    editImageFrameA.show(t, z, true);
                }
            }
        } // if (imageA.isColorImage())
        else { // black and white images

            for (t = 0; t < tDim; t++) {

                for (z = zInitial + 1; z < zDim; z++) {
                    editImageFrameA.show(t, z, null, null, true, interpMode);
                    editImageFrameB.show(t, z, null, null, true, interpMode);
                }

                for (z = 0; z <= zInitial; z++) {
                    editImageFrameA.show(t, z, null, null, true, interpMode);
                    editImageFrameB.show(t, z, null, null, true, interpMode);
                }
            }
        } // black and white images
    }

    /**
     * Paints the image and border.
     *
     * @param  g  Graphics handle
     */
    /* Since paintComponent is used rather than paintAnimate or some other name, then in addition to each
     * direct call to paintComponent in updateImages, 1 or 2 calls to paintComponent maybe made by Jcomponent.paint. */
    public void paintComponent(Graphics g) {
        Vector3f pt;
        int xPos, yPos;
        int leftX, bottomY, rightX, topY;
        int currentX, currentY;
        int bottomSPos, leftSPos;
        int min1X, min1Y;
        Color color1, color2;
        FontMetrics metrics;
        int fontHeight;
        int maxStrWidth;
        int bottomCurrentX;
        int leftCurrentY;

        try {

            if (g == null) {
                return;
            }

            if (imgDest != null) {
                metrics = g.getFontMetrics(g.getFont());
                fontHeight = metrics.getHeight();

                if (imageA.isColorImage()) {
                    maxStrWidth = metrics.stringWidth(String.valueOf(Math.round(max2)));
                } else {
                    maxStrWidth = metrics.stringWidth(String.valueOf(max2));
                }

                g.setClip(getVisibleRect());

                if (((zoomX * resX) != 1.0f) || ((zoomY * resY) != 1.0f)) {
                    g.drawImage(imgDest, 0, 0, (int) ((zoomX * imageDim.width * resX) + 0.5),
                                (int) ((zoomY * imageDim.height * resY) + 0.5), 0, 0, imageDim.width, imageDim.height,
                                this);
                } else {
                    g.drawImage(imgDest, 0, 0, this);
                }

                /*  DRAWING DISABLED
                lineVOI.drawSelf(zoomX, zoomY, resX, resY, 0, 0, destImage.getFileInfo(0).getResolutions(),
                                 destImage.getFileInfo(0).getUnitsOfMeasure(), 0, FileInfoBase.UNKNOWN_ORIENT, g);
*/
                pt = pointVOI.exportPoint();
                xPos = (int) Math.round(pt.X * zoomX * resX);
                yPos = (int) Math.round(pt.Y * zoomY * resY);
                g.setColor(Color.cyan);
                g.fillRect(xPos - 2, yPos - 2, 5, 5);
                g.setColor(Color.yellow);
                g.drawRect(xPos - 2, yPos - 2, 5, 5);
                rightX = (int) (((imageDim.width - 1 - rightPad) * zoomX * resX) + 0.5);
                topY = (int) ((topPad * zoomY * resY) + 0.5);

                if (regionLinesDisplay) {
                    g.drawLine(xPos + 2, yPos, rightX, yPos);
                    g.drawLine(xPos, yPos - 2, xPos, topY);
                }

                leftX = (int) (((leftPad - 20) * zoomX * resX) + 0.5);
                leftSPos = Math.max(leftX - 35, 0);
                leftCurrentY = Math.max(leftSPos - maxStrWidth, 0);
                bottomY = (int) (((imageDim.height - 1 - (bottomPad - 20)) * zoomY * resY) + 0.5);
                bottomSPos = Math.min(bottomY + 20, (int) (((imageDim.height - 1) * zoomY * resY) + 0.5));
                bottomCurrentX = Math.min(bottomSPos + fontHeight,
                                          (int) (((imageDim.height - 1) * zoomY * resY) + 0.5));
                min1X = (int) ((leftPad * zoomX * resX) + 0.5);
                min1Y = (int) (((imageDim.height - 1 - bottomPad) * zoomY * resY) + 0.5);
                currentX = (int) (min1X + ((rightX - min1X) * (firstIndex - min1) / (max1 - min1)));
                currentY = (int) (min1Y + ((topY - min1Y) * (secondIndex - min2) / (max2 - min2)));

                if (useRed && useGreen) {
                    color1 = Color.red;
                    color2 = Color.green;
                } else if (useRed && useBlue) {
                    color1 = Color.red;
                    color2 = Color.blue;
                } else if (useGreen && useBlue) {
                    color1 = Color.green;
                    color2 = Color.blue;
                } else {
                    color1 = Color.white;
                    color2 = Color.white;
                }

                g.setColor(color1);
                g.drawLine(leftX, bottomY, rightX, bottomY);
                g.drawLine(min1X, bottomY - 2, min1X, bottomY + 2);

                if (imageA.isColorImage()) {
                    g.drawString(String.valueOf(Math.round(min1)), min1X, bottomSPos);
                } else {
                    g.drawString(String.valueOf(min1), min1X, bottomSPos);
                }

                g.drawLine(currentX, bottomY - 2, currentX, bottomY + 2);

                if (imageA.isColorImage()) {
                    g.drawString(String.valueOf(Math.round(firstIndex)), currentX, bottomCurrentX);
                } else {
                    g.drawString(String.valueOf(firstIndex), currentX, bottomCurrentX);
                }

                g.drawLine(rightX, bottomY - 2, rightX, bottomY + 2);

                if (imageA.isColorImage()) {
                    g.drawString(String.valueOf(Math.round(max1)), rightX, bottomSPos);
                } else {
                    g.drawString(String.valueOf(max1), rightX, bottomSPos);
                }

                g.setColor(color2);
                g.drawLine(leftX, bottomY, leftX, topY);
                g.drawLine(leftX - 2, min1Y, leftX + 2, min1Y);

                if (imageA.isColorImage()) {
                    g.drawString(String.valueOf(Math.round(min2)), leftSPos, min1Y);
                } else {
                    g.drawString(String.valueOf(min2), leftSPos, min1Y);
                }

                g.drawLine(leftX - 2, currentY, leftX + 2, currentY);

                if (imageA.isColorImage()) {
                    g.drawString(String.valueOf(Math.round(secondIndex)), leftCurrentY, currentY);
                } else {
                    g.drawString(String.valueOf(secondIndex), leftCurrentY, currentY);
                }

                g.drawLine(leftX - 2, topY, leftX + 2, topY);

                if (imageA.isColorImage()) {
                    g.drawString(String.valueOf(Math.round(max2)), leftSPos, topY);
                } else {
                    g.drawString(String.valueOf(max2), leftSPos, topY);
                }
            } // if (img != null)
        } // try
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentColocalizationRegression.paint.");
        }
    }

    /**
     * Passes arrays needed in free range mode.
     *
     * @param  haveFreeRangeThreshold    DOCUMENT ME!
     * @param  freeRangeRThreshold       DOCUMENT ME!
     * @param  freeRangeColocSize        DOCUMENT ME!
     * @param  freeRangeColocIntensity1  DOCUMENT ME!
     * @param  freeRangeColocIntensity2  DOCUMENT ME!
     */
    public void passFreeRangeArrays(boolean[] haveFreeRangeThreshold, float[] freeRangeRThreshold,
                                    float[] freeRangeColocSize, float[] freeRangeColocIntensity1,
                                    float[] freeRangeColocIntensity2) {

        this.haveFreeRangeThreshold = haveFreeRangeThreshold;
        this.freeRangeRThreshold = freeRangeRThreshold;
        this.freeRangeColocSize = freeRangeColocSize;
        this.freeRangeColocIntensity1 = freeRangeColocIntensity1;
        this.freeRangeColocIntensity2 = freeRangeColocIntensity2;
    }

    /**
     * Method to set the brightness and contrast of the animate images.
     *
     * @param  brightness  int going from -255 to 255
     * @param  contrast    float scale factor
     */
    public void setBrightness(int brightness, float contrast) {
        this.brightness = brightness;
        this.contrast = contrast;
        haveFiltered = true;
        buildImageDestObject(null, true);
        paintComponent(getGraphics());
    }

    /**
     * Accessor that sets freeRangeMode.
     *
     * @param  freeRangeMode  DOCUMENT ME!
     */
    public void setFreeRangeMode(boolean freeRangeMode) {
        this.freeRangeMode = freeRangeMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  logMagDisplay  DOCUMENT ME!
     */
    public void setLogMagDisplay(boolean logMagDisplay) {
        this.logMagDisplay = logMagDisplay;
    }

    /**
     * Modifies component after mouseReleased in contour VOI indicating that the VOI has been moved.
     *
     * @param  originalLineSlope   DOCUMENT ME!
     * @param  originalLineOffset  DOCUMENT ME!
     * @param  haveThreshold       DOCUMENT ME!
     * @param  rThreshold          DOCUMENT ME!
     * @param  colocSize           DOCUMENT ME!
     * @param  colocIntensity1     DOCUMENT ME!
     * @param  colocIntensity2     DOCUMENT ME!
     * @param  min1                DOCUMENT ME!
     * @param  max1                DOCUMENT ME!
     * @param  min2                DOCUMENT ME!
     * @param  max2                DOCUMENT ME!
     * @param  scale1              DOCUMENT ME!
     * @param  scale2              DOCUMENT ME!
     * @param  lineMin1            DOCUMENT ME!
     * @param  lineMax1            DOCUMENT ME!
     * @param  lineMin2            DOCUMENT ME!
     * @param  lineMax2            DOCUMENT ME!
     * @param  thresholdOn1        DOCUMENT ME!
     * @param  linearCorrelation   DOCUMENT ME!
     */
    public void setNewVar(float originalLineSlope, float originalLineOffset, boolean[] haveThreshold,
                          float[] rThreshold, float[] colocSize, float[] colocIntensity1, float[] colocIntensity2,
                          double min1, double max1, double min2, double max2, double scale1, double scale2,
                          double lineMin1, double lineMax1, double lineMin2, double lineMax2, boolean thresholdOn1,
                          float linearCorrelation) {
        this.originalLineSlope = originalLineSlope;
        this.originalLineOffset = originalLineOffset;
        this.haveThreshold = haveThreshold;
        this.rThreshold = rThreshold;
        this.colocSize = colocSize;
        this.colocIntensity1 = colocIntensity1;
        this.colocIntensity2 = colocIntensity2;
        this.min1 = min1;
        this.max1 = max1;
        this.min2 = min2;
        this.max2 = max2;
        this.scale1 = scale1;
        this.scale2 = scale2;
        this.linearCorrelation = linearCorrelation;
        this.thresholdOn1 = thresholdOn1;

        if (thresholdOn1) {
            lineMin = (int) Math.ceil(lineMin1);
        } else {
            lineMin = (int) Math.ceil(lineMin2);
        }

        minimumx = ((lineMin1 - min1) * scale1) + leftPad;
        maximumx = ((lineMax1 - min1) * scale1) + leftPad;

        VOIs = destImage.getVOIs();

        if (VOIs != null) {
            nVOI = VOIs.size();
        } else {
            nVOI = 0;
        }

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.LINE) {
                lineVOI = VOIs.VOIAt(i);
            } else if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                pointVOI = VOIs.VOIAt(i);
            }
        }

        if (lineVOI == null) {
            MipavUtil.displayError("Expected line VOI not found");

            return;
        }

        if (pointVOI == null) {
            MipavUtil.displayError("Expected point VOI not found");

            return;
        }

        ((VOILine) (lineVOI.getCurves().elementAt(0))).exportArrays(xArray, yArray, zArray);

        lineSlope = (yArray[1] - yArray[0]) / (xArray[1] - xArray[0]);
        lineOffset = yArray[0] - (xArray[0] * lineSlope);

        xdiff = xArray[1] - xArray[0];
        ydiff = yArray[1] - yArray[0];

        paintBitmapDest = destImage.getMask();

        buildImageDestObject(null, true);
        update();
    }

    /**
     * Accessor that sets the paint mask.
     *
     * @param  mask  DOCUMENT ME!
     */
    public void setPaintMaskDest(BitSet mask) {
        paintBitmapDest = mask;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  xS  DOCUMENT ME!
     * @param  yS  DOCUMENT ME!
     */
    public void setPosition(int xS, int yS) {
        int xIntersect, yIntersect;
        double xIntersectd, yIntersectd;
        int index1 = 0;
        int index2 = 0;
        float index1f = 0.0f;
        float index2f = 0.0f;
        float threshold;
        String currentThreshold;
        float colocAreaPercent;
        String currentColoc;
        float colocIntensityPercent1;
        String currentIntensity1;
        float colocIntensityPercent2;
        String currentIntensity2;
        int t, z;
        int zInitial;
        int ySInvert;
        int ch1, ch2;
        int index = 0;
        String dataLine1;
        String dataLine2;

        if (freeRangeMode) {
            ch1 = xS - leftPad;

            if (ch1 < 0) {
                ch1 = 0;
            }

            if (ch1 >= bin1) {
                ch1 = bin1 - 1;
            }

            index1 = (int) Math.round((ch1 / scale1) + min1);
            ySInvert = (2 * topPad) + bin2 - 1 - yS;
            ch2 = ySInvert - topPad;

            if (ch2 < 0) {
                ch2 = 0;
            }

            if (ch2 >= bin2) {
                ch2 = bin2 - 1;
            }

            index2 = (int) Math.round((ch2 / scale2) + min2);
            index = ch1 + (bin1 * ch2);

            if (!haveFreeRangeThreshold[index]) {
                alg.calculateFreeRangeThreshold(ch1, ch2);
            }

            threshold = freeRangeRThreshold[index];
            colocAreaPercent = freeRangeColocSize[index];
            colocIntensityPercent1 = freeRangeColocIntensity1[index];
            colocIntensityPercent2 = freeRangeColocIntensity2[index];
            firstIndex = index1;
            secondIndex = index2;

            if (imageA.getNDims() > 2) {
                currentColoc = "     % colocalization volume = " + colocAreaPercent;
                dataLine1 = "%coloc vol\t";
            } else {
                currentColoc = "     % colocalization area = " + colocAreaPercent;
                dataLine1 = "%coloc area\t";
            }

            dataLine2 = colocAreaPercent + "\t";

            if (useRed && useGreen) {
                currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % green colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% red coloc\t% green coloc\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            } else if (useRed && useBlue) {
                currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% red coloc\t% blue coloc\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            } else if (useGreen && useBlue) {
                currentIntensity1 = "     % green colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% green coloc\t% blue coloc\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            } else {
                currentIntensity1 = "     % " + imageA.getImageName() + " colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "     % " + imageB.getImageName() + " colocalization = " + colocIntensityPercent2;
                dataLine1 = dataLine1 + "% coloc1\t% coloc2\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
            }

            if (doSecondIteration) {
                dataLine1 = dataLine1 + "Linear correlation coefficient - 2 iters\t";
                dataLine2 = dataLine2 + linearCorrelation + "\n";
            } else {
                dataLine1 = dataLine1 + "Linear correlation coefficient - 1 iter\t";
                dataLine2 = dataLine2 + linearCorrelation + "\n";
            }

            if (Float.isNaN(threshold)) {
                currentThreshold = "     Linear correlation coefficient is undefined";
                dataLine1 += "Linear correlation coefficient is undefined";
            } else {
                currentThreshold = "     Linear correlation coefficient = " + threshold;
                dataLine1 += "Linear correlation coefficient = " + threshold;
            }

            if ((useRed) && (useGreen)) {
                currentThreshold += " for pixels with red < " + index1 + " or green < " + index2;
                dataLine1 += " for pixels with red < " + index1 + " or green < " + index2 + "\n";
            } else if ((useRed) && (useBlue)) {
                currentThreshold += " for pixels with red < " + index1 + " or blue < " + index2;
                dataLine1 += " for pixels with red < " + index1 + " or blue < " + index2 + "\n";
            } else if ((useGreen) && (useBlue)) {
                currentThreshold += " for pixels with green < " + index1 + " or blue < " + index2;
                dataLine1 += " for pixels with green < " + index1 + " or blue < " + index2 + "\n";
            } else {
                currentThreshold += " for pixels with " + imageA.getImageName() + " < " + index1 + " or " +
                                    imageB.getImageName() + " < " + index2;
                dataLine1 += " for pixels with " + imageA.getImageName() + " < " + index1 + " or " +
                             imageB.getImageName() + " < " + index2 + "\n";
            }

            localizeFrame.setCurrentLabels(currentColoc, currentIntensity1, currentIntensity2, currentThreshold);
            userInterface.setDataText(dataLine1);
            userInterface.setDataText(dataLine2);
            userInterface.setDataText("\n");

            ((VOIPoint) (pointVOI.getCurves().elementAt(0))).locateVOIPoint(xS, yS, 0, xDim, yDim, 1);
        } // if (freeRangeMode)
        else { // !freeRangeMode

            // Find the intersection of the perpindicular from the mouse to
            // the least squares fit line
            xIntersectd = ((xS * xdiff) + ((yS - lineOffset) * ydiff)) / (xdiff + (lineSlope * ydiff));

            if (xIntersectd < minimumx) {
                xIntersectd = minimumx;
            }

            if (xIntersectd > maximumx) {
                xIntersectd = maximumx;
            }

            xIntersect = (int) Math.round(xIntersectd);
            yIntersectd = (lineSlope * xIntersectd) + lineOffset;
            yIntersect = (int) Math.round(yIntersectd);

            if (thresholdOn1) {
                index1 = (int) Math.round(((xIntersectd - leftPad) / scale1) + min1);

                if (index1 < lineMin) {
                    index1 = lineMin;
                }

                if (index1 > (lineMin + rThreshold.length - 1)) {
                    index1 = lineMin + rThreshold.length - 1;
                }

                index2f = (index1 * originalLineSlope) + originalLineOffset;
                index2 = Math.round(index2f);
                threshold = rThreshold[index1 - lineMin];
                colocAreaPercent = colocSize[index1 - lineMin];
                colocIntensityPercent1 = colocIntensity1[index1 - lineMin];
                colocIntensityPercent2 = colocIntensity2[index1 - lineMin];
                firstIndex = index1;
                secondIndex = index2f;
            } else {
                yIntersectd = (2 * topPad) + bin2 - 1 - yIntersectd;
                index2 = (int) Math.round(((yIntersectd - topPad) / scale2) + min2);

                if (index2 < lineMin) {
                    index2 = lineMin;
                }

                if (index2 > (lineMin + rThreshold.length - 1)) {
                    index2 = lineMin + rThreshold.length - 1;
                }

                index1f = (index2 - originalLineOffset) / originalLineSlope;
                index1 = Math.round(index1f);
                threshold = rThreshold[index2 - lineMin];
                colocAreaPercent = colocSize[index2 - lineMin];
                colocIntensityPercent1 = colocIntensity1[index2 - lineMin];
                colocIntensityPercent2 = colocIntensity2[index2 - lineMin];
                firstIndex = index1f;
                secondIndex = index2;
            }

            currentColoc = "    % colocalization area = " + colocAreaPercent;

            if (useRed && useGreen) {
                currentIntensity1 = "    % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "    % green colocalization = " + colocIntensityPercent2;
            } else if (useRed && useBlue) {
                currentIntensity1 = "    % red colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "    % blue colocalization = " + colocIntensityPercent2;
            } else if (useGreen && useBlue) {
                currentIntensity1 = "    % green colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "    % blue colocalization = " + colocIntensityPercent2;
            } else {
                currentIntensity1 = "    % " + imageA.getImageName() + " colocalization = " + colocIntensityPercent1;
                currentIntensity2 = "    % " + imageB.getImageName() + " colocalization = " + colocIntensityPercent2;
            }

            if (Float.isNaN(threshold)) {
                currentThreshold = "     Linear correlation coefficient is undefined";
            } else {
                currentThreshold = "     Linear correlation coefficient = " + threshold;
            }

            if (thresholdOn1) {

                if ((useRed) && (useGreen)) {
                    currentThreshold += " for pixels with red < " + index1 + " or green < " + index2f;
                } else if ((useRed) && (useBlue)) {
                    currentThreshold += " for pixels with red < " + index1 + " or blue < " + index2f;
                } else if ((useGreen) && (useBlue)) {
                    currentThreshold += " for pixels with green < " + index1 + " or blue < " + index2f;
                } else {
                    currentThreshold += " for pixels with " + imageA.getImageName() + " < " + index1 + " or " +
                                        imageB.getImageName() + " < " + index2f;
                }
            } else {

                if ((useRed) && (useGreen)) {
                    currentThreshold += " for pixels with red < " + index1f + " or green < " + index2;
                } else if ((useRed) && (useBlue)) {
                    currentThreshold += " for pixels with red < " + index1f + " or blue < " + index2;
                } else if ((useGreen) && (useBlue)) {
                    currentThreshold += " for pixels with green < " + index1f + " or blue < " + index2;
                } else {
                    currentThreshold += " for pixels with " + imageA.getImageName() + " < " + index1f + " or " +
                                        imageB.getImageName() + " < " + index2;
                }
            }

            localizeFrame.setCurrentLabels(currentColoc, currentIntensity1, currentIntensity2, currentThreshold);

            ((VOIPoint) (pointVOI.getCurves().elementAt(0))).locateVOIPoint(xIntersect, yIntersect, 0, xDim, yDim,
                                                                               1);
        } // else not freeRangeMode

        paintComponent(getGraphics());
        editImageFrameA.setThresholds(firstIndex, secondIndex);

        if (editImageFrameB != null) {
            editImageFrameB.setThresholds(firstIndex, secondIndex);
        }

        zInitial = imageA.getParentFrame().getComponentImage().getSlice();

        if (imageA.isColorImage()) {

            for (t = 0; t < tDim; t++) {

                for (z = zInitial + 1; z < zDim; z++) {
                    editImageFrameA.show(t, z, true);
                }

                for (z = 0; z <= zInitial; z++) {
                    editImageFrameA.show(t, z, true);
                }
            }
        } // if (imageA.isColorImage())
        else { // black and white images

            for (t = 0; t < tDim; t++) {

                for (z = zInitial + 1; z < zDim; z++) {
                    editImageFrameA.show(t, z, null, null, true, interpMode);
                    editImageFrameB.show(t, z, null, null, true, interpMode);
                }

                for (z = 0; z <= zInitial; z++) {
                    editImageFrameA.show(t, z, null, null, true, interpMode);
                    editImageFrameB.show(t, z, null, null, true, interpMode);
                }
            }
        } // black and white images
    }

    /**
     * DOCUMENT ME!
     *
     * @param  regionLinesDisplay  DOCUMENT ME!
     */
    public void setRegionLinesDisplay(boolean regionLinesDisplay) {
        this.regionLinesDisplay = regionLinesDisplay;
    }

    // The following 2 functions set the RGB tables for ARGB images A and B.
    /**
     * Sets the RGB table for the ARGB image A.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTA(ModelRGB RGBT) {
        RGBTA = RGBT;
    }

    /**
     * Sets the RGB table for the ARGB image B.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTB(ModelRGB RGBT) {
        RGBTB = RGBT;
    }

    /**
     * Sets the magnification in both x and y directions.
     *
     * @param  zX  zoom in the x direction
     * @param  zY  zoom in the y direction
     */
    public void setZoom(float zX, float zY) {
        zoomX = zX;
        zoomY = zY;
        setSize(Math.round(zX * imageDim.width * resX), Math.round(zY * imageDim.height * resY));
    }

    /**
     * Method to call paint without erasing background this reduces flicker!
     */
    public void update() {
        paintComponent(getGraphics());
    }


    /**
     * Method to call paint without erasing background this reduces flicker!
     *
     * @param  g  Graphics handle
     */
    public void update(Graphics g) {
        paintComponent(g);
    }

}
