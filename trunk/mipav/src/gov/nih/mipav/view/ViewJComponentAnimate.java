package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.image.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class ViewJComponentAnimate extends ViewJComponentBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2707021974594065054L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Opacity value used by the paint brush. value = 1.0 - opaque value = 0.25 - default (mostly see through) */
    public float OPACITY = 0.25f;

    /** alphaBlending values for compositing two images. */
    protected float alphaBlend = 0.5f;

    /** DOCUMENT ME! */
    protected float alphaPrime = 0.5f;

    /** frame - frame where the component image is displayed. */
    protected ViewJFrameBase frame;

    /** imageA - model for image A. */
    protected ModelImage imageA;

    /** imageB - model for image B. */
    protected ModelImage imageB;

    /** DOCUMENT ME! */
    protected Image[] img;

    /** DOCUMENT ME! */
    protected int interpMode = SMOOTH;

    /** LUTa - lookup table for image A. */
    protected ModelLUT LUTa;

    /** LUTb - lookup table for image B. */
    protected ModelLUT LUTb;


    /** DOCUMENT ME! */
    protected int[] lutBufferRemapped = null;

    /** mode - used to describe the cursor mode. */
    protected int mode;

    /** true if selected red, green, blue components present in RGB image. */
    protected ModelRGB RGBTA;

    /** DOCUMENT ME! */
    protected ModelRGB RGBTB;

    /** DOCUMENT ME! */
    protected int slice = -99;

    /** DOCUMENT ME! */
    protected String string;

    /** DOCUMENT ME! */
    protected long time;

    /** DOCUMENT ME! */
    protected int timeSlice = 0;

    /** DOCUMENT ME! */
    private Color borderCol; // for 4D color of border around each z slice

    /** DOCUMENT ME! */
    private int brightness; // offset ranging from -255 to 255 add to each
                            // scaled red, green, and blue

    /** DOCUMENT ME! */
    private float contrast; // scale factor ranging from 0.1 to 10.0
                            // by which to multiply each red, green, and blue

    /** DOCUMENT ME! */
    private int curSlice; // z slice given by the z slice slider

    /** DOCUMENT ME! */
    private boolean disposeImage; // whether or not to dispose of imageA and imageB
                                  // true unless ViewJFrameAnimate was passed an
                                  // unscaled 3D image

    /** DOCUMENT ME! */
    private boolean haveFiltered = false; // whether or not the brightness/
                                          // contrast filter has been invoked

    /** DOCUMENT ME! */
    private boolean[] ignoreSlice; // set true if deleteSlice hit in ViewJFrameAnimate

    /** DOCUMENT ME! */
    private ModelImage imageActive = null;

    /** DOCUMENT ME! */
    private float[] imageBufferA = null;

    /** DOCUMENT ME! */
    private float[] imageBufferActive = null;

    /** DOCUMENT ME! */
    private float[] imageBufferB = null;

    /** DOCUMENT ME! */
    private boolean logMagDisplay = false;

    /** DOCUMENT ME! */
    private MemoryImageSource memImage;

    /** DOCUMENT ME! */
    private MediaTracker mt = null;

    /** DOCUMENT ME! */
    private int nRow, nColumn; // in 4D row and column numbers for z slices

    /** DOCUMENT ME! */
    private int nVOI; // number of vois

    /** DOCUMENT ME! */
    private int originalZDim; // the number of z slices in a 4D image

    // equal to zDim before the JDialogAnimate 4D to 3D conversion

    /** Buffer used to indicate if the pixel location is painted (true) or unpainted (false). */
    private BitSet paintBitmap;

    /** Buffer that displays the combined paintBitmap and pixBuffer buffers. */
    private int[] paintBuffer = null;

    /** Buffer used to store ARGB images of the image presently being displayed. */
    private int[] pixBuffer = null;

    /** DOCUMENT ME! */
    private int red, green, blue;

    /** DOCUMENT ME! */
    private float resX, resY; // x and y resolutions

    /** DOCUMENT ME! */
    private boolean showNumbers; // whether to number each z slice for 4D

    /** DOCUMENT ME! */
    private boolean showSliceNumber; // whether to number complete frame for 3D

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;

    /** DOCUMENT ME! */
    private int xDim, yDim, zSlice, zDim;

    /** DOCUMENT ME! */
    private int[] xLabel; // in 4D x location of slice numbering string

    /** DOCUMENT ME! */
    private int[] yLabel; // in 4D y location of slice numbering string

    /** DOCUMENT ME! */
    private float zoomX = 1; // magnification, here zoomX = zoomY

    /** DOCUMENT ME! */
    private float zoomY = 1; // and zoom is always a power of 2

    /** DOCUMENT ME! */
    private String[] zString; // string for displaying slice number

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  _frame         frame where the controls are obtained
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
     * @param  alphaBl        alphaBlend
     * @param  disposeImage   if true dispose of imageA and imageB
     */
    public ViewJComponentAnimate(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
                                 ModelImage _imageB, ModelLUT _LUTb, float[] imgBufferB, int[] pixelBuffer, float zoom,
                                 int[] extents, boolean logMagDisplay, float alphaBl, boolean disposeImage) {

        super(new Dimension(_imageA.getExtents()[0], _imageA.getExtents()[1]));

        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;
        VOIs = imageActive.getVOIs();

        if (VOIs != null) {
            nVOI = VOIs.size();
        } else {
            nVOI = 0;
        }

        this.disposeImage = disposeImage;
        zDim = imageA.getExtents()[2];
        img = new Image[zDim];

        // Before delete slice is pressed in ViewJFrameAnimate don't ignore any slices
        ignoreSlice = new boolean[zDim];

        for (int i = 0; i < zDim; i++) {
            ignoreSlice[i] = false;
        }

        string = "0";
        xDim = imageA.getExtents()[0];
        yDim = imageA.getExtents()[1];

        resX = imageA.getFileInfo(0).getResolutions()[0];
        resY = imageA.getFileInfo(0).getResolutions()[1];

        if ((resX <= 0.0f) || (resY <= 0.0f)) {
            resX = 1.0f;
            resY = 1.0f;
        }

        if (resX >= resY) {
            resX = resX / resY;
            resY = 1.0f;
        } else if (resY > resX) {
            resY = resY / resX;
            resX = 1.0f;
        }

        setSize(Math.round(imageDim.width * resX), Math.round(imageDim.height * resY));


        LUTa = _LUTa;
        LUTb = _LUTb;
        lutBufferRemapped = new int[1];
        alphaBlend = alphaBl;
        alphaPrime = 1 - alphaBlend;

        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
        pixBuffer = pixelBuffer;
        paintBitmap = imageA.getMask();
        imageBufferActive = imageBufferA;
        this.logMagDisplay = logMagDisplay;

        setZoom(zoom, zoom);
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * For generating the display of 1 or 2 RGB images.
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean buildImageObject(int tSlice, int zSlice, boolean forceShow) {
        // Note that alphaBlending is applied with 1 component taken as zero if both components are not present -for
        // example, if either imageA or imageB but not both has red, then the red component is alphaBlended with zero.

        int j;
        int bufferSize;
        int offset;
        int index;
        int Ra, Ga, Ba, Rb, Gb, Bb;
        int imageSize;
        int pixValue;
        float opacityPrime;
        float redMapped, greenMapped, blueMapped;
        int[] RGBIndexBufferA = new int[256];
        int[] RGBIndexBufferB = new int[256];
        float maxColorA = 255;
        float maxColorB = 255;
        float normColorB = 1;
        float normColorA = 1;
        int timeSliceA, timeSliceB;

        bufferSize = imageA.getSliceSize() * 4;
        imageSize = imageA.getSliceSize();

        if (imageA.getType() != ModelStorageBase.ARGB) {
            maxColorA = (float) imageA.getMaxR();
            maxColorA = Math.max((float) imageA.getMaxG(), maxColorA);
            maxColorA = Math.max((float) imageA.getMaxB(), maxColorA);
        }

        normColorA = 255 / maxColorA;

        if ((imageB != null) && (imageB.getType() != ModelStorageBase.ARGB)) {
            maxColorB = (float) imageB.getMaxR();
            maxColorB = Math.max((float) imageB.getMaxG(), maxColorB);
            maxColorB = Math.max((float) imageB.getMaxB(), maxColorB);
        }

        normColorB = 255 / maxColorB;

        if (frame.getControls() != null) {
            OPACITY = frame.getControls().getTools().getOpacity();
        }

        opacityPrime = 1 - OPACITY;
        red = borderCol.getRed();
        green = borderCol.getGreen();
        blue = borderCol.getBlue();

        // System.out.println(" RGBTA = " + RGBTA);
        if (RGBTA != null) {
            RGBIndexBufferA = RGBTA.exportIndexedRGB();
        }

        if ((imageB != null) && (RGBTB != null)) {
            RGBIndexBufferB = RGBTB.exportIndexedRGB();
        }

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
                zDimSlices = imageA.getExtents()[2];
            }

            try {
                imageA.exportData(((timeSliceA * zDimSlices * bufferSize) + (zSlice * bufferSize)), bufferSize,
                                  imageBufferA);

                if (imageB != null) {
                    imageB.exportData(((timeSliceB * zDimSlices * bufferSize) + (zSlice * bufferSize)), bufferSize,
                                      imageBufferB);
                }
            } catch (IOException error) {
                MipavUtil.displayError("" + error);

                return false;
            }
        }

        if (imageB == null) {
            offset = zSlice * imageSize;

            for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                if (RGBTA != null) {

                    if (RGBTA.getROn()) {
                        redMapped = (RGBIndexBufferA[(int) (imageBufferA[index + 1] * normColorA)] & 0x00ff0000) >> 16;
                    } else {
                        redMapped = 0;
                    }

                    if (RGBTA.getGOn()) {
                        greenMapped = (RGBIndexBufferA[(int) (imageBufferA[index + 2] * normColorA)] & 0x0000ff00) >> 8;
                    } else {
                        greenMapped = 0;
                    }

                    if (RGBTA.getBOn()) {
                        blueMapped = (RGBIndexBufferA[(int) (imageBufferA[index + 3] * normColorA)] & 0x000000ff);
                    } else {
                        blueMapped = 0;
                    }
                } // end of if (RGBTA != null)
                else {
                    redMapped = imageBufferA[index + 1] * normColorA;
                    greenMapped = imageBufferA[index + 2] * normColorA;
                    blueMapped = imageBufferA[index + 3] * normColorA;
                }

                if (paintBitmap.get(offset + j) == true) {
                    pixValue = 0xff000000 |
                                   (((int) ((redMapped * opacityPrime) + red) << 16) |
                                        (((int) ((greenMapped * opacityPrime) + green) << 8) |
                                             ((int) ((blueMapped * opacityPrime) + blue))));
                    paintBuffer[j] = pixValue;
                    pixBuffer[j] = 0xff000000 | ((int) redMapped << 16) | ((int) greenMapped << 8) | (int) blueMapped;
                } // end of if (paintBitmap.get(offset+j) == true)
                else {
                    pixValue = 0xff000000 |
                                   (((int) (redMapped) << 16) | (((int) (greenMapped) << 8) | ((int) (blueMapped))));
                    paintBuffer[j] = pixBuffer[j] = pixValue;
                }
            }
        } else {
            offset = zSlice * imageSize;

            for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                if ((RGBTA != null) && (RGBTB != null)) {

                    if (RGBTA.getROn()) {
                        Ra = (RGBIndexBufferA[(int) (imageBufferA[index + 1] * normColorA)] & 0x00ff0000) >> 16;
                    } else {
                        Ra = 0;
                    }

                    if (RGBTA.getGOn()) {
                        Ga = (RGBIndexBufferA[(int) (imageBufferA[index + 2] * normColorA)] & 0x0000ff00) >> 8;
                    } else {
                        Ga = 0;
                    }

                    if (RGBTA.getBOn()) {
                        Ba = (RGBIndexBufferA[(int) (imageBufferA[index + 3] * normColorA)] & 0x000000ff);
                    } else {
                        Ba = 0;
                    }


                    if (RGBTB.getROn()) {
                        Rb = (RGBIndexBufferB[(int) (imageBufferB[index + 1] * normColorB)] & 0x00ff0000) >> 16;
                    } else {
                        Rb = 0;
                    }

                    if (RGBTB.getGOn()) {
                        Gb = (RGBIndexBufferB[(int) (imageBufferB[index + 2] * normColorB)] & 0x0000ff00) >> 8;
                    } else {
                        Gb = 0;
                    }

                    if (RGBTB.getBOn()) {
                        Bb = (RGBIndexBufferB[(int) (imageBufferB[index + 3] * normColorB)] & 0x000000ff);
                    } else {
                        Bb = 0;
                    }
                } else {
                    Ra = (int) (imageBufferA[index + 1] * normColorA);
                    Ga = (int) (imageBufferA[index + 2] * normColorA);
                    Ba = (int) (imageBufferA[index + 3] * normColorA);

                    Rb = (int) (imageBufferB[index + 1] * normColorB);
                    Gb = (int) (imageBufferB[index + 2] * normColorB);
                    Bb = (int) (imageBufferB[index + 3] * normColorB);
                }

                if ((Rb == 0) && (Gb == 0) && (Bb == 0)) {
                    Ra = (int) (Ra);
                    Ga = (int) (Ga);
                    Ba = (int) (Ba);
                } else if ((Ra == 0) && (Ga == 0) && (Ba == 0)) {
                    Ra = (int) (Rb);
                    Ga = (int) (Gb);
                    Ba = (int) (Bb);
                } else {
                    Ra = (int) ((Ra * alphaBlend) + (Rb * alphaPrime));
                    Ga = (int) ((Ga * alphaBlend) + (Gb * alphaPrime));
                    Ba = (int) ((Ba * alphaBlend) + (Bb * alphaPrime));
                }

                if (paintBitmap.get(offset + j) == true) {
                    pixValue = 0xff000000 |
                                   (((int) ((Ra * opacityPrime) + red) << 16) |
                                        (((int) ((Ga * opacityPrime) + green) << 8) |
                                             ((int) ((Ba * opacityPrime) + blue))));
                    paintBuffer[j] = pixValue;
                    pixBuffer[j] = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                } // end of if (paintBitmap.get(offset+j) == true)
                else {
                    pixValue = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                    paintBuffer[j] = pixBuffer[j] = pixValue;
                }
            }
        }

        importImage(paintBuffer);

        return true;
    }

    /**
     * Shows the image.
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   _LUTa      LUTa - to change to new LUT for imageA else null
     * @param   _LUTb      LUTb - to change to new LUT for imageB else null
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean buildImageObject(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow) {

        float imageMinB = 0, imageMaxB = 0;
        int xDim, yDim;
        int bufferSize;
        int lutHeightA = 0;
        int index;
        int indexA, indexB;
        float[][] RGB_LUTa = null, RGB_LUTb = null;
        int[][] iRGB_LUTa = null, iRGB_LUTb = null;
        int Ra, Ga, Ba, Rb, Gb, Bb;
        int pix;
        float opacityPrime;
        int j;

        int timeSliceA, timeSliceB;

        if (imageA.isColorImage()) {

            // call the show method for displaying RGB images
            return (buildImageObject(tSlice, zSlice, forceShow));
        }

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

        xDim = imageA.getExtents()[0];
        yDim = imageA.getExtents()[1];
        zDim = imageA.getExtents()[2];

        if (lutHeightA != lutBufferRemapped.length) {

            try {
                lutBufferRemapped = new int[lutHeightA];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ViewJComponentEditAnimate.buildImageObject");

                return false;
            }
        }

        if (imageB == null) {
            LUTa.exportIndexedLUT(lutBufferRemapped);
        }

        bufferSize = xDim * yDim;

        if (imageB != null) {
            RGB_LUTa = LUTa.exportRGB_LUT(true);
            RGB_LUTb = LUTb.exportRGB_LUT(true);
            iRGB_LUTa = new int[3][RGB_LUTa[0].length];
            iRGB_LUTb = new int[3][RGB_LUTb[0].length];

            for (int c = 0; c < RGB_LUTa[0].length; c++) {
                iRGB_LUTa[0][c] = (int) ((RGB_LUTa[0][c] * alphaBlend) + 0.5f);
                iRGB_LUTb[0][c] = (int) ((RGB_LUTb[0][c] * alphaPrime) + 0.5f);
                iRGB_LUTa[1][c] = (int) ((RGB_LUTa[1][c] * alphaBlend) + 0.5f);
                iRGB_LUTb[1][c] = (int) ((RGB_LUTb[1][c] * alphaPrime) + 0.5f);
                iRGB_LUTa[2][c] = (int) ((RGB_LUTa[2][c] * alphaBlend) + 0.5f);
                iRGB_LUTb[2][c] = (int) ((RGB_LUTb[2][c] * alphaPrime) + 0.5f);
            }
        } // if (imageB != null)

        if (frame.getControls() != null) {
            OPACITY = frame.getControls().getTools().getOpacity();
        }

        opacityPrime = 1 - OPACITY;
        red = borderCol.getRed();
        green = borderCol.getGreen();
        blue = borderCol.getBlue();

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
                zDimSlices = imageA.getExtents()[2];
            }

            try {

                // if (imageA.getType() == ModelStorageBase.DCOMPLEX)
                // imageA.ExportDComplexSliceXY(timeSliceA*zDimSlices + slice,imageBufferA, logMagDisplay);
                if (imageA.getType() == ModelStorageBase.COMPLEX) {
                    imageA.exportComplexSliceXY((timeSliceA * zDimSlices) + slice, imageBufferA, logMagDisplay);
                } else {
                    imageA.exportSliceXY((timeSliceA * zDimSlices) + slice, imageBufferA);
                }

                if (imageB != null) {

                    // if (imageB.getType() == ModelStorageBase.DCOMPLEX)
                    // imageB.exportDComplexSliceXY(timeSliceB*zDimSlices + slice,imageBufferB, logMagDisplay);
                    if (imageB.getType() == ModelStorageBase.COMPLEX) {
                        imageB.exportComplexSliceXY((timeSliceB * zDimSlices) + slice, imageBufferB, logMagDisplay);
                    } else {
                        imageB.exportSliceXY((timeSliceB * zDimSlices) + slice, imageBufferB);
                    }
                }
            } catch (IOException error) {
                MipavUtil.displayError("" + error); // Need to fix this

                return false;
            }
        }

        if (imageB == null) {
            int offset = zSlice * bufferSize;
            int value;
            pix = 0;

            TransferFunction tf_imgA = LUTa.getTransferFunction();

            for (index = 0; index < bufferSize; index++) {
                pix = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);

                if (paintBitmap.get(offset + index) == true) {
                    value = lutBufferRemapped[pix];
                    Ra = (value & 0x00ff0000) >> 16;
                    Ga = (value & 0x0000ff00) >> 8;
                    Ba = (value & 0x000000ff);
                    value = 0xff000000 |
                                (((int) ((Ra * opacityPrime) + red) << 16) |
                                     (((int) ((Ga * opacityPrime) + green) << 8) |
                                          ((int) ((Ba * opacityPrime) + blue))));
                    paintBuffer[index] = value;
                    pixBuffer[index] = lutBufferRemapped[pix];
                } else {
                    paintBuffer[index] = pixBuffer[index] = lutBufferRemapped[pix];
                }
            }
        } else {
            int offset = zSlice * bufferSize;
            indexA = indexB = 0;

            TransferFunction tf_imgA = LUTa.getTransferFunction();
            TransferFunction tf_imgB = LUTb.getTransferFunction();

            for (index = 0; index < bufferSize; index++) {
                indexA = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5f);
                indexB = (int) (tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5f);

                Ra = iRGB_LUTa[0][indexA];
                Rb = iRGB_LUTb[0][indexB];
                Ga = iRGB_LUTa[1][indexA];
                Gb = iRGB_LUTb[1][indexB];
                Ba = iRGB_LUTa[2][indexA];
                Bb = iRGB_LUTb[2][indexB];

                Ra = (Ra + Rb);
                Ga = (Ga + Gb);
                Ba = (Ba + Bb);

                if (paintBitmap.get(offset + index) == true) {
                    pix = 0xff000000 |
                              (((int) ((Ra * opacityPrime) + red) << 16) |
                                   (((int) ((Ga * opacityPrime) + green) << 8) | ((int) ((Ba * opacityPrime) + blue))));
                    paintBuffer[index] = pix;
                    pixBuffer[index] = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                } else {
                    pix = 0xff000000 | (Ra << 16) | (Ga << 8) | Ba;
                    paintBuffer[index] = pixBuffer[index] = pix;
                }
            }
        }

        importImage(paintBuffer);

        return true;
    }

    /**
     * In 4D whether to show numbers for each z slice.
     *
     * @param  showNumbers  if true display numbers for each z slice in 4D
     */
    public void displayNumbers(boolean showNumbers) {
        this.showNumbers = showNumbers;
    }


    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  gcFlag  if true garbage collector should be called.
     */
    public void dispose(boolean gcFlag) {

        lutBufferRemapped = null;
        imageBufferA = null;
        imageBufferB = null;
        pixBuffer = null;
        paintBuffer = null;
        paintBitmap = null;
        imageActive = null;
        imageBufferActive = null;
        frame = null;

        if (disposeImage) {

            if (imageA != null) {
                imageA.disposeLocal();
            }

            if (imageB != null) {
                imageB.disposeLocal();
            }
        }

        mt = null;

        if (img != null) {

            for (int i = 0; i < img.length; i++) {

                if (img[i] != null) {
                    img[i].flush();
                    img[i] = null;
                }
            }

            img = null;
        }

        LUTa = null;
        LUTb = null;

        if (gcFlag == true) {
            System.gc();
            System.runFinalization();
            System.gc();
        }
    }


    /**
     * Clean up some resources!
     */
    public void finalizeLocal() {

        if (img != null) {

            for (int i = 0; i < img.length; i++) {

                if (img[i] != null) {
                    img[i].flush();
                    img[i] = null;
                }
            }

            img = null;
        }

        string = null;
    }


    /**
     * Accessor that returns the active image.
     *
     * @return  active image
     */
    public ModelImage getActiveImage() {
        return imageActive;
    }

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
     * Gets Java image.
     *
     * @return  Java image
     */
    public Image getImage() {
        return img[slice];
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
     * Gets the interpolation mode.
     *
     * @return  returns the interpolation mode
     */
    public int getInterpMode() {
        return interpMode;
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
    public BitSet getpaintBitmap() {
        return paintBitmap;
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
     * Method to ensure img[slice] is not displayed.
     */
    public void ignoreSlice() {
        ignoreSlice[slice] = true;
        setLabelZ();
    }

    /**
     * Creates a Image object form an array of ints that have been formatted (packed) properly (i.e. aRGB)
     *
     * @param  data  Data (image) to be displayed that has been formatted (packed) properly (i.e. aRGB)
     */
    public void importImage(int[] data) {
        // If the MemoryImageSource and createImage steps are separated, then animate displays only the last image.
        // createImage must be executed right after MemoryImageSource.

        if (data != null) {
            mt = null;
            mt = new MediaTracker(this);
            memImage = null;

            try {
                memImage = new MemoryImageSource(imageDim.width, imageDim.height, data, 0, imageDim.width);
                memImage.setAnimated(false);

                if (!haveFiltered) {
                    img[slice] = createImage(memImage);
                } else {
                    img[slice] = createImage(new FilteredImageSource(memImage,
                                                                     new ViewJFilterAnimate(brightness, contrast)));
                }

            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentBase.importImage.");
            }


            try {
                mt.addImage(img[slice], slice);
                mt.waitForID(slice);
            } catch (InterruptedException e) {
                System.gc();
                MipavUtil.displayError("Interrutped Exception: ComponentBase.importImage.");
            }

        }
    }

    /**
     * Paints the image and border.
     *
     * @param  g  Graphics handle
     */
    /* Since paintComponent is used rather than paintAnimate or some other name, then in addition to each
     * direct call to paintComponent in updateImages, 1 or 2 calls to paintComponent maybe made by Jcomponent.paint. */
    public void paintComponent(Graphics g) {
        int i;

        try {

            if (g == null) {
                return;
            }

            if (img != null) {
                g.setClip(getVisibleRect());

                if (((zoomX * resX) != 1.0f) || ((zoomY * resY) != 1.0f)) {
                    g.drawImage(img[slice], 0, 0, (int) ((zoomX * imageDim.width * resX) + 0.5),
                                (int) ((zoomY * imageDim.height * resY) + 0.5), 0, 0, imageDim.width, imageDim.height,
                                this);
                } else {
                    g.drawImage(img[slice], 0, 0, this);
                }

                if (showNumbers) {
                    g.setFont(MipavUtil.font12);
                    g.setColor(Color.white);

                    for (i = 0; i < originalZDim; i++) {
                        g.drawString(zString[i], (int) ((zoomX * resX * xLabel[i]) + 0.5),
                                     (int) ((zoomY * resY * yLabel[i]) + 0.5));
                    }
                }

                if (showSliceNumber) {
                    g.setFont(MipavUtil.font12);
                    g.setColor(Color.white);
                    g.drawString(zString[slice], 5, (int) ((zoomY * resY * imageDim.height) + 0.5) - 5);
                }

                for (i = nVOI - 1; i >= 0; i--) {
                    VOIs.VOIAt(i).drawSelf(zoomX, zoomY, resX, resY, 0, 0, imageActive.getFileInfo(0).getResolutions(),
                                           imageActive.getFileInfo(0).getUnitsOfMeasure(), slice, FileInfoBase.UNKNOWN_ORIENT, g);
                }
            } // if (img != null)
        } // try
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentAnimate.paint.");
        }
    }

    /**
     * Specifications that are only relevant to 4D images.
     *
     * @param  originalZDim  number of z slices
     * @param  nColumn       the number of columns of z slices
     * @param  nRow          the number of rows of z slices
     */
    public void set4DSpecs(int originalZDim, int nColumn, int nRow) {
        this.originalZDim = originalZDim;
        this.nColumn = nColumn;
        this.nRow = nRow;
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed
     */
    public void setAlphaBlend(int value) {
        alphaBlend = value / 100.0f;
        alphaPrime = 1 - alphaBlend;
    }

    /**
     * Sets color of the border surrounding each z slice in 4D images.
     *
     * @param  borderCol  border color surounding each z slice
     */
    public void setBorderCol(Color borderCol) {
        this.borderCol = borderCol;
    }

    /**
     * Method to set the brightness and contrast of the animate images.
     *
     * @param  brightness  int going from -255 to 255
     * @param  contrast    float scale factor
     */
    public void setBrightness(int brightness, float contrast) {
        int i;
        this.brightness = brightness;
        this.contrast = contrast;
        haveFiltered = true;
        curSlice = slice;

        for (i = 0; ((i < zDim) && (!ignoreSlice[i])); i++) {
            buildImageObject(0, i, null, null, true);
        }

        slice = curSlice;
        paintComponent(getGraphics());
    }


    /**
     * The frame in which the image(s) is displayed, allocates the memory and uses this method to pass the references to
     * the buffers.
     *
     * @param  imgBufferA  storage buffer used to display image A
     * @param  imgBufferB  storage buffer used to display image B
     * @param  pixBuff     storage buffer used to build a displayable image
     * @param  paintBuff   storage buffer used to display the combined paintBitmap and pixBuffer buffers
     */
    public void setBuffers(float[] imgBufferA, float[] imgBufferB, int[] pixBuff, int[] paintBuff) {

        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
        pixBuffer = pixBuff;
        paintBuffer = paintBuff;
        imageBufferActive = imageBufferA;
    }


    /**
     * Sets the interpolation mode.
     *
     * @param  mode  mode to set it to
     */
    public void setInterpMode(int mode) {
        interpMode = mode;
    }

    /**
     * For 4D sets the numbering string of each z slice and its x and y positions.
     */
    public void setLabelXY() {
        int originalXDim, originalYDim;
        int colNumber, rowNumber;
        int i;
        originalXDim = (xDim - (9 * nColumn) + 3) / nColumn;
        originalYDim = (yDim - (9 * nRow) + 3) / nRow;
        xLabel = new int[originalZDim];
        yLabel = new int[originalZDim];
        zString = new String[originalZDim];

        for (i = 0; i < originalZDim; i++) {
            colNumber = i % nColumn;
            rowNumber = i / nColumn;
            xLabel[i] = 5 + (colNumber * (originalXDim + 9));
            yLabel[i] = originalYDim - 2 + (rowNumber * (originalYDim + 9));
            zString[i] = String.valueOf(i + 1);
        } // end of for (i = 0; i < originalZDim; i++)
    }

    /**
     * setlabelZ - For 3D sets the numbering string of each frame.
     */
    public void setLabelZ() {
        int i;
        int k = 1;
        zString = new String[zDim];

        for (i = 0; (i < zDim); i++) {

            // Only produce strings for nondeleted slices
            if (!ignoreSlice[i]) {
                zString[i] = String.valueOf(k);
                k++;
            }
        }
    }

    /**
     * Sets the paintBitmap to produce colored borders for 4D images.
     */
    // Note that separating 2 z slices are 3 black pixels, followed by 3 colored pixels,
    // followed by 3 colored pixels.  */
    public void setPaintMask() {
        int originalXDim, originalYDim;
        int colNumber, rowNumber;
        int i, j, k, l, offset;

        /* The originalXDim and originalYDim are the first and second dimensions in the
         *image before the 4D to 3D conversion in JDialogAnimate */
        originalXDim = (xDim - (9 * nColumn) + 3) / nColumn;
        originalYDim = (yDim - (9 * nRow) + 3) / nRow;

        for (i = 0; i < originalZDim; i++) {
            colNumber = i % nColumn;
            rowNumber = i / nColumn;

            for (j = 0; j < zDim; j++) {
                offset = j * xDim * yDim;

                for (k = 0; k < 3; k++) {

                    for (l = rowNumber * (originalYDim + 9); l < ((rowNumber * (originalYDim + 9)) + originalYDim + 6);
                             l++) {
                        paintBitmap.set(offset + k + (colNumber * (originalXDim + 9)) + (l * xDim));
                        paintBitmap.set(offset + k + originalXDim + 3 + (colNumber * (originalXDim + 9)) + (l * xDim));
                    }

                    for (l = colNumber * (originalXDim + 9); l < ((colNumber * (originalXDim + 9)) + originalXDim + 6);
                             l++) {
                        paintBitmap.set(offset + ((k + (rowNumber * (originalYDim + 9))) * xDim) + l);
                        paintBitmap.set(offset + ((k + originalYDim + 3 + (rowNumber * (originalYDim + 9))) * xDim) +
                                        l);
                    }
                } // end of for (k = 0; k < 3; k++)
            } // end of (j = 0; j < zDim; j++)
        } // end of for (i = 0; i < originalZDim; i++)
    }

    /**
     * Accessor that sets the paint mask.
     *
     * @param  mask  DOCUMENT ME!
     */
    public void setPaintMask(BitSet mask) {
        paintBitmap = mask;
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
     * In 3D whether to show number for complete frame.
     *
     * @param  flag  if true show number for complete frame
     */
    public void setShowSliceNumber(boolean flag) {
        showSliceNumber = flag;
    }

    /**
     * Accessor that sets the slice of the image.
     *
     * @param  _slice  image slice to be displayed
     */
    public void setSlice(int _slice) {
        slice = _slice;
    }

    /**
     * Method to set the brightness and contrast of the animate slice.
     *
     * @param  brightness  int going from -255 to 255
     * @param  contrast    float scale factor
     */
    public void setSliceBrightness(int brightness, float contrast) {
        this.brightness = brightness;
        this.contrast = contrast;
        haveFiltered = true;
        buildImageObject(0, slice, null, null, true);
        paintComponent(getGraphics());
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
