package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Algorithm to place an image into the center of a larger image, as if the image was given margins or a border around
 * its outside.
 */
public class AlgorithmAddMargins extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Blue value to use in the area not filled in by source image. */
    private double blueValue;

    /** Color factor 1 for noncolor, 4 for color (needed for buffer). */
    private int colorFactor;

    /** Number to add to side & front(pixels), front & back (slices). */
    private int frontMargin = 0, // in slices
                backMargin = 0; // in slices

    /** Green value to use in the area not filled in by source image. */
    private double greenValue;

    /** Image position coordinates, start locations used in DICOM. */
    private float[] imgOriginLPS;

    /** Value to use in the area not filled in by source image. */
    private double marginValue;

    /** Red value to use in the area not filled in by source image. */
    private double redValue;

    /** Number to add to side & top(pixels), left & right (pixels). */
    private int topMargin, // in pixels
                bottomMargin, // passed implicitly if destImage != null
                leftMargin, // in pixels
                rightMargin; // in pixels

    /**
     * Using buffers creates a buffer in the algo to do image copies, then transfers the lot back to the model image
     * rather than copying into the model image.
     */
    private boolean useBuffers = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Import source and destination images into the class (shorthand for a 2d image). Used for noncolor.
     *
     * @param  srcImage   Source image (image to clip from)
     * @param  destImage  Destination image (image to paste to)
     * @param  n          Value to use in portions of the destination not clipped from the source
     * @param  width      Border or margin to be added on each side
     * @param  height     Border or margin to be added on both top and bottom
     */
    public AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, double n, int width, int height) {
        super(destImage, srcImage);

        if (srcImage.getNDims() != 2) {
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        if (srcImage.isColorImage()) {
            colorFactor = 4;
            redValue = n;
            greenValue = n;
            blueValue = n;
        } else {
            colorFactor = 1; // black and white
            marginValue = n;
        }


        // additions
        rightMargin = width;
        leftMargin = width;
        topMargin = height;
    }

    /**
     * Import source image into the class (shorthand for a 2d image). Used for noncolor.
     *
     * @param  srcImage  Source image (image to clip from)
     * @param  n         Value to use in portions of the destination not clipped from the source
     * @param  width     Border or margin to be added on each side
     * @param  top       Border or margin to be added on top
     * @param  bottom    Border or margin to be added on bottom
     */
    public AlgorithmAddMargins(ModelImage srcImage, double n, int width, int top, int bottom) {
        super(null, srcImage);

        if (srcImage.getNDims() != 2) {
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        if (srcImage.isColorImage()) {
            colorFactor = 4;
            redValue = n;
            greenValue = n;
            blueValue = n;
        } else {
            colorFactor = 1; // black and white
            marginValue = n;
        }

        // additions
        rightMargin = width;
        leftMargin = width;
        topMargin = top;
        bottomMargin = bottom;
    }


    /**
     * Import source and destination images into the class (shorthand for a 2d image). Used for noncolor.
     *
     * @param  srcImage    Source image (image to clip from)
     * @param  destImage   Destination image (image to paste to)
     * @param  n           Value to use in portions of the destination not clipped from the source
     * @param  leftWidth   Left border or margin to be added on each side
     * @param  rightWidth  Right border or margin to be added on each side
     * @param  height      Border or margin to be added on both top and bottom
     */
    public AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, double n, int leftWidth, int rightWidth,
                               int height) {
        super(destImage, srcImage);

        if (srcImage.getNDims() != 2) {
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        if (srcImage.isColorImage()) {
            colorFactor = 4;
            redValue = n;
            greenValue = n;
            blueValue = n;
        } else {
            colorFactor = 1; // black and white
            marginValue = n;
        }

        // additions
        leftMargin = leftWidth;
        rightMargin = rightWidth;
        topMargin = height;
    }

    /**
     * Import source image into the class (shorthand for a 2d image). Used for noncolor.
     *
     * @param  srcImage    Source image (image to clip from)
     * @param  n           Value to use in portions of the destination not clipped from the source
     * @param  leftWidth   Left border or margin to be added on each side
     * @param  rightWidth  Right border or margin to be added on each side
     * @param  top         Border or margin to be added on top
     * @param  bottom      Border or margin to be added on bottom
     */
    public AlgorithmAddMargins(ModelImage srcImage, double n, int leftWidth, int rightWidth, int top, int bottom) {
        super(null, srcImage);

        if (srcImage.getNDims() != 2) {
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        if (srcImage.isColorImage()) {
            colorFactor = 4;
            redValue = n;
            greenValue = n;
            blueValue = n;
        } else {
            colorFactor = 1; // black and white
            marginValue = n;
        }

        // additions
        leftMargin = leftWidth;
        rightMargin = rightWidth;
        topMargin = top;
        bottomMargin = bottom;
    }


    /**
     * Import source and destination images into the class (shorthand for a 2d image). Used for color.
     *
     * @param  srcImage   Source image (image to clip from)
     * @param  destImage  Destination image (image to paste to)
     * @param  r          Red value to use in portions of the destination not clipped from the source
     * @param  g          Green value to use in portions of the destination not cipped from the source
     * @param  b          Blue value to use in poritons of the destination not clipped from the source
     * @param  width      Border or margin to be added on each side
     * @param  height     Border or margin to be added on both top and bottom
     */
    public AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, double r, double g, double b, int width,
                               int height) {
        super(destImage, srcImage);

        if (srcImage.getNDims() != 2) {
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        colorFactor = 4; // color

        // additions
        rightMargin = width;
        leftMargin = width;
        topMargin = height;

        redValue = r;
        greenValue = g;
        blueValue = b;
    }

    /**
     * Import source image into the class (shorthand for a 2d image). Used for color.
     *
     * @param  srcImage  Source image (image to clip from)
     * @param  r         Red value to use in portions of the destination not clipped from the source
     * @param  g         Green value to use in portions of the destination not cipped from the source
     * @param  b         Blue value to use in poritons of the destination not clipped from the source
     * @param  width     Border or margin to be added on each side
     * @param  top       Border or margin to be added on top
     * @param  bottom    Border or margin to be added on bottom
     */
    public AlgorithmAddMargins(ModelImage srcImage, double r, double g, double b, int width, int top, int bottom) {
        super(null, srcImage);

        if (srcImage.getNDims() != 2) {
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        colorFactor = 4; // color

        // additions
        rightMargin = width;
        leftMargin = width;
        topMargin = top;
        bottomMargin = bottom;

        redValue = r;
        greenValue = g;
        blueValue = b;
    }


    /**
     * Import source and destination images into the class. Used for noncolor, 3D.
     *
     * @param  srcImage   Source image (image to clip from)
     * @param  destImage  Destination image (image to paste to)
     * @param  n          Value to use in portions of the destination not clipped from the source
     * @param  width      Border or margin to be added on each side
     * @param  height     Border or margin to be added on both top and bottom
     * @param  front      Border or margin to be added to front of image
     * @param  back       Border or margin to be added to back of image
     */
    public AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, double n, int width, int height, int front,
                               int back) {
        super(destImage, srcImage);

        if (srcImage.isColorImage()) {
            colorFactor = 4;
            redValue = n;
            greenValue = n;
            blueValue = n;
        } else {
            colorFactor = 1; // black and white
            marginValue = n;
        }

        rightMargin = width;
        leftMargin = width;
        topMargin = height;
        this.frontMargin = front;
        this.backMargin = back;
    }

    /**
     * Import source image into the class. Used for noncolor, 3D.
     *
     * @param  srcImage  Source image (image to clip from)
     * @param  n         Value to use in portions of the destination not clipped from the source
     * @param  width     Border or margin to be added on each side
     * @param  top       Border or margin to be added on top
     * @param  bottom    Border or margin to be added on bottom
     * @param  front     Border or margin to be added to front of image
     * @param  back      Border or margin to be added to back of image
     */
    public AlgorithmAddMargins(ModelImage srcImage, double n, int width, int top, int bottom, int front, int back) {
        super(null, srcImage);

        if (srcImage.isColorImage()) {
            colorFactor = 4;
            redValue = n;
            greenValue = n;
            blueValue = n;
        } else {
            colorFactor = 1; // black and white
            marginValue = n;
        }

        rightMargin = width;
        leftMargin = width;
        topMargin = top;
        bottomMargin = bottom;
        this.frontMargin = front;
        this.backMargin = back;
    }


    /**
     * Import source and destination images into the class (shorthand for a 2d image). Used for color.
     *
     * @param  srcImage    Source image (image to clip from)
     * @param  destImage   Destination image (image to paste to)
     * @param  r           Red value to use in portions of the destination not clipped from the source
     * @param  g           Green value to use in portions of the destination not cipped from the source
     * @param  b           Blue value to use in poritons of the destination not clipped from the source
     * @param  leftWidth   Left border or margin to be added on each side
     * @param  rightWidth  Right border or margin to be added on each side
     * @param  height      Border or margin to be added on both top and bottom
     */
    public AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, double r, double g, double b, int leftWidth,
                               int rightWidth, int height) {
        super(destImage, srcImage);

        if (srcImage.getNDims() != 2) {
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        colorFactor = 4; // color

        // additions
        leftMargin = leftWidth;
        rightMargin = rightWidth;
        topMargin = height;

        redValue = r;
        greenValue = g;
        blueValue = b;
    }

    /**
     * Import source image into the class (shorthand for a 2d image). Used for color.
     *
     * @param  srcImage    Source image (image to clip from)
     * @param  r           Red value to use in portions of the destination not clipped from the source
     * @param  g           Green value to use in portions of the destination not cipped from the source
     * @param  b           Blue value to use in poritons of the destination not clipped from the source
     * @param  leftWidth   Left border or margin to be added on each side
     * @param  rightWidth  Right border or margin to be added on each side
     * @param  top         Border or margin to be added on top
     * @param  bottom      Border or margin to be added on bottom
     */
    public AlgorithmAddMargins(ModelImage srcImage, double r, double g, double b, int leftWidth, int rightWidth,
                               int top, int bottom) {
        super(null, srcImage);

        if (srcImage.getNDims() != 2) {
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        colorFactor = 4; // color

        // additions
        leftMargin = leftWidth;
        rightMargin = rightWidth;
        topMargin = top;
        bottomMargin = bottom;

        redValue = r;
        greenValue = g;
        blueValue = b;
    }


    /**
     * Import source and destination images into the class. Used for noncolor, 3D.
     *
     * @param  srcImage    Source image (image to clip from)
     * @param  destImage   Destination image (image to paste to)
     * @param  n           Value to use in portions of the destination not clipped from the source
     * @param  leftWidth   left border or margin to be added on each side
     * @param  rightWidth  right border or margin to be added on each side
     * @param  height      Border or margin to be added on both top and bottom
     * @param  front       Border or margin to be added to front of image
     * @param  back        Border or margin to be added to back of image
     */
    public AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, double n, int leftWidth, int rightWidth,
                               int height, int front, int back) {
        super(destImage, srcImage);

        if (srcImage.isColorImage()) {
            colorFactor = 4;
            redValue = n;
            greenValue = n;
            blueValue = n;
        } else {
            colorFactor = 1; // black and white
            marginValue = n;
        }

        leftMargin = leftWidth;
        rightMargin = rightWidth;

        topMargin = height;
        this.frontMargin = front;
        this.backMargin = back;
    }

    /**
     * Import source image into the class. Used for noncolor, 3D.
     *
     * @param  srcImage    Source image (image to clip from)
     * @param  n           Value to use in portions of the destination not clipped from the source
     * @param  leftWidth   left border or margin to be added on each side
     * @param  rightWidth  right border or margin to be added on each side
     * @param  top         Border or margin to be added on top
     * @param  bottom      Border of margin to be added to bottom
     * @param  front       Border or margin to be added to front of image
     * @param  back        Border or margin to be added to back of image
     */
    public AlgorithmAddMargins(ModelImage srcImage, double n, int leftWidth, int rightWidth, int top, int bottom,
                               int front, int back) {
        super(null, srcImage);

        if (srcImage.isColorImage()) {
            colorFactor = 4;
            redValue = n;
            greenValue = n;
            blueValue = n;
        } else {
            colorFactor = 1; // black and white
            marginValue = n;
        }

        leftMargin = leftWidth;
        rightMargin = rightWidth;

        topMargin = top;
        bottomMargin = bottom;
        this.frontMargin = front;
        this.backMargin = back;
    }


    /**
     * Import source and destination images into the class. Used for color, 3D.
     *
     * @param  srcImage   Source image (image to clip from)
     * @param  destImage  Destination image (image to paste to)
     * @param  r          Red value to use in portions of the destination not clipped from the source
     * @param  g          Green value to use in portions of the destination not clipped from the source
     * @param  b          Blue value to use in portions of the destination not clipped from the source
     * @param  width      Border or margin to be added on each side
     * @param  height     Border or margin to be added on both top and bottom
     * @param  front      Border or margin to be added to front of image
     * @param  back       Border or margin to be added to back of image
     */
    public AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, double r, double g, double b, int width,
                               int height, int front, int back) {
        super(destImage, srcImage);
        colorFactor = 4;

        rightMargin = width;
        leftMargin = width;
        topMargin = height;
        this.frontMargin = front;
        this.backMargin = back;

        redValue = r;
        greenValue = g;
        blueValue = b;
    }

    /**
     * Import source image into the class. Used for color, 3D.
     *
     * @param  srcImage  Source image (image to clip from)
     * @param  r         Red value to use in portions of the destination not clipped from the source
     * @param  g         Green value to use in portions of the destination not clipped from the source
     * @param  b         Blue value to use in portions of the destination not clipped from the source
     * @param  width     Border or margin to be added on each side
     * @param  top       Border or margin to be added on top
     * @param  bottom    Border or margin to be added on bottom
     * @param  front     Border or margin to be added to front of image
     * @param  back      Border or margin to be added to back of image
     */
    public AlgorithmAddMargins(ModelImage srcImage, double r, double g, double b, int width, int top, int bottom,
                               int front, int back) {
        super(null, srcImage);
        colorFactor = 4;

        rightMargin = width;
        leftMargin = width;
        topMargin = top;
        bottomMargin = bottom;
        this.frontMargin = front;
        this.backMargin = back;

        redValue = r;
        greenValue = g;
        blueValue = b;
    }


    /**
     * Import source and destination images into the class. Used for color, 3D.
     *
     * @param  srcImage    Source image (image to clip from)
     * @param  destImage   Destination image (image to paste to)
     * @param  r           Red value to use in portions of the destination not clipped from the source
     * @param  g           Green value to use in portions of the destination not clipped from the source
     * @param  b           Blue value to use in portions of the destination not clipped from the source
     * @param  leftWidth   Left border or margin to be added on each side
     * @param  rightWidth  Right border or margin to be added on each side
     * @param  height      Border or margin to be added on both top and bottom
     * @param  front       Border or margin to be added to front of image
     * @param  back        Border or margin to be added to back of image
     */
    public AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, double r, double g, double b, int leftWidth,
                               int rightWidth, int height, int front, int back) {
        super(destImage, srcImage);
        colorFactor = 4;

        leftMargin = leftWidth;
        rightMargin = rightWidth;
        topMargin = height;
        this.frontMargin = front;
        this.backMargin = back;

        redValue = r;
        greenValue = g;
        blueValue = b;
    }

    /**
     * Import source image into the class. Used for color, 3D.
     *
     * @param  srcImage    Source image (image to clip from)
     * @param  r           Red value to use in portions of the destination not clipped from the source
     * @param  g           Green value to use in portions of the destination not clipped from the source
     * @param  b           Blue value to use in portions of the destination not clipped from the source
     * @param  leftWidth   Left border or margin to be added on each side
     * @param  rightWidth  Right border or margin to be added on each side
     * @param  top         Border or margin to be added to the top of the image
     * @param  bottom      Border or margin to be added to the bottom of the image
     * @param  front       Border or margin to be added to front of image
     * @param  back        Border or margin to be added to back of image
     */
    public AlgorithmAddMargins(ModelImage srcImage, double r, double g, double b, int leftWidth, int rightWidth,
                               int top, int bottom, int front, int back) {
        super(null, srcImage);
        colorFactor = 4;

        leftMargin = leftWidth;
        rightMargin = rightWidth;
        topMargin = top;
        bottomMargin = bottom;
        this.frontMargin = front;
        this.backMargin = back;

        redValue = r;
        greenValue = g;
        blueValue = b;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        imgOriginLPS = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Accessor returns srcImage.
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getSrcImage() {
        return srcImage;
    }


    /**
     * Alert the algorithm whther or not to perform the algorithm by copying the image data using a locally-held buffer,
     * or by import the image data directly to the destination model image.
     *
     * @param  withBuffer  -- true indicates using the local buffer, false imports into model image
     */
    public void performCopiesWithBuffers(boolean withBuffer) {
        useBuffers = withBuffer;
    }

    /**
     * Runs the add image margins algorithm.
     */
    public void runAlgorithm() {
        

        if (destImage != null) {
            calcStoreInDest();
        } else {
            calcStoreInPlace();
        }
    }

    /**
     * Does the translation and copy of the source image in a local buffer. This method could be inline code, but is run
     * as a method to simplify reading the algorithm. Likewise, the arguments are generated from class-defined
     * variables, which could be easily be made local variables, but this makes reading this method a little easier.
     *
     * @param  srcWidth    width of the source image (or that slice of the image)
     * @param  srcHeight   height of the source image (or that slice of the image)
     * @param  destWidth   width of the destination image (or that slice of the image)
     * @param  destHeight  height of the destination image (or that slice of the image)
     */
    protected void useLocalBuffer(int srcWidth, int srcHeight, int destWidth, int destHeight) {
        int i, z;
        int srcSliceArea = srcWidth * srcHeight;
        int srcDepth, destDepth;
        int destSliceArea = destWidth * destHeight;

        float[] sourceSlice;
        float[] destSlice;

        int row, col;
        int topMarginRow = topMargin, bottomMarginRow = srcHeight + topMargin, leftMarginCol = leftMargin,
            rightMarginCol = srcWidth + rightMargin;

        int mod = destSliceArea / 100; // mod is 1 percent of length for the percentage in the progress bar

        try {
            sourceSlice = new float[srcSliceArea];
            destSlice = new float[destSliceArea];
        } catch (OutOfMemoryError error) {
            sourceSlice = null;
            destSlice = null;
            System.gc();
            displayError("Algorithm Add Image Margins: Out of memory");
            setCompleted(false);


            return;
        }

        // chk for 2D
        if (srcImage.getNDims() == 2) {

            // skip through the added 'top' margin, then copy the source image into offset place
            try {
                srcImage.exportData(0, srcSliceArea, sourceSlice);
            } catch (IOException ioe) {
                sourceSlice = null;
                destSlice = null;
                MipavUtil.displayError("AlgorithmAddMargins reports:\n" + ioe.toString());

                setCompleted(false);

                return;
            }

            for (i = 0; (i < destSliceArea) && !threadStopped; i++) {

                if (((i % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) i / (destSliceArea) * 100));
                }

                row = i / destWidth;
                col = i % destWidth;

                if (((row <= topMarginRow) || (row >= bottomMarginRow)) ||
                        ((col <= leftMarginCol) || (col >= rightMarginCol))) { // if out of bounds on the
                                                                               // top-or-bottom, or on the left-or-right
                                                                               // side
                    destSlice[i] = (float) marginValue;
                } else {

                    // srcImage pixel location := current destImage pixel - margins on both sides down to this row +
                    // the margin on the right - all the elements in the top margin
                    destSlice[i] = sourceSlice[i + ((-2 * leftMargin * (row + 1)) + rightMargin) -
                                               (topMargin * srcWidth)];
                }
            }

            if (threadStopped) {
                sourceSlice = null;
                destSlice = null;
                finalize();

                return;
            }

            try {
                destImage.importData(0, destSlice, true);
            } catch (IOException ioe) {
                sourceSlice = null;
                destSlice = null;
                MipavUtil.displayError("AlgorithmAddMargins reports:\n" + ioe.toString());

                setCompleted(false);

                return;
            }

        } else { // source is 3D

            try {
                srcDepth = srcImage.getExtents()[2];
                destDepth = destImage.getExtents()[2];

                int imageLength = destDepth * destSliceArea;

                for (z = 0; (z < destDepth) && !threadStopped; z++) { // for all slices in the old image

                    if ((z < frontMargin) || (z >= (srcDepth + frontMargin))) {

                        for (i = 0; i < destSliceArea; i++) { // leading & trailing slices
                            destSlice[i] = (float) marginValue; // are filled with the margin value
                        }
                    } else { // z  in range of source slice, & gets the margins and srcImage image

                        // skip through the added 'top' margin, then copy the source image into offset
                        // placerogressBar.updateValue(Math.round( (float)(z*destSliceArea + i)/(imageLength) * 100));
                        srcImage.exportSliceXY(z - frontMargin, sourceSlice);

                        for (i = 0; (i < destSliceArea) && !threadStopped; i++) {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round((float) ((z * destSliceArea) + i) / (imageLength) *
                                                                        100));
                            }

                            row = i / destWidth;
                            col = i % destWidth;

                            if (((row <= topMarginRow) || (row >= bottomMarginRow)) ||
                                    ((col <= leftMarginCol) || (col >= rightMarginCol))) { // if out of bounds on the
                                                                                           // top-or-bottom, or on the
                                                                                           // left-or-right side
                                destSlice[i] = (float) marginValue;
                            } else {

                                // srcImage pixel location := current destImage pixel - margins on both sides down to
                                // this row + the margin on the right - all the elements in the top margin
                                destSlice[i] = sourceSlice[i + ((-2 * leftMargin * (row + 1)) + rightMargin) -
                                                           (topMargin * srcWidth)];
                                // destSlice[i] = sourceSlice[srcWidth*(row - topMargin) + col - leftMarginCol];
                            }
                        }
                    }

                    if (threadStopped) {
                        sourceSlice = null;
                        destSlice = null;
                        finalize();

                        return;
                    }

                    destImage.importData(z * destSliceArea, destSlice, false);
                }

                if (threadStopped) {
                    sourceSlice = null;
                    destSlice = null;
                    finalize();

                    return;
                }
            } catch (IOException ioe) {
                sourceSlice = null;
                destSlice = null;
                MipavUtil.displayError("AlgorithmAddMargins reports:\n" + ioe.toString());

                setCompleted(false);

                return;
            }
        }

        sourceSlice = null;
        destSlice = null;
    }

    /**
     * Does the translation and copy of the source image in a local buffer. This method could be inline code, but is run
     * as a method to simplify reading the algorithm. Likewise, the arguments are generated from class-defined
     * variables, which could be easily be made local variables, but this makes reading this method a little easier.
     *
     * @param  srcWidth    width of the source image (or that slice of the image)
     * @param  srcHeight   height of the source image (or that slice of the image)
     * @param  destWidth   width of the destination image (or that slice of the image)
     * @param  destHeight  height of the destination image (or that slice of the image)
     */
    protected void useLocalBufferForSource(int srcWidth, int srcHeight, int destWidth, int destHeight) {
        int i, z, t;
        int Z;
        int srcSliceArea = srcWidth * srcHeight;
        int srcDepth = 1;
        int destDepth = 1;
        int destSliceArea = destWidth * destHeight;

        if (srcImage.getNDims() >= 3) {
            srcDepth = srcImage.getExtents()[2];
            destDepth = srcDepth + frontMargin + backMargin;
        }

        int tDim = 1;

        if (srcImage.getNDims() >= 4) {
            tDim = srcImage.getExtents()[3];
        }

        int tNewOffset;
        int[] newExtents;

        float[] sourceSlice;
        float[] destSlice;

        int row, col;
        int topMarginRow = topMargin, bottomMarginRow = srcHeight + topMargin, leftMarginCol = colorFactor * leftMargin,
            rightMarginCol = srcWidth + leftMarginCol;

        // For updating origin
        float[] newOriginLPS = new float[3];
        int[] marginVector = new int[3];
        String stringForDicom;
        FileInfoDicom[] fileInfoDicomBuffer = null; // buffer of type DICOM
        FileInfoBase[] fileInfoBuffer = null; // buffer of any old type
        boolean isDicom = false;
        int dataType;
        String imageName;

        dataType = srcImage.getType();
        imageName = srcImage.getImageName();

        int mod = destSliceArea / 100; // mod is 1 percent of length for the percentage in the progress bar

        try {
            sourceSlice = new float[srcSliceArea];
            destSlice = new float[tDim * destDepth * destSliceArea];
        } catch (OutOfMemoryError error) {
            sourceSlice = null;
            destSlice = null;
            System.gc();
            displayError("Algorithm Add Image Margins: Out of memory");
            setCompleted(false);


            return;
        }

        // chk for 2D
        if (srcImage.getNDims() == 2) {

            // skip through the added 'top' margin, then copy the source image into offset place
            try {
                srcImage.exportData(0, srcSliceArea, sourceSlice);
            } catch (IOException ioe) {
                sourceSlice = null;
                MipavUtil.displayError("AlgorithmAddMargins reports:\n" + ioe.toString());

                setCompleted(false);

                return;
            }

            newExtents = new int[2];
            newExtents[0] = srcImage.getExtents()[0] + leftMargin + rightMargin;
            newExtents[1] = srcImage.getExtents()[1] + topMargin + bottomMargin;

            marginVector[0] = leftMargin;
            marginVector[1] = topMargin;
            marginVector[2] = 0; // since 2D here

            newOriginLPS = calculateNewOrigin(srcImage, marginVector);
            //System.out.println("New LPS origin: " + Float.toString(newOriginLPS[0]) + ", " +
                               //Float.toString(newOriginLPS[1]) + ", " + Float.toString(newOriginLPS[2]));

            // FILE INFO: add the file info    (if the original is a DICOM image, do a special file info...)
            fireProgressStateChanged("Updating File Info...");

            if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                fileInfoDicomBuffer = new FileInfoDicom[1];
                fileInfoDicomBuffer[0] = (FileInfoDicom) srcImage.getFileInfo(0).clone(); // copy into buffer
                fileInfoDicomBuffer[0].setExtents(newExtents);

                stringForDicom = Float.toString(newOriginLPS[0]) + "\\" + Float.toString(newOriginLPS[1]) + "\\" +
                                 Float.toString(newOriginLPS[2]);
                fileInfoDicomBuffer[0].getTagTable().setValue("0020,0032", stringForDicom, stringForDicom.length());
                fileInfoDicomBuffer[0].setOrigin(newOriginLPS);
                isDicom = true;
            } else { // not a DICOM image,
                fileInfoBuffer = new FileInfoBase[1];
                fileInfoBuffer[0] = (FileInfoBase) srcImage.getFileInfo(0).clone();
                fileInfoBuffer[0].setOrigin(newOriginLPS);
                fileInfoBuffer[0].setExtents(newExtents); // SET extents for the destination
                isDicom = false;
            }


            if (srcImage.getParentFrame() != null) {
                srcImage.getParentFrame().close();
            }

            srcImage.disposeLocal();
            srcImage = null;

            try {
                destSlice = new float[tDim * destDepth * destSliceArea];
            } catch (OutOfMemoryError error) {
                sourceSlice = null;
                destSlice = null;
                System.gc();
                displayError("Algorithm Add Image Margins: Out of memory");
                setCompleted(false);


                return;
            }


            for (i = 0; (i < destSliceArea) && !threadStopped; i++) {

                if (((i % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) i / (destSliceArea) * 100));
                }

                row = i / destWidth;
                col = i % destWidth;

                if (((row <= topMarginRow) || (row >= bottomMarginRow)) ||
                        ((col <= leftMarginCol) || (col >= rightMarginCol))) { // if out of bounds on the
                                                                               // top-or-bottom, or on the left-or-right
                                                                               // side

                    if (colorFactor == 1) {
                        destSlice[i] = (float) marginValue;
                    } else if ((i % 4) == 0) {
                        destSlice[i] = 255.0f;
                    } else if ((i % 4) == 1) {
                        destSlice[i] = (float) redValue;
                    } else if ((i % 4) == 2) {
                        destSlice[i] = (float) greenValue;
                    } else {
                        destSlice[i] = (float) blueValue;
                    }
                } else {

                    // srcImage pixel location := current destImage pixel - margins on both sides down to this row +
                    // the margin on the right - all the elements in the top margin
                    destSlice[i] = sourceSlice[i - (leftMargin * colorFactor * (row + 1)) -
                                               (rightMargin * colorFactor * row) - (topMargin * srcWidth)];
                }
            }

            if (threadStopped) {
                sourceSlice = null;
                destSlice = null;
                finalize();

                return;
            }

            sourceSlice = null;

            srcImage = new ModelImage(dataType, newExtents, imageName);


            try {
                srcImage.importData(0, destSlice, true);
            } catch (IOException ioe) {
                sourceSlice = null;
                destSlice = null;
                MipavUtil.displayError("AlgorithmAddMargins reports:\n" + ioe.toString());

                setCompleted(false);

                return;
            }

            if (isDicom) {
                srcImage.setFileInfo(fileInfoDicomBuffer[0], 0);

                // set image rows ("0028,0010")
                stringForDicom = String.valueOf(newExtents[0]);
                fileInfoDicomBuffer[0].getTagTable().setValue("0028,0010", stringForDicom);

                // set image columns ("0028,0011")
                stringForDicom = String.valueOf(newExtents[1]);
                fileInfoDicomBuffer[0].getTagTable().setValue("0028,0011", stringForDicom);

            } else { // not a DICOM image
                srcImage.setFileInfo(fileInfoBuffer[0], 0);
            }

            fireProgressStateChanged(100); // show at 100%

        } else { // source is 3D or 4D

            try {
                int imageLength = tDim * destDepth * destSliceArea;

                for (t = 0; (t < tDim) && !threadStopped; t++) {
                    tNewOffset = t * destDepth * destSliceArea;

                    for (z = 0; (z < destDepth) && !threadStopped; z++) { // for all slices in the old image

                        if ((z < frontMargin) || (z >= (srcDepth + frontMargin))) {

                            for (i = 0; i < destSliceArea; i++) { // leading & trailing slices

                                if (colorFactor == 1) {
                                    destSlice[tNewOffset + (z * destSliceArea) + i] = (float) marginValue; // are filled with the margin value
                                } else if ((i % 4) == 0) {
                                    destSlice[tNewOffset + (z * destSliceArea) + i] = 255.0f;
                                } else if ((i % 4) == 1) {
                                    destSlice[tNewOffset + (z * destSliceArea) + i] = (float) redValue;
                                } else if ((i % 4) == 2) {
                                    destSlice[tNewOffset + (z * destSliceArea) + i] = (float) greenValue;
                                } else {
                                    destSlice[tNewOffset + (z * destSliceArea) + i] = (float) blueValue;
                                }
                            }
                        } else { // z  in range of source slice, & gets the margins and srcImage image

                            // skip through the added 'top' margin, then copy the source image into offset
                            // placerogressBar.updateValue(Math.round( (float)(z*destSliceArea + i)/(imageLength) *
                            // 100));
                            srcImage.exportData(((t * srcDepth) + z - frontMargin) * srcSliceArea, srcSliceArea,
                                                sourceSlice);

                            for (i = 0; (i < destSliceArea) && !threadStopped; i++) {

                                if (((i % mod) == 0)) {
                                    fireProgressStateChanged(Math.round((float) (tNewOffset + (z * destSliceArea) + i) /
                                                                            (imageLength) * 100));
                                }

                                row = i / destWidth;
                                col = i % destWidth;

                                if (((row <= topMarginRow) || (row >= bottomMarginRow)) ||
                                        ((col <= leftMarginCol) || (col >= rightMarginCol))) { // if out of bounds on
                                                                                               // the top-or-bottom, or
                                                                                               // on the left-or-right
                                                                                               // side

                                    if (colorFactor == 1) {
                                        destSlice[tNewOffset + (z * destSliceArea) + i] = (float) marginValue;
                                    } else if ((i % 4) == 0) {
                                        destSlice[tNewOffset + (z * destSliceArea) + i] = 255.0f;
                                    } else if ((i % 4) == 1) {
                                        destSlice[tNewOffset + (z * destSliceArea) + i] = (float) redValue;
                                    } else if ((i % 4) == 2) {
                                        destSlice[tNewOffset + (z * destSliceArea) + i] = (float) greenValue;
                                    } else {
                                        destSlice[tNewOffset + (z * destSliceArea) + i] = (float) blueValue;
                                    }
                                } else {

                                    // srcImage pixel location := current destImage pixel - margins on both sides down
                                    // to this row + the margin on the right - all the elements in the top margin
                                    destSlice[tNewOffset + (z * destSliceArea) + i] = sourceSlice[i -
                                                                                                  (leftMargin *
                                                                                                       colorFactor *
                                                                                                       (row + 1)) -
                                                                                                  (rightMargin *
                                                                                                       colorFactor *
                                                                                                       row) -
                                                                                                  (topMargin * srcWidth)];
                                    // destSlice[i] = sourceSlice[srcWidth*(row - topMargin) + col - leftMarginCol];
                                }
                            }
                        }

                        if (threadStopped) {
                            sourceSlice = null;
                            destSlice = null;
                            finalize();

                            return;
                        }

                    } // for (z = 0; z < destDepth && !threadStopped; z++)
                } // for (t = 0; t < tDim && !threadStopped; t++)

                sourceSlice = null;

                if (srcImage.getNDims() == 3) {
                    newExtents = new int[3];
                } else {
                    newExtents = new int[4];
                    newExtents[3] = srcImage.getExtents()[3];
                }

                newExtents[0] = srcImage.getExtents()[0] + leftMargin + rightMargin;
                newExtents[1] = srcImage.getExtents()[1] + topMargin + bottomMargin;
                newExtents[2] = srcImage.getExtents()[2] + frontMargin + backMargin;

                if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                    fileInfoDicomBuffer = new FileInfoDicom[destDepth * tDim];
                } else {
                    fileInfoBuffer = new FileInfoBase[destDepth * tDim];
                }

                for (t = 0; (t < tDim) && !threadStopped; t++) {


                    marginVector[0] = leftMargin;
                    marginVector[1] = topMargin;
                    marginVector[2] = frontMargin;

                    newOriginLPS = calculateNewOrigin(srcImage, marginVector);
                    //System.out.println("New LPS origin: " + Float.toString(newOriginLPS[0]) + ", " +
                                       //Float.toString(newOriginLPS[1]) + ", " + Float.toString(newOriginLPS[2]));

                    float delta = srcImage.getFileInfo()[0].getResolutions()[2];
                    int axisOrient = srcImage.getFileInfo()[0].getAxisOrientation(2);

                    if ((axisOrient != FileInfoBase.ORI_A2P_TYPE) && (axisOrient != FileInfoBase.ORI_R2L_TYPE) &&
                            (axisOrient != FileInfoBase.ORI_I2S_TYPE)) {
                        delta = -delta;
                    }

                    float[] originImg = originLPS2Img(newOriginLPS, srcImage);
                    float startLoc = originImg[2];
                    //System.out.println("Start location is " + startLoc + ".\n");

                    // FILE INFO: add the file info for 3D images
                    if ((tDim == 1)) {
                        fireProgressStateChanged("Updating File Info...");
                        // int fillLength = Math.round((float)z/destDepth); int piece = (1 - fillLength);
                    }

                    z = 0; // z is the counter for the orig image

                    for (Z = 0; (Z < destDepth) && !threadStopped; Z++) {

                        if ((tDim == 1)) {
                            fireProgressStateChanged(Math.round((float) (Z) / destDepth * 100));
                        }

                        // DICOM
                        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                            fileInfoDicomBuffer[(t * destDepth) + Z] = (FileInfoDicom) srcImage.getFileInfo(z).clone();

                            fileInfoDicomBuffer[(t * destDepth) + Z].setExtents(newExtents); // modify extents to use
                                                                                             // the extents of destImage
                                                                                             // img

                            // change the slice number ("0020,0013"):
                            // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                            // Reset the image (slice) number with the new number ordering
                            stringForDicom = Integer.toString(Z + 1);
                            fileInfoDicomBuffer[(t * destDepth) + Z].getTagTable().setValue("0020,0013", stringForDicom,
                                                                                            stringForDicom.length());

                            if (newOriginLPS != null) {
                                originImg[2] = startLoc + (delta * Z);
                                newOriginLPS = originImg2LPS(originImg, srcImage);

                                stringForDicom = Float.toString(newOriginLPS[0]) + "\\" +
                                                 Float.toString(newOriginLPS[1]) + "\\" +
                                                 Float.toString(newOriginLPS[2]);
                                fileInfoDicomBuffer[(t * destDepth) + Z].getTagTable().setValue("0020,0032",
                                                                                                stringForDicom,
                                                                                                stringForDicom.length());
                                fileInfoDicomBuffer[(t * destDepth) + Z].setOrigin(newOriginLPS);
                            }

                            // readjust the slice location ("0020,1041")
                            if (imgOriginLPS != null) {
                                stringForDicom = String.valueOf(imgOriginLPS[2]);
                                fileInfoDicomBuffer[(t * destDepth) + Z].getTagTable().setValue("0020,1041",
                                                                                                stringForDicom,
                                                                                                stringForDicom.length());
                            }

                            // set image columns ("0028,0011")
                            // stringForDicom = String.valueOf(newExtents[0]);
                            // fileInfoDicomBuffer[t*destDepth + Z].setValue("0028,0011", stringForDicom);
                            fileInfoDicomBuffer[(t * destDepth) + Z].getTagTable().setValue("0028,0010",
                                                                                            new Short((short)
                                                                                                      fileInfoDicomBuffer[(t *
                                                                                                                               destDepth) +
                                                                                                                          Z].getExtents()[1]),
                                                                                            2);
                            fileInfoDicomBuffer[(t * destDepth) + Z].getTagTable().setValue("0028,0011",
                                                                                            new Short((short)
                                                                                                      fileInfoDicomBuffer[(t *
                                                                                                                               destDepth) +
                                                                                                                          Z].getExtents()[0]),
                                                                                            2);

                            // set image rows ("0028,0010") stringForDicom = String.valueOf(destImage.getExtents()[1]);
                            // fileInfoBuffer.setValue("0028,0010", stringForDicom);
                            // fileInfoBuffer.setValue("0028,0010", new Short((short)fileInfoBuffer.getExtents()[1]),
                            // 2);
                            isDicom = true;
                        } else { // NOT DICOM
                            fileInfoBuffer[(t * destDepth) + Z] = (FileInfoBase)
                                                                      srcImage.getFileInfo((t * srcDepth) + z).clone();
                            fileInfoBuffer[(t * destDepth) + Z].setOrigin(newOriginLPS);
                            fileInfoBuffer[(t * destDepth) + Z].setExtents(newExtents);
                            isDicom = false;
                        }

                        if (!((Z < frontMargin) || (Z >= (srcDepth + frontMargin - 1)))) {

                            /* While the destImage slice offset is outside the range of the srcImage image,
                             * dont update the srcImage counter.  This way: For new slices before the start of the
                             * original image set, copy the first FileInfoBuffer and modify. For new slices that
                             * correspond to an existing image, copy that FileInfoBuffer and modify. For new slices
                             * after the end of the original image set, copy the last FileInfoBuffer and modify.
                             */
                            z++; // goto the next slice in the source image
                        }
                    }
                } // for (t = 0; t < tDim; t++)

                if (srcImage.getParentFrame() != null) {
                    srcImage.getParentFrame().close();
                }

                srcImage.disposeLocal();
                srcImage = null;

                srcImage = new ModelImage(dataType, newExtents, imageName);

                try {
                    srcImage.importData(0, destSlice, true);
                } catch (IOException ioe) {
                    sourceSlice = null;
                    destSlice = null;
                    MipavUtil.displayError("AlgorithmAddMargins reports:\n" + ioe.toString());

                    setCompleted(false);

                    return;
                }


                for (t = 0; t < tDim; t++) {

                    for (Z = 0; Z < destDepth; Z++) {

                        if (isDicom) {
                            srcImage.setFileInfo(fileInfoDicomBuffer[(t * destDepth) + Z], ((t * destDepth) + Z));
                        } else {
                            srcImage.setFileInfo(fileInfoBuffer[(t * destDepth) + Z], ((t * destDepth) + Z));
                        }
                    }
                } // for (t = 0; t < tDim; t++)

                fireProgressStateChanged(100); // show at 100%

                if (threadStopped) {
                    sourceSlice = null;
                    destSlice = null;
                    finalize();

                    return;
                }
            } catch (IOException ioe) {
                sourceSlice = null;
                destSlice = null;
                MipavUtil.displayError("AlgorithmAddMargins reports:\n" + ioe.toString());

                setCompleted(false);

                return;
            }
        }

        srcImage.calcMinMax();
        sourceSlice = null;
        destSlice = null;
    }

    /**
     * Adds image margins and stores result in destImage.
     */
    private void calcStoreInDest() {
        int srcPlate, destPlate; // an indexed slice, or 'plate,' of image. One for source image, one for destImage,

        int i;
        int z; // z is slice-depth of srcImage; Z is slice-depth of destination
        int startDraw; // starting column for writing the source image data

        int srcWidth; // width of a slice??
        int srcHeight;
        int srcDepth;
        int srcSliceArea; // area (x*y) of the source

        int destWidth;
        int destHeight;
        int destDepth;
        int destSliceArea; // area (x*y) of the destination
        int t;
        int tDim;
        int tOldOffset, tNewOffset;

        // For updating origin
        float[] newOriginLPS = new float[3];
        int[] marginVector = new int[3];
        String stringForDicom;

        srcWidth = srcImage.getExtents()[0];
        srcHeight = srcImage.getExtents()[1];
        destWidth = destImage.getExtents()[0];
        destHeight = destImage.getExtents()[1];

        int topRows = destWidth * topMargin; // number of elements in the top of the destImage image but are not in
                                             // srcImage

        srcSliceArea = srcWidth * srcHeight;
        destSliceArea = destWidth * destHeight;

        if (srcImage.getNDims() == 4) {
            tDim = srcImage.getExtents()[3];
        } else {
            tDim = 1;
        }

        // make a location & view the progressbar; make length & increment of progressbar.
        fireProgressStateChanged(srcImage.getImageName(), "Adding image borders...");


        Number[] headerFooterMargins;
        Number[] leftMargins;
        Number[] rightMargins;
        Number[] values;

        if (!useBuffers) { // do not use local buffer

            try {
                headerFooterMargins = new Number[colorFactor * destWidth];
                leftMargins = new Number[colorFactor * leftMargin];
                rightMargins = new Number[colorFactor * rightMargin];
                values = new Number[colorFactor * srcWidth];

                // load minimum into the margins
                if (colorFactor == 1) {

                    for (i = 0; (i < destWidth) && !threadStopped; i++) {
                        headerFooterMargins[i] = new Double(marginValue); // load the minimum into

                        if (i < leftMargin) {
                            leftMargins[i] = new Double(marginValue);
                        }

                        if (i < rightMargin) {
                            rightMargins[i] = new Double(marginValue);
                        }

                    }
                } // if (colorFactor == 1)
                else { // colorFactor = 4

                    for (i = 0; (i < (4 * destWidth)) && !threadStopped; i = i + 4) {
                        headerFooterMargins[i] = new Double(255.0);
                        headerFooterMargins[i + 1] = new Double(redValue);
                        headerFooterMargins[i + 2] = new Double(greenValue);
                        headerFooterMargins[i + 3] = new Double(blueValue);

                        if (i < (4 * leftMargin)) {
                            leftMargins[i] = new Double(255.0);
                            leftMargins[i + 1] = new Double(redValue);
                            leftMargins[i + 2] = new Double(greenValue);
                            leftMargins[i + 3] = new Double(blueValue);
                        }

                        if (i < (4 * rightMargin)) {
                            rightMargins[i] = new Double(255.0);
                            rightMargins[i + 1] = new Double(redValue);
                            rightMargins[i + 2] = new Double(greenValue);
                            rightMargins[i + 3] = new Double(blueValue);
                        }
                    }
                } // else colorFactor == 4
            } catch (OutOfMemoryError error) {
                headerFooterMargins = null;
                leftMargins = null;
                rightMargins = null;
                values = null;
                System.gc();
                displayError("Algorithm Add Image Margins: Out of memory");
                setCompleted(false);


                return;
            }

            if (threadStopped) {
                headerFooterMargins = null;
                leftMargins = null;
                rightMargins = null;
                values = null;
                finalize();

                return;
            }

            try { // TRY to make import/export for 2d & 3d when not using buffers

                // chk for 2D
                if (srcImage.getNDims() == 2) {

                    // TOP MARGIN LATITUDES: over the top margin, copy in the default values
                    for (i = 0; (i < topMargin) && !threadStopped; i++) {
                        fireProgressStateChanged(Math.round((float) i / destHeight * 100));

                        destImage.importData(i * destWidth * colorFactor, headerFooterMargins, false);
                    }

                    // IMAGE LATITUDES: skip through the added 'top' margin, then copy the source image into offset
                    // place
                    for (i = 0; (i < srcHeight) && !threadStopped; i++) {
                        fireProgressStateChanged(Math.round((float) (i + topMargin) / destHeight * 100));

                        destImage.importData(colorFactor * (topRows + (destWidth * i)), leftMargins, false); // left margin

                        // image
                        srcImage.exportData(colorFactor * srcWidth * i, colorFactor * srcWidth, values); // pull out img data row by row
                        startDraw = topRows + (destWidth * i) + leftMargin;
                        destImage.importData(colorFactor * startDraw, values, false); // place the srcImage image,
                                                                                      // copying row of img data by row.
                        destImage.importData(colorFactor * (startDraw + srcWidth), rightMargins, false); // right margin
                    }

                    // BOTTOM MARGIN LATITUDES: over the bottom margin, copy in the default values
                    for (i = topMargin + srcHeight; (i < destHeight) && !threadStopped; i++) {
                        fireProgressStateChanged(Math.round((float) i / destHeight * 100));

                        destImage.importData(colorFactor * i * destWidth, headerFooterMargins, false);
                    }

                    if (threadStopped) {
                        headerFooterMargins = null;
                        leftMargins = null;
                        rightMargins = null;
                        values = null;
                        finalize();

                        return;
                    }

                    marginVector[0] = leftMargin;
                    marginVector[1] = topMargin;
                    marginVector[2] = 0; // since 2D here

                    newOriginLPS = calculateNewOrigin(srcImage, marginVector);
                    //System.out.println("New LPS origin: " + Float.toString(newOriginLPS[0]) + ", " +
                                       //Float.toString(newOriginLPS[1]) + ", " + Float.toString(newOriginLPS[2]));

                    // FILE INFO: add the file info    (if the original is a DICOM image, do a special file info...)
                    fireProgressStateChanged("Updating File Info...");

                    if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        FileInfoDicom fileInfoBuffer; // buffer of type DICOM
                        fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(0).clone(); // copy into buffer
                        fileInfoBuffer.setExtents(destImage.getExtents());

                        stringForDicom = Float.toString(newOriginLPS[0]) + "\\" + Float.toString(newOriginLPS[1]) +
                                         "\\" + Float.toString(newOriginLPS[2]);
                        fileInfoBuffer.getTagTable().setValue("0020,0032", stringForDicom, stringForDicom.length());
                        fileInfoBuffer.setOrigin(newOriginLPS);
                        destImage.setFileInfo(fileInfoBuffer, 0);

                        // set image rows ("0028,0010")
                        stringForDicom = String.valueOf(destImage.getExtents()[0]);
                        fileInfoBuffer.getTagTable().setValue("0028,0010", stringForDicom);

                        // set image columns ("0028,0011")
                        stringForDicom = String.valueOf(destImage.getExtents()[1]);
                        fileInfoBuffer.getTagTable().setValue("0028,0011", stringForDicom);

                    } else { // not a DICOM image,

                        FileInfoBase fileInfoBuffer; // buffer of any old type
                        fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo(0).clone();
                        fileInfoBuffer.setOrigin(newOriginLPS);
                        fileInfoBuffer.setExtents(destImage.getExtents()); // SET extents for the destination
                        destImage.setFileInfo(fileInfoBuffer, 0);
                    }

                    fireProgressStateChanged(100); // show at 100%
                } else { // source is 3D or 4D

                    int Z;

                    srcDepth = srcImage.getExtents()[2];
                    destDepth = destImage.getExtents()[2];

                    // insert margin values into the blank slices
                    for (t = 0; (t < tDim) && !threadStopped; t++) {
                        tOldOffset = t * srcDepth * srcHeight * srcWidth;
                        tNewOffset = t * destDepth * destHeight * destWidth;

                        for (z = 0; (z < frontMargin) && !threadStopped; z++) {
                            destPlate = z * destSliceArea;

                            fireProgressStateChanged(Math.round(((float) ((t * destDepth) + z)) / (tDim * destDepth) *
                                                                    100));

                            for (i = 0; (i < destHeight) && !threadStopped; i++) {
                                destImage.importData(colorFactor * (tNewOffset + destPlate + (i * destWidth)),
                                                     headerFooterMargins, false);
                            }
                        }

                        for (z = 0; (z < srcDepth) && !threadStopped; z++) { // for all slices in the old image
                            srcPlate = z * srcSliceArea;
                            destPlate = (z * destSliceArea) + (frontMargin * destSliceArea);

                            // let user know something is happening by updating the progressbar
                            if ((tDim == 1)) {
                                fireProgressStateChanged(Math.round((float) (z + frontMargin) / destDepth * 100));
                            }

                            // TOP MARGIN LATITUDES: over the top margin, copy in the default values
                            for (i = 0; (i < topMargin) && !threadStopped; i++) { // top margin
                                destImage.importData(colorFactor * (tNewOffset + destPlate + (i * destWidth)),
                                                     headerFooterMargins, false);
                            }

                            // IMAGE LATITUDES: skip through the added 'top' margin, then copy the source image into
                            // offset place
                            for (i = 0; (i < srcHeight) && !threadStopped; i++) {
                                destImage.importData(colorFactor * (tNewOffset + destPlate + (i * destWidth) + topRows),
                                                     leftMargins, false); // left margin

                                srcImage.exportData(colorFactor * (tOldOffset + srcPlate + (srcWidth * i)),
                                                    colorFactor * srcWidth, values); // pull out img data row by row
                                                                                     // from each plate
                                startDraw = destPlate + topRows + (destWidth * i) + leftMargin; // start skips top and
                                                                                                // left margin to
                                destImage.importData(colorFactor * (tNewOffset + startDraw), values, false); // place the srcImage image, copying row of img data by row.
                                destImage.importData(colorFactor * (tNewOffset + startDraw + srcWidth), rightMargins,
                                                     false); // right margin
                            }

                            // BOTTOM MARGIN LATITUDES: over the bottom margin, copy in the default values
                            for (i = srcHeight + topMargin; (i < destHeight) && !threadStopped; i++) { // bottom margin
                                destImage.importData(colorFactor * (tNewOffset + destPlate + (i * destWidth)),
                                                     headerFooterMargins, false);
                            }
                        }

                        // insert places for the back margin
                        for (z = frontMargin + srcDepth; (z < destDepth) && !threadStopped; z++) {
                            destPlate = z * destSliceArea;

                            if ((tDim == 1)) {
                                fireProgressStateChanged(Math.round((float) z / (destDepth) * 100));
                            }

                            for (i = 0; (i < destHeight) && !threadStopped; i++) {
                                destImage.importData(colorFactor * (tNewOffset + destPlate + (i * destWidth)),
                                                     headerFooterMargins, false);
                            }
                        }

                        if (threadStopped) {
                            headerFooterMargins = null;
                            leftMargins = null;
                            rightMargins = null;
                            values = null;
                            finalize();

                            return;
                        }

                        marginVector[0] = leftMargin;
                        marginVector[1] = topMargin;
                        marginVector[2] = frontMargin;

                        newOriginLPS = calculateNewOrigin(srcImage, marginVector);
                        //System.out.println("New LPS origin: " + Float.toString(newOriginLPS[0]) + ", " +
                                           //Float.toString(newOriginLPS[1]) + ", " + Float.toString(newOriginLPS[2]));

                        float delta = srcImage.getFileInfo()[0].getResolutions()[2];
                        int axisOrient = srcImage.getFileInfo()[0].getAxisOrientation(2);

                        if ((axisOrient != FileInfoBase.ORI_A2P_TYPE) && (axisOrient != FileInfoBase.ORI_R2L_TYPE) &&
                                (axisOrient != FileInfoBase.ORI_I2S_TYPE)) {
                            delta = -delta;
                        }

                        float[] originImg = originLPS2Img(newOriginLPS, srcImage);
                        float startLoc = originImg[2];
                        //System.out.println("Start location is " + startLoc + ".\n");

                        // FILE INFO: add the file info for 3D images
                        if ((tDim == 1)) {
                            fireProgressStateChanged("Updating File Info...");
                            // int fillLength = Math.round((float)z/destDepth); int piece = (1 - fillLength);
                        }

                        z = 0; // z is the counter for the orig image

                        for (Z = 0; (Z < destDepth) && !threadStopped; Z++) {

                            if ((tDim == 1)) {
                                fireProgressStateChanged(Math.round((float) (Z) / destDepth * 100));
                            }

                            // DICOM
                            if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                                FileInfoDicom fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(z).clone();

                                fileInfoBuffer.setExtents(destImage.getExtents()); // modify extents to use the extents
                                                                                   // of destImage img

                                // change the slice number ("0020,0013"):
                                // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                                // Reset the image (slice) number with the new number ordering
                                stringForDicom = Integer.toString(Z + 1);
                                fileInfoBuffer.getTagTable().setValue("0020,0013", stringForDicom,
                                                                      stringForDicom.length());

                                if (newOriginLPS != null) {
                                    originImg[2] = startLoc + (delta * Z);
                                    newOriginLPS = originImg2LPS(originImg, srcImage);

                                    stringForDicom = Float.toString(newOriginLPS[0]) + "\\" +
                                                     Float.toString(newOriginLPS[1]) + "\\" +
                                                     Float.toString(newOriginLPS[2]);
                                    fileInfoBuffer.getTagTable().setValue("0020,0032", stringForDicom,
                                                                          stringForDicom.length());
                                    fileInfoBuffer.setOrigin(newOriginLPS);
                                }

                                // readjust the slice location ("0020,1041")
                                if (imgOriginLPS != null) {
                                    stringForDicom = String.valueOf(imgOriginLPS[2]);
                                    fileInfoBuffer.getTagTable().setValue("0020,1041", stringForDicom,
                                                                          stringForDicom.length());
                                }

                                // set image columns ("0028,0011")
                                // stringForDicom = String.valueOf(destImage.getExtents()[0]);
                                // fileInfoBuffer.setValue("0028,0011", stringForDicom);
                                fileInfoBuffer.getTagTable().setValue("0028,0010",
                                                                      new Short((short) fileInfoBuffer.getExtents()[1]),
                                                                      2);
                                fileInfoBuffer.getTagTable().setValue("0028,0011",
                                                                      new Short((short) fileInfoBuffer.getExtents()[0]),
                                                                      2);

                                // set image rows ("0028,0010") stringForDicom =
                                // String.valueOf(destImage.getExtents()[1]); fileInfoBuffer.setValue("0028,0010",
                                // stringForDicom); fileInfoBuffer.setValue("0028,0010", new
                                // Short((short)fileInfoBuffer.getExtents()[1]), 2);
                                destImage.setFileInfo(fileInfoBuffer, Z);
                            } else { // NOT DICOM

                                FileInfoBase fileInfoBuffer;
                                fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * srcDepth) + z).clone();
                                fileInfoBuffer.setOrigin(newOriginLPS);
                                fileInfoBuffer.setExtents(destImage.getExtents());
                                destImage.setFileInfo(fileInfoBuffer, ((t * destDepth) + Z));
                            }

                            if (!((Z < frontMargin) || (Z >= (srcDepth + frontMargin - 1)))) {

                                /* While the destImage slice offset is outside the range of the srcImage image,
                                 * dont update the srcImage counter.  This way: For new slices before the start of the
                                 * original image set, copy the first FileInfoBuffer and modify. For new slices that
                                 * correspond to an existing image, copy that FileInfoBuffer and modify. For new slices
                                 * after the end of the original image set, copy the last FileInfoBuffer and modify.
                                 */
                                z++; // goto the next slice in the source image
                            }
                        }
                    } // for (t = 0; t < tDim; t++)

                    if (threadStopped) {
                        headerFooterMargins = null;
                        leftMargins = null;
                        rightMargins = null;
                        values = null;
                        finalize();

                        return;
                    }

                }
            } catch (IOException ioe) {
                headerFooterMargins = null;
                values = null;
                MipavUtil.displayError("AlgorithmAddImageMargin reports:\n" + ioe.toString());

                setCompleted(false);

                return;
            }

        } else { // use local buffer
            useLocalBuffer(colorFactor * srcWidth, srcHeight, colorFactor * destWidth, destHeight);
        }

        if (threadStopped) {
            headerFooterMargins = null;
            values = null;
            finalize();

            return;
        }

        destImage.calcMinMax(); // calculate the minimum & maximum intensity values for the destImage-image

        // Clean up and let the calling dialog know that algorithm did its job
        headerFooterMargins = null;
        values = null;

        setCompleted(true);
    }

    /**
     * Adds image margins and stores result in srcImage Must use getSrcImage after running this routine.
     */
    private void calcStoreInPlace() {

        int srcWidth;
        int srcHeight;


        int destWidth;
        int destHeight;

        srcWidth = srcImage.getExtents()[0];
        srcHeight = srcImage.getExtents()[1];
        destWidth = srcWidth + leftMargin + rightMargin;
        destHeight = srcHeight + topMargin + bottomMargin;

        // make a location & view the progressbar; make length & increment of progressbar.
        fireProgressStateChanged(srcImage.getImageName(), "Adding image borders...");


        useLocalBufferForSource(colorFactor * srcWidth, srcHeight, colorFactor * destWidth, destHeight);

        if (threadStopped) {
            finalize();

            return;
        }

        // Clean up and let the calling dialog know that algorithm did its job

        setCompleted(true);
    }

    /**
     * Switch origin order from LPS order to Img order.
     *
     * @param   srcImg  DOCUMENT ME!
     * @param   margin  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] calculateNewOrigin(ModelImage srcImg, int[] margin) {

        FileInfoBase fileInfoBuffer; // buffer of any old tYPE
        fileInfoBuffer = srcImage.getFileInfo()[0];

        float[] imgOriginLPS = new float[3];
        float[] newImgOriginLPS = new float[3];
        float[] originImgOrd = new float[3];

        if (fileInfoBuffer.getFileFormat() == FileUtility.DICOM) {
            FileInfoDicom fileDicom = (FileInfoDicom) fileInfoBuffer;
            imgOriginLPS = convertIntoFloat(fileDicom.parseTagValue("0020,0032"));
        } else {
            imgOriginLPS = fileInfoBuffer.getOrigin();
        }

        //System.out.println("Original LPS origin: " + Float.toString(imgOriginLPS[0]) + ", " +
                           //Float.toString(imgOriginLPS[1]) + ", " + Float.toString(imgOriginLPS[2]));

        originImgOrd = originLPS2Img(imgOriginLPS, srcImage);
        // System.out.println("Original origin in image order: " +Float.toString(originImgOrd[0]) +", "      +
        // Float.toString(originImgOrd[1]) + ", " + Float.toString(originImgOrd[2]));

        float[] tmpResol = fileInfoBuffer.getResolutions();
        float[] resol = new float[3];

        for (int i = 0; i < 2; i++) {
            resol[i] = tmpResol[i];
        }

        if (srcImg.getNDims() == 3) {
            resol[2] = tmpResol[2];
        }

        int axisOrient;

        for (int i = 0; i < 3; i++) {
            axisOrient = fileInfoBuffer.getAxisOrientation(i);

            if ((axisOrient == FileInfoBase.ORI_A2P_TYPE) || (axisOrient == FileInfoBase.ORI_R2L_TYPE) ||
                    (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                originImgOrd[i] = originImgOrd[i] - (resol[i] * margin[i]);
            } else {
                originImgOrd[i] = originImgOrd[i] + (resol[i] * margin[i]);
            }
        }

        newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);

        return newImgOriginLPS;
    }
   
    /**
     * Switch origin order from image order to LPS order.
     *
     * @param   origImg  DOCUMENT ME!
     * @param   img      DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] originImg2LPS(float[] origImg, ModelImage img) {
        float[] origLPS = new float[3];
        TransMatrix img2LPS = img.getMatrix();

        for (int i = 0; i < 3; i++) { // i's are the rows

            for (int j = 0; j < 3; j++) { // j's are the columns

                if (img2LPS.Get(i, j) != 0) {
                    origLPS[i] = origImg[j];
                }
            }
        }

        return origLPS;
    }

    /**
     * Switch origin order from LPS order to Img order.
     *
     * @param   origLPS  DOCUMENT ME!
     * @param   img      DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] originLPS2Img(float[] origLPS, ModelImage img) {
        float[] origImg = new float[3];
        TransMatrix LPS2img = new TransMatrix(img.getMatrix());
        LPS2img.Inverse();

        for (int i = 0; i < 3; i++) { // i's are the rows

            for (int j = 0; j < 3; j++) { // j's are the columns

                if (LPS2img.Get(i, j) != 0) {
                    origImg[i] = origLPS[j];
                }
            }
        }

        return origImg;
    }
}
