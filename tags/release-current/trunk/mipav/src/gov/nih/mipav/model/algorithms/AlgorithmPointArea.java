package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmPointArea extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] averageIntensities = null;

    /** DOCUMENT ME! */
    private boolean doColor = false;

    /** source image. */
    private ModelImage image;

    /** if x is an even number, use the extra space on left half (otherwise right). */
    private boolean leftSpace = false;

    /** DOCUMENT ME! */
    private float[][] rgbAverageIntensities = null;

    /** DOCUMENT ME! */
    private float threshold;

    /** if y is an even number, use the extra space on the top half (otherwise bottom). */
    private boolean topSpace = false;

    /** DOCUMENT ME! */
    private boolean useThreshold = false;

    /** x value of the origin. */
    private int xLocation;

    /** number of pixels in x direction. */
    private int xSpace;

    /** y value of the origin. */
    private int yLocation;

    /** number of pixels in y direction. */
    private int ySpace;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmPointArea object.
     *
     * @param  srcImage      DOCUMENT ME!
     * @param  xLoc          DOCUMENT ME!
     * @param  yLoc          DOCUMENT ME!
     * @param  xSpace        DOCUMENT ME!
     * @param  ySpace        DOCUMENT ME!
     * @param  leftSpace     DOCUMENT ME!
     * @param  topSpace      DOCUMENT ME!
     * @param  useThreshold  DOCUMENT ME!
     * @param  threshold     DOCUMENT ME!
     */
    public AlgorithmPointArea(ModelImage srcImage, int xLoc, int yLoc, int xSpace, int ySpace, boolean leftSpace,
                              boolean topSpace, boolean useThreshold, float threshold) {
        super(null, srcImage);
        this.image = srcImage;
        this.xLocation = xLoc;
        this.yLocation = yLoc;
        this.xSpace = xSpace;
        this.ySpace = ySpace;
        this.leftSpace = leftSpace;
        this.topSpace = topSpace;
        this.doColor = image.isColorImage();
        this.useThreshold = useThreshold;
        this.threshold = threshold;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        image = null;
        averageIntensities = null;
        rgbAverageIntensities = null;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getAverageIntensities() {
        return averageIntensities;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[][] getRGBAverageIntensities() {
        return rgbAverageIntensities;
    }

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {

        int i, j, k, c;
        int xVal;
        int yVal;

        // check to see that image is 3 dim
        if (image.getNDims() != 3) {
            MipavUtil.displayError("AlgorithmPointAverageIntensities not supported for " + image.getNDims() + "D images");
            disposeLocal();

            return;
        }

        // check to see if xSpace is between 2 and image.getExtents()[0]
        if ((xSpace < 2) || (xSpace > image.getExtents()[0])) {
            MipavUtil.displayError("X-space must be between 2 and " + image.getExtents()[0]);
            disposeLocal();

            return;
        } else if ((ySpace < 2) || (ySpace > image.getExtents()[1])) {
            MipavUtil.displayError("Y-space must be between 2 and " + image.getExtents()[1]);
            disposeLocal();

            return;
        } else if ((xLocation > image.getExtents()[0]) || (yLocation > image.getExtents()[1])) {
            MipavUtil.displayError("Starting location must be within bounds: (" + image.getExtents()[0] + "," +
                                   image.getExtents()[1] + ")");
            disposeLocal();

            return;
        }

        if (doColor) {
            rgbAverageIntensities = new float[3][image.getExtents()[2]];
        } else {
            averageIntensities = new float[image.getExtents()[2]];
        }

        // check to see if xSpace is odd or even and set the starting X
        if ((xSpace % 2) == 0) { // xSpace is even

            if (leftSpace) { // space goes left
                xVal = xLocation - (xSpace / 2);
            } else { // space goes right
                xVal = (xLocation + 1) - (xSpace / 2);
            }
        } else { // xSpace is odd
            xVal = xLocation - ((xSpace - 1) / 2);
        }

        // check to see if ySpace is odd or even and set the starting Y
        if ((ySpace % 2) == 0) { // ySpace is even

            if (topSpace) { // space goes top
                yVal = yLocation - (ySpace / 2);
            } else { // space goes bottom
                yVal = (yLocation + 1) - (ySpace / 2);
            }
        } else { // ySpace is odd
            yVal = yLocation - ((ySpace - 1) / 2);
        }

        int denom;
        int extentX = image.getExtents()[0];
        int extentY = image.getExtents()[1];

        float temp = 0.0f;

        if (!doColor) {
            float sum;

            for (i = 0; i < averageIntensities.length; i++) {
                denom = 0;
                sum = 0.0f;

                for (j = 0; j < xSpace; j++) {

                    for (k = 0; k < ySpace; k++) {

                        // check to see that this point is within the bounds
                        if (((xVal + j) < 0) || ((yVal + k) < 0) || ((xVal + j) > extentX) || ((yVal + k) > extentY)) { // we won't count this location because it isn't in bounds
                        } else { // within bounds

                            // get the intensity at point (xVal + j),(yVal + k)
                            temp = image.getFloat((int) ((i * extentX * extentY) + ((yVal + k) * extentX) +
                                                         (xVal + j)));

                            if (useThreshold && (temp < threshold)) {

                                // do nothing
                                System.err.println("fell below threshold");
                            } else {
                                denom++;
                                sum += temp;
                            }
                        }
                    }
                }

                // divide sum by denom for slice average
                averageIntensities[i] = sum / denom;
                //   System.err.println("Average intensity for slice: " + i + " is " +
                // averageIntensities[i]);
            }
        } else {
            float[] rgbSum = new float[3];
            float[] rgbTemp = new float[3];

            temp = 0.0f;

            for (i = 0; i < rgbAverageIntensities[0].length; i++) {
                denom = 0;
                rgbSum[0] = 0.0f;
                rgbSum[1] = 0.0f;
                rgbSum[2] = 0.0f;

                for (j = 0; j < xSpace; j++) {

                    for (k = 0; k < ySpace; k++) {

                        // check to see that this point is within the bounds
                        if (((xVal + j) < 0) || ((yVal + k) < 0) || ((xVal + j) > extentX) || ((yVal + k) > extentY)) { // we won't count this location because it isn't in bounds
                        } else { // within bounds
                            rgbTemp[0] = 0.0f;
                            rgbTemp[1] = 0.0f;
                            rgbTemp[2] = 0.0f;

                            // get the rgb intensities at point (xVal + j),(yVal + k)
                            for (c = 0; c < 3; c++) {
                                rgbTemp[c] += image.getFloat((int) ((4 *
                                                                         ((i * extentX * extentY) +
                                                                              ((yVal + k) * (xVal + j)))) + c + 1));
                            }

                            if (useThreshold && ((rgbTemp[0] + rgbTemp[1] + rgbTemp[2]) < threshold)) { // do nothing.. didn't meet threshold reqs
                            } else {

                                for (c = 0; c < 3; c++) {
                                    rgbSum[c] += rgbTemp[c];
                                }

                                denom++;
                            }

                        }
                    }
                }

                for (c = 0; c < 3; c++) {
                    rgbAverageIntensities[c][i] = rgbSum[c] / denom;
                }
            }
        }

        this.setCompleted(true);
    }

}
