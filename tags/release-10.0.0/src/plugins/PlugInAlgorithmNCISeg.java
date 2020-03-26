import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewUserInterface;

import java.util.*;


/**
 * <p>Title: PlugInAlgorithmNCISeg</p>
 *
 * <p>Description: Finds "browness" in image using two threshold values</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */
public class PlugInAlgorithmNCISeg extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    boolean doBrown = false;

    /** DOCUMENT ME! */
    boolean doMask = false;

    /** DOCUMENT ME! */
    private float[] destBuffer = null;

    /** DOCUMENT ME! */
    private float diffThreshold;

    /** DOCUMENT ME! */
    private float[] srcBuffer = null;

    /** DOCUMENT ME! */
    private float threshold;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for blue or brown quantification.
     *
     * @param  destImg     ModelImage
     * @param  srcImg      ModelImage
     * @param  doBrown     DOCUMENT ME!
     * @param  thres       float threshold value for inverse blue
     * @param  diff_thres  float threshold value for difference btwn inverse blue and inverse r/g channels
     */
    public PlugInAlgorithmNCISeg(ModelImage destImg, ModelImage srcImg, boolean doBrown, float thres,
                                 float diff_thres) {
        this.srcImage = srcImg;
        this.destImage = destImg;
        this.threshold = thres;
        this.diffThreshold = diff_thres;
        this.doBrown = doBrown;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if ((srcImage == null) && (destImage == null)) {
            displayError("NCI Segmentation.run(): Source  and/or Destination image is null");

            return;
        }

        if (srcImage.isColorImage() == false) {
            displayError("NCI Segmentation.run(): Source Image is not a RGB type");

            return;
        }

        int numVOIs = srcImage.getVOIs().size();

        for (int i = 0; i < numVOIs; i++) {

            if (srcImage.getVOIs().VOIAt(i).getCurveType() == VOI.CONTOUR) {
                doMask = true;

                break;
            }
        }

        
        calcStoreInDest();
    }

    /**
     * DOCUMENT ME!
     */
    private void calcStoreInDest() {
        BitSet mask = null;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];

        int srcLength = xDim * yDim * 4;
        int destLength = xDim * yDim;

        srcBuffer = new float[srcLength];
        destBuffer = new float[destLength];

        try {
            srcImage.exportData(0, srcLength, srcBuffer);
        } catch (Exception ex) {
            // do somethng
        }

        if (doMask) {
            mask = srcImage.generateVOIMask();
        }

        float redInverse = 0f;
        float greenInverse = 0f;
        float blueInverse = 0f;

        int counter = 0;
        double totalBrowness = 0;

        float blue = 0f;
        float red = 0f;
        float green = 0f;
        double totalBlue = 0;

        // run through the srcBuffer
        for (int i = 0; i < srcLength; i += 4) {

            if (!doMask || mask.get(i / 4)) {

                if (doBrown) {

                    // see if the blue channel (inversed) is greater than a threshold
                    blueInverse = 255f - srcBuffer[i + 3];

                    if (blueInverse > threshold) {

                        // make sure the red and green channels are not significantly greater than the blue channel
                        redInverse = 255f - srcBuffer[i + 1];
                        greenInverse = 255f - srcBuffer[i + 2];

                        if (((redInverse - blueInverse) > diffThreshold) ||
                                ((greenInverse - blueInverse) > diffThreshold)) {

                            // don't count this pixel/value
                            destBuffer[i / 4] = 0;
                        } else {
                            counter++;
                            destBuffer[i / 4] = blueInverse - threshold;
                            totalBrowness += (blueInverse - threshold);
                        }
                    } else {
                        destBuffer[i / 4] = 0;
                    }
                } else {
                    // looking for blue instead

                    red = srcBuffer[i + 1];
                    green = srcBuffer[i + 2];
                    blue = srcBuffer[i + 3];

                    if ((blue > threshold) && (red > threshold) && (green > threshold)) {
                        destBuffer[i / 4] = 0;
                    } else if (((blue - red) > diffThreshold) && ((blue - green) > diffThreshold)) {
                        counter++;
                        destBuffer[i / 4] = (blue - red) + (blue - green);
                        totalBlue += destBuffer[i / 4];
                    } else {
                        destBuffer[i / 4] = 0;
                    }

                }
            } else {
                destBuffer[i / 4] = 0;
            }
        }

        try {
            destImage.importData(0, destBuffer, true);
        } catch (Exception ex) {
            System.err.println("blah");
        }

        double average = 0;

        if (counter > 0) {

            if (doBrown) {
                average = totalBrowness / counter;
            } else {
                average = totalBlue / counter;
            }
        }

        double area = 0;

        area = (srcImage.getFileInfo(0).getResolutions()[0] * srcImage.getFileInfo(0).getResolutions()[1] * counter);

        System.err.println("Number of pixels included: " + counter);
        System.err.println("Area: " + area + " " +
                           (Unit.getUnitFromLegacyNum(srcImage.getFileInfo(0).getUnitsOfMeasure(0))).getAbbrev() +
                           "^2");

        if (doBrown) {
            System.err.println("Total brown: " + totalBrowness);
        } else {
            System.err.println("Total blue: " + totalBlue);
        }

        System.err.println("Average: " + average);

        ViewUserInterface.getReference().setDataText("Image name: " + srcImage.getImageName() + "\n");
        ViewUserInterface.getReference().setDataText("\tNumber of pixels included: " + counter + "\n");
        ViewUserInterface.getReference().setDataText("\tArea: " + area + " " +
         (Unit.getUnitFromLegacyNum(srcImage.getFileInfo(0).getUnitsOfMeasure(0))).getAbbrev() +
                                                "^2\n");

        if (doBrown) {
            ViewUserInterface.getReference().setDataText("\tTotal brown: " + totalBrowness + "\n");
        } else {
            ViewUserInterface.getReference().setDataText("\tTotal blue: " + totalBlue + "\n");
        }

        ViewUserInterface.getReference().setDataText("\tAverage: " + average + "\n");

        destBuffer = null;
        srcBuffer = null;

        setCompleted(true);
    }
}
