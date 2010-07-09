package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 * Compares segmentation results of a test image to segmentation results of an ideal gold standard true image.
 * Comparisons are made for contour or polyline vois having the same ids in the 2 images. For each id number, the false
 * negative volume fraction, the false positive volume fraction, and the positive volume fraction are output to the
 * global data text.
 */

public class AlgorithmEvaluateSegmentation extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** total number of id voxels in true image. */
    private int absoluteTrue;

    /** where trueImage has id but test image does not. */
    private int falseNegative;

    /** where trueImage does not have id but test image does. */
    private int falsePositive;

    /** false negative volume fraction. */
    private float fnvf;

    /** false positive volume fraction. */
    private float fpvf;

    /** DOCUMENT ME! */
    private int length;

    /** DOCUMENT ME! */
    private int nTestVOIs;

    /** DOCUMENT ME! */
    private int nTrueVOIs;

    /** DOCUMENT ME! */
    private int testID;

    /** DOCUMENT ME! */
    private ModelImage testImage;

    /** DOCUMENT ME! */
    private int testLength;

    /** DOCUMENT ME! */
    private short[] testMask;

    /** DOCUMENT ME! */
    private ViewVOIVector testVOIs;

    /** positive volume fraction. */
    private float tpvf;

    /** number of the absoluteTrue found in the test image. */
    private int trueFound;

    /** DOCUMENT ME! */
    private int trueID;

    /** DOCUMENT ME! */
    private short[] trueMask;

    /** DOCUMENT ME! */
    private ViewVOIVector trueVOIs;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmEvaluateSegmentation object.
     *
     * @param  trueImage  image model used as a ideal gold standard
     * @param  testImage  image model tested against the trueImage
     */
    public AlgorithmEvaluateSegmentation(ModelImage trueImage, ModelImage testImage) {
        super(null, trueImage);
        this.testImage = testImage;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        testImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int i, j, k;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        trueVOIs = srcImage.getVOIs();
        nTrueVOIs = trueVOIs.size();
        testVOIs = testImage.getVOIs();
        nTestVOIs = testVOIs.size();
        length = srcImage.getExtents()[0];

        for (i = 1; i < srcImage.getNDims(); i++) {
            length *= srcImage.getExtents()[i];
        }

        testLength = testImage.getExtents()[0];

        for (i = 1; i < testImage.getNDims(); i++) {
            testLength *= testImage.getExtents()[i];
        }

        if (length != testLength) {
            MipavUtil.displayError(srcImage.getImageName() + " and " + testImage.getImageName() +
                                   " are unequal in dimensions");
            setCompleted(false);

            return;
        }

        trueMask = new short[length];
        testMask = new short[length];
        ViewUserInterface.getReference().setGlobalDataText(srcImage.getImageName() + " = true\n");
        ViewUserInterface.getReference().setGlobalDataText(testImage.getImageName() + " = test\n");

        for (i = 0; i < nTrueVOIs; i++) {

            if ((trueVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                    (trueVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                trueID = trueVOIs.VOIAt(i).getID();

                for (j = 0; j < nTestVOIs; j++) {
                    testID = testVOIs.VOIAt(j).getID();

                    if (trueID == testID) {

                        for (k = 0; k < length; k++) {
                            trueMask[k] = -1;
                            testMask[k] = -1;
                        }

                        trueMask = srcImage.generateVOIMask(trueMask, i);
                        testMask = testImage.generateVOIMask(testMask, j);
                        absoluteTrue = 0;
                        trueFound = 0;
                        falseNegative = 0;
                        falsePositive = 0;

                        for (k = 0; k < length; k++) {

                            if (trueMask[k] == trueID) {
                                absoluteTrue++;

                                if (testMask[k] == trueID) {
                                    trueFound++;
                                } else {
                                    falseNegative++;
                                }
                            } // if (trueMask[k] == trueID)
                            else { // trueMask[k] != trueID

                                if (testMask[k] == trueID) {
                                    falsePositive++;
                                }
                            } // else trueMask[k] != trueID
                        } // for (k = 0; k < length; k++)

                        ViewUserInterface.getReference().setGlobalDataText("Statistics for VOIs with ID = " + String.valueOf(trueID) +
                                                        "\n");
                        fnvf = (float) falseNegative / (float) absoluteTrue;
                        ViewUserInterface.getReference().setGlobalDataText("     False negative volume fraction = " +
                                                        String.valueOf(fnvf) + "\n");
                        fpvf = (float) falsePositive / (float) absoluteTrue;
                        ViewUserInterface.getReference().setGlobalDataText("     False positive volume fraction = " +
                                                        String.valueOf(fpvf) + "\n");
                        tpvf = (float) trueFound / (float) absoluteTrue;
                        ViewUserInterface.getReference().setGlobalDataText("     True Positive volume fraction = " + String.valueOf(tpvf) +
                                                        "\n\n");
                    } // if (trueID == testID)
                } // for (j = 0; j < nTestVOIs; j++)
            } // if ((trueVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nTrueVOIs; i++)

        setCompleted(true);
    }

}
