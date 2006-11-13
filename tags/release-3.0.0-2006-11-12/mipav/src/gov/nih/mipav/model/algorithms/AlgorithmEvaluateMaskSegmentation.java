package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Compares segmentation results of a test image to segmentation results of an ideal gold standard true image.
 * Comparisons are made for identical nonzero levels in the 2 images. For each nonzero level, the false negative volume
 * fraction, the false positive volume fraction, and the positive volume fraction are output to the global data text.
 * The images must be boolean, UBYTE, or USHORT.
 */

public class AlgorithmEvaluateMaskSegmentation extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** total number of voxels at a particular nonzero level. */
    private int absoluteTrue;

    /** where trueImage has the nonzero level. */
    private int falseNegative;

    /** where trueImage does not have the nonzero level. */
    private int falsePositive;

    /** false negative volume fraction. */
    private float fnvf;

    /** false positive volume fraction. */
    private float fpvf;

    /** DOCUMENT ME! */
    private int length;

    /** DOCUMENT ME! */
    private int levelsMatch;

    /** DOCUMENT ME! */
    private boolean present;

    /** DOCUMENT ME! */
    private short[] testArray;

    /** DOCUMENT ME! */
    private ModelImage testImage;

    /** DOCUMENT ME! */
    private int testLength;

    /** DOCUMENT ME! */
    private int[] testLevelArray = new int[20];

    /** DOCUMENT ME! */
    private int testLevels;

    /** positive volume fraction. */
    private float tpvf;

    /** DOCUMENT ME! */
    private short[] trueArray;

    /** in the true image. */
    private int trueFound;

    /** DOCUMENT ME! */
    private int[] trueLevelArray = new int[20];

    /** DOCUMENT ME! */
    private int trueLevels;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmEvaluateMaskSegmentation object.
     *
     * @param  trueImage  image model used as a ideal gold standard
     * @param  testImage  image model tested against the trueImage
     */
    public AlgorithmEvaluateMaskSegmentation(ModelImage trueImage, ModelImage testImage) {
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
        int tLevel;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

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

        trueArray = new short[length];
        testArray = new short[length];

        try {
            srcImage.exportData(0, length, trueArray);
        } catch (IOException e) {
            MipavUtil.displayError("IOError on trueImage.exportData");
            setCompleted(false);

            return;
        }

        for (i = 0; i < length; i++) {

            if (trueArray[i] != 0) {
                present = false;

                for (j = 0; (j < trueLevels) && (!present); j++) {

                    if (trueArray[i] == trueLevelArray[j]) {
                        present = true;
                    }
                }

                if (!present) {
                    trueLevelArray[trueLevels++] = trueArray[i];
                }
            }
        }

        try {
            testImage.exportData(0, length, testArray);
        } catch (IOException e) {
            MipavUtil.displayError("IOError on testImage.exportData");
            setCompleted(false);

            return;
        }

        for (i = 0; i < length; i++) {

            if (testArray[i] != 0) {
                present = false;

                for (j = 0; (j < testLevels) && (!present); j++) {

                    if (testArray[i] == testLevelArray[j]) {
                        present = true;
                    }
                }

                if (!present) {
                    testLevelArray[testLevels++] = testArray[i];
                }
            }
        }

        if (trueLevels != testLevels) {
            MipavUtil.displayError(srcImage.getImageName() + " and " + testImage.getImageName() +
                                   " have unequal numbers of nonzero levels");
            setCompleted(false);

            return;
        }

        if (trueLevels == testLevels) {
            levelsMatch = 0;

            for (i = 0; i < trueLevels; i++) {

                for (j = 0; j < trueLevels; j++) {

                    if (trueLevelArray[i] == testLevelArray[j]) {
                        levelsMatch++;
                    }
                }
            }
        }

        if (trueLevels != levelsMatch) {
            MipavUtil.displayError(srcImage.getImageName() + " and " + testImage.getImageName() +
                                   " have different nonzero levels");
            setCompleted(false);

            return;
        }

        ViewUserInterface.getReference().setGlobalDataText(srcImage.getImageName() + " = true\n");
        ViewUserInterface.getReference().setGlobalDataText(testImage.getImageName() + " = test\n");

        for (i = 0; i < trueLevels; i++) {
            tLevel = trueLevelArray[i];
            absoluteTrue = 0;
            trueFound = 0;
            falseNegative = 0;
            falsePositive = 0;

            for (k = 0; k < length; k++) {

                if (trueArray[k] == tLevel) {
                    absoluteTrue++;

                    if (testArray[k] == tLevel) {
                        trueFound++;
                    } else {
                        falseNegative++;
                    }
                } // if (trueArray[k] == tLevel)
                else { // trueArray[k] != tLevel

                    if (testArray[k] == tLevel) {
                        falsePositive++;
                    }
                } // else trueArray[k] != tLevel
            } // for (k = 0; k < length; k++)

            ViewUserInterface.getReference().setGlobalDataText("Statistics for level = " + String.valueOf(tLevel) + "\n");
            fnvf = (float) falseNegative / (float) absoluteTrue;
            ViewUserInterface.getReference().setGlobalDataText("     False negative volume fraction = " + String.valueOf(fnvf) + "\n");
            fpvf = (float) falsePositive / (float) absoluteTrue;
            ViewUserInterface.getReference().setGlobalDataText("     False positive volume fraction = " + String.valueOf(fpvf) + "\n");
            tpvf = (float) trueFound / (float) absoluteTrue;
            ViewUserInterface.getReference().setGlobalDataText("     True Positive volume fraction = " + String.valueOf(tpvf) + "\n\n");

        } // for (i = 0; i < trueLevels; i++)

        setCompleted(true);
    }

}
