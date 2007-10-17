package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmStereoDepth extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean isColorImage = false; // indicates the image being messed with is a colour image

    /** DOCUMENT ME! */
    private ModelImage leftImage;

    /** DOCUMENT ME! */
    private ModelImage rightImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which depths are placed in destImage.
     *
     * @param  destImg     image model where result image is to stored
     * @param  leftImage   DOCUMENT ME!
     * @param  rightImage  DOCUMENT ME!
     */
    public AlgorithmStereoDepth(ModelImage destImg, ModelImage leftImage, ModelImage rightImage) {

        super(destImg, rightImage);
        this.leftImage = leftImage;
        this.rightImage = rightImage;

        if (leftImage.isColorImage()) {
            isColorImage = true;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        leftImage = null;
        rightImage = null;
        destImage = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {

        if (leftImage == null) {
            displayError("Left Image is null");

            return;
        }

        if (rightImage == null) {
            displayError("Right Image is null");

            return;
        }

        

        calcStereoDepth();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  image        DOCUMENT ME!
     * @param  resultImage  DOCUMENT ME!
     */
    public void updateFileInfo(ModelImage image, ModelImage resultImage) {
        FileInfoBase[] fileInfo;

        if (resultImage.getNDims() == 2) {
            fileInfo = resultImage.getFileInfo();
            fileInfo[0].setModality(image.getFileInfo()[0].getModality());
            fileInfo[0].setFileDirectory(image.getFileInfo()[0].getFileDirectory());

            // fileInfo[0].setDataType(image.getFileInfo()[0].getDataType());
            fileInfo[0].setEndianess(image.getFileInfo()[0].getEndianess());
            fileInfo[0].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
            fileInfo[0].setResolutions(image.getFileInfo()[0].getResolutions());
            fileInfo[0].setExtents(resultImage.getExtents());
            fileInfo[0].setMax(resultImage.getMax());
            fileInfo[0].setMin(resultImage.getMin());
            fileInfo[0].setImageOrientation(image.getImageOrientation());
            fileInfo[0].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation());
            fileInfo[0].setOrigin(image.getFileInfo()[0].getOrigin());
            fileInfo[0].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
            fileInfo[0].setPhotometric(image.getFileInfo()[0].getPhotometric());
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  image        DOCUMENT ME!
     * @param  resultImage  DOCUMENT ME!
     */
    public void updateSomeFileInfo(ModelImage image, ModelImage resultImage) {
        FileInfoBase[] fileInfo;

        if (resultImage.getNDims() == 2) {
            fileInfo = resultImage.getFileInfo();
            fileInfo[0].setModality(image.getFileInfo()[0].getModality());
            fileInfo[0].setFileDirectory(image.getFileInfo()[0].getFileDirectory());

            // fileInfo[0].setDataType(image.getFileInfo()[0].getDataType());
            fileInfo[0].setEndianess(image.getFileInfo()[0].getEndianess());
            fileInfo[0].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
            fileInfo[0].setResolutions(image.getFileInfo()[0].getResolutions());
            fileInfo[0].setExtents(resultImage.getExtents());

            // fileInfo[0].setMax(resultImage.getMax());
            // fileInfo[0].setMin(resultImage.getMin());
            fileInfo[0].setImageOrientation(image.getImageOrientation());
            fileInfo[0].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation());
            fileInfo[0].setOrigin(image.getFileInfo()[0].getOrigin());
            fileInfo[0].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
            fileInfo[0].setPhotometric(image.getFileInfo()[0].getPhotometric());
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void calcStereoDepth() {
        int length;
        int i;

        float[] leftBuffer;
        float[] rightBuffer;
        boolean[] leftBoolean;
        boolean[] rightBoolean;
        float[] colorBuffer;
        ModelImage leftBWImage;
        ModelImage rightBWImage;
        AlgorithmChangeType changeTypeAlgo;
        AlgorithmRegAIR air;

        int xDim, yDim;
        int x, y;
        float threshold = 10.0f;
        int num;
        float xCenter, yCenter;
        int xCenInt, yCenInt;
        BitSet leftMask;
        BitSet rightMask;
        boolean doAgain;
        int leftBoundary, rightBoundary, topBoundary, bottomBoundary;
        float radius;
        float radsq;
        float distsq;

        xDim = leftImage.getExtents()[0];
        yDim = leftImage.getExtents()[1];

        length = xDim * yDim;

        try {
            leftBuffer = new float[length];
            rightBuffer = new float[length];
        } catch (OutOfMemoryError oome) {
            leftBuffer = null;
            rightBuffer = null;
            errorCleanUp("Algorithm Stereo Depth: Out of memory", true);

            return;
        }

        fireProgressStateChanged(leftImage.getImageName(), "Stereo Depth ...");

        leftBWImage = new ModelImage(ModelImage.FLOAT, leftImage.getExtents(), leftImage.getImageName() + "_changed");

        rightBWImage = new ModelImage(ModelImage.FLOAT, rightImage.getExtents(),
                                      rightImage.getImageName() + "_changed");

        if (!isColorImage) {

            // Make algorithm
            changeTypeAlgo = new AlgorithmChangeType(leftBWImage, leftImage, leftImage.getMin(), leftImage.getMax(),
                                                     leftImage.getMin(), leftImage.getMax(), false);
            changeTypeAlgo.run();
            updateFileInfo(leftImage, leftBWImage);

            // Make algorithm
            changeTypeAlgo = new AlgorithmChangeType(rightBWImage, rightImage, rightImage.getMin(), rightImage.getMax(),
                                                     rightImage.getMin(), rightImage.getMax(), false);
            changeTypeAlgo.run();
            updateFileInfo(rightImage, rightBWImage);

            try {
                leftBWImage.exportData(0, length, leftBuffer);
            } catch (IOException e) {
                leftBuffer = null;
                errorCleanUp("Algorithm Stereo Depth: IOException on leftBWImage.exportData", true);

                return;
            }

            try {
                rightBWImage.exportData(0, length, rightBuffer);
            } catch (IOException e) {
                rightBuffer = null;
                errorCleanUp("Algorithm Stereo Depth: IOException on rightBWImage.exportData", true);

                return;
            }

            for (i = 0; i < length; i++) {
                leftBuffer[i] = 0.0f;
            }

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    i = x + (y * xDim);

                    if ((x >= ((xDim / 2) - 50)) && (x <= ((xDim / 2) + 50)) && (y >= (yDim - 101))) {
                        leftBuffer[i] = 50 + x - ((xDim / 2) - 50) + y - (yDim - 101);
                    }
                }
            }

            for (i = 0; i < length; i++) {
                rightBuffer[i] = 0.0f;
            }

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    i = x + (y * xDim);

                    if ((x >= ((xDim / 2) - 50)) && (x <= ((xDim / 2) + 50)) && (y >= 301) && (y <= 401)) {
                        rightBuffer[i] = 50 + x - ((xDim / 2) - 50) + y - 301;
                    }
                }
            }

            try {
                leftBWImage.importData(0, leftBuffer, true);
            } catch (IOException e) {
                leftBuffer = null;
                errorCleanUp("Algorithm StereoDepth: IOException on leftBWImage.importData", true);

                return;
            }

            try {
                rightBWImage.importData(0, rightBuffer, true);
            } catch (IOException e) {
                rightBuffer = null;
                errorCleanUp("Algorithm StereoDepth: IOException on rightBWImage.importData", true);

                return;
            }

        } else { // color

            // convert color to black and white
            try {
                colorBuffer = new float[4 * length];
            } catch (OutOfMemoryError e) {
                colorBuffer = null;
                errorCleanUp("Algorithm Stereo Depth: OutOfMemoryError on creating colorBuffer", true);

                return;
            }

            try {
                leftImage.exportData(0, 4 * length, colorBuffer);
            } catch (IOException e) {
                colorBuffer = null;
                errorCleanUp("Algorithm Stereo Depth: IOException on leftImage.exportData", true);

                return;
            }

            for (i = 0; i < length; i++) {
                leftBuffer[i] = (colorBuffer[(4 * i) + 1] + colorBuffer[(4 * i) + 2] + colorBuffer[(4 * i) + 3]) / 3.0f;
            }

            try {
                leftBWImage.importData(0, leftBuffer, true);
            } catch (IOException e) {
                leftBuffer = null;
                errorCleanUp("Algorithm StereoDepth: IOException on leftBWImage.importData", true);

                return;
            }

            try {
                rightImage.exportData(0, 4 * length, colorBuffer);
            } catch (IOException e) {
                colorBuffer = null;
                errorCleanUp("Algorithm Stereo Depth: IOException on rightImage.exportData", true);

                return;
            }

            for (i = 0; i < length; i++) {
                rightBuffer[i] = (colorBuffer[(4 * i) + 1] + colorBuffer[(4 * i) + 2] + colorBuffer[(4 * i) + 3]) /
                                     3.0f;
            }

            try {
                rightBWImage.importData(0, rightBuffer, true);
            } catch (IOException e) {
                rightBuffer = null;
                errorCleanUp("Algorithm StereoDepth: IOException on rightBWImage.importData", true);

                return;
            }
        }

        // Strip all pixels except those in a great circle
        // Find center of mass of left image
        num = 0;
        xCenter = 0.0f;
        yCenter = 0.0f;

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                i = x + (y * xDim);

                if (leftBuffer[i] > threshold) {
                    xCenter += x;
                    yCenter += y;
                    num++;
                }
            }
        }

        xCenter /= num;
        yCenter /= num;
        xCenInt = (int) (xCenter + 0.5f);
        yCenInt = (int) (yCenter + 0.5f);
        leftMask = new BitSet(length);

        for (i = 0; i < length; i++) {
            leftMask.clear(i);
        }

        doAgain = true;
        leftMask.set(xCenInt + (yCenInt * xDim));
        leftBoundary = xDim - 1;
        rightBoundary = 0;
        topBoundary = yDim - 1;
        bottomBoundary = 0;

        while (doAgain) {
            doAgain = false;

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    i = x + (y * xDim);

                    if ((leftBuffer[i] > threshold) && (!leftMask.get(i))) {

                        if ((x >= 1) && (leftMask.get(i - 1))) {
                            leftMask.set(i);
                            doAgain = true;

                            if (x > rightBoundary) {
                                rightBoundary = x;
                            }

                            if (x < leftBoundary) {
                                leftBoundary = x;
                            }

                            if (y > bottomBoundary) {
                                bottomBoundary = y;
                            }

                            if (y < topBoundary) {
                                topBoundary = y;
                            }
                        }

                        if ((x <= (xDim - 2)) && (leftMask.get(i + 1))) {
                            leftMask.set(i);
                            doAgain = true;

                            if (x > rightBoundary) {
                                rightBoundary = x;
                            }

                            if (x < leftBoundary) {
                                leftBoundary = x;
                            }

                            if (y > bottomBoundary) {
                                bottomBoundary = y;
                            }

                            if (y < topBoundary) {
                                topBoundary = y;
                            }
                        }

                        if ((y >= 1) && (leftMask.get(i - xDim))) {
                            leftMask.set(i);
                            doAgain = true;

                            if (x > rightBoundary) {
                                rightBoundary = x;
                            }

                            if (x < leftBoundary) {
                                leftBoundary = x;
                            }

                            if (y > bottomBoundary) {
                                bottomBoundary = y;
                            }

                            if (y < topBoundary) {
                                topBoundary = y;
                            }
                        }

                        if ((y <= (yDim - 2)) && (leftMask.get(i + xDim))) {
                            leftMask.set(i);
                            doAgain = true;

                            if (x > rightBoundary) {
                                rightBoundary = x;
                            }

                            if (x < leftBoundary) {
                                leftBoundary = x;
                            }

                            if (y > bottomBoundary) {
                                bottomBoundary = y;
                            }

                            if (y < topBoundary) {
                                topBoundary = y;
                            }
                        }
                    } // if ((leftBuffer[i] > threshold) && (!leftMask.get(i)))
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
        } // while (doAgain)

        xCenter = (leftBoundary + rightBoundary) / 2.0f;
        yCenter = (topBoundary + bottomBoundary) / 2.0f;
        radius = 0.5f * Math.min(rightBoundary - leftBoundary, bottomBoundary - topBoundary);
        radsq = radius * radius;

        for (i = 0; i < length; i++) {
            leftMask.clear(i);
        }

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                distsq = ((x - xCenter) * (x - xCenter)) + ((y - yCenter) * (y - yCenter));

                if (distsq <= radsq) {
                    leftMask.set(x + (y * xDim));
                }
            }
        }

        // Find center of mass of right image
        num = 0;
        xCenter = 0.0f;
        yCenter = 0.0f;

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                i = x + (y * xDim);

                if (rightBuffer[i] > threshold) {
                    xCenter += x;
                    yCenter += y;
                    num++;
                }
            }
        }

        xCenter /= num;
        yCenter /= num;
        xCenInt = (int) (xCenter + 0.5f);
        yCenInt = (int) (yCenter + 0.5f);
        rightMask = new BitSet(length);

        for (i = 0; i < length; i++) {
            rightMask.clear(i);
        }

        doAgain = true;
        rightMask.set(xCenInt + (yCenInt * xDim));
        leftBoundary = xDim - 1;
        rightBoundary = 0;
        topBoundary = yDim - 1;
        bottomBoundary = 0;

        while (doAgain) {
            doAgain = false;

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    i = x + (y * xDim);

                    if ((rightBuffer[i] > threshold) && (!rightMask.get(i))) {

                        if ((x >= 1) && (rightMask.get(i - 1))) {
                            rightMask.set(i);
                            doAgain = true;

                            if (x > rightBoundary) {
                                rightBoundary = x;
                            }

                            if (x < leftBoundary) {
                                leftBoundary = x;
                            }

                            if (y > bottomBoundary) {
                                bottomBoundary = y;
                            }

                            if (y < topBoundary) {
                                topBoundary = y;
                            }
                        }

                        if ((x <= (xDim - 2)) && (rightMask.get(i + 1))) {
                            rightMask.set(i);
                            doAgain = true;

                            if (x > rightBoundary) {
                                rightBoundary = x;
                            }

                            if (x < leftBoundary) {
                                leftBoundary = x;
                            }

                            if (y > bottomBoundary) {
                                bottomBoundary = y;
                            }

                            if (y < topBoundary) {
                                topBoundary = y;
                            }
                        }

                        if ((y >= 1) && (rightMask.get(i - xDim))) {
                            rightMask.set(i);
                            doAgain = true;

                            if (x > rightBoundary) {
                                rightBoundary = x;
                            }

                            if (x < leftBoundary) {
                                leftBoundary = x;
                            }

                            if (y > bottomBoundary) {
                                bottomBoundary = y;
                            }

                            if (y < topBoundary) {
                                topBoundary = y;
                            }
                        }

                        if ((y <= (yDim - 2)) && (rightMask.get(i + xDim))) {
                            rightMask.set(i);
                            doAgain = true;

                            if (x > rightBoundary) {
                                rightBoundary = x;
                            }

                            if (x < leftBoundary) {
                                leftBoundary = x;
                            }

                            if (y > bottomBoundary) {
                                bottomBoundary = y;
                            }

                            if (y < topBoundary) {
                                topBoundary = y;
                            }
                        }
                    } // if ((rightBuffer[i] > threshold) && (!rightMask.get(i)))
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
        } // while (doAgain)

        xCenter = (leftBoundary + rightBoundary) / 2.0f;
        yCenter = (topBoundary + bottomBoundary) / 2.0f;
        radius = 0.5f * Math.min(rightBoundary - leftBoundary, bottomBoundary - topBoundary);
        radsq = radius * radius;

        for (i = 0; i < length; i++) {
            rightMask.clear(i);
        }

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                distsq = ((x - xCenter) * (x - xCenter)) + ((y - yCenter) * (y - yCenter));

                if (distsq <= radsq) {
                    rightMask.set(x + (y * xDim));
                }
            }
        }

        // Register the right image to the left image
        /* leftRefImage = new ModelImage(ModelImage.FLOAT, leftImage.getExtents(),
         * leftImage.getImageName()+"_ref", leftImage.getUserInterface()); for (i = 0; i < length; i++) { if
         * (leftMask.get(i)){ leftBuffer[i] = 1.0f; } else { leftBuffer[i] = 0.0f; } }*/

        leftBoolean = new boolean[length];

        for (i = 0; i < length; i++) {
            leftBoolean[i] = false;
        }

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                i = x + (y * xDim);

                if ((x >= ((xDim / 2) - 50)) && (x <= ((xDim / 2) + 50)) && (y >= (yDim - 101))) {
                    leftBoolean[i] = true;
                }
            }
        }

        /* try {
         * leftRefImage.importData(0, leftBuffer, true); } catch(IOException e) { leftBuffer = null;
         * errorCleanUp("Algorithm StereoDepth: IOException on leftRefImage.importData", true); return; }
         *
         * updateSomeFileInfo(leftImage,leftRefImage);
         *
         * rightRefImage = new ModelImage(ModelImage.FLOAT, rightImage.getExtents(), rightImage.getImageName()+"_ref",
         * rightImage.getUserInterface());
         *
         * for (i = 0; i < length; i++) { if (rightMask.get(i)){ rightBuffer[i] = 1.0f; } else { rightBuffer[i] = 0.0f; }
         * }*/

        rightBoolean = new boolean[length];

        for (i = 0; i < length; i++) {
            rightBoolean[i] = false;
        }

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                i = x + (y * xDim);

                if ((x >= ((xDim / 2) - 50)) && (x <= ((xDim / 2) + 50)) && (y >= 301) && (y <= 401)) {
                    rightBoolean[i] = true;
                }
            }
        }

        /* try {
         * rightRefImage.importData(0, rightBuffer, true); } catch(IOException e) { rightBuffer = null;
         * errorCleanUp("Algorithm StereoDepth: IOException on rightRefImage.importData", true); return; }
         *
         * updateSomeFileInfo(rightImage,rightRefImage);
         *
         * try { leftRefImage.importData(0, leftBuffer, true); } catch(IOException e) { leftBuffer = null;
         * errorCleanUp("Algorithm Stereo Depth: IOException on leftRefImage.importData", true); return; }
         *
         * try { rightRefImage.importData(0, rightBuffer, true); } catch(IOException e) { rightBuffer = null;
         * errorCleanUp("Algorithm Stereo Depth: IOException on rightRefImage.importData", true); return; }*/

        try {

            boolean entireTarget = false;
            boolean entireSource = false;
            int transformation = AlgorithmRegAIR.RIGID_BODY2D;
            int interpolation = AlgorithmTransform.BILINEAR;
            float sourceThreshold = 40.0f;
            float targetThreshold = 40.0f;
            float sourceGaussX = 0.0f;
            float sourceGaussY = 0.0f;
            float sourceGaussZ = 0.0f;
            float targetGaussX = 0.0f;
            float targetGaussY = 0.0f;
            float targetGaussZ = 0.0f;
            int sourcePartitions = 1;
            int targetPartitions = 1;
            int costFxn = 2;
            boolean cubicInterpolation = false;
            boolean interaction = false;
            float precision = 1.0e-10f;
            int iterations = 1000;

            air = new AlgorithmRegAIR(destImage, leftBWImage, leftBoolean, rightBWImage, rightBoolean, transformation,
                                      interpolation, sourceThreshold, targetThreshold, sourceGaussX, sourceGaussY,
                                      sourceGaussZ, targetGaussX, targetGaussY, targetGaussZ, sourcePartitions,
                                      targetPartitions, costFxn, cubicInterpolation, interaction, precision,
                                      iterations);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("AlgorithmStereoDepth: unable to allocate enough memory");

            return;
        }

        air.run();

        /* rigidReg = new AlgorithmRegOAR2D(leftBWImage, rightBWImage,leftRefImage, rightRefImage,
         * AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED_WGT, dof, AlgorithmTransform.BILINEAR, coarseBegin,
         * coarseEnd, coarseRate, fineBegin, fineEnd, fineRate); rigidReg.setProgressBarVisible(false); rigidReg.run();
         * int xdimA   = leftImage.getExtents()[0]; int ydimA   = leftImage.getExtents()[1]; float xresA =
         * leftImage.getFileInfo(0).getResolutions()[0]; float yresA = leftImage.getFileInfo(0).getResolutions()[1];
         *
         * AlgorithmTransform transform = null; TransMatrix xfrm = rigidReg.getTransform(); double A[][] =
         * xfrm.getMatrix();
         *
         * for (i = 0; i < 3; i++) { for (int j = 0; j < 3; j++) { System.out.println("A["+i+"]["+j+"]= " + A[i][j]); } }
         *
         * transform = new AlgorithmTransform(rightBWImage, rigidReg.getTransform(), AlgorithmTransform.BILINEAR, xresA,
         * yresA, xdimA, ydimA, false, false);
         *
         *
         * transform.run(); rightBWImage = transform.getTransformedImage(); try {
         * rightBWImage.exportData(0,length,rightBuffer); } catch (IOException e) { rightBuffer = null;
         * errorCleanUp("Algorithm Stereo Depth: IOException on rightBWImage.exportData", true); return; }
         *
         * try { destImage.importData(0,rightBuffer,true); } catch(IOException e) { rightBuffer = null;
         * errorCleanUp("Algorithm StereoDepth: IOException on destImage.importData", true); return; } */

        setCompleted(true);

    }
}
