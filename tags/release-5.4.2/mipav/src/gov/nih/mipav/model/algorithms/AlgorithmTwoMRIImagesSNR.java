package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.IOException;
import java.text.DecimalFormat;


/**
 * This module implements the National Electrical Manufacturers Association (NEMA) standard for SNR estimation in
 * diagnostic MRI. This method requires a perfect geometrical registration of 2 MRI images scanned sequentailly with
 * less than five minutes elapsed time from the end of the first scan to the beginning of the second scan. A simple 2
 * step process is used: 1.) Determine the standard deviation of the difference image over the same VOI region used for
 * the signal mean difference variance = (1/(VOI pixel number - 1)) * sum over VOI region of (image[i] - image2[i])**2
 * difference standard deviation = square root(difference variance)
 * 
 * <p>
 * 2.) SNR for a signal VOI = square root(2) * (voi mean)/difference standard deviation The factor square root(2) is
 * required because the standard deviation used is from the difference image rather than one of the original images The
 * contrast to noise ratio is simply SNR for VOI 1 - SNR for VOI 2.
 * </p>
 * 
 * <p>
 * An optional registration may be performed before SNR. In this registration image2 is registered to image.
 * AlgorithmRegOAR2D is used with the cost function being the only registration parameter the user can vary in the
 * dialog box. Correlation ratio is the default cost function, but the user can also select least squares, normalized
 * cross correlation, or normalized mutual information. The SNR will be performed with the registered image2 rather than
 * on the original prebleached image.
 * </p>
 * 
 * <p>
 * Note that even for 2 perfectly aligned images the results differ substantially for processing without and with
 * registration. In 1 example for 2 aligned images without registration the standard deviation equalled 12.3 and the SNR
 * equalled 8.44. With registration the standard deviation equalled 7.66 and the SNR equalled 13.6.
 * </p>
 * 
 * <p>
 * The code here assumes the presence of 1 or 2 VOI regions - a required signal region and optional second signal
 * region. The VOIs must all be placed in the the same image. Radio buttons in the dialog box are used to select a red
 * signal 1 or a green signal 2 VOI. Either an ellipse VOI, rectangle VOI, or polyline VOI will be selected from the top
 * MIPAV toolbar. There is no need to hit the NEW_VOI button.
 * </p>
 * 
 * <p>
 * Reference: 1.) PhD. Thesis Signal and Noise Estimation From Magnetic Resonance Images by Jan Sijbers, Universiteit
 * Antwerpen, Department Natuurkunde, Section 7.1.2 The NEMA standard, p. 49.
 * </p>
 * 
 * <p>
 * 2.) Acceptance Testing of Magnetic Resonance Imaging Systems, AAPM Report No. 34, Published by the American
 * Assoication of Physicists in Medicine by the American Institute of Physics, March, 1992 (Reprinted from Medical
 * Physics, Vol. 19, Issue 1, 1992).
 * </p>
 */
public class AlgorithmTwoMRIImagesSNR extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private boolean createRegImage;

    /** private ModelImage srcImage;. */
    private ModelImage image2;

    /** DOCUMENT ME! */
    private boolean register;

    /** DOCUMENT ME! */
    private int signal2Index;

    /** DOCUMENT ME! */
    private int signalImage;

    /** DOCUMENT ME! */
    private int signalIndex;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmTwoMRIImagesSNR object.
     * 
     * @param image First MRI image
     * @param image2 Second MRI image
     * @param signalIndex the index of the signal VOI
     * @param signalImage image the signal VOI belongs to; 1 if image, 2 if image 2
     * @param signal2Index the index of the second signal VOI if >= 0
     * @param register If true register the image2 to the image before SNR
     * @param cost Cost function used in registration
     * @param createRegImage If register = true and createRegImage = true, then create a frame with the registered image
     */
    public AlgorithmTwoMRIImagesSNR(ModelImage image, ModelImage image2, int signalIndex, int signalImage,
            int signal2Index, boolean register, int cost, boolean createRegImage) {

        super(null, image);
        this.image2 = image2;
        this.signalIndex = signalIndex;
        this.signalImage = signalImage;
        this.signal2Index = signal2Index;
        this.register = register;
        this.cost = cost;
        this.createRegImage = createRegImage;
        UI = ViewUserInterface.getReference();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {
        int i;
        int xDim, yDim, sliceSize;
        int zDim;
        int imageLength;
        float[] floatBuffer;
        float[] floatBuffer2;
        ModelImage regImage = null;

        VOIVector VOIs = null;
        VOIVector VOIsReg = null;
        AlgorithmRegOAR2D regAlgo = null;
        AlgorithmRegOAR3D regAlgo3D = null;
        AlgorithmTransform transform = null;
        short[] mask;

        int[] srcExtents = new int[2];

        VOI inVOI;
        float difference;
        double differenceVar = 0.0;
        double differenceStdDev;
        float mean = 0.0f;
        int meanCount = 0;
        float mean2 = 0.0f;
        int mean2Count = 0;
        double snr;
        double snr2;
        double cnr;
        DecimalFormat nf;

        if (srcImage == null) {
            displayError("image is null");

            return;
        }

        if (image2 == null) {
            displayError("image2 is null");

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Performing Two MRI Image SNR ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        srcExtents[0] = xDim;
        srcExtents[1] = yDim;
        sliceSize = xDim * yDim;

        zDim = 1;

        if (srcImage.getNDims() > 2) {
            zDim = srcImage.getExtents()[2];
        }

        imageLength = sliceSize * zDim;
        floatBuffer = new float[imageLength];
        floatBuffer2 = new float[imageLength];

        nf = new DecimalFormat("0.00E0");

        VOIs = srcImage.getVOIs();

        if (signalImage == 1) {
            inVOI = (VOI) VOIs.VOIAt(signalIndex).clone();
            VOIsReg = new VOIVector();
            VOIsReg.addElement(inVOI);
        }

        if (signal2Index >= 0) {

            if (signalImage == 1) {
                inVOI = (VOI) VOIs.VOIAt(signal2Index).clone();
                VOIsReg.addElement(inVOI);
            }
        } // if (signal2Index >= 0)

        if (register) {
            fireProgressStateChanged("Registering images");

            int DOF; // rigid transformation
            int interp = AlgorithmTransform.BILINEAR;
            int interp2 = AlgorithmTransform.BILINEAR;
            float rotateBegin = -3.0f;
            float rotateEnd = 3.0f;
            float coarseRate = 3.0f;
            float fineRate = 1.0f;
            float rotateBeginX = -3.0f;
            float rotateEndX = 3.0f;
            float coarseRateX = 3.0f;
            float fineRateX = 1.0f;
            float rotateBeginY = -3.0f;
            float rotateEndY = 3.0f;
            float coarseRateY = 3.0f;
            float fineRateY = 1.0f;
            float rotateBeginZ = -3.0f;
            float rotateEndZ = 3.0f;
            float coarseRateZ = 3.0f;
            float fineRateZ = 1.0f;
            boolean maxResol = true;
            boolean doSubsample;
            boolean doMultiThread;
            boolean fastMode = false;
            int maxIterations = 2;
            int numMinima = 3;
            boolean transformVOI;

            if (signalImage == 2) {
                transformVOI = true;
            } else {
                transformVOI = false;
            }

            if (srcImage.getNDims() == 2) {
                DOF = 3;
                doSubsample = false;
                doMultiThread = true;
                interp = AlgorithmTransform.BILINEAR;
                interp2 = AlgorithmTransform.BILINEAR;
                regAlgo = new AlgorithmRegOAR2D(srcImage, image2, cost, DOF, interp, rotateBegin, rotateEnd,
                        coarseRate, fineRate, doSubsample, doMultiThread, maxIterations, numMinima);
                regAlgo.run();
                transform = new AlgorithmTransform(image2, regAlgo.getTransform(), interp2, srcImage.getFileInfo()[0]
                        .getResolutions()[0], srcImage.getFileInfo()[0].getResolutions()[1], srcImage.getExtents()[0],
                        srcImage.getExtents()[1], new int[] {srcImage.getUnitsOfMeasure(0),
                                srcImage.getUnitsOfMeasure(1)}, transformVOI, false, false);
                regAlgo.finalize();
                regAlgo = null;
            } // if (srcImage.getNDims() == 2)
            else if (srcImage.getNDims() == 3) {
                DOF = 6;
                doSubsample = true;
                doMultiThread = true;
                interp = AlgorithmTransform.TRILINEAR;
                interp2 = AlgorithmTransform.TRILINEAR;
                regAlgo3D = new AlgorithmRegOAR3D(srcImage, image2, cost, DOF, interp, rotateBeginX, rotateEndX,
                        coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                        rotateEndZ, coarseRateZ, fineRateZ, maxResol, doSubsample, doMultiThread, fastMode,
                        maxIterations, numMinima);
                regAlgo3D.run();
                transform = new AlgorithmTransform(image2, regAlgo3D.getTransform(), interp2, srcImage.getFileInfo()[0]
                        .getResolutions()[0], srcImage.getFileInfo()[0].getResolutions()[1], srcImage.getFileInfo()[0]
                        .getResolutions()[2], srcImage.getExtents()[0], srcImage.getExtents()[1],
                        srcImage.getExtents()[2], transformVOI, false, false);
                regAlgo3D.finalize();
                regAlgo3D = null;
            } // else if (srcImagre.getNDims() == 3)

            transform.setUpdateOriginFlag(true);
            transform.run();
            regImage = transform.getTransformedImage();
            transform.finalize();

            if (createRegImage) {

                regImage.setImageName(image2.getImageName() + "_registered");
                regImage.calcMinMax();

                if (signalImage == 1) {
                    regImage.setVOIs(VOIsReg);
                }

                new ViewJFrameImage(regImage);
            } // if (createRegImage)
        } // if (register)

        try {
            srcImage.exportData(0, imageLength, floatBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.export(0, imageLength, floatBuffer)");

            setCompleted(false);

            return;
        }

        if (register) {

            try {
                regImage.exportData(0, imageLength, floatBuffer2);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on  regImage.export(0, imageLength, floatBuffer2)");

                setCompleted(false);

                return;
            }
        } // if (register)
        else {

            try {
                image2.exportData(0, imageLength, floatBuffer2);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on image2.export(0, imageLength, floatBuffer2)");

                setCompleted(false);

                return;
            }
        } // else

        fireProgressStateChanged("Cacluating snr");
        mask = new short[imageLength];

        for (i = 0; i < mask.length; i++) {
            mask[i] = -1;
        }

        if (register) {
            mask = regImage.generateVOIMask(mask, signalIndex);
        } else if (signalImage == 1) {
            mask = srcImage.generateVOIMask(mask, signalIndex);
        } else {
            mask = image2.generateVOIMask(mask, signalIndex);
        }

        if (signal2Index >= 0) {

            if (register) {
                mask = regImage.generateVOIMask(mask, signal2Index);
            } else if (signalImage == 1) {
                mask = srcImage.generateVOIMask(mask, signal2Index);
            } else {
                mask = image2.generateVOIMask(mask, signal2Index);
            }
        } // if (signal2Index == 0)

        for (i = 0; i < imageLength; i++) {

            if (mask[i] == signalIndex) {
                mean += floatBuffer[i];
                meanCount++;
                difference = floatBuffer[i] - floatBuffer2[i];
                differenceVar = differenceVar + (difference * difference);
            }

            if (signal2Index >= 0) {

                if (mask[i] == signal2Index) {
                    mean2 += floatBuffer[i];
                    mean2Count++;
                }
            } // if (signal2Index >= 0)
        } // for (i = 0; i < imageLength; i++)

        differenceVar = differenceVar / ((double) meanCount - 1.0);
        differenceStdDev = Math.sqrt(differenceVar);
        Preferences.debug("Image difference standard deviation = " + nf.format(differenceStdDev) + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        UI.setDataText("Image difference standard deviation = " + nf.format(differenceStdDev) + "\n");

        mean = mean / meanCount;
        snr = Math.sqrt(2.0) * mean / differenceStdDev;
        Preferences.debug("SNR for signal 1 VOI = " + nf.format(snr) + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        UI.setDataText("SNR for signal 1 VOI = " + nf.format(snr) + "\n");

        if (signal2Index >= 0) {
            mean2 = mean2 / mean2Count;
            snr2 = Math.sqrt(2.0) * mean2 / differenceStdDev;
            Preferences.debug("SNR for signal 2 VOI = " + nf.format(snr2) + "\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("SNR for signal 2 VOI = " + nf.format(snr2) + "\n");
            cnr = snr - snr2;
            Preferences.debug("Contrast to noise ratio for 1 - 2 = " + nf.format(cnr) + "\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("Contrast to noise ratio for 1 - 2 = " + nf.format(cnr) + "\n");
        } // if (signal2Index >= 0)

        setCompleted(true);

        return;
    }
}
