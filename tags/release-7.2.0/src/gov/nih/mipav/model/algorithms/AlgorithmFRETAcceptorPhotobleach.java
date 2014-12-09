package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;


/**
 * Fluorescence Resonance Energy Transfer FRET refers to the nonradiative transfer of energy from an excited state donor
 * fluorescent molecule to a nearby acceptor fluorescent molecule. The energy transfer efficiency E, defined as the
 * number of energy transfer events divided by the number of photons absorbed by the donor, is related to the distance R
 * between the acceptor and donor by: E = 1/[1 + (r/R0)**6] (eq. 1) where R0, the Forster critical distance, is the
 * distance at which E = 0.5. We also have E = 1 - FDA/FD, (eq. 2) where FDA is the donor fluorescence in the presence
 * of an acceptor and FD is the donor fluorescence in the absence of the acceptor. The equivalent of an acceptor's
 * absence can be created by photobleaching the acceptor. Thus, a method using equation 2, can be performed on a single
 * sample by measuring the donor fluorescence before and after photobleaching the acceptor molecules.
 *
 * <p>Because FRET falls off as the sixth power of the distance between the donor and the acceptor, no FRET occurs for
 * distances greater than 2R0. Since R0 is on the order of 10 to 70 Angstroms, by performing FRET measurements it is
 * possible to distinguish proteins that are merely nearby in the same compartment from those proteins that are
 * interacting with each other.</p>
 *
 * <p>This program compares the fluorescence of two 2D images or of two 2D slices inside a 2 slice 3D image - an image
 * before acceptor photobleaching and an image after acceptor photobleaching. In the 3D image the prebleached slice is
 * required to be the first slice and the postbleached slice is required to be the second slice. Before acceptor
 * photobleaching, the donor fluorescence is quenched by FRET with the unbleached acceptor. After destroying the
 * acceptor by photobleaching, the donor fluorescence will increase. Only 1 color will be used from a color image.</p>
 *
 * <p>An optional registration may be performed before FRET. In this registration the prebleached image is registered to
 * the postbleached image. AlgorithmRegOAR2D is used with the cost function being the only registration parameter the
 * user can vary in the dialog box. Correlation ratio is the default cost function, but the user can also select least
 * squares, normalized cross correlation, or normalized mutual information. The FRET will be performed with the
 * registered prebleached image rather than on the original prebleached image. If the image is a color image, the
 * selected color will determine the transformations for all the colors.</p>
 *
 * <p>The code here assumes the presence of 1 or 2 or 3 VOI regions - a required donor region and optional background
 * region and optional signal normalization region. The VOIs must all be placed in the postbleached image. Radio buttons
 * in the dialog box are used to select a red photobleached or a blue background VOI or a green signal VOI. Either an
 * ellipse VOI, rectangle VOI, or polyline VOI will be selected from the top MIPAV toolbar. There is no need to hit the
 * NEW_VOI button. The background region will have a smaller average intensity than the donor region.</p>
 *
 * <p>If a background VOI is present, the average of the prebleached background is subtracted from the average of the
 * prebleached donor region and the average of the postbleached background is subtracted is subtracted from the average
 * of the postbleached donor region. Then, the energy transfer efficiency is calculated as (background subtracted
 * postbleached donor intensity - (background subtracted prebleached donor intensity)/ background subtracted
 * postbleached donor intensity</p>
 *
 * <p>A signal VOI cannot be used unless a background VOI is also present. Let b1 and s1 be the prebleached values and
 * b2 and s2 be the postbleached values. The following equation is used to convert a donor value from the prebleached
 * image into a donor value on the postbleached image: postbleached = ((s2 - b2)/(s1 - b1)) * prebleached + (b2*s1 -
 * b1*s2)/(s1 - b1)</p>
 *
 * <p>Reference: 1.) "Imaging Protein-Protein Interactions Using Fluorescence Energy Transfer Microscopy" by Anne K.
 * Kenworthy, Methods, 24, 2001, pp. 289 - 296.</p>
 *
 * <p>2.) "Quantitative Fluorescence Resonance Energy Transfer Measurements Using Fluorescence Microscopy" by Gerald W.
 * Gordon, Gail Berry, Xiao Huan Liang, Beth Levine, and Brian Herman, Biophysical Journal, Volume 74, May, 1998, pp.
 * 2702-2713.</p>
 */
public class AlgorithmFRETAcceptorPhotobleach extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int backgroundIndex;

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private boolean createRegImage;

    /** DOCUMENT ME! */
    private int donorIndex;

    /** private ModelImage srcImage; // image before acceptor photobleaching. */
    private ModelImage postImage; // image after acceptor photobleaching

    /** DOCUMENT ME! */
    private boolean register;

    /** DOCUMENT ME! */
    private int signalIndex;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmFRETAcceptorPhotobleach object.
     *
     * @param  preImage         image before acceptor photobleaching or 2 slice 3D image
     * @param  postImage        image after acceptor photobleaching null if 3D image is used
     * @param  useRed           If true, use the red color values for the FRET
     * @param  useGreen         If true, use the green color values for the FRET
     * @param  useBlue          If true, use the blue color values for the FRET
     * @param  donorIndex       the index of the donor VOI
     * @param  backgroundIndex  the index of the background VOI if >= 0
     * @param  signalIndex      the index of the signal normalization VOI if >= 0
     * @param  register         If true register the preImage to the postImage before FRET
     * @param  cost             Cost function used in registration
     * @param  createRegImage   If register = true and createRegImage = true, then create a frame with the registered
     *                          image
     */
    public AlgorithmFRETAcceptorPhotobleach(ModelImage preImage, ModelImage postImage, boolean useRed, boolean useGreen,
                                            boolean useBlue, int donorIndex, int backgroundIndex, int signalIndex,
                                            boolean register, int cost, boolean createRegImage) {

        super(null, preImage);
        this.postImage = postImage;
        this.useRed = useRed;
        this.useGreen = useGreen;
        this.useBlue = useBlue;
        this.donorIndex = donorIndex;
        this.backgroundIndex = backgroundIndex;
        this.signalIndex = signalIndex;
        this.register = register;
        this.cost = cost;
        this.createRegImage = createRegImage;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
        int c = 0;
        int c2 = 0;
        int c3 = 0;
        int xDim, yDim, sliceSize;
        float[] floatBuffer;

        // black and white image created from selected color of color image
        ModelImage bwImage = null;
        ModelImage bwImage2 = null;
        ModelImage bwImage3 = null;
        ModelImage bwPostImage = null;
        ModelImage colorImageReg = null;
        VOIVector VOIs = null;
        VOIVector VOIs2 = null;
        AlgorithmRegOAR2D regAlgo = null;
        AlgorithmTransform transform = null;
        short[] mask;
        int preDonorCount;
        int preBackgroundCount;
        int preSignalCount;
        float preDonorIntensity;
        float correctedPreDonorIntensity;
        float preBackgroundIntensity;
        float preSignalIntensity;
        int postDonorCount;
        int postBackgroundCount;
        int postSignalCount;
        float postDonorIntensity;
        float correctedPostDonorIntensity;
        float postBackgroundIntensity;
        float postSignalIntensity;
        float preSignalRange = 0.0f;
        float postSignalRange;
        float efficiency;

        double minR, minG, minB;
        double maxR, maxG, maxB;
        boolean haveRed = false;
        boolean haveGreen = false;
        boolean haveBlue = false;
        int colorsPresent = 0;
        int[] srcExtents = new int[2];
        int nVOI;
        VOI inVOI, outVOI;
        Color voiColor;

        if (srcImage == null) {
            displayError("Prebleached Image is null");

            return;
        }

        if ((srcImage.getNDims() == 2) && (postImage == null)) {
            displayError("Postbleached Image is null");

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Performing FRET ...");

        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        srcExtents[0] = xDim;
        srcExtents[1] = yDim;
        sliceSize = xDim * yDim;
        floatBuffer = new float[sliceSize];

        if (srcImage.getNDims() == 2) {
            VOIs = postImage.getVOIs();
        } else {
            VOIs = srcImage.getVOIs();
            nVOI = VOIs.size();
            VOIs2 = new VOIVector();

            for (i = 0; i < nVOI; i++) {
                inVOI = (VOI) VOIs.VOIAt(i).clone();
                voiColor = inVOI.getColor();
                outVOI = new VOI(inVOI.getID(), inVOI.getName(), inVOI.getCurveType(), -1.0f);
                outVOI.setColor(voiColor);
                outVOI.importNewVOI(0, 1, inVOI, true);
                VOIs2.addElement(outVOI);
            }
        }

        // Create black and white image using only the selected color
        if (srcImage.isColorImage()) {
            fireProgressStateChanged("Creating black and white image");
            minR = srcImage.getMinR();
            maxR = srcImage.getMaxR();

            if (minR != maxR) {
                haveRed = true;
            }

            minG = srcImage.getMinG();
            maxG = srcImage.getMaxG();

            if (minG != maxG) {
                haveGreen = true;
            }

            minB = srcImage.getMinB();
            maxB = srcImage.getMaxB();

            if (minB != maxB) {
                haveBlue = true;
            }

            colorsPresent = 0;

            if (haveRed) {
                colorsPresent++;
            }

            if (haveGreen) {
                colorsPresent++;
            }

            if (haveBlue) {
                colorsPresent++;
            }

            bwImage = new ModelImage(ModelStorageBase.FLOAT, srcExtents, srcImage.getImageName() + "_bw");

            if (postImage != null) {
                bwPostImage = new ModelImage(ModelStorageBase.FLOAT, srcExtents, postImage.getImageName() + "_bw");
            } else {
                bwPostImage = new ModelImage(ModelStorageBase.FLOAT, srcExtents, srcImage.getImageName() + "_postbw");
            }

            if (createRegImage && (colorsPresent >= 2)) {
                bwImage2 = new ModelImage(ModelStorageBase.FLOAT, srcExtents, srcImage.getImageName() + "_bw2");
                colorImageReg = new ModelImage(srcImage.getType(), srcExtents, srcImage.getImageName() + "_registered");
            }

            if (createRegImage && (colorsPresent == 3)) {
                bwImage3 = new ModelImage(ModelStorageBase.FLOAT, srcExtents, srcImage.getImageName() + "_bw3");
            }

            if (useRed) {
                c = 1;
            } else if (useGreen) {
                c = 2;
            } else {
                c = 3;
            }

            try {
                srcImage.exportRGBData(c, 0, sliceSize, floatBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData");
                setCompleted(false);

                return;
            }

            try {
                bwImage.importData(0, floatBuffer, false);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on bwImage.importData");
                setCompleted(false);

                return;
            }

            bwImage.calcMinMax();

            if (srcImage.getNDims() == 2) {

                try {
                    postImage.exportRGBData(c, 0, sliceSize, floatBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on postImage.exportRGBData");
                    setCompleted(false);

                    return;
                }
            } // if (srcImage.getNDims() == 2)
            else { // slice 3D image

                try {
                    srcImage.exportRGBData(c, 4 * sliceSize, sliceSize, floatBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData");
                    setCompleted(false);

                    return;
                }

            }

            try {
                bwPostImage.importData(0, floatBuffer, false);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on bwPostImage.importData");
                setCompleted(false);

                return;
            }

            bwPostImage.calcMinMax();

            if (srcImage.getNDims() == 2) {
                bwPostImage.setVOIs(VOIs);
            } else {
                bwPostImage.setVOIs(VOIs2);
            }

            if (createRegImage && !useRed && haveRed) {
                c2 = 1;
            } else if (createRegImage && !useGreen && haveGreen) {
                c2 = 2;
            } else if (createRegImage && !useBlue && haveBlue) {
                c2 = 3;
            }

            if (c2 > 0) {

                try {
                    srcImage.exportRGBData(c2, 0, sliceSize, floatBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData");
                    setCompleted(false);

                    return;
                }

                try {
                    bwImage2.importData(0, floatBuffer, true);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on bwImage2.importData");
                    setCompleted(false);

                    return;
                }

                if ((c2 == 1) && !useGreen && haveGreen) {
                    c3 = 2;
                } else if (((c2 == 1) || (c2 == 2)) && !useBlue && haveBlue) {
                    c3 = 3;
                }
            } // if (c2 > 0)

            if (c3 > 0) {

                try {
                    srcImage.exportRGBData(c3, 0, sliceSize, floatBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData");
                    setCompleted(false);

                    return;
                }

                try {
                    bwImage3.importData(0, floatBuffer, true);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on bwImage3.importData");
                    setCompleted(false);

                    return;
                }
            } // if (c3 > 0)
        } // if (srcImage.isColorImage())
        else {

            if (srcImage.getNDims() == 2) {
                bwImage = new ModelImage(ModelStorageBase.FLOAT, srcExtents, srcImage.getImageName() + "_bw");

                try {
                    srcImage.exportData(0, sliceSize, floatBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
                    setCompleted(false);

                    return;
                }

                try {
                    bwImage.importData(0, floatBuffer, true);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on bwImage.importData");
                    setCompleted(false);

                    return;
                }

                bwPostImage = new ModelImage(ModelStorageBase.FLOAT, srcExtents, srcImage.getImageName() + "_postbw");

                try {
                    postImage.exportData(0, sliceSize, floatBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
                    setCompleted(false);

                    return;
                }

                try {
                    bwPostImage.importData(0, floatBuffer, true);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on bwPostImage.importData");
                    setCompleted(false);

                    return;
                }

                bwPostImage.setVOIs(VOIs);
            } // if (srcImage.getNDims() == 2)
            else { // 2 slice 3D image
                bwImage = new ModelImage(ModelStorageBase.FLOAT, srcExtents, srcImage.getImageName() + "_bw");

                try {
                    srcImage.exportData(0, sliceSize, floatBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
                    setCompleted(false);

                    return;
                }

                try {
                    bwImage.importData(0, floatBuffer, true);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on bwImage.importData");
                    setCompleted(false);

                    return;
                }

                bwPostImage = new ModelImage(ModelStorageBase.FLOAT, srcExtents, srcImage.getImageName() + "_postbw");

                try {
                    srcImage.exportData(sliceSize, sliceSize, floatBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
                    setCompleted(false);

                    return;
                }

                try {
                    bwPostImage.importData(0, floatBuffer, true);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on bwPostImage.importData");
                    setCompleted(false);

                    return;
                }

                bwPostImage.setVOIs(VOIs2);
            } // 2 slice 3D image
        }

        if (register) {
            fireProgressStateChanged("Registering images");

            int DOF = 3; // rigid transformation
            int interp = AlgorithmTransform.BILINEAR;
            int interp2 = AlgorithmTransform.BILINEAR;
            float rotateBegin = -3.0f;
            float rotateEnd = 3.0f;
            float coarseRate = 3.0f;
            float fineRate = 1.0f;
            boolean doSubsample = false;
            boolean doMultiThread = true;
            int maxIterations = 2;
            int numMinima = 3;

            regAlgo = new AlgorithmRegOAR2D(bwPostImage, bwImage, cost, DOF, interp, rotateBegin, rotateEnd, coarseRate,
                                            fineRate, doSubsample, doMultiThread, maxIterations, numMinima);
            regAlgo.run();
            transform = new AlgorithmTransform(bwImage, regAlgo.getTransform(), interp2,
                                               bwPostImage.getFileInfo()[0].getResolutions()[0],
                                               bwPostImage.getFileInfo()[0].getResolutions()[1],
                                               bwPostImage.getExtents()[0], bwPostImage.getExtents()[1], false, false,
                                               false);

            transform.setUpdateOriginFlag(true);
            transform.run();
            bwImage = transform.getTransformedImage();
            transform.finalize();

            if (colorsPresent >= 2) {
                transform = new AlgorithmTransform(bwImage2, regAlgo.getTransform(), interp2,
                                                   bwPostImage.getFileInfo()[0].getResolutions()[0],
                                                   bwPostImage.getFileInfo()[0].getResolutions()[1],
                                                   bwPostImage.getExtents()[0], bwPostImage.getExtents()[1], false,
                                                   false, false);

                transform.setUpdateOriginFlag(true);
                transform.run();
                bwImage2 = transform.getTransformedImage();
                transform.finalize();
            } // if (colorsPresent >= 2)

            if (colorsPresent == 3) {
                transform = new AlgorithmTransform(bwImage3, regAlgo.getTransform(), interp2,
                                                   bwPostImage.getFileInfo()[0].getResolutions()[0],
                                                   bwPostImage.getFileInfo()[0].getResolutions()[1],
                                                   bwPostImage.getExtents()[0], bwPostImage.getExtents()[1], false,
                                                   false, false);

                transform.setUpdateOriginFlag(true);
                transform.run();
                bwImage3 = transform.getTransformedImage();
                transform.finalize();
            } // if (colorsPresent == 3)

            regAlgo.finalize();
            regAlgo = null;

            if (createRegImage) {

                if (colorsPresent >= 2) {

                    try {
                        bwImage.exportData(0, sliceSize, floatBuffer);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException " + e + " on bwImage.exportData");
                        setCompleted(false);

                        return;
                    }

                    try {
                        colorImageReg.importRGBData(c, 0, floatBuffer, false);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException " + e + " on colorImageReg.importRGBData");
                        setCompleted(false);

                        return;
                    }

                    try {
                        bwImage2.exportData(0, sliceSize, floatBuffer);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException " + e + " on bwImage2.exportData");
                        setCompleted(false);

                        return;
                    }

                    try {
                        colorImageReg.importRGBData(c2, 0, floatBuffer, false);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException " + e + " on colorImageReg.importRGBData");
                        setCompleted(false);

                        return;
                    }

                    if (colorsPresent == 3) {

                        try {
                            bwImage3.exportData(0, sliceSize, floatBuffer);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException " + e + " on bwImage3.exportData");
                            setCompleted(false);

                            return;
                        }

                        try {
                            colorImageReg.importRGBData(c3, 0, floatBuffer, false);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException " + e + " on colorImageReg.importRGBData");
                            setCompleted(false);

                            return;
                        }
                    } // if (colorsPresent == 3)
                    else { // colorsPresent == 2

                        if (((c == 2) || (c2 == 2)) && ((c == 3) || (c2 == 3))) {
                            c3 = 1;
                        } else if (((c == 1) || (c2 == 1)) && ((c == 3) || (c2 == 3))) {
                            c3 = 2;
                        } else {
                            c3 = 3;
                        }

                        for (i = 0; i < floatBuffer.length; i++) {
                            floatBuffer[i] = 0.0f;
                        }

                        try {
                            colorImageReg.importRGBData(c3, 0, floatBuffer, false);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException " + e + " on colorImageReg.importRGBData");
                            setCompleted(false);

                            return;
                        }
                    } // else colorsPresent == 2

                    colorImageReg.calcMinMax();

                    if (srcImage.getNDims() == 2) {
                        colorImageReg.setVOIs(VOIs);
                        bwImage.setVOIs(VOIs);
                    } else {
                        colorImageReg.setVOIs(VOIs2);
                        bwImage.setVOIs(VOIs2);
                    }

                    colorImageReg.setImageName(srcImage.getImageName() + "_registered");
                    new ViewJFrameImage(colorImageReg);
                } // if (colorsPresent >= 2)
                else {
                    bwImage.setImageName(srcImage.getImageName() + "_registered");
                    bwImage.calcMinMax();

                    if (srcImage.getNDims() == 2) {
                        bwImage.setVOIs(VOIs);
                    } else {
                        bwImage.setVOIs(VOIs2);
                    }

                    new ViewJFrameImage(bwImage);
                }
            } // if (createRegImage)
        } // if (register)
        else { // not registered

            if (srcImage.getNDims() == 2) {
                srcImage.setVOIs(VOIs);
                bwImage.setVOIs(VOIs);
            } else {
                bwImage.setVOIs(VOIs2);
            }
        } // else not registered

        fireProgressStateChanged("Cacluating average intensities");
        mask = new short[sliceSize];

        for (i = 0; i < mask.length; i++) {
            mask[i] = -1;
        }

        mask = bwPostImage.generateVOIMask(mask, donorIndex);

        if (backgroundIndex >= 0) {
            mask = bwPostImage.generateVOIMask(mask, backgroundIndex);
        }

        if (signalIndex >= 0) {
            mask = bwPostImage.generateVOIMask(mask, signalIndex);
        }
        // Find the average intensities in each VOI
        // The backgroundVOI intensity will be less than the donorVOI intensity

        try {
            bwImage.exportData(0, sliceSize, floatBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on bwImage.export(0,sliceSize,floatBuffer)");
            setCompleted(false);

            return;
        }

        preDonorCount = 0;
        preBackgroundCount = 0;
        preSignalCount = 0;
        preDonorIntensity = 0.0f;
        preBackgroundIntensity = 0.0f;
        preSignalIntensity = 0.0f;

        for (i = 0; i < sliceSize; i++) {

            if (mask[i] == donorIndex) {
                preDonorIntensity += floatBuffer[i];
                preDonorCount++;
            }

            if (backgroundIndex >= 0) {

                if (mask[i] == backgroundIndex) {
                    preBackgroundIntensity += floatBuffer[i];
                    preBackgroundCount++;
                }
            } // if (backgroundIndex >= 0)

            if (signalIndex >= 0) {

                if (mask[i] == signalIndex) {
                    preSignalIntensity += floatBuffer[i];
                    preSignalCount++;
                }
            } // if (signalIndex >= 0)

        }

        preDonorIntensity = preDonorIntensity / preDonorCount;

        if (backgroundIndex >= 0) {
            preBackgroundIntensity = preBackgroundIntensity / preBackgroundCount;
            correctedPreDonorIntensity = preDonorIntensity - preBackgroundIntensity;
        } else {
            correctedPreDonorIntensity = preDonorIntensity;
        }

        if (signalIndex >= 0) {
            preSignalIntensity = preSignalIntensity / preSignalCount;
            preSignalRange = preSignalIntensity - preBackgroundIntensity;
        } // if (signalIntensity >= 0)

        try {
            bwPostImage.exportData(0, sliceSize, floatBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on bwPostImage.export(0,sliceSize,floatBuffer)");
            setCompleted(false);

            return;
        }

        postDonorCount = 0;
        postBackgroundCount = 0;
        postSignalCount = 0;
        postDonorIntensity = 0.0f;
        postBackgroundIntensity = 0.0f;
        postSignalIntensity = 0.0f;

        for (i = 0; i < sliceSize; i++) {

            if (mask[i] == donorIndex) {
                postDonorIntensity += floatBuffer[i];
                postDonorCount++;
            }

            if (backgroundIndex >= 0) {

                if (mask[i] == backgroundIndex) {
                    postBackgroundIntensity += floatBuffer[i];
                    postBackgroundCount++;
                }
            } // if (backgroundIndex >= 0)

            if (signalIndex >= 0) {

                if (mask[i] == signalIndex) {
                    postSignalIntensity += floatBuffer[i];
                    postSignalCount++;
                }
            } // if (signalIndex >= 0)

        }

        postDonorIntensity = postDonorIntensity / postDonorCount;

        if (backgroundIndex >= 0) {
            postBackgroundIntensity = postBackgroundIntensity / postBackgroundCount;
            correctedPostDonorIntensity = postDonorIntensity - postBackgroundIntensity;
        } else {
            correctedPostDonorIntensity = postDonorIntensity;
        }

        if (signalIndex >= 0) {
            postSignalIntensity = postSignalIntensity / postSignalCount;
            postSignalRange = postSignalIntensity - postBackgroundIntensity;

            // Scale the donor signal from the prebleached image to the postbleached
            // image using the 2 background intensities and the 2 signal intensities
            correctedPreDonorIntensity = ((postSignalRange / preSignalRange) * preDonorIntensity) +
                                         (((postBackgroundIntensity * preSignalIntensity) -
                                           (preBackgroundIntensity * postSignalIntensity)) / preSignalRange);
            correctedPostDonorIntensity = postDonorIntensity;
        }

        efficiency = (correctedPostDonorIntensity - correctedPreDonorIntensity) / correctedPostDonorIntensity;

        if ((colorsPresent >= 2) || (!createRegImage)) {

            if (bwImage != null) {
                bwImage.disposeLocal();
                bwImage = null;
            }
        }

        if (bwImage2 != null) {
            bwImage2.disposeLocal();
            bwImage2 = null;
        }

        if (bwImage3 != null) {
            bwImage3.disposeLocal();
            bwImage3 = null;
        }

        if (bwPostImage != null) {
            bwPostImage.disposeLocal();
            bwPostImage = null;
        }

        if (backgroundIndex >= 0) {
            ViewUserInterface.getReference().setDataText("Prebleached background intensity = " + preBackgroundIntensity + "\n");
        }

        ViewUserInterface.getReference().setDataText("Prebleached donor intensity = " + preDonorIntensity + "\n");

        if (signalIndex >= 0) {
            ViewUserInterface.getReference().setDataText("Prebleached signal intensity = " + preSignalIntensity + "\n");
        }

        if ((backgroundIndex >= 0) && (signalIndex >= 0)) {
            ViewUserInterface.getReference().setDataText("Linearly scaled prebleached donor intensity = " + correctedPreDonorIntensity + "\n");
        } else if (backgroundIndex >= 0) {
            ViewUserInterface.getReference().setDataText("Background subtracted prebleached donor intensity = " + correctedPreDonorIntensity + "\n");
        }

        if (backgroundIndex >= 0) {
            ViewUserInterface.getReference().setDataText("Postbleached background intensity = " + postBackgroundIntensity + "\n");
        }

        ViewUserInterface.getReference().setDataText("Postbleached donor intensity = " + postDonorIntensity + "\n");

        if (signalIndex >= 0) {
            ViewUserInterface.getReference().setDataText("Postbleached signal intensity = " + postSignalIntensity + "\n");
        }

        if ((signalIndex < 0) && (backgroundIndex >= 0)) {
            ViewUserInterface.getReference().setDataText("Background subtracted postbleached donor intensity = " + correctedPostDonorIntensity +
                           "\n");
        }

        ViewUserInterface.getReference().setDataText("Efficiency = " + efficiency + "\n");

    }

}
