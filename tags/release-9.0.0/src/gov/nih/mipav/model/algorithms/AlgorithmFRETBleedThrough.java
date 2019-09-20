package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;


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
 * <p>This code is a port of the MATLAB routine TwoD_FRET_and_2FP_bleed_thru_from_1FP provided by Dr. Stephen Lockett.
 * </p>
 *
 * <p>The code here requires 3 images - image with a FP1 dye taken with a FP1 filter, image with a FP1 dye taken with a
 * FRET filter, and image with a FP1 dye taken with an FP2 filter. The code is run with a set of 3 images with donor dye
 * only to obtain the DFP to FRET bleed thru and the DFP to AFP bleed thru. The code is run with a set of 3 images with
 * acceptor dye only to obtain the AFP to FRET bleed thru and the AFP to DFP bleed thru. When these 4 bleed thru
 * parameters have been obtained, then a set of 3 images containing both donor and acceptor dye is used with
 * AlgorithmFRETEfficiency to obtain a FRET efficiency on each active VOI. So a set of 9 images is used to obtain the
 * FRET efficiency. Only 1 user selected color will be used from a color image.</p>
 *
 * <p>The code here assumes the presence of 2 or more VOI regions - a backgorund region and 1 or more active regions
 * whose areas are used for the bleedthrough calculations. If a pixel is saturated in any of the 3 images, then that
 * pixel will be excluded from all calculations. The VOIs must all be placed in the source image and the source image
 * must be an image of the 1FP taken with a 1FP filter. That is, the source image must either be an image with donor dye
 * only taken with a donor fluorescent peak filter or an image with acceptor dye only taken with an acceptor fluorescent
 * peak filter. Radio buttons in the dialog box are used to select an active VOI or a blue background VOI. Either an
 * ellipse VOI, rectangle VOI, levelset VOI, or polyline VOI will be selected from the top MIPAV toolbar. Hit the
 * NEW_VOI button to create other active VOIs. Just do not use blue as an active VOI color. The background region will
 * have a smaller average intensity than the active region.</p>
 *
 * <p>denom = mean(FP1 dye with FP1 filter active VOI) - mean(FP1 dye with FP1 filter background VOI)</p>
 *
 * <p>FRET bleed through = [mean(FP1 dye with FRET filter active VOI) - mean(FP1 dye with FRET filter background
 * VOI)]/denom</p>
 *
 * <p>FP2 bleed through = [mean(FP1 dye with FP2 filter active VOI) - mean(FP1 dye with FP2 filter background VOI)]/
 * denom</p>
 *
 * <p>The code outputs the FP1 to FRET bleed thru and the FP1 to FP2 bleed thru for each active VOI region.</p>
 *
 * <p>Reference: 1.) "Imaging Protein-Protein Interactions Using Fluorescence Energy Transfer Microscopy" by Anne K.
 * Kenworthy, Methods, 24, 2001, pp. 289 - 296.</p>
 *
 * <p>2.) "Quantitative Fluorescence Resonance Energy Transfer Measurements Using Fluorescence Microscopy" by Gerald W.
 * Gordon, Gail Berry, Xiao Huan Liang, Beth Levine, and Brian Herman, Biophysical Journal, Volume 74, May, 1998, pp.
 * 2702-2713.</p>
 */
public class AlgorithmFRETBleedThrough extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * If acceptorRun is true, run on 3 images with acceptor dye only. If acceptorRun is false, run on 3 images with
     * donor dye only.
     */
    private boolean acceptorRun = true;

    /** DOCUMENT ME! */
    private ModelImage FP2Image; // FP1 taken with FP2 filter

    /** private ModelImage FP1Image; // FP1 taken with FP1 filter. */
    private ModelImage FRETImage; // FP1 taken with FRET filter

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmFRETBleedThrough object.
     *
     * @param  FP1Image     FP1 taken with FP1 filter
     * @param  FRETImage    FP1 taken with FRET filter
     * @param  FP2Image     FP1 taken with FP2 filter
     * @param  useRed       If true, use the red color values for the FRET
     * @param  useGreen     If true, use the green color values for the FRET
     * @param  useBlue      If true, use the blue color values for the FRET
     * @param  acceptorRun  If true, run on 3 images with only acceptor dye If false, run on 3 images with only donor
     *                      dye
     */
    public AlgorithmFRETBleedThrough(ModelImage FP1Image, ModelImage FRETImage, ModelImage FP2Image, boolean useRed,
                                     boolean useGreen, boolean useBlue, boolean acceptorRun) {

        super(null, FP1Image);
        this.FRETImage = FRETImage;
        this.FP2Image = FP2Image;
        this.useRed = useRed;
        this.useGreen = useGreen;
        this.useBlue = useBlue;
        this.acceptorRun = acceptorRun;
        UI = ViewUserInterface.getReference();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        FRETImage = null;
        FP2Image = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {
        int i;
        int c;
        int nVOIs;
        float[] hsb;
        float hue;
        ViewVOIVector VOIs;
        int nBoundingVOIs;
        int activeIndex;
        int backgroundIndex;
        float[] FP1Buffer;
        float[] FRETBuffer;
        float[] FP2Buffer;
        short[] mask;
        int nDims;
        int imageSize;
        double maxFP1, maxFRET, maxFP2, maxAll;
        double saturation;
        double FP1Back, FRETBack, FP2Back;
        int backPixels;
        double[] FRETBleed, FP2Bleed, active;
        double averageFRETBleed, averageFP2Bleed;
        int[] activePixels, activeNumber;
        int totalActivePixels;
        double denom;
        NumberFormat nf;
        File writeFile;
        RandomAccessFile raFile;
        String lineString;

        if (srcImage == null) {
            displayError("FP1 Image is null");

            return;
        }

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        nBoundingVOIs = 0;
        activeIndex = -1;
        backgroundIndex = -1;


        fireProgressStateChanged(srcImage.getImageName(), "Performing FRET bleed thru...");

        

        nDims = srcImage.getNDims();
        imageSize = srcImage.getExtents()[0];

        for (i = 1; i < nDims; i++) {
            imageSize *= srcImage.getExtents()[i];
        }

        mask = new short[imageSize];

        for (i = 0; i < mask.length; i++) {
            mask[i] = -1;
        }

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                mask = srcImage.generateVOIMask(mask, i);
                nBoundingVOIs++;
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - (2.0f / 3.0f))) < 0.0001f) {

                    if (backgroundIndex == -1) {
                        backgroundIndex = i;
                        VOIs.VOIAt(i).setName("Background");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 background VOI");
                        setCompleted(false);


                        return;
                    }
                } else {
                    activeIndex = i;
                    VOIs.VOIAt(i).setName("Active" + i);
                }
            }
        } // for (i = 0; i < nVOIs; i++)


        if (activeIndex == -1) {
            MipavUtil.displayError("Must specify an active VOI");
            setCompleted(false);


            return;
        }

        if (backgroundIndex == -1) {
            MipavUtil.displayError("Must specify a background VOI");
            setCompleted(false);


            return;
        }

        try {
            FP1Buffer = new float[imageSize];
            FRETBuffer = new float[imageSize];
            FP2Buffer = new float[imageSize];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory error in AlgorithmFRETBleedThrough");
            setCompleted(false);


            return;
        }

        if (useRed || useGreen || useBlue) {

            if (useRed) {
                c = 1;
            } else if (useGreen) {
                c = 2;
            } else {
                c = 3;
            }

            try {
                srcImage.exportRGBData(c, 0, imageSize, FP1Buffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData");
                setCompleted(false);


                return;
            }

            if (FRETImage.isColorImage()) {

                try {
                    FRETImage.exportRGBData(c, 0, imageSize, FRETBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on FRETImage.exportRGBData");
                    setCompleted(false);


                    return;
                }
            } // if (FRETImage.isColorImage())
            else { // black and white FRETImage

                try {
                    FRETImage.exportData(0, imageSize, FRETBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on FRETImage.exportData");
                    setCompleted(false);


                    return;
                }
            } // else black and white FRET image

            if (FP2Image.isColorImage()) {

                try {
                    FP2Image.exportRGBData(c, 0, imageSize, FP2Buffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on FP2Image.exportRGBData");
                    setCompleted(false);


                    return;
                }
            } // if (FP2Image.isColorImage())
            else { // black and white FP2Image

                try {
                    FP2Image.exportData(0, imageSize, FP2Buffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on FP2Image.exportData");
                    setCompleted(false);


                    return;
                }
            } // black and white FP2Image

        } // if (useRed || useGreen || useBlue)
        else { // not color

            try {
                srcImage.exportData(0, imageSize, FP1Buffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
                setCompleted(false);


                return;
            }

            try {
                FRETImage.exportData(0, imageSize, FRETBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on FRETImage.exportData");
                setCompleted(false);


                return;
            }

            try {
                FP2Image.exportData(0, imageSize, FP2Buffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on FP2Image.exportData");
                setCompleted(false);


                return;
            }

        } // else not color


        // Determine saturation and do not count saturating pixels
        maxFP1 = srcImage.getMax();
        maxFRET = FRETImage.getMax();
        maxFP2 = FP2Image.getMax();
        maxAll = Math.max(maxFP1, Math.max(maxFRET, maxFP2));

        if (maxAll <= 255.0) {
            saturation = 255.0;
        } else if (maxAll <= 4095.0) {
            saturation = 4095.0;
        } else {
            saturation = maxAll;
        }

        // Exclude saturation regions
        for (i = 0; i < imageSize; i++) {

            if ((FP1Buffer[i] == saturation) || (FRETBuffer[i] == saturation) || (FP2Buffer[i] == saturation)) {
                mask[i] = -1;
            }
        } // for (i = 0; i < imageSize; i++)

        FP1Back = 0.0;
        FRETBack = 0.0;
        FP2Back = 0.0;
        backPixels = 0;
        FRETBleed = new double[nBoundingVOIs - 1];
        FP2Bleed = new double[nBoundingVOIs - 1];
        active = new double[nBoundingVOIs - 1];
        activePixels = new int[nBoundingVOIs - 1];
        activeNumber = new int[nBoundingVOIs - 1];

        for (i = 0; i < nBoundingVOIs; i++) {

            if (i < backgroundIndex) {
                activeNumber[i] = i;
            } else if (i > backgroundIndex) {
                activeNumber[i - 1] = i;
            }
        } // for (i = 0; i < nBoundingVOIs; i++)

        for (i = 0; i < imageSize; i++) {

            if (mask[i] == backgroundIndex) {
                FP1Back += FP1Buffer[i];
                FRETBack += FRETBuffer[i];
                FP2Back += FP2Buffer[i];
                backPixels++;
            } // if (mask[i] == background)
            else if ((mask[i] >= 0) && (mask[i] < backgroundIndex)) {
                active[mask[i]] = active[mask[i]] + FP1Buffer[i];
                FRETBleed[mask[i]] = FRETBleed[mask[i]] + FRETBuffer[i];
                FP2Bleed[mask[i]] = FP2Bleed[mask[i]] + FP2Buffer[i];
                activePixels[mask[i]]++;
            } // else if ((mask[i] >= 0) && (mask[i] < backgroundIndex))
            else if (mask[i] >= 0) {
                active[mask[i] - 1] = active[mask[i] - 1] + FP1Buffer[i];
                FRETBleed[mask[i] - 1] = FRETBleed[mask[i] - 1] + FRETBuffer[i];
                FP2Bleed[mask[i] - 1] = FP2Bleed[mask[i] - 1] + FP2Buffer[i];
                activePixels[mask[i] - 1]++;
            }
        } // for (i = 0; i < imageSize; i++)

        FP1Back = FP1Back / backPixels;
        FRETBack = FRETBack / backPixels;
        FP2Back = FP2Back / backPixels;
        nf = NumberFormat.getNumberInstance();
        nf.setMaximumFractionDigits(6);

        if (acceptorRun) {
            Preferences.debug("Acceptor dye only run\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("Acceptor dye only run\n");
        } else {
            Preferences.debug("Donor dye only run\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("Donor dye only run\n");
        }

        Preferences.debug("1FP image with 1FP filter\t" + "1FP image with FRET filter\t" +
                          "1FP image with 2FP filter\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug(srcImage.getImageName() + "\t\t" + FRETImage.getImageName() + "\t\t" +
                          FP2Image.getImageName() + "\n\n", Preferences.DEBUG_ALGORITHM);
        UI.setDataText("1FP image with 1FP filter\t" + "1FP image with FRET filter\t" + "1FP image with 2FP filter\n");
        UI.setDataText(srcImage.getImageName() + "\t\t" + FRETImage.getImageName() + "\t\t" + FP2Image.getImageName() +
                       "\n\n");
        Preferences.debug("VOI \t\tFRET bleed thru \tFP2 bleed thru\n", Preferences.DEBUG_ALGORITHM);
        UI.setDataText("VOI \t\tFRET bleed thru \tFP2 bleed thru\n");
        totalActivePixels = 0;
        averageFRETBleed = 0.0;
        averageFP2Bleed = 0.0;

        for (i = 0; i < (nBoundingVOIs - 1); i++) {
            totalActivePixels = totalActivePixels + activePixels[i];
            active[i] = active[i] / activePixels[i];
            denom = active[i] - FP1Back;
            FRETBleed[i] = FRETBleed[i] / activePixels[i];
            FRETBleed[i] = (FRETBleed[i] - FRETBack) / denom;
            averageFRETBleed = averageFRETBleed + (FRETBleed[i] * activePixels[i]);
            FP2Bleed[i] = FP2Bleed[i] / activePixels[i];
            FP2Bleed[i] = (FP2Bleed[i] - FP2Back) / denom;
            averageFP2Bleed = averageFP2Bleed + (FP2Bleed[i] * activePixels[i]);
            Preferences.debug(activeNumber[i] + "\t\t" + String.valueOf(nf.format(FRETBleed[i])) + "\t\t" +
                              String.valueOf(nf.format(FP2Bleed[i])) + "\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText(activeNumber[i] + "\t\t" + String.valueOf(nf.format(FRETBleed[i])) + "\t\t" +
                           String.valueOf(nf.format(FP2Bleed[i])) + "\n");

            if (nBoundingVOIs <= 2) {
                Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
                UI.setDataText("\n");
            }
        }

        averageFRETBleed = averageFRETBleed / totalActivePixels;
        averageFP2Bleed = averageFP2Bleed / totalActivePixels;

        File currentFileDir = new File(srcImage.getFileInfo(0).getFileDirectory());

        if (currentFileDir.exists() && currentFileDir.isDirectory()) {
            File[] files = currentFileDir.listFiles();

            for (i = 0; i < files.length; i++) {

                if (acceptorRun) {

                    if (files[i].getName().startsWith("acceptor_")) {

                        if (files[i].exists()) {

                            try {
                                files[i].delete();
                            } catch (SecurityException sc) {
                                MipavUtil.displayError("Security error occurs while trying to " + "delete " +
                                                       files[i].getName());
                            }
                        }
                    }
                } // if (acceptorRun)
                else { // not acceptorRun

                    if (files[i].getName().startsWith("donor_")) {

                        if (files[i].exists()) {

                            try {
                                files[i].delete();
                            } catch (SecurityException sc) {
                                MipavUtil.displayError("Security error occurs while trying to " + "delete " +
                                                       files[i].getName());
                            }
                        }
                    }
                } // else not acceptorRun
            } // for (i = 0; i < files.length; i++)
        } // if (currentFileDir.exists() && currentFileDir.isDirectory())

        if (acceptorRun) {
            writeFile = new File(srcImage.getFileInfo(0).getFileDirectory() + "acceptor_" +
                                 srcImage.getFileInfo(0).getFileName());
        } // if acceptorRun)
        else {
            writeFile = new File(srcImage.getFileInfo(0).getFileDirectory() + "donor_" +
                                 srcImage.getFileInfo(0).getFileName());
        }

        try {
            raFile = new RandomAccessFile(writeFile, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
            lineString = new String(String.valueOf(nf.format(averageFRETBleed)) + "\n" +
                                    String.valueOf(nf.format(averageFP2Bleed)) + "\n");
            raFile.write(lineString.getBytes());
            raFile.close();
        } catch (IOException e) {
            MipavUtil.displayError("IOException writing " + writeFile.getName());
        }


        if (nBoundingVOIs > 2) {
            Preferences.debug("Weighted average\t" + String.valueOf(nf.format(averageFRETBleed)) + "\t\t" +
                              String.valueOf(nf.format(averageFP2Bleed)) + "\n\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("Weighted average\t" + String.valueOf(nf.format(averageFRETBleed)) + "\t\t" +
                           String.valueOf(nf.format(averageFP2Bleed)) + "\n\n");
        } // if (nBoundingVOIs > 2)

        FRETImage.disposeLocal();
        FRETImage = null;
        FP2Image.disposeLocal();
        FP2Image = null;


        setCompleted(true);
    }
}
