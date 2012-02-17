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
 * <p>The code here is a port of the MATLAB routine Two_D_Sensitized_FRET provided by Dr. Stephen Lockett.</p>
 *
 * <p>The code here requires 3 images - image donor and acceptor dyes taken with a donor filter, image with donor and
 * acceptor dyes taken with a FRET filter, and image with donor and acceptor dyes taken with an acceptor filter. Only 1
 * color will be used from a color image.</p>
 *
 * <p>The code here requires the user to provide 4 bleed thru parameters. AlgorithmFRETBleedThrough will be used to
 * obtain the 4 bleed thru parameters. The DFP into FRET bleed thru and the DFP into AFP bleed thru will be obtained
 * from 1 run of AlgorithmFRETBleedThrough on a set of 3 images containing donor dye only. The AFP into FRET bleed thru
 * and the AFP into DFP bleed thru will be obtained from 1 run of AlgorithmFRETBleedThrough on a set of 3 images
 * containing acceptor dye only. Then AlgorithmFRETEfficiency is run with a set of 3 images containing both donor and
 * acceptor dye, so obtaining the FRET efficiency requires a set of 9 images.</p>
 *
 * <p>The code here assumes the presence of 2 or more VOI regions - a backgorund region and 1 or more active regions
 * whose areas are used for the efficiency calculations. Pixels with saturation values in any of the 3 images are
 * excluded from all calculations. The VOIs must all be placed in the source image and the source image must be the
 * image with donor and acceptor dyes taken with a donor fluorescent peak filter. Radio buttons in the dialog box are
 * used to select an active VOI or a blue background VOI. Either an ellipse VOI, rectangle VOI, levelset VOI, or
 * polyline VOI will be selected from the top MIPAV toolbar. Hit the NEW_VOI button to create other active VOIs. Just do
 * not use blue as an active VOI color. The background region will have a smaller average intensity than the active
 * region.</p>
 *
 * <p>This program outputs the FRET efficiency and the adjusted donor and adjusted acceptor intensities for each active
 * VOI region. Optionally, the user may create floating point images with the adjustedFRET/adjusted donor,
 * adjustedFRET/adjusted acceptor or efficiency given at every pixel location.</p>
 *
 * <p>Reference: 1.) "Imaging Protein-Protein Interactions Using Fluorescence Energy Transfer Microscopy" by Anne K.
 * Kenworthy, Methods, 24, 2001, pp. 289 - 296.</p>
 *
 * <p>2.) "Quantitative Fluorescence Resonance Energy Transfer Measurements Using Fluorescence Microscopy" by Gerald W.
 * Gordon, Gail Berry, Xiao Huan Liang, Beth Levine, and Brian Herman, Biophysical Journal, Volume 74, May, 1998, pp.
 * 2702-2713.</p>
 */
public class AlgorithmFRETEfficiency extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage acceptorImage; // image taken with acceptor filter

    /** DOCUMENT ME! */
    private float AFPintoDFP;

    /** DOCUMENT ME! */
    private float AFPintoFRET;

    /** DOCUMENT ME! */
    private float DFPintoAFP;

    /** DOCUMENT ME! */
    private float DFPintoFRET;

    /** DOCUMENT ME! */
    private ModelImage efficiencyImage;

    /** DOCUMENT ME! */
    private ModelImage fDivAImage;

    /** DOCUMENT ME! */
    private ModelImage fDivDImage;

    /** private ModelImage donorImage; // image taken with donor filter. */
    private ModelImage FRETImage; // image taken with FRET filter

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
     * Creates a new AlgorithmFRETEfficiency object.
     *
     * @param  donorImage       image taken with donor filter
     * @param  FRETImage        image taken with FRET filter
     * @param  acceptorImage    image taken with acceptor filter
     * @param  useRed           If true, use the red color values for the FRET
     * @param  useGreen         If true, use the green color values for the FRET
     * @param  useBlue          If true, use the blue color values for the FRET
     * @param  DFPintoFRET      DOCUMENT ME!
     * @param  AFPintoFRET      DOCUMENT ME!
     * @param  DFPintoAFP       DOCUMENT ME!
     * @param  AFPintoDFP       DOCUMENT ME!
     * @param  fDivDImage       If not null, create adjusted FRET/adjusted donor image.
     * @param  fDivAImage       If not null, create adjusted FRET/adjusted acceptor image.
     * @param  efficiencyImage  If not null, create efficiency image.
     */
    public AlgorithmFRETEfficiency(ModelImage donorImage, ModelImage FRETImage, ModelImage acceptorImage,
                                   boolean useRed, boolean useGreen, boolean useBlue, float DFPintoFRET,
                                   float AFPintoFRET, float DFPintoAFP, float AFPintoDFP, ModelImage fDivDImage,
                                   ModelImage fDivAImage, ModelImage efficiencyImage) {

        super(null, donorImage);
        this.FRETImage = FRETImage;
        this.acceptorImage = acceptorImage;
        this.useRed = useRed;
        this.useGreen = useGreen;
        this.useBlue = useBlue;
        this.DFPintoFRET = DFPintoFRET;
        this.AFPintoFRET = AFPintoFRET;
        this.DFPintoAFP = DFPintoAFP;
        this.AFPintoDFP = AFPintoDFP;
        this.fDivDImage = fDivDImage;
        this.fDivAImage = fDivAImage;
        this.efficiencyImage = efficiencyImage;
        UI = ViewUserInterface.getReference();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        FRETImage = null;
        acceptorImage = null;
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
        float[] donorBuffer;
        float[] FRETBuffer;
        float[] acceptorBuffer;
        short[] mask;
        int nDims;
        int imageSize;
        double maxDonor, maxFRET, maxacceptor, maxAll;
        double saturation;
        double donorBack, FRETBack, acceptorBack;
        int backPixels;
        double[] FRET, acceptor, donor;
        int totalActivePixels;
        int[] activePixels, activeNumber;
        double denom;
        NumberFormat nf;
        double averageAdjustedDonor;
        double averageAdjustedFRET;
        double averageAdjustedAcceptor;
        double adjustedDonor;
        double adjustedFRET;
        double adjustedAcceptor;
        double efficiency;
        double pixelDonor;
        double pixelFRET;
        double pixelAcceptor;
        float[] fDivDBuffer = null;
        float[] fDivABuffer = null;
        float[] effBuffer = null;

        if (srcImage == null) {
            displayError("Donor filter image is null");

            return;
        }

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        nBoundingVOIs = 0;
        activeIndex = -1;
        backgroundIndex = -1;

        fireProgressStateChanged(srcImage.getImageName(), "Performing FRETEfficiency ...");

        

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
            donorBuffer = new float[imageSize];
            FRETBuffer = new float[imageSize];
            acceptorBuffer = new float[imageSize];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory error in AlgorithmFRETEfficiency");
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
                srcImage.exportRGBData(c, 0, imageSize, donorBuffer);
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
            } // else black and white FRETImage

            if (acceptorImage.isColorImage()) {

                try {
                    acceptorImage.exportRGBData(c, 0, imageSize, acceptorBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on acceptorImage.exportRGBData");
                    setCompleted(false);


                    return;
                }
            } // if (acceptorImage.isColorImage())
            else { // black and white acceptorImage

                try {
                    acceptorImage.exportData(0, imageSize, acceptorBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on acceptorImage.exportData");
                    setCompleted(false);


                    return;
                }
            } // else black and white acceptorImage

        } // if (useRed || useGreen || useBlue)
        else { // not color

            try {
                srcImage.exportData(0, imageSize, donorBuffer);
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
                acceptorImage.exportData(0, imageSize, acceptorBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on acceptorImage.exportData");
                setCompleted(false);


                return;
            }

        } // else not color


        // Determine saturation and do not count saturating pixels
        maxDonor = srcImage.getMax();
        maxFRET = FRETImage.getMax();
        maxacceptor = acceptorImage.getMax();
        maxAll = Math.max(maxDonor, Math.max(maxFRET, maxacceptor));

        if (maxAll <= 255.0) {
            saturation = 255.0;
        } else if (maxAll <= 4095.0) {
            saturation = 4095.0;
        } else {
            saturation = maxAll;
        }

        // Exclude saturation regions
        for (i = 0; i < imageSize; i++) {

            if ((donorBuffer[i] == saturation) || (FRETBuffer[i] == saturation) || (acceptorBuffer[i] == saturation)) {
                mask[i] = -1;
            }
        } // for (i = 0; i < imageSize; i++)

        donorBack = 0.0;
        FRETBack = 0.0;
        acceptorBack = 0.0;
        backPixels = 0;
        FRET = new double[nBoundingVOIs - 1];
        acceptor = new double[nBoundingVOIs - 1];
        donor = new double[nBoundingVOIs - 1];
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
                donorBack += donorBuffer[i];
                FRETBack += FRETBuffer[i];
                acceptorBack += acceptorBuffer[i];
                backPixels++;
            } // if (mask[i] == background)
            else if ((mask[i] >= 0) && (mask[i] < backgroundIndex)) {
                donor[mask[i]] = donor[mask[i]] + donorBuffer[i];
                FRET[mask[i]] = FRET[mask[i]] + FRETBuffer[i];
                acceptor[mask[i]] = acceptor[mask[i]] + acceptorBuffer[i];
                activePixels[mask[i]]++;
            } // else if ((mask[i] >= 0) && (mask[i] < backgroundIndex))
            else if (mask[i] >= 0) {
                donor[mask[i] - 1] = donor[mask[i] - 1] + donorBuffer[i];
                FRET[mask[i] - 1] = FRET[mask[i] - 1] + FRETBuffer[i];
                acceptor[mask[i] - 1] = acceptor[mask[i] - 1] + acceptorBuffer[i];
                activePixels[mask[i] - 1]++;
            }
        } // for (i = 0; i < imageSize; i++)

        donorBack = donorBack / backPixels;
        FRETBack = FRETBack / backPixels;
        acceptorBack = acceptorBack / backPixels;
        denom = 1.0 - (AFPintoDFP * DFPintoAFP);
        nf = NumberFormat.getNumberInstance();
        nf.setMaximumFractionDigits(6);
        Preferences.debug("Image with DFP filter\t" + "Image with FRET filter\t" + "Image with AFP filter\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug(srcImage.getImageName() + "\t\t" + FRETImage.getImageName() + "\t\t" +
                          acceptorImage.getImageName() + "\n\n", Preferences.DEBUG_ALGORITHM);
        UI.setDataText("Image with DFP filter\t" + "Image with FRET filter\t" + "Image with AFP filter\n");
        UI.setDataText(srcImage.getImageName() + "\t\t" + FRETImage.getImageName() + "\t\t" +
                       acceptorImage.getImageName() + "\n\n");

        Preferences.debug("VOI \t\t FRET efficiency \tDonor intensity \tAcceptor intensity \tEfficiency * Donor\n", 
        		Preferences.DEBUG_ALGORITHM);
        UI.setDataText("VOI \t\t FRET efficiency \tDonor intensity \tAcceptor intensity \tEfficiency * Donor\n");
        totalActivePixels = 0;
        averageAdjustedDonor = 0.0;
        averageAdjustedFRET = 0.0;
        averageAdjustedAcceptor = 0.0;

        for (i = 0; i < (nBoundingVOIs - 1); i++) {
            totalActivePixels = totalActivePixels + activePixels[i];
            donor[i] = donor[i] / activePixels[i];
            donor[i] = donor[i] - donorBack;
            FRET[i] = FRET[i] / activePixels[i];
            FRET[i] = FRET[i] - FRETBack;
            acceptor[i] = acceptor[i] / activePixels[i];
            acceptor[i] = acceptor[i] - acceptorBack;
            adjustedDonor = (donor[i] - (AFPintoDFP * acceptor[i])) / denom;
            averageAdjustedDonor = averageAdjustedDonor + (activePixels[i] * adjustedDonor);
            adjustedAcceptor = (acceptor[i] - (DFPintoAFP * donor[i])) / denom;
            averageAdjustedAcceptor = averageAdjustedAcceptor + (activePixels[i] * adjustedAcceptor);
            adjustedFRET = FRET[i] - (DFPintoFRET * adjustedDonor) - (AFPintoFRET * adjustedAcceptor);
            averageAdjustedFRET = averageAdjustedFRET + (activePixels[i] * adjustedFRET);
            efficiency = adjustedFRET / adjustedDonor / adjustedAcceptor;
            Preferences.debug(activeNumber[i] + "\t\t" + String.valueOf(nf.format(efficiency)) + "\t\t" +
                              String.valueOf(nf.format(adjustedDonor)) + "\t\t" +
                              String.valueOf(nf.format(adjustedAcceptor)) + "t\t" +
                              String.valueOf(nf.format(efficiency * adjustedDonor)) + "\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText(activeNumber[i] + "\t\t" + String.valueOf(nf.format(efficiency)) + "\t\t" +
                           String.valueOf(nf.format(adjustedDonor)) + "\t\t" +
                           String.valueOf(nf.format(adjustedAcceptor)) + "\t\t" +
                           String.valueOf(nf.format(efficiency * adjustedDonor)) + "\n");

            if (nBoundingVOIs <= 2) {
                Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
                UI.setDataText("\n");
            }
        }

        if (nBoundingVOIs > 2) {
            averageAdjustedDonor = averageAdjustedDonor / totalActivePixels;
            averageAdjustedAcceptor = averageAdjustedAcceptor / totalActivePixels;
            averageAdjustedFRET = averageAdjustedFRET / totalActivePixels;
            efficiency = averageAdjustedFRET / averageAdjustedDonor / averageAdjustedAcceptor;
            Preferences.debug("WeightedAverage\t" + String.valueOf(nf.format(efficiency)) + "\t\t" +
                              String.valueOf(nf.format(averageAdjustedDonor)) + "\t\t" +
                              String.valueOf(nf.format(averageAdjustedAcceptor)) + "\t\t" +
                              String.valueOf(nf.format(efficiency * averageAdjustedDonor)) + "\n\n", 
                              Preferences.DEBUG_ALGORITHM);
            UI.setDataText("WeightedAverage\t" + String.valueOf(nf.format(efficiency)) + "\t\t" +
                           String.valueOf(nf.format(averageAdjustedDonor)) + "\t\t" +
                           String.valueOf(nf.format(averageAdjustedAcceptor)) + "\t\t" +
                           String.valueOf(nf.format(efficiency * averageAdjustedDonor)) + "\n\n");
        } // if (nBoundingVOIs > 2)

        FRETImage.disposeLocal();
        FRETImage = null;
        acceptorImage.disposeLocal();
        acceptorImage = null;

        if (fDivDImage != null) {
            fDivDBuffer = new float[imageSize];
        }

        if (fDivAImage != null) {
            fDivABuffer = new float[imageSize];
        }

        if (efficiencyImage != null) {
            effBuffer = new float[imageSize];
        }

        if ((fDivDImage != null) || (fDivAImage != null) || (efficiencyImage != null)) {

            for (i = 0; i < imageSize; i++) {
                pixelDonor = donorBuffer[i] - donorBack;
                pixelFRET = FRETBuffer[i] - FRETBack;
                pixelAcceptor = acceptorBuffer[i] - acceptorBack;
                adjustedDonor = (pixelDonor - (AFPintoDFP * pixelAcceptor)) / denom;
                adjustedAcceptor = (pixelAcceptor - (DFPintoAFP * pixelDonor)) / denom;
                adjustedFRET = pixelFRET - (DFPintoFRET * adjustedDonor) - (AFPintoFRET * adjustedAcceptor);

                if (fDivDImage != null) {
                    fDivDBuffer[i] = (float) (adjustedFRET / adjustedDonor);
                }

                if (fDivAImage != null) {
                    fDivABuffer[i] = (float) (adjustedFRET / adjustedAcceptor);
                }

                if (efficiencyImage != null) {
                    effBuffer[i] = (float) (adjustedFRET / adjustedDonor / adjustedAcceptor);
                }
            } // for (i = 0; i < imageSize; i++)

            if (fDivDImage != null) {

                try {
                    fDivDImage.importData(0, fDivDBuffer, true);
                } catch (IOException e) {
                    MipavUtil.displayError("Error on fDivDImage.importData(0, fDivDBuffer,true)");
                }
            } // if (fDivDImage != null)

            if (fDivAImage != null) {

                try {
                    fDivAImage.importData(0, fDivABuffer, true);
                } catch (IOException e) {
                    MipavUtil.displayError("Error on fDivAImage.importData(0, fDivABuffer,true)");
                }
            } // if (fDivAImage != null)

            if (efficiencyImage != null) {

                try {
                    efficiencyImage.importData(0, effBuffer, true);
                } catch (IOException e) {
                    MipavUtil.displayError("Error on efficiencyImage.importData(0,effBuffer,true)");
                }
            } // if (efficiencyImage != null)
        } // if ((fDivDImage != null) || (fDivAImage != null) || (efficiencyImage != null))

        setCompleted(true);
    }
}
