package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;

import java.io.IOException;


/**
 * <p>
 * In hard thresholding a wavelet coefficient whose magnitude is below the product of threshold and the maximum wavelet
 * magnitude is zeroed, and a wavelet coefficient whose magnitude is greater than or equal to the product is left
 * unchanged. In soft thresholding a wavelet coefficient whose magnitude is below the product of threshold and the
 * maximum wavelet magnitude is zeroed, and a wavelet coefficient whose magnitude is greater than or equal to the
 * product has its magnitude decreased by the product. Generally, soft thresholding is a better choice. The signal
 * reconstructed with the coefficients left after hard thresholding may have undesirable artifacts including sidelobes.
 * Then, an inverse wavelet transform is performed. The image is stripped down to its original size. The data is clamped
 * so as not to exceed the bounds of the data type of the image into which it is imported. This is a simple method of
 * reducing white noise.
 * </p>
 * 
 * <p>
 * The forward and inverse Daubechies wavelet transform routines are taken from Numerical Recipes in C The Art of
 * Scientific Computing, 2nd edition, by William H. Press, Saul A. Teukolsky, William T. Vetterling, and Brian P.
 * Flannery, Cambridge University Press, 1997, Chapter 13.10, pp. 591 - 606. The wavelet routines daub4, pwt, wt1,
 * wtn, and pwtset in Numerical Recipes in C are in the public domain. See {@link http://www.nr.com/public-domain.html}
 * for more information.
 * </p>
 * 
 * <p>
 * The wavelet transform is mathematically defined only within a signal; image applications need to solve the boundary
 * problem. 4 different techniques exist:
 * <ol>
 * <li>Mirror image replication</li>
 * <li>Zero padding</li>
 * <li>Linear extrapolation</li>
 * <li>Circular convolution</li>
 * </ol>
 * </p>
 * 
 * <p>
 * In this module zero padding is used. As stated in the Medx 3.4 User's Guide in Chapter 17 on Advanced Image
 * Processing Techniques: "The mirror condition can only be used with symmetric wavelet basis." "Daubechies wavelet
 * bases are non-symmetrical orthogonal wavelets with compact support and are good for data suppression. Only the
 * periodic boundary condition should be used when using the Daubechies wavelet basis." In Empirical Evaluation of
 * Boundary Policies for Wavelet-based Image Coding by Claudia Schremmer zero padding, circular convolution, and mirror
 * image padding are compared. She concludes that mirror padding works best with regard to quality, zero padding
 * performs worst with regard to quality, and circular convolution is midway between the 2. The matlab wavelet toolbox
 * literature says that "the disadvantage of zero padding is that discontinuities are artificially created at the
 * border." Mirror padding "has the disadvantage of artificially creating discontinuities of the first derivative at the
 * border, but this method works well in general for images." Extrapolation "works well in general for smooth signals."
 * The wavelet toolbox uses mirror padding as its default mode. How much padding is required? Claudia Schremmer states:
 * "With padding, the coefficients of the signal on either side of the border are padded with filter_length - 2
 * coefficients. Consequently, each signal coefficient enters into filter_length/2 calculations of convolution, and the
 * transform is reversible."
 * </p>
 * 
 * <p>
 * The image is expanded so that all dimensions are powers of 2 and there is zero padding going past each original
 * boundary by the coefficient number - 2. The image is transformed into wavelet coefficients.
 * </p>
 */
public class AlgorithmWaveletThreshold extends AlgorithmBase {
    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** FORWARD and INVERSE are 2 possible transformDir values */
    public static final int INVERSE = -1;

    /** DOCUMENT ME! */
    public static final int FORWARD = 1;

    /**
     * underlying wavelet filter For 4 coefficients the routine daub4 is considerably faster than pwt. DAUB4 implements
     * a Daubechies 4-coefficient wavelet filter. PWT implements Daubechies filters with 4, 12, and 20 coefficients.
     * DAUB4 and PWT use different default centerings. In spite of the faster speed of DAUB4, the dialog is designed
     * only to call PWT so that the user only sees changes due to the number of coefficients and is not confused by
     * changes in centering. Reference on NONNEGATIVE_GARROTE and SCAD thresholding: "Wavelet Estimators in
     * Nonparametric Regression: A Comparative Simulation Study" by Anestis Antoniadis, Jeremie Bigot, and Theofanis
     * Sapatinas, Journal of Statistical Software, Vol. 6, Issue 6, pp. 1-83, 2001.
     */
    public static final int DAUB4 = 1;

    /** DOCUMENT ME! */
    public static final int PWT = 2;

    /** coefficents for DAUB4 wavelet filter. */
    public static final double C0 = 0.4829629131445341;

    /** DOCUMENT ME! */
    public static final double C1 = 0.8365163037378079;

    /** DOCUMENT ME! */
    public static final double C2 = 0.2241438680420134;

    /** DOCUMENT ME! */
    public static final double C3 = -0.1294095225512604;

    /** coefficients for the PWT filter. */
    public static final double[] c4 = {0.0, 0.4829629131445341, 0.8365163037378079, 0.2241438680420134,
            -0.1294095225512604};

    /** DOCUMENT ME! */
    public static final double[] c12 = {0.0, 0.111540743350, 0.494623890398, 0.751133908021, 0.315250351709,
            -0.226264693965, -0.129766867567, 0.097501605587, 0.027522865530, -0.031582039318, 0.000553842201,
            0.004777257511, -0.001077301085};

    /** DOCUMENT ME! */
    public static final double[] c20 = {0.0, 0.026670057901, 0.188176800078, 0.527201188932, 0.688459039454,
            0.281172343661, -0.249846424327, -0.195946274377, 0.127369340336, 0.093057364604, -0.071394147166,
            -0.029457536822, 0.033212674059, 0.003606553567, -0.010733175483, 0.001395351747, 0.001992405295,
            -0.000685856695, -0.000116466855, 0.000093588670, -0.000013264203};

    /** thresholding estimator. */
    public static final int HARD = 1;

    /** DOCUMENT ME! */
    public static final int SOFT = 2;

    public static final int NONNEGATIVE_GARROTE = 3;

    public static final int SCAD = 4;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] aArray;

    /** DOCUMENT ME! */
    private float aCutoff;

    /** DOCUMENT ME! */
    private float[] aExp;

    /** DOCUMENT ME! */
    private float[] aLog;

    /** DOCUMENT ME! */
    private float aMax;

    /** DOCUMENT ME! */
    private int arrayLength;

    /** DOCUMENT ME! */
    private int belowThreshold;

    /** DOCUMENT ME! */
    private double[] cc;

    /** DOCUMENT ME! */
    private final int cm2; // cNum - 2

    /** DOCUMENT ME! */
    private final int cNum;

    /** DOCUMENT ME! */
    private double[] cr;

    /** DOCUMENT ME! */
    private int dataType;

    /** DOCUMENT ME! */
    private final boolean doWaveletImage; // display wavelet transform in log magnitude if true

    /** DOCUMENT ME! */
    private int[] extents;

    /** DOCUMENT ME! */
    private final int filterType;

    /** DOCUMENT ME! */
    private boolean foundSize;

    /** DOCUMENT ME! */
    private int ioff;

    /** DOCUMENT ME! */
    private int joff;

    /** DOCUMENT ME! */
    private int minSize;

    /** DOCUMENT ME! */
    private int nDims;

    /** DOCUMENT ME! */
    private int newArrayLength;

    /** DOCUMENT ME! */
    private int[] newExtents;

    /** DOCUMENT ME! */
    private int newXDim, newYDim, newZDim;

    /** DOCUMENT ME! */
    private int offsetX;

    /** DOCUMENT ME! */
    private int offsetY;

    /** DOCUMENT ME! */
    private int offsetZ;

    /** DOCUMENT ME! */
    private final float threshold; // fraction of maximum magnitude below which coefficients

    // are zeroed

    /** DOCUMENT ME! */
    private final int thresholdType; // HARD or SOFT

    /** DOCUMENT ME! */
    private int transformDir;

    /** DOCUMENT ME! */
    private ModelImage waveletImage = null;

    /** DOCUMENT ME! */
    private int xDim, yDim, zDim;

    /** DOCUMENT ME! */
    private int xy, newXY;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmWaveletThreshold object.
     * 
     * @param srcImg source image model
     * @param filterType wavelet filter
     * @param cNum number of coefficients in the pwt filter
     * @param thresholdType HARD or SOFT thresholding
     * @param threshold fraction of maximum magnitude below which coefficients are zeroed
     * @param doWaveletImage display wavelet transform image in log magnitude if true
     * @param userInterface DOCUMENT ME!
     */
    public AlgorithmWaveletThreshold(final ModelImage srcImg, final int filterType, int cNum, final int thresholdType,
            final float threshold, final boolean doWaveletImage) {
        super(null, srcImg);

        this.filterType = filterType;
        this.cNum = cNum;

        if (filterType == AlgorithmWaveletThreshold.DAUB4) {
            cNum = 4;
        }

        cm2 = cNum - 2;
        this.thresholdType = thresholdType;
        this.threshold = threshold;
        this.doWaveletImage = doWaveletImage;
    }

    /**
     * Creates a new AlgorithmWaveletThreshold object.
     * 
     * @param destImg image model where result image is to be stored
     * @param srcImg source image model
     * @param filterType wavelet filter
     * @param cNum number of coefficients in the pwt filter
     * @param thresholdType HARD or SOFT thresholding
     * @param threshold fraction of maximum magnitude below which coefficients are zeroed
     * @param doWaveletImage display log maagnitude wavelet transform image if true
     * @param userInterface DOCUMENT ME!
     */
    public AlgorithmWaveletThreshold(final ModelImage destImg, final ModelImage srcImg, final int filterType, int cNum,
            final int thresholdType, final float threshold, final boolean doWaveletImage) {
        super(destImg, srcImg);

        this.filterType = filterType;
        this.cNum = cNum;

        if (filterType == AlgorithmWaveletThreshold.DAUB4) {
            cNum = 4;
        }

        cm2 = cNum - 2;
        this.thresholdType = thresholdType;
        this.threshold = threshold;
        this.doWaveletImage = doWaveletImage;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepare this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        waveletImage = null;
        extents = null;
        cc = null;
        cr = null;
        aArray = null;
        aExp = null;
        aLog = null;
        super.finalize();
    }

    /**
     * Accessor that returns the image.
     * 
     * @return the wavelet image
     */
    public ModelImage getWaveletImage() {
        return waveletImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int k;
        int x, y, z;
        float aCutSquared;

        double sig = -1.0;

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Importing source image...");

        if (filterType == AlgorithmWaveletThreshold.PWT) {

            switch (cNum) {

                case 4:
                    cc = AlgorithmWaveletThreshold.c4;
                    cr = new double[5];
                    break;

                case 12:
                    cc = AlgorithmWaveletThreshold.c12;
                    cr = new double[13];
                    break;

                case 20:
                    cc = AlgorithmWaveletThreshold.c20;
                    cr = new double[21];
                    break;
            } // switch(cNum)

            for (k = 1; k <= cNum; k++) {
                cr[cNum + 1 - k] = sig * cc[k];
                sig = -sig;
            }

            ioff = joff = - (cNum >> 1);
            // These values of ioff and joff center the "support" of the
            // wavelets at each level. Alternatively, the "peaks" of the
            // wavelets can be approximately centered by the choices
            // ioff = -2 and joff = -cNum + 2. Note that
            // daub4 and pwt use different default centerings.
        } // if (filterType == PWT)

        nDims = srcImage.getNDims();
        extents = srcImage.getExtents();
        xDim = extents[0];
        yDim = extents[1];

        if (nDims > 2) {
            zDim = extents[2];
        }

        arrayLength = 1;

        for (int i = 0; i < nDims; i++) {
            arrayLength *= extents[i];
        }

        try {
            aArray = new float[arrayLength];
        } catch (final OutOfMemoryError e) {
            aArray = null;
            System.gc();
            displayError("AlgorithmWaveletThreshold: Out of memory creating a");

            setCompleted(false);

            return;
        }

        try {
            srcImage.exportData(0, arrayLength, aArray);
        } catch (final IOException error) {
            displayError("AlgorithmWaveletThreshold: Source image is locked");

            setCompleted(false);

            return;
        }

        // Create aExp with all dimensions powers of 2 and with zero padding
        // going in cNum - 2 past each boundary
        newExtents = new int[nDims];

        for (int i = 0; i < nDims; i++) {
            minSize = extents[i] + (2 * cm2);
            foundSize = false;

            for (int j = 2; (j < 30) && ( !foundSize); j++) {

                if ((int) Math.pow(2, j) >= minSize) {
                    newExtents[i] = (int) Math.pow(2, j);
                    foundSize = true;
                } // if ((int)Math.pow(2,j) >= minSize)
            } // for (j = 2; (j < 30) && (!foundSize); j++)
        } // for (i = 0; i < nDims; i++)

        newXDim = newExtents[0];
        newYDim = newExtents[1];

        if (nDims > 2) {
            newZDim = newExtents[2];
        }

        newArrayLength = 1;

        for (int i = 0; i < nDims; i++) {
            newArrayLength *= newExtents[i];
        }

        try {
            aExp = new float[newArrayLength];
        } catch (final OutOfMemoryError e) {
            aExp = null;
            System.gc();
            displayError("AlgorithmWaveletThreshold: Out of memory creating aExp");

            setCompleted(false);

            return;
        }

        for (int i = 0; i < newArrayLength; i++) {
            aExp[i] = 0.0f;
        }

        if (nDims == 2) {
            offsetX = (newXDim - xDim) / 2;
            offsetY = (newYDim - yDim) / 2;

            for (x = 0; x < xDim; x++) {

                for (y = 0; y < yDim; y++) {
                    aExp[x + offsetX + (newXDim * (y + offsetY))] = aArray[x + (xDim * y)];
                }
            }

            // Mirror the rows
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (j = 0; j < cm2; j++) {
            // aExp[x + newXDim*(offsetY - j - 1)] =
            // aExp[x + newXDim*(offsetY + j)];
            // aExp[x + newXDim*(offsetY + yDim + j)] =
            // aExp[x + newXDim*(offsetY + yDim - j - 1)];
            // }
            // }

            // Mirror the columns
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (j = 0; j < cm2; j++) {
            // aExp[offsetX - j - 1 + newXDim*y] =
            // aExp[offsetX + j + newXDim*y];
            // aExp[offsetX + xDim + j + newXDim*y] =
            // aExp[offsetX + xDim - j - 1 + newXDim*y];
            // }
            // }

            // Have (cNum-2) squared in each corner to mirror
            // upper left corner
            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // aExp[x + newXDim*y] =
            // aExp[x + delX + newXDim*(y + delY)];
            // }
            // }

            // upper right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // aExp[x + newXDim*y] =
            // aExp[x - delX + newXDim*(y + delY)];
            // }
            // }

            // lower left corner
            // for (x = offsetX - cm2, delX = 2*cm2 - 1; x < offsetX; x++, delX -=2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // aExp[x + newXDim*y] =
            // aExp[x + delX + newXDim*(y - delY)];
            // }
            // }

            // lower right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // aExp[x + newXDim*y] =
            // aExp[x - delX + newXDim*(y - delY)];
            // }
            // }
        } // if (nDims == 2)
        else if (nDims == 3) {
            offsetX = (newXDim - xDim) / 2;
            offsetY = (newYDim - yDim) / 2;
            offsetZ = (newZDim - zDim) / 2;
            xy = xDim * yDim;
            newXY = newXDim * newYDim;

            for (x = 0; x < xDim; x++) {

                for (y = 0; y < yDim; y++) {

                    for (z = 0; z < zDim; z++) {
                        aExp[x + offsetX + (newXDim * (y + offsetY)) + (newXY * (z + offsetZ))] = aArray[x + (xDim * y)
                                + (xy * z)];
                    }
                }
            }

            // Mirror the rows
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // for (j = 0; j < cm2; j++) {
            // aExp[x + newXDim*(offsetY - j - 1) + newXY*z] =
            // aExp[x + newXDim*(offsetY + j) + newXY*z];
            // aExp[x + newXDim*(offsetY + yDim + j) + newXY*z] =
            // aExp[x + newXDim*(offsetY + yDim - j - 1) + newXY*z];
            // }
            // }
            // }

            // Mirror the columns
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // for (j = 0; j < cm2; j++) {
            // aExp[offsetX - j - 1 + newXDim*y + newXY*z] =
            // aExp[offsetX + j + newXDim*y + newXY*z];
            // aExp[offsetX + xDim + j + newXDim*y + newXY*z] =
            // aExp[offsetX + xDim - j - 1 + newXDim*y + newXY*z];
            // }
            // }
            // }

            // Mirror the planes
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (j = 0; j < cm2; j++) {
            // aExp[x + newXDim*y + newXY*(offsetZ - j - 1)] =
            // aExp[x + newXDim*y + newXY*(offsetZ + j)];
            // aExp[x + newXDim*y + newXY*(offsetZ + zDim + j)] =
            // aExp[x + newXDim*y + newXY*(offsetZ + zDim - j - 1)];
            // }
            // }
            // }

            // Mirror 4 xys, 4 xzs, 4 yzs, and 8 xyzs
            // Mirror 4 xys
            // upper left corner
            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y + delY) + newXY*z];
            // }
            // }
            // }

            // upper right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y + delY) + newXY*z];
            // }
            // }
            // }

            // lower left corner
            // for (x = offsetX - cm2, delX = 2*cm2 - 1; x < offsetX; x++, delX -=2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y - delY) + newXY*z];
            // }
            // }
            // }

            // lower right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y - delY) + newXY*z];
            // }
            // }
            // }

            // Mirror 4 xzs
            // upper left corner
            // for (x = offsetX - cm2, delX = 2*cm2 - 1; x < offsetX; x++, delX -=2) {
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*y + newXY*(z + delZ)];
            // }
            // }
            // }

            // upper right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*y + newXY*(z + delZ)];
            // }
            // }
            // }

            // lower left corner
            // for (x = offsetX - cm2, delX = 2*cm2 - 1; x < offsetX; x++, delX -=2) {
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*y + newXY*(z - delZ)];
            // }
            // }
            // }

            // lower right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*y + newXY*(z - delZ)];
            // }
            // }
            // }

            // Mirror 4 yzs
            // upper left corner
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + newXDim*(y + delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // upper right corner
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + newXDim*(y - delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // lower left corner
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x+ newXDim*(y + delY) + newXY*(z - delZ)];
            // }
            // }
            // }

            // lower right corner
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + newXDim*(y - delY) + newXY*(z - delZ)];
            // }
            // }
            // }

            // 8 xyzs
            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y + delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y + delY) + newXY*(z - delZ)];
            // }
            // }
            // }

            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y - delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y - delY) + newXY*(z - delZ)];
            // }
            // }
            // }

            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y + delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y + delY) + newXY*(z - delZ)];
            // }
            // }
            // }

            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y - delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y - delY) + newXY*(z - delZ)];
            // }
            // }
            // }
        } // else if (nDims == 3)

        fireProgressStateChanged("Performing forward wavelet transform");
        fireProgressStateChanged(20);
        transformDir = AlgorithmWaveletThreshold.FORWARD;
        wtn(aExp);

        if (doWaveletImage) {
            waveletImage = new ModelImage(ModelStorageBase.FLOAT, newExtents, "wavelet transform");
            waveletImage.setOriginalExtents(extents);

            try {
                aLog = new float[newArrayLength];
            } catch (final OutOfMemoryError e) {
                aLog = null;
                System.gc();
                displayError("AlgorithmWaveletThreshold: Out of memory creating aLog");

                setCompleted(false);

                return;
            }

            for (int i = 0; i < newArrayLength; i++) {
                aLog[i] = (float) (0.4342944819 * Math.log(1 + Math.abs(aExp[i])));
            }

            try {
                waveletImage.importData(0, aLog, true);
            } catch (final IOException error) {
                displayError("AlgorithmWaveletThreshold: IOException on wavelet image import data");

                setCompleted(false);

                return;
            }
        } // if (doWaveletImage)

        fireProgressStateChanged("Zeroing coefficients below cutoff");
        fireProgressStateChanged(50);
        aMax = -Float.MAX_VALUE;

        for (final float element : aExp) {

            if (Math.abs(element) > aMax) {
                aMax = Math.abs(element);
            }
        }

        belowThreshold = 0;
        aCutoff = aMax * threshold;

        switch (thresholdType) {

            case HARD:
                for (int i = 0; i < aExp.length; i++) {

                    if (Math.abs(aExp[i]) < aCutoff) {
                        aExp[i] = 0.0f;
                        belowThreshold++;
                    }
                }

                break;

            case SOFT:
                for (int i = 0; i < aExp.length; i++) {

                    if (aExp[i] >= aCutoff) {
                        aExp[i] -= aCutoff;
                    } else if (aExp[i] <= -aCutoff) {
                        aExp[i] += aCutoff;
                    } else {
                        aExp[i] = 0.0f;
                        belowThreshold++;
                    }
                }

                break;

            case NONNEGATIVE_GARROTE:
                aCutSquared = aCutoff * aCutoff;
                for (int i = 0; i < aExp.length; i++) {
                    if (Math.abs(aExp[i]) < aCutoff) {
                        aExp[i] = 0.0f;
                        belowThreshold++;
                    } else {
                        aExp[i] -= aCutSquared / aExp[i];
                    }
                }
                break;

            case SCAD:
                for (int i = 0; i < aExp.length; i++) {
                    if (Math.abs(aExp[i]) < aCutoff) {
                        aExp[i] = 0.0f;
                        belowThreshold++;
                    } else if ( (aExp[i] >= aCutoff) && (aExp[i] <= 2 * aCutoff)) {
                        aExp[i] -= aCutoff;
                    } else if ( (aExp[i] <= -aCutoff) && (aExp[i] >= -2 * aCutoff)) {
                        aExp[i] += aCutoff;
                    } else if ( (aExp[i] > 2 * aCutoff) && (aExp[i] <= 3.7 * aCutoff)) {
                        aExp[i] = (float) ( (2.7 * aExp[i] - 3.7 * aCutoff) / 1.7);
                    } else if ( (aExp[i] < -2 * aCutoff) && (aExp[i] >= -3.7 * aCutoff)) {
                        aExp[i] = (float) ( (2.7 * aExp[i] + 3.7 * aCutoff) / 1.7);
                    }
                }
                break;
        } // switch(thresholdType)
        System.out.println(belowThreshold + " of the " + aExp.length + " coefficents are below threshold");

        fireProgressStateChanged("Performing inverse wavelet transform");
        fireProgressStateChanged(60);
        transformDir = AlgorithmWaveletThreshold.INVERSE;
        wtn(aExp);

        // Strip the outer pixels from aExp to return to the originally
        // sized a
        if (nDims == 2) {

            for (x = 0; x < xDim; x++) {

                for (y = 0; y < yDim; y++) {
                    aArray[x + (xDim * y)] = aExp[x + offsetX + (newXDim * (y + offsetY))];
                }
            }
        } // if (nDims == 2)
        else if (nDims == 3) {

            for (x = 0; x < xDim; x++) {

                for (y = 0; y < yDim; y++) {

                    for (z = 0; z < zDim; z++) {
                        aArray[x + (xDim * y) + (xy * z)] = aExp[x + offsetX + (newXDim * (y + offsetY))
                                + (newXY * (z + offsetZ))];
                    }
                }
            }
        } // else if (nDims == 3)

        aExp = null;

        // clamp to mins and maxs allowed by data type
        if (destImage != null) {
            dataType = destImage.getType();
        } else {
            dataType = srcImage.getType();
        }

        if (dataType == ModelStorageBase.UBYTE) {

            for (int i = 0; i < aArray.length; i++) {

                if (aArray[i] > 255.0f) {
                    aArray[i] = 255.0f;
                } else if (aArray[i] < 0.0f) {
                    aArray[i] = 0.0f;
                }
            }
        } // if (dataType == ModelImage.UBYTE)
        else if (dataType == ModelStorageBase.BYTE) {

            for (int i = 0; i < aArray.length; i++) {

                if (aArray[i] > 127.0f) {
                    aArray[i] = 127.0f;
                } else if (aArray[i] < -128.0f) {
                    aArray[i] = -128.0f;
                }
            }
        } // else if (dataType == ModelImage.BYTE)
        else if (dataType == ModelStorageBase.USHORT) {

            for (int i = 0; i < aArray.length; i++) {

                if (aArray[i] > 65535.0f) {
                    aArray[i] = 65535.0f;
                } else if (aArray[i] < 0.0f) {
                    aArray[i] = 0.0f;
                }
            }
        } // else if (dataType == ModelImage.USHORT)
        else if (dataType == ModelStorageBase.SHORT) {

            for (int i = 0; i < aArray.length; i++) {

                if (aArray[i] > 32767.0f) {
                    aArray[i] = 32767.0f;
                } else if (aArray[i] < -32768.0f) {
                    aArray[i] = -32768.0f;
                }
            }
        } // else if (dataType == ModelImage.SHORT)
        else if (dataType == ModelStorageBase.UINTEGER) {

            for (int i = 0; i < aArray.length; i++) {

                if (aArray[i] > 4294967295L) {
                    aArray[i] = 4294967295L;
                } else if (aArray[i] < 0) {
                    aArray[i] = 0;
                }
            }
        } else if (dataType == ModelStorageBase.INTEGER) {

            for (int i = 0; i < aArray.length; i++) {

                if (aArray[i] > Integer.MAX_VALUE) {
                    aArray[i] = Integer.MAX_VALUE;
                }

                if (aArray[i] < Integer.MIN_VALUE) {
                    aArray[i] = Integer.MIN_VALUE;
                }
            }
        }

        fireProgressStateChanged("Importing noise filtered image");
        fireProgressStateChanged(90);

        if (destImage != null) {

            try {
                destImage.importData(0, aArray, true);
            } catch (final IOException error) {
                displayError("AlgorithmWaveletThreshold: IOException on destination image import data");

                setCompleted(false);

                return;
            }
        } else {

            try {
                srcImage.importData(0, aArray, true);
            } catch (final IOException error) {
                displayError("AlgorithmWaveletThreshold: IOException on source image import data");

                setCompleted(false);

                return;
            }

        }

        setCompleted(true);

    }

    /**
     * DOCUMENT ME!
     * 
     * @param a data vector to which Daubechies 4-coefficient wavelet filter or its transpose (for transformDir ==
     *            INVERSE) is applied
     * @param n DOCUMENT ME!
     */
    private void daub4(final float[] a, final int n) {
        double[] wksp;
        int nh, nh1, i, j;

        if (n < 4) {
            MipavUtil.displayError("Length of daub4 array must be at least 4");

            setCompleted(false);

            return;
        }

        wksp = new double[n];
        nh1 = (nh = n >> 1) + 1;

        if (transformDir == AlgorithmWaveletThreshold.FORWARD) { // Apply filter

            for (i = 0, j = 0; j < (n - 3); j += 2, i++) {
                wksp[i] = (AlgorithmWaveletThreshold.C0 * a[j]) + (AlgorithmWaveletThreshold.C1 * a[j + 1])
                        + (AlgorithmWaveletThreshold.C2 * a[j + 2]) + (AlgorithmWaveletThreshold.C3 * a[j + 3]);
                wksp[i + nh] = (AlgorithmWaveletThreshold.C3 * a[j]) - (AlgorithmWaveletThreshold.C2 * a[j + 1])
                        + (AlgorithmWaveletThreshold.C1 * a[j + 2]) - (AlgorithmWaveletThreshold.C0 * a[j + 3]);
            }

            wksp[i] = (AlgorithmWaveletThreshold.C0 * a[n - 2]) + (AlgorithmWaveletThreshold.C1 * a[n - 1])
                    + (AlgorithmWaveletThreshold.C2 * a[0]) + (AlgorithmWaveletThreshold.C3 * a[1]);
            wksp[i + nh] = (AlgorithmWaveletThreshold.C3 * a[n - 2]) - (AlgorithmWaveletThreshold.C2 * a[n - 1])
                    + (AlgorithmWaveletThreshold.C1 * a[0]) - (AlgorithmWaveletThreshold.C0 * a[1]);
        } // if (transformDir == FORWARD)
        else { // Apply transpose filter
            wksp[0] = (AlgorithmWaveletThreshold.C2 * a[nh - 1]) + (AlgorithmWaveletThreshold.C1 * a[n - 1])
                    + (AlgorithmWaveletThreshold.C0 * a[0]) + (AlgorithmWaveletThreshold.C3 * a[nh1 - 1]);
            wksp[1] = (AlgorithmWaveletThreshold.C3 * a[nh - 1]) - (AlgorithmWaveletThreshold.C0 * a[n - 1])
                    + (AlgorithmWaveletThreshold.C1 * a[0]) - (AlgorithmWaveletThreshold.C2 * a[nh1 - 1]);

            for (i = 0, j = 2; i < (nh - 1); i++) {
                wksp[j++] = (AlgorithmWaveletThreshold.C2 * a[i]) + (AlgorithmWaveletThreshold.C1 * a[i + nh])
                        + (AlgorithmWaveletThreshold.C0 * a[i + 1]) + (AlgorithmWaveletThreshold.C3 * a[i + nh1]);
                wksp[j++] = (AlgorithmWaveletThreshold.C3 * a[i]) - (AlgorithmWaveletThreshold.C0 * a[i + nh])
                        + (AlgorithmWaveletThreshold.C1 * a[i + 1]) - (AlgorithmWaveletThreshold.C2 * a[i + nh1]);
            }
        } // else apply transpose filter

        for (i = 0; i < n; i++) {
            a[i] = (float) wksp[i];
        }

        wksp = null;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param a data to which wavelet filter (for transformDir == FORWARD) or to which transpose wavelet filter is
     *            applied (for transformDir == INVERSE)
     * @param n Used hierarchically by routines wt1 and wtn
     */
    private void pwt(final float[] a, final int n) {
        double ai, ai1;
        double[] wksp;
        int i, ii, j, jf, jr, k, n1, ni, nj, nh, nmod;

        if (n < 4) {
            MipavUtil.displayError("Length of pwt array must be at least 4");

            setCompleted(false);

            return;
        }

        wksp = new double[n];
        nmod = cNum * n;
        n1 = n - 1; // Mask of all bits, since n a power of 2
        nh = n >> 1;

        for (j = 0; j < n; j++) {
            wksp[j] = 0.0;
        }

        if (transformDir == AlgorithmWaveletThreshold.FORWARD) { // Apply filter

            for (ii = 1, i = 1; i <= n; i += 2, ii++) {
                ni = i + nmod + ioff; // Pointer to be incremented and wrapped around
                nj = i + nmod + joff;

                for (k = 1; k <= cNum; k++) {
                    jf = n1 & (ni + k); // use bitwise and to wrap around the pointers
                    jr = n1 & (nj + k);
                    wksp[ii - 1] += cc[k] * a[jf];
                    wksp[ii + nh - 1] += cr[k] * a[jr];
                }
            }
        } // if (transformDir == FORWARD)
        else { // Apply transpose filter

            for (ii = 1, i = 1; i <= n; i += 2, ii++) {
                ai = a[ii - 1];
                ai1 = a[ii + nh - 1];
                ni = i + nmod + ioff;
                nj = i + nmod + joff;

                for (k = 1; k <= cNum; k++) {
                    jf = (n1 & (ni + k)) + 1;
                    jr = (n1 & (nj + k)) + 1;
                    wksp[jf - 1] += cc[k] * ai;
                    wksp[jr - 1] += cr[k] * ai1;
                }
            }
        } // else apply transpose filter

        for (j = 0; j < n; j++) {
            a[j] = (float) wksp[j];
        }

        wksp = null;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param a data replaced by its wavelet transform for transformDir == FORWARD or by its inverse wavelet transform
     *            for transformDir == INVERSE One dimensional discrete wavelet transform. Note that the length of a must
     *            be an integer power of 2
     */
    @SuppressWarnings("unused")
    private void wt1(final float[] a) {
        int nn;
        int len;
        len = a.length;

        if (len < 4) {
            MipavUtil.displayError("Length of wt1 array must be at least 4");

            setCompleted(false);

            return;
        }

        if (transformDir == AlgorithmWaveletThreshold.FORWARD) { // wavelet transform

            // Start at larget hierarchy, and work towards smallest
            switch (filterType) {

                case DAUB4:
                    for (nn = len; nn >= 4; nn >>= 1) {
                        daub4(a, nn);
                    }

                    break;

                case PWT:
                    for (nn = len; nn >= 4; nn >>= 1) {
                        pwt(a, nn);
                    }

                    break;
            } // switch(filterType)
        } // if (transformDir == FORWARD)
        else { // inverse wavelet transform

            // Start at smallest hierarchy, and work towards largest
            switch (filterType) {

                case DAUB4:
                    for (nn = 4; nn <= len; nn <<= 1) {
                        daub4(a, nn);
                    }

                    break;

                case PWT:
                    for (nn = 4; nn <= len; nn <<= 1) {
                        pwt(a, nn);
                    }

                    break;
            } // switch(filterType)
        } // else inverse wavelet transform
    }

    /**
     * DOCUMENT ME!
     * 
     * @param a data replaced by its wavelet transform for transformDir == FORWARD or by its inverse wavelet transform
     *            for transformDir == INVERSE n-dimensional discrete wavelet transform. Note that the length of each
     *            dimension of a must be an integer power of 2
     */
    private void wtn(final float[] a) {
        int i1, i2, i3, k, n, nnew, nprev = 1, nt, ntot = 1, idim, ndim;
        float[] wksp;

        ndim = newExtents.length;

        for (idim = 0; idim < ndim; idim++) {
            ntot *= newExtents[idim];
        }

        wksp = new float[ntot];

        for (idim = 1; idim <= ndim; idim++) { // Main loop over dimensions
            n = newExtents[idim - 1];
            nnew = n * nprev;

            if (n > 4) {

                for (i2 = 0; i2 < ntot; i2 += nnew) {

                    for (i1 = 1; i1 <= nprev; i1++) {

                        for (i3 = i1 + i2, k = 1; k <= n; k++, i3 += nprev) {
                            wksp[k - 1] = a[i3 - 1];
                        }

                        // Copy the relevant row or column or etc. into workspace
                        if (transformDir == AlgorithmWaveletThreshold.FORWARD) { // one-dimensional wavelet transform

                            switch (filterType) {

                                case DAUB4:
                                    for (nt = n; nt >= 4; nt >>= 1) {
                                        daub4(wksp, nt);
                                    }

                                    break;

                                case PWT:
                                    for (nt = n; nt >= 4; nt >>= 1) {
                                        pwt(wksp, nt);
                                    }

                                    break;
                            } // switch(filterType)
                        } // if (transformDir == FORWARD)
                        else { // or inverse transform

                            switch (filterType) {

                                case DAUB4:
                                    for (nt = 4; nt <= n; nt <<= 1) {
                                        daub4(wksp, nt);
                                    }

                                    break;

                                case PWT:
                                    for (nt = 4; nt <= n; nt <<= 1) {
                                        pwt(wksp, nt);
                                    }

                                    break;
                            } // switch(filterType)
                        } // else inverse transform

                        for (i3 = i1 + i2, k = 1; k <= n; k++, i3 += nprev) {
                            a[i3 - 1] = wksp[k - 1];
                        }
                        // Copy back from workspace
                    }
                }
            }

            nprev = nnew;
        }

        wksp = null;
    }
}
