package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;
import java.util.Arrays;


/**
 * <p>
 * This is a port of the SUSAN Nonlinear Noise reduction program. SUSAN noise reduction uses nonlinear filtering to
 * reduce noise in an image (2D or 3D) while preserving the underlying structure. It does this by only averaging a voxel
 * with local voxels which have similar intensity. The original code could only handle integer data going from 0 to 255.
 * 2 main changes were made to allow any type of black and white data. In setupBrightness temp =
 * ((float)k)/(float)brightThres); was changed to: temp=(float)((k*(srcMax-srcMin))/(bpSize * brightThres)); In
 * smoothing and smoothing3d bp[Math.abs(center-brightness)] was changed to:
 * bp[(int)Math.abs(256*(center-brightness)/(srcMax - srcMin))]; Also note that using the Math.abs allowed changing the
 * bp allocation from bpSize*2 + 1 to bpSize + 1. In the dialog the default value of brightThres was set equal to 0.1 *
 * (srcMax - srcMin).
 * </p>
 * 
 * <p>
 * The area of a mask excluding the center which has a same or similar intensity value as the center of the mask is
 * known as the USAN. The SUSAN filter works by taking an average of the pixels in the locality which lie in the USAN.
 * The SUSAN filter uses a Gaussian in both the brightness and spatial domains. If useMedian is true, the sums taken
 * over the local neighborhod do not include the center pixel itself. This allows good reduction of impulse noise. If
 * the USAN area is zero and useMedian is true, then the median of the pixel's 8 nearest neighbors in 2D or 26 nearest
 * neighbors in 3D is used to estimate the pixel's correct value. If useMedian is false, then the sums include the
 * center pixel. If useMedian is false and the USAN is zero, then the pixel remains unchanged.
 * </p>
 * 
 * <p>
 * For useMedian == true the complete equation for the SUSAN filter is: J(x,y) = num/denom, where num =
 * sum(i,j)!=(0,0)I(x+i,y+j)*exp[-(r*r/2*sigma*sigma) - (I(x+i,y+j) - I(x,y))**2/thresh*thresh] denom = sum(i,j)!=(0,0)
 * exp[-(r*r/2*sigma*sigma) - (I(x+i,y+j) - I(x,y))**2/thresh*thresh] with r = sqrt(i*i + j*j) and the above replacement
 * rule is used when the denominator or USAN is zero. If useMedian == false, the restriction (i,j) != (0,0) in the above
 * is removed.
 * </p>
 * 
 * <p>
 * The SUSAN filter tends to preserve edges. Pixels are pulled toward the neighboring region to which they are closest
 * in value.
 * </p>
 * 
 * <p>
 * SUSAN Version 2i_medx - nonlinear 2D/3D smoothing
 * </p>
 * 
 * <p>
 * Oxford center for Functional Magnetic Resonance Imaging of the Brain, Department of Clinical Neurology, Oxford
 * University, Oxford, UK (Previously in Computer Vision and Image Processing Group - now Computer Vision and Electro
 * Optics Group - DERA Chertsey, UK) Email: steve@fmrib.ox.ac.uk WWW: http://www.fmrib.ox.ac.uk/~steve
 * </p>
 * 
 * <p>
 * (C) Crown Copyright (1995-1999), Defence Evaluation and Research Agency, Farnborough, Hampshire, GU14 6TD, UK DERA
 * WWW site: http://www.dera.gov.uk/ DERA Computer Vision and Electro Optics Group WWW site:
 * http://www.dera.gov.uk/imageprocessing/dera/group_home.html DERA Computer Vision and Electro Optics Group point of
 * contact: Dr. John Savage, jtsavage@dera.gov.uk, +44 1344 633203
 * </p>
 * 
 * <p>
 * A UK patent has been granted: "Method for digitally processing images to determine the position of edges and/or
 * corners therein for guidance of unmanned vehicle", UK Patent 2272285. Proprietor: Secretary of State for Defence, UK.
 * 15 January 1997
 * </p>
 * 
 * <p>
 * This code is issued for research purposes only and remains the property of the UK Secretary of State for Defence.
 * This code must not be passed on without this header information being kept intact. This code must not be sold.
 * </p>
 * 
 * <p>
 * This 3D version derived from 2d version SUSAN Version 2i SUSAN = Smallest Univalue Segment Assimilating Nucleus
 * Email: steve@fmrib.ox.ac.uk WWW: http://www.fmrib.ox.ac.uk/~steve Related paper: article{Smith97, author = "Smith,
 * S.M. and Brady, J.M.", title = "{SUSAN} - A New Approach to Low Level Image Processing", journal = "Int. Journal of
 * Computer Vision", pages = "45--78", volume = "23", number = "1", month = "May", year = 1997}
 * </p>
 * 
 * <p>
 * To be registered for automatic (bug) updates of SUSAN, send an email.
 * </p>
 * 
 * <p>
 * Note: FDT is the image data type
 * </p>
 * 
 * <p>
 * Note: edge and corner finding have been taken out of the 3D version of SUSAN - it is not thought that they would be
 * of great value.
 * </p>
 * 
 * <p>
 * See following section for different machine information. Please report any bugs (and fixes). There are a few optional
 * changes that can be made in the "defines" section which follows shortly.
 * </p>
 * 
 * <p>
 * This code is written using an emacs folding mode, making moving around the different sections very easy. This is why
 * there are various marks within comments and why comments are indented.
 * </p>
 * 
 * <p>
 * SPATIAL CONTROL: d
 * </p>
 * <p>
 * In SUSAN smoothing d controls the size of the Gaussian mask; its default is 4.0. Increasing d gives more smoothing.
 * In edge finding, a fixed flat mask is used, either 37 pixels arranged in a "circle" (default), or a 3 by 3 mask which
 * gives finer detail. In corner finding, only the larger 37 pixel mask is used; d is not variable. In smoothing, the
 * flat 3 by 3 mask can be used instead of a larger Gaussian mask; this gives low smoothing and fast operation.
 * </p>
 * 
 * <p>
 * BRIGHTNESS CONTROL: t
 * </p>
 * <p>
 * In all three algorithms, t can be varied (default=20); this is the main threshold to be varied. It determines the
 * maximum difference in greylevels between two pixels which allows them to be considered part of the same "region" in
 * the image. Thus it can be reduced to give more edges or corners, i.e. to be more sensitive, and vice versa. In
 * smoothing, reducing t gives less smoothing, and vice versa. Set t=10 for the test image available from the SUSAN web
 * page.
 * 
 * <p>
 * ITERATIONS:
 * </p>
 * <p>
 * With SUSAN smoothing, more smoothing can also be obtained by iterating the algorithm several times. This has a
 * different effect from varying d or t.
 * </p>
 * 
 * <p>
 * BRIGHTNESS FUNCTION LUT IMPLEMENTATION: (Only read this if you are interested in the C implementation)
 * </p>
 * <p>
 * The SUSAN brightness function is implemented as a LUT (Look-Up-Table) for speed. The resulting pointer-based code is
 * a little hard to follow, so here is a brief explanation. In setupBrightness() the LUT is setup. This mallocs enough
 * space for *bp and then repositions the pointer to the center of the malloced space. The SUSAN function e^-(x^6) or
 * e^-(x^2) is calculated and converted to a unsigned char in the range 0-100, for all possible image brightness
 * differences (including negative ones). Thus bp[23] is the output for a brightness difference of 23 greylevels. In the
 * SUSAN algorithms this LUT is used as follows:
 * </p>
 * <p>
 * <code>p=in + (i-3)*x_size + j - 1;</code> p points to the first image pixel in the circular mask surrounding point
 * (x,y).
 * </p>
 * <p>
 * <code>cp=bp + in[i*x_size+j];</code> cp points to a position in the LUT corresponding to the brightness of the
 * center pixel (x,y).
 * </p>
 * <p>
 * now for every pixel within the mask surrounding (x,y), <code>n+=*(cp-*p++);</code> the brightness difference
 * function is found by moving the cp pointer down by an amount equal to the value of the pixel pointed to by p, thus
 * subtracting the two brightness values and performing the exponential function. This value is added to n, the running
 * USAN area.
 * </p>
 * <p>
 * in SUSAN smoothing, the variable height mask is implemented by multiplying the above by the moving mask pointer,
 * reset for each new center pixel. <code>tmp = *dpt++ * *(cp-brightness);</code>
 * </p>
 */
public class AlgorithmNLNoiseReduction extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private byte[] brightnessTable;

    /** DOCUMENT ME! */
    private final double brightThres; // brightness threshold

    /** DOCUMENT ME! */
    private int bTableSize;

    /** DOCUMENT ME! */
    private int dimension; // 2 for slice by slice processing

    // 3 for entire volume processing

    /** DOCUMENT ME! */
    private byte[] gaussMask;

    /** DOCUMENT ME! */
    private float[][] inVolume;

    /** DOCUMENT ME! */
    private float[] inVolume3d;

    /** DOCUMENT ME! */
    private int maskSize, xMaskSize = 1, yMaskSize = 1, zMaskSize = 1;

    /** DOCUMENT ME! */
    private final float maskStdDev; // spatial size of smoothing - maskStdDev

    /** DOCUMENT ME! */
    private float[] medianVal = new float[26];

    /** DOCUMENT ME! */
    private int newProgressValue;

    /** DOCUMENT ME! */
    private int progressValue;

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private int smallSliceSize;

    /** DOCUMENT ME! */
    private double srcMin, srcMax;

    /** DOCUMENT ME! */
    private boolean threeByThree = false;

    /** DOCUMENT ME! */
    private float[] tmpImage;

    /** DOCUMENT ME! */
    private boolean useMedian = true;

    /** DOCUMENT ME! */
    private int volSize;

    /** DOCUMENT ME! */
    private int xSize, ySize, zSize; // image dimensions

    /** DOCUMENT ME! */
    private float xStdDev, yStdDev, zStdDev;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmNLNoiseReduction - Constructor.
     * 
     * @param srcImg source image model
     * @param bt Brightness threshold: this allows discrimination between noise and the underlying image. Ideally, the
     *            value should be set greater than the noise level and less than the contrast of the underlying image.
     *            Edges of contrast smaller than this threshold will be blurred whereas those of greater contrast will
     *            not be. Reducing bt gives less smoothing. bt has a default value = 0.1 * (srcMax - srcMin).
     * @param maskSD This determines the spatial extent of the smoothing. The mask is basically Gaussian with standard
     *            deviation (in image units - e.g. mm.) set by the user. However, for a small, fast, flat response with
     *            a 3x3 or 3x3x3 voxel mask, set maskSD to zero. maskSD has a default value equal to the x resolution.
     * @param useMedian If true, the center pixel is not included in the sums. When the local neighborhood of similar
     *            brightness voxels is empty, a local median filter is used. This allows the correction of
     *            impulse("salt-and-pepper")noise. If false, this feature is turned off. In this case, the center pixel
     *            is included in the sums. When no neighborhood is found, the original intensity of the voxel of
     *            interest remains unchanged.
     * @param img25D Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *            images disregard this flag.
     */
    public AlgorithmNLNoiseReduction(final ModelImage srcImg, final double bt, final float maskSD,
            final boolean useMedian, final boolean img25D) {
        super(null, srcImg);

        brightThres = bt;
        maskStdDev = maskSD;
        this.useMedian = useMedian;
        image25D = img25D;
    }

    /**
     * AlgorithmNLNoiseReduction - Constructor.
     * 
     * @param destImg image model where result image is to stored
     * @param srcImg source image model
     * @param bt Brightness threshold: this allows discrimination between noise and the underlying image. Ideally, the
     *            value should be set greater than the noise level and less than the contrast of the underlying image.
     *            Edges of contrast smaller than this threshold will be blurred whereas those of greater contrast will
     *            not be. Reducing bt gives less smoothing. bt has a default value = 0.1 * (srcMax - srcMin).
     * @param maskSD This determines the spatial extent of the smoothing. The mask is basically Gaussian with standard
     *            deviation (in image units - e.g. mm.) set by the user. However, for a small, fast, flat response with
     *            a 3x3 or 3x3x3 voxel mask, set maskSD to zero. maskSD has a default value equal to the x resolution.
     * @param useMedian If true, the center pixel is not included in the sums. When the local neighborhood of similar
     *            brightness voxels is empty, a local median filter is used. This allows the correction of
     *            impulse("salt-and-pepper")noise. If false, this feature is turned off. In this case, the center pixel
     *            is included in the sums. When no neighborhood is found, the original intensity of the voxel of
     *            interest remains unchanged.
     * @param img25D Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *            images disregard this flag.
     */
    public AlgorithmNLNoiseReduction(final ModelImage destImg, final ModelImage srcImg, final double bt,
            final float maskSD, final boolean useMedian, final boolean img25D) {
        super(destImg, srcImg);

        this.brightThres = bt;
        maskStdDev = maskSD;
        this.useMedian = useMedian;
        image25D = img25D;

    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        destImage = null;
        srcImage = null;
        inVolume = null;
        inVolume3d = null;
        tmpImage = null;
        brightnessTable = null;
        gaussMask = null;
        medianVal = null;

        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Performing the nonlinear noise reduction ...");

        progressValue = 0;

        xSize = srcImage.getExtents()[0];
        ySize = srcImage.getExtents()[1];
        sliceSize = xSize * ySize;

        if (srcImage.getNDims() == 3) {
            zSize = srcImage.getExtents()[2];
            volSize = sliceSize * zSize;
        } else {
            zSize = 1;
        }

        if ( (image25D) || (zSize < 2)) {
            dimension = 2;
        } else {
            dimension = 3;
        }

        if (dimension == 2) { // 2D and 2.5D

            try {
                inVolume = new float[zSize][sliceSize];
            } catch (final OutOfMemoryError e) {
                inVolume = null;
                errorCleanUp("Algorithm NLNoiseReduction: Out of memory", true);

                return;
            }

            for (int z = 0; z < zSize; z++) {

                try {
                    srcImage.exportData(z * sliceSize, sliceSize, inVolume[z]); // locks and releases lock
                } catch (final IOException error) {
                    inVolume = null;
                    errorCleanUp("Algorithm NLNoiseReduction exportData: Image(s) locked", true);

                    return;
                }
            }
        } // if (dimension == 2)
        else { // dimension == 3

            try {
                inVolume3d = new float[volSize];
                srcImage.exportData(0, volSize, inVolume3d); // locks and releases lock
            } catch (final IOException error) {
                inVolume3d = null;
                errorCleanUp("Algorithm NLNoiseReduction exportData: Image(s) locked", true);

                return;
            } catch (final OutOfMemoryError e) {
                inVolume3d = null;
                errorCleanUp("Algorithm NLNoiseReduction: Out of memory", true);

                return;
            }
        } // else dimension == 3

        srcImage.calcMinMax();
        srcMin = srcImage.getMin();
        srcMax = srcImage.getMax();

        xStdDev = maskStdDev / srcImage.getResolutions(0)[0];
        yStdDev = maskStdDev / srcImage.getResolutions(0)[1];

        if ( (xStdDev < 0.5f) || (yStdDev < 0.5f)) {
            threeByThree = true;
        }

        if (srcImage.getNDims() == 3) {
            zStdDev = maskStdDev / srcImage.getResolutions(0)[2];

            if (zStdDev < 0.5f) {
                threeByThree = true;
            }
        }

        if (dimension == 2) {

            if ( !threeByThree) {
                xMaskSize = ((int) (2.0 * xStdDev)) + 1;
                yMaskSize = ((int) (2.0 * yStdDev)) + 1;
            } else {
                yMaskSize = xMaskSize = 3;
            }

            try {
                maskSize = Math.max(xMaskSize, yMaskSize);
                tmpImage = new float[ (xSize + (maskSize * 2)) * (ySize + (maskSize * 2))];
                gaussMask = new byte[ ( (xMaskSize * 2) + 1) * ( (yMaskSize * 2) + 1)];
            } catch (final OutOfMemoryError e) {
                tmpImage = null;
                gaussMask = null;
                errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);

                return;
            }

            // 2D and 2.5D nonlinear smoothing
            for (int z = 0; z < zSize; z++) {
                calcMinMaxSlice(inVolume[z]);
                setupBrightness();
                smoothing2D(z);
            }

            if (threadStopped) {
                finalize();

                return;
            }
        } else { // dimension == 3
            setupBrightness();
            smoothing3D();

            if (threadStopped) {
                finalize();

                return;
            }
        }

        if (destImage != null) {

            if (dimension == 2) {

                for (int z = 0; (z < zSize) && !threadStopped; z++) {

                    try {
                        destImage.importData(z * sliceSize, inVolume[z], false);
                    } catch (final IOException error) {
                        errorCleanUp("Algorithm NLNoiseReduction importData: Image(s) locked", false);

                        return;
                    }
                }

                destImage.calcMinMax();
            } // if (dimension == 2)
            else { // dimension == 3

                try {
                    destImage.importData(0, inVolume3d, true);
                } catch (final IOException error) {
                    errorCleanUp("Algorithm NLNoiseReduction importData: Image locked", true);

                    return;
                }
            } // else dimension == 3
        } // if (destImage != null)
        else { // destImage == null

            if (dimension == 2) {

                for (int z = 0; (z < zSize) && !threadStopped; z++) {

                    try {
                        srcImage.importData(z * sliceSize, inVolume[z], false);
                    } catch (final IOException error) {
                        errorCleanUp("Algorithm NLNoiseReduction importData: Image(s) locked", false);

                        return;
                    }
                }

                srcImage.calcMinMax();
            } // if (dimension == 2)
            else { // dimension == 3

                try {
                    srcImage.importData(0, inVolume3d, true);
                } catch (final IOException error) {
                    errorCleanUp("Algorithm NLNoiseReduction importData: Image locked", false);

                    return;
                }
            } // else dimension == 3
        } // else destImage == null

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);

    }

    /**
     * DOCUMENT ME!
     * 
     * @param in DOCUMENT ME!
     * @param tmpEnImage DOCUMENT ME!
     * @param border DOCUMENT ME!
     */
    private void addBorder(final float[] in, final float[] tmpEnImage, final int border) {
        int i, j;

        for (i = 0; i < ySize; i++) { // copy data into tmpImage

            for (j = 0; j < xSize; j++) {
                tmpEnImage[ ( (i + border) * (xSize + (2 * border))) + border + j] = in[ (i * xSize) + j];
            }
        }

        for (i = 0; i < border; i++) { // copy top and bottom rows; invert as many as necessary

            for (j = 0; j < xSize; j++) {
                tmpEnImage[ ( (border - 1 - i) * (xSize + (2 * border))) + border + j] = in[ (i * xSize) + j];
                tmpEnImage[ ( (ySize + border + i) * (xSize + (2 * border))) + border + j] = in[ ( (ySize - i - 1) * xSize)
                        + j];
            }
        }

        for (i = 0; i < border; i++) { // copy left and right columns

            for (j = 0; j < (ySize + (2 * border)); j++) {
                tmpEnImage[ (j * (xSize + (2 * border))) + border - 1 - i] = tmpEnImage[ (j * (xSize + (2 * border)))
                        + border + i];
                tmpEnImage[ (j * (xSize + (2 * border))) + xSize + border + i] = tmpEnImage[ (j * (xSize + (2 * border)))
                        + xSize + border - 1 - i];
            }
        }

        xSize += 2 * border; // adjust the size of the image
        ySize += 2 * border;
    }

    /**
     * This method adds a border to the input data so that borders can be dealt with easily.
     * 
     * @param data DOCUMENT ME!
     * @param tmpEnImage DOCUMENT ME!
     * @param border DOCUMENT ME!
     */
    private void addBorder3D(final float[] data, final float[] tmpEnImage, final int border) {
        int i, j, k;

        for (k = 0; k < zSize; k++) { // copy data into tmpImage

            for (i = 0; i < ySize; i++) {

                for (j = 0; j < xSize; j++) {
                    tmpEnImage[ ( (k + border) * (xSize + (2 * border)) * (ySize + (2 * border)))
                            + ( (i + border) * (xSize + (2 * border))) + border + j] = data[ (k * xSize * ySize)
                            + (i * xSize) + j];
                }
            }
        }

        for (k = 0; k < zSize; k++) {

            for (i = 0; i < border; i++) { // copy top and bottom rows; invert as many as necessary

                for (j = 0; j < xSize; j++) {
                    tmpImage[ ( (k + border) * (xSize + (2 * border)) * (ySize + (2 * border)))
                            + ( (border - 1 - i) * (xSize + (2 * border))) + border + j] = data[ (k * ySize * xSize)
                            + (i * xSize) + j];
                    tmpImage[ ( (k + border) * (xSize + (2 * border)) * (ySize + (2 * border)))
                            + ( (ySize + border + i) * (xSize + (2 * border))) + border + j] = data[ (k * ySize * xSize)
                            + ( (ySize - i - 1) * xSize) + j];
                }
            }

            for (i = 0; i < border; i++) {

                for (j = 0; j < (ySize + (2 * border)); j++) {
                    tmpImage[ ( (k + border) * (xSize + (2 * border)) * (ySize + (2 * border)))
                            + (j * (xSize + (2 * border))) + border - 1 - i] = tmpImage[ ( (k + border)
                            * (xSize + (2 * border)) * (ySize + (2 * border)))
                            + (j * (xSize + (2 * border))) + border + i];
                    tmpImage[ ( (k + border) * (xSize + (2 * border)) * (ySize + (2 * border)))
                            + (j * (xSize + (2 * border))) + xSize + border + i] = tmpImage[ ( (k + border)
                            * (xSize + (2 * border)) * (ySize + (2 * border)))
                            + (j * (xSize + (2 * border))) + xSize + border - 1 - i];
                }
            }
        }

        for (k = 0; k < border; k++) {

            for (j = 0; j < ( (xSize + (2 * border)) * (ySize + (2 * border))); j++) {
                tmpImage[ ( (border - 1 - k) * (xSize + (2 * border)) * (ySize + (2 * border))) + j] = tmpImage[ ( (border + k)
                        * (xSize + (2 * border)) * (ySize + (2 * border)))
                        + j];
                tmpImage[ ( (zSize + border + k) * (xSize + (2 * border)) * (ySize + (2 * border))) + j] = tmpImage[ ( (zSize
                        + border - 1 - k)
                        * (xSize + (2 * border)) * (ySize + (2 * border)))
                        + j];
            }
        }

        xSize += 2 * border;
        ySize += 2 * border;
        zSize += 2 * border;
    }

    /**
     * Calculates the min and max values for the image array, so that the image is displayed properly.
     * 
     * @param data DOCUMENT ME!
     */
    private void calcMinMaxSlice(final float[] data) {

        float min = Float.POSITIVE_INFINITY;
        float max = Float.NEGATIVE_INFINITY;

        int i;
        float value;

        for (i = 0; i < data.length; i++) {
            value = data[i];

            if (value > max) {
                max = value;
            }

            if (value < min) {
                min = value;
            }
        }

        srcMax = max;
        srcMin = min;
    }

    /**
     * Simple 3x3 median filter.
     * 
     * @param data image data
     * @param x width index into image data
     * @param y height index into image data
     * 
     * @return median value
     */
    private float median2D(final float[] data, final int x, final int y) {
        int i, j;
        float tmp;

        medianVal[0] = data[ ( (x - 1) * xSize) + y - 1];
        medianVal[1] = data[ ( (x - 1) * xSize) + y];
        medianVal[2] = data[ ( (x - 1) * xSize) + y + 1];
        medianVal[3] = data[ ( (x) * xSize) + y - 1];
        medianVal[4] = data[ ( (x) * xSize) + y + 1];
        medianVal[5] = data[ ( (x + 1) * xSize) + y - 1];
        medianVal[6] = data[ ( (x + 1) * xSize) + y];
        medianVal[7] = data[ ( (x + 1) * xSize) + y + 1];

        for (i = 0; i < 7; i++) {

            for (j = 0; j < (7 - i); j++) {

                if (medianVal[j] > medianVal[j + 1]) {
                    tmp = medianVal[j];
                    medianVal[j] = medianVal[j + 1];
                    medianVal[j + 1] = tmp;
                }
            }
        }

        return ( (medianVal[3] + medianVal[4]) / 2.0f);
    }

    /**
     * Simple 3x3x3 median filter.
     * 
     * @param data image data
     * @param x width index into image data
     * @param y height index into image data
     * @param z image slice index into image data
     * 
     * @return median value
     */
    private float median3D(final float[] data, final int x, final int y, final int z) {
        int index1, indexX1, indexX2, indexX3;

        indexX1 = (x - 1) * xSize;
        indexX2 = x * xSize;
        indexX3 = (x + 1) * xSize;

        index1 = (z - 1) * sliceSize;
        medianVal[0] = data[index1 + indexX1 + y - 1];
        medianVal[1] = data[index1 + indexX1 + y];
        medianVal[2] = data[index1 + indexX1 + y + 1];
        medianVal[3] = data[index1 + indexX2 + y - 1];
        medianVal[4] = data[index1 + indexX2 + y];
        medianVal[5] = data[index1 + indexX2 + y + 1];
        medianVal[6] = data[index1 + indexX3 + y - 1];
        medianVal[7] = data[index1 + indexX3 + y];
        medianVal[8] = data[index1 + indexX3 + y + 1];

        index1 = z * sliceSize;
        medianVal[9] = data[index1 + indexX1 + y - 1];
        medianVal[10] = data[index1 + indexX1 + y];
        medianVal[11] = data[index1 + indexX1 + y + 1];
        medianVal[12] = data[index1 + indexX2 + y - 1];
        medianVal[13] = data[index1 + indexX2 + y + 1];
        medianVal[14] = data[index1 + indexX3 + y - 1];
        medianVal[15] = data[index1 + indexX3 + y];
        medianVal[16] = data[index1 + indexX3 + y + 1];

        index1 = (z + 1) * sliceSize;
        medianVal[17] = data[index1 + indexX1 + y - 1];
        medianVal[18] = data[index1 + indexX1 + y];
        medianVal[19] = data[index1 + indexX1 + y + 1];
        medianVal[20] = data[index1 + indexX2 + y - 1];
        medianVal[21] = data[index1 + indexX2 + y];
        medianVal[22] = data[index1 + indexX2 + y + 1];
        medianVal[23] = data[index1 + indexX3 + y - 1];
        medianVal[24] = data[index1 + indexX3 + y];
        medianVal[25] = data[index1 + indexX3 + y + 1];

        Arrays.sort(medianVal);

        return ( (medianVal[12] + medianVal[13]) / 2.0f);
    }

    /**
     * The SUSAN brightness function is implemented as a LUT (Look-Up-Table) for speed. In setupBrightness() the LUT is
     * setup. This creates a 257 entry brightness table. The SUSAN function e^-(x^2) is calculated and converted to a
     * byte in the range 0-100, for all possible image brightness differences (including negative ones). Thus
     * brightnessTable[23] is the output for a brightness difference of 23*(srcMax - srcMin)/256. In the smoothing and
     * smoothing3D algorithms this LUT is used as follows: ip = ((i-yMaskSize)*xSize) + j - xMaskSize; ip = (i-1)*x_size +
     * j - 1; ip points to the first image pixel in the circular mask surrounding point (x,y). ip =
     * (k-zMaskSize)*xSize*ySize + (i-yMaskSize)*xSize + j-xMaskSize; ip = (k-1)*xSize*ySize + (i-1)*xSize + j-1; ip
     * points to the first image voxel in the spherical mask surrounding point (x,y,z). brightness is the intensity
     * value at this mask point i*x_size+j points to the center pixel (x,y). k*xSize*ySize+i*xSize+j points to the
     * center voxel (x,y,z). center is the intensity value of this center point tmp = gaussMask[dpt++] *
     * brightnessTable[(int)Math.abs(256*(center-brightness)/(srcMax - srcMin))]; brightnessTable gives the SUSAN
     * brightness function of the absolute value of the difference of the 2 intensity values. dpt gives the current
     * position inside the mask, so the SUSAN brightness function is multiplied by a Gaussian weighting function. In
     * area += tmp, tmp is added to the area and in total += tmp * brightness, the total area*brightness is calculated.
     * After summing over all mask values and performing median replacement if requested, total is divided by area to
     * obtain the filtered intensity value.
     */
    private void setupBrightness() {
        int i;
        float temp;

        bTableSize = 256;
        brightnessTable = new byte[bTableSize + 1];

        for (i = 0; i <= bTableSize; i++) {
            temp = (float) ( (i * (srcMax - srcMin)) / (bTableSize * brightThres));
            temp = temp * temp;
            brightnessTable[i] = (byte) (100.0 * Math.exp( -temp));
        }
    }

    /**
     * Performs 2D and 2.5 nonlinear smoothing.
     * 
     * @param z The z slice
     */
    private void smoothing2D(final int z) {

        int increment, i, j, x, y, area, tmp;
        int ip;
        float center, brightness;
        int maskPt;
        int dataPt;
        float total;
        float xStdDevLocal, yStdDevLocal;

        // Restore xSize and ySize when performing multiple reps of smoothing
        xSize = srcImage.getExtents()[0];
        ySize = srcImage.getExtents()[1];
        addBorder(inVolume[z], tmpImage, maskSize);

        increment = xSize - ( (xMaskSize * 2) + 1);

        // Gaussian masks
        maskPt = 0;
        xStdDevLocal = - (2 * xStdDev * xStdDev);
        yStdDevLocal = - (2 * yStdDev * yStdDev);

        if (z == 0) {

            for (i = -yMaskSize; i <= yMaskSize; i++) {

                for (j = -xMaskSize; j <= xMaskSize; j++) {
                    gaussMask[maskPt++] = (byte) (100.0 * Math.exp( ( ( (i * i)) / yStdDevLocal)
                            + ( ( (j * j)) / xStdDevLocal)));
                }
            }
        }

        dataPt = 0;

        double index;
        final double constant = 256.0 / (srcMax - srcMin);
        smallSliceSize = (ySize - (2 * maskSize)) * (xSize - (2 * maskSize));

        for (i = maskSize; (i < (ySize - maskSize)) && !threadStopped; i++) { // use maskSize as enlarge was isotropic
            newProgressValue = 100 * ( (z * smallSliceSize) + ( (i - maskSize + 1) * (xSize - (2 * maskSize))))
                    / (zSize * smallSliceSize);

            if (newProgressValue >= (progressValue + 5)) {
                progressValue = newProgressValue;
                fireProgressStateChanged(progressValue);
            }

            for (j = maskSize; (j < (xSize - maskSize)) && !threadStopped; j++) {
                area = 0;
                total = 0;
                maskPt = 0;
                ip = ( (i - yMaskSize) * xSize) + j - xMaskSize;
                center = tmpImage[ (i * xSize) + j];

                for (y = -yMaskSize; y <= yMaskSize; y++) {

                    for (x = -xMaskSize; x <= xMaskSize; x++) {
                        brightness = tmpImage[ip++];
                        index = (center - brightness) * constant;

                        if (index < 0) {
                            index = -index;
                        }

                        tmp = gaussMask[maskPt++] * brightnessTable[(int) index];

                        // tmp = gaussMask[maskPt++] * brightnessTable[(int)Math.abs(256*(center-brightness)/(srcMax
                        // - srcMin))];
                        area += tmp;
                        total += tmp * brightness;
                    }

                    ip += increment;
                }

                if (useMedian) {
                    tmp = area - 10000;

                    if (tmp < 10000) {
                        inVolume[z][dataPt++] = median2D(tmpImage, i, j);
                    } else {
                        inVolume[z][dataPt++] = ( (total - (center * 10000)) / tmp);
                    }
                } else {
                    inVolume[z][dataPt++] = total / area;
                }
            }
        }
    }

    /**
     * Performs 3D nonlinear smoothing.
     */
    private void smoothing3D() {

        int increment, i, j, k, x, y, z, area, tmp;
        float center, brightness;
        int ip;
        int dataPt;
        int maskPt;
        float total;

        if ( !threeByThree) {
            xMaskSize = ((int) (2.0 * xStdDev)) + 1;
            yMaskSize = ((int) (2.0 * yStdDev)) + 1;
            zMaskSize = ((int) (2.0 * zStdDev)) + 1;
        } else {
            zMaskSize = yMaskSize = xMaskSize = 3;
        }

        maskSize = Math.max(Math.max(xMaskSize, yMaskSize), zMaskSize);
        tmpImage = new float[ (xSize + (maskSize * 2)) * (ySize + (maskSize * 2)) * (zSize + (maskSize * 2))];

        addBorder3D(inVolume3d, tmpImage, maskSize);

        increment = xSize - ( (xMaskSize * 2) + 1);

        gaussMask = new byte[ ( (xMaskSize * 2) + 1) * ( (yMaskSize * 2) + 1) * ( (zMaskSize * 2) + 1)];
        maskPt = 0;
        xStdDev = - (2 * xStdDev * xStdDev);
        yStdDev = - (2 * yStdDev * yStdDev);
        zStdDev = - (2 * zStdDev * zStdDev);

        for (k = -zMaskSize; k <= zMaskSize; k++) {

            for (i = -yMaskSize; i <= yMaskSize; i++) {

                for (j = -xMaskSize; j <= xMaskSize; j++) {
                    gaussMask[maskPt++] = (byte) (100.0 * Math.exp( ( ( (i * i)) / xStdDev) + ( ( (j * j)) / yStdDev)
                            + ( ( (k * k)) / zStdDev)));
                }
            }
        }

        dataPt = 0;

        double index;
        final double constant = 256.0 / (srcMax - srcMin);
        int constant1;
        int constant2;
        final int constant3 = (yMaskSize * 2) + 1;
        sliceSize = xSize * ySize;
        smallSliceSize = (ySize - (2 * maskSize)) * (xSize - (2 * maskSize));

        for (k = maskSize; (k < (zSize - maskSize)) && !threadStopped; k++) { // use maskSize as enlarge was isotropic
            constant1 = (k - zMaskSize) * sliceSize;
            constant2 = k * sliceSize;

            for (i = maskSize; (i < (ySize - maskSize)) && !threadStopped; i++) {
                newProgressValue = 100
                        * ( ( (k - maskSize) * smallSliceSize) + ( (i - maskSize) * (xSize - (2 * maskSize))))
                        / ( (zSize - (2 * maskSize)) * smallSliceSize);

                if (newProgressValue >= (progressValue + 5)) {
                    progressValue = newProgressValue;
                    fireProgressStateChanged(progressValue);
                }

                for (j = maskSize; j < (xSize - maskSize); j++) {
                    area = 0;
                    total = 0;
                    maskPt = 0;
                    ip = constant1 + ( (i - yMaskSize) * xSize) + j - xMaskSize;
                    center = tmpImage[constant2 + (i * xSize) + j];

                    for (z = -zMaskSize; z <= zMaskSize; z++) {

                        for (y = -yMaskSize; y <= yMaskSize; y++) {

                            for (x = -xMaskSize; x <= xMaskSize; x++) {
                                brightness = tmpImage[ip++];
                                index = (center - brightness) * constant;

                                if (index < 0) {
                                    index = -index;
                                }

                                tmp = gaussMask[maskPt++] * brightnessTable[(int) index];

                                // brightnessTable[(int)Math.abs(256*(center-brightness)/(srcMax - srcMin))];
                                area += tmp;
                                total += tmp * brightness;
                            }

                            ip += increment;
                        }

                        ip += xSize * (ySize - constant3);
                    }

                    if (useMedian) {
                        tmp = area - 10000;

                        if (tmp < 20000) {
                            inVolume3d[dataPt++] = median3D(tmpImage, i, j, k);
                        } else {
                            inVolume3d[dataPt++] = (total - (center * 10000)) / tmp;
                        }
                    } else {
                        inVolume3d[dataPt++] = total / area;
                    }
                }
            }
        }
    }

}
