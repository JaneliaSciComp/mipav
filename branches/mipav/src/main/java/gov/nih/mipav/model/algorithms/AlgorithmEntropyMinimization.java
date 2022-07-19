package gov.nih.mipav.model.algorithms;

import de.jtem.numericalMethods.calculus.function.RealFunctionOfSeveralVariables;
import de.jtem.numericalMethods.calculus.minimizing.Powell;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmEntropyMinimization extends AlgorithmBase implements RealFunctionOfSeveralVariables {
    // This software performs retrospective shading correction based on entropy minimization. If the image is color, the
    // colors are independently processed one at a time and then recombined. The user can select either both
    // multiplicative and additive quadratic noise, multiplicative quadratic noise only, or cubic multiplicative noise
    // only.  Quartic multiplicative noise could not be made to work. Shading distorts an image with spurious intensity
    // variations not present in the original object. These intensity variations can be caused by nonuniform
    // illumination, imperfections in the optics, uneven camera sensitivity, or imperfect specimen preparation.  Let
    // U(x,y) represent the true image, N(x,y) represent the acquired image, SM(x,y) represent the multiplicative
    // shading component, and SA(x,y) represent the additive shading component.  Then: N(x,y) = U(x,y) * SM(x,y) +
    // SA(x,y) Shading correction calculates the corrected image U(x,y) with U(x,y) = (N(x,y) - SA(x,y))/SM(x,y) SA(x,y)
    // and SM(x,y) are modeled by second-order polynomials For 2D: SA(x,y) = a1*(x - ac1)/ad1+ a2*(y -ac2)/ad2 + a3*(x*y
    // -ac3)/ad3           + a4*(x*x - ac4)/ad4 + a5*(y*y - ac5)/ad5 SM(x,y) = 1 + m1*(x - mc1)/md1 + m2*(y - mc2)/md2 +
    // m3*(x*y -mc3)/md3           + m4*(x*x - mc4)/md4 + m5*(y*y - mc5)/md5 The software clamps SA(x,y) to be between
    // -image maximum/2 and +image maximum/2.  The software clamps SM(x,y) to be between 0.05 and 20.0.

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * The user can choose to use thresholding to exclude the background from any processing. Excluded pixels will have
     * the same values as the original source image. All pixels with values below threshold that are 4 or 6 neighbor
     * connected thru below threshold pixels to a boundary pixel which is below threshold will be excluded from
     * processing. By default image subsampling is used in histogram formation since using 5000 images samples is
     * sufficient to form the histogram. If subsampling is selected, pixelIncrement for both x and y is set equal to:
     * Math.max(1,(int)Math.sqrt(histogramAvailable/5000)) If subsampling is not selected, pixelIncrement equals 1
     * Powell's algorithm is used to search for the values of the 10 shading parameters that minimize the entropy of the
     * blurred histogram of the U(x,y) image. A histogram is formed to represent integers from the floor(image U
     * minimum) to the ceiling(image U maximum). For each floating point gray value gp in U(x,y) a count of 1 is split
     * between the 2 integer bins encompassing it. Let the bins be k and k+1. k+1-gp is added to bin k and gp - k is
     * added to bin k+1. Gray values in locations corresponding to the image minimum or image maximum in the original
     * input image are not used in creating this histogram, since they may represent regions where saturation effects
     * are occurring. Then, a blurred histogram is created from the original histogram. The original histogram is
     * blurred with a triangular window of size 5: blurredHistogram[i] = originalHistogram[i-2] + 2*
     * originalHistogram[i-1] + 3*originalHistogram[i] + 2*originalHistogram[i+1] + originalHistogram[i+2] The blurred
     * histogram is normalized to give probabilities by dividing the count in each bin by the total number of counts in
     * the blurred histogram. The entropy is formed by summing the -probability(n)*log(probability(n)) over all bins n
     * with nonzero counts. References: This software implements the algorithm discussed in: 1.) Retrospective shading
     * correction based on entropy minimization by B. Likar, J. B. A. Maintz, M. A. Viergever, and F. Pernus, Journal of
     * Microscopy, Vol. 197, Pt 3., March, 2000, pp. 285-295. 2.) Retrospective Correction of MR Intensity Inhomogeneity
     * by Information Minimization by Bostjan Likar, Max Viergever, and Franjo Pernus, IEEE Transactions on Medical
     * Imaging, Vol. 20, No. 12, December, 2001, pp. 1398-1410. 3.) Real-time automated visual inspection of color
     * tablets in pharmaceutical blisters by Joze Derganc, Bostjan Likar, Rok Bernard, Dejan Tomazevic, and Franjo
     * Pernus, Real-Time Imaging, Vol. 9, 2003, pp. 113-124. 
     */
    public static final int NOISE_MA2 = 1;

    /** Quadratic multiplicative noise. */
    public static final int NOISE_M2 = 2;

    /** Cubic multiplicative noise. */
    public static final int NOISE_M3 = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double ac1, ac2, ac3, ac4, ac5, ac6, ac7, ac8, ac9;

    /** DOCUMENT ME! */
    private double ad1, ad2, ad3, ad4, ad5, ad6, ad7, ad8, ad9;

    /** DOCUMENT ME! */
    private int area;

    /** DOCUMENT ME! */
    private boolean backgroundPresent = false;

    /** DOCUMENT ME! */
    private float[] buffer;

    /** DOCUMENT ME! */
    private double grayCount;

    /** Number of pixels available for histogram formation. */
    private int histogramAvailable;

    /** Buffer holding calculation for noiseless U(x,y). */
    private double[] idealBuffer;

    /** DOCUMENT ME! */
    private int iter;

    /** original image maximum. */
    private double maximum;

    /** DOCUMENT ME! */
    private double maximumR, maximumG, maximumB;

    /** DOCUMENT ME! */
    private double mc1, mc2, mc3, mc4, mc5, mc6, mc7, mc8, mc9, mc10, mc11, mc12, mc13, mc14, mc15, mc16, mc17, mc18,
                   mc19;

    /** DOCUMENT ME! */
    private double md1, md2, md3, md4, md5, md6, md7, md8, md9, md10, md11, md12, md13, md14, md15, md16, md17, md18,
                   md19;

    /** original image minimum. */
    private double minimum;

    /** DOCUMENT ME! */
    private double minimumR, minimumG, minimumB;

    /** NOISE_MA2 or NOISE_M2. */
    private int noiseType;

    /** Numbering of shading correction parameters. */
    private int nParams;

    /** DOCUMENT ME! */
    private boolean[] objectBuffer;

    /** pixelIncrement is increased above 1 if subsampling. */
    private int pixelIncrement = 1;
    
    /** Handle case where zDim is far less than xDim and yDim */
    private int zPixelIncrement = 1;

    /** Tolerance passed to Powell's algorithm. */
    private double powellTolerance = 1.0e-6;

    /** xDim * yDim. */
    private int sliceSize;

    /** If true, subsample image for histogram formation. */
    private boolean subsample;

    /** DOCUMENT ME! */
    private float thresholdLevel;

    /**
     * If true don't use pixels below thresholdLevel that are 4 or 6 neighbor connected to boundary pixels below
     * thresholdLevel.
     */
    private boolean thresholdSelected;

    /** True for source input locations not having image min or image max values. */
    private boolean[] validBuffer;

    /** DOCUMENT ME! */
    private int volSize;

    /** DOCUMENT ME! */
    private int volume;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private int zDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmEntropyMinimization object.
     *
     * @param  srcImage           original image
     * @param  thresholdSelected  If true, don't use pixels that are 4 or 6 neighbor connected to boundary pixels below
     *                            thresholdLevel
     * @param  thresholdLevel     DOCUMENT ME!
     * @param  subsample          DOCUMENT ME!
     * @param  noiseType          DOCUMENT ME!
     */
    public AlgorithmEntropyMinimization(ModelImage srcImage, boolean thresholdSelected, float thresholdLevel,
                                        boolean subsample, int noiseType) {

        super(null, srcImage);
        this.thresholdSelected = thresholdSelected;
        this.thresholdLevel = thresholdLevel;
        this.subsample = subsample;
        this.noiseType = noiseType;
    }


    /**
     * Creates a new AlgorithmEntropyMinimization object.
     *
     * @param  resultImage        entropy minimized image
     * @param  srcImage           original image
     * @param  thresholdSelected  If true, don't use pixels that are 4 or 6 neighbor connected to boundary pixels below
     *                            thresholdLevel
     * @param  thresholdLevel     DOCUMENT ME!
     * @param  subsample          DOCUMENT ME!
     * @param  noiseType          DOCUMENT ME!
     */
    public AlgorithmEntropyMinimization(ModelImage resultImage, ModelImage srcImage, boolean thresholdSelected,
                                        float thresholdLevel, boolean subsample, int noiseType) {

        super(resultImage, srcImage);
        this.thresholdSelected = thresholdSelected;
        this.thresholdLevel = thresholdLevel;
        this.subsample = subsample;
        this.noiseType = noiseType;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        destImage = null;
        buffer = null;
        validBuffer = null;
        idealBuffer = null;
        objectBuffer = null;
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {

        if (srcImage.getNDims() == 2) {

            if (noiseType == NOISE_MA2) {
                nParams = 10;
                run2Da2m2();
            } else if (noiseType == NOISE_M2) {
                nParams = 5;
                run2Dm2();
            } else if (noiseType == NOISE_M3) {
                nParams = 9;
                run2Dm3();
            }
        } // if (srcImage.getNDims() == 2)
        else if (srcImage.getNDims() == 3) {

            if (noiseType == NOISE_MA2) {
                nParams = 18;
                run3Da2m2();
            } else if (noiseType == NOISE_M2) {
                nParams = 9;
                run3Dm2();
            } else if (noiseType == NOISE_M3) {
                nParams = 19;
                run3Dm3();
            }
        } // else if (srcImage.getNDims() == 3)
    }


    /**
     * DOCUMENT ME!
     *
     * @param   p  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double entropyFunction(double[] p) {       
        double a1, a2, a3, a4, a5, a6, a7, a8, a9;
        double m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19;
        int x;
        int y;
        int xx, yy, xy;
        int z;
        int i;
        int j;
        int k;
        double say;
        double smy;
        double saz;
        double smz;
        double sa;
        double sm;
        double gmin = Double.MAX_VALUE;
        double gmax = -Double.MAX_VALUE;
        int gbottom;
        int gtop;
        int glow;
        int ghigh;
        int histogramLength;
        double[] originalHistogram;
        double[] blurredHistogram;
        double histogramTotal;
        int probabilityLength;
        double[] probability;
        double entropy;

        if (srcImage.getNDims() == 2) {

            if (noiseType == NOISE_MA2) {
                a1 = p[0];
                a2 = p[1];
                a3 = p[2];
                a4 = p[3];
                a5 = p[4];
                m1 = p[5];
                m2 = p[6];
                m3 = p[7];
                m4 = p[8];
                m5 = p[9];

                for (y = 0; y < yDim; y += pixelIncrement) {
                    yy = y * y;
                    j = y * xDim;
                    say = (a2 * (y - ac2) / ad2) + (a5 * (yy - ac5) / ad5);
                    smy = 1.0 + (m2 * (y - mc2) / md2) + (m5 * (yy - mc5) / md5);

                    for (x = 0; x < xDim; x += pixelIncrement) {
                        xx = x * x;
                        xy = x * y;
                        i = j + x;

                        if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                            sa = say + (a1 * (x - ac1) / ad1) + (a3 * (xy - ac3) / ad3) + (a4 * (xx - ac4) / ad4);

                            if (sa < (-maximum / 2.0)) {
                                sa = -maximum / 2.0;
                            } else if (sa > (maximum / 2.0)) {
                                sa = maximum / 2.0;
                            }

                            sm = smy + (m1 * (x - mc1) / md1) + (m3 * (xy - mc3) / md3) + (m4 * (xx - mc4) / md4);

                            if (sm < 0.05) {
                                sm = 0.05;
                            } else if (sm > 20.0) {
                                sm = 20.0;
                            }

                            idealBuffer[i] = (buffer[i] - sa) / sm;

                            if (idealBuffer[i] < gmin) {
                                gmin = idealBuffer[i];
                            }

                            if (idealBuffer[i] > gmax) {
                                gmax = idealBuffer[i];
                            }
                        } // if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i]))
                    } // for (x = 0; x < xDim; x += pixelIncrement)
                } // for (y = 0; y < yDim; y += pixelIncrement)
            } // if (noiseType == NOISE_MA2)
            else if (noiseType == NOISE_M2) {
                m1 = p[0];
                m2 = p[1];
                m3 = p[2];
                m4 = p[3];
                m5 = p[4];

                for (y = 0; y < yDim; y += pixelIncrement) {
                    j = y * xDim;
                    smy = 1.0 + (m2 * (y - mc2) / md2) + (m5 * ((y * y) - mc5) / md5);

                    for (x = 0; x < xDim; x += pixelIncrement) {
                        i = j + x;

                        if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                            sm = smy + (m1 * (x - mc1) / md1) + (m3 * ((x * y) - mc3) / md3) +
                                 (m4 * ((x * x) - mc4) / md4);

                            if (sm < 0.05) {
                                sm = 0.05;
                            } else if (sm > 20.0) {
                                sm = 20.0;
                            }

                            idealBuffer[i] = buffer[i] / sm;

                            if (idealBuffer[i] < gmin) {
                                gmin = idealBuffer[i];
                            }

                            if (idealBuffer[i] > gmax) {
                                gmax = idealBuffer[i];
                            }
                        } // if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i]))
                    } // for (x = 0; x < xDim; x += pixelIncrement)
                } // for (y = 0; y < yDim; y += pixelIncrement)
            } // else if (noiseType == NOISE_M2)
            else if (noiseType == NOISE_M3) {
                m1 = p[0];
                m2 = p[1];
                m3 = p[2];
                m4 = p[3];
                m5 = p[4];
                m6 = p[5];
                m7 = p[6];
                m8 = p[7];
                m9 = p[8];

                for (y = 0; y < yDim; y += pixelIncrement) {
                    yy = y * y;
                    j = y * xDim;
                    smy = 1.0 + (m2 * (y - mc2) / md2) + (m5 * (yy - mc5) / md5) + (m9 * ((yy * y) - mc9) / md9);

                    for (x = 0; x < xDim; x += pixelIncrement) {
                        xx = x * x;
                        xy = x * y;
                        i = j + x;

                        if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                            sm = smy + (m1 * (x - mc1) / md1) + (m3 * (xy - mc3) / md3) + (m4 * (xx - mc4) / md4) +
                                 (m6 * ((xx * x) - mc6) / md6) + (m7 * ((x * xy) - mc7) / md7) +
                                 (m8 * ((xy * y) - mc8) / md8);

                            if (sm < 0.05) {
                                sm = 0.05;
                            } else if (sm > 20.0) {
                                sm = 20.0;
                            }

                            idealBuffer[i] = buffer[i] / sm;

                            if (idealBuffer[i] < gmin) {
                                gmin = idealBuffer[i];
                            }

                            if (idealBuffer[i] > gmax) {
                                gmax = idealBuffer[i];
                            }
                        } // if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i]))
                    } // for (x = 0; x < xDim; x += pixelIncrement)
                } // for (y = 0; y < yDim; y += pixelIncrement)

            } // else if (noiseType == NOISE_M3)

            gbottom = (int) Math.floor(gmin);
            gtop = (int) Math.ceil(gmax);
            histogramLength = gtop - gbottom + 1;

            // Preferences.debug("gmin = " + gmin + " gmax = "+ gmax + "\n", Preferences.DEBUG_ALGORITHM);
            originalHistogram = new double[histogramLength];

            for (y = 0; y < yDim; y += pixelIncrement) {
                j = y * xDim;

                for (x = 0; x < xDim; x += pixelIncrement) {
                    i = j + x;

                    if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                        glow = (int) idealBuffer[i];
                        ghigh = glow + 1;
                        originalHistogram[glow - gbottom] += ghigh - idealBuffer[i];

                        if (ghigh <= gtop) {
                            originalHistogram[ghigh - gbottom] += idealBuffer[i] - glow;
                        }
                    } // if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i]))
                } // for (x = 0; x < xDim; x += pixelIncrement)
            } // for (y = 0; y < yDim; y += pixelIncrement)
        } // if (srcImage.getNDims() == 2)
        else { // srcImage.getNDims() == 3

            if (noiseType == NOISE_MA2) {
                a1 = p[0];
                a2 = p[1];
                a3 = p[2];
                a4 = p[3];
                a5 = p[4];
                a6 = p[5];
                a7 = p[6];
                a8 = p[7];
                a9 = p[8];
                m1 = p[9];
                m2 = p[10];
                m3 = p[11];
                m4 = p[12];
                m5 = p[13];
                m6 = p[14];
                m7 = p[15];
                m8 = p[16];
                m9 = p[17];

                for (z = 0; z < zDim; z += zPixelIncrement) {
                    k = z * sliceSize;
                    saz = (a3 * (z - ac3) / ad3) + (a9 * ((z * z) - ac9) / ad9);
                    smz = 1.0 + (m3 * (z - mc3) / md3) + (m9 * ((z * z) - mc9) / md9);

                    for (y = 0; y < yDim; y += pixelIncrement) {
                        j = k + (y * xDim);
                        say = saz + (a2 * (y - ac2) / ad2) + (a8 * ((y * y) - ac8) / ad8) +
                              (a6 * ((y * z) - ac6) / ad6);
                        smy = smz + (m2 * (y - mc2) / md2) + (m8 * ((y * y) - mc8) / md8) +
                              (m6 * ((y * z) - mc6) / md6);

                        for (x = 0; x < xDim; x += pixelIncrement) {
                            i = j + x;

                            if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                                sa = say + (a1 * (x - ac1) / ad1) + (a4 * ((x * y) - ac4) / ad4) +
                                     (a7 * ((x * x) - ac7) / ad7) + (a5 * ((x * z) - ac5) / ad5);

                                if (sa < (-maximum / 2.0)) {
                                    sa = -maximum / 2.0;
                                } else if (sa > (maximum / 2.0)) {
                                    sa = maximum / 2.0;
                                }

                                sm = smy + (m1 * (x - mc1) / md1) + (m4 * ((x * y) - mc4) / md4) +
                                     (m7 * ((x * x) - mc7) / md7) + (m5 * ((x * z) - mc5) / md5);

                                if (sm < 0.05) {
                                    sm = 0.05;
                                } else if (sm > 20.0) {
                                    sm = 20.0;
                                }

                                idealBuffer[i] = (buffer[i] - sa) / sm;

                                if (idealBuffer[i] < gmin) {
                                    gmin = idealBuffer[i];
                                }

                                if (idealBuffer[i] > gmax) {
                                    gmax = idealBuffer[i];
                                }
                            } // if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i]))
                        } // for (x = 0; x < xDim; x += pixelIncrement)
                    } // for (y = 0; y < yDim; y += pixelIncrement)
                } // for (z = 0; z < zDim; z += zPixelIncrement)
            } // if (noiseType == NOISE_MA2)
            else if (noiseType == NOISE_M2) {
                m1 = p[0];
                m2 = p[1];
                m3 = p[2];
                m4 = p[3];
                m5 = p[4];
                m6 = p[5];
                m7 = p[6];
                m8 = p[7];
                m9 = p[8];

                for (z = 0; z < zDim; z += zPixelIncrement) {
                    k = z * sliceSize;
                    smz = 1.0 + (m3 * (z - mc3) / md3) + (m9 * ((z * z) - mc9) / md9);

                    for (y = 0; y < yDim; y += pixelIncrement) {
                        j = k + (y * xDim);
                        smy = smz + (m2 * (y - mc2) / md2) + (m8 * ((y * y) - mc8) / md8) +
                              (m6 * ((y * z) - mc6) / md6);

                        for (x = 0; x < xDim; x += pixelIncrement) {
                            i = j + x;

                            if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                                sm = smy + (m1 * (x - mc1) / md1) + (m4 * ((x * y) - mc4) / md4) +
                                     (m7 * ((x * x) - mc7) / md7) + (m5 * ((x * z) - mc5) / md5);

                                if (sm < 0.05) {
                                    sm = 0.05;
                                } else if (sm > 20.0) {
                                    sm = 20.0;
                                }

                                idealBuffer[i] = buffer[i] / sm;

                                if (idealBuffer[i] < gmin) {
                                    gmin = idealBuffer[i];
                                }

                                if (idealBuffer[i] > gmax) {
                                    gmax = idealBuffer[i];
                                }
                            } // if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i]))
                        } // for (x = 0; x < xDim; x += pixelIncrement)
                    } // for (y = 0; y < yDim; y += pixelIncrement)
                } // for (z = 0; z < zDim; z += zPixelIncrement)
            } // else if (noiseType == NOISE_M2)
            else if (noiseType == NOISE_M3) {
                m1 = p[0];
                m2 = p[1];
                m3 = p[2];
                m4 = p[3];
                m5 = p[4];
                m6 = p[5];
                m7 = p[6];
                m8 = p[7];
                m9 = p[8];
                m10 = p[9];
                m11 = p[10];
                m12 = p[11];
                m13 = p[12];
                m14 = p[13];
                m15 = p[14];
                m16 = p[15];
                m17 = p[16];
                m18 = p[17];
                m19 = p[18];

                for (z = 0; z < zDim; z += zPixelIncrement) {
                    k = z * sliceSize;
                    smz = 1.0 + (m3 * (z - mc3) / md3) + (m9 * ((z * z) - mc9) / md9) +
                          (m19 * ((z * z * z) - mc19) / md19);

                    for (y = 0; y < yDim; y += pixelIncrement) {
                        j = k + (y * xDim);
                        smy = smz + (m2 * (y - mc2) / md2) + (m8 * ((y * y) - mc8) / md8) +
                              (m6 * ((y * z) - mc6) / md6) + (m16 * ((y * y * y) - mc16) / md16) +
                              (m17 * ((y * y * z) - mc17) / md17) + (m18 * ((y * z * z) - mc18) / md18);

                        for (x = 0; x < xDim; x += pixelIncrement) {
                            i = j + x;

                            if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                                sm = smy + (m1 * (x - mc1) / md1) + (m4 * ((x * y) - mc4) / md4) +
                                     (m7 * ((x * x) - mc7) / md7) + (m5 * ((x * z) - mc5) / md5) +
                                     (m10 * ((x * x * x) - mc10) / md10) + (m11 * ((x * x * y) - mc11) / md11) +
                                     (m12 * ((x * x * z) - mc12) / md12) + (m13 * ((x * y * y) - mc13) / md13) +
                                     (m14 * ((x * y * z) - mc14) / md14) + (m15 * ((x * z * z) - mc15) / md15);

                                if (sm < 0.05) {
                                    sm = 0.05;
                                } else if (sm > 20.0) {
                                    sm = 20.0;
                                }

                                idealBuffer[i] = buffer[i] / sm;

                                if (idealBuffer[i] < gmin) {
                                    gmin = idealBuffer[i];
                                }

                                if (idealBuffer[i] > gmax) {
                                    gmax = idealBuffer[i];
                                }
                            } // if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i]))
                        } // for (x = 0; x < xDim; x += pixelIncrement)
                    } // for (y = 0; y < yDim; y += pixelIncrement)
                } // for (z = 0; z < zDim; z += zPixelIncrement)
            } // else if (noiseType == NOISE_M3)

            gbottom = (int) Math.floor(gmin);
            gtop = (int) Math.ceil(gmax);
            histogramLength = gtop - gbottom + 1;

            // Preferences.debug("gmin = " + gmin + " gmax = "+ gmax + "\n", Preferences.DEBUG_ALGORITHM);
            originalHistogram = new double[histogramLength];

            for (z = 0; z < zDim; z += zPixelIncrement) {
                k = z * sliceSize;

                for (y = 0; y < yDim; y += pixelIncrement) {
                    j = k + (y * xDim);

                    for (x = 0; x < xDim; x += pixelIncrement) {
                        i = j + x;

                        if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                            glow = (int) idealBuffer[i];
                            ghigh = glow + 1;
                            originalHistogram[glow - gbottom] += ghigh - idealBuffer[i];

                            if (ghigh <= gtop) {
                                originalHistogram[ghigh - gbottom] += idealBuffer[i] - glow;
                            }
                        } // if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i]))
                    } // for (x = 0; x < xDim; x += pixelIncrement)
                } // for (y = 0; y < yDim; y += pixelIncrement)
            } // for (z = 0; z < zDim; z += zPixelIncrement)
        } // else srcImage.getNDims() == 3

        blurredHistogram = new double[histogramLength];
        blurredHistogram[0] = 1.5 *
                                  ((3.0 * originalHistogram[0]) + (2.0 * originalHistogram[1]) + originalHistogram[2]);
        blurredHistogram[1] = (9.0 / 8.0) *
                                  ((2.0 * originalHistogram[0]) + (3.0 * originalHistogram[1]) +
                                       (2.0 * originalHistogram[2]) + originalHistogram[3]);

        for (i = 2; i < (histogramLength - 2); i++) {
            blurredHistogram[i] = originalHistogram[i - 2] + (2.0 * originalHistogram[i - 1]) +
                                  (3.0 * originalHistogram[i]) + (2.0 * originalHistogram[i + 1]) +
                                  originalHistogram[i + 2];
        }

        blurredHistogram[histogramLength - 2] = (9.0 / 8.0) *
                                                    (originalHistogram[histogramLength - 4] +
                                                         (2.0 * originalHistogram[histogramLength - 3]) +
                                                         (3.0 * originalHistogram[histogramLength - 2]) +
                                                         (2.0 * originalHistogram[histogramLength - 1]));
        blurredHistogram[histogramLength - 1] = 1.5 *
                                                    (originalHistogram[histogramLength - 3] +
                                                         (2.0 * originalHistogram[histogramLength - 2]) +
                                                         (3.0 * originalHistogram[histogramLength - 1]));

        histogramTotal = 0.0;
        probabilityLength = 0;

        for (i = 0; i < histogramLength; i++) {

            if (blurredHistogram[i] > 0.0) {
                histogramTotal += blurredHistogram[i];
                probabilityLength++;
            }
        }

        probability = new double[probabilityLength];

        for (i = 0, j = 0; i < histogramLength; i++) {

            if (blurredHistogram[i] > 0.0) {
                probability[j++] = blurredHistogram[i] / histogramTotal;
            }
        }

        entropy = 0.0;

        for (i = 0; i < probabilityLength; i++) {
            entropy -= probability[i] * Math.log(probability[i]);
        }
        return entropy;
    }

    /**
     * Minimization of entropy function of nParams variables. Input consists of an initial starting point
     * p[0...nParams-1]; an initial matrix xi[0...nParams-1][0...nParams-1], whose columns contain the initial set of
     * directions (usually the nParams unit vectors); and ftol, the fractional tolerance in the function value such that
     * failure to decrease by more than this amount on one iteration signals doneness. On output, p is set to the best
     * point found, xi is the then-current direction set, fret is the returned function value at p, and iter is the
     * number of iterations taken.
     *
     * @param  p     DOCUMENT ME!
     * @param  xi    DOCUMENT ME!
     * @param  ftol  DOCUMENT ME!
     */
    private void powell(double[] p, double[][] xi, double ftol) {
        int ITMAX = 2000000000;
        Powell.search( p, xi, ftol, this, ITMAX, null );
    }

    /**
     * DOCUMENT ME!
     */
    private void run2Da2m2() {
        int i;
        double[] p = new double[nParams];
        double[][] xi = new double[nParams][nParams];
        double a1, a2, a3, a4, a5;
        double m1, m2, m3, m4, m5;
        int x;
        int y;
        int j;
        int c;
        double say;
        double smy;
        double sa;
        double sm;
        boolean found;
        int imageType;
        boolean doRed = false;
        boolean doGreen = false;
        boolean doBlue = false;
        int maxColor = 1;

        fireProgressStateChanged(srcImage.getImageName(), "Performing entropy minimization...");
        

        srcImage.calcMinMax();
        minimum = srcImage.getMin();
        maximum = srcImage.getMax();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        area = sliceSize;

        if (srcImage.isColorImage()) {
            minimumR = srcImage.getMinR();
            maximumR = srcImage.getMaxR();

            if (minimumR != maximumR) {
                doRed = true;
            }

            minimumG = srcImage.getMinG();
            maximumG = srcImage.getMaxG();

            if (minimumG != maximumG) {
                doGreen = true;
                maxColor = 2;
            }

            minimumB = srcImage.getMinB();
            maximumB = srcImage.getMaxB();

            if (minimumB != maximumB) {
                doBlue = true;
                maxColor = 3;
            }
        } // if srcImage.isColorImage())

        try {
            buffer = new float[sliceSize];
            idealBuffer = new double[sliceSize];
            validBuffer = new boolean[sliceSize];

            for (c = 1; c <= maxColor; c++) {

                if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) || ((c == 2) && (doGreen)) ||
                        ((c == 3) && (doBlue))) {

                    if (srcImage.isColorImage()) {
                        srcImage.exportRGBData(c, 0, sliceSize, buffer);

                        if (c == 1) {
                            minimum = minimumR;
                            maximum = maximumR;
                            fireProgressStateChanged("Performing red entropy minimization");
                        } else if (c == 2) {
                            minimum = minimumG;
                            maximum = maximumG;
                            fireProgressStateChanged("Performing green entropy minimization");
                        } else if (c == 3) {
                            minimum = minimumB;
                            maximum = maximumB;
                            fireProgressStateChanged("Performing blue entropy minimization");
                        }
                    } // if (srcImage.isColorImage())
                    else {
                        srcImage.exportData(0, sliceSize, buffer);
                    }

                    if (thresholdSelected) {
                        objectBuffer = new boolean[sliceSize];

                        for (i = 0; i < sliceSize; i++) {
                            objectBuffer[i] = true;
                        }

                        // Check x = 0;
                        for (i = 0; i < sliceSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < sliceSize; i += xDim)

                        // Check x = xDim - 1
                        for (i = xDim - 1; i < sliceSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = xDim - 1; i < sliceSize; i += xDim)

                        // Check y = 0
                        for (i = 0; i < xDim; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < xDim; i++)

                        // Check y = yDim - 1
                        for (i = xDim * (yDim - 1); i < sliceSize; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = xDim*(yDim-1); i < sliceSize; i++)

                        if (backgroundPresent) {
                            found = true;

                            while (found) {
                                found = false;

                                for (y = 1; y < (yDim - 1); y++) {
                                    j = y * xDim;

                                    for (x = 1; x < (xDim - 1); x++) {
                                        i = j + x;

                                        if ((objectBuffer[i]) && (buffer[i] < thresholdLevel) &&
                                                ((!objectBuffer[i - 1]) || (!objectBuffer[i + 1]) ||
                                                     (!objectBuffer[i - xDim]) || (!objectBuffer[i + xDim]))) {
                                            objectBuffer[i] = false;
                                            found = true;
                                        }
                                    } // for (x = 1; x < xDim - 1; x++)
                                } // for (y = 1; y < yDim - 1; y++)
                            } // while (found)

                            area = 0;

                            for (i = 0; i < sliceSize; i++) {

                                if (objectBuffer[i]) {
                                    area++;
                                }
                            }
                        } // if (backgroundPresent)
                    } // if (thresholdSelected)

                    ac1 = 0.0;
                    ac2 = 0.0;
                    ac3 = 0.0;
                    ac4 = 0.0;
                    ac5 = 0.0;
                    mc1 = 0.0;
                    mc2 = 0.0;
                    mc3 = 0.0;
                    mc4 = 0.0;
                    mc5 = 0.0;
                    grayCount = 0.0;

                    for (y = 0; y < yDim; y++) {
                        j = y * xDim;

                        for (x = 0; x < xDim; x++) {
                            i = j + x;

                            if ((!backgroundPresent) || (objectBuffer[i])) {
                                ac1 += x;
                                ac2 += y;
                                ac3 += x * y;
                                ac4 += x * x;
                                ac5 += y * y;
                                mc1 += buffer[i] * x;
                                mc2 += buffer[i] * y;
                                mc3 += buffer[i] * x * y;
                                mc4 += buffer[i] * x * x;
                                mc5 += buffer[i] * y * y;
                                grayCount += buffer[i];
                            }
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)

                    ac1 = ac1 / area;
                    ac2 = ac2 / area;
                    ac3 = ac3 / area;
                    ac4 = ac4 / area;
                    ac5 = ac5 / area;
                    mc1 = mc1 / grayCount;
                    mc2 = mc2 / grayCount;
                    mc3 = mc3 / grayCount;
                    mc4 = mc4 / grayCount;
                    mc5 = mc5 / grayCount;

                    ad1 = 0.0;
                    ad2 = 0.0;
                    ad3 = 0.0;
                    ad4 = 0.0;
                    ad5 = 0.0;
                    md1 = 0.0;
                    md2 = 0.0;
                    md3 = 0.0;
                    md4 = 0.0;
                    md5 = 0.0;

                    for (y = 0; y < yDim; y++) {
                        j = y * xDim;

                        for (x = 0; x < xDim; x++) {
                            i = j + x;

                            if ((!backgroundPresent) || (objectBuffer[i])) {
                                ad1 += Math.abs(x - ac1);
                                ad2 += Math.abs(y - ac2);
                                ad3 += Math.abs((x * y) - ac3);
                                ad4 += Math.abs((x * x) - ac4);
                                ad5 += Math.abs((y * y) - ac5);
                                md1 += Math.abs(buffer[i] * (x - mc1));
                                md2 += Math.abs(buffer[i] * (y - mc2));
                                md3 += Math.abs(buffer[i] * ((x * y) - mc3));
                                md4 += Math.abs(buffer[i] * ((x * x) - mc4));
                                md5 += Math.abs(buffer[i] * ((y * y) - mc5));
                            }
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)

                    ad1 = ad1 / area;
                    ad2 = ad2 / area;
                    ad3 = ad3 / area;
                    ad4 = ad4 / area;
                    ad5 = ad5 / area;
                    md1 = md1 / area;
                    md2 = md2 / area;
                    md3 = md3 / area;
                    md4 = md4 / area;
                    md5 = md5 / area;

                    // The bottom and top grey values of the input image are to be ignored
                    // as they may represent spurious uniform regions caused by the
                    // saturation of the grey level image

                    for (i = 0; i < sliceSize; i++) {

                        if ((buffer[i] != minimum) && (buffer[i] != maximum)) {
                            validBuffer[i] = true;
                        } else {
                            validBuffer[i] = false;
                        }
                    }

                    if (subsample) {
                        histogramAvailable = 0;

                        for (i = 0; i < sliceSize; i++) {

                            if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                                histogramAvailable++;
                            }
                        } // for (i = 0; i < sliceSize; i++)

                        pixelIncrement = Math.max(1, (int) Math.sqrt(histogramAvailable / 5000));
                    } // if (subsample)

                    for (i = 0; i < nParams; i++) {
                        xi[i][i] = 1.0;
                    }

                    powell(p, xi, powellTolerance);
                    a1 = p[0];
                    a2 = p[1];
                    a3 = p[2];
                    a4 = p[3];
                    a5 = p[4];
                    m1 = p[5];
                    m2 = p[6];
                    m3 = p[7];
                    m4 = p[8];
                    m5 = p[9];

                    if (srcImage.isColorImage()) {

                        if (c == 1) {
                            Preferences.debug("Red\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 2) {
                            Preferences.debug("Green\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 3) {
                            Preferences.debug("Blue\n", Preferences.DEBUG_ALGORITHM);
                        }
                    } // if (srcImage.isColorImage())

                    Preferences.debug("a1 = " + a1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac1 = " + ac1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad1 = " + ad1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a2 = " + a2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac2 = " + ac2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad2 = " + ad2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a3 = " + a3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac3 = " + ac3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad3 = " + ad3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a4 = " + a4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac4 = " + ac4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad4 = " + ad4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a5 = " + a5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac5 = " + ac5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad5 = " + ad5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m1 = " + m1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc1 = " + mc1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md1 = " + md1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m2 = " + m2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc2 = " + mc2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md2 = " + md2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m3 = " + m3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc3 = " + mc3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md3 = " + md3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m4 = " + m4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc4 = " + mc4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md4 = " + md4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m5 = " + m5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc5 = " + mc5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md5 = " + md5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Iterations of Powell's algorithm = " + iter + "\n", Preferences.DEBUG_ALGORITHM);

                    if (destImage != null) {
                        imageType = destImage.getType();
                    } else {
                        imageType = srcImage.getType();
                    }

                    for (y = 0; y < yDim; y++) {
                        j = y * xDim;
                        say = (a2 * (y - ac2) / ad2) + (a5 * ((y * y) - ac5) / ad5);
                        smy = 1.0 + (m2 * (y - mc2) / md2) + (m5 * ((y * y) - mc5) / md5);

                        for (x = 0; x < xDim; x++) {
                            i = j + x;

                            if ((!backgroundPresent) || (objectBuffer[i])) {
                                sa = say + (a1 * (x - ac1) / ad1) + (a3 * ((x * y) - ac3) / ad3) +
                                     (a4 * ((x * x) - ac4) / ad4);

                                if (sa < (-maximum / 2.0)) {
                                    sa = -maximum / 2.0;
                                } else if (sa > (maximum / 2.0)) {
                                    sa = maximum / 2.0;
                                }

                                sm = smy + (m1 * (x - mc1) / md1) + (m3 * ((x * y) - mc3) / md3) +
                                     (m4 * ((x * x) - mc4) / md4);

                                if (sm < 0.05) {
                                    sm = 0.05;
                                } else if (sm > 20.0) {
                                    sm = 20.0;
                                }

                                buffer[i] = (float) ((buffer[i] - sa) / sm);

                                switch (imageType) {

                                    case ModelStorageBase.BYTE:
                                        if (buffer[i] < -128.0f) {
                                            buffer[i] = -128.0f;
                                        } else if (buffer[i] > 127.0f) {
                                            buffer[i] = 127.0f;
                                        }

                                        break;

                                    case ModelStorageBase.UBYTE:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 255.0f) {
                                            buffer[i] = 255.0f;
                                        }

                                        break;

                                    case ModelStorageBase.SHORT:
                                        if (buffer[i] < -32768.0f) {
                                            buffer[i] = -32768.0f;
                                        } else if (buffer[i] > 32767.0f) {
                                            buffer[i] = 32767.0f;
                                        }

                                        break;

                                    case ModelStorageBase.USHORT:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 65535.0f) {
                                            buffer[i] = 65535.0f;
                                        }

                                        break;

                                    case ModelStorageBase.INTEGER:
                                        if (buffer[i] < Integer.MIN_VALUE) {
                                            buffer[i] = Integer.MIN_VALUE;
                                        } else if (buffer[i] > Integer.MAX_VALUE) {
                                            buffer[i] = Integer.MAX_VALUE;
                                        }

                                        break;

                                    case ModelStorageBase.UINTEGER:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 4294967295L) {
                                            buffer[i] = 4294967295L;
                                        }

                                        break;

                                    case ModelStorageBase.ARGB:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 255.0f) {
                                            buffer[i] = 255.0f;
                                        }

                                        break;

                                    case ModelStorageBase.ARGB_USHORT:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 65535.0f) {
                                            buffer[i] = 65535.0f;
                                        }

                                        break;
                                } // switch(imageType)
                            } // if ((!backgroundPresent) || (objectBuffer[i]))
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)

                    if (destImage != null) {

                        if (destImage.isColorImage()) {
                            destImage.importRGBData(c, 0, buffer, true);
                        } else {
                            destImage.importData(0, buffer, true);
                        }
                    } else {

                        if (srcImage.isColorImage()) {
                            srcImage.importRGBData(c, 0, buffer, true);
                        } else {
                            srcImage.importData(0, buffer, true);
                        }
                    }
                } // if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) ||
            } // for (c = 1; c <= maxColor; c++)

        } catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy minimization reports:\n" + ioe.toString());

            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy Minimization reports:\n" + error.toString());

        

            setCompleted(false);

            return;
        }


        setCompleted(true);

        return;
    }


    /**
     * DOCUMENT ME!
     */
    private void run2Dm2() {
        int i;
        double[] p = new double[nParams];
        double[][] xi = new double[nParams][nParams];
        double m1, m2, m3, m4, m5;
        int x;
        int y;
        int j;
        int c;
        double smy;
        double sm;
        boolean found;
        int imageType;
        boolean doRed = false;
        boolean doGreen = false;
        boolean doBlue = false;
        int maxColor = 1;
        
        fireProgressStateChanged(srcImage.getImageName(), "Performing entropy minimization...");
        

        srcImage.calcMinMax();
        minimum = srcImage.getMin();
        maximum = srcImage.getMax();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        area = sliceSize;

        if (srcImage.isColorImage()) {
            minimumR = srcImage.getMinR();
            maximumR = srcImage.getMaxR();

            if (minimumR != maximumR) {
                doRed = true;
            }

            minimumG = srcImage.getMinG();
            maximumG = srcImage.getMaxG();

            if (minimumG != maximumG) {
                doGreen = true;
                maxColor = 2;
            }

            minimumB = srcImage.getMinB();
            maximumB = srcImage.getMaxB();

            if (minimumB != maximumB) {
                doBlue = true;
                maxColor = 3;
            }
        } // if srcImage.isColorImage())

        try {
            buffer = new float[sliceSize];
            idealBuffer = new double[sliceSize];
            validBuffer = new boolean[sliceSize];

            for (c = 1; c <= maxColor; c++) {

                if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) || ((c == 2) && (doGreen)) ||
                        ((c == 3) && (doBlue))) {

                    if (srcImage.isColorImage()) {
                        srcImage.exportRGBData(c, 0, sliceSize, buffer);

                        if (c == 1) {
                            minimum = minimumR;
                            maximum = maximumR;
                            fireProgressStateChanged("Performing red entropy minimization");
                        } else if (c == 2) {
                            minimum = minimumG;
                            maximum = maximumG;
                            fireProgressStateChanged("Performing green entropy minimization");
                        } else if (c == 3) {
                            minimum = minimumB;
                            maximum = maximumB;
                            fireProgressStateChanged("Performing blue entropy minimization");
                        }
                    } // if (srcImage.isColorImage())
                    else {
                        srcImage.exportData(0, sliceSize, buffer);
                    }

                    if (thresholdSelected) {
                        objectBuffer = new boolean[sliceSize];

                        for (i = 0; i < sliceSize; i++) {
                            objectBuffer[i] = true;
                        }

                        // Check x = 0;
                        for (i = 0; i < sliceSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < sliceSize; i += xDim)

                        // Check x = xDim - 1
                        for (i = xDim - 1; i < sliceSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = xDim - 1; i < sliceSize; i += xDim)

                        // Check y = 0
                        for (i = 0; i < xDim; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < xDim; i++)

                        // Check y = yDim - 1
                        for (i = xDim * (yDim - 1); i < sliceSize; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = xDim*(yDim-1); i < sliceSize; i++)

                        if (backgroundPresent) {
                            found = true;

                            while (found) {
                                found = false;

                                for (y = 1; y < (yDim - 1); y++) {
                                    j = y * xDim;

                                    for (x = 1; x < (xDim - 1); x++) {
                                        i = j + x;

                                        if ((objectBuffer[i]) && (buffer[i] < thresholdLevel) &&
                                                ((!objectBuffer[i - 1]) || (!objectBuffer[i + 1]) ||
                                                     (!objectBuffer[i - xDim]) || (!objectBuffer[i + xDim]))) {
                                            objectBuffer[i] = false;
                                            found = true;
                                        }
                                    } // for (x = 1; x < xDim - 1; x++)
                                } // for (y = 1; y < yDim - 1; y++)
                            } // while (found)

                            area = 0;

                            for (i = 0; i < sliceSize; i++) {

                                if (objectBuffer[i]) {
                                    area++;
                                }
                            }
                        } // if (backgroundPresent)
                    } // if (thresholdSelected)

                    mc1 = 0.0;
                    mc2 = 0.0;
                    mc3 = 0.0;
                    mc4 = 0.0;
                    mc5 = 0.0;
                    grayCount = 0.0;

                    for (y = 0; y < yDim; y++) {
                        j = y * xDim;

                        for (x = 0; x < xDim; x++) {
                            i = j + x;

                            if ((!backgroundPresent) || (objectBuffer[i])) {
                                mc1 += buffer[i] * x;
                                mc2 += buffer[i] * y;
                                mc3 += buffer[i] * x * y;
                                mc4 += buffer[i] * x * x;
                                mc5 += buffer[i] * y * y;
                                grayCount += buffer[i];
                            }
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)

                    mc1 = mc1 / grayCount;
                    mc2 = mc2 / grayCount;
                    mc3 = mc3 / grayCount;
                    mc4 = mc4 / grayCount;
                    mc5 = mc5 / grayCount;

                    md1 = 0.0;
                    md2 = 0.0;
                    md3 = 0.0;
                    md4 = 0.0;
                    md5 = 0.0;

                    for (y = 0; y < yDim; y++) {
                        j = y * xDim;

                        for (x = 0; x < xDim; x++) {
                            i = j + x;

                            if ((!backgroundPresent) || (objectBuffer[i])) {
                                md1 += Math.abs(buffer[i] * (x - mc1));
                                md2 += Math.abs(buffer[i] * (y - mc2));
                                md3 += Math.abs(buffer[i] * ((x * y) - mc3));
                                md4 += Math.abs(buffer[i] * ((x * x) - mc4));
                                md5 += Math.abs(buffer[i] * ((y * y) - mc5));
                            }
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)

                    md1 = md1 / area;
                    md2 = md2 / area;
                    md3 = md3 / area;
                    md4 = md4 / area;
                    md5 = md5 / area;

                    // The bottom and top grey values of the input image are to be ignored
                    // as they may represent spurious uniform regions caused by the
                    // saturation of the grey level image

                    for (i = 0; i < sliceSize; i++) {

                        if ((buffer[i] != minimum) && (buffer[i] != maximum)) {
                            validBuffer[i] = true;
                        } else {
                            validBuffer[i] = false;
                        }
                    }

                    if (subsample) {
                        histogramAvailable = 0;

                        for (i = 0; i < sliceSize; i++) {

                            if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                                histogramAvailable++;
                            }
                        } // for (i = 0; i < sliceSize; i++)

                        pixelIncrement = Math.max(1, (int) Math.sqrt(histogramAvailable / 5000));
                    } // if (subsample)

                    for (i = 0; i < nParams; i++) {
                        xi[i][i] = 1.0;
                    }

                    powell(p, xi, powellTolerance);
                    m1 = p[0];
                    m2 = p[1];
                    m3 = p[2];
                    m4 = p[3];
                    m5 = p[4];

                    if (srcImage.isColorImage()) {

                        if (c == 1) {
                            Preferences.debug("Red\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 2) {
                            Preferences.debug("Green\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 3) {
                            Preferences.debug("Blue\n", Preferences.DEBUG_ALGORITHM);
                        }
                    } // if (srcImage.isColorImage())

                    Preferences.debug("m1 = " + m1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc1 = " + mc1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md1 = " + md1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m2 = " + m2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc2 = " + mc2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md2 = " + md2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m3 = " + m3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc3 = " + mc3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md3 = " + md3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m4 = " + m4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc4 = " + mc4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md4 = " + md4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m5 = " + m5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc5 = " + mc5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md5 = " + md5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Iterations of Powell's algorithm = " + iter + "\n", Preferences.DEBUG_ALGORITHM);

                    if (destImage != null) {
                        imageType = destImage.getType();
                    } else {
                        imageType = srcImage.getType();
                    }

                    for (y = 0; y < yDim; y++) {
                        j = y * xDim;
                        smy = 1.0 + (m2 * (y - mc2) / md2) + (m5 * ((y * y) - mc5) / md5);

                        for (x = 0; x < xDim; x++) {
                            i = j + x;

                            if ((!backgroundPresent) || (objectBuffer[i])) {
                                sm = smy + (m1 * (x - mc1) / md1) + (m3 * ((x * y) - mc3) / md3) +
                                     (m4 * ((x * x) - mc4) / md4);

                                if (sm < 0.05) {
                                    sm = 0.05;
                                } else if (sm > 20.0) {
                                    sm = 20.0;
                                }

                                buffer[i] = (float) (buffer[i] / sm);

                                switch (imageType) {

                                    case ModelStorageBase.BYTE:
                                        if (buffer[i] < -128.0f) {
                                            buffer[i] = -128.0f;
                                        } else if (buffer[i] > 127.0f) {
                                            buffer[i] = 127.0f;
                                        }

                                        break;

                                    case ModelStorageBase.UBYTE:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 255.0f) {
                                            buffer[i] = 255.0f;
                                        }

                                        break;

                                    case ModelStorageBase.SHORT:
                                        if (buffer[i] < -32768.0f) {
                                            buffer[i] = -32768.0f;
                                        } else if (buffer[i] > 32767.0f) {
                                            buffer[i] = 32767.0f;
                                        }

                                        break;

                                    case ModelStorageBase.USHORT:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 65535.0f) {
                                            buffer[i] = 65535.0f;
                                        }

                                        break;

                                    case ModelStorageBase.INTEGER:
                                        if (buffer[i] < Integer.MIN_VALUE) {
                                            buffer[i] = Integer.MIN_VALUE;
                                        } else if (buffer[i] > Integer.MAX_VALUE) {
                                            buffer[i] = Integer.MAX_VALUE;
                                        }

                                        break;

                                    case ModelStorageBase.UINTEGER:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 4294967295L) {
                                            buffer[i] = 4294967295L;
                                        }

                                        break;

                                    case ModelStorageBase.ARGB:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 255.0f) {
                                            buffer[i] = 255.0f;
                                        }

                                        break;

                                    case ModelStorageBase.ARGB_USHORT:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 65535.0f) {
                                            buffer[i] = 65535.0f;
                                        }

                                        break;
                                } // switch(imageType)
                            } // if ((!backgroundPresent) || (objectBuffer[i]))
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)

                    if (destImage != null) {

                        if (destImage.isColorImage()) {
                            destImage.importRGBData(c, 0, buffer, true);
                        } else {
                            destImage.importData(0, buffer, true);
                        }
                    } else {

                        if (srcImage.isColorImage()) {
                            srcImage.importRGBData(c, 0, buffer, true);
                        } else {
                            srcImage.importData(0, buffer, true);
                        }
                    }
                } // if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) ||
            } // for (c = 1; c <= maxColor; c++)

        } catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy minimization reports:\n" + ioe.toString());

          
            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy Minimization reports:\n" + error.toString());

         
            setCompleted(false);

            return;
        }



        setCompleted(true);

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void run2Dm3() {
        int i;
        double[] p = new double[nParams];
        double[][] xi = new double[nParams][nParams];
        double m1, m2, m3, m4, m5, m6, m7, m8, m9;
        int x;
        int y;
        int xx, xy, yy;
        int j;
        int c;
        double smy;
        double sm;
        boolean found;
        int imageType;
        boolean doRed = false;
        boolean doGreen = false;
        boolean doBlue = false;
        int maxColor = 1;
        
        fireProgressStateChanged(srcImage.getImageName(), "Performing entropy minimization...");
        

        srcImage.calcMinMax();
        minimum = srcImage.getMin();
        maximum = srcImage.getMax();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        area = sliceSize;

        if (srcImage.isColorImage()) {
            minimumR = srcImage.getMinR();
            maximumR = srcImage.getMaxR();

            if (minimumR != maximumR) {
                doRed = true;
            }

            minimumG = srcImage.getMinG();
            maximumG = srcImage.getMaxG();

            if (minimumG != maximumG) {
                doGreen = true;
                maxColor = 2;
            }

            minimumB = srcImage.getMinB();
            maximumB = srcImage.getMaxB();

            if (minimumB != maximumB) {
                doBlue = true;
                maxColor = 3;
            }
        } // if srcImage.isColorImage())

        try {
            buffer = new float[sliceSize];
            idealBuffer = new double[sliceSize];
            validBuffer = new boolean[sliceSize];

            for (c = 1; c <= maxColor; c++) {

                if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) || ((c == 2) && (doGreen)) ||
                        ((c == 3) && (doBlue))) {

                    if (srcImage.isColorImage()) {
                        srcImage.exportRGBData(c, 0, sliceSize, buffer);

                        if (c == 1) {
                            minimum = minimumR;
                            maximum = maximumR;
                            fireProgressStateChanged("Performing red entropy minimization");
                        } else if (c == 2) {
                            minimum = minimumG;
                            maximum = maximumG;
                            fireProgressStateChanged("Performing green entropy minimization");
                        } else if (c == 3) {
                            minimum = minimumB;
                            maximum = maximumB;
                            fireProgressStateChanged("Performing blue entropy minimization");
                        }
                    } // if (srcImage.isColorImage())
                    else {
                        srcImage.exportData(0, sliceSize, buffer);
                    }

                    if (thresholdSelected) {
                        objectBuffer = new boolean[sliceSize];

                        for (i = 0; i < sliceSize; i++) {
                            objectBuffer[i] = true;
                        }

                        // Check x = 0;
                        for (i = 0; i < sliceSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < sliceSize; i += xDim)

                        // Check x = xDim - 1
                        for (i = xDim - 1; i < sliceSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = xDim - 1; i < sliceSize; i += xDim)

                        // Check y = 0
                        for (i = 0; i < xDim; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < xDim; i++)

                        // Check y = yDim - 1
                        for (i = xDim * (yDim - 1); i < sliceSize; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = xDim*(yDim-1); i < sliceSize; i++)

                        if (backgroundPresent) {
                            found = true;

                            while (found) {
                                found = false;

                                for (y = 1; y < (yDim - 1); y++) {
                                    j = y * xDim;

                                    for (x = 1; x < (xDim - 1); x++) {
                                        i = j + x;

                                        if ((objectBuffer[i]) && (buffer[i] < thresholdLevel) &&
                                                ((!objectBuffer[i - 1]) || (!objectBuffer[i + 1]) ||
                                                     (!objectBuffer[i - xDim]) || (!objectBuffer[i + xDim]))) {
                                            objectBuffer[i] = false;
                                            found = true;
                                        }
                                    } // for (x = 1; x < xDim - 1; x++)
                                } // for (y = 1; y < yDim - 1; y++)
                            } // while (found)

                            area = 0;

                            for (i = 0; i < sliceSize; i++) {

                                if (objectBuffer[i]) {
                                    area++;
                                }
                            }
                        } // if (backgroundPresent)
                    } // if (thresholdSelected)

                    mc1 = 0.0;
                    mc2 = 0.0;
                    mc3 = 0.0;
                    mc4 = 0.0;
                    mc5 = 0.0;
                    mc6 = 0.0;
                    mc7 = 0.0;
                    mc8 = 0.0;
                    mc9 = 0.0;
                    grayCount = 0.0;

                    for (y = 0; y < yDim; y++) {
                        j = y * xDim;

                        for (x = 0; x < xDim; x++) {
                            i = j + x;

                            if ((!backgroundPresent) || (objectBuffer[i])) {
                                mc1 += buffer[i] * x;
                                mc2 += buffer[i] * y;
                                mc3 += buffer[i] * x * y;
                                mc4 += buffer[i] * x * x;
                                mc5 += buffer[i] * y * y;
                                mc6 += buffer[i] * x * x * x;
                                mc7 += buffer[i] * x * x * y;
                                mc8 += buffer[i] * x * y * y;
                                mc9 += buffer[i] * y * y * y;
                                grayCount += buffer[i];
                            }
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)

                    mc1 = mc1 / grayCount;
                    mc2 = mc2 / grayCount;
                    mc3 = mc3 / grayCount;
                    mc4 = mc4 / grayCount;
                    mc5 = mc5 / grayCount;
                    mc6 = mc6 / grayCount;
                    mc7 = mc7 / grayCount;
                    mc8 = mc8 / grayCount;
                    mc9 = mc9 / grayCount;

                    md1 = 0.0;
                    md2 = 0.0;
                    md3 = 0.0;
                    md4 = 0.0;
                    md5 = 0.0;
                    md6 = 0.0;
                    md7 = 0.0;
                    md8 = 0.0;
                    md9 = 0.0;

                    for (y = 0; y < yDim; y++) {
                        j = y * xDim;

                        for (x = 0; x < xDim; x++) {
                            i = j + x;

                            if ((!backgroundPresent) || (objectBuffer[i])) {
                                md1 += Math.abs(buffer[i] * (x - mc1));
                                md2 += Math.abs(buffer[i] * (y - mc2));
                                md3 += Math.abs(buffer[i] * ((x * y) - mc3));
                                md4 += Math.abs(buffer[i] * ((x * x) - mc4));
                                md5 += Math.abs(buffer[i] * ((y * y) - mc5));
                                md6 += Math.abs(buffer[i] * ((x * x * x) - mc6));
                                md7 += Math.abs(buffer[i] * ((x * x * y) - mc7));
                                md8 += Math.abs(buffer[i] * ((x * y * y) - mc8));
                                md9 += Math.abs(buffer[i] * ((y * y * y) - mc9));
                            }
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)

                    md1 = md1 / area;
                    md2 = md2 / area;
                    md3 = md3 / area;
                    md4 = md4 / area;
                    md5 = md5 / area;
                    md6 = md6 / area;
                    md7 = md7 / area;
                    md8 = md8 / area;
                    md9 = md9 / area;

                    // The bottom and top grey values of the input image are to be ignored
                    // as they may represent spurious uniform regions caused by the
                    // saturation of the grey level image

                    for (i = 0; i < sliceSize; i++) {

                        if ((buffer[i] != minimum) && (buffer[i] != maximum)) {
                            validBuffer[i] = true;
                        } else {
                            validBuffer[i] = false;
                        }
                    }

                    if (subsample) {
                        histogramAvailable = 0;

                        for (i = 0; i < sliceSize; i++) {

                            if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                                histogramAvailable++;
                            }
                        } // for (i = 0; i < sliceSize; i++)

                        pixelIncrement = Math.max(1, (int) Math.sqrt(histogramAvailable / 5000));
                    } // if (subsample)

                    for (i = 0; i < nParams; i++) {
                        xi[i][i] = 1.0;
                    }

                    powell(p, xi, powellTolerance);
                    m1 = p[0];
                    m2 = p[1];
                    m3 = p[2];
                    m4 = p[3];
                    m5 = p[4];
                    m6 = p[5];
                    m7 = p[6];
                    m8 = p[7];
                    m9 = p[8];

                    if (srcImage.isColorImage()) {

                        if (c == 1) {
                            Preferences.debug("Red\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 2) {
                            Preferences.debug("Green\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 3) {
                            Preferences.debug("Blue\n", Preferences.DEBUG_ALGORITHM);
                        }
                    } // if (srcImage.isColorImage())

                    Preferences.debug("m1 = " + m1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc1 = " + mc1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md1 = " + md1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m2 = " + m2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc2 = " + mc2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md2 = " + md2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m3 = " + m3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc3 = " + mc3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md3 = " + md3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m4 = " + m4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc4 = " + mc4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md4 = " + md4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m5 = " + m5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc5 = " + mc5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md5 = " + md5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m6 = " + m6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc6 = " + mc6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md6 = " + md6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m7 = " + m7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc7 = " + mc7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md7 = " + md7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m8 = " + m8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc8 = " + mc8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md8 = " + md8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m9 = " + m9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc9 = " + mc9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md9 = " + md9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Iterations of Powell's algorithm = " + iter + "\n", Preferences.DEBUG_ALGORITHM);

                    if (destImage != null) {
                        imageType = destImage.getType();
                    } else {
                        imageType = srcImage.getType();
                    }

                    for (y = 0; y < yDim; y++) {
                        yy = y * y;
                        j = y * xDim;
                        smy = 1.0 + (m2 * (y - mc2) / md2) + (m5 * (yy - mc5) / md5) + (m9 * ((yy * y) - mc9) / md9);

                        for (x = 0; x < xDim; x++) {
                            xx = x * x;
                            xy = x * y;
                            i = j + x;

                            if ((!backgroundPresent) || (objectBuffer[i])) {
                                sm = smy + (m1 * (x - mc1) / md1) + (m3 * (xy - mc3) / md3) + (m4 * (xx - mc4) / md4) +
                                     (m6 * ((xx * x) - mc6) / md6) + (m7 * ((xx * y) - mc7) / md7) +
                                     (m8 * ((xy * y) - mc8) / md8);

                                if (sm < 0.05) {
                                    sm = 0.05;
                                } else if (sm > 20.0) {
                                    sm = 20.0;
                                }

                                buffer[i] = (float) (buffer[i] / sm);

                                switch (imageType) {

                                    case ModelStorageBase.BYTE:
                                        if (buffer[i] < -128.0f) {
                                            buffer[i] = -128.0f;
                                        } else if (buffer[i] > 127.0f) {
                                            buffer[i] = 127.0f;
                                        }

                                        break;

                                    case ModelStorageBase.UBYTE:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 255.0f) {
                                            buffer[i] = 255.0f;
                                        }

                                        break;

                                    case ModelStorageBase.SHORT:
                                        if (buffer[i] < -32768.0f) {
                                            buffer[i] = -32768.0f;
                                        } else if (buffer[i] > 32767.0f) {
                                            buffer[i] = 32767.0f;
                                        }

                                        break;

                                    case ModelStorageBase.USHORT:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 65535.0f) {
                                            buffer[i] = 65535.0f;
                                        }

                                        break;

                                    case ModelStorageBase.INTEGER:
                                        if (buffer[i] < Integer.MIN_VALUE) {
                                            buffer[i] = Integer.MIN_VALUE;
                                        } else if (buffer[i] > Integer.MAX_VALUE) {
                                            buffer[i] = Integer.MAX_VALUE;
                                        }

                                        break;

                                    case ModelStorageBase.UINTEGER:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 4294967295L) {
                                            buffer[i] = 4294967295L;
                                        }

                                        break;

                                    case ModelStorageBase.ARGB:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 255.0f) {
                                            buffer[i] = 255.0f;
                                        }

                                        break;

                                    case ModelStorageBase.ARGB_USHORT:
                                        if (buffer[i] < 0.0f) {
                                            buffer[i] = 0.0f;
                                        } else if (buffer[i] > 65535.0f) {
                                            buffer[i] = 65535.0f;
                                        }

                                        break;
                                } // switch(imageType)
                            } // if ((!backgroundPresent) || (objectBuffer[i]))
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)

                    if (destImage != null) {

                        if (destImage.isColorImage()) {
                            destImage.importRGBData(c, 0, buffer, true);
                        } else {
                            destImage.importData(0, buffer, true);
                        }
                    } else {

                        if (srcImage.isColorImage()) {
                            srcImage.importRGBData(c, 0, buffer, true);
                        } else {
                            srcImage.importData(0, buffer, true);
                        }
                    }
                } // if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) ||
            } // for (c = 1; c <= maxColor; c++)

        } catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy minimization reports:\n" + ioe.toString());

          
            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy Minimization reports:\n" + error.toString());

          

            setCompleted(false);

            return;
        }


        setCompleted(true);

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void run3Da2m2() {
        int i;
        double[] p = new double[nParams];
        double[][] xi = new double[nParams][nParams];
        double a1, a2, a3, a4, a5, a6, a7, a8, a9;
        double m1, m2, m3, m4, m5, m6, m7, m8, m9;
        int x;
        int y;
        int z;
        int j;
        int k;
        int c;
        double saz;
        double smz;
        double say;
        double smy;
        double sa;
        double sm;
        boolean found;
        int imageType;
        boolean doRed = false;
        boolean doGreen = false;
        boolean doBlue = false;
        int maxColor = 1;

        fireProgressStateChanged(srcImage.getImageName(), "Performing entropy minimization...");
        

        srcImage.calcMinMax();
        minimum = srcImage.getMin();
        maximum = srcImage.getMax();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        volSize = sliceSize * zDim;
        area = sliceSize;
        volume = volSize;

        if (srcImage.isColorImage()) {
            minimumR = srcImage.getMinR();
            maximumR = srcImage.getMaxR();

            if (minimumR != maximumR) {
                doRed = true;
            }

            minimumG = srcImage.getMinG();
            maximumG = srcImage.getMaxG();

            if (minimumG != maximumG) {
                doGreen = true;
                maxColor = 2;
            }

            minimumB = srcImage.getMinB();
            maximumB = srcImage.getMaxB();

            if (minimumB != maximumB) {
                doBlue = true;
                maxColor = 3;
            }
        } // if srcImage.isColorImage())

        try {
            buffer = new float[volSize];
            idealBuffer = new double[volSize];
            validBuffer = new boolean[volSize];

            for (c = 1; c <= maxColor; c++) {

                if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) || ((c == 2) && (doGreen)) ||
                        ((c == 3) && (doBlue))) {

                    if (srcImage.isColorImage()) {
                        srcImage.exportRGBData(c, 0, volSize, buffer);

                        if (c == 1) {
                            minimum = minimumR;
                            maximum = maximumR;
                            fireProgressStateChanged("Performing red entropy minimization");
                        } else if (c == 2) {
                            minimum = minimumG;
                            maximum = maximumG;
                            fireProgressStateChanged("Performing green entropy minimization");
                        } else if (c == 3) {
                            minimum = minimumB;
                            maximum = maximumB;
                            fireProgressStateChanged("Performing blue entropy minimization");
                        }
                    } // if (srcImage.isColorImage())
                    else {
                        srcImage.exportData(0, volSize, buffer);
                    }

                    if (thresholdSelected) {
                        objectBuffer = new boolean[volSize];

                        for (i = 0; i < volSize; i++) {
                            objectBuffer[i] = true;
                        }

                        // Check x = 0;
                        for (i = 0; i < volSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < volSize; i += xDim)

                        // Check x = xDim - 1
                        for (i = xDim - 1; i < volSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = xDim - 1; i < volSize; i += xDim)

                        // Check y = 0
                        for (z = 0; z < zDim; z++) {
                            j = z * sliceSize;

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if (buffer[i] < thresholdLevel) {
                                    objectBuffer[i] = false;
                                    backgroundPresent = true;
                                }
                            } // for (x = 0; x < xDim; x++)
                        } // for (z = 0; z < zDim; z++)

                        // Check y = yDim - 1
                        for (z = 0; z < zDim; z++) {
                            j = z * sliceSize;

                            for (x = xDim * (yDim - 1); x < sliceSize; x++) {
                                i = j + x;

                                if (buffer[i] < thresholdLevel) {
                                    objectBuffer[i] = false;
                                    backgroundPresent = true;
                                }
                            } // for (x = xDim*(yDim-1); x < sliceSize; x++)
                        } // z = 0; z < zDim; z++)

                        // Check z = 0;
                        for (i = 0; i < sliceSize; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < sliceSize; i++)

                        // Check z = zDim - 1
                        for (i = (zDim - 1) * sliceSize; i < volSize; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = (zDim-1)*sliceSize; i < volSize; i++)

                        if (backgroundPresent) {
                            found = true;

                            while (found) {
                                found = false;

                                for (z = 1; z < (zDim - 1); z++) {
                                    k = z * sliceSize;

                                    for (y = 1; y < (yDim - 1); y++) {
                                        j = k + (y * xDim);

                                        for (x = 1; x < (xDim - 1); x++) {
                                            i = j + x;

                                            if ((objectBuffer[i]) && (buffer[i] < thresholdLevel) &&
                                                    ((!objectBuffer[i - 1]) || (!objectBuffer[i + 1]) ||
                                                         (!objectBuffer[i - xDim]) || (!objectBuffer[i + xDim]) ||
                                                         (!objectBuffer[i - sliceSize]) ||
                                                         (!objectBuffer[i + sliceSize]))) {
                                                objectBuffer[i] = false;
                                                found = true;
                                            }
                                        } // for (x = 1; x < xDim - 1; x++)
                                    } // for (y = 1; y < yDim - 1; y++)
                                } // for (z = 1; z < zDim - 1; z++)
                            } // while (found)

                            volume = 0;

                            for (i = 0; i < volSize; i++) {

                                if (objectBuffer[i]) {
                                    volume++;
                                }
                            }
                        } // if (backgroundPresent)
                    } // if (thresholdSelected)

                    ac1 = 0.0;
                    ac2 = 0.0;
                    ac3 = 0.0;
                    ac4 = 0.0;
                    ac5 = 0.0;
                    ac6 = 0.0;
                    ac7 = 0.0;
                    ac8 = 0.0;
                    ac9 = 0.0;
                    mc1 = 0.0;
                    mc2 = 0.0;
                    mc3 = 0.0;
                    mc4 = 0.0;
                    mc5 = 0.0;
                    mc6 = 0.0;
                    mc7 = 0.0;
                    mc8 = 0.0;
                    mc9 = 0.0;
                    grayCount = 0.0;

                    for (z = 0; z < zDim; z++) {
                        k = z * sliceSize;

                        for (y = 0; y < yDim; y++) {
                            j = k + (y * xDim);

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if ((!backgroundPresent) || (objectBuffer[i])) {
                                    ac1 += x;
                                    ac2 += y;
                                    ac3 += z;
                                    ac4 += x * y;
                                    ac5 += x * z;
                                    ac6 += y * z;
                                    ac7 += x * x;
                                    ac8 += y * y;
                                    ac9 += z * z;
                                    mc1 += buffer[i] * x;
                                    mc2 += buffer[i] * y;
                                    mc3 += buffer[i] * z;
                                    mc4 += buffer[i] * x * y;
                                    mc5 += buffer[i] * x * z;
                                    mc6 += buffer[i] * y * z;
                                    mc7 += buffer[i] * x * x;
                                    mc8 += buffer[i] * y * y;
                                    mc9 += buffer[i] * z * z;
                                    grayCount += buffer[i];
                                }
                            } // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)
                    } // for (z = 0; z < zDim; z++)

                    ac1 = ac1 / volume;
                    ac2 = ac2 / volume;
                    ac3 = ac3 / volume;
                    ac4 = ac4 / volume;
                    ac5 = ac5 / volume;
                    ac6 = ac6 / volume;
                    ac7 = ac7 / volume;
                    ac8 = ac8 / volume;
                    ac9 = ac9 / volume;
                    mc1 = mc1 / grayCount;
                    mc2 = mc2 / grayCount;
                    mc3 = mc3 / grayCount;
                    mc4 = mc4 / grayCount;
                    mc5 = mc5 / grayCount;
                    mc6 = mc6 / grayCount;
                    mc7 = mc7 / grayCount;
                    mc8 = mc8 / grayCount;
                    mc9 = mc9 / grayCount;

                    ad1 = 0.0;
                    ad2 = 0.0;
                    ad3 = 0.0;
                    ad4 = 0.0;
                    ad5 = 0.0;
                    ad6 = 0.0;
                    ad7 = 0.0;
                    ad8 = 0.0;
                    ad9 = 0.0;
                    md1 = 0.0;
                    md2 = 0.0;
                    md3 = 0.0;
                    md4 = 0.0;
                    md5 = 0.0;
                    md6 = 0.0;
                    md7 = 0.0;
                    md8 = 0.0;
                    md9 = 0.0;

                    for (z = 0; z < zDim; z++) {
                        k = z * sliceSize;

                        for (y = 0; y < yDim; y++) {
                            j = k + (y * xDim);

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if ((!backgroundPresent) || (objectBuffer[i])) {
                                    ad1 += Math.abs(x - ac1);
                                    ad2 += Math.abs(y - ac2);
                                    ad3 += Math.abs(z - ac3);
                                    ad4 += Math.abs((x * y) - ac4);
                                    ad5 += Math.abs((x * z) - ac5);
                                    ad6 += Math.abs((y * z) - ac6);
                                    ad7 += Math.abs((x * x) - ac7);
                                    ad8 += Math.abs((y * y) - ac8);
                                    ad9 += Math.abs((z * z) - ac9);
                                    md1 += Math.abs(buffer[i] * (x - mc1));
                                    md2 += Math.abs(buffer[i] * (y - mc2));
                                    md3 += Math.abs(buffer[i] * (z - mc3));
                                    md4 += Math.abs(buffer[i] * ((x * y) - mc4));
                                    md5 += Math.abs(buffer[i] * ((x * z) - mc5));
                                    md6 += Math.abs(buffer[i] * ((y * z) - mc6));
                                    md7 += Math.abs(buffer[i] * ((x * x) - mc7));
                                    md8 += Math.abs(buffer[i] * ((y * y) - mc8));
                                    md9 += Math.abs(buffer[i] * ((z * z) - mc9));
                                }
                            } // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)
                    } // for (z = 0; z < zDim; z++)

                    ad1 = ad1 / volume;
                    ad2 = ad2 / volume;
                    ad3 = ad3 / volume;
                    ad4 = ad4 / volume;
                    ad5 = ad5 / volume;
                    ad6 = ad6 / volume;
                    ad7 = ad7 / volume;
                    ad8 = ad8 / volume;
                    ad9 = ad9 / volume;
                    md1 = md1 / volume;
                    md2 = md2 / volume;
                    md3 = md3 / volume;
                    md4 = md4 / volume;
                    md5 = md5 / volume;
                    md6 = md6 / volume;
                    md7 = md7 / volume;
                    md8 = md8 / volume;
                    md9 = md9 / volume;

                    // The bottom and top grey values of the input image are to be ignored
                    // as they may represent spurious uniform regions caused by the
                    // saturation of the grey level image

                    for (i = 0; i < volSize; i++) {

                        if ((buffer[i] != minimum) && (buffer[i] != maximum)) {
                            validBuffer[i] = true;
                        } else {
                            validBuffer[i] = false;
                        }
                    }

                    if (subsample) {
                        histogramAvailable = 0;

                        for (i = 0; i < volSize; i++) {

                            if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                                histogramAvailable++;
                            }
                        } // for (i = 0; i < volSize; i++)

                        pixelIncrement = Math.max(1, (int) Math.pow((double) (histogramAvailable / 5000), 1.0 / 3.0));
                        if (zDim < xDim) {
                            zPixelIncrement = Math.max(1, pixelIncrement * zDim/xDim);
                        }
                        else{
                            zPixelIncrement = pixelIncrement;
                        }
                    } // if (subsample)

                    for (i = 0; i < nParams; i++) {
                        xi[i][i] = 1.0;
                    }

                    powell(p, xi, powellTolerance);
                    a1 = p[0];
                    a2 = p[1];
                    a3 = p[2];
                    a4 = p[3];
                    a5 = p[4];
                    a6 = p[5];
                    a7 = p[6];
                    a8 = p[7];
                    a9 = p[8];
                    m1 = p[9];
                    m2 = p[10];
                    m3 = p[11];
                    m4 = p[12];
                    m5 = p[13];
                    m6 = p[14];
                    m7 = p[15];
                    m8 = p[16];
                    m9 = p[17];

                    if (srcImage.isColorImage()) {

                        if (c == 1) {
                            Preferences.debug("Red\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 2) {
                            Preferences.debug("Green\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 3) {
                            Preferences.debug("Blue\n", Preferences.DEBUG_ALGORITHM);
                        }
                    } // if (srcImage.isColorImage())

                    Preferences.debug("a1 = " + a1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac1 = " + ac1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad1 = " + ad1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a2 = " + a2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac2 = " + ac2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad2 = " + ad2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a3 = " + a3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac3 = " + ac3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad3 = " + ad3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a4 = " + a4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac4 = " + ac4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad4 = " + ad4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a5 = " + a5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac5 = " + ac5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad5 = " + ad5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a6 = " + a6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac6 = " + ac6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad6 = " + ad6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a7 = " + a7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac7 = " + ac7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad7 = " + ad7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a8 = " + a8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac8 = " + ac8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad8 = " + ad8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("a9 = " + a9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ac9 = " + ac9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ad9 = " + ad9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m1 = " + m1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc1 = " + mc1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md1 = " + md1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m2 = " + m2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc2 = " + mc2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md2 = " + md2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m3 = " + m3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc3 = " + mc3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md3 = " + md3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m4 = " + m4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc4 = " + mc4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md4 = " + md4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m5 = " + m5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc5 = " + mc5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md5 = " + md5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m6 = " + m6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc6 = " + mc6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md6 = " + md6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m7 = " + m7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc7 = " + mc7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md7 = " + md7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m8 = " + m8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc8 = " + mc8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md8 = " + md8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m9 = " + m9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc9 = " + mc9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md9 = " + md9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Iterations of Powell's algorithm = " + iter + "\n", Preferences.DEBUG_ALGORITHM);

                    if (destImage != null) {
                        imageType = destImage.getType();
                    } else {
                        imageType = srcImage.getType();
                    }

                    for (z = 0; z < zDim; z++) {
                        k = z * sliceSize;
                        saz = (a3 * (z - ac3) / ad3) + (a9 * ((z * z) - ac9) / ad9);
                        smz = 1.0 + (m3 * (z - mc3) / md3) + (m9 * ((z * z) - mc9) / md9);

                        for (y = 0; y < yDim; y++) {
                            j = k + (y * xDim);
                            say = saz + (a2 * (y - ac2) / ad2) + (a8 * ((y * y) - ac8) / ad8) +
                                  (a6 * ((y * z) - ac6) / ad6);
                            smy = smz + (m2 * (y - mc2) / md2) + (m8 * ((y * y) - mc8) / md8) +
                                  (m6 * ((y * z) - mc6) / md6);

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if ((!backgroundPresent) || (objectBuffer[i])) {
                                    sa = say + (a1 * (x - ac1) / ad1) + (a4 * ((x * y) - ac4) / ad4) +
                                         (a7 * ((x * x) - ac7) / ad7) + (a5 * ((x * z) - ac5) / ad5);

                                    if (sa < (-maximum / 2.0)) {
                                        sa = -maximum / 2.0;
                                    } else if (sa > (maximum / 2.0)) {
                                        sa = maximum / 2.0;
                                    }

                                    sm = smy + (m1 * (x - mc1) / md1) + (m4 * ((x * y) - mc4) / md4) +
                                         (m7 * ((x * x) - mc7) / md7) + (m5 * ((x * z) - mc5) / md5);

                                    if (sm < 0.05) {
                                        sm = 0.05;
                                    } else if (sm > 20.0) {
                                        sm = 20.0;
                                    }

                                    buffer[i] = (float) ((buffer[i] - sa) / sm);

                                    switch (imageType) {

                                        case ModelStorageBase.BYTE:
                                            if (buffer[i] < -128.0f) {
                                                buffer[i] = -128.0f;
                                            } else if (buffer[i] > 127.0f) {
                                                buffer[i] = 127.0f;
                                            }

                                            break;

                                        case ModelStorageBase.UBYTE:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 255.0f) {
                                                buffer[i] = 255.0f;
                                            }

                                            break;

                                        case ModelStorageBase.SHORT:
                                            if (buffer[i] < -32768.0f) {
                                                buffer[i] = -32768.0f;
                                            } else if (buffer[i] > 32767.0f) {
                                                buffer[i] = 32767.0f;
                                            }

                                            break;

                                        case ModelStorageBase.USHORT:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 65535.0f) {
                                                buffer[i] = 65535.0f;
                                            }

                                            break;

                                        case ModelStorageBase.INTEGER:
                                            if (buffer[i] < Integer.MIN_VALUE) {
                                                buffer[i] = Integer.MIN_VALUE;
                                            } else if (buffer[i] > Integer.MAX_VALUE) {
                                                buffer[i] = Integer.MAX_VALUE;
                                            }

                                            break;

                                        case ModelStorageBase.UINTEGER:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 4294967295L) {
                                                buffer[i] = 4294967295L;
                                            }

                                            break;

                                        case ModelStorageBase.ARGB:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 255.0f) {
                                                buffer[i] = 255.0f;
                                            }

                                            break;

                                        case ModelStorageBase.ARGB_USHORT:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 65535.0f) {
                                                buffer[i] = 65535.0f;
                                            }

                                            break;
                                    } // switch(imageType)
                                } // if ((!backgroundPresent) || (objectBuffer[i]))
                            } // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)
                    } // for (z = 0; z < zDim; z++)

                    if (destImage != null) {

                        if (destImage.isColorImage()) {
                            destImage.importRGBData(c, 0, buffer, true);
                        } else {
                            destImage.importData(0, buffer, true);
                        }
                    } else {

                        if (srcImage.isColorImage()) {
                            srcImage.importRGBData(c, 0, buffer, true);
                        } else {
                            srcImage.importData(0, buffer, true);
                        }
                    }
                } // if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) ||
            } // for (c = 1; c <= maxColor; c++)

        } catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy minimization reports:\n" + ioe.toString());

            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy Minimization reports:\n" + error.toString());

      
            setCompleted(false);

            return;
        }


        setCompleted(true);

        return;
    }


    /**
     * DOCUMENT ME!
     */
    private void run3Dm2() {
        int i;
        double[] p = new double[nParams];
        double[][] xi = new double[nParams][nParams];
        double m1, m2, m3, m4, m5, m6, m7, m8, m9;
        int x;
        int y;
        int z;
        int j;
        int k;
        int c;
        double smz;
        double smy;
        double sm;
        boolean found;
        int imageType;
        boolean doRed = false;
        boolean doGreen = false;
        boolean doBlue = false;
        int maxColor = 1;

        fireProgressStateChanged(srcImage.getImageName(), "Performing entropy minimization ...");
        

        srcImage.calcMinMax();
        minimum = srcImage.getMin();
        maximum = srcImage.getMax();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        volSize = sliceSize * zDim;
        area = sliceSize;
        volume = volSize;

        if (srcImage.isColorImage()) {
            minimumR = srcImage.getMinR();
            maximumR = srcImage.getMaxR();

            if (minimumR != maximumR) {
                doRed = true;
            }

            minimumG = srcImage.getMinG();
            maximumG = srcImage.getMaxG();

            if (minimumG != maximumG) {
                doGreen = true;
                maxColor = 2;
            }

            minimumB = srcImage.getMinB();
            maximumB = srcImage.getMaxB();

            if (minimumB != maximumB) {
                doBlue = true;
                maxColor = 3;
            }
        } // if srcImage.isColorImage())

        try {
            buffer = new float[volSize];
            idealBuffer = new double[volSize];
            validBuffer = new boolean[volSize];

            for (c = 1; c <= maxColor; c++) {

                if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) || ((c == 2) && (doGreen)) ||
                        ((c == 3) && (doBlue))) {

                    if (srcImage.isColorImage()) {
                        srcImage.exportRGBData(c, 0, volSize, buffer);

                        if (c == 1) {
                            minimum = minimumR;
                            maximum = maximumR;
                            fireProgressStateChanged("Performing red entropy minimization");
                        } else if (c == 2) {
                            minimum = minimumG;
                            maximum = maximumG;
                            fireProgressStateChanged("Performing green entropy minimization");
                        } else if (c == 3) {
                            minimum = minimumB;
                            maximum = maximumB;
                            fireProgressStateChanged("Performing blue entropy minimization");
                        }
                    } // if (srcImage.isColorImage())
                    else {
                        srcImage.exportData(0, volSize, buffer);
                    }

                    if (thresholdSelected) {
                        objectBuffer = new boolean[volSize];

                        for (i = 0; i < volSize; i++) {
                            objectBuffer[i] = true;
                        }

                        // Check x = 0;
                        for (i = 0; i < volSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < volSize; i += xDim)

                        // Check x = xDim - 1
                        for (i = xDim - 1; i < volSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = xDim - 1; i < volSize; i += xDim)

                        // Check y = 0
                        for (z = 0; z < zDim; z++) {
                            j = z * sliceSize;

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if (buffer[i] < thresholdLevel) {
                                    objectBuffer[i] = false;
                                    backgroundPresent = true;
                                }
                            } // for (x = 0; x < xDim; x++)
                        } // for (z = 0; z < zDim; z++)

                        // Check y = yDim - 1
                        for (z = 0; z < zDim; z++) {
                            j = z * sliceSize;

                            for (x = xDim * (yDim - 1); x < sliceSize; x++) {
                                i = j + x;

                                if (buffer[i] < thresholdLevel) {
                                    objectBuffer[i] = false;
                                    backgroundPresent = true;
                                }
                            } // for (x = xDim*(yDim-1); x < sliceSize; x++)
                        } // z = 0; z < zDim; z++)

                        // Check z = 0;
                        for (i = 0; i < sliceSize; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < sliceSize; i++)

                        // Check z = zDim - 1
                        for (i = (zDim - 1) * sliceSize; i < volSize; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = (zDim-1)*sliceSize; i < volSize; i++)

                        if (backgroundPresent) {
                            found = true;

                            while (found) {
                                found = false;

                                for (z = 1; z < (zDim - 1); z++) {
                                    k = z * sliceSize;

                                    for (y = 1; y < (yDim - 1); y++) {
                                        j = k + (y * xDim);

                                        for (x = 1; x < (xDim - 1); x++) {
                                            i = j + x;

                                            if ((objectBuffer[i]) && (buffer[i] < thresholdLevel) &&
                                                    ((!objectBuffer[i - 1]) || (!objectBuffer[i + 1]) ||
                                                         (!objectBuffer[i - xDim]) || (!objectBuffer[i + xDim]) ||
                                                         (!objectBuffer[i - sliceSize]) ||
                                                         (!objectBuffer[i + sliceSize]))) {
                                                objectBuffer[i] = false;
                                                found = true;
                                            }
                                        } // for (x = 1; x < xDim - 1; x++)
                                    } // for (y = 1; y < yDim - 1; y++)
                                } // for (z = 1; z < zDim - 1; z++)
                            } // while (found)

                            volume = 0;

                            for (i = 0; i < volSize; i++) {

                                if (objectBuffer[i]) {
                                    volume++;
                                }
                            }
                        } // if (backgroundPresent)
                    } // if (thresholdSelected)

                    mc1 = 0.0;
                    mc2 = 0.0;
                    mc3 = 0.0;
                    mc4 = 0.0;
                    mc5 = 0.0;
                    mc6 = 0.0;
                    mc7 = 0.0;
                    mc8 = 0.0;
                    mc9 = 0.0;
                    grayCount = 0.0;

                    for (z = 0; z < zDim; z++) {
                        k = z * sliceSize;

                        for (y = 0; y < yDim; y++) {
                            j = k + (y * xDim);

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if ((!backgroundPresent) || (objectBuffer[i])) {
                                    mc1 += buffer[i] * x;
                                    mc2 += buffer[i] * y;
                                    mc3 += buffer[i] * z;
                                    mc4 += buffer[i] * x * y;
                                    mc5 += buffer[i] * x * z;
                                    mc6 += buffer[i] * y * z;
                                    mc7 += buffer[i] * x * x;
                                    mc8 += buffer[i] * y * y;
                                    mc9 += buffer[i] * z * z;
                                    grayCount += buffer[i];
                                }
                            } // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)
                    } // for (z = 0; z < zDim; z++)

                    mc1 = mc1 / grayCount;
                    mc2 = mc2 / grayCount;
                    mc3 = mc3 / grayCount;
                    mc4 = mc4 / grayCount;
                    mc5 = mc5 / grayCount;
                    mc6 = mc6 / grayCount;
                    mc7 = mc7 / grayCount;
                    mc8 = mc8 / grayCount;
                    mc9 = mc9 / grayCount;

                    md1 = 0.0;
                    md2 = 0.0;
                    md3 = 0.0;
                    md4 = 0.0;
                    md5 = 0.0;
                    md6 = 0.0;
                    md7 = 0.0;
                    md8 = 0.0;
                    md9 = 0.0;

                    for (z = 0; z < zDim; z++) {
                        k = z * sliceSize;

                        for (y = 0; y < yDim; y++) {
                            j = k + (y * xDim);

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if ((!backgroundPresent) || (objectBuffer[i])) {
                                    md1 += Math.abs(buffer[i] * (x - mc1));
                                    md2 += Math.abs(buffer[i] * (y - mc2));
                                    md3 += Math.abs(buffer[i] * (z - mc3));
                                    md4 += Math.abs(buffer[i] * ((x * y) - mc4));
                                    md5 += Math.abs(buffer[i] * ((x * z) - mc5));
                                    md6 += Math.abs(buffer[i] * ((y * z) - mc6));
                                    md7 += Math.abs(buffer[i] * ((x * x) - mc7));
                                    md8 += Math.abs(buffer[i] * ((y * y) - mc8));
                                    md9 += Math.abs(buffer[i] * ((z * z) - mc9));
                                }
                            } // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)
                    } // for (z = 0; z < zDim; z++)

                    md1 = md1 / volume;
                    md2 = md2 / volume;
                    md3 = md3 / volume;
                    md4 = md4 / volume;
                    md5 = md5 / volume;
                    md6 = md6 / volume;
                    md7 = md7 / volume;
                    md8 = md8 / volume;
                    md9 = md9 / volume;

                    // The bottom and top grey values of the input image are to be ignored
                    // as they may represent spurious uniform regions caused by the
                    // saturation of the grey level image

                    for (i = 0; i < volSize; i++) {

                        if ((buffer[i] != minimum) && (buffer[i] != maximum)) {
                            validBuffer[i] = true;
                        } else {
                            validBuffer[i] = false;
                        }
                    }

                    if (subsample) {
                        histogramAvailable = 0;

                        for (i = 0; i < volSize; i++) {

                            if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                                histogramAvailable++;
                            }
                        } // for (i = 0; i < volSize; i++)

                        pixelIncrement = Math.max(1, (int) Math.pow((double) (histogramAvailable / 5000), 1.0 / 3.0));
                        if (zDim < xDim) {
                            zPixelIncrement = Math.max(1, pixelIncrement * zDim/xDim);
                        }
                        else{
                            zPixelIncrement = pixelIncrement;
                        }
                    } // if (subsample)

                    for (i = 0; i < nParams; i++) {
                        xi[i][i] = 1.0;
                    }

                    powell(p, xi, powellTolerance);
                    m1 = p[0];
                    m2 = p[1];
                    m3 = p[2];
                    m4 = p[3];
                    m5 = p[4];
                    m6 = p[5];
                    m7 = p[6];
                    m8 = p[7];
                    m9 = p[8];

                    if (srcImage.isColorImage()) {

                        if (c == 1) {
                            Preferences.debug("Red\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 2) {
                            Preferences.debug("Green\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 3) {
                            Preferences.debug("Blue\n", Preferences.DEBUG_ALGORITHM);
                        }
                    } // if (srcImage.isColorImage())

                    Preferences.debug("m1 = " + m1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc1 = " + mc1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md1 = " + md1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m2 = " + m2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc2 = " + mc2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md2 = " + md2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m3 = " + m3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc3 = " + mc3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md3 = " + md3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m4 = " + m4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc4 = " + mc4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md4 = " + md4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m5 = " + m5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc5 = " + mc5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md5 = " + md5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m6 = " + m6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc6 = " + mc6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md6 = " + md6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m7 = " + m7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc7 = " + mc7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md7 = " + md7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m8 = " + m8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc8 = " + mc8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md8 = " + md8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m9 = " + m9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc9 = " + mc9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md9 = " + md9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Iterations of Powell's algorithm = " + iter + "\n", Preferences.DEBUG_ALGORITHM);

                    if (destImage != null) {
                        imageType = destImage.getType();
                    } else {
                        imageType = srcImage.getType();
                    }

                    for (z = 0; z < zDim; z++) {
                        k = z * sliceSize;
                        smz = 1.0 + (m3 * (z - mc3) / md3) + (m9 * ((z * z) - mc9) / md9);

                        for (y = 0; y < yDim; y++) {
                            j = k + (y * xDim);
                            smy = smz + (m2 * (y - mc2) / md2) + (m8 * ((y * y) - mc8) / md8) +
                                  (m6 * ((y * z) - mc6) / md6);

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if ((!backgroundPresent) || (objectBuffer[i])) {
                                    sm = smy + (m1 * (x - mc1) / md1) + (m4 * ((x * y) - mc4) / md4) +
                                         (m7 * ((x * x) - mc7) / md7) + (m5 * ((x * z) - mc5) / md5);

                                    if (sm < 0.05) {
                                        sm = 0.05;
                                    } else if (sm > 20.0) {
                                        sm = 20.0;
                                    }

                                    buffer[i] = (float) (buffer[i] / sm);

                                    switch (imageType) {

                                        case ModelStorageBase.BYTE:
                                            if (buffer[i] < -128.0f) {
                                                buffer[i] = -128.0f;
                                            } else if (buffer[i] > 127.0f) {
                                                buffer[i] = 127.0f;
                                            }

                                            break;

                                        case ModelStorageBase.UBYTE:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 255.0f) {
                                                buffer[i] = 255.0f;
                                            }

                                            break;

                                        case ModelStorageBase.SHORT:
                                            if (buffer[i] < -32768.0f) {
                                                buffer[i] = -32768.0f;
                                            } else if (buffer[i] > 32767.0f) {
                                                buffer[i] = 32767.0f;
                                            }

                                            break;

                                        case ModelStorageBase.USHORT:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 65535.0f) {
                                                buffer[i] = 65535.0f;
                                            }

                                            break;

                                        case ModelStorageBase.INTEGER:
                                            if (buffer[i] < Integer.MIN_VALUE) {
                                                buffer[i] = Integer.MIN_VALUE;
                                            } else if (buffer[i] > Integer.MAX_VALUE) {
                                                buffer[i] = Integer.MAX_VALUE;
                                            }

                                            break;

                                        case ModelStorageBase.UINTEGER:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 4294967295L) {
                                                buffer[i] = 4294967295L;
                                            }

                                            break;

                                        case ModelStorageBase.ARGB:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 255.0f) {
                                                buffer[i] = 255.0f;
                                            }

                                            break;

                                        case ModelStorageBase.ARGB_USHORT:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 65535.0f) {
                                                buffer[i] = 65535.0f;
                                            }

                                            break;
                                    } // switch(imageType)
                                } // if ((!backgroundPresent) || (objectBuffer[i]))
                            } // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)
                    } // for (z = 0; z < zDim; z++)

                    if (destImage != null) {

                        if (destImage.isColorImage()) {
                            destImage.importRGBData(c, 0, buffer, true);
                        } else {
                            destImage.importData(0, buffer, true);
                        }
                    } else {

                        if (srcImage.isColorImage()) {
                            srcImage.importRGBData(c, 0, buffer, true);
                        } else {
                            srcImage.importData(0, buffer, true);
                        }
                    }
                } // if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) ||
            } // for (c = 1; c <= maxColor; c++)

        } catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy minimization reports:\n" + ioe.toString());

          

            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy Minimization reports:\n" + error.toString());

       

            setCompleted(false);

            return;
        }


        setCompleted(true);

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void run3Dm3() {
        int i;
        double[] p = new double[nParams];
        double[][] xi = new double[nParams][nParams];
        double m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19;
        int x;
        int y;
        int z;
        int j;
        int k;
        int c;
        double smz;
        double smy;
        double sm;
        boolean found;
        int imageType;
        boolean doRed = false;
        boolean doGreen = false;
        boolean doBlue = false;
        int maxColor = 1;

        fireProgressStateChanged(srcImage.getImageName(), "Performing entropy minimization...");
        

        srcImage.calcMinMax();
        minimum = srcImage.getMin();
        maximum = srcImage.getMax();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        volSize = sliceSize * zDim;
        area = sliceSize;
        volume = volSize;

        if (srcImage.isColorImage()) {
            minimumR = srcImage.getMinR();
            maximumR = srcImage.getMaxR();

            if (minimumR != maximumR) {
                doRed = true;
            }

            minimumG = srcImage.getMinG();
            maximumG = srcImage.getMaxG();

            if (minimumG != maximumG) {
                doGreen = true;
                maxColor = 2;
            }

            minimumB = srcImage.getMinB();
            maximumB = srcImage.getMaxB();

            if (minimumB != maximumB) {
                doBlue = true;
                maxColor = 3;
            }
        } // if srcImage.isColorImage())

        try {
            buffer = new float[volSize];
            idealBuffer = new double[volSize];
            validBuffer = new boolean[volSize];

            for (c = 1; c <= maxColor; c++) {

                if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) || ((c == 2) && (doGreen)) ||
                        ((c == 3) && (doBlue))) {

                    if (srcImage.isColorImage()) {
                        srcImage.exportRGBData(c, 0, volSize, buffer);

                        if (c == 1) {
                            minimum = minimumR;
                            maximum = maximumR;
                            fireProgressStateChanged("Performing red entropy minimization");
                        } else if (c == 2) {
                            minimum = minimumG;
                            maximum = maximumG;
                            fireProgressStateChanged("Performing green entropy minimization");
                        } else if (c == 3) {
                            minimum = minimumB;
                            maximum = maximumB;
                            fireProgressStateChanged("Performing blue entropy minimization");
                        }
                    } // if (srcImage.isColorImage())
                    else {
                        srcImage.exportData(0, volSize, buffer);
                    }

                    if (thresholdSelected) {
                        objectBuffer = new boolean[volSize];

                        for (i = 0; i < volSize; i++) {
                            objectBuffer[i] = true;
                        }

                        // Check x = 0;
                        for (i = 0; i < volSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < volSize; i += xDim)

                        // Check x = xDim - 1
                        for (i = xDim - 1; i < volSize; i += xDim) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = xDim - 1; i < volSize; i += xDim)

                        // Check y = 0
                        for (z = 0; z < zDim; z++) {
                            j = z * sliceSize;

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if (buffer[i] < thresholdLevel) {
                                    objectBuffer[i] = false;
                                    backgroundPresent = true;
                                }
                            } // for (x = 0; x < xDim; x++)
                        } // for (z = 0; z < zDim; z++)

                        // Check y = yDim - 1
                        for (z = 0; z < zDim; z++) {
                            j = z * sliceSize;

                            for (x = xDim * (yDim - 1); x < sliceSize; x++) {
                                i = j + x;

                                if (buffer[i] < thresholdLevel) {
                                    objectBuffer[i] = false;
                                    backgroundPresent = true;
                                }
                            } // for (x = xDim*(yDim-1); x < sliceSize; x++)
                        } // z = 0; z < zDim; z++)

                        // Check z = 0;
                        for (i = 0; i < sliceSize; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = 0; i < sliceSize; i++)

                        // Check z = zDim - 1
                        for (i = (zDim - 1) * sliceSize; i < volSize; i++) {

                            if (buffer[i] < thresholdLevel) {
                                objectBuffer[i] = false;
                                backgroundPresent = true;
                            }
                        } // for (i = (zDim-1)*sliceSize; i < volSize; i++)

                        if (backgroundPresent) {
                            found = true;

                            while (found) {
                                found = false;

                                for (z = 1; z < (zDim - 1); z++) {
                                    k = z * sliceSize;

                                    for (y = 1; y < (yDim - 1); y++) {
                                        j = k + (y * xDim);

                                        for (x = 1; x < (xDim - 1); x++) {
                                            i = j + x;

                                            if ((objectBuffer[i]) && (buffer[i] < thresholdLevel) &&
                                                    ((!objectBuffer[i - 1]) || (!objectBuffer[i + 1]) ||
                                                         (!objectBuffer[i - xDim]) || (!objectBuffer[i + xDim]) ||
                                                         (!objectBuffer[i - sliceSize]) ||
                                                         (!objectBuffer[i + sliceSize]))) {
                                                objectBuffer[i] = false;
                                                found = true;
                                            }
                                        } // for (x = 1; x < xDim - 1; x++)
                                    } // for (y = 1; y < yDim - 1; y++)
                                } // for (z = 1; z < zDim - 1; z++)
                            } // while (found)

                            volume = 0;

                            for (i = 0; i < volSize; i++) {

                                if (objectBuffer[i]) {
                                    volume++;
                                }
                            }
                        } // if (backgroundPresent)
                    } // if (thresholdSelected)

                    mc1 = 0.0;
                    mc2 = 0.0;
                    mc3 = 0.0;
                    mc4 = 0.0;
                    mc5 = 0.0;
                    mc6 = 0.0;
                    mc7 = 0.0;
                    mc8 = 0.0;
                    mc9 = 0.0;
                    mc10 = 0.0;
                    mc11 = 0.0;
                    mc12 = 0.0;
                    mc13 = 0.0;
                    mc14 = 0.0;
                    mc15 = 0.0;
                    mc16 = 0.0;
                    mc17 = 0.0;
                    mc18 = 0.0;
                    mc19 = 0.0;
                    grayCount = 0.0;

                    for (z = 0; z < zDim; z++) {
                        k = z * sliceSize;

                        for (y = 0; y < yDim; y++) {
                            j = k + (y * xDim);

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if ((!backgroundPresent) || (objectBuffer[i])) {
                                    mc1 += buffer[i] * x;
                                    mc2 += buffer[i] * y;
                                    mc3 += buffer[i] * z;
                                    mc4 += buffer[i] * x * y;
                                    mc5 += buffer[i] * x * z;
                                    mc6 += buffer[i] * y * z;
                                    mc7 += buffer[i] * x * x;
                                    mc8 += buffer[i] * y * y;
                                    mc9 += buffer[i] * z * z;
                                    mc10 += buffer[i] * x * x * x;
                                    mc11 += buffer[i] * x * x * y;
                                    mc12 += buffer[i] * x * x * z;
                                    mc13 += buffer[i] * x * y * y;
                                    mc14 += buffer[i] * x * y * z;
                                    mc15 += buffer[i] * x * z * z;
                                    mc16 += buffer[i] * y * y * y;
                                    mc17 += buffer[i] * y * y * z;
                                    mc18 += buffer[i] * y * z * z;
                                    mc19 += buffer[i] * z * z * z;
                                    grayCount += buffer[i];
                                }
                            } // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)
                    } // for (z = 0; z < zDim; z++)

                    mc1 = mc1 / grayCount;
                    mc2 = mc2 / grayCount;
                    mc3 = mc3 / grayCount;
                    mc4 = mc4 / grayCount;
                    mc5 = mc5 / grayCount;
                    mc6 = mc6 / grayCount;
                    mc7 = mc7 / grayCount;
                    mc8 = mc8 / grayCount;
                    mc9 = mc9 / grayCount;
                    mc10 = mc10 / grayCount;
                    mc11 = mc11 / grayCount;
                    mc12 = mc12 / grayCount;
                    mc13 = mc13 / grayCount;
                    mc14 = mc14 / grayCount;
                    mc15 = mc15 / grayCount;
                    mc16 = mc16 / grayCount;
                    mc17 = mc17 / grayCount;
                    mc18 = mc18 / grayCount;
                    mc19 = mc19 / grayCount;

                    md1 = 0.0;
                    md2 = 0.0;
                    md3 = 0.0;
                    md4 = 0.0;
                    md5 = 0.0;
                    md6 = 0.0;
                    md7 = 0.0;
                    md8 = 0.0;
                    md9 = 0.0;
                    md10 = 0.0;
                    md11 = 0.0;
                    md12 = 0.0;
                    md13 = 0.0;
                    md14 = 0.0;
                    md15 = 0.0;
                    md16 = 0.0;
                    md17 = 0.0;
                    md18 = 0.0;
                    md19 = 0.0;

                    for (z = 0; z < zDim; z++) {
                        k = z * sliceSize;

                        for (y = 0; y < yDim; y++) {
                            j = k + (y * xDim);

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if ((!backgroundPresent) || (objectBuffer[i])) {
                                    md1 += Math.abs(buffer[i] * (x - mc1));
                                    md2 += Math.abs(buffer[i] * (y - mc2));
                                    md3 += Math.abs(buffer[i] * (z - mc3));
                                    md4 += Math.abs(buffer[i] * ((x * y) - mc4));
                                    md5 += Math.abs(buffer[i] * ((x * z) - mc5));
                                    md6 += Math.abs(buffer[i] * ((y * z) - mc6));
                                    md7 += Math.abs(buffer[i] * ((x * x) - mc7));
                                    md8 += Math.abs(buffer[i] * ((y * y) - mc8));
                                    md9 += Math.abs(buffer[i] * ((z * z) - mc9));
                                    md10 += Math.abs(buffer[i] * ((x * x * x) - mc10));
                                    md11 += Math.abs(buffer[i] * ((x * x * y) - mc11));
                                    md12 += Math.abs(buffer[i] * ((x * x * z) - mc12));
                                    md13 += Math.abs(buffer[i] * ((x * y * y) - mc13));
                                    md14 += Math.abs(buffer[i] * ((x * y * z) - mc14));
                                    md15 += Math.abs(buffer[i] * ((x * z * z) - mc15));
                                    md16 += Math.abs(buffer[i] * ((y * y * y) - mc16));
                                    md17 += Math.abs(buffer[i] * ((y * y * z) - mc17));
                                    md18 += Math.abs(buffer[i] * ((y * z * z) - mc18));
                                    md19 += Math.abs(buffer[i] * ((z * z * z) - mc19));
                                }
                            } // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)
                    } // for (z = 0; z < zDim; z++)

                    md1 = md1 / volume;
                    md2 = md2 / volume;
                    md3 = md3 / volume;
                    md4 = md4 / volume;
                    md5 = md5 / volume;
                    md6 = md6 / volume;
                    md7 = md7 / volume;
                    md8 = md8 / volume;
                    md9 = md9 / volume;
                    md10 = md10 / volume;
                    md11 = md11 / volume;
                    md12 = md12 / volume;
                    md13 = md13 / volume;
                    md14 = md14 / volume;
                    md15 = md15 / volume;
                    md16 = md16 / volume;
                    md17 = md17 / volume;
                    md18 = md18 / volume;
                    md19 = md19 / volume;

                    // The bottom and top grey values of the input image are to be ignored
                    // as they may represent spurious uniform regions caused by the
                    // saturation of the grey level image

                    for (i = 0; i < volSize; i++) {

                        if ((buffer[i] != minimum) && (buffer[i] != maximum)) {
                            validBuffer[i] = true;
                        } else {
                            validBuffer[i] = false;
                        }
                    }

                    if (subsample) {
                        histogramAvailable = 0;

                        for (i = 0; i < volSize; i++) {

                            if (((!backgroundPresent) || (objectBuffer[i])) && (validBuffer[i])) {
                                histogramAvailable++;
                            }
                        } // for (i = 0; i < volSize; i++)

                        pixelIncrement = Math.max(1, (int) Math.pow((double) (histogramAvailable / 5000), 1.0 / 3.0));
                        if (zDim < xDim) {
                            zPixelIncrement = Math.max(1, pixelIncrement * zDim/xDim);
                        }
                        else{
                            zPixelIncrement = pixelIncrement;
                        }
                    } // if (subsample)

                    for (i = 0; i < nParams; i++) {
                        xi[i][i] = 1.0;
                    }

                    powell(p, xi, powellTolerance);
                    m1 = p[0];
                    m2 = p[1];
                    m3 = p[2];
                    m4 = p[3];
                    m5 = p[4];
                    m6 = p[5];
                    m7 = p[6];
                    m8 = p[7];
                    m9 = p[8];
                    m10 = p[9];
                    m11 = p[10];
                    m12 = p[11];
                    m13 = p[12];
                    m14 = p[13];
                    m15 = p[14];
                    m16 = p[15];
                    m17 = p[16];
                    m18 = p[17];
                    m19 = p[18];

                    if (srcImage.isColorImage()) {

                        if (c == 1) {
                            Preferences.debug("Red\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 2) {
                            Preferences.debug("Green\n", Preferences.DEBUG_ALGORITHM);
                        } else if (c == 3) {
                            Preferences.debug("Blue\n", Preferences.DEBUG_ALGORITHM);
                        }
                    } // if (srcImage.isColorImage())

                    Preferences.debug("m1 = " + m1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc1 = " + mc1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md1 = " + md1 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m2 = " + m2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc2 = " + mc2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md2 = " + md2 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m3 = " + m3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc3 = " + mc3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md3 = " + md3 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m4 = " + m4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc4 = " + mc4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md4 = " + md4 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m5 = " + m5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc5 = " + mc5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md5 = " + md5 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m6 = " + m6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc6 = " + mc6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md6 = " + md6 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m7 = " + m7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc7 = " + mc7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md7 = " + md7 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m8 = " + m8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc8 = " + mc8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md8 = " + md8 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m9 = " + m9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc9 = " + mc9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md9 = " + md9 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m10 = " + m10 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc10 = " + mc10 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md10 = " + md10 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m11 = " + m11 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc11 = " + mc11 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md11 = " + md11 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m12 = " + m12 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc12 = " + mc12 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md12 = " + md12 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m13 = " + m13 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc13 = " + mc13 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md13 = " + md13 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m14 = " + m14 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc14 = " + mc14 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md14 = " + md14 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m15 = " + m15 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc15 = " + mc15 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md15 = " + md15 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m16 = " + m16 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc16 = " + mc16 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md16 = " + md16 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m17 = " + m17 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc17 = " + mc17 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md17 = " + md17 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m18 = " + m18 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc18 = " + mc18 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md18 = " + md18 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("m19 = " + m19 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("mc19 = " + mc19 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("md19 = " + md19 + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Iterations of Powell's algorithm = " + iter + "\n", Preferences.DEBUG_ALGORITHM);

                    if (destImage != null) {
                        imageType = destImage.getType();
                    } else {
                        imageType = srcImage.getType();
                    }

                    for (z = 0; z < zDim; z++) {
                        k = z * sliceSize;
                        smz = 1.0 + (m3 * (z - mc3) / md3) + (m9 * ((z * z) - mc9) / md9) +
                              (m19 * ((z * z * z) - mc19) / md19);

                        for (y = 0; y < yDim; y++) {
                            j = k + (y * xDim);
                            smy = smz + (m2 * (y - mc2) / md2) + (m8 * ((y * y) - mc8) / md8) +
                                  (m6 * ((y * z) - mc6) / md6) + (m16 * ((y * y * y) - mc16) / md16) +
                                  (m17 * ((y * y * z) - mc17) / md17) + (m18 * ((y * z * z) - mc18) / md18);

                            for (x = 0; x < xDim; x++) {
                                i = j + x;

                                if ((!backgroundPresent) || (objectBuffer[i])) {
                                    sm = smy + (m1 * (x - mc1) / md1) + (m4 * ((x * y) - mc4) / md4) +
                                         (m7 * ((x * x) - mc7) / md7) + (m5 * ((x * z) - mc5) / md5) +
                                         (m10 * ((x * x * x) - mc10) / md10) + (m11 * ((x * x * y) - mc11) / md11) +
                                         (m12 * ((x * x * z) - mc12) / md12) + (m13 * ((x * y * y) - mc13) / md13) +
                                         (m14 * ((x * y * z) - mc14) / md14) + (m15 * ((x * z * z) - mc15) / md15);

                                    if (sm < 0.05) {
                                        sm = 0.05;
                                    } else if (sm > 20.0) {
                                        sm = 20.0;
                                    }

                                    buffer[i] = (float) (buffer[i] / sm);

                                    switch (imageType) {

                                        case ModelStorageBase.BYTE:
                                            if (buffer[i] < -128.0f) {
                                                buffer[i] = -128.0f;
                                            } else if (buffer[i] > 127.0f) {
                                                buffer[i] = 127.0f;
                                            }

                                            break;

                                        case ModelStorageBase.UBYTE:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 255.0f) {
                                                buffer[i] = 255.0f;
                                            }

                                            break;

                                        case ModelStorageBase.SHORT:
                                            if (buffer[i] < -32768.0f) {
                                                buffer[i] = -32768.0f;
                                            } else if (buffer[i] > 32767.0f) {
                                                buffer[i] = 32767.0f;
                                            }

                                            break;

                                        case ModelStorageBase.USHORT:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 65535.0f) {
                                                buffer[i] = 65535.0f;
                                            }

                                            break;

                                        case ModelStorageBase.INTEGER:
                                            if (buffer[i] < Integer.MIN_VALUE) {
                                                buffer[i] = Integer.MIN_VALUE;
                                            } else if (buffer[i] > Integer.MAX_VALUE) {
                                                buffer[i] = Integer.MAX_VALUE;
                                            }

                                            break;

                                        case ModelStorageBase.UINTEGER:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 4294967295L) {
                                                buffer[i] = 4294967295L;
                                            }

                                            break;

                                        case ModelStorageBase.ARGB:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 255.0f) {
                                                buffer[i] = 255.0f;
                                            }

                                            break;

                                        case ModelStorageBase.ARGB_USHORT:
                                            if (buffer[i] < 0.0f) {
                                                buffer[i] = 0.0f;
                                            } else if (buffer[i] > 65535.0f) {
                                                buffer[i] = 65535.0f;
                                            }

                                            break;
                                    } // switch(imageType)
                                } // if ((!backgroundPresent) || (objectBuffer[i]))
                            } // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)
                    } // for (z = 0; z < zDim; z++)

                    if (destImage != null) {

                        if (destImage.isColorImage()) {
                            destImage.importRGBData(c, 0, buffer, true);
                        } else {
                            destImage.importData(0, buffer, true);
                        }
                    } else {

                        if (srcImage.isColorImage()) {
                            srcImage.importRGBData(c, 0, buffer, true);
                        } else {
                            srcImage.importData(0, buffer, true);
                        }
                    }
                } // if ((!srcImage.isColorImage()) || ((c == 1) && (doRed)) ||
            } // for (c = 1; c <= maxColor; c++)

        } catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy minimization reports:\n" + ioe.toString());

            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Entropy Minimization reports:\n" + error.toString());


            setCompleted(false);

            return;
        }

        setCompleted(true);

        return;
    }

    @Override
    public double eval(double[] x) {
        return entropyFunction( x );
    }


    @Override
    public int getNumberOfVariables() {
        return nParams;
    }

}
