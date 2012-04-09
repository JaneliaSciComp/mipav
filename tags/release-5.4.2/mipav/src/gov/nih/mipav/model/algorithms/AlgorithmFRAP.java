package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;

import de.jtem.numericalMethods.calculus.function.RealFunctionOfOneVariable;
import de.jtem.numericalMethods.calculus.integration.RungeKuttaFehlbergIntegrator;


/**
 * Fluorescence Recovery after PhotoBleaching Only 1 color will be used from a color image.
 *
 * <p>An optional registration may be performed before FRAP. In this registration slices are registered to the first
 * slice after photobleaching - firstSliceNum. AlgorithmRegOAR25D is used with the cost function and firstSliceNum being
 * the only registration parameters the user can vary in the dialog box. Correlation ratio is the default cost function,
 * but the user can also select least squares, normalized cross correlation, or normalized mutual information. The FRAP
 * will be performed on the registered image rather than on the original image.</p>
 *
 * <p>The narrow band code is based on compartmental models. These compartments must be homogeneous in composition;
 * there must be no spatial gradients inside a compartment. If a compartment is not homogeneous, it must be divided into
 * multiple compartments or partial differential equations must be used to describe the system. The code here uses
 * equations in the Carrero reference for the case which has a photobleached narrow band with 2 neighboring unbleached
 * regions.</p>
 *
 * <p>Solutions are also implemented for the pure 1D diffusion case and for the single exponential model.</p>
 *
 * <p>The 2D circle case is implemented according to the full reaction-diffusion system model equations found in
 * Analysis of Binding Reactions by Fluorescence Recovery by Brian L. Sprague, Robert L. Pego, Diana A. Stavreva, and
 * James G. McNally.</p>
 *
 * <p>The code here assumes the presence of 1, 2 or 3 VOI regions. The photobleached region is always required. The
 * whole organ region is required for whole organ normalization, which must always be used in the narrow band case,
 * since Fa/Fo, the afterBeforeRatio = wholeOrganIntensity[firstSliceNum]/wholeOrganIntensity[firstSliceNum - 1] is
 * required in this case. However, whole organ normalization is optional in the pure 1D diffusion and single exponential
 * cases and whole organ normalization is never used in the 2D circle case. Curves can be placed in any slice. There is
 * no reason to propagate curves to more than 1 slice. When the algorithm executes, the photobleached and whole organ
 * VOIs will be propagated to the other slices.</p>
 *
 * <p>Radio buttons in the dialog box are used to select a red photobleached, a green whole organ, or a blue background
 * VOI. Either an ellipse VOI, rectangle VOI, polyline VOI, or levelset VOI will be selected from the top MIPAV toolbar.
 * There is no need to hit the NEW_VOI button. The photobleached VOI should be contained within the whole organ VOI. The
 * whole organ region will have a greater average intensity than the photobleached region and the background region will
 * have a smaller average intensity than the photobleached region.</p>
 *
 * <p>The VOIs should be set after the dialog is created, so that the photobleached VOI has the correct red hue = 0.0,
 * the whole organ VOI has the correct green hue = 1/3, and the background VOI has the correct blue hue = 2/3. If the
 * VOIs are set before the dialog is created, the VOIs might not have the required hue values of 0, 1/3, or 2/3.</p>
 *
 * <p>The average of the background VOI is used to obtain the backgroundConstant which is subtracted from the
 * photoBleachedIntensity and wholeOrganIntensity arrays.</p>
 *
 * <p>Then, Fa/F0 of the whole organ VOI is obtained, the ratio of the first slice after photobleaching to the slice
 * just before photobleaching. If whole organ normalization is used, the background corrected photobleached values are
 * divided by the background corrected whole organ values to correct for the loss of fluorescence.</p>
 *
 * <p>Even with low illumination during the recovery phase, there is expected to be fluorescence loss through
 * photobleaching over the course of the recovery curve. For this loss the time that matters is the exposure time for
 * each slice picture and not the absolute time at which the exposure was made so the time here is be proportional to
 * the number of slices.</p>
 *
 * <p>If whole organ normalization is used, the values of corrected photobleached/corrected whole organ are normalized
 * by dividing by the value of this ratio just before photobleaching. These normalized photobleached ratios are fitted
 * to a slightly modified version of equation 19 in the Carrero article: R(t;alpha,beta,gamma) = bottom + span*(1 -
 * gamma*exp(alpha*t) - (1 - gamma)*exp(beta*t)) The mobile fraction equals bottom + span. In this curve fitting the
 * time used is the time elapsed since photobleaching. Alpha, beta, gamma, and the ratio of the whole organ region
 * fluorescence after bleaching to before bleaching are output. The dissociation rate, the association rate, and the
 * diffusion transfer coefficient are obtained from alpha, beta, gamma, and the afterBeforeRatio.</p>
 *
 * <p>For 1D pure diffusion fitting: I(t) = Ifinal(1 - 1/sqrt(1 + 4*PI*D*t/w**2)), where w is the width of the bleaching
 * along the membrane in um and D is the effective 1-D diffusion to fit (um**2/sec). Solve for D/w**2</p>
 *
 * <p>For the 2D circle case: p is a complex Laplace variable average frap(p) = 2*(Feq/p)*I1(qw)*K1(qw))*(1 + kon/(p +
 * koff)) w is the spot radius in um. q**2 = (p/Df)*(1 + kon/(p + koff)) Df is the diffusion constant in micrometers
 * squared per second I1 and K1 are modified Bessel functions of the first and second kind. Feq = koff/(kon + koff) Ceq
 * = kon/(kon + koff) frap(t) is obtained by numerically taking the inverse Laplace transform of frap(p). In the 2D
 * circle case the user inputs the radius and diffusion constant and nonlinear fitting is used to obtain kon and koff.
 * </p>
 *
 * <p>If whole organ normalization is not used, the values of corrected photobleached are normalized by assuming a
 * mobile fraction of 1 and dividing by the maximum value of the corrected photobleached array occurring after
 * bleaching.</p>
 *
 * <p>For narrow band 2D the effective diffusion coefficient in micrometers**2/sec is found from the diffusion transfer
 * coefficient. The photobleached width is measured as the minimum dimension of the photobleached VOI bounding box and
 * the whole organ length is measured along the same direction. Deff = Dt * photobleached width * (whole organ length -
 * photobleached width)/4.0 Note that the expected asymptote effect as whole organ length goes to infinity is missing
 * from this equation. For pure 1D diffusion: Deff = D/w**2 * photobleached width * photobleached width</p>
 *
 * <p>An optional checkbox titled Grid search with parameter variation can be selected to doublecheck the nonlinear
 * fitting results with a simple grid search pattern. The sum of squares of errors (sse) is calculated at every point on
 * a grid. The sum of the squares of the (fitted photobleached data minus the actual photobleached data) is found for
 * every slice starting with the first slice after photobleaching. For 2D narrow band a 3D grid with 201 alpha points,
 * 201 beta points, and 101 gamma points is used. alpha and beta are both varied from 0.05 times the values found in the
 * nonlinear fit to 20 times the values found in the nonlinear fit. The values are geometrically incremented so that
 * each value is about 1.03 times as great as the previous value. gamma is arithmetically incremented by 0.01 from 0 to
 * 1. The global sse minimum is found and in addition if present any local sse minimums which are the lowest point in a
 * 5 by 5 by 5 cube are also found. The 3D space is searched with the restriction that beta <= alpha. Points with beta >
 * alpha are not included in the 3D search and are simply filled in with sse values equal to 1.1 times the sse maximum
 * found over the permissible space. The search is conducted with the bottom and span values kept fixed at the values
 * found by the nonlinear fit. Since these values are very likely to have been accurately determined, this should not be
 * a problem. In any event a search over a five dimensional space would be very time consuming. A 201 by 201 by 101 3D
 * error image is created to display the calculated sse values. The error image name is the source image name with _err
 * appended on the end. Point VOIs appear at the locations of the global minimum and at local minimums if any exist. The
 * point VOIs are stored in a folder titled defaultVOI_sourceImageName_err. For the pure 1D fit and the single
 * exponential fit the search is simply one dimensional so no error image is created. For the pure 1D fit 201 D/w**2
 * values going from 0.05 times the nonlinear fit value to 20 times the nonlinear fit value are used to calculate sse.
 * For the single exponential fit 201 thalf values going form 0.05 times the nonlinear fit value to 20 times the
 * nonlinear fit value are used to calculate sse. For circular 2D diffusion a 201 by 201 kon by koff grid is generated.
 * </p>
 *
 * <p>References: 1.) Analysis of Binding Reactions by Fluorescence Recovery after Photobleaching by Brian L. Sprague,
 * Robert L. Pego, Diana A. Stavera, and James G. McNally, Bipphysical Journal, Volume 86, June 2004, pp. 3473-3495.</p>
 *
 * <p>2.) Mobility Measurement By Analysis of Fluorescence Photobleaching Recovery Kinetics by D. Axelrod, D.E. Koppel,
 * J. Schlessinger, E. Elson, and W.W. Webb, Biophysical Journal, Volume 16, 1976, pp. 1055-1069.</p>
 *
 * <p>3.) Monitoring the Dynamics and Mobility of Membrane Proteins Tagged with Green Flurorescent Protein by J.
 * Lippincott-Schwartz, J.F. Presley, K.J.M. Zaal, K. Hirschberg, C.D. Miller, and J. Ellenberg, Methods in Cell
 * Biology, Volume 58, 1999, pp. 261-281.</p>
 *
 * <p>4.) Using FRAP and mathematical modeling to determine the in vivo kinetics of nuclear proteins by Gustavo Carrero,
 * Darin McDonald, Ellen Crawford, Gerda de Vries, and Michael J. Hendzel, Methods, Volume 29, 2003, pp. 14-28</p>
 *
 * <p>5.) Practical Kinetic Modeling of Large Scale Biological Systems by Robert D. Phair at
 * http://www.bioinformaticsservices.com/bis/resources/ cybertext/IBcont.html</p>
 */
public class AlgorithmFRAP extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Diffusion models. */
    private static final int NARROW_BAND_2D = 1;

    /** DOCUMENT ME! */
    private static final int CIRCLE_2D = 2;

    /** DOCUMENT ME! */
    private static final int PURE_1D = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int backgroundIndex;

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private boolean createRegImage;

    /** Diffusion constant in um*um/sec. */
    private float diffusion;

    /** DOCUMENT ME! */
    private int firstSliceNum;

    /** DOCUMENT ME! */
    private int model = NARROW_BAND_2D;

    /** DOCUMENT ME! */
    private boolean paramVary;

    /** DOCUMENT ME! */
    private int photoBleachedIndex;

    /** Radius of bleach spot in um. */
    private float radius;

    /** DOCUMENT ME! */
    private boolean register;

    /** DOCUMENT ME! */
    private double[] timeStamp = null;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    /** DOCUMENT ME! */
    private boolean useTestData = true;

    /** DOCUMENT ME! */
    private int wholeOrganIndex;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmFRAP object.
     *
     * @param  srcImg              source image
     * @param  useRed              If true, use the red color values for the FRAP
     * @param  useGreen            If true, use the green color values for the FRAP
     * @param  useBlue             If true, use the blue color values for the FRAP
     * @param  firstSliceNum       first slice after bleach operation
     * @param  photoBleachedIndex  the index of the photoBleached VOI
     * @param  wholeOrganIndex     the index of the wholeOrgan VOI if >= 0
     * @param  backgroundIndex     the index of the background VOI if >= 0
     * @param  model               NARROW_BAND_2D, CIRCLE_2D, or PURE_1D
     * @param  register            If true register all slices to the wholeOrgan slice before FRAP
     * @param  cost                Cost function used in registration
     * @param  createRegImage      If register = true and createRegImage = true, then create a frame with the registered
     *                             image
     * @param  paramVary           Calculate sum of square of errors for different parameter values and output the
     *                             minimum
     * @param  radius              radius of bleach spot in um.
     * @param  diffusion           diffusion constant in um*um/sec
     */
    public AlgorithmFRAP(ModelImage srcImg, boolean useRed, boolean useGreen, boolean useBlue, int firstSliceNum,
                         int photoBleachedIndex, int wholeOrganIndex, int backgroundIndex, int model, boolean register,
                         int cost, boolean createRegImage, boolean paramVary, float radius, float diffusion) {

        super(null, srcImg);

        this.useRed = useRed;
        this.useGreen = useGreen;
        this.useBlue = useBlue;
        this.firstSliceNum = firstSliceNum;
        this.photoBleachedIndex = photoBleachedIndex;
        this.wholeOrganIndex = wholeOrganIndex;
        this.backgroundIndex = backgroundIndex;
        this.model = model;
        this.register = register;
        this.cost = cost;
        this.createRegImage = createRegImage;
        this.paramVary = paramVary;
        this.radius = radius;
        this.diffusion = diffusion;
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
    @SuppressWarnings("unchecked")
    public void runAlgorithm() {
        int i, j, z;
        int c = 0;
        int c2 = 0;
        int c3 = 0;
        int xDim, yDim, zDim, sliceSize;
        float[] floatBuffer;

        // black and white image created from selected color of color image
        ModelImage bwImage = null;
        ModelImage bwImage2 = null;
        ModelImage bwImage3 = null;
        ModelImage colorImageReg = null;
        VOIVector VOIs;
        AlgorithmRegOAR25D2 regAlgo = null;
        boolean haveLower;
        boolean haveUpper;
        int lower;
        int upper;
        short[] mask;
        short[] organMask = null;
        float[] wholeOrganIntensity = null;
        float[] photoBleachedIntensity;
        float[] pIntensity;
        float backgroundIntensity;
        float backgroundConstant = 0.0f;
        int[] wholeOrganCount = null;
        int[] photoBleachedCount;
        int backgroundCount;
        double[] initial;
        double[] tValues;
        float[][] tfValues;
        float[][] pfValues;
        float[][] pfValues2 = null;
        boolean doSecondFit = false;
        float[] tgValues;
        double refStamp;
        double[] params;
        FitDoubleExponentialModel fdem = null;
        FitDoubleExponentialNoWholeModel fdemnw = null;
        FitPure1DModel fp1D = null;
        FitPure1DNoWholeModel fp1DNW = null;
        FitSingleExponentialModel fsem = null;
        FitSingleExponentialNoWholeModel fsemnw = null;
        boolean haveHalf;
        double afterBeforeRatio = 1.0;
        double s1, s2;
        double kdD;
        double num;
        double denom;
        double ka, kd, Dt, Deff;
        FileInfoLSM fileInfo;
        FileInfoImageXML fileInfoImageXML;
        ViewJFrameGraph photoBleachGraph;
        ViewJFrameGraph photoBleachGraph2;
        ViewJFrameGraph ucPhotoBleachGraph;
        ViewJFrameGraph wholeOrganGraph;
        @SuppressWarnings("unused")
        ViewJFrameImage imageFrame;
        ModelImage errorImage;
        ViewJFrameImage errorFrame;
        double minR, minG, minB;
        double maxR, maxG, maxB;
        boolean haveRed = false;
        boolean haveGreen = false;
        boolean haveBlue = false;
        int colorsPresent = 0;
        Color[] colorArray;

        // By default a ViewJFrameGraph is put at 50,50 by setVisible
        int yStart = 50;
        double dTemp;

        // mobile fraction
        float mf = 1.0f;
        float pMin;
        float pMax;
        float pNorm;
        float pHalf;
        double bottom;
        double span;
        float[] xBounds = new float[2];
        float[] yBounds = new float[2];
        float[] zBounds = new float[2];
        float wholeOrganLength = 0.0f;
        float photoBleachedWidth, photoBleachedWidthX, photoBleachedWidthY;
        int sourceUnitsX;
        float resX;
        float newResX;
        int sourceUnitsY;
        float resY;
        float newResY;
        DecimalFormat nf;
        String dataString = "";

        if (useTestData) {
            //double[] cyr = new double[1];
            //double[] cyi = new double[1];
            //int[] nz = new int[1];
            //int[] errorFlag = new int[1];
            //double rek0;
            //double imk0;
            //double rek1;
            //double imk1;
            //double reVar;
            //double imVar;

            // Bessel testBessel = new Bessel( Bessel.AIRY_AI, true );
            // new Bessel(Bessel.AIRY_BI,2.0,0.0,0,Bessel.UNSCALED_FUNCTION,cyr,cyi,nz,errorFlag);
            /*Bessel testBessel =
             * new Bessel(Bessel.BESSEL_I,0.0,1.5,1.0,Bessel.UNSCALED_FUNCTION,1,cyr,cyi,nz,errorFlag);
             * testBessel.run(); Preferences.debug("I1(1.5i) = " + cyr[0] + " i*" + cyi[0] + "\n", 
             * Preferences.DEBUG_ALGORITHM);
             *
             * testBessel = new Bessel(Bessel.BESSEL_J,1.5,0.0,1.0,Bessel.UNSCALED_FUNCTION,1,cyr,cyi,nz,errorFlag);
             * testBessel.run();Preferences.debug("J1(1.5) = " + cyr[0] + " i*" + cyi[0] + "\n", 
             * Preferences.DEBUG_ALGORITHM);*/


            // runLapTest2();*/
            // runLapTestqd();
            // runFullModelTest();

            // runIntegrationTest();
            // runIntegrationTest2();
            // FFTUtility fft = new FFTUtility(FFTUtility.SELF_TEST);
            // fft.run();
            return;
        }

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Performing FRAP ...");

        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        floatBuffer = new float[sliceSize];
        VOIs = srcImage.getVOIs();
        nf = new DecimalFormat("0.00E0");

        // The photobleaching recovery curve starts at firstSliceNum
        tValues = new double[zDim - firstSliceNum];

        for (i = 0; i < (zDim - firstSliceNum); i++) {
            tValues[i] = (double) i * srcImage.getFileInfo()[0].getResolutions()[2];
        }

        try { // In this case, the file must be LSM
            fileInfo = (FileInfoLSM) srcImage.getFileInfo(0);

            if (fileInfo.getTimeStamp() != null) {

                // The time increments between slices need not be constant
                // Get the absolute times if they are available
                timeStamp = fileInfo.getTimeStamp();
                refStamp = timeStamp[firstSliceNum];

                for (z = 0; z < zDim; z++) {
                    timeStamp[z] -= refStamp;
                }

                for (z = firstSliceNum; z < zDim; z++) {
                    tValues[z - firstSliceNum] = timeStamp[z];
                }
            }
        } catch (ClassCastException e) { // If it isn't, catch the exception
        }

        try { // In this case, the file must be XML

            int timeStampLength = 0;
            double[] timeStampTemp = new double[1000];
            fileInfoImageXML = (FileInfoImageXML) srcImage.getFileInfo(0);

            /** go through the hashtable of parameter sets */
            Enumeration<String> setEnum = fileInfoImageXML.getPSetKeys();

            while (setEnum.hasMoreElements()) {
                String temp = setEnum.nextElement();
                Enumeration<String> paramEnum = fileInfoImageXML.getPSet(temp).getParameterKeys();

                while (paramEnum.hasMoreElements()) {
                    String paramName = paramEnum.nextElement();

                    if ((paramName.length() > 11) && (paramName.substring(0, 10).equals("timeStamp["))) {
                        int endIndex = paramName.indexOf(']');
                        String arrayIndexString = paramName.substring(10, endIndex);
                        int arrayIndex = Integer.valueOf(arrayIndexString).intValue();
                        String timeStampString = fileInfoImageXML.getPSet(temp).getParameter(paramName).getValue();
                        timeStampTemp[arrayIndex] = Double.valueOf(timeStampString).doubleValue();
                        timeStampLength++;
                    }
                } // while (paramEnum.hasMoreElements())
            } // while (setEnum.hasMoreElements())

            if (timeStampLength > 0) {
                timeStamp = new double[timeStampLength];

                for (i = 0; i < timeStamp.length; i++) {
                    timeStamp[i] = timeStampTemp[i];
                }

                refStamp = timeStamp[firstSliceNum];

                for (z = 0; z < zDim; z++) {
                    timeStamp[z] -= refStamp;
                }

                for (z = firstSliceNum; z < zDim; z++) {
                    tValues[z - firstSliceNum] = timeStamp[z];
                }
            } // if (timeStampLength > 0)
        } catch (ClassCastException e) { // If it isn't, catch the exception
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

            bwImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_bw");

            if (createRegImage && (colorsPresent >= 2)) {
                bwImage2 = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(),
                                          srcImage.getImageName() + "_bw2");
                colorImageReg = new ModelImage(srcImage.getType(), srcImage.getExtents(),
                                               srcImage.getImageName() + "_registered");
            }

            if (createRegImage && (colorsPresent == 3)) {
                bwImage3 = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(),
                                          srcImage.getImageName() + "_bw3");
            }

            if (useRed) {
                c = 1;
            } else if (useGreen) {
                c = 2;
            } else {
                c = 3;
            }

            for (z = 0; z < zDim; z++) {

                try {
                    srcImage.exportRGBData(c, 4 * z * sliceSize, sliceSize, floatBuffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData");
                    setCompleted(false);

                    return;
                }

                try {
                    bwImage.importData(z * sliceSize, floatBuffer, false);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException " + e + " on bwImage.importData");
                    setCompleted(false);

                    return;
                }
            } // for (z = 0; z < zDim; z++)

            bwImage.calcMinMax();
            bwImage.setVOIs(srcImage.getVOIs());

            if (createRegImage && !useRed && haveRed) {
                c2 = 1;
            } else if (createRegImage && !useGreen && haveGreen) {
                c2 = 2;
            } else if (createRegImage && !useBlue && haveBlue) {
                c2 = 3;
            }

            if (c2 > 0) {

                for (z = 0; z < zDim; z++) {

                    try {
                        srcImage.exportRGBData(c2, 4 * z * sliceSize, sliceSize, floatBuffer);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData");
                        setCompleted(false);

                        return;
                    }

                    try {
                        bwImage2.importData(z * sliceSize, floatBuffer, false);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException " + e + " on bwImage2.importData");
                        setCompleted(false);

                        return;
                    }
                } // for (z = 0; z < zDim; z++)

                bwImage2.calcMinMax();

                if ((c2 == 1) && !useGreen && haveGreen) {
                    c3 = 2;
                } else if (((c2 == 1) || (c2 == 2)) && !useBlue && haveBlue) {
                    c3 = 3;
                }
            } // if (c2 > 0)

            if (c3 > 0) {

                for (z = 0; z < zDim; z++) {

                    try {
                        srcImage.exportRGBData(c3, 4 * z * sliceSize, sliceSize, floatBuffer);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData");
                        setCompleted(false);

                        return;
                    }

                    try {
                        bwImage3.importData(z * sliceSize, floatBuffer, false);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException " + e + " on bwImage3.importData");
                        setCompleted(false);

                        return;
                    }
                } // for (z = 0; z < zDim; z++)

                bwImage3.calcMinMax();
            } // if (c3 > 0)
        } // if (srcImage.isColorImage())
        else {
            bwImage = (ModelImage) srcImage.clone();
            bwImage.calcMinMax();
            bwImage.setVOIs(srcImage.getVOIs());
        }

        if (register) {
            fireProgressStateChanged("Registering slices");

            int DOF = 3; // rigid transformation
            int interp = AlgorithmTransform.BILINEAR;
            int interp2 = AlgorithmTransform.BILINEAR;
            boolean doAdjacent = false;
            float rotateBegin = -3.0f;
            float rotateEnd = 3.0f;
            float coarseRate = 3.0f;
            float fineRate = 1.0f;
            boolean doGraph = false;
            boolean doSubsample = false;
            boolean transformVOIs = true;
            int maxIterations = 2;
            int numMinima = 3;
            regAlgo = new AlgorithmRegOAR25D2(bwImage, cost, DOF, interp, interp2, doAdjacent, firstSliceNum,
                                              rotateBegin, rotateEnd, coarseRate, fineRate, doGraph, doSubsample,
                                              transformVOIs, maxIterations, numMinima);

            if (bwImage2 != null) {
                regAlgo.setInputImage2(bwImage2);
            }

            if (bwImage3 != null) {
                regAlgo.setInputImage3(bwImage3);
            }

            regAlgo.run();
            //registration algorithm is no longer needed, freeing memory
            regAlgo.disposeLocal();
            regAlgo.finalize();
            regAlgo = null;
            VOIs = bwImage.getVOIs();

            // Propagate the 2 photobleach and whole organ VOIs to all the slices
            haveLower = false;
            lower = 0;
            Vector<VOIBase>[] sortedCurves = VOIs.VOIAt(photoBleachedIndex).getSortedCurves(zDim);
            for (z = 0; (z < zDim) && (!haveLower); z++) {

                if (sortedCurves[z].size() > 0) {
                    haveLower = true;
                    lower = z;
                }
            }

            Vector<VOIBase> copyCurves = new Vector<VOIBase>();
            for (j = 0; j < sortedCurves[lower].size(); j++) {
                copyCurves.add(sortedCurves[lower].elementAt(j));
            }
            
            for (z = lower - 1; z >= 0; z--) {

                for (j = 0; j < copyCurves.size(); j++) {
                    VOIs.VOIAt(photoBleachedIndex).importPolygon(((VOIContour)copyCurves.elementAt(j)).exportPolygon(), z);
                }
            }

            haveUpper = false;
            upper = zDim - 1;
            sortedCurves = VOIs.VOIAt(photoBleachedIndex).getSortedCurves(zDim);
            for (z = zDim - 1; (z >= lower) && (!haveUpper); z--) {

                if (sortedCurves[z].size() > 0) {
                    haveUpper = true;
                    upper = z;
                }
            }

            for (z = upper + 1; z < zDim; z++) {

                for (j = 0; j < copyCurves.size(); j++) {
                    VOIs.VOIAt(photoBleachedIndex).importPolygon(((VOIContour)copyCurves.elementAt(j)).exportPolygon(), z);
                }
            }

            if (wholeOrganIndex >= 0) {
                haveLower = false;
                lower = 0;

                sortedCurves = VOIs.VOIAt(wholeOrganIndex).getSortedCurves(zDim);
                for (z = 0; (z < zDim) && (!haveLower); z++) {

                    if (sortedCurves[z].size() > 0) {
                        haveLower = true;
                        lower = z;
                    }
                }

                copyCurves = new Vector<VOIBase>();
                for (j = 0; j < sortedCurves[lower].size(); j++) {
                    copyCurves.add(sortedCurves[lower].elementAt(j));
                }

                for (z = lower - 1; z >= 0; z--) {

                    for (j = 0; j < copyCurves.size(); j++) {
                        VOIs.VOIAt(wholeOrganIndex).importPolygon(((VOIContour)copyCurves.elementAt(j)).exportPolygon(), z);
                    }
                }

                upper = zDim - 1;
                haveUpper = false;

                sortedCurves = VOIs.VOIAt(wholeOrganIndex).getSortedCurves(zDim);
                for (z = zDim - 1; (z >= lower) && (!haveUpper); z--) {

                    if (sortedCurves[z].size() > 0) {
                        haveUpper = true;
                        upper = z;
                    }
                }

                for (z = upper + 1; z < zDim; z++) {

                    for (j = 0; j < copyCurves.size(); j++) {
                        VOIs.VOIAt(wholeOrganIndex).importPolygon(((VOIContour)copyCurves.elementAt(j)).exportPolygon(), z);
                    }
                }
            } // if (wholeOrganIndex >= 0)

            if (createRegImage) {

                if (colorsPresent >= 2) {

                    for (z = 0; z < zDim; z++) {

                        try {
                            bwImage.exportData(z * sliceSize, sliceSize, floatBuffer);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException " + e + " on bwImage.exportData");
                            setCompleted(false);

                            return;
                        }

                        try {
                            colorImageReg.importRGBData(c, 4 * z * sliceSize, floatBuffer, false);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException " + e + " on colorImageReg.importRGBData");
                            setCompleted(false);

                            return;
                        }
                    } // for (z = 0; z < zDim; z++)

                    for (z = 0; z < zDim; z++) {

                        try {
                            bwImage2.exportData(z * sliceSize, sliceSize, floatBuffer);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException " + e + " on bwImage2.exportData");
                            setCompleted(false);

                            return;
                        }

                        try {
                            colorImageReg.importRGBData(c2, 4 * z * sliceSize, floatBuffer, false);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException " + e + " on colorImageReg.importRGBData");
                            setCompleted(false);

                            return;
                        }
                    } // for (z = 0; z < zDim; z++)

                    if (colorsPresent == 3) {

                        for (z = 0; z < zDim; z++) {

                            try {
                                bwImage3.exportData(z * sliceSize, sliceSize, floatBuffer);
                            } catch (IOException e) {
                                MipavUtil.displayError("IOException " + e + " on bwImage3.exportData");
                                setCompleted(false);

                                return;
                            }

                            try {
                                colorImageReg.importRGBData(c3, 4 * z * sliceSize, floatBuffer, false);
                            } catch (IOException e) {
                                MipavUtil.displayError("IOException " + e + " on colorImageReg.importRGBData");
                                setCompleted(false);

                                return;
                            }
                        } // for (z = 0; z < zDim; z++)
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

                        for (z = 0; z < zDim; z++) {

                            try {
                                colorImageReg.importRGBData(c3, 4 * z * sliceSize, floatBuffer, false);
                            } catch (IOException e) {
                                MipavUtil.displayError("IOException " + e + " on colorImageReg.importRGBData");
                                setCompleted(false);

                                return;
                            }
                        } // for (z = 0; z < zDim; z++)
                    } // else colorsPresent == 2

                    colorImageReg.calcMinMax();
                    colorImageReg.setVOIs(VOIs);
                    updateFileInfo(srcImage, colorImageReg);
                    bwImage.setVOIs(VOIs);
                    colorImageReg.setImageName(srcImage.getImageName() + "_registered");
                    imageFrame = new ViewJFrameImage(colorImageReg, null, new Dimension(610, 200));
                } else {
                    bwImage.setImageName(srcImage.getImageName() + "_registered");
                    bwImage.setVOIs(VOIs);
                    updateFileInfo(srcImage, bwImage);
                    imageFrame = new ViewJFrameImage(bwImage, null, new Dimension(610, 200));
                }
            } // if (createRegImage)
        } // if (register)
        else { // not registered

            // Propagate the 2 photobleach and whole organ VOIs to all the slices
            haveLower = false;
            lower = 0;

            Vector<VOIBase>[] sortedCurves = VOIs.VOIAt(photoBleachedIndex).getSortedCurves(zDim);
            for (z = 0; (z < zDim) && (!haveLower); z++) {

                if (sortedCurves[z].size() > 0) {
                    haveLower = true;
                    lower = z;
                }
            }

            Vector<VOIBase> copyCurves = new Vector<VOIBase>();
            for (j = 0; j < sortedCurves[lower].size(); j++) {
                copyCurves.add(sortedCurves[lower].elementAt(j));
            }
            

            for (z = lower - 1; z >= 0; z--) {

                for (j = 0; j < copyCurves.size(); j++) {
                    VOIs.VOIAt(photoBleachedIndex).importPolygon(((VOIContour)copyCurves.elementAt(j)).exportPolygon(), z);
                }
            }

            haveUpper = false;
            upper = zDim - 1;
            sortedCurves = VOIs.VOIAt(photoBleachedIndex).getSortedCurves(zDim);
            for (z = zDim - 1; (z >= lower) && (!haveUpper); z--) {

                if (sortedCurves[z].size() > 0) {
                    haveUpper = true;
                    upper = z;
                }
            }

            for (z = upper + 1; z < zDim; z++) {

                for (j = 0; j < copyCurves.size(); j++) {
                    VOIs.VOIAt(photoBleachedIndex).importPolygon(((VOIContour)copyCurves.elementAt(j)).exportPolygon(), z);
                }
            }

            if (wholeOrganIndex >= 0) {
                haveLower = false;
                lower = 0;

                sortedCurves = VOIs.VOIAt(wholeOrganIndex).getSortedCurves(zDim);
                for (z = 0; (z < zDim) && (!haveLower); z++) {
                    if (sortedCurves[z].size() > 0) {
                        haveLower = true;
                        lower = z;
                    }
                }

                copyCurves = new Vector<VOIBase>();
                for (j = 0; j < sortedCurves[lower].size(); j++) {
                    copyCurves.add(sortedCurves[lower].elementAt(j));
                }
                

                for (z = lower - 1; z >= 0; z--) {

                    for (j = 0; j < copyCurves.size(); j++) {
                        VOIs.VOIAt(wholeOrganIndex).importPolygon(((VOIContour)copyCurves.elementAt(j)).exportPolygon(), z);
                    }
                }

                haveUpper = false;
                upper = zDim - 1;

                sortedCurves = VOIs.VOIAt(wholeOrganIndex).getSortedCurves(zDim);
                for (z = zDim - 1; (z >= lower) && (!haveUpper); z--) {
                    if (sortedCurves[z].size() > 0) {
                        haveUpper = true;
                        upper = z;
                    }
                }

                for (z = upper + 1; z < zDim; z++) {

                    for (j = 0; j < copyCurves.size(); j++) {
                        VOIs.VOIAt(wholeOrganIndex).importPolygon(((VOIContour)copyCurves.elementAt(j)).exportPolygon(), z);
                    }
                }
            } // if (wholeOrganIndex >= 0)

            srcImage.setVOIs(VOIs);
            bwImage.setVOIs(VOIs);
        } // else not registered

        sourceUnitsX = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        resX = srcImage.getFileInfo(0).getResolutions()[0];

        // Convert to micrometers
        if (sourceUnitsX == Unit.MILLIMETERS.getLegacyNum()) {
            newResX = 1.0e3f * resX;
        } else if (sourceUnitsX == Unit.INCHES.getLegacyNum()) {
            newResX = 2.54e4f * resX;
        } else if (sourceUnitsX == Unit.MILS.getLegacyNum()) {
            newResX = 2.54e1f * resX;
        } else if (sourceUnitsX == Unit.CENTIMETERS.getLegacyNum()) {
            newResX = 1.0e4f * resX;
        } else if (sourceUnitsX == Unit.ANGSTROMS.getLegacyNum()) {
            newResX = 1.0e-4f * resX;
        } else if (sourceUnitsX == Unit.NANOMETERS.getLegacyNum()) {
            newResX = 1.0e-3f * resX;
        } else if (sourceUnitsX == Unit.MICROMETERS.getLegacyNum()) {
            newResX = resX;
        } else if (sourceUnitsX == Unit.METERS.getLegacyNum()) {
            newResX = 1.0e6f * resX;
        } else if (sourceUnitsX == Unit.KILOMETERS.getLegacyNum()) {
            newResX = 1.0e9f * resX;
        } else if (sourceUnitsX == Unit.MILES.getLegacyNum()) {
            newResX = 1.6093e9f * resX;
        } else {
            newResX = resX;
        }

        sourceUnitsY = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        resY = srcImage.getFileInfo(0).getResolutions()[1];

        // Convert to micrometers
        if (sourceUnitsY == Unit.MILLIMETERS.getLegacyNum()) {
            newResY = 1.0e3f * resY;
        } else if (sourceUnitsY == Unit.INCHES.getLegacyNum()) {
            newResY = 2.54e4f * resY;
        } else if (sourceUnitsY == Unit.MILS.getLegacyNum()) {
            newResY = 2.54e1f * resY;
        } else if (sourceUnitsY == Unit.CENTIMETERS.getLegacyNum()) {
            newResY = 1.0e4f * resY;
        } else if (sourceUnitsY == Unit.ANGSTROMS.getLegacyNum()) {
            newResY = 1.0e-4f * resY;
        } else if (sourceUnitsY == Unit.NANOMETERS.getLegacyNum()) {
            newResY = 1.0e-3f * resY;
        } else if (sourceUnitsY == Unit.MICROMETERS.getLegacyNum()) {
            newResY = resY;
        } else if (sourceUnitsY == Unit.METERS.getLegacyNum()) {
            newResY = 1.0e6f * resY;
        } else if (sourceUnitsY == Unit.KILOMETERS.getLegacyNum()) {
            newResY = 1.0e9f * resY;
        } else if (sourceUnitsY == Unit.MILES.getLegacyNum()) {
            newResY = 1.6093e9f * resY;
        } else {
            newResY = resY;
        }

        VOIs.VOIAt(photoBleachedIndex).getBounds(xBounds, yBounds, zBounds);
        photoBleachedWidthX = Math.abs(newResX * (xBounds[1] - xBounds[0]));
        photoBleachedWidthY = Math.abs(newResY * (yBounds[1] - yBounds[0]));
        photoBleachedWidth = Math.min(photoBleachedWidthX, photoBleachedWidthY);
        Preferences.debug("photobleached region width = " + photoBleachedWidth + " microns\n", Preferences.DEBUG_ALGORITHM);
        ViewUserInterface.getReference().setDataText("photobleached region width = " + nf.format(photoBleachedWidth) +
                                                     " microns\n");
        dataString += "photobleached region width = " + nf.format(photoBleachedWidth) + " microns\n";

        if (wholeOrganIndex >= 0) {
            VOIs.VOIAt(wholeOrganIndex).getBounds(xBounds, yBounds, zBounds);

            if (photoBleachedWidthY < photoBleachedWidthX) {
                wholeOrganLength = Math.abs(newResY * (yBounds[1] - yBounds[0]));
            } else {
                wholeOrganLength = Math.abs(newResX * (xBounds[1] - xBounds[0]));
            }

            Preferences.debug("whole organ length = " + wholeOrganLength + " microns\n", Preferences.DEBUG_ALGORITHM);
            ViewUserInterface.getReference().setDataText("whole organ length = " + nf.format(wholeOrganLength) +
                                                         " microns\n");
            dataString += "whole organ length = " + nf.format(wholeOrganLength) + " microns\n";
        } // if (wholeOrganIndex >= 0)

        fireProgressStateChanged("Cacluating average intensities");
        mask = new short[sliceSize * zDim];

        if (wholeOrganIndex >= 0) {
            organMask = new short[sliceSize * zDim];
        }

        for (i = 0; i < mask.length; i++) {
            mask[i] = -1;
        }

        if (wholeOrganIndex >= 0) {

            for (i = 0; i < mask.length; i++) {
                organMask[i] = -1;
            }
        }

        mask = bwImage.generateVOIMask(mask, photoBleachedIndex);

        if (wholeOrganIndex >= 0) {
            organMask = bwImage.generateVOIMask(organMask, wholeOrganIndex);
        }

        if (backgroundIndex >= 0) {
            mask = bwImage.generateVOIMask(mask, backgroundIndex);
        }
        // In the first slice after bleaching find the average intensities in each VOI
        // The wholeOrganVOI intensity will be greater than the photoBleachedVOI intensity
        // The backgroundVOI intensity will be less than the photoBleachedVOI intensity

        photoBleachedIntensity = new float[zDim];
        photoBleachedCount = new int[zDim];

        if (wholeOrganIndex >= 0) {
            wholeOrganIntensity = new float[zDim];
            wholeOrganCount = new int[zDim];
        }

        if (backgroundIndex >= 0) {
            backgroundIntensity = 0.0f;
            backgroundCount = 0;

            boolean haveBackground = false;
            int backgroundSlice = 0;
            Vector<VOIBase>[] sortedCurves = VOIs.VOIAt(backgroundIndex).getSortedCurves(zDim);
            for (z = 0; (z < zDim) && (!haveBackground); z++) {

                if (sortedCurves[z].size() > 0) {
                    haveBackground = true;
                    backgroundSlice = z;
                }
            }

            try {
                bwImage.exportData(backgroundSlice * sliceSize, sliceSize, floatBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e +
                                       " on bwImage.export(backgroundSlice*sliceSize,sliceSize,floatBuffer)");
                setCompleted(false);

                return;
            }

            for (i = 0; i < sliceSize; i++) {

                if (mask[i + (backgroundSlice * sliceSize)] == backgroundIndex) {
                    backgroundIntensity += floatBuffer[i];
                    backgroundCount++;
                }
            }

            backgroundConstant = backgroundIntensity / backgroundCount;
            Preferences.debug("Background constant = " + backgroundConstant + "\n", Preferences.DEBUG_ALGORITHM);
            ViewUserInterface.getReference().setDataText("background constant = " + nf.format(backgroundConstant) +
                                                         "\n");
            dataString += "background constant = " + nf.format(backgroundConstant) + "\n";
        } // if (backgroundIndex >= 0)

        for (z = 0; z < zDim; z++) {

            try {
                bwImage.exportData(z * sliceSize, sliceSize, floatBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on bwImage.export(z*sliceSize,sliceSize,floatBuffer)");
                setCompleted(false);

                return;
            }

            photoBleachedCount[z] = 0;
            photoBleachedIntensity[z] = 0.0f;

            for (i = 0; i < sliceSize; i++) {

                if (mask[(z * sliceSize) + i] == photoBleachedIndex) {
                    photoBleachedIntensity[z] += floatBuffer[i];
                    photoBleachedCount[z]++;
                }
            }

            if (wholeOrganIndex >= 0) {
                wholeOrganCount[z] = 0;
                wholeOrganIntensity[z] = 0.0f;

                for (i = 0; i < sliceSize; i++) {

                    if (organMask[(z * sliceSize) + i] == wholeOrganIndex) {
                        wholeOrganIntensity[z] += floatBuffer[i];
                        wholeOrganCount[z]++;
                    }
                }
            } // if wholeOrganIndex >= 0)
        } // for (z = 0; z < zDim; z++)

        mask = null;
        organMask = null;

        // Plot the intensity of the uncorrected photobleached region with time
        tgValues = new float[zDim];

        if (timeStamp != null) {

            for (i = 0; i < zDim; i++) {
                tgValues[i] = (float) timeStamp[i];
            }
        } // if (timeStamp != null)
        else { // timeStamp == null

            for (i = 0; i < zDim; i++) {
                tgValues[i] = (float) (i - firstSliceNum) * srcImage.getFileInfo()[0].getResolutions()[2];
            }
        } // else timeStamp == null

        ucPhotoBleachGraph = new ViewJFrameGraph(tgValues, photoBleachedIntensity,
                                                 "Uncorrected Photobleaching Recovery Curve", "Seconds",
                                                 "Fluorescence");
        ucPhotoBleachGraph.setVisible(true);
        yStart += 20;

        if (wholeOrganIndex >= 0) {
            wholeOrganGraph = new ViewJFrameGraph(tgValues, wholeOrganIntensity, "Uncorrected whole organ", "Seconds",
                                                  "Fluorescence", Color.green);
            wholeOrganGraph.setBounds(50, yStart, 500, 400);
            yStart += 20;
            wholeOrganGraph.setVisible(true);
        } // if (wholeOrganIndex >= 0)

        if (backgroundIndex >= 0) {

            // Subtract background from photbleached and whole organ VOIs
            for (z = 0; z < zDim; z++) {
                photoBleachedIntensity[z] -= backgroundConstant * photoBleachedCount[z];
            }

            if (wholeOrganIndex >= 0) {

                for (z = 0; z < zDim; z++) {
                    wholeOrganIntensity[z] -= backgroundConstant * wholeOrganCount[z];
                }
            }
        } // if (backgroundIndex >= 0)

        if (wholeOrganIndex >= 0) {

            if (firstSliceNum > 1) {
                afterBeforeRatio = wholeOrganIntensity[firstSliceNum] / wholeOrganIntensity[firstSliceNum - 1];
            } else {
                afterBeforeRatio = 1;
            }

            ViewUserInterface.getReference().setDataText("The ratio of the whole organ region fluorescence after\n");
            dataString += "The ratio of the whole organ region fluorescence after\n";
            ViewUserInterface.getReference().setDataText("bleaching to before bleaching = " +
                                                         nf.format(afterBeforeRatio) + "\n");
            dataString += "bleaching to before bleaching = " + nf.format(afterBeforeRatio) + "\n";
            Preferences.debug("The ratio of the whole organ region fluorescence after\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("bleaching to before bleaching = " + afterBeforeRatio + "\n", Preferences.DEBUG_ALGORITHM);

            // Correct the photobleached region for the total loss of fluorescence by dividing
            // by the wholeOrganIntensity
            for (z = 0; z < zDim; z++) {
                photoBleachedIntensity[z] /= wholeOrganIntensity[z];
            }
        } // if (wholeOrganIndex >= 0)

        // Even with low illumination during the recovery phase, there is expected to be
        // fluorescence loss through photobleaching over the course of the recovery curve.
        // Assume this loss is of the form exp(-lambda*time)
        /*if (wholeOrganIntensity[firstSliceNum] > wholeOrganIntensity[zDim - 1]) {
         * fireProgressStateChanged("Finding whole organ model"); // The whole organ region is fit to a function of the
         * form // a0*exp(a1*t), // where a1 < 0.
         *
         * // The exponential decay due to photobleaching does not depend on absolute times. // It only depends on the
         * bleaching during the photography of each slice. // Hence for correction of the photobleaching recovery make
         * the time // proportional to the slice number trValues = new double[zDim - firstSliceNum]; woIntensity = new
         * float[zDim - firstSliceNum]; for (i = 0; i < zDim - firstSliceNum; i++) { trValues[i] = (double)i;
         * woIntensity[i] = wholeOrganIntensity[i + firstSliceNum]; }
         *
         * initial = new double[2]; initial[0] = wholeOrganIntensity[firstSliceNum]; initial[1] =
         * Math.log(wholeOrganIntensity[zDim - 1]/ wholeOrganIntensity[0])/(zDim - firstSliceNum - 1);
         * Preferences.debug("Whole organ initial[0] = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM); 
         * Preferences.debug("Whole organ initial[1] = " + initial[1] + "\n", Preferences.DEBUG_ALGORITHM);
         *
         *
         * // Multiply the photobleached data by exp(-params[1]*t) to compensate for // the photobleaching loss over the
         * recovery curve
         *
         * if (params[1] < 0.0) { for (z = firstSliceNum; z < zDim; z++) { photoBleachedIntensity[z] =
         * (float)(photoBleachedIntensity[z] * Math.exp(-params[1]*(z-firstSliceNum))); } }
         *
         * } // if (refIntensity[0] > refIntensity[zDim - 1]) else { UI.setDataText("Whole organ VOI exponential
         * correction could not be used\n"); UI.setDataText("Did not find decrease in whole organ region intensity\n");
         * Preferences.debug("Whole organ VOI exponential correction could not be used\n", Preferences.DEBUG_ALGORITHM);
         *  Preferences.debug("Did not find decrease in whole organ region intensity\n", Preferences.DEBUG_ALGORITHM); }*/

        // If whole organ normalization is used, divide photobleached intensity
        // values by the photobleached VOI intensity just before bleaching so
        // that pIntensity goes from some positive bottom value up to an asymptote
        // of mf. If whole organ normalization is not used, normalize the
        // photobleached intensity by the maximum of the postbleaching values.
        pIntensity = new float[zDim - firstSliceNum];

        if (wholeOrganIndex >= 0) {
            pNorm = photoBleachedIntensity[firstSliceNum - 1];

            for (z = 0; z < zDim; z++) {
                photoBleachedIntensity[z] = photoBleachedIntensity[z] / pNorm;
            }
        } // if (wholeOrganIndex >= 0)
        else {
            pMax = -Float.MAX_VALUE;

            for (z = firstSliceNum; z < zDim; z++) {

                if (photoBleachedIntensity[z] > pMax) {
                    pMax = photoBleachedIntensity[z];
                }
            }

            if (model != CIRCLE_2D) {

                for (z = 0; z < zDim; z++) {
                    photoBleachedIntensity[z] = photoBleachedIntensity[z] / pMax;
                }
            } // if (model != CIRCLE_2D)
        }

        for (z = firstSliceNum; z < zDim; z++) {
            pIntensity[z - firstSliceNum] = photoBleachedIntensity[z];
        }

        if (model == NARROW_BAND_2D) {
            fireProgressStateChanged("Finding double exponential model");
        } else if (model == CIRCLE_2D) {
            fireProgressStateChanged("Finiding full reaction-diffusion model");
        } else if (model == PURE_1D) {
            fireProgressStateChanged("Finding 1D pure diffusion model");
        } else {
            fireProgressStateChanged("Finding single exponential model");
        }

        fireProgressStateChanged(50);

        if (model == NARROW_BAND_2D) {
            // Determine the constraints on alpha beta and gamma.
            // 1 >= gamma >= 0
            // beta < alpha < 0
            // At time ta, we have a recovered fraction fa given by:
            // pIntensity[0] + span*[1 - gamma*exp(alpha*ta) - (1 - gamma)*exp(beta*ta)] = fa
            // span equals about pIntensity[pIntensity.length-1] - pIntensity[0]
            // and the mobile fraction equals pIntensity[0] + span
            // If we had no beta and only alpha, then the recovery would
            // be slower so that
            // 1 - exp(alpha*ta) <= (fa - pIntensity[0])/span
            // exp(alpha*ta) >= 1 - (fa - pIntensity[0])/span
            // alpha >= ln(1 - (fa-pIntensity[0])/span)/ta
            // Noise may be present in the data, so take the alpha minimum as the
            // as the minimum of all the ln(1 - (fa-pIntensity[0])/span)/ta values.
            // 0 > alpha >= alphaMinimum

            if (wholeOrganIndex >= 0) {
                initial = new double[5];
                initial[0] = 0.5; // gamma

                // If only a single exponential were present, the exponential constant
                // would = ln(0.5)/(time to recover halfway)
                // Note that beta < alpha since alpha and beta are negative.
                haveHalf = false;
                pMin = Float.MAX_VALUE;
                pMax = -Float.MAX_VALUE;

                for (i = 0; i < pIntensity.length; i++) {

                    if (pIntensity[i] > pMax) {
                        pMax = pIntensity[i];
                    }

                    if (pIntensity[i] < pMin) {
                        pMin = pIntensity[i];
                    }
                }

                pHalf = (pMin + pMax) / 2.0f;

                for (i = 0; (i < pIntensity.length) && (!haveHalf); i++) {

                    if (pIntensity[i] >= pHalf) {
                        haveHalf = true;
                        initial[1] = 0.5 * Math.log(0.5) / (tValues[i]); // alpha
                        initial[2] = 4.0 * initial[1]; // beta
                    }
                }

                initial[3] = pMin; // bottom guess
                initial[4] = pMax - pMin; // span guess
                Preferences.debug("gamma guess initial[0] = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("alpha guess initial[1] = " + initial[1] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("beta guess initial[2] = " + initial[2] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom guess initial[3] = " + initial[3] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("span guess initial[4] = " + initial[4] + "\n", Preferences.DEBUG_ALGORITHM);

                fdem = new FitDoubleExponentialModel(zDim - firstSliceNum, tValues, pIntensity, initial);
                fdem.driver();
                fdem.dumpResults();

                /*chiSquared = fdem.getChiSquared();
                 * if (Double.isNaN(chiSquared)) { MipavUtil.displayError( "Fit double exponential failed - Chi-squared
                 * was not a valid number\n"); if  {  } setCompleted(false);
                 *
                 * return; }*/
                params = fdem.getParameters();
                bottom = params[3];
                span = params[4];
            } // if (wholeOrganOrganIndex >= 0)
            else { // no whole organ
                initial = new double[4];
                initial[0] = 0.5; // gamma

                // If only a single exponential were present, the exponential constant
                // would = ln(0.5)/(time to recover halfway)
                // Note that beta < alpha since alpha and beta are negative.
                haveHalf = false;
                pMin = Float.MAX_VALUE;
                pMax = -Float.MAX_VALUE;

                for (i = 0; i < pIntensity.length; i++) {

                    if (pIntensity[i] > pMax) {
                        pMax = pIntensity[i];
                    }

                    if (pIntensity[i] < pMin) {
                        pMin = pIntensity[i];
                    }
                }

                pHalf = (pMin + pMax) / 2.0f;

                for (i = 0; (i < pIntensity.length) && (!haveHalf); i++) {

                    if (pIntensity[i] >= pHalf) {
                        haveHalf = true;
                        initial[1] = 0.5 * Math.log(0.5) / (tValues[i]); // alpha
                        initial[2] = 4.0 * initial[1]; // beta
                    }
                }

                initial[3] = pMin; // bottom guess
                Preferences.debug("gamma guess initial[0] = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("alpha guess initial[1] = " + initial[1] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("beta guess initial[2] = " + initial[2] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom guess initial[3] = " + initial[3] + "\n", Preferences.DEBUG_ALGORITHM);

                fdemnw = new FitDoubleExponentialNoWholeModel(zDim - firstSliceNum, tValues, pIntensity, initial);
                fdemnw.driver();
                fdemnw.dumpResults();

                /*chiSquared = fdemnw.getChiSquared();
                 * if (Double.isNaN(chiSquared)) { MipavUtil.displayError( "Fit double exponential failed - Chi-squared
                 * was not a valid number\n"); if  {  } setCompleted(false);
                 *
                 * return; }*/
                params = fdemnw.getParameters();
                bottom = params[3];
                span = 1.0 - params[3];
            } // else no whole organ

            // The nonlinear fitting routine might interchange alpha and beta
            // If so, change back
            if (params[2] > params[1]) {
                dTemp = params[1];
                params[1] = params[2];
                params[2] = dTemp;
                params[0] = 1.0 - params[0];
            }

            ViewUserInterface.getReference().setDataText("In the recovery curve\n" +
                                                         "bottom + span*[1 - gamma*exp(alpha*t) - (1 - gamma)*exp(beta*t)]\n");
            dataString += "In the recovery curve\n" +
                          "bottom + span*[1 - gamma*exp(alpha*t) - (1 - gamma)*exp(beta*t)]\n";
            ViewUserInterface.getReference().setDataText("bottom = " + nf.format(bottom) + " span = " +
                                                         nf.format(span) + "\n" + "alpha = " + nf.format(params[1]) +
                                                         " beta = " + nf.format(params[2]) + " gamma = " +
                                                         nf.format(params[0]) + "\n");
            dataString += "bottom = " + nf.format(bottom) + " span = " + nf.format(span) + "\n" + "alpha = " +
                          nf.format(params[1]) + " beta = " + nf.format(params[2]) + " gamma = " +
                          nf.format(params[0]) + "\n";
            Preferences.debug("In the recovery curve\n" +
                              "bottom + span*[1 - gamma*exp(alpha*t) - (1 - gamma)*exp(beta*t)]\n",
                              Preferences.DEBUG_ALGORITHM);
            Preferences.debug("bottom = " + bottom + " span = " + span + "\n" + "alpha = " + params[1] + " beta = " +
                              params[2] + " gamma = " + params[0] + "\n", Preferences.DEBUG_ALGORITHM);

            mf = (float) (bottom + span);

            if (mf > 1.0f) {
                mf = 1.0f;
            } else if (mf < 0.0f) {
                mf = 0.0f;
            }

            ViewUserInterface.getReference().setDataText("Mobile fraction = " + nf.format(mf) + "\n");
            dataString += "Mobile fraction = " + nf.format(mf) + "\n";
            Preferences.debug("Mobile fraction = " + mf + "\n", Preferences.DEBUG_ALGORITHM);

            // From equation (20) s1 = -(alpha + beta)/2, s2 = (alpha - beta)/2
            s1 = -(params[1] + params[2]) / 2.0;
            s2 = (params[1] - params[2]) / 2.0;

            // From equation (21)
            // s2 = sqrt(s1**2 - 2kdD/(2 - Fa/F0))
            // kdD = 0.5*(2 - Fa/F0)*(s1**2 - s2**2)
            kdD = 0.5 * (2.0 - afterBeforeRatio) * ((s1 * s1) - (s2 * s2));

            // s1 = (ka + kd)/2 + kdD/(kd*(2 - Fa/F0))
            // ka + kd = 2*s1 - 2kdD/(kd*(2 - Fa/F0))
            // ka = 2*s1 - kd - 2kdD/(kd*(2 - Fa/F0))
            // gamma1 = 1 + (s1 - s2)/kd + ka/kd + 2kdD/(kd*kd*(2 - Fa/F0))
            // = 1 + (s1 - s2)/kd + 2*s1/kd - 1 - 2kdD/(kd*kd*(2 - Fa/F0))
            // + 2kdD/(kd*kd*(2 - Fa/F0))
            // = (3*s1 - s2)/kd
            // gamma = 0.5*(kd/(ka + kd))*gamma1*gamma2
            // = 0.5*((3*s1 - s2)/(ka + kd))*gamma2
            // gamma*(2*s1 - 2kdD/(kd*(2 - Fa/F0))) = 0.5*(3*s1 - s2)*gamma2
            // gamma*(2*s1*kd*(2 - Fa/F0) - 2kdD) = 0.5*(3*s1 - s2)*kd*(2 - Fa/F0)*gamma2
            // = 0.5*(3*s1 - s2)*((1 + s1/s2)*kd*(2 - Fa/F0) - 2kdD/s2)
            // gamma*(2*s1*kd*(2 - Fa/F0) - 2kdD)*s2 =
            // 0.5*(3*s1 - s2)*((s2 + s1)*(2 - Fa/F0)*kd - 2kdD)
            // kd(gamma*2*s1*(2 - Fa/F0)*s2 - 0.5*(3*s1 - s2)*(s2 + s1)*(2 - Fa/F0)) =
            // gamma*2*kdD*s2 - (3*s1 - s2)*kdD
            num = kdD * ((2.0 * params[0] * s2) - (3.0 * s1) + s2);
            denom = (2.0 - afterBeforeRatio) * ((2.0 * params[0] * s1 * s2) - (0.5 * ((3.0 * s1) - s2) * (s1 + s2)));
            kd = num / denom;
            Dt = kdD / kd;
            ka = (2.0 * s1) - kd - (2.0 * kdD / (kd * (2.0 - afterBeforeRatio)));
            ViewUserInterface.getReference().setDataText("Association rate = " + nf.format(ka) + "\n");
            dataString += "Association rate = " + nf.format(ka) + "\n";
            ViewUserInterface.getReference().setDataText("Dissociation rate = " + nf.format(kd) + "\n");
            dataString += "Dissociation rate = " + nf.format(kd) + "\n";
            ViewUserInterface.getReference().setDataText("Diffusion transfer coefficient = " + nf.format(Dt) + "\n");
            dataString += "Diffusion transfer coefficient = " + nf.format(Dt) + "\n";
            Preferences.debug("Association rate = " + ka + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Dissociation rate = " + kd + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Diffusion transfer coefficient = " + Dt + "\n", Preferences.DEBUG_ALGORITHM);
            Deff = Dt * photoBleachedWidth * (wholeOrganLength - photoBleachedWidth) / 4.0;
            ViewUserInterface.getReference().setDataText("Effective diffusion constant = " + nf.format(Deff) +
                                                         " um**2/sec\n\n");
            dataString += "Effective diffusion constant = " + nf.format(Deff) + " um**2/sec\n\n";
            Preferences.debug("Effective diffusion constant = " + Deff + " um**2/sec\n\n", Preferences.DEBUG_ALGORITHM);

            /*s1 = ((ka + kd)*(2 - afterBeforeRatio) + 2*Dt)/(2*(2 - afterBeforeRatio));
             * s2 = Math.sqrt(s1*s1 - 2*kd*Dt/(2 - afterBeforeRatio)); double alpha = -s1 + s2; double beta =  -s1 - s2;
             * double gamma1 = 1 + ((2 - afterBeforeRatio)*(s1 - s2) + (2 - afterBeforeRatio)*ka + 2*Dt)/(kd * (2 -
             * afterBeforeRatio)); double gamma2 = ((2 - afterBeforeRatio)*(s1 + s2) - 2*Dt)/ ((2 -
             * afterBeforeRatio)*s2); double gamma = 0.5*kd*gamma1*gamma2/(ka + kd); UI.setDataText("alpha doublecheck =
             * " + alpha + "\n"); UI.setDataText("beta doublecheck = " + beta + "\n"); UI.setDataText("gamma doublecheck
             * = " + gamma + "\n");*/

            // Plot the intensity of the photobleached region with time
            tfValues = new float[2][zDim];
            pfValues = new float[2][zDim];

            if (timeStamp != null) {

                for (i = 0; i < zDim; i++) {
                    tfValues[0][i] = (float) timeStamp[i];
                    tfValues[1][i] = tfValues[0][i];
                }
            } // if (timeStamp != null)
            else { // timeStamp == null

                for (i = 0; i < zDim; i++) {
                    tfValues[0][i] = (float) (i - firstSliceNum) * srcImage.getFileInfo()[0].getResolutions()[2];
                    tfValues[1][i] = tfValues[0][i];
                }
            } // else timeStamp == null

            for (z = 0; z < photoBleachedIntensity.length; z++) {
                pfValues[0][z] = photoBleachedIntensity[z];
            }

            for (z = 0; z < firstSliceNum; z++) {
                pfValues[1][z] = 0.0f;
            }

            for (z = firstSliceNum; z < zDim; z++) {
                pfValues[1][z] = (float) (bottom +
                                          (span *
                                               (1 - (params[0] * Math.exp(params[1] * tValues[z - firstSliceNum])) -
                                                    ((1 - params[0]) *
                                                         Math.exp(params[2] * tValues[z - firstSliceNum])))));
            }

            if (paramVary) {

                // Find the sum of square of errors for variations in alpha, beta, and
                // gamma
                double alpha, beta, gamma;
                int nPts = zDim - firstSliceNum;
                double pFit;
                double dy;
                double sse;
                double ssemin;
                double ssemax;
                int x, y;
                float[] buffer = new float[201 * 201 * 101];
                int xydim = 201 * 201;
                int indexZ;
                int indexY;
                int index;
                int[] errExtents = new int[3];
                int xmin = 100;
                int ymin = 100;
                int zmin = (int) Math.round(100 * params[0]);
                int indexmin = (zmin * xydim) + (ymin * 201) + xmin;
                int indexOriginal = indexmin;
                int k;
                boolean localFound;
                errExtents[0] = 201;
                errExtents[1] = 201;
                errExtents[2] = 101;

                int localMinNumber = 0;
                int[] localMinIndex = new int[100];
                double alphaScale;
                double betaScale;
                int xysep;
                short voiID;
                VOI globalPtVOI;
                VOI[] localPtVOI = null;
                float[] xPt = new float[1];
                float[] yPt = new float[1];
                float[] zPt = new float[1];

                fireProgressStateChanged("Calculating sse array for param variations");

                // Require beta <= alpha <= 0
                // 0.05*params[2]*Math.pow(400.0,0.005*y) <=
                // 0.05*params[1]*Math.pow(400.0,0.005*x)
                // Since division is by a negative number, params[1], the less than sign
                // must be flipped to a greater than sign
                // params[2]/params[1] >= Math.pow(400.0,0.005*(x-y))
                // Math.log(params[2]/params[1]) >= 0.005*(x-y)*Math.log(400)
                // x - y <= 200*Math.log(params[2]/params[1])/Math.log(400)
                xysep = (int) Math.floor(200.0 * Math.log(params[2] / params[1]) / Math.log(400));
                ssemin = Double.MAX_VALUE;
                ssemax = -Double.MAX_VALUE;

                for (z = 0; z <= 100; z++) {
                    fireProgressStateChanged(z);
                    indexZ = xydim * z;
                    gamma = 0.01 * z;

                    for (y = 0; y <= 200; y++) {
                        indexY = indexZ + (201 * y);
                        beta = 0.05 * params[2] * Math.pow(400.0, 0.005 * y);

                        for (x = 0; x <= 200; x++) {
                            index = indexY + x;

                            if (x > (y + xysep)) {
                                buffer[index] = -1;
                            } else { // x <= (y + xysep)
                                alpha = 0.05 * params[1] * Math.pow(400.0, 0.005 * x);
                                sse = 0.0;

                                for (i = 0; i < nPts; i++) {
                                    pFit = bottom +
                                           (span *
                                                (1.0 - (gamma * Math.exp(alpha * tValues[i])) -
                                                     ((1.0 - gamma) * Math.exp(beta * tValues[i]))));
                                    dy = pFit - pIntensity[i];
                                    sse += dy * dy;
                                } // for (i = 0; i < nPts; i++)

                                buffer[index] = (float) sse;

                                if (sse < ssemin) {
                                    ssemin = sse;
                                    xmin = x;
                                    ymin = y;
                                    zmin = z;
                                    indexmin = index;
                                } // if (sse < ssemin)

                                if (sse > ssemax) {
                                    ssemax = sse;
                                }
                            } // else for x <= (y + xysep)
                        } // for (x = 0; x <= 200; x++)
                    } // for (y = 0; y <= 200; y++)
                } // for (z = 0; z <= 100; z++)

                // Replace -1 for impossible alpha < beta values with 1.1*ssemax
                // so as not to make the data range too much greater
                for (i = 0; i < buffer.length; i++) {

                    if (buffer[i] == -1) {
                        buffer[i] = (float) (1.1 * ssemax);
                    }
                }

                fireProgressStateChanged("Checking for local minima");

                for (z = 2; z <= 98; z++) {
                    fireProgressStateChanged(z + 2);
                    indexZ = xydim * z;

                    for (y = 2; y <= 198; y++) {
                        indexY = indexZ + (201 * y);

                        for (x = 2; x <= 198; x++) {
                            index = indexY + x;

                            if (index != indexmin) {
                                localFound = true;

                                for (k = index - (2 * xydim); localFound && (k <= (index + (2 * xydim))); k += xydim) {

                                    for (j = k - (2 * 201); localFound && (j <= (k + (2 * 201))); j += 201) {

                                        for (i = j - 2; localFound && (i <= (j + 2)); i++) {

                                            if (i != index) {

                                                if (buffer[index] >= buffer[i]) {
                                                    localFound = false;
                                                }
                                            } // if (i != index)
                                        } // for (i = j-2; localFound && (i <= j+2); i++)
                                    } // for (j = k-2*201; localFound &&
                                    // (j <= k+2*201); j+= 201)
                                } // for (k = index-2*xydim; localFound &&

                                // (k <= index+2*xydim); k+= xydim)
                                if (localFound) {
                                    localMinNumber++;

                                    if (localMinNumber <= 100) {
                                        localMinIndex[localMinNumber - 1] = index;
                                    }
                                } // if (localFound)
                            } // if (index != indexmin)
                        } // for (x = 2; x <= 198; x++)
                    } // for (y = 2; y <= 198; y++)
                } // for (z = 2; z <= 98; z++)

                Preferences.debug("alpha and beta are incremented in multiplicative factors of " +
                                  Math.pow(400.0, 0.005) + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("from 1/20 * original alpha and beta to 20 * original alpha and beta\n", 
                		Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("alpha and beta are incremented in multiplicative factors of " +
                                                             nf.format(Math.pow(400.0, 0.005)) + "\n");
                dataString += "alpha and beta are incremented in multiplicative factors of " +
                              nf.format(Math.pow(400.0, 0.005)) + "\n";
                ViewUserInterface.getReference().setDataText("from 1/20 * original alpha and beta to 20 * original alpha and beta\n");
                dataString += "from 1/20 * original alpha and beta to 20 * original alpha and beta\n";
                Preferences.debug("gamma is additively incremented by 0.01 from 0 to 1\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("gamma is additively incremented by 0.01 from 0 to 1\n");
                dataString += "gamma is additively incremented by 0.01 from 0 to 1\n";

                if (indexmin != indexOriginal) {
                    Preferences.debug("sse original parameters = " + buffer[indexOriginal] + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("sse original parameters = " + buffer[indexOriginal] +
                                                                 "\n");
                    dataString += "sse original parameters = " + buffer[indexOriginal] + "\n";
                } // if (indexmin != indexOriginal)

                Preferences.debug("sse global min = " + ssemin + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("sse global min = " + ssemin + "\n");
                dataString += "sse global min = " + ssemin + "\n";
                alphaScale = 0.05 * Math.pow(400.0, 0.005 * xmin);
                Preferences.debug("x = " + xmin + " or new alpha = " + alphaScale + " * original alpha = " +
                                  (alphaScale * params[1]) + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("x = " + xmin + " or new alpha = " +
                                                             nf.format(alphaScale) + " * original alpha = " +
                                                             nf.format(alphaScale * params[1]) + "\n");
                dataString += "x = " + xmin + " or new alpha = " + nf.format(alphaScale) + " * original alpha = " +
                              nf.format(alphaScale * params[1]) + "\n";
                betaScale = 0.05 * Math.pow(400.0, 0.005 * ymin);
                Preferences.debug("y = " + ymin + " or new beta = " + betaScale + " * original beta = " +
                                  (betaScale * params[2]) + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("y = " + ymin + " or new beta = " + nf.format(betaScale) +
                                                             " * original beta = " + nf.format(betaScale * params[2]) +
                                                             "\n");
                dataString += "y = " + ymin + " or new beta = " + nf.format(betaScale) + " * original beta = " +
                              nf.format(betaScale * params[2]) + "\n";
                Preferences.debug("slice = " + (zmin + 1) + " or new gamma = " + nf.format(0.01 * zmin) +
                                  " while original gamma = " + params[0] + "\n\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("slice = " + (zmin + 1) + " or new gamma = " +
                                                             nf.format(0.01 * zmin) + " while original gamma = " +
                                                             nf.format(params[0]) + "\n\n");
                dataString += "slice = " + (zmin + 1) + " or new gamma = " + nf.format(0.01 * zmin) +
                              " while original gamma = " + nf.format(params[0]) + "\n\n";

                if (localMinNumber >= 1) {
                    Preferences.debug("In addition to the global minimum\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("In addition to the global minimum\n");
                    dataString += "In addition to the global minimum\n";

                    if (localMinNumber == 1) {
                        Preferences.debug("1 local minimum was located at:\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText("1 local minimum was located at:\n");
                        dataString += "1 local minimum was located at:\n";
                    } else {
                        Preferences.debug(localMinNumber + " local minima were located at:\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText(localMinNumber +
                                                                     " local minima were located at:\n");
                        dataString += localMinNumber + " local minima were located at:\n";
                    }

                    for (i = 0; i < localMinNumber; i++) {
                        index = localMinIndex[i];
                        z = index / xydim;
                        y = (index % xydim) / 201;
                        x = (index % xydim) % 201;
                        Preferences.debug("sse local min  = " + buffer[index] + "\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText("sse local min min = " + buffer[index] + "\n");
                        dataString += "sse local min min = " + buffer[index] + "\n";
                        alphaScale = 0.05 * Math.pow(400.0, 0.005 * x);
                        Preferences.debug("x = " + x + " or new alpha = " + alphaScale + " * original alpha = " +
                                          (alphaScale * params[1]) + "\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText("x = " + x + " or new alpha = " +
                                                                     nf.format(alphaScale) + " * original alpha = " +
                                                                     nf.format(alphaScale * params[1]) + "\n");
                        dataString += "x = " + x + " or new alpha = " + nf.format(alphaScale) + " * original alpha = " +
                                      nf.format(alphaScale * params[1]) + "\n";
                        betaScale = 0.05 * Math.pow(400.0, 0.005 * y);
                        Preferences.debug("y = " + y + " or new beta = " + betaScale + " * original beta = " +
                                          (betaScale * params[2]) + "\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText("y = " + y + " or new beta = " +
                                                                     nf.format(betaScale) + " * original beta = " +
                                                                     nf.format(betaScale * params[2]) + "\n");
                        dataString += "y = " + y + " or new beta = " + nf.format(betaScale) + " * original beta = " +
                                      nf.format(betaScale * params[2]) + "\n";
                        Preferences.debug("slice = " + (z + 1) + " or new gamma = " + (0.01 * z) +
                                          " while original gamma = " + params[0] + "\n\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText("slice = " + (z + 1) + " or new gamma = " +
                                                                     (0.01 * z) + " while original gamma = " +
                                                                     nf.format(params[0]) + "\n\n");
                        dataString += "slice = " + (z + 1) + " or new gamma = " + (0.01 * z) +
                                      " while original gamma = " + nf.format(params[0]) + "\n\n";
                    } // for (i = 0; i < localMinNumber; i++)
                } // if (localMinNumber >= 1)

                errorImage = new ModelImage(ModelStorageBase.FLOAT, errExtents, srcImage.getImageName() + "_err");

                try {
                    errorImage.importData(0, buffer, true);
                } catch (IOException err) {
                    MipavUtil.displayError("IOException " + err + " on errorImage.importData");
                    setCompleted(false);

                    return;
                }

                try {
                    errorFrame = new ViewJFrameImage(errorImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                    setCompleted(false);

                    return;
                }

                try {
                    voiID = 0;
                    globalPtVOI = new VOI(voiID, "pointGlobalMin.voi", VOI.POINT, -1.0f);
                    xPt[0] = xmin;
                    yPt[0] = ymin;
                    zPt[0] = zmin;
                    globalPtVOI.importCurve(xPt, yPt, zPt);

                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory on globalPtVOI creation");
                    setCompleted(false);

                    return;
                }

                errorImage.registerVOI(globalPtVOI);
                globalPtVOI.setActive(true);
                ((VOIPoint) (globalPtVOI.getCurves().elementAt(0))).setActive(true);

                if (localMinNumber > 0) {
                    localPtVOI = new VOI[localMinNumber];
                }

                for (i = 0; i < localMinNumber; i++) {

                    try {
                        voiID = (short) (i + 1);
                        localPtVOI[i] = new VOI(voiID, "pointLocalMin" + String.valueOf(i + 1) + ".voi",
                                                VOI.POINT, -1.0f);
                        index = localMinIndex[i];
                        zPt[0] = index / xydim;
                        zmin = index / xydim;
                        yPt[0] = (index % xydim) / 201;
                        xPt[0] = (index % xydim) % 201;
                        localPtVOI[i].importCurve(xPt, yPt, zPt);
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory on localPtVOI" + (i + 1) + " creation");
                        setCompleted(false);

                        return;
                    }

                    errorImage.registerVOI(localPtVOI[i]);
                    localPtVOI[i].setActive(true);
                    ((VOIPoint) (localPtVOI[i].getCurves().elementAt(0))).setActive(true);
                } // for (i = 0; i < localMinNumber; i++)

                errorFrame.updateImages();
                errorImage.getFileInfo()[0].setFileDirectory(srcImage.getFileInfo(0).getFileDirectory());
                errorFrame.saveAllVOIs();
                ViewUserInterface.getReference().getMessageFrame().getData().setText(dataString);
            } // if (paramVary)
        } // if (model == NARROW_BAND_2D)
        else if (model == CIRCLE_2D) {
            double[] kons = new double[] { -5.0, -2.5, 0.0, 2.5, 5.0 };
            double[] koffs = new double[] { -5.0, -2.5, 0.0, 2.5, 5.0 };
            double[] initial_kon = new double[26];
            double[] initial_koff = new double[26];
            int count = 0;
            int k;

            for (i = 0; i < 5; i++) {

                for (j = 0; j < 5; j++) {
                    initial_kon[count] = Math.pow(10.0, kons[i]);
                    initial_koff[count++] = Math.pow(10.0, koffs[j]);
                } // for (j = 0; j < 5; j++)
            } // for (i = 0; i < 5; i++)

            initial_kon[25] = 1.0E7;
            initial_koff[25] = 1.0E-7;

            // An estimate for the maximum of the real parts of the singularities
            // of F. If unknown, set largestPole = 0.0
            double largestPole = 0.0;

            // numerical tolerance of approaching pole (default = 1.0e-9)
            double tol = 1.0e-9;
            FitFullModel lmod;
            FitWholeNL2solModel nonlinmod;
            FitWholeNLConModel nlinmod2;
            double[] timeFunction = null;
            double[] fitR = new double[pIntensity.length - 1];
            double[] sses = new double[26];
            double minsses = Double.MAX_VALUE;
            int kmin = 0;
            initial = new double[2];

            double[] residuals;

            // Must strip out value with time = 0 or inverseLaplace will blow up
            // when taking logarithm of zero.
            double[] tStripZero = new double[tValues.length - 1];
            float[] pStripZero = new float[tValues.length - 1];

            for (i = 0; i < (tValues.length - 1); i++) {
                tStripZero[i] = tValues[i + 1];
                pStripZero[i] = pIntensity[i + 1];
            }

            for (k = 0; k < 26; k++) {
                fireProgressStateChanged("Performing " + (k + 1) + " of 26 grid search runs");
                fireProgressStateChanged(50 + k);
                lmod = new FitFullModel(tStripZero, largestPole, tol, initial_kon[k], initial_koff[k]);
                lmod.driver();
                timeFunction = lmod.getTimeFunction();

                for (i = 0; i < timeFunction.length; i++) {
                    fitR[i] = timeFunction[i] - pStripZero[i];
                    sses[k] = sses[k] + (fitR[i] * fitR[i]);
                } // for (i = 0; i < timeFunction.length; i++)

                if (sses[k] < minsses) {
                    minsses = sses[k];
                    kmin = k;
                }
            } // for (k = 0; k < 26; k++)

            initial[0] = initial_kon[kmin];
            initial[1] = initial_koff[kmin];
            Preferences.debug("Best of 26 trials yields initial guesses of:\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Best initial kon guess = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Best initial koff guess = " + initial[1] + "\n", Preferences.DEBUG_ALGORITHM);

            fireProgressStateChanged("Performing NL2sol nonlinear fit");
            double xp[] = new double[3];
            xp[1] = initial[0];
            xp[2] = initial[1];
            int iv[] = new int[63]; // 61 + number of coefficients
            int vLength = 94 + tStripZero.length*2 + 3*tStripZero.length + 2*(3*2+33)/2;
        	double v[] = new double[vLength];
        	boolean useAnalyticJacobian = false;
            nonlinmod = new FitWholeNL2solModel(tStripZero.length, tStripZero, pStripZero, xp,
            		    iv, v, useAnalyticJacobian, largestPole, tol);
            nonlinmod.driver();
            nonlinmod.dumpResults();
            ViewUserInterface.getReference().setDataText("NL2sol nonlinear fit\n");
            ViewUserInterface.getReference().setDataText("kon = " + xp[1] + "\n");
            ViewUserInterface.getReference().setDataText("koff = " + xp[2] + "\n");
            dataString += "NL2sol nonlinear fit\n";
            dataString += "kon = " + xp[1] + "\n";
            dataString += "koff = " + xp[2] + "\n";
            Preferences.debug("NL2sol nonlinear fit\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("kon = " + xp[1] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("koff = " + xp[2] + "\n", Preferences.DEBUG_ALGORITHM);

            // Plot the intensity of the photobleached region with time
            tfValues = new float[2][zDim];
            pfValues = new float[2][zDim];

            if (timeStamp != null) {

                for (i = 0; i < zDim; i++) {
                    tfValues[0][i] = (float) timeStamp[i];
                    tfValues[1][i] = tfValues[0][i];
                }
            } // if (timeStamp != null)
            else { // timeStamp == null

                for (i = 0; i < zDim; i++) {
                    tfValues[0][i] = (float) (i - firstSliceNum) * srcImage.getFileInfo()[0].getResolutions()[2];
                    tfValues[1][i] = tfValues[0][i];
                }
            } // else timeStamp == null

            for (z = 0; z < photoBleachedIntensity.length; z++) {
                pfValues[0][z] = photoBleachedIntensity[z];
            }

            for (z = 0; z < (firstSliceNum + 1); z++) {
                pfValues[1][z] = 0.0f;
            }

            for (z = firstSliceNum + 1; z < zDim; z++) {
            	// Residuals start at v of iv[50]
                pfValues[1][z] = (float) (pStripZero[z - (firstSliceNum + 1)] + v[iv[50] + z - (firstSliceNum + 1)]);
            }

            fireProgressStateChanged("Performing ELSUNC nonlinear fit");
            doSecondFit = true;
            nlinmod2 = new FitWholeNLConModel(tStripZero.length, tStripZero, pStripZero, initial, largestPole, tol);
            nlinmod2.driver();
            nlinmod2.dumpResults();
            residuals = nlinmod2.getResiduals();
            params = nlinmod2.getParameters();
            ViewUserInterface.getReference().setDataText("ELSUNC nonlinear fit\n");
            ViewUserInterface.getReference().setDataText("kon = " + params[0] + "\n");
            ViewUserInterface.getReference().setDataText("koff = " + params[1] + "\n");
            dataString += "ELSUNC nonlinear fit\n";
            dataString += "kon = " + params[0] + "\n";
            dataString += "koff = " + params[1] + "\n";
            Preferences.debug("ELSUNC nonlinear fit\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("kon = " + params[0] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("koff = " + params[1] + "\n", Preferences.DEBUG_ALGORITHM);

            pfValues2 = new float[2][zDim];

            for (z = 0; z < photoBleachedIntensity.length; z++) {
                pfValues2[0][z] = photoBleachedIntensity[z];
            }

            for (z = 0; z < (firstSliceNum + 1); z++) {
                pfValues2[1][z] = 0.0f;
            }

            for (z = firstSliceNum + 1; z < zDim; z++) {
                pfValues2[1][z] = (float) (pStripZero[z - (firstSliceNum + 1)] + residuals[z - (firstSliceNum + 1)]);
            }

            if (paramVary) {

                // Find the sum of square of errors for variations in kon and koff
                double ssemin = Double.MAX_VALUE;
                double ssemax = -Double.MAX_VALUE;
                sses = new double[201 * 201];

                int y;
                int x;
                int indexY;
                int index;
                double kon;
                double koff;
                int xmin = 100;
                ;

                int ymin = 100;
                int indexmin = 0;
                boolean localFound;
                int localMinNumber = 0;
                int[] localMinIndex = new int[100];
                indexmin = (ymin * 201) + xmin;

                int indexOriginal = indexmin;
                double konScale;
                double koffScale;
                int[] errExtents = new int[2];
                errExtents[0] = 201;
                errExtents[1] = 201;

                short voiID;
                VOI globalPtVOI;
                VOI[] localPtVOI = null;
                float[] xPt = new float[1];
                float[] yPt = new float[1];
                float[] zPt = new float[1];

                fireProgressStateChanged("Calculating sse array for kon and koff variations");

                for (y = 0; y <= 200; y++) {
                    fireProgressStateChanged(y / 2);
                    indexY = 201 * y;
                    koff = 0.02 * params[1] * Math.pow(2500.0, 0.005 * y);

                    for (x = 0; x <= 200; x++) {
                        index = indexY + x;
                        kon = 0.02 * params[0] * Math.pow(2500.0, 0.005 * x);
                        lmod = new FitFullModel(tStripZero, largestPole, tol, kon, koff);
                        lmod.driver();
                        timeFunction = lmod.getTimeFunction();

                        for (i = 0; i < timeFunction.length; i++) {
                            fitR[i] = timeFunction[i] - pStripZero[i];
                            sses[index] = sses[index] + (fitR[i] * fitR[i]);
                        } // for (i = 0; i < timeFunction.length; i++)

                        if (sses[index] < ssemin) {
                            ssemin = sses[index];
                            xmin = x;
                            ymin = y;
                            indexmin = index;
                        } // if (sse < ssemin)

                        if (sses[index] > ssemax) {
                            ssemax = sses[index];
                        }
                    } // for (x = 0; x <= 200; x++)
                } // for (y = 0; y <= 200; y++)

                fireProgressStateChanged("Checking for local minima");

                for (y = 2; y <= 198; y++) {
                    fireProgressStateChanged(y / 2);
                    indexY = 201 * y;

                    for (x = 2; x <= 198; x++) {
                        index = indexY + x;

                        if (index != indexmin) {
                            localFound = true;

                            for (j = index - (2 * 201); localFound && (j <= (index + (2 * 201))); j += 201) {

                                for (i = j - 2; localFound && (i <= (j + 2)); i++) {

                                    if (i != index) {

                                        if (sses[index] >= sses[i]) {
                                            localFound = false;
                                        }
                                    } // if (i != index)
                                } // for (i = j-2; localFound && (i <= j+2); i++)
                            } // for (j = index-2*201; localFound && (j <= index+2*201); j+= 201)

                            if (localFound) {
                                localMinNumber++;

                                if (localMinNumber <= 100) {
                                    localMinIndex[localMinNumber - 1] = index;
                                }
                            } // if (localFound)
                        } // if (index != indexmin)
                    } // for (x = 2; x <= 198; x++)
                } // for (y = 2; y <= 198; y++)

                if (indexmin != indexOriginal) {
                    Preferences.debug("sse original parameters = " + sses[indexOriginal] + "\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("sse original parameters = " + sses[indexOriginal] +
                                                                 "\n");
                    dataString += "sse original parameters = " + sses[indexOriginal] + "\n";
                } // if (indexmin != indexOriginal)

                Preferences.debug("sse global min = " + ssemin + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("sse global min = " + ssemin + "\n");
                dataString += "sse global min = " + ssemin + "\n";
                konScale = 0.02 * Math.pow(2500.0, 0.005 * xmin);
                Preferences.debug("x = " + xmin + " or new kon = " + konScale + " * original kon = " +
                                  (konScale * params[0]) + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("x = " + xmin + " or new kon = " + nf.format(konScale) +
                                                             " * original kon = " + nf.format(konScale * params[0]) +
                                                             "\n");
                dataString += "x = " + xmin + " or new kon = " + nf.format(konScale) + " * original kon = " +
                              nf.format(konScale * params[0]) + "\n";
                koffScale = 0.02 * Math.pow(2500.0, 0.005 * ymin);
                Preferences.debug("y = " + ymin + " or new koff = " + koffScale + " * original koff = " +
                                  (koffScale * params[1]) + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("y = " + ymin + " or new koff = " + nf.format(koffScale) +
                                                             " * original koff = " + nf.format(koffScale * params[1]) +
                                                             "\n");
                dataString += "y = " + ymin + " or new koff = " + nf.format(koffScale) + " * original koff = " +
                              nf.format(koffScale * params[1]) + "\n";

                if (localMinNumber >= 1) {
                    Preferences.debug("In addition to the global minimum\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("In addition to the global minimum\n");
                    dataString += "In addition to the global minimum\n";

                    if (localMinNumber == 1) {
                        Preferences.debug("1 local minimum was located at:\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText("1 local minimum was located at:\n");
                        dataString += "1 local minimum was located at:\n";
                    } else {
                        Preferences.debug(localMinNumber + " local minima were located at:\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText(localMinNumber +
                                                                     " local minima were located at:\n");
                        dataString += localMinNumber + " local minima were located at:\n";
                    }

                    for (i = 0; i < localMinNumber; i++) {
                        index = localMinIndex[i];
                        y = index / 201;
                        x = index % 201;
                        Preferences.debug("sse local min  = " + sses[index] + "\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText("sse local min min = " + sses[index] + "\n");
                        dataString += "sse local min min = " + sses[index] + "\n";
                        konScale = 0.02 * Math.pow(2500.0, 0.005 * x);
                        Preferences.debug("x = " + x + " or new kon = " + konScale + " * original kon = " +
                                          (konScale * params[0]) + "\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText("x = " + x + " or new kon = " +
                                                                     nf.format(konScale) + " * original kon = " +
                                                                     nf.format(konScale * params[0]) + "\n");
                        dataString += "x = " + x + " or new kon = " + nf.format(konScale) + " * original kon = " +
                                      nf.format(konScale * params[0]) + "\n";
                        koffScale = 0.02 * Math.pow(2500.0, 0.005 * y);
                        Preferences.debug("y = " + y + " or new koff = " + koffScale + " * original koff = " +
                                          (koffScale * params[1]) + "\n", Preferences.DEBUG_ALGORITHM);
                        ViewUserInterface.getReference().setDataText("y = " + y + " or new koff = " +
                                                                     nf.format(koffScale) + " * original koff = " +
                                                                     nf.format(koffScale * params[1]) + "\n");
                        dataString += "y = " + y + " or new koff = " + nf.format(koffScale) + " * original koff = " +
                                      nf.format(koffScale * params[1]) + "\n";
                    } // for (i = 0; i < localMinNumber; i++)
                } // if (localMinNumber >= 1)

                errorImage = new ModelImage(ModelStorageBase.FLOAT, errExtents, srcImage.getImageName() + "_err");

                try {
                    errorImage.importData(0, sses, true);
                } catch (IOException err) {
                    MipavUtil.displayError("IOException " + err + " on errorImage.importData");
                    setCompleted(false);

                    return;
                }

                try {
                    errorFrame = new ViewJFrameImage(errorImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                    setCompleted(false);

                    return;
                }

                try {
                    voiID = 0;
                    globalPtVOI = new VOI(voiID, "pointGlobalMin.voi", VOI.POINT, -1.0f);
                    xPt[0] = xmin;
                    yPt[0] = ymin;
                    zPt[0] = 0.0f;
                    globalPtVOI.importCurve(xPt, yPt, zPt);

                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory on globalPtVOI creation");
                    setCompleted(false);

                    return;
                }

                errorImage.registerVOI(globalPtVOI);
                globalPtVOI.setActive(true);
                ((VOIPoint) (globalPtVOI.getCurves().elementAt(0))).setActive(true);

                if (localMinNumber > 0) {
                    localPtVOI = new VOI[localMinNumber];
                }

                for (i = 0; i < localMinNumber; i++) {

                    try {
                        voiID = (short) (i + 1);
                        localPtVOI[i] = new VOI(voiID, "pointLocalMin" + String.valueOf(i + 1) + ".voi", VOI.POINT,
                                                -1.0f);
                        index = localMinIndex[i];
                        zPt[0] = 0.0f;
                        yPt[0] = index / 201;
                        xPt[0] = index % 201;
                        localPtVOI[i].importCurve(xPt, yPt, zPt);
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory on localPtVOI" + (i + 1) + " creation");
                        setCompleted(false);

                        return;
                    }

                    errorImage.registerVOI(localPtVOI[i]);
                    localPtVOI[i].setActive(true);
                    ((VOIPoint) (localPtVOI[i].getCurves().elementAt(0))).setActive(true);
                } // for (i = 0; i < localMinNumber; i++)

                errorFrame.updateImages();
                errorImage.getFileInfo()[0].setFileDirectory(srcImage.getFileInfo(0).getFileDirectory());
                errorFrame.saveAllVOIs();
                ViewUserInterface.getReference().getMessageFrame().getData().setText(dataString);
            } // if (paramVary)


        } // else if (model == CIRCLE_2D)
        else if (model == PURE_1D) {

            // I(t) = bottom + span(1 - w/sqrt(w**2 + 4*PI*D*t))
            // (I(t) - bottom)/span = (1 - 1/sqrt(1 + (4*PI*D*t/w**2))
            // Let C = D/w**2
            // (I(t) - bottom)/span = (1 - 1/sqrt(1 + 4*PI*C*t))
            // Derivative of (I(t) - bottom)/span with respect to C:
            // 2*PI*t*(1 + 4*PI*C*t)**(-1.5)
            // p = (I(t) - bottom)/span
            // p = (1 - 1/sqrt(1 + 4*PI*C*t))
            // 1/sqrt(1 + 4*PI*C*t) = 1 - p
            // sqrt(1 + 4*PI*C*t) = 1/(1 - p)
            // 1 + 4*PI*C*t = (1/(1-p))**2
            // C = [(1/(1-p))**2 - 1]/(4*PI*t)
            // If no whole organ normalization is performed, the mobile fraction
            // is assumed to be 1 and span = 1 - bottom.
            if (wholeOrganIndex >= 0) {
                initial = new double[3];
                pMin = Float.MAX_VALUE;
                pMax = -Float.MAX_VALUE;

                for (i = 0; i < pIntensity.length; i++) {

                    if (pIntensity[i] > pMax) {
                        pMax = pIntensity[i];
                    }

                    if (pIntensity[i] < pMin) {
                        pMin = pIntensity[i];
                    }
                }

                pHalf = (pMin + pMax) / 2.0f;
                haveHalf = false;
                initial[1] = pMin;
                initial[2] = pMax - pMin;

                for (i = 0; (i < pIntensity.length) && (!haveHalf); i++) {

                    if (pIntensity[i] >= pHalf) {
                        haveHalf = true;
                        pNorm = (float) ((pIntensity[i] - initial[1]) / initial[2]);
                        initial[0] = (1.0 / (1.0 - pNorm));
                        initial[0] = ((initial[0] * initial[0]) - 1.0) / (4.0 * Math.PI * tValues[i]);
                    }
                }

                Preferences.debug("D/w**2 guess initial[0] = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom guess initial[1] = " + initial[1] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("span guess initial[2] = " + initial[2] + "\n", Preferences.DEBUG_ALGORITHM);
                fp1D = new FitPure1DModel(zDim - firstSliceNum, tValues, pIntensity, initial);
                fp1D.driver();
                fp1D.dumpResults();

                /*chiSquared = fp1D.getChiSquared();
                 * if (Double.isNaN(chiSquared)) { MipavUtil.displayError( "Fit pure 1D duffusion failed - Chi-squared
                 * was not a valid number\n"); if  {  } setCompleted(false);
                 * return; }*/
                params = fp1D.getParameters();
                bottom = params[1];
                span = params[2];
            } // if (wholeOrganIndex >= 0)
            else { // no whole organ normalization
                initial = new double[2];
                pMin = Float.MAX_VALUE;
                pMax = -Float.MAX_VALUE;

                for (i = 0; i < pIntensity.length; i++) {

                    if (pIntensity[i] > pMax) {
                        pMax = pIntensity[i];
                    }

                    if (pIntensity[i] < pMin) {
                        pMin = pIntensity[i];
                    }
                }

                pHalf = (pMin + pMax) / 2.0f;
                haveHalf = false;
                initial[1] = pMin;

                for (i = 0; (i < pIntensity.length) && (!haveHalf); i++) {

                    if (pIntensity[i] >= pHalf) {
                        haveHalf = true;
                        pNorm = (float) ((pIntensity[i] - initial[1]) / (1.0 - initial[1]));
                        initial[0] = (1.0 / (1.0 - pNorm));
                        initial[0] = ((initial[0] * initial[0]) - 1.0) / (4.0 * Math.PI * tValues[i]);
                    }
                }

                Preferences.debug("D/w**2 guess initial[0] = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom guess initial[1] = " + initial[1] + "\n", Preferences.DEBUG_ALGORITHM);
                double x[] = new double[3];
                x[1] = initial[0];
                x[2] = initial[1];
                int iv[] = new int[63]; // 61 + number of coefficients
                int vLength = 94 + (zDim - firstSliceNum)*2 + 3*(zDim - firstSliceNum) + 2*(3*2+33)/2;
                double v[] = new double[vLength];
                boolean useAnalyticalJacobian = true;
                fp1DNW = new FitPure1DNoWholeModel(zDim - firstSliceNum, tValues, pIntensity, x, iv, v, useAnalyticalJacobian);
                fp1DNW.driver();
                fp1DNW.dumpResults();

                /*chiSquared = fp1DNW.getChiSquared();
                 * if (Double.isNaN(chiSquared)) { MipavUtil.displayError( "Fit pure 1D no no whole diffusion failed -
                 * Chi-squared was not a valid number\n"); if  {  } setCompleted(false);
                 *
                 * return; }*/
                params = new double[2];
                params[0] = x[1];
                params[1] = x[2];
                
                bottom = params[1];
                span = 1.0 - params[1];
            } // else no whole organ normalization

            ViewUserInterface.getReference().setDataText("In the recovery curve bottom + span*[1 - (1/sqrt(1 + 4*PI*D*t/w**2))]\n");
            ViewUserInterface.getReference().setDataText("bottom = " + nf.format(bottom) + "\n");
            ViewUserInterface.getReference().setDataText("span = " + nf.format(span) + "\n");
            ViewUserInterface.getReference().setDataText("D/w**2 = " + nf.format(params[0]) + "\n");
            Deff = params[0] * photoBleachedWidth * photoBleachedWidth;
            ViewUserInterface.getReference().setDataText("Deff = " + nf.format(Deff) + " um**2/sec\n");
            Preferences.debug("In the recovery curve bottom + span*[1 - (1/sqrt(1 + 4*PI*D*t/w**2))]\n",
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("bottom = " + bottom + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("span = " + span + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("D/w**2 = " + params[0] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Deff = " + Deff + " um**2/sec\n", Preferences.DEBUG_ALGORITHM);
            mf = (float) (bottom + span);

            if (mf > 1.0f) {
                mf = 1.0f;
            } else if (mf < 0.0f) {
                mf = 0.0f;
            }

            ViewUserInterface.getReference().setDataText("Mobile fraction = " + nf.format(mf) + "\n");
            Preferences.debug("Mobile fraction = " + mf + "\n", Preferences.DEBUG_ALGORITHM);

            // Plot the intensity of the photobleached region with time
            tfValues = new float[2][zDim];
            pfValues = new float[2][zDim];

            if (timeStamp != null) {

                for (i = 0; i < zDim; i++) {
                    tfValues[0][i] = (float) timeStamp[i];
                    tfValues[1][i] = tfValues[0][i];
                }
            } // if (timeStamp != null)
            else { // timeStamp == null

                for (i = 0; i < zDim; i++) {
                    tfValues[0][i] = (float) (i - firstSliceNum) * srcImage.getFileInfo()[0].getResolutions()[2];
                    tfValues[1][i] = tfValues[0][i];
                }
            } // else timeStamp == null

            for (z = 0; z < photoBleachedIntensity.length; z++) {
                pfValues[0][z] = photoBleachedIntensity[z];
            }

            for (z = 0; z < firstSliceNum; z++) {
                pfValues[1][z] = 0.0f;
            }

            for (z = firstSliceNum; z < zDim; z++) {
                pfValues[1][z] = (float) (bottom +
                                          (span *
                                               (1 -
                                                    (1 /
                                                         Math.sqrt(1.0 +
                                                                       (4.0 * Math.PI * params[0] *
                                                                            tValues[z - firstSliceNum]))))));
            }

            if (paramVary) {
                int nPts = zDim - firstSliceNum;
                double dw2;
                double sse;
                double ssemin = Double.MAX_VALUE;
                int x;
                double dy;
                double pFit;
                int xmin = 100;

                for (x = 0; x <= 200; x++) {
                    dw2 = 0.05 * params[0] * Math.pow(400.0, 0.005 * x);
                    sse = 0.0;

                    for (i = 0; i < nPts; i++) {
                        pFit = bottom + (span * (1.0 - (1 / Math.sqrt(1.0 + (4.0 * Math.PI * dw2 * tValues[i])))));
                        dy = pFit - pIntensity[i];
                        sse += dy * dy;
                    } // for (i = 0; i < nPts; i++)

                    if (sse < ssemin) {
                        ssemin = sse;
                        xmin = x;
                    }

                    Preferences.debug("D/w**2 = " + dw2 + " sse = " + sse + "\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("D/w**2 = " + nf.format(dw2) + " sse = " + sse + "\n");
                } // for (x = 0; x <= 200; x++)

                Preferences.debug("D/w**2 is incremented in multiplicative factors of " + Math.pow(400.0, 0.005) +
                                  "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("from 1/20 * original D/w**2 to 20 * original D/w**2\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("D/w**2 is incremented in multiplicative factors of " +
                                                             nf.format(Math.pow(400.0, 0.005)) + "\n");
                ViewUserInterface.getReference().setDataText("from 1/20 * original D/w**2 to 20 * original D/w**2\n");
                Preferences.debug("ssemin = " + ssemin + " found at new D/w**2 = " +
                                  (0.05 * Math.pow(400.0, 0.005 * xmin)) + " * original D/w**2 = " +
                                  (0.05 * params[0] * Math.pow(400.0, 0.005 * xmin)) + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("ssemin = " + ssemin + " found at new D/w**2 = " +
                                                             nf.format(0.05 * Math.pow(400.0, 0.005 * xmin)) +
                                                             " * original D/w**2 = " +
                                                             nf.format(0.05 * params[0] * Math.pow(400.0,
                                                                                                       0.005 * xmin)) +
                                                             "\n");

            } // if (paramVary)
        } // else if (model == PURE1D)
        else { // model == SINGLE_EXPONENTIAL

            // bottom + span*[1 - exp(-ln(2)*t/thalf)]
            // If no whole organ normalization is performed, the mobile fraction
            // is assumed to be 1 and span = 1 - bottom.
            if (wholeOrganIndex >= 0) {
                initial = new double[3];
                pMin = Float.MAX_VALUE;
                pMax = -Float.MAX_VALUE;

                for (i = 0; i < pIntensity.length; i++) {

                    if (pIntensity[i] > pMax) {
                        pMax = pIntensity[i];
                    }

                    if (pIntensity[i] < pMin) {
                        pMin = pIntensity[i];
                    }
                }

                pHalf = (pMin + pMax) / 2.0f;
                haveHalf = false;

                for (i = 0; (i < pIntensity.length) && (!haveHalf); i++) {

                    if (pIntensity[i] >= pHalf) {
                        haveHalf = true;
                        initial[0] = tValues[i];
                    }
                }

                initial[1] = pMin;
                initial[2] = pMax - pMin;
                Preferences.debug("thalf = initial[0] = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom = initial[1] = " + initial[1] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("span = initial[2] = " + initial[2] + "\n", Preferences.DEBUG_ALGORITHM);
                fsem = new FitSingleExponentialModel(zDim - firstSliceNum, tValues, pIntensity, initial);
                fsem.driver();
                fsem.dumpResults();
                params = fsem.getParameters();
                bottom = params[1];
                span = params[2];
            } // if (wholeOrganIndex >= 0)
            else { // no whole organ normalization
                initial = new double[2];
                pMin = Float.MAX_VALUE;
                pMax = -Float.MAX_VALUE;

                for (i = 0; i < pIntensity.length; i++) {

                    if (pIntensity[i] > pMax) {
                        pMax = pIntensity[i];
                    }

                    if (pIntensity[i] < pMin) {
                        pMin = pIntensity[i];
                    }
                }

                pHalf = (pMin + pMax) / 2.0f;
                haveHalf = false;

                for (i = 0; (i < pIntensity.length) && (!haveHalf); i++) {

                    if (pIntensity[i] >= pHalf) {
                        haveHalf = true;
                        initial[0] = tValues[i];
                    }
                }

                initial[1] = pMin;
                Preferences.debug("thalf = initial[0] = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom = initial[1] = " + initial[1] + "\n", Preferences.DEBUG_ALGORITHM);
                double x[] = new double[3];
                x[1] = initial[0];
                x[2] = initial[1];
                int iv[] = new int[63]; // 61 + number of parameters
                int vLength = 94 + (zDim - firstSliceNum)*2 + 3*(zDim - firstSliceNum) + 2*(3*2+33)/2;
                double v[] = new double[vLength];
                boolean useAnalyticJacobian = true;
                fsemnw = new FitSingleExponentialNoWholeModel(zDim - firstSliceNum, tValues, pIntensity, x, iv, v, useAnalyticJacobian);
                fsemnw.driver();
                fsemnw.dumpResults();
                bottom = x[2];
                span = 1.0 - x[2];
                params = new double[2];
                params[0] = x[1];
                params[1] = x[2];
            } // else no whole organ normalization

            ViewUserInterface.getReference().setDataText("In the recovery curve bottom + span*[1 - exp(-ln(2)*t/thalf)]\n");
            ViewUserInterface.getReference().setDataText("bottom  = " + nf.format(bottom) + "\n");
            ViewUserInterface.getReference().setDataText("span = " + nf.format(span) + "\n");
            ViewUserInterface.getReference().setDataText("thalf = " + nf.format(params[0]) + "\n");
            Preferences.debug("In the recovery curve bottom + span*[1 - exp(-ln(2)*t/thalf)]\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("bottom = " + bottom + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("span = " + span + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("thalf = " + params[0] + "\n", Preferences.DEBUG_ALGORITHM);
            mf = (float) (bottom + span);

            if (mf > 1.0f) {
                mf = 1.0f;
            } else if (mf < 0.0f) {
                mf = 0.0f;
            }

            ViewUserInterface.getReference().setDataText("Mobile fraction = " + nf.format(mf) + "\n");
            Preferences.debug("Mobile fraction = " + mf + "\n", Preferences.DEBUG_ALGORITHM);

            // Plot the intensity of the photobleached region with time
            tfValues = new float[2][zDim];
            pfValues = new float[2][zDim];

            if (timeStamp != null) {

                for (i = 0; i < zDim; i++) {
                    tfValues[0][i] = (float) timeStamp[i];
                    tfValues[1][i] = tfValues[0][i];
                }
            } // if (timeStamp != null)
            else { // timeStamp == null

                for (i = 0; i < zDim; i++) {
                    tfValues[0][i] = (float) (i - firstSliceNum) * srcImage.getFileInfo()[0].getResolutions()[2];
                    tfValues[1][i] = tfValues[0][i];
                }
            } // else timeStamp == null

            for (z = 0; z < photoBleachedIntensity.length; z++) {
                pfValues[0][z] = photoBleachedIntensity[z];
            }

            for (z = 0; z < firstSliceNum; z++) {
                pfValues[1][z] = 0.0f;
            }

            for (z = firstSliceNum; z < zDim; z++) {
                pfValues[1][z] = (float) (bottom +
                                          (span *
                                               (1 - Math.exp(-Math.log(2) * tValues[z - firstSliceNum] / params[0]))));
            }

            if (paramVary) {
                int nPts = zDim - firstSliceNum;
                double thalf;
                double sse;
                double ssemin = Double.MAX_VALUE;
                int x;
                double dy;
                double pFit;
                int xmin = 100;

                for (x = 0; x <= 200; x++) {
                    thalf = 0.05 * params[0] * Math.pow(400.0, 0.005 * x);
                    sse = 0.0;

                    for (i = 0; i < nPts; i++) {
                        pFit = bottom + (span * (1.0 - Math.exp(-Math.log(2) * tValues[i] / thalf)));
                        dy = pFit - pIntensity[i];
                        sse += dy * dy;
                    } // for (i = 0; i < nPts; i++)

                    if (sse < ssemin) {
                        ssemin = sse;
                        xmin = x;
                    }

                    Preferences.debug("thalf = " + thalf + " sse = " + sse + "\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("thalf = " + nf.format(thalf) + " sse = " + sse +
                                                                 "\n");
                } // for (x = 0; x <= 200; x++)

                Preferences.debug("thalf is incremented in multiplicative factors of " + Math.pow(400.0, 0.005) + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("from 1/20 * original thalf to 20 * original thalf\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("thalf is incremented in multiplicative factors of " +
                                                             nf.format(Math.pow(400.0, 0.005)) + "\n");
                ViewUserInterface.getReference().setDataText("from 1/20 * original thalf to 20 * original thalf\n");
                Preferences.debug("ssemin = " + ssemin + " found at new thalf = " +
                                  (0.05 * Math.pow(400.0, 0.005 * xmin)) + " * original thalf = " +
                                  (0.05 * params[0] * Math.pow(400.0, 0.005 * xmin)) + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("ssemin = " + ssemin + " found at new thalf = " +
                                                             nf.format(0.05 * Math.pow(400.0, 0.005 * xmin)) +
                                                             " * original thalf = " +
                                                             nf.format(0.05 * params[0] * Math.pow(400.0,
                                                                                                       0.005 * xmin)) +
                                                             "\n");

            } // if (paramVary)
        } // else model == SINGLE_EXPONENTIAL

        colorArray = new Color[2];
        colorArray[0] = Color.red;
        colorArray[1] = Color.cyan;
        photoBleachGraph = new ViewJFrameGraph(tfValues, pfValues, "Photobleaching Recovery Curve", "Seconds",
                                               "Fluorescence", colorArray);
        photoBleachGraph.setBounds(50, yStart, 500, 400);
        photoBleachGraph.setVisible(true);

        if (doSecondFit) {
            photoBleachGraph2 = new ViewJFrameGraph(tfValues, pfValues2, "Photobleaching Recovery Curve ELSUNC",
                                                    "Seconds", "Fluorescence", colorArray);
            photoBleachGraph2.setBounds(50, yStart + 20, 500, 400);
            photoBleachGraph2.setVisible(true);

        }
        
        setCompleted(true);
        return;

    }

    /**
     * DOCUMENT ME!
     */
    public void runTest24D() {

        // The correct answer is a0 = 72.4326,  a1 = 28.2519, a2 = 0.5968
        double[] xSeries = new double[5];
        float[] ySeries = new float[5];
        double[] initial = new double[3];
        int nPoints = 5;
        Fit24DModel fmod;
        xSeries[0] = 0.0;
        xSeries[1] = 1.0;
        xSeries[2] = 2.0;
        xSeries[3] = 3.0;
        xSeries[4] = 4.0;
        ySeries[0] = 44.4f;
        ySeries[1] = 54.6f;
        ySeries[2] = 63.8f;
        ySeries[3] = 65.7f;
        ySeries[4] = 68.9f;
        initial[0] = 0.0;
        initial[1] = 10.0;
        initial[2] = 0.2;
        fmod = new Fit24DModel(nPoints, xSeries, ySeries, initial);
        fmod.driver();
        fmod.dumpResults();
    }

    /**
     * Copy important file information to resultant image structure.
     *
     * @param  image        Source image.
     * @param  resultImage  Resultant image.
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
        } else if (resultImage.getNDims() == 3) {
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                int j = Math.min(i, image.getExtents()[2] - 1);
                fileInfo[i].setModality(image.getFileInfo()[j].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[j].getFileDirectory());

                // fileInfo[i].setDataType(image.getFileInfo()[j].getDataType());
                fileInfo[i].setEndianess(image.getFileInfo()[j].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[j].getUnitsOfMeasure());
                fileInfo[i].setResolutions(image.getFileInfo()[j].getResolutions());
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(image.getImageOrientation());
                fileInfo[i].setAxisOrientation(image.getFileInfo()[j].getAxisOrientation());
                fileInfo[i].setOrigin(image.getFileInfo()[j].getOrigin());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[j].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[j].getPhotometric());

                if ((fileInfo[i] instanceof FileInfoXML) && (timeStamp != null)) {
                    ((FileInfoImageXML) fileInfo[i]).createPSet("timeStamp");

                    for (int q = 0; q < timeStamp.length; q++) {
                        ((FileInfoImageXML) fileInfo[i]).getCurrentPSet().addParameter("timeStamp[" + q + "]");
                        ((FileInfoImageXML) fileInfo[i]).getCurrentPSet().getCurrentParameter().setValueType("double");
                        ((FileInfoImageXML) fileInfo[i]).getCurrentPSet().getCurrentParameter().setValue(String.valueOf(timeStamp[q]));
                    }
                }
            }
        } else if (resultImage.getNDims() == 4) {
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < (resultImage.getExtents()[2] * resultImage.getExtents()[3]); i++) {
                fileInfo[i].setModality(image.getFileInfo()[i].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[i].getFileDirectory());
                fileInfo[i].setEndianess(image.getFileInfo()[i].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[i].getUnitsOfMeasure());
                fileInfo[i].setResolutions(image.getFileInfo()[i].getResolutions());
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(image.getImageOrientation());
                fileInfo[i].setAxisOrientation(image.getFileInfo()[i].getAxisOrientation());
                fileInfo[i].setOrigin(image.getFileInfo()[i].getOrigin());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[i].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[i].getPhotometric());
            }
        }

    }


    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private double actInt(double x) {
        return 2.0 * Math.sqrt(x);
    }

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void runFullModelTest() {
        File file;
        RandomAccessFile raFile;
        String lineString = null;
        double[] tValues = null;
        float[] pIntensity = null;
        double[][] timeM = null;
        float[][] dataM = null;
        StringTokenizer t;
        boolean exceptionOccurred;
        String varString;
        int i;
        int j;
        String dataString = "";
        double[] initial_kon = new double[401 * 401];
        double[] initial_koff = new double[401 * 401];
        int count = 0;

        for (i = 0; i <= 400; i++) {

            for (j = 0; j <= 400; j++) {
                initial_kon[count] = 1.0E-5 * Math.pow(1.0E10, 0.0025 * i);
                initial_koff[count++] = 1.0E-5 * Math.pow(1.0E10, 0.0025 * j);
            } // for (j = 0; j <= 400; j++)
        } // for (i = 0; i <= 400; i++)

        // An estimate for the maximum of the real parts of the singularities
        // of F. If unknown, set largestPole = 0.0
        //double largestPole = 0.0;

        // numerical tolerance of approaching pole (default = 1.0e-9)
        //double tol = 1.0e-9;
        //FitFullModel lmod;
        //FitFullModel2 lmod2;
        //FitFullModelqd lmodqd;
        //FitWholeNL2solModel nonlinmod;
        //FitWholeNLConModel nlinmod2;
        //FitWholeNLConModel2 nlinmod4;
        //FitWholeNLConModel3 nlinmod6;
        //FitWholeNLConModelqd nlinmod8;
        FitWholeNL2solInt2 nlinmod11;
        FitWholeNLConInt2 nlinmod12;
        //FitFullIntModel imod;
        //FitFullIntModel imod2;
        //FitFullIntModel imod3;
        //FitFullIntModel2i imod2i;
        //FitFullIntModel2s imod2s;
        //double bound;
        //double lowers = 0.0;
        //double uppers;
        //int inf = 1;
        //int routinei = Integration2.DQAGIE;
        //int routines = Integration2.DQAGSE;
        //double epsabsi = 0.0;
        //double epsabss = 0.0;
        //double epsreli = 1.0E-3;
        //double epsrels = 1.0E-3;
        //int limiti = 100;
        //int limits = 100;
        double[] timeFunction = null;
        //double[] fitR = new double[200];
        //double[] sses = new double[401 * 401];
        double minsses;
        //int kmin = 0;
        double[] initial = new double[2];
        double[] params;
        //double abscissa = 0.0;
        //double relEps = 1.0E-12;
        //double absEps = 1.0E-12;
        double[] result = new double[200];
        //double[] estErr = new double[200];
        //int[] evaluations = new int[200];
        //int[] errStatus = new int[200];
        //double eps = 3.0E-6;
        //double part1;
        //double part2;
        double[] tValuesWithZero;
        //double[] tOne = new double[1];
        //int nLaguerre = 16;
        //double sig0 = 0.0;
        double sigmax;
        double bmax;
        //double tols = 1.0e-6;
        //double tolb = 1.0e-6;
        //double sig;
        //double b;
        //double[] estimatedError;
        long time;

        radius = 1.0f;
        diffusion = 10.0f;

        try {
            tValues = new double[200];
            tValuesWithZero = new double[201];
            pIntensity = new float[200];
            timeM = new double[8][200];
            dataM = new float[8][200];
            file = new File("C:/images/FRAP/Sample_data.txt");
            raFile = new RandomAccessFile(file, "r");
            lineString = raFile.readLine();
            lineString = raFile.readLine();
            lineString = raFile.readLine();
            lineString = raFile.readLine();

            for (j = 0; j < 200; j++) {
                lineString = raFile.readLine();
                t = new StringTokenizer(lineString);
                exceptionOccurred = false;

                while (!exceptionOccurred) {

                    try {

                        for (i = 0; i < 8; i++) {
                            varString = t.nextToken();
                            timeM[i][j] = Double.parseDouble(varString);
                            varString = t.nextToken();
                            dataM[i][j] = Float.parseFloat(varString);
                        }
                    } catch (NoSuchElementException e) {
                        exceptionOccurred = true;
                    }
                } // while (!exceptionOcurred)
            } // for (j = 0; j < 200; j++)

            raFile.close();

            // for ( i = 0; i < 8; i++ ) {
            i = 0;

            if (i == 0) {
                Preferences.debug("Data Set A\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Data Set A\n");
                dataString += "Data Set A\n";
            } else if (i == 1) {
                Preferences.debug("Data Set B\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Data Set B\n");
                dataString += "Data Set B\n";
            } else if (i == 2) {
                Preferences.debug("Data Set C\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Data Set C\n");
                dataString += "Data Set C\n";
            } else if (i == 3) {
                Preferences.debug("Data Set D\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Data Set D\n");
                dataString += "Data Set D\n";
            } else if (i == 4) {
                Preferences.debug("Data Set E\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Data Set E\n");
                dataString += "Data Set E\n";
            } else if (i == 5) {
                Preferences.debug("Data Set F\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Data Set F\n");
                dataString += "Data Set F\n";
            } else if (i == 6) {
                Preferences.debug("Data Set G\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Data Set G\n");
                dataString += "Data Set G\n";
            } else {
                Preferences.debug("Data Set H\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Data Set H\n");
                dataString += "Data Set H\n";
            }

            tValuesWithZero[0] = 0.0;

            for (j = 0; j < 200; j++) {
                tValues[j] = timeM[i][j];

                // tValuesWithZero[j + 1] = tValues[j];
                pIntensity[j] = dataM[i][j];
            }

            minsses = Double.MAX_VALUE;
            timeFunction = new double[tValues.length];
            sigmax = 30.0;
            bmax = 50.0;
            // tOne[0] = tValues[tValues.length/2];
            // timeFunction = new double[tValuesWithZero.length];
            // time = System.currentTimeMillis();
            // for (k = 0; k < 401*401; k++) {
            // Preferences.debug("k = " + k + "\n", Preferences.DEBUG_ALGORITHM);
            // sses[k] = 0.0;
            // lmodWeeks = new FitFullModelWeeks(tOne, nLaguerre, sig0, sigmax,
            // bmax, tols, tolb,
            // initial_kon[k],
            // initial_koff[k]);
            // lmodWeeks.wpar2();
            // b = lmodWeeks.getBOpt();
            // sig = lmodWeeks.getSigOpt();
            // lmodWeeks = new FitFullModelWeeks(tValues, nLaguerre, sig, b,
            // initial_kon[k], initial_koff[k]);
            // lmodWeeks.driver();
            // timeFunction = lmodWeeks.getTimeFunction();
            // estimatedError = lmodWeeks.getEstimatedError();
            // lmodqd = new FitFullModelqd(tValuesWithZero, initial_kon[k],
            // initial_koff[k]);
            // lmodqd.driver();
            // timeFunction = lmodqd.getTimeFunction();*/
            /*k = 0;
             *           initial_kon[k] = 0.01;          initial_koff[k] = 0.01;          lower = 0.0;          upper =
             * initial_koff[k];          lower2 = initial_koff[k] + initial_kon[k];*/
            // uppers = initial_koff[k];
            // bound = initial_kon[k] + initial_koff[k];
            // for (j = 0; j < timeFunction.length - 1; j++) {
            // for (j = 0; j < timeFunction.length; j++) {
            /*imod = new FitFullIntModel(tValues[j], initial_kon[k],
             *         initial_koff[k], lower, upper, routine, eps); imod.driver(); part1 = imod.getIntegral(); imod2 =
             * new FitFullIntModel(tValues[j], initial_kon[k],        initial_koff[k], lower2, upper2, routine2, eps2);
             * imod2.driver();part2 = imod2.getIntegral();*/
            // imod2s = new FitFullIntModel2s(tValues[j],initial_kon[k],
            // initial_koff[k],lowers,uppers,routines,epsabss,epsrels,limits);
            // imod2s.driver();
            // part1 = imod2s.getIntegral();
            // imod2i = new FitFullIntModel2i(tValues[j], initial_kon[k],
            // initial_koff[k],bound,routinei,inf,epsabsi,epsreli,limiti);
            // imod2i.driver();
            // part2 = imod2i.getIntegral();
            // timeFunction[j] = 1 - part1 - part2;

            // Preferences.debug("j = " + j + " pIntensity = " + pIntensity[j] +
            // " timeFunction = " + timeFunction[j] + "\n", Preferences.DEBUG_ALGORITHM);
            // Preferences.debug("part1 = " + part1 + " part2 = " + part2 + "\n", Preferences.DEBUG_ALGORITHM);
            // fitR[j] = timeFunction[j+1] - pIntensity[j];
            // fitR[j] = timeFunction[j] - pIntensity[j];
            // sses[k] = sses[k] + fitR[j] * fitR[j];

            // } // for (j = 0; j < timeFunction.length; j++)
            ///if (sses[k] < minsses) {
            // minsses = sses[k];
            // kmin = k;
            // }
            // } // for (k = 0; k < 401*401; k++)

            /*initial[0] = initial_kon[kmin];
             * initial[1] = initial_koff[kmin]; Preferences.debug("Best of 401*401 trials yields initial guesses
             * of:\n", Preferences.DEBUG_ALGORITHM); 
             * Preferences.debug("Best initial kon guess = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM); 
             * Preferences.debug("Best initial koff guess = " + initial[1] + "\n", Preferences.DEBUG_ALGORITHM); 
             * UI.setDataText("Best of 401*401 trials yields initial
             * guesses of:\n"); UI.setDataText("Best initial kon guess = " + initial[0] + "\n"); UI.setDataText("Best
             * initial koff guess = " + initial[1] + "\n"); dataString += "Best of 401*401 trials yields initial guesses
             * of:\n"; dataString += "Best initial kon guess = " + initial[0] + "\n"; dataString += "Best initial koff
             * guess = " + initial[1] + "\n"; time = System.currentTimeMillis() - time; UI.setDataText("Time in minutes
             * = " + time/(1000.0 * 60.0) + "\n"); dataString += "Time in minutes = " + time/(1000.0 * 60.0) +
             * "\n";Preferences.debug("Time in minutes = " + time/(1000.0 * 60.0) + "\n", Preferences.DEBUG_ALGORITHM);*/

            initial[0] = 2.0 * 0.01;
            initial[1] = 0.5 * 0.01;
            ViewUserInterface.getReference().setDataText("initial kon = " + initial[0] +
                                                         " is twice the actual value\n");
            ViewUserInterface.getReference().setDataText("initial koff = " + initial[1] +
                                                         " is half the actual value\n");
            Preferences.debug("initial kon = " + initial[0] + " is twice the actual value\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("initial koff = " + initial[1] + " is half the actual value\n", Preferences.DEBUG_ALGORITHM);
            // lmod2 = new FitFullModel2(tValues, abscissa, relEps, absEps, result, estErr, evaluations, errStatus,
            // initial_kon[kmin], initial_koff[kmin]); lmod2.driver(); for (j = 0; j < result.length; j++) { if
            // (errStatus[j] == 1) { Preferences.debug("time[" + j + "] did not converge after maximum  allowed
            // iterations\n", Preferences.DEBUG_ALGORITHM); } } // for (j = 0; j < result.length; j++)

            
            time = System.currentTimeMillis();
            double x[] = new double[3];
            x[1] = initial[0];
            x[2] = initial[1];
            int iv[] = new int[63]; // 61 + number of parameters
            int vLength = 94 + tValues.length*2 + 3*tValues.length + 2*(3*2+33)/2;
            double v[] = new double[vLength];
            boolean useAnalyticJacobian = false;
            nlinmod11 = new FitWholeNL2solInt2(tValues.length, tValues, pIntensity, x, iv, v, useAnalyticJacobian);
            nlinmod11.driver();
            nlinmod11.dumpResults();

            ViewUserInterface.getReference().setDataText("NL2sol nonlinear fit\n");
            ViewUserInterface.getReference().setDataText("kon = " + x[1] + "\n");
            ViewUserInterface.getReference().setDataText("koff = " + x[2] + "\n");
            dataString += "NL2sol nonlinear fit\n";
            dataString += "kon = " + x[1] + "\n";
            dataString += "koff = " + x[2] + "\n";
            Preferences.debug("NL2sol nonlinear fit\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("kon = " + x[1] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("koff = " + x[2] + "\n", Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis() - time;
            ViewUserInterface.getReference().setDataText("Time in minutes = " + (time / (1000.0 * 60.0)) + "\n");
            dataString += "Time in minutes = " + (time / (1000.0 * 60.0)) + "\n";
            Preferences.debug("Time in minutes = " + (time / (1000.0 * 60.0)) + "\n", Preferences.DEBUG_ALGORITHM);

            // nlinmod2 = new FitWholeNLConModel(tValues.length, tValues, pIntensity,
            // initial, largestPole, tol);
            // nlinmod2.driver();
            // nlinmod2.dumpResults();
            // params = nlinmod2.getParameters();
            // nlinmod4 = new FitWholeNLConModel2(tValues.length, tValues, pIntensity,
            // //initial, abscissa, relEps, absEps,
            // result, estErr, evaluations,
            // errStatus);
            // nlinmod4.driver();
            // nlinmod4.dumpResults();
            // params = nlinmod4.getParameters();
            // nlinmod6 = new FitWholeNLConModel3(tValues.length, tValues, pIntensity,
            // initial,upper, routine, eps, result);
            // nlinmod6.driver();
            // nlinmod6.dumpResults();
            // params = nlinmod6.getParameters();
            time = System.currentTimeMillis();
            nlinmod12 = new FitWholeNLConInt2(tValues.length, tValues, pIntensity, initial, result);
            nlinmod12.driver();
            nlinmod12.dumpResults();
            params = nlinmod12.getParameters();
            // nlinmod8 = new FitWholeNLConModelqd(tValues.length, tValues, pIntensity, initial); nlinmod8.driver();
            // nlinmod8.dumpResults(); params = nlinmod8.getParameters();
            /*time = System.currentTimeMillis();
             * nlinmod10 = new FitWholeNLConModelWeeks(tValues.length, tValues, pIntensity, initial, nLaguerre,
             *        sig0, sigmax, bmax,                  tols, tolb); nlinmod10.driver();
             * nlinmod10.dumpResults();params =
             * nlinmod10.getParameters();*/

            ViewUserInterface.getReference().setDataText("ELSUNC nonlinear fit\n");
            ViewUserInterface.getReference().setDataText("kon = " + params[0] + "\n");
            ViewUserInterface.getReference().setDataText("koff = " + params[1] + "\n");
            dataString += "ELSUNC nonlinear fit\n";
            dataString += "kon = " + params[0] + "\n";
            dataString += "koff = " + params[1] + "\n";
            Preferences.debug("ELSUNC nonlinear fit\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("kon = " + params[0] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("koff = " + params[1] + "\n", Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis() - time;
            ViewUserInterface.getReference().setDataText("Time in minutes = " + (time / (1000.0 * 60.0)) + "\n");
            dataString += "Time in minutes = " + (time / (1000.0 * 60.0)) + "\n";
            Preferences.debug("Time in minutes = " + (time / (1000.0 * 60.0)) + "\n", Preferences.DEBUG_ALGORITHM);


            /*paramVary = false;
             * if ( paramVary && (i == 7)) { // Find the sum of square of errors for variations in kon and koff
             * ModelImage errorImage; ViewJFrameImage errorFrame; double sse; double ssemin = Double.MAX_VALUE; double
             * ssemax = -Double.MAX_VALUE; int nPts = 200; sses = new double[401 * 401]; int y; int x; int indexY; int
             * index; double kon; double koff; int xmin = 100; int ymin = 100; int  indexmin = 0; boolean localFound;
             * int localMinNumber = 0; int[] localMinIndex = new int[1000]; indexmin = ymin * 401 + xmin; int
             * indexOriginal = indexmin; double konScale; double koffScale; int errExtents[] = new int[2]; errExtents[0]
             * = 401; errExtents[1] = 401; short voiID; VOI globalPtVOI; VOI[] localPtVOI = null; float[] xPt = new
             * float[1]; float[] yPt = new float[1]; float[] zPt = new float[1]; int p;
             *
             *
             * for (y = 0; y <= 400; y++) { System.out.println("y = " + y); indexY = 401 * y; koff = 1.0E-5 *
             * Math.pow(1.0E10, 0.0025*y); for (x = 0; x <= 400; x++) {   index = indexY + x;   sses[index] = 0.0;   kon
             * = 1.0E-5 * Math.pow(1.0E10, 0.0025*x);   lmod = new FitFullModel(tValues, largestPole, tol, kon, koff);
             * lmod.driver();   timeFunction = lmod.getTimeFunction();   for (j = 0; j < timeFunction.length; j++) {
             * fitR[j] = timeFunction[j] - pIntensity[j];     sses[index] = sses[index] + fitR[j]*fitR[j];   } // for (i
             * = 0; i < timeFunction.length; i++)   if ( sses[index] < ssemin ) {       ssemin = sses[index];       xmin
             * = x;       ymin = y;       indexmin = index;   } // if (sse < ssemin)   if ( sses[index] > ssemax ) {
             * ssemax = sses[index];   } } // for (x = 0; x <= 200; x++) } // for (y = 0; y <= 200; y++)
             *
             * for ( y = 2; y <= 398; y++ ) {   indexY = 401 * y;   for ( x = 2; x <= 398; x++ ) {     index = indexY + x;
             *  if ( index != indexmin ) {       localFound = true;       for ( j = index - 2 * 401; localFound && ( j
             * <= index + 2 * 401 ); j += 401 ) {         for ( p = j - 2; localFound && ( p <= j + 2 ); p++ ) {    if (
             * p != index ) {             if ( 1.000001*sses[index] >= sses[p] ) {               localFound = false;   }
             *           } // if (i != index)         } // for (i = j-2; localFound && (i <= j+2); i++) } // for (j =
             * index-2*201; localFound && (j <= index+2*201); j+= 201)       if ( localFound ) { localMinNumber++;    if
             * ( localMinNumber <= 1000 ) { localMinIndex[localMinNumber - 1] = index;   }       } // if (localFound)
             *  } // if (index != indexmin)   } // for (x = 2; x <= 198; x++) } // for (y = 2; y <= 198; y++)
             *
             *
             * Preferences.debug( "sse global min = " + ssemin + "\n", Preferences.DEBUG_ALGORITHM ); 
             * UI.setDataText( "sse global min = " + ssemin +
             * "\n" ); dataString += "sse global min = " + ssemin + "\n"; kon = 1.0E-5 * Math.pow( 1.0E10, 0.0025 * xmin
             * ); Preferences.debug("kon = " + kon + "\n", Preferences.DEBUG_ALGORITHM );
             *  UI.setDataText(         "kon = " + nf.format( kon )  + "\n"
             * ); dataString += "kon = " + nf.format( kon ) + "\n"; koff = 1.0E-5 * Math.pow( 1.0E10, 0.0025 * ymin );
             * Preferences.debug(         "koff = " + koff + "\n", Preferences.DEBUG_ALGORITHM );
             *  UI.setDataText(         "koff = " + nf.format( koff
             * ) + "\n" ); dataString += "koff = " + nf.format( koff ) + "\n";
             *
             * if (localMinNumber >= 1) {   
             * Preferences.debug("In addition to the global minimum\n", Preferences.DEBUG_ALGORITHM);   
             * UI.setDataText("In
             * addition to the global minimum\n");   dataString += "In addition to the global minimum\n";   if
             * (localMinNumber == 1) {     Preferences.debug("1 local minimum was located at:\n", Preferences.DEBUG_ALGORITHM);    
             *  UI.setDataText("1
             * local minimum was located at:\n");     dataString += "1 local minimum was located at:\n";   }   else {
             * Preferences.debug(localMinNumber +                       " local minima were located at:\n", 
             * Preferences.DEBUG_ALGORITHM);
             * UI.setDataText(localMinNumber + " local minima were located at:\n");     dataString += localMinNumber + "
             * local minima were located at:\n";   }   for (j = 0; j < localMinNumber; j++) {     index =
             * localMinIndex[j];     y = index / 401;     x = index % 401;     Preferences.debug("sse local min  = " +
             * sses[index] + "\n", Preferences.DEBUG_ALGORITHM);     UI.setDataText("sse local min min = " + sses[index] +
             *  "\n");     dataString +=
             * "sse local min min = " + sses[index] + "\n";     kon = 1.0E-5 * Math.pow(1.0E10, 0.0025 * x);
             * Preferences.debug(         "kon = " + kon + "\n", Preferences.DEBUG_ALGORITHM);     
             * UI.setDataText(         "kon = " + nf.format(kon)
             * + "\n");     dataString += "kon = " +         nf.format(kon) + "\n";     koff = 1.0E-5 * Math.pow(1.0E10,
             * 0.0025 * y);     Preferences.debug(         "koff = " + koff + "\n", Preferences.DEBUG_ALGORITHM);    
             *  UI.setDataText(         "koff =
             * " + nf.format(koff) + "\n");     dataString += "koff = " + nf.format(koff) + "\n";   } // for (i = 0; i <
             * localMinNumber; i++) } // if (localMinNumber >= 1) errorImage = new ModelImage(ModelStorageBase.FLOAT,
             * errExtents,                             srcImage.getImageName() + "_err", UI); try {
             * errorImage.importData(0, sses, true); } catch (IOException err) {   MipavUtil.displayError("IOException "
             * + err +                          " on errorImage.importData");   setCompleted(false);   return; }
             *
             * try {   errorFrame = new ViewJFrameImage(errorImage, null,                                    new
             * Dimension(610, 200), UI); } catch (OutOfMemoryError error) {   System.gc();   MipavUtil.displayError("Out
             * of memory: unable to open new frame");   setCompleted(false);   return; }
             *
             * try {   voiID = 0;   globalPtVOI = new VOI(voiID, "pointGlobalMin.voi",                         1,
             * VOI.POINT, -1.0f);   xPt[0] = xmin;   yPt[0] = ymin;   zPt[0] = 0.0f;   globalPtVOI.importCurve(xPt, yPt,
             * zPt, 0);
             *
             * } catch (OutOfMemoryError error) {   System.gc();   MipavUtil.displayError("Out of memory on globalPtVOI
             * creation");   setCompleted(false);   return; }
             *
             * errorImage.registerVOI(globalPtVOI); globalPtVOI.setActive(true); ( (VOIPoint)
             * (globalPtVOI.getCurves()[0].elementAt(0))).setActive(true);
             *
             * if (localMinNumber > 0) {   localPtVOI = new VOI[localMinNumber]; } for (j = 0; j < localMinNumber; j++) {
             * try {     voiID = (short) (j + 1);     localPtVOI[j] = new VOI(voiID, "pointLocalMin" + String.valueOf(j
             * + 1) +                             ".voi", 1, VOI.POINT,                             -1.0f);     index =
             * localMinIndex[j];     zPt[0] = 0.0f; yPt[0] = index / 401;     xPt[0] = index % 401;
             * localPtVOI[j].importCurve(xPt, yPt, zPt, 0);   } catch (OutOfMemoryError error) {     System.gc();
             * MipavUtil.displayError("Out of memory on localPtVOI" + (j + 1) +                            " creation");
             *   setCompleted(false);     return;   }
             *
             * errorImage.registerVOI(localPtVOI[j]);   localPtVOI[j].setActive(true);   ( (VOIPoint)
             * (localPtVOI[j].getCurves()[0].elementAt(0))).       setActive(true); } // for (j = 0; j < localMinNumber;
             * j++) errorFrame.updateImages(); errorImage.getFileInfo()[0].setFileDirectory(srcImage.getFileInfo(0).
             * getFileDirectory()); errorFrame.saveAllVOIs(); errorImage.getDataArea().setText(dataString);} // if
             * (paramVary)*/

            // } // for (i = 0; i < 8; i++)

            /*file = new File( "C:/images/FRAP/Sample_results.txt" );
             * raFile = new RandomAccessFile( file, "rw" ); // Necessary so that if this is an overwritten file there
             * isn't any // junk at the end raFile.setLength( 0 ); raFile.write( dataString.getBytes()
             * );raFile.close();*/


        } // try
        catch (Exception e) {
            System.gc();
            MipavUtil.displayError("Exception " + e);
        }

        return;
    }

    /**
     * private void runIntegrationTest2() { IntModel2 imod; double lower = 0.0; double upper = 1.0; int
     * routine = Integration2.DQAGPE; double breakPoints[] = new double[4]; breakPoints[0] = 1.0/7.0; breakPoints[1] =
     * 2.0/3.0; double epsabs = 0.0; double epsrel = 1.0E-4; int limit = 100; double numInt; int errorStatus; double
     * absError; int neval; double eps = 5.0e-7; int steps; imod = new
     * IntModel2(lower, upper, routine, breakPoints, epsabs, epsrel, limit); imod.driver(); numInt = imod.getIntegral();
     * errorStatus = imod.getErrorStatus(); absError = imod.getAbserr(); neval = imod.getNeval();
     * Preferences.debug("Numerical Integral = " + numInt + " after " + neval +" integrand evaluations used\n", 
     * Preferences.DEBUG_ALGORITHM);
     * Preferences.debug("Error status = " + errorStatus +" with absolute error = " + absError + "\n", 
     * Preferences.DEBUG_ALGORITHM); 
     */
    @SuppressWarnings("unused")
    private void runIntegrationTest2() {
        IntModel2 imod;
        int routine = Integration2.DQAGE;
        int key = 6;
        double epsabs = 0.0;
        double epsrel = 1.0E-3;
        int limit = 100;
        double numInt;
        int errorStatus;
        double absError;
        int neval;
        double lower = 0.0;
        double upper = 1.0;
        imod = new IntModel2(lower, upper, routine, key, epsabs, epsrel, limit);
        imod.driver();
        numInt = imod.getIntegral();
        errorStatus = imod.getErrorStatus();
        absError = imod.getAbserr();
        neval = imod.getNeval();
        Preferences.debug("Numerical Integral = " + numInt + " after " + neval + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + errorStatus + " with absolute error = " + absError + "\n", 
        		Preferences.DEBUG_ALGORITHM);
    }

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void runLapTest() {

        // This routine tests the numerical inverse laplace transform of a
        // function in InverseLaplace with an analytically known inverse.  The
        // function to be transformed is exp(a*t)
        // parameter a in exp(a*t)
        double a = 1.0;

        // An estimate for the maximum of the real parts of the singularities
        // of F. If unknown, set largestPole = 0.0
        double largestPole = 1.0;

        // numerical tolerance of approaching pole (default = 1.0e-9)
        double tol = 1.0e-9;

        // number of times to invert for
        int n = 30;

        // vector of times to invert for
        double[] t = new double[n];

        // true value of the function exp(a*t)
        double[] ftrue = new double[n];
        int i;
        FitExpModel lmod;

        double[] timeFunction = null;

        for (i = 1; i <= n; i++) {
            t[i - 1] = Math.pow(10.0, (0.5 * (double) i) - 13.0);
            ftrue[i - 1] = Math.exp(a * t[i - 1]);
        }

        lmod = new FitExpModel(t, largestPole, tol);
        lmod.driver();
        timeFunction = lmod.getTimeFunction();

        for (i = 0; i < n; i++) {
            Preferences.debug("time = " + t[i] + " routineFunction = " + timeFunction[i] + " trueFunction = " +
                              ftrue[i] + "\n", Preferences.DEBUG_ALGORITHM);
        }

    }

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void runLapTest2() {

        // This routine tests the numerical inverese Laplace transform of a
        // function in InverseLaplace2 with an analytically known inverse.  The
        // function to be transformed is sin(t)
        double[] time = new double[] {
                            0.1, 1.0, 2.0, 3.0, 4.0, 5.0, 1.0E1, 2.0E1, 3.0E1, 4.0E1, 5.0E1, 6.0E1, 7.0E1, 8.0E1, 9.0E1,
                            1.0E2
                        };
        double abscissa = 0.0;
        double relEps = 1.0E-12;
        double absEps = 1.0E-12;
        double[] result = new double[time.length];
        double[] estErr = new double[time.length];
        int[] evaluations = new int[time.length];
        int[] errStatus = new int[time.length];
        FitSineModel smod;
        int i;

        smod = new FitSineModel(time, abscissa, relEps, absEps, result, estErr, evaluations, errStatus);
        smod.driver();

        for (i = 0; i < time.length; i++) {

            if (errStatus[i] == 2) {
                Preferences.debug("time[" + i + "] is illegally <= 0\n", Preferences.DEBUG_ALGORITHM);
            } else if (errStatus[i] == 1) {
                Preferences.debug("time[" + i + "] had computations terminated\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The maximum bound on Laplace evaluations was reached\n", Preferences.DEBUG_ALGORITHM);
            } else {
                Preferences.debug("time = " + time[i] + " routineFunction = " + result[i] + " trueFunction = " +
                                  Math.sin(time[i]) + "\n", Preferences.DEBUG_ALGORITHM);

            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void runLapTestqd() {

        // This routine tests the numerical inverse laplace transform of a
        // function in InverseLaplace with an analytically known inverse.  The
        // function to be transformed is exp(a*t)
        // parameter a in exp(a*t)
        double a = 1.0;

        // An estimate for the maximum of the real parts of the singularities
        // of F. If unknown, set largestPole = 0.0
        double largestPole = 1.0;

        // numerical tolerance of approaching pole (default = 1.0e-9)
        double tol = 1.0e-9;

        // number of times to invert for
        int n = 103;
        int matrixSizeParameter = 3;

        // vector of times to invert for
        double[] t = new double[n];

        // true value of the function exp(a*t)
        double[] ftrue = new double[n];
        int i;
        FitExpModelqd lmod;
        double sse = 0;
        double diff;
        double rms;

        double[] timeFunction = null;


        for (i = 0; i < n; i++) {
            t[i] = i * 0.1;
            ftrue[i] = Math.exp(a * t[i]);
        }

        Preferences.debug("matrixSizeParameter = " + matrixSizeParameter + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        lmod = new FitExpModelqd(n, t[n - 1], largestPole, tol, matrixSizeParameter);
        lmod.driver();
        timeFunction = lmod.getTimeFunction();

        for (i = 0; i < n; i++) {
            Preferences.debug("time = " + t[i] + " routineFunction = " + timeFunction[i] + " trueFunction = " +
                              ftrue[i] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        sse = 0.0;

        for (i = 1; i < n; i++) {
            diff = timeFunction[i] - ftrue[i];
            sse = sse + (diff * diff);
        }

        rms = Math.sqrt(sse / (n - 1));
        Preferences.debug("rms error = " + rms + "\n", Preferences.DEBUG_ALGORITHM);

    }

   

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void runTest() {
        File file;
        RandomAccessFile raFile;
        //long fileLength;
        String lineString = null;
        int i, j;
        double[] tValues = null;
        float[] pIntensity = null;
        double[] initialSingle = null;
        double[] initial = null;
        double[] initialPure = null;
        double[][] timeM = null;
        float[][] dataM = null;
        StringTokenizer t;
        boolean exceptionOccurred;
        String varString;
        float pMax;
        float pMin;
        boolean haveHalf;
        float pHalf;
        float pNorm;
        FitDoubleExponentialNoWholeNL2solModel fdemNL2solnw = null;
        FitDoubleExponentialNoWholeConstrainedModel fdemConstrainednw = null;
        FitSingleExponentialNoWholeModel fsemnw = null;
        FitPure1DNoWholeModel fp1DNW = null;
        double[] params = null;
        double bottom;
        double span;
        double dTemp;
        DecimalFormat nf;
        String dataString = "";
        double s1, s2;
        double kdD;
        double ka, kd, Dt;

        try {
            tValues = new double[200];
            pIntensity = new float[200];
            initialPure = new double[2];
            initialSingle = new double[2];
            initial = new double[4];
            timeM = new double[8][200];
            dataM = new float[8][200];
            nf = new DecimalFormat("0.00E0");
            file = new File("C:/images/Sample_data_model5.txt");
            raFile = new RandomAccessFile(file, "r");
            lineString = raFile.readLine();

            for (j = 0; j < 200; j++) {
                lineString = raFile.readLine();
                t = new StringTokenizer(lineString);
                exceptionOccurred = false;

                while (!exceptionOccurred) {

                    try {

                        for (i = 0; i < 8; i++) {
                            varString = t.nextToken();
                            timeM[i][j] = Double.parseDouble(varString);
                            varString = t.nextToken();
                            dataM[i][j] = Float.parseFloat(varString);
                        }
                    } catch (NoSuchElementException e) {
                        exceptionOccurred = true;
                    }
                } // while (!exceptionOcurred)
            } // for (j = 0; j < 200; j++)

            raFile.close();

            for (i = 0; i < 8; i++) {

                if (i == 0) {
                    Preferences.debug("Data Set A\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("Data Set A\n");
                    dataString += "Data Set A\n";
                } else if (i == 1) {
                    Preferences.debug("Data Set B\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("Data Set B\n");
                    dataString += "Data Set B\n";
                } else if (i == 2) {
                    Preferences.debug("Data Set C\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("Data Set C\n");
                    dataString += "Data Set C\n";
                } else if (i == 3) {
                    Preferences.debug("Data Set D\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("Data Set D\n");
                    dataString += "Data Set D\n";
                } else if (i == 4) {
                    Preferences.debug("Data Set E\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("Data Set E\n");
                    dataString += "Data Set E\n";
                } else if (i == 5) {
                    Preferences.debug("Data Set F\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("Data Set F\n");
                    dataString += "Data Set F\n";
                } else if (i == 6) {
                    Preferences.debug("Data Set G\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("Data Set G\n");
                    dataString += "Data Set G\n";
                } else {
                    Preferences.debug("Data Set H\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("Data Set H\n");
                    dataString += "Data Set H\n";
                }

                pMax = -Float.MAX_VALUE;

                for (j = 0; j < 200; j++) {
                    tValues[j] = timeM[i][j];
                    pIntensity[j] = dataM[i][j];

                    if (pIntensity[j] > pMax) {
                        pMax = pIntensity[j];
                    }
                }

                for (j = 0; j < 200; j++) {
                    pIntensity[j] = pIntensity[j] / pMax;
                }

                ViewUserInterface.getReference().setDataText("NL2sol fitting engine pure 1D diffusion\n");
                Preferences.debug("NL2sol fitting engine pure 1D diffusion\n", Preferences.DEBUG_ALGORITHM);
                dataString += "NL2sol fitting engine pure 1D diffusion\n";
                pMin = Float.MAX_VALUE;
                pMax = -Float.MAX_VALUE;

                for (j = 0; j < pIntensity.length; j++) {

                    if (pIntensity[j] > pMax) {
                        pMax = pIntensity[j];
                    }

                    if (pIntensity[j] < pMin) {
                        pMin = pIntensity[j];
                    }
                }

                pHalf = (pMin + pMax) / 2.0f;
                haveHalf = false;
                initialPure[1] = pMin;

                for (j = 0; (j < pIntensity.length) && (!haveHalf); j++) {

                    if (pIntensity[j] >= pHalf) {
                        haveHalf = true;
                        pNorm = (float) ((pIntensity[j] - initialPure[1]) / (1.0 - initialPure[1]));
                        initialPure[0] = (1.0 / (1.0 - pNorm));
                        initialPure[0] = ((initialPure[0] * initialPure[0]) - 1.0) / (4.0 * Math.PI * tValues[j]);
                    }
                }

                Preferences.debug("D/w**2 guess initialPure[0] = " + initialPure[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom guess initial[1] = " + initialPure[1] + "\n", Preferences.DEBUG_ALGORITHM);
                double x[] = new double[3];
                x[1] = initialPure[0];
                x[2] = initialPure[1];
                int iv[] = new int[63]; // 61 + number of coefficients
                int vLength = 94 + 200*2 + 3*200 + 2*(3*2+33)/2;
                double v[] = new double[vLength];
                boolean useAnalyticalJacobian = true;
                fp1DNW = new FitPure1DNoWholeModel(200, tValues, pIntensity, x, iv, v, useAnalyticalJacobian);
                fp1DNW.driver();
                fp1DNW.dumpResults();

                /*chiSquared = fp1DNW.getChiSquared();
                 * if (Double.isNaN(chiSquared)) { MipavUtil.displayError( "Fit pure 1D no no whole diffusion failed -
                 * Chi-squared was not a valid number\n"); if  {  } setCompleted(false);
                 *
                 * return; }*/
                params = new double[2];
                params[0] = x[1];
                params[1] = x[2];
                bottom = params[1];
                span = 1.0 - params[1];
                ViewUserInterface.getReference().setDataText("In the recovery curve bottom + span*[1 - (1/sqrt(1 + 4*PI*D*t/w**2))]\n");
                ViewUserInterface.getReference().setDataText("bottom = " + nf.format(bottom) + "\n");
                ViewUserInterface.getReference().setDataText("span = " + nf.format(span) + "\n");
                ViewUserInterface.getReference().setDataText("D/w**2 = " + nf.format(params[0]) + "\n");
                Preferences.debug("In the recovery curve bottom + span*[1 - (1/sqrt(1 + 4*PI*D*t/w**2))]\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom = " + bottom + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("span = " + span + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("D/w**2 = " + params[0] + "\n", Preferences.DEBUG_ALGORITHM);
                dataString += "In the recovery curve bottom + span*[1 - (1/sqrt(1 + 4*PI*D*t/w**2))]\n";
                dataString += "bottom = " + nf.format(bottom) + "\n";
                dataString += "D/w**2 = " + nf.format(params[0]) + "\n";
                ViewUserInterface.getReference().setDataText("Mobile fraction = 1.0\n\n");
                dataString += "Mobile fraction = 1.0\n\n";
                Preferences.debug("Mobile fraction = 1.0\n\n", Preferences.DEBUG_ALGORITHM);

                ViewUserInterface.getReference().setDataText("NL2sol fitting engine single exponential\n");
                Preferences.debug("NL2sol fitting engine single exponential\n", Preferences.DEBUG_ALGORITHM);
                dataString += "NL2sol fitting engine single exponential\n";

                pMin = Float.MAX_VALUE;
                pMax = -Float.MAX_VALUE;

                for (j = 0; j < pIntensity.length; j++) {

                    if (pIntensity[j] > pMax) {
                        pMax = pIntensity[j];
                    }

                    if (pIntensity[j] < pMin) {
                        pMin = pIntensity[j];
                    }
                }

                pHalf = (pMin + pMax) / 2.0f;
                haveHalf = false;

                for (j = 0; (j < pIntensity.length) && (!haveHalf); j++) {

                    if (pIntensity[j] >= pHalf) {
                        haveHalf = true;
                        initialSingle[0] = tValues[j];
                    }
                }

                initialSingle[1] = pMin;
                Preferences.debug("thalf = initialSingle[0] = " + initialSingle[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom = initialSingle[1] = " + initialSingle[1] + "\n", Preferences.DEBUG_ALGORITHM);
                x = new double[3];
                x[1] = initialSingle[0];
                x[2] = initialSingle[1];
                iv = new int[63]; // 61 + number of parameters
                vLength = 94 + 200*2 + 3*200 + 2*(3*2+33)/2;
                v = new double[vLength];
                useAnalyticalJacobian = true;
                fsemnw = new FitSingleExponentialNoWholeModel(200, tValues, pIntensity, x, iv, v, useAnalyticalJacobian);
                fsemnw.driver();
                fsemnw.dumpResults();
                bottom = x[2];
                span = 1.0 - x[2];
                ViewUserInterface.getReference().setDataText("In the recovery curve bottom + span*[1 - exp(-ln(2)*t/thalf)]\n");
                ViewUserInterface.getReference().setDataText("bottom  = " + nf.format(bottom) + "\n");
                ViewUserInterface.getReference().setDataText("span = " + nf.format(span) + "\n");
                ViewUserInterface.getReference().setDataText("thalf = " + nf.format(x[1]) + "\n");
                dataString += "In the recovery curve bottom + span*[1 - exp(-ln(2)*t/thalf)]\n";
                dataString += "bottom  = " + nf.format(bottom) + "\n";
                dataString += "span = " + nf.format(span) + "\n";
                dataString += "thalf = " + nf.format(x[1]) + "\n";
                Preferences.debug("In the recovery curve bottom + span*[1 - exp(-ln(2)*t/thalf)]\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom = " + bottom + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("span = " + span + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("thalf = " + x[1] + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Mobile fraction = 1.0\n\n");
                dataString += "Mobile fraction = 1.0\n\n";
                Preferences.debug("Mobile fraction = 1.0\n\n", Preferences.DEBUG_ALGORITHM);

                initial[0] = 0.5; // gamma

                // If only a single exponential were present, the exponential constant
                // would = ln(0.5)/(time to recover halfway)
                // Note that beta < alpha since alpha and beta are negative.
                haveHalf = false;
                pMin = Float.MAX_VALUE;
                pMax = -Float.MAX_VALUE;

                for (j = 0; j < pIntensity.length; j++) {

                    if (pIntensity[j] > pMax) {
                        pMax = pIntensity[j];
                    }

                    if (pIntensity[j] < pMin) {
                        pMin = pIntensity[j];
                    }
                }

                pHalf = (pMin + pMax) / 2.0f;

                for (j = 0; (j < pIntensity.length) && (!haveHalf); j++) {

                    if (pIntensity[j] >= pHalf) {
                        haveHalf = true;
                        initial[1] = 0.5 * Math.log(0.5) / (tValues[j]); // alpha
                        initial[2] = 4.0 * initial[1]; // beta
                    }
                }

                initial[3] = pMin; // bottom guess
                Preferences.debug("gamma guess initial[0] = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("alpha guess initial[1] = " + initial[1] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("beta guess initial[2] = " + initial[2] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom guess initial[3] = " + initial[3] + "\n", Preferences.DEBUG_ALGORITHM);
                
                ViewUserInterface.getReference().setDataText("NL2sol fitting engine double exponential\n");
                Preferences.debug("Nl2sol fitting engine double exponential\n", Preferences.DEBUG_ALGORITHM);
                dataString += "NL2sol fitting engine double exponential\n";
                x = new double[5];
                x[1] = initial[0];
                x[2] = initial[1];
                x[3] = initial[2];
                x[4] = initial[3];
                iv = new int[65]; // 61 + number of coefficients
                vLength = 94 + 200*4 + 3*200 + 4*(3*4+33)/2;
                v = new double[vLength];
                useAnalyticalJacobian = true;
                fdemNL2solnw = new FitDoubleExponentialNoWholeNL2solModel(200, tValues, pIntensity, x,
                		           iv, v, useAnalyticalJacobian);
                fdemNL2solnw.driver();
                fdemNL2solnw.dumpResults();

                bottom = x[4];
                span = 1.0 - x[4];

                // The nonlinear fitting routine might interchange alpha and beta
                // If so, change back
                if (x[3] > x[2]) {
                    dTemp = x[2];
                    x[2] = x[3];
                    x[3] = dTemp;
                    x[1] = 1.0 - x[1];
                }

                ViewUserInterface.getReference().setDataText("In the recovery curve\n" +
                                                             "bottom + span*[1 - gamma*exp(alpha*t) - (1 - gamma)*exp(beta*t)]\n");
                dataString += "In the recovery curve\n" +
                              "bottom + span*[1 - gamma*exp(alpha*t) - (1 - gamma)*exp(beta*t)]\n";
                ViewUserInterface.getReference().setDataText("bottom = " + nf.format(bottom) + " span = " +
                                                             nf.format(span) + "\n" + "alpha = " +
                                                             nf.format(x[2]) + " beta = " + nf.format(x[3]) +
                                                             " gamma = " + nf.format(x[1]) + "\n");
                dataString += "bottom = " + nf.format(bottom) + " span = " + nf.format(span) + "\n" + "alpha = " +
                              nf.format(x[2]) + " beta = " + nf.format(x[3]) + " gamma = " +
                              nf.format(x[1]) + "\n";
                Preferences.debug("In the recovery curve\n" +
                                  "bottom + span*[1 - gamma*exp(alpha*t) - (1 - gamma)*exp(beta*t)]\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom = " + bottom + " span = " + span + "\n" + "alpha = " + x[2] +
                                  " beta = " + x[3] + " gamma = " + x[1] + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Mobile fraction = 1.0\n");
                dataString += "Mobile fraction = 1.0\n";
                Preferences.debug("Mobile fraction = 1.0\n", Preferences.DEBUG_ALGORITHM);

                // From equation (20) s1 = -(alpha + beta)/2, s2 = (alpha - beta)/2
                s1 = -(x[2] + x[3]) / 2.0;
                s2 = (x[2] - x[3]) / 2.0;

                // s1 = (D + ka + kd)/2
                // s2 = sqrt((D + ka + D)**2 - 4*D*kd)/2
                // gamma = (-D + ka + kd + 2*s2)/(4*s2)
                // s2 = sqrt(s1**2 - kdD)
                // kdD = s1**2 - s2**2
                kdD = (s1 * s1) - (s2 * s2);

                // D + ka + kd = 2s1
                // -D + ka + kd = params[0]*4*s2 - 2*s2;
                // 2*D = 2*s1 - params[0]*4*s2 + 2*s2;
                // D = s1 - 2*params[0]*s2 + s2
                Dt = s1 - (2.0 * x[1] * s2) + s2;
                kd = kdD / Dt;
                ka = (2.0 * s1) - kd - Dt;
                ViewUserInterface.getReference().setDataText("Association rate = " + nf.format(ka) + "\n");
                dataString += "Association rate = " + nf.format(ka) + "\n";
                ViewUserInterface.getReference().setDataText("Dissociation rate = " + nf.format(kd) + "\n");
                dataString += "Dissociation rate = " + nf.format(kd) + "\n";
                ViewUserInterface.getReference().setDataText("Diffusion transfer coefficient = " + nf.format(Dt) +
                                                             "\n\n");
                dataString += "Diffusion transfer coefficient = " + nf.format(Dt) + "\n\n";
                Preferences.debug("Association rate = " + ka + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Dissociation rate = " + kd + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Diffusion transfer coefficient = " + Dt + "\n\n", Preferences.DEBUG_ALGORITHM);

                ViewUserInterface.getReference().setDataText("ELSUNC fitting engine double exponential\n");
                Preferences.debug("ELSUNC fitting engine double exponential\n", Preferences.DEBUG_ALGORITHM);
                dataString += "ELSUNC fitting engine double exponential\n";
                fdemConstrainednw = new FitDoubleExponentialNoWholeConstrainedModel(200, tValues, pIntensity, initial);
                fdemConstrainednw.driver();
                fdemConstrainednw.dumpResults();

                params = fdemConstrainednw.getParameters();
                bottom = params[3];
                span = 1.0 - params[3];

                // The nonlinear fitting routine might interchange alpha and beta
                // If so, change back
                if (params[2] > params[1]) {
                    dTemp = params[1];
                    params[1] = params[2];
                    params[2] = dTemp;
                    params[0] = 1.0 - params[0];
                }

                ViewUserInterface.getReference().setDataText("In the recovery curve\n" +
                                                             "bottom + span*[1 - gamma*exp(alpha*t) - (1 - gamma)*exp(beta*t)]\n");
                dataString += "In the recovery curve\n" +
                              "bottom + span*[1 - gamma*exp(alpha*t) - (1 - gamma)*exp(beta*t)]\n";
                ViewUserInterface.getReference().setDataText("bottom = " + nf.format(bottom) + " span = " +
                                                             nf.format(span) + "\n" + "alpha = " +
                                                             nf.format(params[1]) + " beta = " + nf.format(params[2]) +
                                                             " gamma = " + nf.format(params[0]) + "\n");
                dataString += "bottom = " + nf.format(bottom) + " span = " + nf.format(span) + "\n" + "alpha = " +
                              nf.format(params[1]) + " beta = " + nf.format(params[2]) + " gamma = " +
                              nf.format(params[0]) + "\n";
                Preferences.debug("In the recovery curve\n" +
                                  "bottom + span*[1 - gamma*exp(alpha*t) - (1 - gamma)*exp(beta*t)]\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("bottom = " + bottom + " span = " + span + "\n" + "alpha = " + params[1] +
                                  " beta = " + params[2] + " gamma = " + params[0] + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Mobile fraction = 1.0\n");
                dataString += "Mobile fraction = 1.0\n";
                Preferences.debug("Mobile fraction = 1.0\n", Preferences.DEBUG_ALGORITHM);

                // From equation (20) s1 = -(alpha + beta)/2, s2 = (alpha - beta)/2
                s1 = -(params[1] + params[2]) / 2.0;
                s2 = (params[1] - params[2]) / 2.0;

                // s1 = (D + ka + kd)/2
                // s2 = sqrt((D + ka + D)**2 - 4*D*kd)/2
                // gamma = (-D + ka + kd + 2*s2)/(4*s2)
                // s2 = sqrt(s1**2 - kdD)
                // kdD = s1**2 - s2**2
                kdD = (s1 * s1) - (s2 * s2);

                // D + ka + kd = 2s1
                // -D + ka + kd = params[0]*4*s2 - 2*s2;
                // 2*D = 2*s1 - params[0]*4*s2 + 2*s2;
                // D = s1 - 2*params[0]*s2 + s2
                Dt = s1 - (2.0 * params[0] * s2) + s2;
                kd = kdD / Dt;
                ka = (2.0 * s1) - kd - Dt;
                ViewUserInterface.getReference().setDataText("Association rate = " + nf.format(ka) + "\n");
                dataString += "Association rate = " + nf.format(ka) + "\n";
                ViewUserInterface.getReference().setDataText("Dissociation rate = " + nf.format(kd) + "\n");
                dataString += "Dissociation rate = " + nf.format(kd) + "\n";
                ViewUserInterface.getReference().setDataText("Diffusion transfer coefficient = " + nf.format(Dt) +
                                                             "\n\n");
                dataString += "Diffusion transfer coefficient = " + nf.format(Dt) + "\n\n";
                Preferences.debug("Association rate = " + ka + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Dissociation rate = " + kd + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Diffusion transfer coefficient = " + Dt + "\n\n", Preferences.DEBUG_ALGORITHM);
            } // for (i = 0; i < 8; i++)

            file = new File("C:/images/Sample_results.txt");
            raFile = new RandomAccessFile(file, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
            raFile.write(dataString.getBytes());
            raFile.close();

        } // try
        catch (Exception e) {
            System.gc();
            MipavUtil.displayError("Exception " + e);
        }

    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    class Fit24DModel extends NLConstrainedEngine {
    	private double xData[];
    	private float yData[];

        /**
         * Creates a new Fit24DModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public Fit24DModel(int nPoints, double[] xData, float[] yData, double[] initial) {
        	

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3);
            this.xData = xData;
            this.yData = yData;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain alpha
            bl[0] = -1000.0;
            bu[0] = 1000.0;

            // Constrain beta
            bl[1] = -1000.0;
            bu[1] = 1000.0;

            // Constrain rho
            bl[2] = 0.0;
            bu[2] = 1.0;

            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;

            gues[0] = initial[0];
            gues[1] = initial[1];
            gues[2] = initial[2];
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitDoubleExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * Fit to function - a0 - a1*(a2**x).
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int i;
            double ymodel = 0.0;

            try {
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {

                    // evaluate the residuals[i] = ymodel[i] - yData[i]
                    for (i = 0; i < nPts; i++) {
                        ymodel = a[0] - (a[1] * Math.pow(a[2], xData[i]));
                        residuals[i] = ymodel - yData[i];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {

                    // Calculate the Jacobian analytically
                    for (i = 0; i < nPts; i++) {
                        covarMat[i][0] = 1.0;
                        covarMat[i][1] = -Math.pow(a[2], xData[i]);
                        covarMat[i][2] = -xData[i] * a[1] * Math.pow(a[2], xData[i] - 1.0);
                    }
                } // else if (ctrl == 2)
                // If the user wishes to calculate the Jacobian numerically
                /* else if (ctrl == 2) {
                 * ctrlMat[0] = 0; } */
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
    }

    /**
     * DOCUMENT ME!
     */
    class FitDoubleExponentialModel extends NLConstrainedEngine {
        private double xData[];
        private float yData[];
        /**
         * Creates a new FitDoubleExponentialModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitDoubleExponentialModel(int nPoints, double[] xData, float[] yData, double[] initial) {

            // nPoints data points, 5 coefficients, and exponential fitting
            super(nPoints, 5);
            this.xData = xData;
            this.yData = yData;
            
            bounds = 0; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            
            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;
            
            gues[0] = initial[0];
            gues[1] = initial[1];
            gues[2] = initial[2];
            gues[3] = initial[3];
            gues[4] = initial[4];

        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitDoubleExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);

            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a3 " + String.valueOf(a[3]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a4 " + String.valueOf(a[4]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /** 
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j;
            double ymod = 0;

            try {
                ctrl = ctrlMat[0];
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n", 
                //Preferences.DEBUG_ALGORITHM);
                Preferences.debug("a[3] = " + a[3] + " a[4] = " + a[4] + "\n", Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                    	ymod = a[3] + (a[4] * (1 - (a[0] * Math.exp(a[1] * xData[j])) -
                                     ((1 - a[0]) * Math.exp(a[2] * xData[j]))));
                        residuals[j] = ymod - yData[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                    	covarMat[j][0] = a[4]*(-Math.exp(a[1]*xData[j]) + Math.exp(a[2]*xData[j]));
                    	covarMat[j][1] = -a[4]*a[0]*xData[j]*Math.exp(a[1] * xData[j]);
                    	covarMat[j][2] = -a[4]*(1 - a[0])*xData[j]*Math.exp(a[2] * xData[j]);
                    	covarMat[j][3] = 1.0;
                    	covarMat[j][4] = 1 - a[0]*Math.exp(a[1]*xData[j])  - (1 -a[0])*Math.exp(a[2]*xData[j]);
                    }
                }
                // Calculate the Jacobian numerically
                // else if (ctrl == 2) {
                // ctrlMat[0] = 0;
                // }
            } catch (final Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }

        /**
         * Fit to function - a3 + a4*[1 - ao*exp(a1*x) - (1 - a0)*exp(a2*x)].
         *
         * @param   x1    The x value of the data point.
         * @param   atry  The best guess parameter values.
         *
         * @return  The calculated y value.
         */
        public double[] fitToFunction(double[] x1, double[] atry /*, double dyda[]*/) {

            // mrqcof calls function
            // mrqcof supplies x1 and best guess parameters atry[]
            // function returns the partial derivatives dyda and the calculated ymod
            double[] ymod = new double[x1.length];
            int i;

            try {

                for (i = 0; i < x1.length; i++) {
                    ymod[i] = atry[3] +
                              (atry[4] *
                                   (1 - (atry[0] * Math.exp(atry[1] * x1[i])) -
                                        ((1 - atry[0]) * Math.exp(atry[2] * x1[i]))));
                    // a0 partial derivative dyda[0] = atry[4]*(-Math.exp(atry[1]*x1) + Math.exp(atry[2]*x1)); a1
                    // partial derivative dyda[1] = -atry[4]*atry[0]*x1*Math.exp(atry[1] * x1); a2 partial derivative
                    // dyda[2] = -atry[4]*(1 - atry[0])*x1*Math.exp(atry[2] * x1); a3 partial derivative dyda[3] = 1; a4
                    // partial derivative dyda[4] = 1 - atry[0]*Math.exp(atry[1]*x1)             - (1 -
                    // atry[0])*Math.exp(atry[2]*x1);
                }
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return ymod;
        }
    }


    /**
     * DOCUMENT ME!
     */
    class FitDoubleExponentialNoWholeConstrainedModel extends NLConstrainedEngine {
        private double xData[];
        private float yData[];
        /**
         * Creates a new FitDoubleExponentialNoWholeConstrainedModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitDoubleExponentialNoWholeConstrainedModel(int nPoints, double[] xData, float[] yData,
                                                           double[] initial) {

            // nPoints data points, 4 coefficients, and exponential fitting
            super(nPoints, 4);
            this.xData = xData;
            this.yData = yData;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain gamma to be between 0.0 and 1.0
            bl[0] = 0.0;
            bu[0] = 1.0;

            // Constrain alpha to be negative
            bl[1] = -Double.MAX_VALUE;
            bu[1] = -Double.MIN_VALUE;

            // Constrain beta to be negative
            bl[2] = -Double.MAX_VALUE;
            bu[2] = -Double.MIN_VALUE;

            // Constrain bottom to be between 0.0 and 1.0
            bl[3] = 0.0;
            bu[3] = 1.0;

            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;

            gues[0] = initial[0];
            gues[1] = initial[1];
            gues[2] = initial[2];
            gues[3] = initial[3];
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitDoubleExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a3 " + String.valueOf(a[3]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * Fit to function - a3 + (1-a3)*[1 - ao*exp(a1*x) - (1 - a0)*exp(a2*x)].
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int i;
            double ymodel = 0.0;
            double e1, e2;

            try {
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {

                    // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                    for (i = 0; i < nPts; i++) {
                        ymodel = a[3] +
                                 ((1.0 - a[3]) *
                                      (1 - (a[0] * Math.exp(a[1] * xData[i])) -
                                           ((1.0 - a[0]) * Math.exp(a[2] * xData[i]))));
                        residuals[i] = ymodel - yData[i];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {

                    // Calculate the Jacobian analytically
                    for (i = 0; i < nPts; i++) {
                        e1 = Math.exp(a[1] * xData[i]);
                        e2 = Math.exp(a[2] * xData[i]);
                        covarMat[i][0] = (1.0 - a[3]) * (-e1 + e2);
                        covarMat[i][1] = -(1.0 - a[3]) * a[0] * xData[i] * e1;
                        covarMat[i][2] = -(1.0 - a[3]) * (1.0 - a[0]) * xData[i] * e2;
                        covarMat[i][3] = (a[0] * e1) + ((1.0 - a[0]) * e2);
                    }
                } // else if (ctrl == 2)
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
    }
    
    /**
     * DOCUMENT ME!
     */
    class FitDoubleExponentialNoWholeNL2solModel extends NL2sol {
    	int iv[];
    	double v[];
    	double x[];
    	double xData[];
    	float yData[];
    	
    	/**
         * Creates a new FitDoubleExponentialNoWholeNL2solModel object.
         * 
         * @param nPoints DOCUMENT ME!
         * @param xData
         * @param yData DOCUMENT ME!
         * @param x DOCUMENT ME!
         * @param iv
         * @param v
         * @param useAnalyticJacobian
         */
        public FitDoubleExponentialNoWholeNL2solModel(final int nPoints, double[] xData, float[] yData,
                double[] x, int iv[], double v[], boolean useAnalyticJacobian) {
            
            // nPoints data points
        	// 4 coefficients
        	// x[] is a length 5 initial guess at input and best estimate at output
        	// data starts at x[1]
        	// iv[] has length 61 + number of coefficients = 65
        	// v[] has length at least 94 + n*p + 3*n + p*(3*p+33)/2
        	// uiparm, integer parameter array = null
        	// urparm, double parameter array = null
            super(nPoints, 4, x, iv, v, useAnalyticJacobian, null, null);
            this.x = x;
            this.iv = iv;
            this.v = v;
            this.xData = xData;
            this.yData = yData;
        
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitDoubleExponentialNoWholeNL2solModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iv[31]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(2.0 * v[10]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(x[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(x[2]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(x[3]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a3 " + String.valueOf(x[4]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        public void calcr(final int meqn, final int nvar, final double x[], final int nf, final double r[],
                final int uiparm[], final double urparm[]) {
        	double ymod;
            int j;
            
        	
            // evaluate the residuals[j] = ymodel[j] - ySeries[j]
            for (j = 0; j < meqn; j++) {
            	ymod = x[4] + ((1 - x[4]) * (1 - (x[1] * Math.exp(x[2] * xData[j])) -
                          ((1 - x[1]) * Math.exp(x[3] * xData[j]))));
                r[j+1] = ymod - yData[j];
                // Preferences.debug("residuals["+ (j+1) + "] = " + r[j+1] + "\n", Preferences.DEBUG_ALGORITHM);
            }

        }
        
        public void calcj(final int meqn, final int nvar, final double x[], final int nf, final double jac[][],
                final int uiparm[], final double urparm[]) {
	        int j;
	        for (j = 0; j < meqn; j++) {
	        	jac[j+1][1] = (1.0 - x[4])*(-Math.exp(x[2]*xData[j]) + Math.exp(x[3]*xData[j]));
	        	jac[j+1][2] = -(1.0 - x[4])*x[1]*xData[j]*Math.exp(x[2] * xData[j]);
	        	jac[j+1][3] = -(1.0 - x[4])*(1 - x[1])*xData[j]*Math.exp(x[3] * xData[j]);
	        	jac[j+1][4] = x[1]*Math.exp(x[2]*xData[j]) + (1 - x[1])*Math.exp(x[3]*xData[j]);
	        }
        }

    }



    /**
     * DOCUMENT ME!
     */
    class FitDoubleExponentialNoWholeModel extends NLConstrainedEngine {
    	private double xData[];
    	private float yData[];

        /**
         * Creates a new FitDoubleExponentialNoWholeModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitDoubleExponentialNoWholeModel(int nPoints, double[] xData, float[] yData, double[] initial) {

            // nPoints data points, 4 coefficients, and exponential fitting
            super(nPoints, 4);
            this.xData = xData;
            this.yData = yData;
            
            bounds = 0; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            
            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;

            gues[0] = initial[0];
            gues[1] = initial[1];
            gues[2] = initial[2];
            gues[3] = initial[3];
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitDoubleExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a3 " + String.valueOf(a[3]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /**
         * Fit to function - Fit to function - a3 + (1-a3)*[1 - ao*exp(a1*x) - (1 - a0)*exp(a2*x)].
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int i;
            double ymodel = 0.0;
            double e1, e2;

            try {
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {

                    // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                    for (i = 0; i < nPts; i++) {
                        ymodel = a[3] +
                                 ((1.0 - a[3]) *
                                      (1 - (a[0] * Math.exp(a[1] * xData[i])) -
                                           ((1.0 - a[0]) * Math.exp(a[2] * xData[i]))));
                        residuals[i] = ymodel - yData[i];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {

                    // Calculate the Jacobian analytically
                    for (i = 0; i < nPts; i++) {
                        e1 = Math.exp(a[1] * xData[i]);
                        e2 = Math.exp(a[2] * xData[i]);
                        covarMat[i][0] = (1.0 - a[3]) * (-e1 + e2);
                        covarMat[i][1] = -(1.0 - a[3]) * a[0] * xData[i] * e1;
                        covarMat[i][2] = -(1.0 - a[3]) * (1.0 - a[0]) * xData[i] * e2;
                        covarMat[i][3] = (a[0] * e1) + ((1.0 - a[0]) * e2);
                    }
                } // else if (ctrl == 2)
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
    }



    


    /**
     * DOCUMENT ME!
     */
    class FitExpModel extends InverseLaplace {

        /**
         * Creates a new FitExpModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitExpModel(double[] time, double largestPole, double tol) {
            super(time, largestPole, tol);
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {

            // The Laplace transform of exp(at) = 1/(p-a)
            double[][] ans = new double[imagS.length][2];
            double a = 1.0;
            int i;
            double denom;
            double realDenom;
            realDenom = (realS - a) * (realS - a);

            for (i = 0; i < imagS.length; i++) {
                denom = realDenom + (imagS[i] * imagS[i]);

                // real part
                ans[i][0] = (realS - a) / denom;
                ans[i][1] = -imagS[i] / denom;
            }

            return ans;
        }

    }

    /**
     * DOCUMENT ME!
     */
    class FitExpModelqd extends InverseLaplaceqd {

        /**
         * Creates a new FitExpModelqd object.
         *
         * @param  timePoints           DOCUMENT ME!
         * @param  endTime              DOCUMENT ME!
         * @param  largestPole          DOCUMENT ME!
         * @param  tol                  DOCUMENT ME!
         * @param  matrixSizeParameter  DOCUMENT ME!
         */
        public FitExpModelqd(int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter) {
            super(timePoints, endTime, largestPole, tol, matrixSizeParameter);
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {

            // The Laplace transform of exp(at) = 1/(p-a)
            double[][] ans = new double[imagS.length][2];
            double a = 1.0;
            int i;
            double denom;
            double realDenom;
            realDenom = (realS - a) * (realS - a);

            for (i = 0; i < imagS.length; i++) {
                denom = realDenom + (imagS[i] * imagS[i]);

                // real part
                ans[i][0] = (realS - a) / denom;
                ans[i][1] = -imagS[i] / denom;
            }

            return ans;
        }

    }

  

    /**
     * DOCUMENT ME!
     */
    class FitFullIntModel implements RealFunctionOfOneVariable {

        /** DOCUMENT ME! */
        double arg;

        /** DOCUMENT ME! */
        double[] cyi = new double[1];

        /** DOCUMENT ME! */
        double[] cyr = new double[1];

        /** DOCUMENT ME! */
        int[] errorFlag = new int[1];

        /** DOCUMENT ME! */
        double feq;

        /** DOCUMENT ME! */
        double initialOrder = 1.0;

        /** DOCUMENT ME! */
        double koff;

        /** DOCUMENT ME! */
        double kon;

        /** DOCUMENT ME! */
        double mk;

        /** DOCUMENT ME! */
        Bessel modelBessel;

        /** DOCUMENT ME! */
        int[] nz = new int[1];

        /** DOCUMENT ME! */
        int sequenceNumber = 1;

        /** DOCUMENT ME! */
        double time;

        /**
         * Creates a new FitFullIntModel object.
         *
         * @param  time     DOCUMENT ME!
         * @param  kon      DOCUMENT ME!
         */
        public FitFullIntModel(double time, double kon, double koff) {
            this.time = time;
            this.kon = kon;
            this.koff = koff;
        }


        /**
         * DOCUMENT ME!
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double intFunc(double x) {
            double xkoff;
            double mk;
            double arg;
            double[] cyr = new double[1];
            double[] cyi = new double[1];
            int[] nz = new int[1];
            int[] errorFlag = new int[1];
            Bessel modelBessel;
            double initialOrder = 1.0;
            int sequenceNumber = 1;
            double feq;
            double i2;

            feq = koff / (kon + koff);
            xkoff = -x + koff;
            mk = 1.0 + (kon / xkoff);
            arg = radius * Math.sqrt(x * mk / diffusion);
            modelBessel = new Bessel(Bessel.BESSEL_J, arg, 0.0, initialOrder, Bessel.UNSCALED_FUNCTION, sequenceNumber,
                                     cyr, cyi, nz, errorFlag);

            modelBessel.run();
            i2 = cyr[0] * cyr[0];

            return feq * Math.exp(-x * time) * i2 * mk / x;
        }

		@Override
		public double eval(double x) {
			return intFunc(x);
		}

    }


    /**
     * DOCUMENT ME!
     */
    class FitFullIntModel2i extends Integration2 {

        /** DOCUMENT ME! */
        double arg;

        /** DOCUMENT ME! */
        double[] cyi = new double[1];

        /** DOCUMENT ME! */
        double[] cyr = new double[1];

        /** DOCUMENT ME! */
        int[] errorFlag = new int[1];

        /** DOCUMENT ME! */
        double feq;

        /** DOCUMENT ME! */
        double initialOrder = 1.0;

        /** DOCUMENT ME! */
        double koff;

        /** DOCUMENT ME! */
        double kon;

        /** DOCUMENT ME! */
        double mk;

        /** DOCUMENT ME! */
        Bessel modelBessel;

        /** DOCUMENT ME! */
        int[] nz = new int[1];

        /** DOCUMENT ME! */
        int sequenceNumber = 1;

        /** DOCUMENT ME! */
        double time;

        /**
         * Creates a new FitFullIntModel2i object.
         *
         * @param  time     DOCUMENT ME!
         * @param  kon      DOCUMENT ME!
         * @param  koff     DOCUMENT ME!
         * @param  bound    DOCUMENT ME!
         * @param  routine  DOCUMENT ME!
         * @param  inf      DOCUMENT ME!
         * @param  epsabs   DOCUMENT ME!
         * @param  epsrel   DOCUMENT ME!
         * @param  limit    DOCUMENT ME!
         */
        public FitFullIntModel2i(double time, double kon, double koff, double bound, int routine, int inf,
                                 double epsabs, double epsrel, int limit) {
            super(bound, routine, inf, epsabs, epsrel, limit);
            this.time = time;
            this.kon = kon;
            this.koff = koff;
        }

        /**
         * DOCUMENT ME!
         */
        public void driver() {
            super.driver();
        }

        /**
         * DOCUMENT ME!
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double intFunc(double x) {
            double xkoff;
            double mk;
            double arg;
            double[] cyr = new double[1];
            double[] cyi = new double[1];
            int[] nz = new int[1];
            int[] errorFlag = new int[1];
            Bessel modelBessel;
            double initialOrder = 1.0;
            int sequenceNumber = 1;
            double feq;
            double i2;

            feq = koff / (kon + koff);
            xkoff = -x + koff;
            mk = 1.0 + (kon / xkoff);
            arg = radius * Math.sqrt(x * mk / diffusion);
            modelBessel = new Bessel(Bessel.BESSEL_J, arg, 0.0, initialOrder, Bessel.UNSCALED_FUNCTION, sequenceNumber,
                                     cyr, cyi, nz, errorFlag);

            modelBessel.run();
            i2 = cyr[0] * cyr[0];

            return feq * Math.exp(-x * time) * i2 * mk / x;
        }

    }

    /**
     * DOCUMENT ME!
     */
    class FitFullIntModel2p extends Integration2 {

        /** DOCUMENT ME! */
        double arg;

        /** DOCUMENT ME! */
        double[] cyi = new double[1];

        /** DOCUMENT ME! */
        double[] cyr = new double[1];

        /** DOCUMENT ME! */
        int[] errorFlag = new int[1];

        /** DOCUMENT ME! */
        double feq;

        /** DOCUMENT ME! */
        double initialOrder = 1.0;

        /** DOCUMENT ME! */
        double koff;

        /** DOCUMENT ME! */
        double kon;

        /** DOCUMENT ME! */
        double mk;

        /** DOCUMENT ME! */
        Bessel modelBessel;

        /** DOCUMENT ME! */
        int[] nz = new int[1];

        /** DOCUMENT ME! */
        int sequenceNumber = 1;

        /** DOCUMENT ME! */
        double time;

        /**
         * Creates a new FitFullIntModel2p object.
         *
         * @param  time         DOCUMENT ME!
         * @param  kon          DOCUMENT ME!
         * @param  koff         DOCUMENT ME!
         * @param  lower        DOCUMENT ME!
         * @param  upper        DOCUMENT ME!
         * @param  routine      DOCUMENT ME!
         * @param  breakPoints  DOCUMENT ME!
         * @param  epsabs       DOCUMENT ME!
         * @param  epsrel       DOCUMENT ME!
         * @param  limit        DOCUMENT ME!
         */
        public FitFullIntModel2p(double time, double kon, double koff, double lower, double upper, int routine,
                                 double[] breakPoints, double epsabs, double epsrel, int limit) {
            super(lower, upper, routine, breakPoints, epsabs, epsrel, limit);
            this.time = time;
            this.kon = kon;
            this.koff = koff;
        }

        /**
         * DOCUMENT ME!
         */
        public void driver() {
            super.driver();
        }

        /**
         * DOCUMENT ME!
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double intFunc(double x) {
            double xkoff;
            double mk;
            double arg;
            double[] cyr = new double[1];
            double[] cyi = new double[1];
            int[] nz = new int[1];
            int[] errorFlag = new int[1];
            Bessel modelBessel;
            double initialOrder = 1.0;
            int sequenceNumber = 1;
            double feq;
            double i2;

            feq = koff / (kon + koff);
            xkoff = -x + koff;
            mk = 1.0 + (kon / xkoff);
            arg = radius * Math.sqrt(x * mk / diffusion);
            modelBessel = new Bessel(Bessel.BESSEL_J, arg, 0.0, initialOrder, Bessel.UNSCALED_FUNCTION, sequenceNumber,
                                     cyr, cyi, nz, errorFlag);

            modelBessel.run();
            i2 = cyr[0] * cyr[0];

            return feq * Math.exp(-x * time) * i2 * mk / x;
        }

    }

    /**
     * DOCUMENT ME!
     */
    class FitFullIntModel2s extends Integration2 {

        /** DOCUMENT ME! */
        double arg;

        /** DOCUMENT ME! */
        double[] cyi = new double[1];

        /** DOCUMENT ME! */
        double[] cyr = new double[1];

        /** DOCUMENT ME! */
        int[] errorFlag = new int[1];

        /** DOCUMENT ME! */
        double feq;

        /** DOCUMENT ME! */
        double initialOrder = 1.0;

        /** DOCUMENT ME! */
        double koff;

        /** DOCUMENT ME! */
        double kon;

        /** DOCUMENT ME! */
        double mk;

        /** DOCUMENT ME! */
        Bessel modelBessel;

        /** DOCUMENT ME! */
        int[] nz = new int[1];

        /** DOCUMENT ME! */
        int sequenceNumber = 1;

        /** DOCUMENT ME! */
        double time;

        /**
         * Creates a new FitFullIntModel2s object.
         *
         * @param  time     DOCUMENT ME!
         * @param  kon      DOCUMENT ME!
         * @param  koff     DOCUMENT ME!
         * @param  lower    DOCUMENT ME!
         * @param  upper    DOCUMENT ME!
         * @param  routine  DOCUMENT ME!
         * @param  epsabs   DOCUMENT ME!
         * @param  epsrel   DOCUMENT ME!
         * @param  limit    DOCUMENT ME!
         */
        public FitFullIntModel2s(double time, double kon, double koff, double lower, double upper, int routine,
                                 double epsabs, double epsrel, int limit) {
            super(lower, upper, routine, epsabs, epsrel, limit);
            this.time = time;
            this.kon = kon;
            this.koff = koff;
        }

        /**
         * DOCUMENT ME!
         */
        public void driver() {
            super.driver();
        }

        /**
         * DOCUMENT ME!
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double intFunc(double x) {
            double xkoff;
            double mk;
            double arg;
            double[] cyr = new double[1];
            double[] cyi = new double[1];
            int[] nz = new int[1];
            int[] errorFlag = new int[1];
            Bessel modelBessel;
            double initialOrder = 1.0;
            int sequenceNumber = 1;
            double feq;
            double i2;

            feq = koff / (kon + koff);
            xkoff = -x + koff;
            mk = 1.0 + (kon / xkoff);
            arg = radius * Math.sqrt(x * mk / diffusion);
            modelBessel = new Bessel(Bessel.BESSEL_J, arg, 0.0, initialOrder, Bessel.UNSCALED_FUNCTION, sequenceNumber,
                                     cyr, cyi, nz, errorFlag);

            modelBessel.run();
            i2 = cyr[0] * cyr[0];

            return feq * Math.exp(-x * time) * i2 * mk / x;
        }

    }


    /**
     * DOCUMENT ME!
     */
    class FitFullModel extends InverseLaplace {

        /** DOCUMENT ME! */
        double koff;

        /** DOCUMENT ME! */
        double kon;

        /**
         * Creates a new FitFullModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         * @param  kon          DOCUMENT ME!
         * @param  koff         DOCUMENT ME!
         */
        public FitFullModel(double[] time, double largestPole, double tol, double kon, double koff) {
            super(time, largestPole, tol);
            this.kon = kon;
            this.koff = koff;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {

            // Port of lelpa_m5_web.m
            // Laplace transform for scaled total fluorescence
            // Local equilibrium model
            double[][] ans = new double[imagS.length][2];
            int i;
            double feq;
            double denom;
            double partDenom;
            double realInv;
            double imagInv;
            double realGroup;
            double imagGroup;
            double realSet;
            double imagSet;
            double realBunch;
            double imagBunch;
            double realQsq;
            double imagQsq;
            double realSq;
            double imagSq;
            double realVar;
            double imagVar;
            double[] realQ = new double[1];
            double[] imagQ = new double[1];
            double[] cyr = new double[1];
            double[] cyi = new double[1];
            int[] nz = new int[1];
            int[] errorFlag = new int[1];
            Bessel modelBessel;
            double realBk;
            double imagBk;
            double realBi;
            double imagBi;
            double initialOrder = 1.0;
            int sequenceNumber = 1;


            partDenom = (realS + koff) * (realS + koff);
            realSq = realS * realS;
            feq = koff / (kon + koff);

            for (i = 0; i < imagS.length; i++) {
                imagSq = imagS[i] * imagS[i];
                denom = partDenom + imagSq;

                // 1/(p + koff)
                realInv = (realS + koff) / denom;
                imagInv = -imagS[i] / denom;

                // (1 + kon/(p + koff))
                realGroup = 1.0 + (kon * realInv);
                imagGroup = kon * imagInv;

                // q*q
                realQsq = (realS * realGroup / diffusion) - (imagS[i] * imagGroup / diffusion);
                imagQsq = (realS * imagGroup / diffusion) + (imagS[i] * realGroup / diffusion);
                zsqrt(realQsq, imagQsq, realQ, imagQ);
                modelBessel = new Bessel(Bessel.BESSEL_K, realQ[0] * radius, imagQ[0] * radius, initialOrder,
                                         Bessel.UNSCALED_FUNCTION, sequenceNumber, cyr, cyi, nz, errorFlag);
                modelBessel.run();
                realBk = cyr[0];
                imagBk = cyi[0];
                modelBessel = new Bessel(Bessel.BESSEL_I, realQ[0] * radius, imagQ[0] * radius, initialOrder,
                                         Bessel.UNSCALED_FUNCTION, sequenceNumber, cyr, cyi, nz, errorFlag);
                modelBessel.run();
                realBi = cyr[0];
                imagBi = cyi[0];

                // 2.0*K1(q*radius)*I1(q*radius))
                realSet = (2.0 * realBk * realBi) - (2.0 * imagBk * imagBi);
                imagSet = (2.0 * realBk * imagBi) + (2.0 * imagBk * realBi);

                // Feq/p
                denom = realSq + imagSq;
                realVar = feq * realS / denom;
                imagVar = -feq * imagS[i] / denom;

                // (Feq/p)*2.0*K1(q*radius)*I1(q*radius))
                realBunch = (realSet * realVar) - (imagSet * imagVar);
                imagBunch = (realSet * imagVar) + (imagSet * realVar);

                // (Feq/p)*2.0*K1(q*radius)*I1(q*radius)*(1 + kon/(p+koff))
                ans[i][0] = (realBunch * realGroup) - (imagBunch * imagGroup);
                ans[i][1] = (realBunch * imagGroup) + (imagBunch * realGroup);
            } // for (i = 0; i < imagS.length; i++)

            return ans;
        }

        /**
         * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
         *
         * @param   zr  double
         * @param   zi  double
         *
         * @return  double
         */
        private double zabs(double zr, double zi) {
            double u, v, q, s;
            u = Math.abs(zr);
            v = Math.abs(zi);
            s = u + v;

            // s * 1.0 makes an unnormalized underflow on CDC machines into a true
            // floating zero
            s = s * 1.0;

            if (s == 0.0) {
                return 0.0;
            } else if (u > v) {
                q = v / u;

                return (u * Math.sqrt(1.0 + (q * q)));
            } else {
                q = u / v;

                return (v * Math.sqrt(1.0 + (q * q)));
            }
        }

        /**
         * complex square root b = csqrt(a).
         *
         * @param  ar  double
         * @param  ai  double
         * @param  br  double[]
         * @param  bi  double[]
         */
        private void zsqrt(double ar, double ai, double[] br, double[] bi) {
            double drt = 1.0 / Math.sqrt(2.0);
            double zm;
            double theta;

            zm = zabs(ar, ai);
            zm = Math.sqrt(zm);

            if (ar == 0.0) {

                if (ai == 0.0) {
                    br[0] = 0.0;
                    bi[0] = 0.0;

                    return;
                } // if (ai == 0.0)
                else if (ai > 0.0) {
                    br[0] = zm * drt;
                    bi[0] = zm * drt;

                    return;
                } // else if (ai > 0.0)
                else { // ai < 0.0
                    br[0] = zm * drt;
                    bi[0] = -zm * drt;

                    return;
                } // else ai < 0.0
            } // if (ar == 0.0)
            else if (ai == 0.0) {

                if (ar > 0.0) {
                    br[0] = Math.sqrt(ar);
                    bi[0] = 0.0;

                    return;
                } // if (ar > 0.0)
                else { // ar < 0.0
                    br[0] = 0.0;
                    bi[0] = Math.sqrt(Math.abs(ar));

                    return;
                } // ar < 0.0
            } // else if (ai == 0.0)

            theta = Math.atan(ai / ar);

            if (theta <= 0.0) {

                if (ar < 0.0) {
                    theta = theta + Math.PI;
                }
            } else if (ar < 0.0) {
                theta = theta - Math.PI;
            }

            theta = 0.5 * theta;
            br[0] = zm * Math.cos(theta);
            bi[0] = zm * Math.sin(theta);

            return;
        }

    }


    /**
     * DOCUMENT ME!
     */
    class FitFullModel2 extends InverseLaplace2 {

        /** DOCUMENT ME! */
        double koff;

        /** DOCUMENT ME! */
        double kon;

        /**
         * Creates a new FitFullModel2 object.
         *
         * @param  time         DOCUMENT ME!
         * @param  abscissa     DOCUMENT ME!
         * @param  relEps       DOCUMENT ME!
         * @param  absEps       DOCUMENT ME!
         * @param  result       DOCUMENT ME!
         * @param  estErr       DOCUMENT ME!
         * @param  evaluations  DOCUMENT ME!
         * @param  errStatus    DOCUMENT ME!
         * @param  kon          DOCUMENT ME!
         * @param  koff         DOCUMENT ME!
         */
        public FitFullModel2(double[] time, double abscissa, double relEps, double absEps, double[] result,
                             double[] estErr, int[] evaluations, int[] errStatus, double kon, double koff) {
            super(time, abscissa, relEps, absEps, result, estErr, evaluations, errStatus);
            this.kon = kon;
            this.koff = koff;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  realIn   DOCUMENT ME!
         * @param  imagIn   DOCUMENT ME!
         * @param  realOut  DOCUMENT ME!
         * @param  imagOut  DOCUMENT ME!
         */
        public void fitToLaplace(double realIn, double imagIn, double[] realOut, double[] imagOut) {

            // Port of lelpa_m5_web.m
            // Laplace transform for scaled total fluorescence
            // Local equilibrium model
            double feq;
            double denom;
            double partDenom;
            double realInv;
            double imagInv;
            double realGroup;
            double imagGroup;
            double realSet;
            double imagSet;
            double realBunch;
            double imagBunch;
            double realQsq;
            double imagQsq;
            double realSq;
            double imagSq;
            double realVar;
            double imagVar;
            double[] realQ = new double[1];
            double[] imagQ = new double[1];
            double[] cyr = new double[1];
            double[] cyi = new double[1];
            int[] nz = new int[1];
            int[] errorFlag = new int[1];
            Bessel modelBessel;
            double realBk;
            double imagBk;
            double realBi;
            double imagBi;
            int initialOrder = 1;
            int sequenceNumber = 1;


            partDenom = (realIn + koff) * (realIn + koff);
            realSq = realIn * realIn;
            feq = koff / (kon + koff);
            imagSq = imagIn * imagIn;
            denom = partDenom + imagSq;

            // 1/(p + koff)
            realInv = (realIn + koff) / denom;
            imagInv = -imagIn / denom;

            // (1 + kon/(p + koff))
            realGroup = 1.0 + (kon * realInv);
            imagGroup = kon * imagInv;

            // q*q
            realQsq = (realIn * realGroup / diffusion) - (imagIn * imagGroup / diffusion);
            imagQsq = (realIn * imagGroup / diffusion) + (imagIn * realGroup / diffusion);
            zsqrt(realQsq, imagQsq, realQ, imagQ);
            modelBessel = new Bessel(Bessel.BESSEL_K, realQ[0] * radius, imagQ[0] * radius, initialOrder,
                                     Bessel.UNSCALED_FUNCTION, sequenceNumber, cyr, cyi, nz, errorFlag);
            modelBessel.run();
            realBk = cyr[0];
            imagBk = cyi[0];
            modelBessel = new Bessel(Bessel.BESSEL_I, realQ[0] * radius, imagQ[0] * radius, initialOrder,
                                     Bessel.UNSCALED_FUNCTION, sequenceNumber, cyr, cyi, nz, errorFlag);
            modelBessel.run();
            realBi = cyr[0];
            imagBi = cyi[0];

            // 2.0*K1(q*radius)*I1(q*radius))
            realSet = (2.0 * realBk * realBi) - (2.0 * imagBk * imagBi);
            imagSet = (2.0 * realBk * imagBi) + (2.0 * imagBk * realBi);

            // Feq/p
            denom = realSq + imagSq;
            realVar = feq * realIn / denom;
            imagVar = -feq * imagIn / denom;

            // (Feq/p)*2.0*K1(q*radius)*I1(q*radius))
            realBunch = (realSet * realVar) - (imagSet * imagVar);
            imagBunch = (realSet * imagVar) + (imagSet * realVar);

            // (Feq/p)*2.0*K1(q*radius)*I1(q*radius)*(1 + kon/(p+koff))
            realOut[0] = (realBunch * realGroup) - (imagBunch * imagGroup);
            imagOut[0] = (realBunch * imagGroup) + (imagBunch * realGroup);

            return;
        }

        /**
         * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
         *
         * @param   zr  double
         * @param   zi  double
         *
         * @return  double
         */
        private double zabs(double zr, double zi) {
            double u, v, q, s;
            u = Math.abs(zr);
            v = Math.abs(zi);
            s = u + v;

            // s * 1.0 makes an unnormalized underflow on CDC machines into a true
            // floating zero
            s = s * 1.0;

            if (s == 0.0) {
                return 0.0;
            } else if (u > v) {
                q = v / u;

                return (u * Math.sqrt(1.0 + (q * q)));
            } else {
                q = u / v;

                return (v * Math.sqrt(1.0 + (q * q)));
            }
        }

        /**
         * complex square root b = csqrt(a).
         *
         * @param  ar  double
         * @param  ai  double
         * @param  br  double[]
         * @param  bi  double[]
         */
        private void zsqrt(double ar, double ai, double[] br, double[] bi) {
            double drt = 1.0 / Math.sqrt(2.0);
            double zm;
            double theta;

            zm = zabs(ar, ai);
            zm = Math.sqrt(zm);

            if (ar == 0.0) {

                if (ai == 0.0) {
                    br[0] = 0.0;
                    bi[0] = 0.0;

                    return;
                } // if (ai == 0.0)
                else if (ai > 0.0) {
                    br[0] = zm * drt;
                    bi[0] = zm * drt;

                    return;
                } // else if (ai > 0.0)
                else { // ai < 0.0
                    br[0] = zm * drt;
                    bi[0] = -zm * drt;

                    return;
                } // else ai < 0.0
            } // if (ar == 0.0)
            else if (ai == 0.0) {

                if (ar > 0.0) {
                    br[0] = Math.sqrt(ar);
                    bi[0] = 0.0;

                    return;
                } // if (ar > 0.0)
                else { // ar < 0.0
                    br[0] = 0.0;
                    bi[0] = Math.sqrt(Math.abs(ar));

                    return;
                } // ar < 0.0
            } // else if (ai == 0.0)

            theta = Math.atan(ai / ar);

            if (theta <= 0.0) {

                if (ar < 0.0) {
                    theta = theta + Math.PI;
                }
            } else if (ar < 0.0) {
                theta = theta - Math.PI;
            }

            theta = 0.5 * theta;
            br[0] = zm * Math.cos(theta);
            bi[0] = zm * Math.sin(theta);

            return;
        }


    }

    /**
     * DOCUMENT ME!
     */
    class FitFullModelqd extends InverseLaplaceqd {

        /** DOCUMENT ME! */
        double koff;

        /** DOCUMENT ME! */
        double kon;

        /**
         * Creates a new FitFullModelqd object.
         *
         * @param  time  DOCUMENT ME!
         * @param  kon   DOCUMENT ME!
         * @param  koff  DOCUMENT ME!
         */
        public FitFullModelqd(double[] time, double kon, double koff) {
            super(time.length, time[time.length - 1]);
            this.kon = kon;
            this.koff = koff;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   realS  DOCUMENT ME!
         * @param   imagS  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[][] fitToLaplace(double realS, double[] imagS) {

            // Port of lelpa_m5_web.m
            // Laplace transform for scaled total fluorescence
            // Local equilibrium model
            double[][] ans = new double[imagS.length][2];
            int i;
            double feq;
            double denom;
            double partDenom;
            double realInv;
            double imagInv;
            double realGroup;
            double imagGroup;
            double realSet;
            double imagSet;
            double realBunch;
            double imagBunch;
            double realQsq;
            double imagQsq;
            double realSq;
            double imagSq;
            double realVar;
            double imagVar;
            double[] realQ = new double[1];
            double[] imagQ = new double[1];
            double[] cyr = new double[1];
            double[] cyi = new double[1];
            int[] nz = new int[1];
            int[] errorFlag = new int[1];
            Bessel modelBessel;
            double realBk;
            double imagBk;
            double realBi;
            double imagBi;
            double initialOrder = 1.0;
            int sequenceNumber = 1;


            partDenom = (realS + koff) * (realS + koff);
            realSq = realS * realS;
            feq = koff / (kon + koff);

            for (i = 0; i < imagS.length; i++) {
                imagSq = imagS[i] * imagS[i];
                denom = partDenom + imagSq;

                // 1/(p + koff)
                realInv = (realS + koff) / denom;
                imagInv = -imagS[i] / denom;

                // (1 + kon/(p + koff))
                realGroup = 1.0 + (kon * realInv);
                imagGroup = kon * imagInv;

                // q*q
                realQsq = (realS * realGroup / diffusion) - (imagS[i] * imagGroup / diffusion);
                imagQsq = (realS * imagGroup / diffusion) + (imagS[i] * realGroup / diffusion);
                zsqrt(realQsq, imagQsq, realQ, imagQ);
                modelBessel = new Bessel(Bessel.BESSEL_K, realQ[0] * radius, imagQ[0] * radius, initialOrder,
                                         Bessel.UNSCALED_FUNCTION, sequenceNumber, cyr, cyi, nz, errorFlag);
                modelBessel.run();
                realBk = cyr[0];
                imagBk = cyi[0];
                modelBessel = new Bessel(Bessel.BESSEL_I, realQ[0] * radius, imagQ[0] * radius, initialOrder,
                                         Bessel.UNSCALED_FUNCTION, sequenceNumber, cyr, cyi, nz, errorFlag);
                modelBessel.run();
                realBi = cyr[0];
                imagBi = cyi[0];

                // 2.0*K1(q*radius)*I1(q*radius))
                realSet = (2.0 * realBk * realBi) - (2.0 * imagBk * imagBi);
                imagSet = (2.0 * realBk * imagBi) + (2.0 * imagBk * realBi);

                // Feq/p
                denom = realSq + imagSq;
                realVar = feq * realS / denom;
                imagVar = -feq * imagS[i] / denom;

                // (Feq/p)*2.0*K1(q*radius)*I1(q*radius))
                realBunch = (realSet * realVar) - (imagSet * imagVar);
                imagBunch = (realSet * imagVar) + (imagSet * realVar);

                // (Feq/p)*2.0*K1(q*radius)*I1(q*radius)*(1 + kon/(p+koff))
                ans[i][0] = (realBunch * realGroup) - (imagBunch * imagGroup);
                ans[i][1] = (realBunch * imagGroup) + (imagBunch * realGroup);
            } // for (i = 0; i < imagS.length; i++)

            return ans;
        }

        /**
         * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
         *
         * @param   zr  double
         * @param   zi  double
         *
         * @return  double
         */
        private double zabs(double zr, double zi) {
            double u, v, q, s;
            u = Math.abs(zr);
            v = Math.abs(zi);
            s = u + v;

            // s * 1.0 makes an unnormalized underflow on CDC machines into a true
            // floating zero
            s = s * 1.0;

            if (s == 0.0) {
                return 0.0;
            } else if (u > v) {
                q = v / u;

                return (u * Math.sqrt(1.0 + (q * q)));
            } else {
                q = u / v;

                return (v * Math.sqrt(1.0 + (q * q)));
            }
        }

        /**
         * complex square root b = csqrt(a).
         *
         * @param  ar  double
         * @param  ai  double
         * @param  br  double[]
         * @param  bi  double[]
         */
        private void zsqrt(double ar, double ai, double[] br, double[] bi) {
            double drt = 1.0 / Math.sqrt(2.0);
            double zm;
            double theta;

            zm = zabs(ar, ai);
            zm = Math.sqrt(zm);

            if (ar == 0.0) {

                if (ai == 0.0) {
                    br[0] = 0.0;
                    bi[0] = 0.0;

                    return;
                } // if (ai == 0.0)
                else if (ai > 0.0) {
                    br[0] = zm * drt;
                    bi[0] = zm * drt;

                    return;
                } // else if (ai > 0.0)
                else { // ai < 0.0
                    br[0] = zm * drt;
                    bi[0] = -zm * drt;

                    return;
                } // else ai < 0.0
            } // if (ar == 0.0)
            else if (ai == 0.0) {

                if (ar > 0.0) {
                    br[0] = Math.sqrt(ar);
                    bi[0] = 0.0;

                    return;
                } // if (ar > 0.0)
                else { // ar < 0.0
                    br[0] = 0.0;
                    bi[0] = Math.sqrt(Math.abs(ar));

                    return;
                } // ar < 0.0
            } // else if (ai == 0.0)

            theta = Math.atan(ai / ar);

            if (theta <= 0.0) {

                if (ar < 0.0) {
                    theta = theta + Math.PI;
                }
            } else if (ar < 0.0) {
                theta = theta - Math.PI;
            }

            theta = 0.5 * theta;
            br[0] = zm * Math.cos(theta);
            bi[0] = zm * Math.sin(theta);

            return;
        }

    }


    

    
    /**
     * DOCUMENT ME!
     */
    class FitPure1DModel extends NLConstrainedEngine {
    	private double xData[];
    	private float yData[];

        /**
         * Creates a new FitPure1DModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitPure1DModel(int nPoints, double[] xData, float[] yData, double[] initial) {

            // nPoints data points, 3 coefficients, and nonlinear fitting
            super(nPoints, 3);
            this.xData = xData;
            this.yData = yData;
            
            bounds = 0; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            
            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;

            gues[0] = initial[0];
            gues[1] = initial[1];
            gues[2] = initial[2];

        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying pure 1D fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitPure1D ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("D/w**2 = " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("bottom = " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("span = " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /**
         * Fit to function - a1 + a2*[1 - 1/sqrt(1 + 4*PI*a0*x)]. 
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j;
            double ymod = 0;
            double term;

            try {
                ctrl = ctrlMat[0];
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n",
                // Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                    	term = (1.0 + (4.0 * Math.PI * a[0] * xData[j]));
                        ymod = a[1] + (a[2] * (1 - (1 / Math.sqrt(term))));
                        residuals[j] = ymod - yData[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                    	term = (1.0 + (4.0 * Math.PI * a[0] * xData[j]));
                    	covarMat[j][0] = 2.0 * a[2] * Math.PI * xData[j] / (term * Math.sqrt(term));
                    	covarMat[j][1] = 1.0;
                    	covarMat[j][2] = (1 - (1 / Math.sqrt(term)));
                    }
                }
                // Calculate the Jacobian numerically
                // else if (ctrl == 2) {
                // ctrlMat[0] = 0;
                // }
            } catch (final Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n");
            }

            return;
        }

    }


    /**
     * DOCUMENT ME!
     */
    class FitPure1DNoWholeModel extends NL2sol {
    	int iv[];
    	double v[];
    	double x[];
        private double xData[];
        private float yData[];
        
        /**
         * Creates a new FitSM2nl2solModel object.
         * 
         * @param nPoints DOCUMENT ME!
         * @param yData DOCUMENT ME!
         * @param x DOCUMENT ME!
         * @param iv
         * @param v
         * @param useAnalyticJacobian
         */
        public FitPure1DNoWholeModel(final int nPoints, double[] xData, float[] yData,
                double[] x, int iv[], double v[], boolean useAnalyticJacobian) {
            
            // nPoints data points
        	// 2 coefficients
        	// x[] is a length 3 initial guess at input and best estimate at output
        	// data starts at x[1]
        	// iv[] has length 61 + number of coefficients = 63
        	// v[] has length at least 94 + n*p + 3*n + p*(3*p+33)/2
        	// uiparm, integer parameter array = null
        	// urparm, double parameter array = null
            super(nPoints, 2, x, iv, v, useAnalyticJacobian, null, null);
            this.x = x;
            this.iv = iv;
            this.v = v;
            this.xData = xData;
            this.yData = yData;
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying pure 1D no whole organ normalization fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitPure1DNoWhole ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iv[31]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(2.0 * v[10]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("D/w**2 = " + String.valueOf(x[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("bottom = " + String.valueOf(x[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        public void calcj(final int meqn, final int nvar, final double x[], final int nf, final double jac[][],
                final int uiparm[], final double urparm[]) {
        	int j;
        	double term;
        	for (j = 0; j < meqn; j++) {
            	term = (1.0 + (4.0 * Math.PI * x[1] * xData[j]));
            	jac[j+1][1] = 2.0 * (1 - x[2]) * Math.PI * xData[j] / (term * Math.sqrt(term));
            	jac[j+1][2] = 1 / Math.sqrt(term);
            }    
        }
        
        /**
         * Fit to function - a1 + (1 - a1)*[1 - 1/sqrt(1 + 4*PI*a0*x)].
         */
        public void calcr(final int meqn, final int nvar, final double x[], final int nf, final double r[],
                final int uiparm[], final double urparm[]) {
            int j;
            double ymod;
            double term;
            for (j = 0; j < meqn; j++) {
            	term = (1.0 + (4.0 * Math.PI * x[1] * xData[j]));
            	ymod = x[2] + ((1 - x[2]) * (1 - (1 / Math.sqrt(term))));
                r[j+1] = ymod - yData[j];
            }
        }
        
    }

    /**
     * DOCUMENT ME!
     */
    class FitSineModel extends InverseLaplace2 {

        /**
         * Creates a new FitSineModel object.
         *
         * @param  time         DOCUMENT ME!
         * @param  abscissa     DOCUMENT ME!
         * @param  relEps       DOCUMENT ME!
         * @param  absEps       DOCUMENT ME!
         * @param  result       DOCUMENT ME!
         * @param  estErr       DOCUMENT ME!
         * @param  evaluations  DOCUMENT ME!
         * @param  errStatus    DOCUMENT ME!
         */
        public FitSineModel(double[] time, double abscissa, double relEps, double absEps, double[] result,
                            double[] estErr, int[] evaluations, int[] errStatus) {
            super(time, abscissa, relEps, absEps, result, estErr, evaluations, errStatus);

        }

        /**
         * DOCUMENT ME!
         *
         * @param  realIn   DOCUMENT ME!
         * @param  imagIn   DOCUMENT ME!
         * @param  realOut  DOCUMENT ME!
         * @param  imagOut  DOCUMENT ME!
         */
        public void fitToLaplace(double realIn, double imagIn, double[] realOut, double[] imagOut) {

            // 1/(s**2 + 1) is the Laplace transform of sin(t)
            double c, d;
            c = (realIn * realIn) - (imagIn * imagIn) + 1.0;
            d = (c * c) + (4.0 * realIn * realIn * imagIn * imagIn);
            realOut[0] = c / d;
            imagOut[0] = -2.0 * realIn * imagIn / d;

            return;
        }
    }


    /**
     * DOCUMENT ME!
     */
    class FitSingleExponentialModel extends NLConstrainedEngine {
        private double xData[];
        private float yData[];
        /**
         * Creates a new FitSingleExponentialModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitSingleExponentialModel(int nPoints, double[] xData, float[] yData, double[] initial) {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3);
            this.xData = xData;
            this.yData = yData;
            
            bounds = 0; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            
            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;

            gues[0] = initial[0];
            gues[1] = initial[1];
            gues[2] = initial[2];

        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitSingleExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);

            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /** 
         * Fit to function - bottom + span*[1 - exp(-ln(2)*t/thalf)] a1 + a2*(1 - exp(-ln(2)*t/a0)).
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j;
            double ymod = 0;

            try {
                ctrl = ctrlMat[0];
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n", 
                // Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                    	ymod = a[1] + (a[2] * (1.0 - Math.exp(-Math.log(2.0) * xData[j] / a[0])));
                        residuals[j] = ymod - yData[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                    	covarMat[j][0] = -xData[j]*a[2]*Math.log(2.0)*Math.exp(-Math.log(2.0)*xData[j]/a[0])/(a[0]*a[0]);
                    	covarMat[j][1] = 1.0;
                    	covarMat[j][2] = 1.0 - Math.exp(-Math.log(2.0)*xData[j]/a[0]);
                    }
                }
                // Calculate the Jacobian numerically
                // else if (ctrl == 2) {
                // ctrlMat[0] = 0;
                // }
            } catch (final Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }

    }


    /**
     * DOCUMENT ME!
     */
    class FitSingleExponentialNoWholeModel extends NL2sol {
    	int iv[];
    	double v[];
    	double x[];
    	private double xData[];
    	private float yData[];
    	
    	/**
         * Creates a new FitSingleExponentialNoWholeModel object.
         * 
         * @param nPoints DOCUMENT ME!
         * @param xData
         * @param yData DOCUMENT ME!
         * @param x DOCUMENT ME!
         * @param iv
         * @param v
         * @param useAnalyticJacobian
         */
        public FitSingleExponentialNoWholeModel(final int nPoints, double[] xData, float[] yData,
                double[] x, int iv[], double v[], boolean useAnalyticJacobian) {
            
            // nPoints data points
        	// 3 coefficients
        	// x[] is a length 3 initial guess at input and best estimate at output
        	// data starts at x[1]
        	// iv[] has length 61 + number of coefficients = 63
        	// v[] has length at least 94 + n*p + 3*n + p*(3*p+33)/2
        	// uiparm, integer parameter array = null
        	// urparm, double parameter array = null
            super(nPoints, 2, x, iv, v, useAnalyticJacobian, null, null);
            this.x = x;
            this.iv = iv;
            this.v = v;
            this.xData = xData;
            this.yData = yData;
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters in the case of no whole organ normalization.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitSingleExponentialNoWholeModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iv[31]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(2.0*v[10]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(x[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(x[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        public void calcj(final int meqn, final int nvar, final double x[], final int nf, final double jac[][],
                final int uiparm[], final double urparm[]) {
        	int j;
        	for (j  = 0; j < meqn; j++) {
        		jac[j+1][1] = -xData[j]*(1.0 - x[2])*Math.log(2.0)*Math.exp(-Math.log(2.0)*xData[j]/x[1])/(x[1]*x[1]);
        		jac[j+1][2] = 1.0 - Math.exp(-Math.log(2.0)*xData[j]/x[1]);
        	}
        }
        
        /**
         * Fit to function - bottom + (1 - bottom)*[1 - exp(-ln(2)*t/thalf)] a1 + (1 - a1)*(1 - exp(-ln(2)*t/a0)).
         */
        public void calcr(final int meqn, final int nvar, final double x[], final int nf, final double r[],
                final int uiparm[], final double urparm[]) {
        	int j;
        	double ymod;
        	for (j = 0; j < meqn; j++) {
        		ymod = x[2] + ((1 - x[2]) * (1.0 - Math.exp(-Math.log(2.0) * xData[j] / x[1])));
                r[j+1] = ymod - yData[j];	
        	}
        }
        
        
    }

    /**
     * DOCUMENT ME!
     */
    class FitWholeNL2solInt2 extends NL2sol {
    	int iv[];
    	double v[];
    	double x[];
    	double xData[];
    	float yData[];

        /** DOCUMENT ME! */
        double epsabs = 0.0;

        /** DOCUMENT ME! */
        double epsrel = 1.0E-6;

        /** DOCUMENT ME! */
        int inf = 1;

        /** DOCUMENT ME! */
        int limit = 2000;

        /** DOCUMENT ME! */
        double lower = 0.0;
        
        /**
         * Creates a new FitWholeNL2solInt2 object.
         * 
         * @param nPoints DOCUMENT ME!
         * @param yData DOCUMENT ME!
         * @param x DOCUMENT ME!
         * @param iv
         * @param v
         * @param useAnalyticJacobian
         */
        public FitWholeNL2solInt2(final int nPoints, double xData[], float[] yData,
                double[] x, int iv[], double v[], boolean useAnalyticJacobian) {
            
            // nPoints data points
        	// 2 coefficients
        	// x[] is a length 3 initial guess at input and best estimate at output
        	// data starts at x[1]
        	// iv[] has length 61 + number of coefficients = 63
        	// v[] has length at least 94 + n*p + 3*n + p*(3*p+33)/2
        	// uiparm, integer parameter array = null
        	// urparm, double parameter array = null
            super(nPoints, 2, x, iv, v, useAnalyticJacobian, null, null);
            this.x = x;
            this.iv = iv;
            this.v = v;
            this.xData = xData;
            this.yData = yData;
        }

        

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* Fit Whole NL2sol Int2 ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iv[31]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(2.0*v[10]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(x[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(x[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        public void calcj(final int meqn, final int nvar, final double x[], final int nf, final double jac[][],
                final int uiparm[], final double urparm[]) {
        	
        }
        
        public void calcr(final int meqn, final int nvar, final double x[], final int nf, final double r[],
                final int uiparm[], final double urparm[]) {
            int j;
            double ymod = 0;
            FitFullIntModel2s imods;
            FitFullIntModel2i imodi;
            
            for (j = 0; j < meqn; j++) {
            	imods = new FitFullIntModel2s(xData[j], x[1], x[2], lower, x[2], Integration2.DQAGSE, epsabs,
            			epsrel, limit);
                imods.driver();
                Preferences.debug("imdos error = " + imods.getErrorStatus() + "\n", Preferences.DEBUG_ALGORITHM);
                imodi = new FitFullIntModel2i(xData[j], x[1], x[2], x[1] + x[2], Integration2.DQAGIE, inf,
                        epsabs, epsrel, limit);
                imodi.driver();
                Preferences.debug("imodi error = " + imodi.getErrorStatus() + "\n", Preferences.DEBUG_ALGORITHM);
                ymod = 1.0 - imods.getIntegral() - imodi.getIntegral();
                r[j+1] = ymod - yData[j];
                // Preferences.debug("residuals["+ (j+1) + "] = " + r[j+1] + "\n", Preferences.DEBUG_ALGORITHM);
            }
            
        }

    }

    /**
     * DOCUMENT ME!
     */
    class FitWholeNL2solModel extends NL2sol {
    	int iv[];
    	double v[];
    	double x[];
        private double xData[];
        private float yData[];
        /** DOCUMENT ME! */
        double largestPole;

        /** DOCUMENT ME! */
        double tol;
        
        /**
         * Creates a new FitWholeNL2solModel object.
         * 
         * @param nPoints DOCUMENT ME!
         * @param xData
         * @param yData DOCUMENT ME!
         * @param x DOCUMENT ME!
         * @param iv
         * @param v
         * @param useAnalyticJacobian
         * @param largestPole
         * @param tol
         */
        public FitWholeNL2solModel(final int nPoints, final double[] xData, final float[] yData,
                double[] x, int iv[], double v[], boolean useAnalyticJacobian, double largestPole,
                double tol) {
            
            // nPoints data points
        	// 2 coefficients
        	// x[] is a length 3 initial guess at input and best estimate at output
        	// data starts at x[1]
        	// iv[] has length 61 + number of coefficients = 63
        	// v[] has length at least 94 + n*p + 3*n + p*(3*p+33)/2
        	// uiparm, integer parameter array = null
        	// urparm, double parameter array = null
            super(nPoints, 2, x, iv, v, useAnalyticJacobian, null, null);
            this.x = x;
            this.iv = iv;
            this.v = v;
            this.xData = xData;
            this.yData = yData;
            this.largestPole = largestPole;
            this.tol = tol;
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }
        
        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* Fit NL2sol Whole Diffusion-Reaction Model ********* \n\n",
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iv[31]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(2.0 * v[10]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(x[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(x[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        public void calcr(final int meqn, final int nvar, final double x[], final int nf, final double r[],
                final int uiparm[], final double urparm[]) {
        	FitFullModel lmod;
        	double[] timeFunction;
        	int j;
        	
        	lmod = new FitFullModel(xData, largestPole, tol, x[1], x[2]);
            lmod.driver();
            timeFunction = lmod.getTimeFunction();
            // evaluate the residuals[j] = ymodel[j] - ySeries[j]
            for (j = 0; j < meqn; j++) {
                r[j+1] = timeFunction[j] - yData[j];
                // Preferences.debug("residuals["+ (j+1) + "] = " + r[j+1] + "\n", Preferences.DEBUG_ALGORITHM);
            }

        }
        
        public void calcj(final int meqn, final int nvar, final double x[], final int nf, final double jac[][],
                final int uiparm[], final double urparm[]) {
        
        }

    }

    

    /**
     * DOCUMENT ME!
     */
    class FitWholeNLConInt2 extends NLConstrainedEngine {
        double xData[];
        float yData[];
        /** DOCUMENT ME! */
        double bound;

        /** DOCUMENT ME! */
        double epsabs = 0.0;

        /** DOCUMENT ME! */
        double epsrel = 1.0E-6;

        /** DOCUMENT ME! */
        int inf = 1;

        /** DOCUMENT ME! */
        int limit = 2000;

        /** DOCUMENT ME! */
        double lower = 0.0;

        /** DOCUMENT ME! */
        double[] result;

        /** DOCUMENT ME! */
        double upper;

        /**
         * Creates a new FitWholeNLConInt2 object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         * @param  result   DOCUMENT ME!
         */
        public FitWholeNLConInt2(int nPoints, double[] xData, float[] yData, double[] initial, double[] result) {
       
            super(nPoints, 2);
            this.xData = xData;
            this.yData = yData;
            this.result = result;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain kon
            bl[0] = 0.0;
            bu[0] = 1.0E20;

            // Constrain koff
            bl[1] = 0.0;
            bu[1] = 1.0E20;


            gues[0] = initial[0];
            gues[1] = initial[1];
        }


        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* Fit Elsunc Whole Diffusion-Reaction Model ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * Fit to function.
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int i;
            FitFullIntModel2s imods;
            FitFullIntModel2i imodi;

            try {
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {

                    for (i = 0; i < result.length; i++) {
                        imods = new FitFullIntModel2s(xData[i], a[0], a[1], lower, a[1], Integration2.DQAGSE, epsabs,
                                                      epsrel, limit);
                        imods.driver();
                        Preferences.debug("imods error = " + imods.getErrorStatus() + "\n", Preferences.DEBUG_ALGORITHM);
                        imodi = new FitFullIntModel2i(xData[i], a[0], a[1], a[0] + a[1], Integration2.DQAGIE, inf,
                                                      epsabs, epsrel, limit);
                        imodi.driver();
                        Preferences.debug("imodi error = " + imodi.getErrorStatus() + "\n", Preferences.DEBUG_ALGORITHM);
                        result[i] = 1.0 - imods.getIntegral() - imodi.getIntegral();
                        residuals[i] = result[i] - yData[i];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))

                // Calculate the Jacobian numerically
                else if (ctrl == 2) {
                    ctrlMat[0] = 0;
                }
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }


    }


    /**
     * DOCUMENT ME!
     */
    class FitWholeNLConModel extends NLConstrainedEngine {
        private double xData[];
        private float yData[];
        /** DOCUMENT ME! */
        double largestPole;

        /** DOCUMENT ME! */
        double tol;

        /**
         * Creates a new FitWholeNLConModel object.
         *
         * @param  nPoints      DOCUMENT ME!
         * @param  xData        DOCUMENT ME!
         * @param  yData        DOCUMENT ME!
         * @param  initial      DOCUMENT ME!
         * @param  largestPole  DOCUMENT ME!
         * @param  tol          DOCUMENT ME!
         */
        public FitWholeNLConModel(int nPoints, double[] xData, float[] yData, double[] initial, double largestPole,
                                  double tol) {

            super(nPoints, 2);
            this.xData = xData;
            this.yData = yData;
            this.largestPole = largestPole;
            this.tol = tol;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain kon
            bl[0] = 0.0;
            bu[0] = 1.0E20;

            // Constrain koff
            bl[1] = 0.0;
            bu[1] = 1.0E20;


            gues[0] = initial[0];
            gues[1] = initial[1];
        }


        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* Fit Elsunc Whole Diffusion-Reaction Model ********* \n\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * Fit to function.
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int i;
            FitFullModel lmod;
            double[] timeFunction;

            try {
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {
                    lmod = new FitFullModel(xData, largestPole, tol, a[0], a[1]);
                    lmod.driver();
                    timeFunction = lmod.getTimeFunction();

                    for (i = 0; i < timeFunction.length; i++) {
                        residuals[i] = timeFunction[i] - yData[i];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))

                // Calculate the Jacobian numerically
                else if (ctrl == 2) {
                    ctrlMat[0] = 0;
                }
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }


    }

    /**
     * DOCUMENT ME!
     */
    class FitWholeNLConModel2 extends NLConstrainedEngine {
        private double xData[];
        private float yData[];
        /** DOCUMENT ME! */
        double abscissa;

        /** DOCUMENT ME! */
        double absEps;

        /** DOCUMENT ME! */
        int[] errStatus;

        /** DOCUMENT ME! */
        double[] estErr;

        /** DOCUMENT ME! */
        int[] evaluations;

        /** DOCUMENT ME! */
        double relEps;

        /** DOCUMENT ME! */
        double[] result;

        /**
         * Creates a new FitWholeNLConModel2 object.
         *
         * @param  nPoints      DOCUMENT ME!
         * @param  xData        DOCUMENT ME!
         * @param  yData        DOCUMENT ME!
         * @param  initial      DOCUMENT ME!
         * @param  abscissa     DOCUMENT ME!
         * @param  relEps       DOCUMENT ME!
         * @param  absEps       DOCUMENT ME!
         * @param  result       DOCUMENT ME!
         * @param  estErr       DOCUMENT ME!
         * @param  evaluations  DOCUMENT ME!
         * @param  errStatus    DOCUMENT ME!
         */
        public FitWholeNLConModel2(int nPoints, double[] xData, float[] yData, double[] initial, double abscissa,
                                   double relEps, double absEps, double[] result, double[] estErr, int[] evaluations,
                                   int[] errStatus) {

            super(nPoints, 2);
            this.xData = xData;
            this.yData = yData;
            this.abscissa = abscissa;
            this.relEps = relEps;
            this.absEps = absEps;
            this.result = result;
            this.estErr = estErr;
            this.evaluations = evaluations;
            this.errStatus = errStatus;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain kon
            bl[0] = 0.0;
            bu[0] = 1.0E20;

            // Constrain koff
            bl[1] = 0.0;
            bu[1] = 1.0E20;


            gues[0] = initial[0];
            gues[1] = initial[1];
        }


        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* Fit Elsunc Whole Diffusion-Reaction Model ********* \n\n",
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * Fit to function.
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int i;
            FitFullModel2 lmod;

            try {
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {
                    lmod = new FitFullModel2(xData, abscissa, relEps, absEps, result, estErr, evaluations, errStatus,
                                             a[0], a[1]);
                    lmod.driver();

                    for (i = 0; i < result.length; i++) {
                        residuals[i] = result[i] - yData[i];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))

                // Calculate the Jacobian numerically
                else if (ctrl == 2) {
                    ctrlMat[0] = 0;
                }
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }


    }

    /**
     * DOCUMENT ME!
     */
    class FitWholeNLConModel3 extends NLConstrainedEngine {
        private double xData[];
        private float yData[];
        /** DOCUMENT ME! */
        double eps;

        /** DOCUMENT ME! */
        double[] result;

        /** DOCUMENT ME! */
        int routine;

        /** DOCUMENT ME! */
        double upper;

        /**
         * Creates a new FitWholeNLConModel3 object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         * @param  upper    DOCUMENT ME!
         * @param  routine  DOCUMENT ME!
         * @param  eps      DOCUMENT ME!
         * @param  result   DOCUMENT ME!
         */
        public FitWholeNLConModel3(int nPoints, double[] xData, float[] yData, double[] initial, double upper,
                                   int routine, double eps, double[] result) {

            super(nPoints, 2);
            this.xData = xData;
            this.yData = yData;
            this.upper = upper;
            this.routine = routine;
            this.eps = eps;
            this.result = result;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain kon
            bl[0] = 0.0;
            bu[0] = 1.0E20;

            // Constrain koff
            bl[1] = 0.0;
            bu[1] = 1.0E20;


            gues[0] = initial[0];
            gues[1] = initial[1];
        }


        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* Fit Elsunc Whole Diffusion-Reaction Model ********* \n\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * Fit to function.
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int i;
            FitFullIntModel imod;

            try {
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {

                    for (i = 0; i < result.length; i++) {
                        imod = new FitFullIntModel(xData[i], a[0], a[1]);         
                        RungeKuttaFehlbergIntegrator kIntegrator = new RungeKuttaFehlbergIntegrator(imod);
                        kIntegrator.setEps(eps);
                        result[i] = 1.0 - kIntegrator.integrate(a[0], upper);
                        residuals[i] = result[i] - yData[i];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))

                // Calculate the Jacobian numerically
                else if (ctrl == 2) {
                    ctrlMat[0] = 0;
                }
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }


    }


    /**
     * DOCUMENT ME!
     */
    class FitWholeNLConModelqd extends NLConstrainedEngine {
        private double xData[];
        private float yData[];
        /**
         * Creates a new FitWholeNLConModelqd object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitWholeNLConModelqd(int nPoints, double[] xData, float[] yData, double[] initial) {

            super(nPoints, 2);
            this.xData = xData;
            this.yData = yData;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain kon
            bl[0] = 0.0;
            bu[0] = 1.0E20;

            // Constrain koff
            bl[1] = 0.0;
            bu[1] = 1.0E20;


            gues[0] = initial[0];
            gues[1] = initial[1];
        }


        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* Fit Elsunc Whole Diffusion-Reaction Model ********* \n\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * Fit to function.
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int i;
            FitFullModelqd lmod;
            double[] timeFunctionWithZero;
            double[] xSeriesWithZero;

            try {
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {
                    xSeriesWithZero = new double[xData.length + 1];
                    xSeriesWithZero[0] = 0.0;

                    for (i = 0; i < xData.length; i++) {
                        xSeriesWithZero[i + 1] = xData[i];
                    }

                    lmod = new FitFullModelqd(xSeriesWithZero, a[0], a[1]);
                    lmod.driver();
                    timeFunctionWithZero = lmod.getTimeFunction();

                    for (i = 0; i < (timeFunctionWithZero.length - 1); i++) {
                        residuals[i] = timeFunctionWithZero[i + 1] - yData[i];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))

                // Calculate the Jacobian numerically
                else if (ctrl == 2) {
                    ctrlMat[0] = 0;
                }
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }


    }

    
    /**
     * class IntModel extends Integration { public IntModel(double lower, double upper, int routine, double eps) {
     * super(lower, upper, routine, eps); } public double intFunc(double x) { double function = 0.0; if ((x != 1.0/7.0)
     * && (x != 2.0/3.0)) { function = Math.pow(Math.abs(x - 1.0/7.0), -0.25) * Math.pow(Math.abs(x - 2.0/3.0), -0.55);
     * } return function; } public void driver() { super.driver(); } }.
     */
    class IntModel implements RealFunctionOfOneVariable {

        /**
         * Creates a new IntModel object.
         */
        public IntModel() {}


        /**
         * DOCUMENT ME!
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double intFunc(double x) {
            double function;
            function = 2.0 / (2.0 + Math.sin(10.0 * Math.PI * x));

            return function;
        }


		@Override
		public double eval(double x) {
			return intFunc(x);
		}

    }

    /**
     * class IntModel2 extends Integration2 { public IntModel2(double lower, double upper, int routine, double
     * breakPoints[], double epsabs, double epsrel, int limit) { super(lower, upper, routine, breakPoints, epsabs,
     * epsrel, limit); } public double intFunc(double x) { double function = 0.0; if ((x != 1.0/7.0) && (x != 2.0/3.0))
     * { function = Math.pow(Math.abs(x - 1.0/7.0), -0.25) * Math.pow(Math.abs(x - 2.0/3.0), -0.55); } return function;
     * } public void driver() { super.driver(); } }.
     */
    class IntModel2 extends Integration2 {

        /**
         * Creates a new IntModel2 object.
         *
         * @param  lower    DOCUMENT ME!
         * @param  upper    DOCUMENT ME!
         * @param  routine  DOCUMENT ME!
         * @param  key      DOCUMENT ME!
         * @param  epsabs   DOCUMENT ME!
         * @param  epsrel   DOCUMENT ME!
         * @param  limit    DOCUMENT ME!
         */
        public IntModel2(double lower, double upper, int routine, int key, double epsabs, double epsrel, int limit) {
            super(lower, upper, routine, key, epsabs, epsrel, limit);
        }

        /**
         * DOCUMENT ME!
         */
        public void driver() {
            super.driver();
        }

        /**
         * DOCUMENT ME!
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double intFunc(double x) {
            double function;
            function = 2.0 / (2.0 + Math.sin(10.0 * Math.PI * x));

            return function;
        }
    }
}
