package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * An optional registration may be performed before colocalization. In this registration both images or colors are moved
 * halfway to a common center point. AlgorithmRegOAR2D is used with the cost function being the only registration
 * parameter the user can vary in the dialog box. Correlation ratio is the default, but the user can also select least
 * squares, normalized cross correlation, or normalized mutual information. The colocalization will be performed on the
 * registered image or images rather than on the original image. If the maximum data range is decreased, the bin1 or
 * bin2 will be decreased if necessary to keep them from exceeding the actual number of channels in the data range.
 *
 * <p>A histogram buffer is generated with bin1 counts along the x axis and bin2 counts along the y axis. bin1 and bin2
 * are user selectable in the dialog. leftPad, rightPad, bottomPad, and topPad spaces are left around the data area in
 * histBuffer to allow room for axis lines and labels.</p>
 *
 * <p>The user specifies the number of gaussian distributions. For each distribution the initial values of the means,
 * standard deviations, and correlation are assigned using a random number generator. Each distribution is initially
 * assigned an equal probability.</p>
 *
 * <p>The ellipse corresponds to a contour of constant probabliity density on which the density is smaller by a factor
 * of exp[-0.5] than it is at the mean point. The probability density function is a jointly gaussian or bivariate
 * gaussian density function. In each iteration: 1.) Find the gaussian distribution j with the form: p(x,y/j) =
 * (1/(2*PI*stdxj*stdyj*sqrt(1 - rhoj**2))) * exp{(-1/(2*(1 - rhoj**2)))[((x-meanxj)**2)/(stdxj**2) -
 * (2*rhoj*(x-meanxj)*(y-meanyj))/(stdxj*stdyj) + ((y - meanyj)**2)/(stdyj**2)]} where stdxj and stdyj are the x and y
 * standard deviations and rhoj is the correlation coefficient.</p>
 *
 * <p>2.) p(x,y) are found from p(x,y) = sum over j of p(x,y/j)*p(j). 3.) Pold(j/(x,y) = (sum over all j of
 * Pold(x,y/j)*Pold(j))/Pold(x,y)</p>
 *
 * <p>The update equations for the values of the new variables for each gaussian distribution j are: 4.) meanxj = (sum
 * over x,y of Pold(j/(x,y))*x)/(sum over x,y of Pold(j/(x,y))) 5.) meanyj = (sum over x,y of Pold(j/(x,y))*y)/(sum over
 * x,y of Pold(j/(x,y))) 6.) stdxj**2 = (sum over x,y of Pold(j/(x,y)) * (x - meanxj)**2)/ (sum over x,y of
 * Pold(j/(x,y))) 7.) stdyj**2 = (sum over x,y of Pold(j/(x,y)) * (y - meanyj)**2)/ (sum over x,y of Pold(j/(x,y))) 8.)
 * rhoj = (sum over x,y of Pold(j/(x,y)) * (x - meanxj) * (y - meanyj)/(stdjx * stdjy))/ (sum over x,y of Pold(j/(x,y)))
 * </p>
 *
 * <p>9.) Each new p[j] is found from (sum over x,y of Pold(j/(x,y)))/number of (x,y) points These 9 basic steps are
 * iterated the number of times specified by the user.</p>
 *
 * <p>The calculated data is passed to ViewJFrameColocalization.</p>
 *
 * <p>References:</p>
 * 
 * <p>1.) Expectation Maximization Algorithm by Ismet Bayraktaroglu (ismet@bayraktaroglu.org)
 * Code for circles was originally found at 
 * http://www-cse.ucsd.edu/users/ibayrakt/java/em
 * but this site is no longer available.</p>
 *
 * <p>2.) Neural Networks for Pattern Recognition by Christopher M. Bishop, Oxford University Press, 1995, Section 2.6.2
 * The EM algorithm, pp. 65 - 72.</p>
 *
 * <p>3.) Probability, Random Variables, and Random Signal Principles, Second Edition by Peyton Z. Peebles, Jr.,
 * McGraw-Hill Book Company, 1987, Section 5.3 Jointly Gaussian Random Variables, pp. 124 - 128.</p>
 *
 * <p>4.) Clustering Algorithms by John A. Hartigan, Wiley, John & Sons, Incorporated, 1975, Chapter 5, Mixtures, pp.
 * 113 - 129.</p>
 *
 * <p>5.) Principles of Data Mining by David Hand, Heikki Mannila, and Padhraic Smith, A Bradford Book The MIT Press,
 * 2001, pp. 260 - 265, 281 - 284, 315 - 323.</p>
 */
public class AlgorithmColocalizationEM extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    float[] b1;

    /** DOCUMENT ME! */
    float[] b2;

    /** DOCUMENT ME! */
    float[] buffer;

    /** DOCUMENT ME! */
    BitSet inputMask = null;

    /** DOCUMENT ME! */
    float[] secondBuffer;

    /** DOCUMENT ME! */
    int xDim, yDim, zDim;

    /** DOCUMENT ME! */
    private ModelImage baseImage = null;

    /** DOCUMENT ME! */
    private int bin1 = 256; // Histogram x axis bin number

    /** DOCUMENT ME! */
    private int bin2 = 256; // Histogram y aixs bin number

    /** DOCUMENT ME! */
    private int bottomPad = 40;

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private int count; // Number of valid points

    /** DOCUMENT ME! */
    private double[] dMax = new double[2];

    /** DOCUMENT ME! */
    private double[] dMin = new double[2];

    /** If true, or thresholds. If false, and thresholds */
    private boolean doOr = true;

    /** DOCUMENT ME! */
    private boolean entireImage = true;

    /** DOCUMENT ME! */
    private int gaussians; // Number of gaussians

    /** DOCUMENT ME! */
    private double[] halfMajor;

    /** DOCUMENT ME! */
    private double[] halfMinor;

    /** DOCUMENT ME! */
    private int iterations; // Number of iterations

    /** Space padding around the histogram data area. */
    private int leftPad = 40;

    /** DOCUMENT ME! */
    private double[][] mean;

    /** DOCUMENT ME! */
    private double[][] meanOld;

    /** DOCUMENT ME! */
    private byte[] mostProbableClass;

    /** DOCUMENT ME! */
    private double[] prob;

    /** DOCUMENT ME! */
    private double[] probOld;

    /** DOCUMENT ME! */
    private double[] probX;

    /** DOCUMENT ME! */
    private double[] probXOld;

    /** DOCUMENT ME! */
    private boolean register;

    /** DOCUMENT ME! */
    private double[] rho;

    /** DOCUMENT ME! */
    private double[] rhoOld;

    /** DOCUMENT ME! */
    private int rightPad = 20;

    /** DOCUMENT ME! */
    private byte[] segBuffer;

    /** Displays classes to which pixels belong. */
    private ModelImage segImage = null;

    /** DOCUMENT ME! */
    private double[][] std;

    /** DOCUMENT ME! */
    private double[][] stdOld;

    /** DOCUMENT ME! */
    private double[] theta;

    /** The minimum values that are considered as data values. */
    private float threshold1 = 1.0f;

    /** DOCUMENT ME! */
    private float threshold2 = 1.0f;

    /** DOCUMENT ME! */
    private int topPad = 20;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which 2D histogram is placed in a predetermined destination image. Used for 2 black and
     * white images
     *
     * @param  destImg      image model where result image is to stored
     * @param  segImage     image to display classes to which pixels belong
     * @param  srcImg       image for x axis of histogram
     * @param  baseImage    image for y axis of histogram
     * @param  bin1         histogram x axis bin number
     * @param  bin2         histogram y axis bin number
     * @param  threshold1   minimum value for data in sourceImage
     * @param  threshold2   minimum value for data in baseImage
     * @param  doOr         If true, or thresholds. If false, and thresholds.
     * @param  leftPad      space to the left of the histogram bin area
     * @param  rightPad     space to the right of the histogram bin area
     * @param  bottomPad    space to the bottom of the histogram bin area
     * @param  topPad       space to the top of the histogram bin area
     * @param  entireImage  If false, only use data from VOI region
     * @param  register     If true register 2 black and white images before colocalization
     * @param  cost         Cost function used in registration
     * @param  gaussians    Number of gaussians
     * @param  iterations   Number of iterations
     */
    public AlgorithmColocalizationEM(ModelImage destImg, ModelImage segImage, ModelImage srcImg, ModelImage baseImage,
                                     int bin1, int bin2, float threshold1, float threshold2, boolean doOr, int leftPad,
                                     int rightPad, int bottomPad, int topPad, boolean entireImage, boolean register,
                                     int cost, int gaussians, int iterations) {

        super(destImg, srcImg);
        this.segImage = segImage;
        this.baseImage = baseImage;
        this.bin1 = bin1;
        this.bin2 = bin2;
        this.threshold1 = threshold1;
        this.threshold2 = threshold2;
        this.doOr = doOr;
        this.leftPad = leftPad;
        this.rightPad = rightPad;
        this.bottomPad = bottomPad;
        this.topPad = topPad;
        this.entireImage = entireImage;
        this.register = register;
        this.cost = cost;
        this.gaussians = gaussians;
        this.iterations = iterations;

        this.mask = srcImage.generateVOIMask();
        prob = new double[gaussians];
        mean = new double[gaussians][2];
        std = new double[gaussians][2];
        rho = new double[gaussians];
        probOld = new double[gaussians];
        meanOld = new double[gaussians][2];
        stdOld = new double[gaussians][2];
        rhoOld = new double[gaussians];
        halfMajor = new double[gaussians];
        halfMinor = new double[gaussians];
        theta = new double[gaussians];
    }

    /**
     * Constructor for images in which 2D histogram is placed in a predetermined destination image. Used for 1 color
     * image.
     *
     * @param  destImg      image model where result image is to stored
     * @param  segImage     image to display classes to which pixels belong
     * @param  srcImg       source image model
     * @param  bin1         histogram x axis bin number
     * @param  bin2         histogram y axis bin number
     * @param  threshold1   minimum data value for first color
     * @param  threshold2   minimum data value for second color
     * @param  doOr         If true, or thresholds. If false, and thresholds.
     * @param  leftPad      space to the left of the histogram bin area
     * @param  rightPad     space to the right of the histogram bin area
     * @param  bottomPad    space to the bottom of the histogram bin area
     * @param  topPad       space to the top of the histogram bin area
     * @param  useRed       DOCUMENT ME!
     * @param  useGreen     DOCUMENT ME!
     * @param  useBlue      DOCUMENT ME!
     * @param  entireImage  If false, use only data from VOI region
     * @param  register     If true, register 2 colors before colocalization
     * @param  cost         Cost function used in registration
     * @param  gaussians    Number of gaussians
     * @param  iterations   Number of iterations
     */
    public AlgorithmColocalizationEM(ModelImage destImg, ModelImage segImage, ModelImage srcImg, int bin1, int bin2,
                                     float threshold1, float threshold2, boolean doOr, int leftPad, int rightPad,
                                     int bottomPad, int topPad, boolean useRed, boolean useGreen, boolean useBlue,
                                     boolean entireImage, boolean register, int cost, int gaussians, int iterations) {

        super(destImg, srcImg);
        this.segImage = segImage;
        this.bin1 = bin1;
        this.bin2 = bin2;
        this.threshold1 = threshold1;
        this.threshold2 = threshold2;
        this.doOr = doOr;
        this.leftPad = leftPad;
        this.rightPad = rightPad;
        this.bottomPad = bottomPad;
        this.topPad = topPad;
        this.useRed = useRed;
        this.useGreen = useGreen;
        this.useBlue = useBlue;
        this.entireImage = entireImage;
        this.register = register;
        this.cost = cost;
        this.gaussians = gaussians;
        this.iterations = iterations;

        this.mask = srcImage.generateVOIMask();
        prob = new double[gaussians];
        mean = new double[gaussians][2];
        std = new double[gaussians][2];
        rho = new double[gaussians];
        probOld = new double[gaussians];
        meanOld = new double[gaussians][2];
        stdOld = new double[gaussians][2];
        rhoOld = new double[gaussians];
        halfMajor = new double[gaussians];
        halfMinor = new double[gaussians];
        theta = new double[gaussians];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        int i;
        baseImage = null;
        destImage = null;
        segImage = null;
        srcImage = null;
        prob = null;
        
        if (mean != null) {

            for (i = 0; i < mean.length; i++) {
                mean[i] = null;
            }

            mean = null;
        }

        if (std != null) {

            for (i = 0; i < std.length; i++) {
                std[i] = null;
            }

            std = null;
        }

        rho = null;
        probOld = null;

        if (meanOld != null) {

            for (i = 0; i < meanOld.length; i++) {
                meanOld[i] = null;
            }

            meanOld = null;
        }

        if (stdOld != null) {

            for (i = 0; i < stdOld.length; i++) {
                stdOld[i] = null;
            }

            stdOld = null;
        }

        rhoOld = null;
        probX = null;
        probXOld = null;
        dMin = null;
        dMax = null;
        halfMajor = null;
        halfMinor = null;
        theta = null;
        mostProbableClass = null;
        segBuffer = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (srcImage.isColorImage()) {

            if (srcImage.getNDims() == 2) {
                calcColor2D();
            } else {
                calcColor3D();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calc2D();
            } else {
                calc3D();
            }

        }
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
     * This function produces a 2D histogram image with srcImage values represented across the x axis and baseImage
     * values represented across the y axis.
     */
    private void calc2D() {
        int i;
        int n;
        int pos;
        double[] histBuffer;
        ModelImage refWeightImage = null;
        ModelImage inputWeightImage = null;
        AlgorithmRegOAR2D reg2;
        int DOF = 3;
        int interp = AlgorithmTransform.BILINEAR;
        float coarseBegin = -3.0f;
        float coarseEnd = 3.0f;
        float coarseRate = 2.0f;
        float fineRate = 1.0f;
        boolean doSubsample = true;
        boolean doMultiThread = true;
        AlgorithmTransform transform;
        ModelImage registeredBaseImage;
        ModelImage registeredSrcImage;
        boolean transformVOI = false;
        boolean clip = true;
        @SuppressWarnings("unused")
        ViewJFrameImage imageFrame;
        @SuppressWarnings("unused")
        ViewJFrameImage imageFrame2;
        TransMatrix xfrm;
        double min1, min2, max1, max2, range1, range2, scale1, scale2;
        int ch1, ch2;
        int length;
        int j, k;
        ViewVOIVector VOIs;
        int nVOIs;
        int nBoundingVOIs;
        @SuppressWarnings("unused")
        ViewJFrameColocalizationEM frameColocalize;
        Vector<ViewImageUpdateInterface> frameList;
        ViewJFrameBase controlFrame = null;
        float[] destRes;
        int voiSize = 0;
        float voiIntensity1 = 0.0f;
        float voiIntensity2 = 0.0f;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;

        inputMask = new BitSet(length);

        if (entireImage) {

            for (i = 0; i < length; i++) {
                inputMask.set(i);
            }
        } else {

            for (i = 0; i < length; i++) {

                if (mask.get(i)) {
                    inputMask.set(i);
                } else {
                    inputMask.clear(i);
                }
            }
        }

        if (!entireImage) {
            VOIs = srcImage.getVOIs();
            nVOIs = VOIs.size();
            nBoundingVOIs = 0;

            for (i = 0; i < nVOIs; i++) {

                if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                    nBoundingVOIs++;
                }
            }

            if (nBoundingVOIs == 0) {
                MipavUtil.displayError("No contour VOI is present");
                setCompleted(false);

                return;
            }

            if (nBoundingVOIs > 1) {
                MipavUtil.displayError("Can only use 1 contour VOI");
                setCompleted(false);

                return;
            }

            if (register) {
                float[] refRes = new float[] {
                                     srcImage.getFileInfo(0).getResolutions()[0],
                                     srcImage.getFileInfo(0).getResolutions()[1]
                                 };
                float[] inputRes = new float[] {
                                       baseImage.getFileInfo(0).getResolutions()[0],
                                       baseImage.getFileInfo(0).getResolutions()[1]
                                   };

                refWeightImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), "VOI ref");
                inputWeightImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), "VOI input");

                refWeightImage.getFileInfo(0).setResolutions(refRes);
                inputWeightImage.getFileInfo(0).setResolutions(inputRes);

                // make new reference and input images based on the VOIs in them.
                // pass those new images to the registration algorithm
                for (i = 0; i < length; i++) {

                    if (!mask.get(i)) {
                        refWeightImage.set(i, 0);
                        inputWeightImage.set(i, 0);
                    } else {
                        refWeightImage.set(i, 1);
                        inputWeightImage.set(i, 1);
                    }
                }
            } // if (register)
        } // if (!entireImage)

        if (register) {

            // Register each image half of the way to the same center point
            if (!entireImage) {
                reg2 = new AlgorithmRegOAR2D(srcImage, baseImage, refWeightImage, inputWeightImage, cost, DOF, interp,
                                             coarseBegin, coarseEnd, coarseRate, fineRate, doSubsample, doMultiThread);
            } else {
                reg2 = new AlgorithmRegOAR2D(srcImage, baseImage, cost, DOF, interp, coarseBegin, coarseEnd, coarseRate,
                                             fineRate, doSubsample, doMultiThread);
            }

            reg2.run();
            xfrm = reg2.getTransform();
            reg2.disposeLocal();
            reg2 = null;
            xfrm.Set(0, 1, xfrm.Get(0, 1) * 0.5f);
            xfrm.Set(0, 2, xfrm.Get(0, 2) * 0.5f);
            xfrm.Set(1, 0, xfrm.Get(1, 0) * 0.5f);
            xfrm.Set(1, 2, xfrm.Get(1, 2) * 0.5f);

            float xresA = baseImage.getFileInfo(0).getResolutions()[0];
            float yresA = baseImage.getFileInfo(0).getResolutions()[1];

            transform = new AlgorithmTransform(baseImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA, xDim, yDim,
                                               transformVOI, clip, false);

            transform.run();
            registeredBaseImage = transform.getTransformedImage();
            updateFileInfo(baseImage, registeredBaseImage);
            registeredBaseImage.calcMinMax();
            registeredBaseImage.setImageName(baseImage.getImageName() + "_register");
            transform.disposeLocal();
            transform = null;

            if (!entireImage) {
                transform = new AlgorithmTransform(inputWeightImage, xfrm, AlgorithmTransform.NEAREST_NEIGHBOR, xresA,
                                                   yresA, xDim, yDim, transformVOI, clip, false);
                transform.run();

                for (i = 0; i < length; i++) {

                    if (transform.getTransformedImage().getUByte(i) == 0) {
                        inputMask.clear(i);
                    } else {
                        inputMask.set(i);
                    }
                }

                transform.disposeLocal();
                transform = null;
            } // if (!entireImage)

            bin2 = Math.min(bin2, (int) Math.round(registeredBaseImage.getMax() - registeredBaseImage.getMin() + 1));

            if (!entireImage) {
                reg2 = new AlgorithmRegOAR2D(baseImage, srcImage, inputWeightImage, refWeightImage, cost, DOF, interp,
                                             coarseBegin, coarseEnd, coarseRate, fineRate, doSubsample, doMultiThread);
            } else {
                reg2 = new AlgorithmRegOAR2D(baseImage, srcImage, cost, DOF, interp, coarseBegin, coarseEnd, coarseRate,
                                             fineRate, doSubsample, doMultiThread);
            }

            reg2.run();
            xfrm = reg2.getTransform();
            reg2.disposeLocal();
            reg2 = null;
            xfrm.Set(0, 1, xfrm.Get(0, 1) * 0.5f);
            xfrm.Set(0, 2, xfrm.Get(0, 2) * 0.5f);
            xfrm.Set(1, 0, xfrm.Get(1, 0) * 0.5f);
            xfrm.Set(1, 2, xfrm.Get(1, 2) * 0.5f);

            xresA = srcImage.getFileInfo(0).getResolutions()[0];
            yresA = srcImage.getFileInfo(0).getResolutions()[1];

            transform = new AlgorithmTransform(srcImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA, xDim, yDim,
                                               transformVOI, clip, false);

            transform.run();
            registeredSrcImage = transform.getTransformedImage();
            updateFileInfo(srcImage, registeredSrcImage);
            registeredSrcImage.calcMinMax();
            registeredSrcImage.setImageName(srcImage.getImageName() + "_register");
            transform.disposeLocal();
            transform = null;

            if (!entireImage) {
                transform = new AlgorithmTransform(refWeightImage, xfrm, AlgorithmTransform.NEAREST_NEIGHBOR, xresA,
                                                   yresA, xDim, yDim, transformVOI, clip, false);
                transform.run();
                refWeightImage = transform.getTransformedImage();
                transform.disposeLocal();
                transform = null;

                for (i = 0; i < length; i++) {

                    if (refWeightImage.getUByte(i) == 0) {
                        inputMask.clear(i);
                    }
                }

                inputWeightImage.disposeLocal();
                inputWeightImage = null;
                refWeightImage.disposeLocal();
                refWeightImage = null;
            } // if (!entireImage)

            bin1 = Math.min(bin1, (int) Math.round(registeredSrcImage.getMax() - registeredSrcImage.getMin() + 1));

            int[] newExtents = new int[2];
            newExtents[0] = bin1 + leftPad + rightPad;
            newExtents[1] = bin2 + bottomPad + topPad;
            destImage.changeExtents(newExtents);

            try {
                imageFrame = new ViewJFrameImage(registeredSrcImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Unable to open frame for registered image");
                setCompleted(false);

                return;
            }

            try {
                imageFrame2 = new ViewJFrameImage(registeredBaseImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Unable to open frame for registered image");
                setCompleted(false);

                return;
            }
        } // if (register)
        else {
            registeredSrcImage = srcImage;
            registeredBaseImage = baseImage;
        }

        try {
            buffer = new float[length];
            secondBuffer = new float[length];

            // Blank spaces at left and bottom
            histBuffer = new double[(bin1 + leftPad + rightPad) * (bin2 + bottomPad + topPad)];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            secondBuffer = null;
            histBuffer = null;
            errorCleanUp("Algorithm ColocalizationEM: Out of memory", true);

            return;
        }

        for (i = 0; i < histBuffer.length; i++) {
            histBuffer[i] = 0.0;
        }

        destImage.releaseLock(); // we need to be able to alter the dest image

        try {

            try {
                fireProgressStateChanged(srcImage.getImageName(), "Creating 2D Colocolization Histogram ...");
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm " +
                                      "and dispose the progressbar between checking for " +
                                      "threadStopping and using it.", Preferences.DEBUG_ALGORITHM);
                }
            }

            k = 0;

            for (j = 0; j <= (yDim - 1); j++) {

                for (i = 0; i <= (xDim - 1); i++) {
                    pos = i + (xDim * j);
                    buffer[k] = registeredSrcImage.getFloat(pos);
                    secondBuffer[k++] = registeredBaseImage.getFloat(pos);
                }
            }

            for (i = 0; i < length; i++) {

                if (inputMask.get(i)) {
                    voiSize++;

                    if (doOr) {

                        if ((buffer[i] >= threshold1) || (secondBuffer[i] >= threshold2)) {
                            voiIntensity1 += buffer[i];
                            voiIntensity2 += secondBuffer[i];
                        }
                    } // if (doOr)
                    else { // and thresholding

                        if ((buffer[i] >= threshold1) && (secondBuffer[i] >= threshold2)) {
                            voiIntensity1 += buffer[i];
                            voiIntensity2 += secondBuffer[i];
                        }
                    } // else and thresholding
                } // if (inputMask.get(i))
            } // for (i = 0; i < length; i++)

            min1 = Double.MAX_VALUE;
            max1 = -Double.MAX_VALUE;
            min2 = Double.MAX_VALUE;
            max2 = -Double.MAX_VALUE;

            for (i = 0; i < length; i++) {

                if ((buffer[i] > max1) && inputMask.get(i)) {
                    max1 = buffer[i];
                }

                if ((buffer[i] < min1) && inputMask.get(i)) {
                    min1 = buffer[i];
                }

                if ((secondBuffer[i] > max2) && inputMask.get(i)) {
                    max2 = secondBuffer[i];
                }

                if ((secondBuffer[i] < min2) && inputMask.get(i)) {
                    min2 = secondBuffer[i];
                }
            }

            range1 = max1 - min1;
            range2 = max2 - min2;
            scale1 = (bin1 - 1) / range1;
            scale2 = (bin2 - 1) / range2;
            // The first bin goes from min1 to min1 + (max1 - min1)/(bin1-1)
            // The second bin goes from min1 + (max1 - min1)/(bin1-1) to
            // min1 + 2*(max1 - min1)/(bin1-1)
            // The last bin goes from min1 + (bin1-2)*(max1 - min1)/(bin1-1)
            // to max1

            emfunction();
            segBuffer = new byte[length];

            fireProgressStateChanged("Generating histogram buffer");

            for (i = 0, n = 0; i < length; i++) {

                if (doOr) {

                    if (((buffer[i] >= threshold1) || (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {
                        ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                        ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                        // invert y
                        histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
                        segBuffer[i] = (byte) ((255 * (mostProbableClass[n++] + 1)) / gaussians);
                    }
                } // if (doOr)
                else { // and thresholding

                    if (((buffer[i] >= threshold1) && (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {
                        ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                        ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                        // invert y
                        histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
                        segBuffer[i] = (byte) ((255 * (mostProbableClass[n++] + 1)) / gaussians);
                    }
                } // else and thresholding
            } // for (i = 0; i < length; i++)

            if (threadStopped) { // do before copying back into image
                finalize();

                return;
            }

            destImage.importData(0, histBuffer, true);
            destRes = new float[2];
            destRes[0] = 1.0f;
            destRes[1] = 1.0f;
            destImage.getFileInfo(0).setResolutions(destRes);
            segImage.importData(0, segBuffer, true);
            updateFileInfo(srcImage, segImage);
            segImage.clearMask();

            try {
                new ViewJFrameImage(segImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Unable to open frame for segImage");
                setCompleted(false);

                return;
            }


        } // try
        catch (IOException error) {
            displayError("Algorithm ColocalizationEM reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            secondBuffer = null;
            displayError("Algorithm ColocalizationEM reports: out of memory" + e);
            setCompleted(false);

            return;
        }

        frameList = srcImage.getImageFrameVector();

        for (i = 0; i < frameList.size(); i++) {

            if ((((ViewJFrameBase) frameList.elementAt(i)).getControls()) != null) {
                controlFrame = ((ViewJFrameBase) frameList.elementAt(i));
            }
        }

        fireProgressStateChanged("Creating ColocalizationEM frame");
        frameColocalize = new ViewJFrameColocalizationEM(registeredSrcImage, null, registeredBaseImage, null, null,
                                                         destImage, controlFrame, useRed, useGreen, useBlue, min1, max1,
                                                         min2, max2, scale1, scale2, leftPad, rightPad, bottomPad,
                                                         topPad, mean, halfMajor, halfMinor, theta);
        if (mean != null) {

            for (i = 0; i < mean.length; i++) {
                mean[i] = null;
            }

            mean = null;
        }


        setCompleted(true);
    }

    /**
     * This function produces a 2D histogram image with srcImage values represented across the x axis and baseImage
     * values represented across the y axis.
     */
    private void calc3D() {
        int i;
        int n;
        int pos;
        double[] histBuffer;
        int[] extents2D;
        ModelImage refImage;
        ModelImage inputImage;
        ModelImage refWeightImage = null;
        ModelImage inputWeightImage = null;
        AlgorithmRegOAR2D reg2;
        int DOF = 3;
        int interp = AlgorithmTransform.BILINEAR;
        float coarseBegin = -3.0f;
        float coarseEnd = 3.0f;
        float coarseRate = 2.0f;
        float fineRate = 1.0f;
        boolean doSubsample = true;
        boolean doMultiThread = true;
        AlgorithmTransform transform;
        ModelImage registeredImage;
        ModelImage registeredSrcImage;
        ModelImage registeredBaseImage;
        boolean transformVOI = false;
        boolean clip = true;
        @SuppressWarnings("unused")
        ViewJFrameImage imageFrame;
        @SuppressWarnings("unused")
        ViewJFrameImage imageFrame2;
        TransMatrix xfrm;
        double min1, min2, max1, max2, range1, range2, scale1, scale2;
        int ch1, ch2;
        int length;
        int volume;
        int zPos;
        int totPos;
        int z;
        int j, k, m;
        ViewVOIVector VOIs;
        int nVOIs;
        int nBoundingVOIs;
        @SuppressWarnings("unused")
        ViewJFrameColocalizationEM frameColocalize;
        Vector<ViewImageUpdateInterface> frameList;
        ViewJFrameBase controlFrame = null;
        float[] destRes;
        int voiSize = 0;
        float voiIntensity1 = 0.0f;
        float voiIntensity2 = 0.0f;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        length = xDim * yDim;
        volume = length * zDim;

        inputMask = new BitSet(volume);

        if (entireImage) {

            for (i = 0; i < volume; i++) {
                inputMask.set(i);
            }
        } else {

            for (i = 0; i < volume; i++) {

                if (mask.get(i)) {
                    inputMask.set(i);
                } else {
                    inputMask.clear(i);
                }
            }
        }

        if (!entireImage) {
            VOIs = srcImage.getVOIs();
            nVOIs = VOIs.size();
            nBoundingVOIs = 0;

            for (i = 0; i < nVOIs; i++) {

                if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                    nBoundingVOIs++;
                }
            }

            if (nBoundingVOIs == 0) {
                MipavUtil.displayError("No contour VOI is present");
                setCompleted(false);

                return;
            }

            if (nBoundingVOIs > 1) {
                MipavUtil.displayError("Can only use 1 contour VOI");
                setCompleted(false);

                return;
            }

        } // if (!entireImage)

        if (register) {

            // Register each image half of the way to the same center point
            extents2D = new int[2];
            extents2D[0] = srcImage.getExtents()[0];
            extents2D[1] = srcImage.getExtents()[1];

            float xresA = baseImage.getFileInfo(0).getResolutions()[0];
            float yresA = baseImage.getFileInfo(0).getResolutions()[1];
            float xresB = srcImage.getFileInfo(0).getResolutions()[0];
            float yresB = srcImage.getFileInfo(0).getResolutions()[1];

            if (!entireImage) {
                float[] refRes = new float[] {
                                     srcImage.getFileInfo(0).getResolutions()[0],
                                     srcImage.getFileInfo(0).getResolutions()[1]
                                 };
                float[] inputRes = new float[] {
                                       baseImage.getFileInfo(0).getResolutions()[0],
                                       baseImage.getFileInfo(0).getResolutions()[1]
                                   };

                refWeightImage = new ModelImage(ModelStorageBase.BYTE, extents2D, "VOI ref");
                inputWeightImage = new ModelImage(ModelStorageBase.BYTE, extents2D, "VOI input");

                refWeightImage.getFileInfo(0).setResolutions(refRes);
                inputWeightImage.getFileInfo(0).setResolutions(inputRes);
                // make new reference and input images based on the VOIs in them. pass those new images to the
                // registration algorithm
            } // if (!entireImage)

            refImage = new ModelImage(srcImage.getType(), extents2D, "refImage");
            inputImage = new ModelImage(baseImage.getType(), extents2D, "inputImage");
            registeredSrcImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), "regSImage");
            registeredBaseImage = new ModelImage(baseImage.getType(), baseImage.getExtents(), "regBImage");

            for (z = 0; z < zDim; z++) {
                zPos = z * length;

                if (!entireImage) {

                    for (i = 0; i < length; i++) {

                        if (!mask.get(zPos + i)) {
                            refWeightImage.set(i, 0);
                            inputWeightImage.set(i, 0);
                        } else {
                            refWeightImage.set(i, 1);
                            inputWeightImage.set(i, 1);
                        }
                    }
                } // if (!entireImage)

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        pos = i + (xDim * j);
                        totPos = pos + zPos;
                        refImage.set(pos, srcImage.get(totPos));
                        inputImage.set(pos, baseImage.get(totPos));
                    }
                }

                refImage.calcMinMax();
                inputImage.calcMinMax();
                updateFileInfo(srcImage, refImage);
                updateFileInfo(baseImage, inputImage);

                if (!entireImage) {
                    reg2 = new AlgorithmRegOAR2D(refImage, inputImage, refWeightImage, inputWeightImage, cost, DOF,
                                                 interp, coarseBegin, coarseEnd, coarseRate, fineRate, doSubsample,
                                                 doMultiThread);
                } else {
                    reg2 = new AlgorithmRegOAR2D(refImage, inputImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                                 coarseRate, fineRate, doSubsample, doMultiThread);
                }

                reg2.run();
                xfrm = reg2.getTransform();
                reg2.disposeLocal();
                reg2 = null;
                xfrm.Set(0, 1, xfrm.Get(0, 1) * 0.5f);
                xfrm.Set(0, 2, xfrm.Get(0, 2) * 0.5f);
                xfrm.Set(1, 0, xfrm.Get(1, 0) * 0.5f);
                xfrm.Set(1, 2, xfrm.Get(1, 2) * 0.5f);
                transform = new AlgorithmTransform(inputImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA, xDim,
                                                   yDim, transformVOI, clip, false);

                transform.run();
                registeredImage = transform.getTransformedImage();

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        pos = i + (xDim * j);
                        totPos = pos + zPos;
                        registeredBaseImage.set(totPos, registeredImage.get(pos));
                    }
                }

                registeredImage.disposeLocal();
                registeredImage = null;

                if (!entireImage) {
                    transform = new AlgorithmTransform(inputWeightImage, xfrm, AlgorithmTransform.NEAREST_NEIGHBOR,
                                                       xresA, yresA, xDim, yDim, transformVOI, clip, false);

                    transform.run();

                    for (i = 0; i < length; i++) {

                        if (transform.getTransformedImage().getUByte(i) == 0) {
                            inputMask.clear(zPos + i);
                        } else {
                            inputMask.set(zPos + i);
                        }
                    }

                    transform.disposeLocal();
                    transform = null;
                    reg2 = new AlgorithmRegOAR2D(inputImage, refImage, inputWeightImage, refWeightImage, cost, DOF,
                                                 interp, coarseBegin, coarseEnd, coarseRate, fineRate, doSubsample,
                                                 doMultiThread);
                } else {
                    reg2 = new AlgorithmRegOAR2D(inputImage, refImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                                 coarseRate, fineRate, doSubsample, doMultiThread);
                }

                reg2.run();
                xfrm = reg2.getTransform();
                reg2.disposeLocal();
                reg2 = null;
                xfrm.Set(0, 1, xfrm.Get(0, 1) * 0.5f);
                xfrm.Set(0, 2, xfrm.Get(0, 2) * 0.5f);
                xfrm.Set(1, 0, xfrm.Get(1, 0) * 0.5f);
                xfrm.Set(1, 2, xfrm.Get(1, 2) * 0.5f);
                transform = new AlgorithmTransform(refImage, xfrm, AlgorithmTransform.BILINEAR, xresB, yresB, xDim,
                                                   yDim, transformVOI, clip, false);

                transform.run();
                registeredImage = transform.getTransformedImage();

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        pos = i + (xDim * j);
                        totPos = pos + zPos;
                        registeredSrcImage.set(totPos, registeredImage.get(pos));
                    }
                }

                registeredImage.disposeLocal();
                registeredImage = null;

                if (!entireImage) {
                    transform = new AlgorithmTransform(refWeightImage, xfrm, AlgorithmTransform.NEAREST_NEIGHBOR, xresA,
                                                       yresA, xDim, yDim, transformVOI, clip, false);

                    transform.run();

                    for (i = 0; i < length; i++) {

                        if (transform.getTransformedImage().getUByte(i) == 0) {
                            inputMask.clear(zPos + i);
                        }
                    }

                    transform.disposeLocal();
                    transform = null;
                }

            } // for (z = 0; z < zDim; z++)

            updateFileInfo(srcImage, registeredSrcImage);
            registeredSrcImage.calcMinMax();
            registeredSrcImage.setImageName(srcImage.getImageName() + "_register");
            bin1 = Math.min(bin1, (int) Math.round(registeredSrcImage.getMax() - registeredSrcImage.getMin() + 1));
            updateFileInfo(baseImage, registeredBaseImage);
            registeredBaseImage.calcMinMax();
            registeredBaseImage.setImageName(baseImage.getImageName() + "_register");
            bin2 = Math.min(bin2, (int) Math.round(registeredBaseImage.getMax() - registeredBaseImage.getMin() + 1));

            int[] newExtents = new int[2];
            newExtents[0] = bin1 + leftPad + rightPad;
            newExtents[1] = bin2 + bottomPad + topPad;
            destImage.changeExtents(newExtents);

            try {
                imageFrame = new ViewJFrameImage(registeredSrcImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Unable to open frame for registered image");
                setCompleted(false);

                return;
            }

            try {
                imageFrame2 = new ViewJFrameImage(registeredBaseImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Unable to open frame for registered image");
                setCompleted(false);

                return;
            }
        } // if (register)
        else {
            registeredBaseImage = baseImage;
            registeredSrcImage = srcImage;
        }

        try {
            buffer = new float[volume];
            secondBuffer = new float[volume];
            histBuffer = new double[(bin1 + leftPad + rightPad) * (bin2 + bottomPad + topPad)];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            secondBuffer = null;
            histBuffer = null;
            errorCleanUp("Algorithm ColocalizationEM: Out of memory", true);

            return;
        }

        for (i = 0; i < histBuffer.length; i++) {
            histBuffer[i] = 0.0;
        }

        destImage.releaseLock(); // we need to be able to alter the dest image

        try {

            try {
                fireProgressStateChanged(srcImage.getImageName(), "Creating 2D Colocolization Histogram ...");
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm " +
                                      "and dispose the progressbar between checking for " +
                                      "threadStopping and using it.", Preferences.DEBUG_ALGORITHM);
                }
            }

            m = 0;

            for (k = 0; k <= (zDim - 1); k++) {

                for (j = 0; j <= (yDim - 1); j++) {

                    for (i = 0; i <= (xDim - 1); i++) {
                        pos = i + (xDim * j) + (length * k);
                        buffer[m] = registeredSrcImage.getFloat(pos);
                        secondBuffer[m++] = registeredBaseImage.getFloat(pos);
                    }
                }
            }

            for (i = 0; i < volume; i++) {

                if (inputMask.get(i)) {
                    voiSize++;

                    if (doOr) {

                        if ((buffer[i] >= threshold1) || (secondBuffer[i] >= threshold2)) {
                            voiIntensity1 += buffer[i];
                            voiIntensity2 += secondBuffer[i];
                        }
                    } // if (doOr)
                    else { // and thresholding

                        if ((buffer[i] >= threshold1) && (secondBuffer[i] >= threshold2)) {
                            voiIntensity1 += buffer[i];
                            voiIntensity2 += secondBuffer[i];
                        }
                    } // else and thresholding
                } // if (inputMask.get(i))
            } // for (i = 0; i < volume; i++)

            min1 = Double.MAX_VALUE;
            max1 = -Double.MAX_VALUE;
            min2 = Double.MAX_VALUE;
            max2 = -Double.MAX_VALUE;

            for (i = 0; i < volume; i++) {

                if ((buffer[i] > max1) && inputMask.get(i)) {
                    max1 = buffer[i];
                }

                if ((buffer[i] < min1) && inputMask.get(i)) {
                    min1 = buffer[i];
                }

                if ((secondBuffer[i] > max2) && inputMask.get(i)) {
                    max2 = secondBuffer[i];
                }

                if ((secondBuffer[i] < min2) && inputMask.get(i)) {
                    min2 = secondBuffer[i];
                }
            }

            range1 = max1 - min1;
            range2 = max2 - min2;
            scale1 = (bin1 - 1) / range1;
            scale2 = (bin2 - 1) / range2;
            // The first bin goes from min1 to min1 + (max1 - min1)/(bin1-1)
            // The second bin goes from min1 + (max1 - min1)/(bin1-1) to
            // min1 + 2*(max1 - min1)/(bin1-1)
            // The last bin goes from min1 + (bin1-2)*(max1 - min1)/(bin1-1)
            // to max1

            emfunction();
            segBuffer = new byte[volume];

            fireProgressStateChanged("Generating histogram buffer");

            for (i = 0, n = 0; i < volume; i++) {

                if (doOr) {

                    if (((buffer[i] >= threshold1) || (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {
                        ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                        ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                        // invert y
                        histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
                        segBuffer[i] = (byte) ((255 * (mostProbableClass[n++] + 1)) / gaussians);
                    }
                } // if (doOr)
                else { // and thresholding

                    if (((buffer[i] >= threshold1) && (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {
                        ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                        ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                        // invert y
                        histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
                        segBuffer[i] = (byte) ((255 * (mostProbableClass[n++] + 1)) / gaussians);
                    }
                } // else and thresholding
            } // for (i = 0; i < volume; i++)

            if (threadStopped) { // do before copying back into image
                finalize();

                return;
            }

            destImage.importData(0, histBuffer, true);
            destRes = new float[2];
            destRes[0] = 1.0f;
            destRes[1] = 1.0f;
            destImage.getFileInfo(0).setResolutions(destRes);
            segImage.importData(0, segBuffer, true);
            updateFileInfo(srcImage, segImage);
            segImage.clearMask();

            try {
                new ViewJFrameImage(segImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Unable to open frame for segImage");
                setCompleted(false);

                return;
            }


        } // try
        catch (IOException error) {
            displayError("Algorithm ColocalizationEM reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            secondBuffer = null;
            displayError("Algorithm ColocalizationEM reports: out of memory" + e);
            setCompleted(false);

            return;
        }

        frameList = srcImage.getImageFrameVector();

        for (i = 0; i < frameList.size(); i++) {

            if ((((ViewJFrameBase) frameList.elementAt(i)).getControls()) != null) {
                controlFrame = ((ViewJFrameBase) frameList.elementAt(i));
            }
        }

        fireProgressStateChanged("Creating ColocalizationEM frame");
        frameColocalize = new ViewJFrameColocalizationEM(registeredSrcImage, null, registeredBaseImage, null, null,
                                                         destImage, controlFrame, useRed, useGreen, useBlue, min1, max1,
                                                         min2, max2, scale1, scale2, leftPad, rightPad, bottomPad,
                                                         topPad, mean, halfMajor, halfMinor, theta);
        if (mean != null) {

            for (i = 0; i < mean.length; i++) {
                mean[i] = null;
            }

            mean = null;
        }

        setCompleted(true);
    }

    /**
     * This function produces a 2D histogram image with first color values represented across the x axis and second
     * color values represented across the y axis.
     */
    private void calcColor2D() {
        int i;
        int n;
        int pos;
        double[] histBuffer;
        ModelImage refImage;
        ModelImage inputImage;
        ModelImage refWeightImage = null;
        ModelImage inputWeightImage = null;
        AlgorithmRegOAR2D reg2;
        int DOF = 3;
        int interp = AlgorithmTransform.BILINEAR;
        float coarseBegin = -3.0f;
        float coarseEnd = 3.0f;
        float coarseRate = 2.0f;
        float fineRate = 1.0f;
        boolean doSubsample = true;
        boolean doMultiThread = true;
        AlgorithmTransform transform;
        ModelImage registeredImageX;
        ModelImage registeredImageY;
        ModelImage registeredSrcImage;
        boolean transformVOI = false;
        boolean clip = true;
        int c;
        @SuppressWarnings("unused")
        ViewJFrameImage imageFrame;
        TransMatrix xfrm;
        double min1, min2, max1, max2, range1, range2, scale1, scale2;
        int ch1, ch2;
        int length;
        int j, k;
        ViewVOIVector VOIs;
        int nVOIs;
        int nBoundingVOIs;
        int color;
        int secondColor;
        @SuppressWarnings("unused")
        ViewJFrameColocalizationEM frameColocalize;
        Vector<ViewImageUpdateInterface> frameList;
        ViewJFrameBase controlFrame = null;
        float[] destRes;
        int voiSize = 0;
        float voiIntensity1 = 0.0f;
        float voiIntensity2 = 0.0f;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;

        inputMask = new BitSet(length);

        if (entireImage) {

            for (i = 0; i < length; i++) {
                inputMask.set(i);
            }
        } else {

            for (i = 0; i < length; i++) {

                if (mask.get(i)) {
                    inputMask.set(i);
                } else {
                    inputMask.clear(i);
                }
            }
        }

        if (!entireImage) {
            VOIs = srcImage.getVOIs();
            nVOIs = VOIs.size();
            nBoundingVOIs = 0;

            for (i = 0; i < nVOIs; i++) {

                if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                    nBoundingVOIs++;
                }
            }

            if (nBoundingVOIs == 0) {
                MipavUtil.displayError("No contour VOI is present");
                setCompleted(false);

                return;
            }

            if (nBoundingVOIs > 1) {
                MipavUtil.displayError("Can only use 1 contour VOI");
                setCompleted(false);

                return;
            }

            if (register) {
                float[] refRes = new float[] {
                                     srcImage.getFileInfo(0).getResolutions()[0],
                                     srcImage.getFileInfo(0).getResolutions()[1]
                                 };
                float[] inputRes = new float[] {
                                       srcImage.getFileInfo(0).getResolutions()[0],
                                       srcImage.getFileInfo(0).getResolutions()[1]
                                   };

                refWeightImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), "VOI ref");
                inputWeightImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), "VOI input");

                refWeightImage.getFileInfo(0).setResolutions(refRes);
                inputWeightImage.getFileInfo(0).setResolutions(inputRes);

                // make new reference and input images based on the VOIs in them.
                // pass those new images to the registration algorithm
                for (i = 0; i < length; i++) {

                    if (!mask.get(i)) {
                        refWeightImage.set(i, 0);
                        inputWeightImage.set(i, 0);
                    } else {
                        refWeightImage.set(i, 1);
                        inputWeightImage.set(i, 1);
                    }
                }
            } // if (register)

        } // if (!entireImage)

        if (useRed) {
            color = 1;
        } else {
            color = 2;
        }

        if (useBlue) {
            secondColor = 3;
        } else {
            secondColor = 2;
        }

        if (register) {
            String name = srcImage.getImageName() + "_register";

            if (srcImage.getType() == ModelStorageBase.ARGB) {
                refImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "refImage");
                inputImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "inputImage");
                registeredSrcImage = new ModelImage(ModelStorageBase.ARGB, srcImage.getExtents(), name);

            } else {
                refImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "refImage");
                inputImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "inputImage");
                registeredSrcImage = new ModelImage(ModelStorageBase.ARGB_USHORT, srcImage.getExtents(), name);

            }

            if (srcImage.getType() == ModelStorageBase.ARGB) {

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        pos = i + (xDim * j);
                        refImage.setUByte(pos, srcImage.getUByte((4 * pos) + color));
                        inputImage.setUByte(pos, srcImage.getUByte((4 * pos) + secondColor));
                    }
                }
            } // if (srcImage.getType() == ModelStorageBase.ARGB)
            else { // srcImage.getType() == ModelStorageBase.ARGB_USHORT

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        pos = i + (xDim * j);
                        refImage.setUShort(pos, srcImage.getUShort((4 * pos) + color));
                        inputImage.setUShort(pos, srcImage.getUShort((4 * pos) + secondColor));
                    }
                }
            } // else srcImage.getType() == ModelStorageBase.ARGB_USHORT

            refImage.calcMinMax();
            inputImage.calcMinMax();
            updateFileInfo(srcImage, refImage);
            updateFileInfo(srcImage, inputImage);

            // Register each color half of the way to the same center point
            if (!entireImage) {
                reg2 = new AlgorithmRegOAR2D(refImage, inputImage, refWeightImage, inputWeightImage, cost, DOF, interp,
                                             coarseBegin, coarseEnd, coarseRate, fineRate, doSubsample, doMultiThread);
            } else {
                reg2 = new AlgorithmRegOAR2D(refImage, inputImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                             coarseRate, fineRate, doSubsample, doMultiThread);
            }

            reg2.run();
            xfrm = reg2.getTransform();
            reg2.disposeLocal();
            reg2 = null;
            xfrm.Set(0, 1, xfrm.Get(0, 1) * 0.5f);
            xfrm.Set(0, 2, xfrm.Get(0, 2) * 0.5f);
            xfrm.Set(1, 0, xfrm.Get(1, 0) * 0.5f);
            xfrm.Set(1, 2, xfrm.Get(1, 2) * 0.5f);

            float xresA = srcImage.getFileInfo(0).getResolutions()[0];
            float yresA = srcImage.getFileInfo(0).getResolutions()[1];

            transform = new AlgorithmTransform(inputImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA, xDim, yDim,
                                               transformVOI, clip, false);

            transform.run();
            registeredImageY = transform.getTransformedImage();
            transform.disposeLocal();
            transform = null;

            if (!entireImage) {
                transform = new AlgorithmTransform(inputWeightImage, xfrm, AlgorithmTransform.NEAREST_NEIGHBOR, xresA,
                                                   yresA, xDim, yDim, transformVOI, clip, false);
                transform.run();

                for (i = 0; i < length; i++) {

                    if (transform.getTransformedImage().getUByte(i) == 0) {
                        inputMask.clear(i);
                    } else {
                        inputMask.set(i);
                    }
                }

                transform.disposeLocal();
                transform = null;
            } // if (!entireImage)

            registeredImageY.calcMinMax();
            bin2 = Math.min(bin2, (int) Math.round(registeredImageY.getMax() - registeredImageY.getMin() + 1));

            if (!entireImage) {
                reg2 = new AlgorithmRegOAR2D(inputImage, refImage, inputWeightImage, refWeightImage, cost, DOF, interp,
                                             coarseBegin, coarseEnd, coarseRate, fineRate, doSubsample, doMultiThread);
            } else {
                reg2 = new AlgorithmRegOAR2D(inputImage, refImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                             coarseRate, fineRate, doSubsample, doMultiThread);
            }

            reg2.run();
            xfrm = reg2.getTransform();
            reg2.disposeLocal();
            reg2 = null;
            xfrm.Set(0, 1, xfrm.Get(0, 1) * 0.5f);
            xfrm.Set(0, 2, xfrm.Get(0, 2) * 0.5f);
            xfrm.Set(1, 0, xfrm.Get(1, 0) * 0.5f);
            xfrm.Set(1, 2, xfrm.Get(1, 2) * 0.5f);

            transform = new AlgorithmTransform(refImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA, xDim, yDim,
                                               transformVOI, clip, false);

            transform.run();
            registeredImageX = transform.getTransformedImage();
            transform.disposeLocal();
            transform = null;

            if (!entireImage) {
                transform = new AlgorithmTransform(refWeightImage, xfrm, AlgorithmTransform.NEAREST_NEIGHBOR, xresA,
                                                   yresA, xDim, yDim, transformVOI, clip, false);
                transform.run();
                refWeightImage = transform.getTransformedImage();
                transform.disposeLocal();
                transform = null;

                for (i = 0; i < length; i++) {

                    if (refWeightImage.getUByte(i) == 0) {
                        inputMask.clear(i);
                    }
                }

                inputWeightImage.disposeLocal();
                inputWeightImage = null;
                refWeightImage.disposeLocal();
                refWeightImage = null;
            } // if (!entireImage)

            registeredImageX.calcMinMax();
            bin1 = Math.min(bin1, (int) Math.round(registeredImageX.getMax() - registeredImageX.getMin() + 1));

            int[] newExtents = new int[2];
            newExtents[0] = bin1 + leftPad + rightPad;
            newExtents[1] = bin2 + bottomPad + topPad;
            destImage.changeExtents(newExtents);

            if (srcImage.getType() == ModelStorageBase.ARGB) {

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        pos = i + (xDim * j);

                        for (c = 0; c <= 3; c++) {

                            if (c == color) {
                                registeredSrcImage.setUByte((4 * pos) + c, registeredImageX.getUByte(pos));
                            } else if (c == secondColor) {
                                registeredSrcImage.setUByte((4 * pos) + c, registeredImageY.getUByte(pos));
                            } else {
                                registeredSrcImage.setUByte((4 * pos) + c, srcImage.getUByte((4 * pos) + c));
                            }
                        }
                    }
                }
            } // if (srcImage.getType() == ModelStorageBase.ARGB)
            else { // srcImage.getType() == ModelStorageBase.ARGB_USHORT

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        pos = i + (xDim * j);

                        for (c = 0; c <= 3; c++) {

                            if (c == color) {
                                registeredSrcImage.setUShort((4 * pos) + c, registeredImageX.getUShort(pos));
                            } else if (c == secondColor) {
                                registeredSrcImage.setUShort((4 * pos) + c, registeredImageY.getUShort(pos));
                            } else {
                                registeredSrcImage.setUShort((4 * pos) + c, srcImage.getUShort((4 * pos) + c));
                            }
                        }
                    }
                }
            } // else srcImage.getType() == ModelStorageBase.ARGB_USHORT

            registeredImageX.disposeLocal();
            registeredImageX = null;
            registeredImageY.disposeLocal();
            registeredImageY = null;
            updateFileInfo(srcImage, registeredSrcImage);
            registeredSrcImage.calcMinMax();

            try {
                imageFrame = new ViewJFrameImage(registeredSrcImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Unable to open frame for registered image");
                setCompleted(false);

                return;
            }
        } // if (register)
        else {
            registeredSrcImage = srcImage;
        }

        try {
            buffer = new float[length];
            secondBuffer = new float[length];

            // Blank spaces at left and bottom
            histBuffer = new double[(bin1 + leftPad + rightPad) * (bin2 + bottomPad + topPad)];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            secondBuffer = null;
            histBuffer = null;
            errorCleanUp("Algorithm ColocalizationEM: Out of memory", true);

            return;
        }

        for (i = 0; i < histBuffer.length; i++) {
            histBuffer[i] = 0.0;
        }

        destImage.releaseLock(); // we need to be able to alter the dest image

        try {

            try {
                fireProgressStateChanged(srcImage.getImageName(), "Creating 2D Colocolization Histogram ...");
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm " +
                                      "and dispose the progressbar between checking for " +
                                      "threadStopping and using it.", Preferences.DEBUG_ALGORITHM);
                }
            }

            k = 0;

            if (srcImage.getType() == ModelStorageBase.ARGB) {

                for (j = 0; j <= (yDim - 1); j++) {

                    for (i = 0; i <= (xDim - 1); i++) {
                        pos = i + (xDim * j);
                        buffer[k] = registeredSrcImage.getUByte((4 * pos) + color);
                        secondBuffer[k++] = registeredSrcImage.getUByte((4 * pos) + secondColor);
                    }
                }
            } // if (srcImage.getType() == ModelStorageBase.ARGB)
            else { // srcImage.getType() == ModelStorageBase.ARGB_USHORT

                for (j = 0; j <= (yDim - 1); j++) {

                    for (i = 0; i <= (xDim - 1); i++) {
                        pos = i + (xDim * j);
                        buffer[k] = registeredSrcImage.getUShort((4 * pos) + color);
                        secondBuffer[k++] = registeredSrcImage.getUShort((4 * pos) + secondColor);
                    }
                }
            } // else srcImage.getType() == ModelStorageBase.ARGB_USHORT

            for (i = 0; i < length; i++) {

                if (inputMask.get(i)) {
                    voiSize++;

                    if (doOr) {

                        if ((buffer[i] >= threshold1) || (secondBuffer[i] >= threshold2)) {
                            voiIntensity1 += buffer[i];
                            voiIntensity2 += secondBuffer[i];
                        }
                    } // if (doOr)
                    else { // and thresholding

                        if ((buffer[i] >= threshold1) && (secondBuffer[i] >= threshold2)) {
                            voiIntensity1 += buffer[i];
                            voiIntensity2 += secondBuffer[i];
                        }
                    } // else and thresholding
                } // if (inputMask.get(i))
            } // for (i = 0; i < length; i++)

            min1 = Double.MAX_VALUE;
            max1 = -Double.MAX_VALUE;
            min2 = Double.MAX_VALUE;
            max2 = -Double.MAX_VALUE;

            for (i = 0; i < length; i++) {

                if ((buffer[i] > max1) && inputMask.get(i)) {
                    max1 = buffer[i];
                }

                if ((buffer[i] < min1) && inputMask.get(i)) {
                    min1 = buffer[i];
                }

                if ((secondBuffer[i] > max2) && inputMask.get(i)) {
                    max2 = secondBuffer[i];
                }

                if ((secondBuffer[i] < min2) && inputMask.get(i)) {
                    min2 = secondBuffer[i];
                }
            }

            range1 = max1 - min1;
            range2 = max2 - min2;
            scale1 = (bin1 - 1) / range1;
            scale2 = (bin2 - 1) / range2;
            // The first bin goes from min1 to min1 + (max1 - min1)/(bin1-1)
            // The second bin goes from min1 + (max1 - min1)/(bin1-1) to
            // min1 + 2*(max1 - min1)/(bin1-1)
            // The last bin goes from min1 + (bin1-2)*(max1 - min1)/(bin1-1)
            // to max1

            emfunction();
            segBuffer = new byte[length];

            fireProgressStateChanged("Generating histogram buffer");

            for (i = 0, n = 0; i < length; i++) {

                if (doOr) {

                    if (((buffer[i] >= threshold1) || (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {
                        ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                        ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                        // invert y
                        histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
                        segBuffer[i] = (byte) ((255 * (mostProbableClass[n++] + 1)) / gaussians);
                    }
                } // if (doOr)
                else { // and thresholding

                    if (((buffer[i] >= threshold1) && (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {
                        ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                        ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                        // invert y
                        histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
                        segBuffer[i] = (byte) ((255 * (mostProbableClass[n++] + 1)) / gaussians);
                    }
                } // else and thresholding
            } // for (i = 0; i < length; i++)

            if (threadStopped) { // do before copying back into image
                finalize();

                return;
            }

            destImage.importData(0, histBuffer, true);
            destRes = new float[2];
            destRes[0] = 1.0f;
            destRes[1] = 1.0f;
            destImage.getFileInfo(0).setResolutions(destRes);
            segImage.importData(0, segBuffer, true);
            updateFileInfo(srcImage, segImage);
            segImage.clearMask();

            try {
                new ViewJFrameImage(segImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Unable to open frame for segImage");
                setCompleted(false);

                return;
            }


        } // try
        catch (IOException error) {
            displayError("Algorithm ColocalizationEM reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            secondBuffer = null;
            displayError("Algorithm ColocalizationEM reports: out of memory" + e);
            setCompleted(false);

            return;
        }

        frameList = srcImage.getImageFrameVector();

        for (i = 0; i < frameList.size(); i++) {

            if ((((ViewJFrameBase) frameList.elementAt(i)).getControls()) != null) {
                controlFrame = ((ViewJFrameBase) frameList.elementAt(i));
            }
        }

        fireProgressStateChanged("Creating ColocalizationEM frame");
        frameColocalize = new ViewJFrameColocalizationEM(registeredSrcImage, null, baseImage, null, null, destImage,
                                                         controlFrame, useRed, useGreen, useBlue, min1, max1, min2,
                                                         max2, scale1, scale2, leftPad, rightPad, bottomPad, topPad,
                                                         mean, halfMajor, halfMinor, theta);
        if (mean != null) {

            for (i = 0; i < mean.length; i++) {
                mean[i] = null;
            }

            mean = null;
        }

        setCompleted(true);
    }

    /**
     * This function produces a 2D histogram image with first color values represented across the x axis and second
     * color values represented across the y axis.
     */
    private void calcColor3D() {
        int i;
        int n;
        int pos;
        double[] histBuffer;
        int[] extents2D;
        ModelImage refImage;
        ModelImage inputImage;
        ModelImage refWeightImage = null;
        ModelImage inputWeightImage = null;
        AlgorithmRegOAR2D reg2;
        int DOF = 3;
        int interp = AlgorithmTransform.BILINEAR;
        float coarseBegin = -3.0f;
        float coarseEnd = 3.0f;
        float coarseRate = 2.0f;
        float fineRate = 1.0f;
        boolean doSubsample = true;
        boolean doMultiThread = true;
        AlgorithmTransform transform;
        ModelImage registeredImageX;
        ModelImage registeredImageY;
        ModelImage registeredSrcImage;
        boolean transformVOI = false;
        boolean clip = true;
        int c;
        @SuppressWarnings("unused")
        ViewJFrameImage imageFrame;
        TransMatrix xfrm;
        double min1, min2, max1, max2, range1, range2, scale1, scale2;
        int ch1, ch2;
        int length;
        int volume;
        int zPos;
        int totPos;
        int z;
        int j, k, m;
        ViewVOIVector VOIs;
        int nVOIs;
        int nBoundingVOIs;
        int color;
        int secondColor;
        @SuppressWarnings("unused")
        ViewJFrameColocalizationEM frameColocalize;
        Vector<ViewImageUpdateInterface> frameList;
        ViewJFrameBase controlFrame = null;
        float[] destRes;
        int voiSize = 0;
        float voiIntensity1 = 0.0f;
        float voiIntensity2 = 0.0f;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        length = xDim * yDim;
        volume = length * zDim;

        inputMask = new BitSet(volume);

        if (entireImage) {

            for (i = 0; i < volume; i++) {
                inputMask.set(i);
            }
        } else {

            for (i = 0; i < volume; i++) {

                if (mask.get(i)) {
                    inputMask.set(i);
                } else {
                    inputMask.clear(i);
                }
            }
        }

        if (!entireImage) {
            VOIs = srcImage.getVOIs();
            nVOIs = VOIs.size();
            nBoundingVOIs = 0;

            for (i = 0; i < nVOIs; i++) {

                if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                    nBoundingVOIs++;
                }
            }

            if (nBoundingVOIs == 0) {
                MipavUtil.displayError("No contour VOI is present");
                setCompleted(false);

                return;
            }

            if (nBoundingVOIs > 1) {
                MipavUtil.displayError("Can only use 1 contour VOI");
                setCompleted(false);

                return;
            }

        } // if (!entireImage)

        if (useRed) {
            color = 1;
        } else {
            color = 2;
        }

        if (useBlue) {
            secondColor = 3;
        } else {
            secondColor = 2;
        }

        if (register) {

            extents2D = new int[2];
            extents2D[0] = srcImage.getExtents()[0];
            extents2D[1] = srcImage.getExtents()[1];

            float xresA = srcImage.getFileInfo(0).getResolutions()[0];
            float yresA = srcImage.getFileInfo(0).getResolutions()[1];

            if (!entireImage) {
                float[] refRes = new float[] {
                                     srcImage.getFileInfo(0).getResolutions()[0],
                                     srcImage.getFileInfo(0).getResolutions()[1]
                                 };
                float[] inputRes = new float[] {
                                       srcImage.getFileInfo(0).getResolutions()[0],
                                       srcImage.getFileInfo(0).getResolutions()[1]
                                   };

                refWeightImage = new ModelImage(ModelStorageBase.BYTE, extents2D, "VOI ref");
                inputWeightImage = new ModelImage(ModelStorageBase.BYTE, extents2D, "VOI input");

                refWeightImage.getFileInfo(0).setResolutions(refRes);
                inputWeightImage.getFileInfo(0).setResolutions(inputRes);
                // make new reference and input images based on the VOIs in them. pass those new images to the
                // registration algorithm
            } // if (!entireImage)

            String name = srcImage.getImageName() + "_register";

            if (srcImage.getType() == ModelStorageBase.ARGB) {
                refImage = new ModelImage(ModelStorageBase.UBYTE, extents2D, "refImage");
                inputImage = new ModelImage(ModelStorageBase.UBYTE, extents2D, "inputImage");
                registeredSrcImage = new ModelImage(ModelStorageBase.ARGB, srcImage.getExtents(), name);

            } else {
                refImage = new ModelImage(ModelStorageBase.USHORT, extents2D, "refImage");
                inputImage = new ModelImage(ModelStorageBase.USHORT, extents2D, "inputImage");
                registeredSrcImage = new ModelImage(ModelStorageBase.ARGB_USHORT, srcImage.getExtents(), name);

            }

            for (z = 0; z < zDim; z++) {
                zPos = z * length;

                if (!entireImage) {

                    for (i = 0; i < length; i++) {

                        if (!mask.get(zPos + i)) {
                            refWeightImage.set(i, 0);
                            inputWeightImage.set(i, 0);
                        } else {
                            refWeightImage.set(i, 1);
                            inputWeightImage.set(i, 1);
                        }
                    }
                } // if (!entireImage)

                if (srcImage.getType() == ModelStorageBase.ARGB) {

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            pos = i + (xDim * j);
                            totPos = pos + zPos;
                            refImage.setUByte(pos, srcImage.getUByte((4 * totPos) + color));
                            inputImage.setUByte(pos, srcImage.getUByte((4 * totPos) + secondColor));
                        }
                    }
                } // if (srcImage.getType() == ModelStorageBase.ARGB)
                else { // srcImage.getType() == ModelStorageBase.ARGB_USHORT

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            pos = i + (xDim * j);
                            totPos = pos + zPos;
                            refImage.setUShort(pos, srcImage.getUShort((4 * totPos) + color));
                            inputImage.setUShort(pos, srcImage.getUShort((4 * totPos) + secondColor));
                        }
                    }
                } // else srcImage.getType() == ModelStorageBase.ARGB_USHORT

                refImage.calcMinMax();
                inputImage.calcMinMax();
                updateFileInfo(srcImage, refImage);
                updateFileInfo(srcImage, inputImage);

                // Register each color half of the way to the same center point
                if (!entireImage) {
                    reg2 = new AlgorithmRegOAR2D(refImage, inputImage, refWeightImage, inputWeightImage, cost, DOF,
                                                 interp, coarseBegin, coarseEnd, coarseRate, fineRate, doSubsample,
                                                 doMultiThread);
                } else {
                    reg2 = new AlgorithmRegOAR2D(refImage, inputImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                                 coarseRate, fineRate, doSubsample, doMultiThread);
                }

                reg2.run();
                xfrm = reg2.getTransform();
                reg2.disposeLocal();
                reg2 = null;
                xfrm.Set(0, 1, xfrm.Get(0, 1) * 0.5f);
                xfrm.Set(0, 2, xfrm.Get(0, 2) * 0.5f);
                xfrm.Set(1, 0, xfrm.Get(1, 0) * 0.5f);
                xfrm.Set(1, 2, xfrm.Get(1, 2) * 0.5f);
                transform = new AlgorithmTransform(inputImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA, xDim,
                                                   yDim, transformVOI, clip, false);

                transform.run();
                registeredImageY = transform.getTransformedImage();
                transform.disposeLocal();
                transform = null;

                if (!entireImage) {
                    transform = new AlgorithmTransform(inputWeightImage, xfrm, AlgorithmTransform.NEAREST_NEIGHBOR,
                                                       xresA, yresA, xDim, yDim, transformVOI, clip, false);

                    transform.run();

                    for (i = 0; i < length; i++) {

                        if (transform.getTransformedImage().getUByte(i) == 0) {
                            inputMask.clear(zPos + i);
                        } else {
                            inputMask.set(zPos + i);
                        }
                    }

                    transform.disposeLocal();
                    transform = null;
                    reg2 = new AlgorithmRegOAR2D(inputImage, refImage, inputWeightImage, refWeightImage, cost, DOF,
                                                 interp, coarseBegin, coarseEnd, coarseRate, fineRate, doSubsample,
                                                 doMultiThread);
                } else {
                    reg2 = new AlgorithmRegOAR2D(inputImage, refImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                                 coarseRate, fineRate, doSubsample, doMultiThread);
                }

                reg2.run();
                xfrm = reg2.getTransform();
                reg2.disposeLocal();
                reg2 = null;
                xfrm.Set(0, 1, xfrm.Get(0, 1) * 0.5f);
                xfrm.Set(0, 2, xfrm.Get(0, 2) * 0.5f);
                xfrm.Set(1, 0, xfrm.Get(1, 0) * 0.5f);
                xfrm.Set(1, 2, xfrm.Get(1, 2) * 0.5f);
                transform = new AlgorithmTransform(refImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA, xDim,
                                                   yDim, transformVOI, clip, false);

                transform.run();
                registeredImageX = transform.getTransformedImage();
                transform.disposeLocal();
                transform = null;

                if (!entireImage) {
                    transform = new AlgorithmTransform(refWeightImage, xfrm, AlgorithmTransform.NEAREST_NEIGHBOR, xresA,
                                                       yresA, xDim, yDim, transformVOI, clip, false);

                    transform.run();

                    for (i = 0; i < length; i++) {

                        if (transform.getTransformedImage().getUByte(i) == 0) {
                            inputMask.clear(zPos + i);
                        }
                    }

                    transform.disposeLocal();
                    transform = null;
                }

                if (srcImage.getType() == ModelStorageBase.ARGB) {

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            pos = i + (xDim * j);
                            totPos = pos + zPos;

                            for (c = 0; c <= 3; c++) {

                                if (c == color) {
                                    registeredSrcImage.setUByte((4 * totPos) + c, registeredImageX.getUByte(pos));
                                } else if (c == secondColor) {
                                    registeredSrcImage.setUByte((4 * totPos) + c, registeredImageY.getUByte(pos));
                                } else {
                                    registeredSrcImage.setUByte((4 * totPos) + c, srcImage.getUByte((4 * totPos) + c));
                                }
                            }
                        }
                    }
                } // if (srcImage.getType() == ModelStorageBase.ARGB)
                else { // srcImage.getType() == ModelStorageBase.ARGB_USHORT

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            pos = i + (xDim * j);
                            totPos = pos + zPos;

                            for (c = 0; c <= 3; c++) {

                                if (c == color) {
                                    registeredSrcImage.setUShort((4 * totPos) + c, registeredImageX.getUShort(pos));
                                }

                                if (c == secondColor) {
                                    registeredSrcImage.setUShort((4 * totPos) + c, registeredImageY.getUShort(pos));
                                } else {
                                    registeredSrcImage.setUShort((4 * totPos) + c,
                                                                 srcImage.getUShort((4 * totPos) + c));
                                }
                            }
                        }
                    }
                } // else srcImage.getType() == ModelStorageBase.ARGB_USHORT

                registeredImageX.disposeLocal();
                registeredImageX = null;
                registeredImageY.disposeLocal();
                registeredImageY = null;
            } // for (z = 0; z < zDim; z++)

            updateFileInfo(srcImage, registeredSrcImage);
            registeredSrcImage.calcMinMax();

            if (useRed) {
                bin1 = Math.min(bin1,
                                (int) Math.round(registeredSrcImage.getMaxR() - registeredSrcImage.getMinR() + 1));
            } else {
                bin1 = Math.min(bin1,
                                (int) Math.round(registeredSrcImage.getMaxG() - registeredSrcImage.getMinG() + 1));
            }

            if (useBlue) {
                bin2 = Math.min(bin2,
                                (int) Math.round(registeredSrcImage.getMaxB() - registeredSrcImage.getMinB() + 1));
            } else {
                bin2 = Math.min(bin2,
                                (int) Math.round(registeredSrcImage.getMaxG() - registeredSrcImage.getMinG() + 1));
            }

            int[] newExtents = new int[2];
            newExtents[0] = bin1 + leftPad + rightPad;
            newExtents[1] = bin2 + bottomPad + topPad;
            destImage.changeExtents(newExtents);

            try {
                imageFrame = new ViewJFrameImage(registeredSrcImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Unable to open frame for registered image");
                setCompleted(false);

                return;
            }
        } // if (register)
        else {
            registeredSrcImage = srcImage;
        }

        try {
            buffer = new float[volume];
            secondBuffer = new float[volume];
            histBuffer = new double[(bin1 + leftPad + rightPad) * (bin2 + bottomPad + topPad)];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            secondBuffer = null;
            histBuffer = null;
            errorCleanUp("Algorithm ColocalizationEM: Out of memory", true);

            return;
        }

        for (i = 0; i < histBuffer.length; i++) {
            histBuffer[i] = 0.0;
        }

        destImage.releaseLock(); // we need to be able to alter the dest image

        try {

            try {
                fireProgressStateChanged(srcImage.getImageName(), "Creating 2D Colocolization Histogram ...");
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm " +
                                      "and dispose the progressbar between checking for " +
                                      "threadStopping and using it.", Preferences.DEBUG_ALGORITHM);
                }
            }

            if (useRed) {
                color = 1;
            } else {
                color = 2;
            }

            if (useBlue) {
                secondColor = 3;
            } else {
                secondColor = 2;
            }

            m = 0;

            if (srcImage.getType() == ModelStorageBase.ARGB) {

                for (k = 0; k <= (zDim - 1); k++) {

                    for (j = 0; j <= (yDim - 1); j++) {

                        for (i = 0; i <= (xDim - 1); i++) {
                            pos = i + (xDim * j) + (length * k);
                            buffer[m] = registeredSrcImage.getUByte((4 * pos) + color);
                            secondBuffer[m++] = registeredSrcImage.getUByte((4 * pos) + secondColor);
                        }
                    }
                }
            } // if (srcImage.getType() == ModelStorageBase.ARGB)
            else { // srcImage.getType() == ModelStorageBase.ARGB_USHORT

                for (k = 0; k <= (zDim - 1); k++) {

                    for (j = 0; j <= (yDim - 1); j++) {

                        for (i = 0; i <= (xDim - 1); i++) {
                            pos = i + (xDim * j) + (length * k);
                            buffer[m] = registeredSrcImage.getUShort((4 * pos) + color);
                            secondBuffer[m++] = registeredSrcImage.getUShort((4 * pos) + secondColor);
                        }
                    }
                }
            } // else srcImage.getType() == ModelStorageBase.ARGB_USHORT

            for (i = 0; i < volume; i++) {

                if (inputMask.get(i)) {
                    voiSize++;

                    if (doOr) {

                        if ((buffer[i] >= threshold1) || (secondBuffer[i] >= threshold2)) {
                            voiIntensity1 += buffer[i];
                            voiIntensity2 += secondBuffer[i];
                        }
                    } // if (doOr)
                    else { // and thresholding

                        if ((buffer[i] >= threshold1) && (secondBuffer[i] >= threshold2)) {
                            voiIntensity1 += buffer[i];
                            voiIntensity2 += secondBuffer[i];
                        }
                    } // else and thresholding
                } // if (inputMask.get(i))
            } // for (i = 0; i < volume; i++)

            min1 = Double.MAX_VALUE;
            max1 = -Double.MAX_VALUE;
            min2 = Double.MAX_VALUE;
            max2 = -Double.MAX_VALUE;

            for (i = 0; i < volume; i++) {

                if ((buffer[i] > max1) && inputMask.get(i)) {
                    max1 = buffer[i];
                }

                if ((buffer[i] < min1) && inputMask.get(i)) {
                    min1 = buffer[i];
                }

                if ((secondBuffer[i] > max2) && inputMask.get(i)) {
                    max2 = secondBuffer[i];
                }

                if ((secondBuffer[i] < min2) && inputMask.get(i)) {
                    min2 = secondBuffer[i];
                }
            }

            range1 = max1 - min1;
            range2 = max2 - min2;
            scale1 = (bin1 - 1) / range1;
            scale2 = (bin2 - 1) / range2;
            // The first bin goes from min1 to min1 + (max1 - min1)/(bin1-1)
            // The second bin goes from min1 + (max1 - min1)/(bin1-1) to
            // min1 + 2*(max1 - min1)/(bin1-1)
            // The last bin goes from min1 + (bin1-2)*(max1 - min1)/(bin1-1)
            // to max1

            emfunction();
            segBuffer = new byte[volume];

            fireProgressStateChanged("Generating histogram buffer");

            for (i = 0, n = 0; i < volume; i++) {

                if (doOr) {

                    if (((buffer[i] >= threshold1) || (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {
                        ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                        ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                        // invert y
                        histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
                        segBuffer[i] = (byte) ((255 * (mostProbableClass[n++] + 1)) / gaussians);
                    }
                } // if (doOr)
                else { // and thresholding

                    if (((buffer[i] >= threshold1) && (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {
                        ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                        ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                        // invert y
                        histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
                        segBuffer[i] = (byte) ((255 * (mostProbableClass[n++] + 1)) / gaussians);
                    }
                } // else and thresholding
            } // for (i = 0; i < volume; i++)

            if (threadStopped) { // do before copying back into image
                finalize();

                return;
            }

            destImage.importData(0, histBuffer, true);
            destRes = new float[2];
            destRes[0] = 1.0f;
            destRes[1] = 1.0f;
            destImage.getFileInfo(0).setResolutions(destRes);
            segImage.importData(0, segBuffer, true);
            updateFileInfo(srcImage, segImage);
            segImage.clearMask();

            try {
                new ViewJFrameImage(segImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Unable to open frame for segImage");
                setCompleted(false);

                return;
            }


        } // try
        catch (IOException error) {
            displayError("Algorithm ColocalizationEM reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            secondBuffer = null;
            displayError("Algorithm ColocalizationEM reports: out of memory" + e);
            setCompleted(false);

            return;
        }

        frameList = srcImage.getImageFrameVector();

        for (i = 0; i < frameList.size(); i++) {

            if ((((ViewJFrameBase) frameList.elementAt(i)).getControls()) != null) {
                controlFrame = ((ViewJFrameBase) frameList.elementAt(i));
            }
        }

        fireProgressStateChanged("Creating ColocalizationEM frame");
        frameColocalize = new ViewJFrameColocalizationEM(registeredSrcImage, null, baseImage, null, null, destImage,
                                                         controlFrame, useRed, useGreen, useBlue, min1, max1, min2,
                                                         max2, scale1, scale2, leftPad, rightPad, bottomPad, topPad,
                                                         mean, halfMajor, halfMinor, theta);
        if (mean != null) {

            for (i = 0; i < mean.length; i++) {
                mean[i] = null;
            }

            mean = null;
        }

        setCompleted(true);
    }

    /**
     * DOCUMENT ME!
     */
    private void emfunction() {
        int i, j, d, n;
        int iter;
        double[] initVar = new double[2];
        double denom;
        double[] initMean = new double[2];
        double[] initStd = new double[2];
        double lambda1, lambda2;
        double lpart;
        double highestProbability;

        dMin[0] = Double.MAX_VALUE;
        dMin[1] = Double.MAX_VALUE;
        dMax[0] = -Double.MAX_VALUE;
        dMax[1] = -Double.MAX_VALUE;

        for (i = 0, n = 0; i < buffer.length; i++) {

            if (doOr) {

                if (((buffer[i] >= threshold1) || (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {

                    if (buffer[i] < dMin[0]) {
                        dMin[0] = buffer[i];
                    }

                    if (secondBuffer[i] < dMin[1]) {
                        dMin[1] = secondBuffer[i];
                    }

                    if (buffer[i] > dMax[0]) {
                        dMax[0] = buffer[i];
                    }

                    if (secondBuffer[i] > dMax[1]) {
                        dMax[1] = secondBuffer[i];
                    }

                    count++;
                }
            } // if (doOr)
            else { // and thresholding

                if (((buffer[i] >= threshold1) && (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {

                    if (buffer[i] < dMin[0]) {
                        dMin[0] = buffer[i];
                    }

                    if (secondBuffer[i] < dMin[1]) {
                        dMin[1] = secondBuffer[i];
                    }

                    if (buffer[i] > dMax[0]) {
                        dMax[0] = buffer[i];
                    }

                    if (secondBuffer[i] > dMax[1]) {
                        dMax[1] = secondBuffer[i];
                    }

                    count++;
                }
            } // else and thresholding
        } // for (i = 0, n = 0; i < buffer.length; i++)

        b1 = new float[count];
        b2 = new float[count];

        probX = new double[count];
        probXOld = new double[count];
        mostProbableClass = new byte[count];

        for (i = 0, n = 0; i < buffer.length; i++) {

            if (doOr) {

                if (((buffer[i] >= threshold1) || (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {
                    b1[n] = buffer[i];
                    b2[n++] = secondBuffer[i];
                }
            } // if (doOr)
            else { // and thresholding

                if (((buffer[i] >= threshold1) && (secondBuffer[i] >= threshold2)) && inputMask.get(i)) {
                    b1[n] = buffer[i];
                    b2[n++] = secondBuffer[i];
                }
            } // else and thresholding
        } // for (i = 0, n = 0; i < buffer.length; i++)

        initMean[0] = 0.0;
        initMean[1] = 0.0;

        for (i = 0; i < count; i++) {
            initMean[0] += b1[i];
            initMean[1] += b2[i];
        }

        initMean[0] /= count;
        initMean[1] /= count;
        initVar[0] = 0.0;
        initVar[1] = 0.0;

        for (i = 0; i < count; i++) {
            initVar[0] += (b1[i] - initMean[0]) * (b1[i] - initMean[0]);
            initVar[1] += (b2[i] - initMean[1]) * (b2[i] - initMean[1]);
        }

        initVar[0] /= count - 1;
        initVar[1] /= count - 1;
        initStd[0] = Math.sqrt(initVar[0]);
        initStd[1] = Math.sqrt(initVar[1]);

        for (j = 0; j < gaussians; j++) {
            prob[j] = 1.0 / (double) gaussians;
            rho[j] = 1.98 * (Math.random() - 0.5);

            // Means vary from 0.75*min + 0.25*max to 0.25*min + 0.75*max
            for (d = 0; d < 2; d++) {
                mean[j][d] = ((dMax[d] + dMin[d]) / 2.0) + ((Math.random() - 0.5) * (dMax[d] - dMin[d]) / 2.0);
                std[j][d] = 0.1 * initStd[d] * (1 + (4 * Math.random()));
            }
        }

        for (iter = 0; iter < iterations; iter++) {
            fireProgressStateChanged("iteration = " + (iter + 1));
            fireProgressStateChanged(100 * iter / iterations);
            newToOld();
            getProbX();
            getProbXOld();

            // Calculate new mean and variance
            for (j = 0; j < gaussians; j++) {
                denom = 0.0;

                for (i = 0; i < count; i++) {
                    denom += probjxOld(i, j);
                }

                mean[j][0] = 0.0;

                for (i = 0; i < count; i++) {
                    mean[j][0] += probjxOld(i, j) * b1[i];
                }

                mean[j][0] /= denom;
                mean[j][1] = 0.0;

                for (i = 0; i < count; i++) {
                    mean[j][1] += probjxOld(i, j) * b2[i];
                }

                mean[j][1] /= denom;

                std[j][0] = 0.0;

                for (i = 0; i < count; i++) {
                    std[j][0] += probjxOld(i, j) * (b1[i] - mean[j][0]) * (b1[i] - mean[j][0]);
                }

                std[j][0] /= denom;
                std[j][0] = Math.sqrt(std[j][0]);

                std[j][1] = 0.0;

                for (i = 0; i < count; i++) {
                    std[j][1] += probjxOld(i, j) * (b2[i] - mean[j][1]) * (b2[i] - mean[j][1]);
                }

                std[j][1] /= denom;
                std[j][1] = Math.sqrt(std[j][1]);

                for (i = 0; i < count; i++) {
                    rho[j] += probjxOld(i, j) * (b1[i] - mean[j][0]) * (b2[i] - mean[j][1]) / (std[j][0] * std[j][1]);
                }

                rho[j] /= denom;

                if (rho[j] >= 0.99) {
                    rho[j] = 0.99;
                }

                if (rho[j] <= -0.99) {
                    rho[j] = -0.99;
                }

                /*
                 * The covariance matrix is: std[j][0]*std[j][0]        rho[j]*std[j][0]*std[j][1]
                 * rho[j]*std[j][0]*std[j][1]    std[j][1]*std[j][1] lambda1 and lambda2 are the eigenvalues of the
                 * covariance matrix The corresponding eigenvalues give the variances along the respective principal
                 * directions. Then the eigenvector corresponding to the major axis is (std[j][0]*std[j][0] - lambda1,
                 * rho[j]*std[j][0]*std[j][1])
                 */
                lpart = Math.sqrt((((std[j][0] * std[j][0]) + (std[j][1] * std[j][1])) *
                                       ((std[j][0] * std[j][0]) + (std[j][1] * std[j][1]))) -
                                  (4.0 * std[j][0] * std[j][0] * std[j][1] * std[j][1] * (1.0 - (rho[j] * rho[j]))));
                lambda1 = ((std[j][0] * std[j][0]) + (std[j][1] * std[j][1]) + lpart) / 2.0;
                lambda2 = ((std[j][0] * std[j][0]) + (std[j][1] * std[j][1]) - lpart) / 2.0;
                halfMajor[j] = Math.sqrt(lambda1);
                halfMinor[j] = Math.sqrt(lambda2);

                if (lambda1 == lambda2) {
                    theta[j] = 0.0;
                } else {
                    theta[j] = Math.atan2(((std[j][0] * std[j][0]) - lambda1), (rho[j] * std[j][0] * std[j][1]));
                }

                prob[j] = denom / (double) count;
            }

            Preferences.debug("Iteration = " + (iter + 1) + "\n", Preferences.DEBUG_ALGORITHM);

            for (j = 0; j < gaussians; j++) {
                Preferences.debug("Gaussian = " + (j + 1) + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Probability = " + prob[j] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("rho = " + rho[j] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Major half axis = " + halfMajor[j] + " " + " Minor half axis = " + halfMinor[j] +
                                  "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Rotation angle to major half axis = " + theta[j] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Mean x = " + mean[j][0] + " Mean y = " + mean[j][1] + "\n", Preferences.DEBUG_ALGORITHM);
            }
        } // for (iter = 0; iter < iterations; iter++)

        // Calculate the most probable class for each point
        for (i = 0; i < count; i++) {
            highestProbability = -1;

            for (j = 0; j < gaussians; j++) {

                if (probjxOld(i, j) > highestProbability) {
                    highestProbability = probjxOld(i, j);
                    mostProbableClass[i] = (byte) j;
                }
            } // for (j = 0; j < gaussians; j++)
        } // for (i = 0; i < count; i++)

        fireProgressStateChanged(100);
    }

    /**
     * DOCUMENT ME!
     */
    private void getProbX() {
        int i, j;
        double temp;

        for (i = 0; i < count; i++) {
            temp = 0.0;

            for (j = 0; j < gaussians; j++) {
                temp += probXj(i, j) * prob[j];
            }

            probX[i] = temp;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void getProbXOld() {
        int i, j;
        double temp;

        for (i = 0; i < count; i++) {
            temp = 0.0;

            for (j = 0; j < gaussians; j++) {
                temp += probXjOld(i, j) * probOld[j];
            }

            probXOld[i] = temp;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void newToOld() {
        int d, j;

        for (j = 0; j < gaussians; j++) {
            probOld[j] = prob[j];
            rhoOld[j] = rho[j];

            for (d = 0; d < 2; d++) {
                meanOld[j][d] = mean[j][d];
                stdOld[j][d] = std[j][d];
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   i  DOCUMENT ME!
     * @param   j  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double probjxOld(int i, int j) {

        if (probXOld[i] == 0.0) {
            return probOld[j];
        } else {
            return probXjOld(i, j) * probOld[j] / probXOld[i];
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   i  DOCUMENT ME!
     * @param   j  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double probXj(int i, int j) {
        double e;
        e = ((b1[i] - mean[j][0]) * (b1[i] - mean[j][0]) / (std[j][0] * std[j][0])) -
            (2.0 * rho[j] * (b1[i] - mean[j][0]) * (b2[i] - mean[j][1]) / (std[j][0] * std[j][1])) +
            ((b2[i] - mean[j][1]) * (b2[i] - mean[j][1]) / (std[j][1] * std[j][1]));
        e /= (-2.0 * (1.0 - (rho[j] * rho[j])));

        return Math.exp(e) / (2.0 * Math.PI * std[j][0] * std[j][1] * Math.sqrt(1.0 - (rho[j] * rho[j])));
    }

    /**
     * DOCUMENT ME!
     *
     * @param   i  DOCUMENT ME!
     * @param   j  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double probXjOld(int i, int j) {
        double e;
        e = ((b1[i] - meanOld[j][0]) * (b1[i] - meanOld[j][0]) / (stdOld[j][0] * stdOld[j][0])) -
            (2.0 * rhoOld[j] * (b1[i] - meanOld[j][0]) * (b2[i] - meanOld[j][1]) / (stdOld[j][0] * stdOld[j][1])) +
            ((b2[i] - meanOld[j][1]) * (b2[i] - meanOld[j][1]) / (stdOld[j][1] * stdOld[j][1]));
        e /= (-2.0 * (1.0 - (rhoOld[j] * rhoOld[j])));

        return Math.exp(e) / (2.0 * Math.PI * stdOld[j][0] * stdOld[j][1] * Math.sqrt(1.0 - (rhoOld[j] * rhoOld[j])));
    }

}
