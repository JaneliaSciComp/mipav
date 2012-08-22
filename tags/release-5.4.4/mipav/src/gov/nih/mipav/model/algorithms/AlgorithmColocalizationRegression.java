package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;


/**
 * This algorithm creates a 2D histogram from 2 colors of a single image or from 2 black and white images and uses an
 * orthogonal line fit of the histogram data to generate a correlation line thru the histogram. Upper right region
 * rectangles with lower left corners on the correlation line are used as colocalization regions, regions where both of
 * the 2 colors are significantly above background. The colocalization frame is tied to a version of the source image or
 * images, so that only pixels located in the colocalization region are displayed. When not in free range mode, a user
 * movable point along the correlation line allows for the display of source image pixels and statistics associated with
 * different upper right colocalization regions. In free range mode, the point can be moved anywhere in the histogram
 * buffer.
 *
 * <p>If neither registration or background subtraction are selected, then the original source image or images are used
 * in the histogram buffer. If registration is selected and background subtraction is not selected, then the registered
 * image or images are used in the histogram buffer. If registration is not selected and background subtraction is
 * selected, then the subtracted image or images are used in the histogram buffer. If both registration and subtraction
 * are selected, then the background averages are obtained from the original source image or images. Next, registration
 * is performed. Finally, background subtraction is performed on the registered image or images and the subtracted image
 * or images are used for the histogram buffer. If the maximum data range is decreased, then bin1 or bin2 will be
 * decreased if necessary so as not to exceed the actual number of channels in the data range.</p>
 *
 * <p>Note that the background selection voi is always blue and the voi to limit analysis to a region of the image is
 * assumed to be a different color.</p>
 *
 * <p>An optional registration may be performed before colocalization. In this registration both images or colors are
 * moved halfway to a common center point. The registration is always done on an entire image. AlgorithmRegOAR2D is used
 * with the cost function being the only registration parameter the user can vary in the dialog box. Correlation ratio
 * is the default, but the user can also select least squares, normalized cross correlation, or normalized mutual
 * information. The colocalization will be performed on the registered image or images rather than on the original
 * image.</p>
 *
 * <p>The amount of colocalization can be measured with the linear correlation coefficient. Let the 2 channels be R and
 * G, the average values be Rav and Gav. Then the linear correlation coefficient = num/denom with num = sum over i of
 * ((R(i) - Rav) * (G(i) - Gav)) denom = square root[(sum over i of (R(i) - Rav)*(R(i) - Rav)) * (sum over i of (G(i) -
 * Gav)* (G(i) - Gav))] This is equivalent to the more traditional form of the equation: with num = Nsum(R(i)G(i)) -
 * sum(R(i))sum(G(i)) denom = sqrt[Nsum(R(i)*R(i)) - (sumR(i))*(sumR(i))] * sqrt[Nsum(G(i)*G(i)) - (sumG(i))*(sumG(i))]
 * The linear correlation coefficient ranges from -1 to 1 with 0 the value for random patterns and 1 the value for 100%
 * colocalization. Negative values of the coefficient are not used for colocalization since that would indicate an
 * anticolocalized situation with a bright pixel in one channel implying a dim pixel in another channel. Note that the
 * medical literature on colocalization refers to the traditional linear correlation coefficient as Pearson's
 * coefficient.</p>
 *
 * <p>The P-value statistic measures the portion of the linear correlation coefficient values of images with 1 randomly
 * scrambled channel whose values are less than the linear correlation coefficient value of the unscrambled image. 200
 * randomizations are performed resulting in a fairly accurate significance test with a maximum P-value of 0.995. A
 * P-value of 0.95 or more is somewhat arbitrarily used to indicate the existence of true colocalization. The P-value
 * option was not found to be useful and is currently disabled.</p>
 *
 * <p>Pixel intensity values are spatially correlated because of the point spread function. Therefore, the number of
 * independent samples in an image is not the number of pixels in the image but rather the number of objects the size of
 * a point spread function that will fit into the image. A normalized autocovariance is calculated for each
 * channel(although much of the medical literature erroneously calls it an autocorrelation). The channel whose
 * autocovariance is smallest is divided up into squares or cubes with sides equal to the full width at half maximum of
 * the smallest autocovariance. These squares or cubes are randomly placed in forming 200 randomly colocalized images.
 * </p>
 *
 * <p>Use linear least squares to calculate the best line fit for secondBuffer = a*firstBuffer + b. Use an orthogonal
 * line fit where the minimized distance measurements are orthgonal to the proposed line. This is an orthogonal
 * regression as opposed to a traditional direct regression of dependent y variable green on independent red variable x
 * which would minimize the vertical distances between the points and the fitted line. An inverse regression of red on
 * green would minimize the horizontal distances between the points and the fitted line. All 3 lines pass through the
 * point (averagex, averagey), but the 3 lines only coincide if the correlation coefficient equals -1 or 1. Use of a
 * direct regression only makes sense if one variable is dependent and one variable is independent. Since red and green
 * dyes used in colocalization are best thought of as 2 independent variables, the orthogonal regression is most
 * appropriate. Note that orthogonal regression is also called total least squares.</p>
 *
 * <p>In the first iteration points which have a dataless background in both images are excluded from the regression
 * line calculations. If buffer[i] < background1 and secondBuffer[i] < background2, the point i is not included in the
 * regression line calculations. If the image is not floating point or double, background1 and background2 are set at
 * 1.0. If the image is floating point or double, background1 and background2 are set at image minimums.</p>
 *
 * <p>In the dialog the user can select to use points from the entire image or only points from the VOI region or only
 * points allowed by a mask file. If VOI region is selected, then when a mouseReleased is performed on the VOI, the
 * calculation is performed again. This allows the calculation to be performed with the same contour VOI in different
 * positions. If the mask file radio button is selected, then the Choose mask file button will be enabled. When the
 * Choose mask file button is pressed, a dialog for selecting a mask file will appear.</p>
 *
 * <p>The second iteration thresholding works the same as the first iteration thresholding except for the line
 * determination. In the first line determination iteration points are only excluded if they are below both background1
 * and background2, but in the second iteration line determination points are excluded if they are below either
 * latPositive1 or lastPositive2. (lastPositive1,lastPositive2) is the last point going from top to bottom on the first
 * iteration regression line before a L shaped subthreshold region with a zero or negative linear correlation
 * coefficient is found. In the second iteration the L shaped subthreshold region determined from the first iteration is
 * excluded from the regression line calculations. The default is to not perform a second iteration. The second
 * iteration thresholding is not applied to the linear correlation coefficients, % colocalization area, % first color
 * colocalization, or the % second color colocalization. The second iteration thresholding is just used for line
 * generation with exclusion of points in the L shaped region.</p>
 *
 * <p>The slope and offset of the orthogonal regression line can be obtained from the eigenvector of the smallest
 * eigenvalue of the matrix: (count-1)*sample y variance -(count-1)*sample xy covariance -(count-1)*sample xy covariance
 * (count-1)*sample x variance The eigenvector has the components (directionx, directiony), where the slope of the line
 * equals = directiony/directionx offset = averagey - slope*averagex However, the method used here is to obtain the
 * slope from the equation: slope = num/denom num = vary - varx + sqrt((vary - varx)*(vary - varx) + 4*covarxy*covarxy)
 * denom = 2*covarxy Again offset = averagey - slope*averagex</p>
 *
 * <p>Let n be the normal vector to the orthogonal regression line and let theta be the angle from the x axis to the
 * normal. theta = arctan(slope) n = averagex*cos(theta) + averagey*sin(theta) The minimized residue, the mean square
 * error, is given by MSE = varx*cos(theta)*cos(theta) + covarxy*sin(2*theta) + vary*sin(theta)*sin(theta) = (varx +
 * 2*slope*covarxy + slope*slope*vary)/(1 + slope*slope)</p>
 *
 * <p>To avoid including outliers in the regression line the intital regression line calculation may be iterated using
 * only points whose distance from the line is is within +-2 times the square root of the mean square error. This will
 * include 95% of the points. The iteration is performed until the fractional changes in the slope and offsets are <=
 * 0.001 or 10 iterations have been performed. Iteration excluding outliers is currently disabled.</p>
 *
 * <p>Note that there are 2 different types of regression line iterations. The first is the 1 or 2 iterations with the
 * first iteration excluding only points with both buffer[i] < background1 and secondBuffer[i] < background2 and the
 * second iteration excluding points with either buffer[i] < lastPositive1 or secondBuffer[i] < lastPositive2. This
 * iteration does not lead to a convergence. Within each of these 1 or 2 major iterations a converging set of iterations
 * may performed to exclude the outliers beyond +- 2 times the square root of the mean square error from the
 * calculation. However, these outlier iterations are currently disabled.</p>
 *
 * <p>A histogram buffer is generated with bin1 counts along the x axis and bin2 counts along the y axis. bin1 and bin2
 * are user selectable in the dialog. leftPad, rightPad, bottomPad, and topPad spaces are left around the data area in
 * histBuffer to allow room for axis lines and labels. For bin1 and bin2 in color images the default bin number = color
 * max - color min + 1.</p>
 *
 * <p>Calculate the linear correlation coefficients for all pixels whose values are either below threshold in buffer or
 * are below a*threshold + b in secondBuffer. The values in buffer range from min1 to max1 and in secondBuffer range
 * from min2 to max2. Calculate along the color that has the greatest range along the line segment. The line segment's
 * (color1,color2) point just above the point where the linear correlation coefficient becomes negative or zero is taken
 * in a nonrigorous fashion as the threshold point separating colocalized data from noncolocalized data.</p>
 *
 * <p>Originally every point along the color that has the greatest range along the line segment was included in the
 * calculation. However, the time required was unacceptable for large images. Only those points necessary to find the
 * transition point from negative to positive correlation coefficient are calculated. This is approximately the log to
 * the base 2 of the number of points along the greatest color range. First the midpoint is calculated. If the midpoint
 * linear correlation coefficient is positive, then the 1/4 point is calculated. If the midpoint linear correlation
 * coefficient is negative, then the 3/4 point is calculated. Each new line segment increment to a new point is 1/2 the
 * size of the previous line segment increment. The process stops when it finds a point with a negative or undefined
 * linear correlation coefficient followed by a point with a positive correlation coefficient. When the mouse is dragged
 * along the line, the histogram frame header display only changes when the mouse passes over a point for which the
 * correlation and colocalizations have been calculated. On a mouse release the correlation and colocalizations will be
 * performed at that point if they have not already been done.</p>
 *
 * <p>Calculate the percent colocalization area, the percent red colocalization, and the percent green colocalization
 * for each point along the correlation line. Each point along the correlation line has a red value = threshold and a
 * green value = secondThreshold. Thus, each point defines an upper right rectanglular region characterized by red >=
 * threshold and green >= secondThreshold. The percent colocalization area is given by 100 times the number of pixels
 * belonging to the upper right rectangular region divided by the number of pixels in the entire image or the selected
 * voi. The percent red colocalization is given by 100 times the sum of the red values in the upper right rectangular
 * region divided by the sum of the red values in the image or the selected voi. The percent green colocalization is
 * given by 100 times the sum of the green values in the upper right rectangular region divided by the sum of the green
 * values in the image or the selected voi. Note that only pixels which equal or exceed at least 1 of the 2 user
 * specified thresholds are included in any of the above sums.</p>
 *
 * <p>In the default option for a color colocalization all pixel values > background are included in the sum in the
 * denominator. In a user selected option only pixels whose values are >= the threshold for the color being colocalized
 * will be included in the denominator sum. For example, in the threshold only option red colocalization is given by 100
 * times the sum of the red values in the upper right rectangular region divided by the sum of the red values >= red
 * threshold in the image or selected voi.</p>
 *
 * <p>Pressing the free region toggle button on the histogram frame allows the user to take the VOI point off the least
 * squares fit line. These correlation and colocalization values are only calculated in response to a mouse release.</p>
 *
 * <p>There are 2 nonrigorous aspects associated with this algorithm. First, a single line is likely derived from at
 * least 4 populations - (no color1, no color2), (no color1, color2), (color1, no color2), (color1, color2) to describe
 * the characteristics of only the (color1, color2) population. Second, a more traditional statistical approach would
 * use data within a fixed perpindicular distance from the line as being validly described by the line.</p>
 *
 * <p>The calculated data is passed to ViewJFrameColocalization. References: 1.) Protein-protein interaction quantified
 * by microscopic co-localization in live cells by Sylvain V. Costes, Dirk Daelemans, Edward H. Cho, George Pavlakis,
 * and Stephen Lockett</p>
 *
 * <p>2.) Dynamics of three-dimensional replication patterns during the S-phase, analysed by double labelling of DNA and
 * confocal microscopy by E.M.M.Manders, J.Stap, G.J.Brakenhoff, R.Van Driel, and J.A.Aten, Journal of Cell Science, Vol
 * 103, 1992, pp. 857-862.</p>
 *
 * <p>3.) Measurement of co-localization of objects in dual-colour confocal images by E.M.M.Manders, F.J.Verbeek, and
 * J.A.Aten, Journal of Microscopy, Vol. 169, Pt. 3, March, 1993, pp. 375-382.</p>
 *
 * <p>5.) Equations for slope and offset of the orthogonal least squares problem from A Critical Examination of
 * Orthogonal Regression and an application to tests of firm size interchangeability by Gishan Dissanaike and Shiyun
 * Wang</p>
 *
 * <p>6.) Equations for slope and mean square error of the orthogonal least squares from Straight Line Extraction Using
 * Iterative Total Least Squares Methods by Jan A. Van Mieghem, Hadar I. Avi-Itzhak, and Roger D. Melen, Journal of
 * Visual Communication and Image Representation, Vol. 6, No. 1, March, 1995, pp. 59-68.</p>
 */
public class AlgorithmColocalizationRegression extends AlgorithmBase implements UpdateVOISelectionListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * generated with randomly scrambled blocks of pixels having a linear correlation coefficient less than that of the
     * actual image.
     */
    private float a, b; // The coefficients for the linear least squares

    /** The minimum values that are considered as data values. */
    private float background1 = 1.0f;

    /** DOCUMENT ME! */
    private float background2 = 1.0f;

    /** DOCUMENT ME! */
    private int backgroundIndex = -1;

    /** DOCUMENT ME! */
    private int backgroundIndex2 = -1;

    /** DOCUMENT ME! */
    private ModelImage baseImage = null;

    /** DOCUMENT ME! */
    private int bin1 = 256; // Histogram x axis bin number

    /** DOCUMENT ME! */
    private int bin2 = 256; // Histogram y aixs bin number

    /** DOCUMENT ME! */
    private int bottomPad = 40;

    /** DOCUMENT ME! */
    private float[] buffer;

    /** DOCUMENT ME! */
    private float[] colocIntensity1;

    /** DOCUMENT ME! */
    private float[] colocIntensity2;

    /** for all pixels with values either below color1 for buffer or below a*color1 + b for secondBuffer. */
    private float[] colocSize;

    /** DOCUMENT ME! */
    private int color;

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private boolean doColocWithThresholds = true;

    /** DOCUMENT ME! */
    private boolean doP = false; // Calculate the P-value

    /** If true, do second iteration excluded subtrhesholded region from first iteration. */
    private boolean doSecondIteration = false;
    @SuppressWarnings("unused")
    private boolean doVOISubtraction = false;

    /** DOCUMENT ME! */
    private boolean entireImage = true;

    /** DOCUMENT ME! */
    private ViewJFrameColocalizationRegression frameColocalize = null;

    /** DOCUMENT ME! */
    private float[] freeRangeColocIntensity1 = null;

    /** DOCUMENT ME! */
    private float[] freeRangeColocIntensity2 = null;

    /** DOCUMENT ME! */
    private float[] freeRangeColocSize = null;

    /**
     * The linear correlation coefficients for all pixels with values either below color1 for buffer or below color2 for
     * secondBuffer.
     */
    private float[] freeRangeRThreshold = null;

    /** If true, the freeRangeRThreshold value has been calculated. */
    private boolean[] haveFreeRangeThreshold = null;

    /** DOCUMENT ME! */
    private boolean[] haveThreshold; // If true, the rThreshold value has been calculated

    /** DOCUMENT ME! */
    private float[] hsb;

    /** DOCUMENT ME! */
    private float hue;

    /** on the contour VOI occurs. */
    @SuppressWarnings("unused")
    private ViewJFrameImage imageFrame = null;

    @SuppressWarnings("unused")
    private ViewJFrameImage imageFrame2 = null;

    @SuppressWarnings("unused")
    private ViewJFrameImage imageFrame3 = null;

    @SuppressWarnings("unused")
    private ViewJFrameImage imageFrame4 = null;

    /** DOCUMENT ME! */
    private BitSet inputMask = null;

    /** Space padding around the histogram data area. */
    private int leftPad = 40;

    /** DOCUMENT ME! */
    private double lineMin1, lineMin2;

    /** DOCUMENT ME! */
    private boolean maskSupplied = false;

    /** DOCUMENT ME! */
    private double min1, min2, scale1, scale2;

    /** DOCUMENT ME! */
    private int nVOIs;

    /** DOCUMENT ME! */
    private float point1 = 0.0f;

    /** DOCUMENT ME! */
    private float point2 = 0.0f;

    /** If pointCalculation is true, go to free range mode and set location at point1, point2. */
    private boolean pointCalculation = false;

    /** DOCUMENT ME! */
    private float PValue = -1.0f; // statistic giving the portion of images

    /** DOCUMENT ME! */
    private float r = 0.0f; // linear correlation coefficient

    /** DOCUMENT ME! */
    private boolean redo = false; // redo is set true when a mouseReleased

    /** DOCUMENT ME! */
    private int regionIndex = -1;

    /** DOCUMENT ME! */
    private boolean register;

    /** DOCUMENT ME! */
    private ModelImage registeredBaseImage = null;

    /** DOCUMENT ME! */
    private ModelImage registeredSrcImage = null;

    /** DOCUMENT ME! */
    private int rightPad = 20;

    /** fitted line to secondBuffer = a*buffer + b;. */
    private float[] rThreshold; // The linear correlation coefficients

    /** DOCUMENT ME! */
    private float[] secondBuffer;

    /** DOCUMENT ME! */
    private int secondColor;

    /** DOCUMENT ME! */
    private short[] shortMask = null;

    /** DOCUMENT ME! */
    private ModelImage subtractedBaseImage = null;

    /** DOCUMENT ME! */
    private ModelImage subtractedSrcImage = null;

    /** DOCUMENT ME! */
    private short[] subtractionMask = null;

    /** DOCUMENT ME! */
    private short[] subtractionMask2 = null;

    /** thresholds used in linear correlation calculations if doSecondIteration is true. */
    private float t1, t2;

    /** DOCUMENT ME! */
    private int thirdColor;

    /** DOCUMENT ME! */
    private boolean thresholdOn1;

    /** DOCUMENT ME! */
    private int thrLength;

    /** DOCUMENT ME! */
    private int topPad = 20;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    /** DOCUMENT ME! */
    private float voiIntensity1 = 0.0f;

    /** DOCUMENT ME! */
    private float voiIntensity2 = 0.0f;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs = null;

    /** DOCUMENT ME! */
    private int voiSize = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which 2D histogram is placed in a predetermined destination image. Used for 2 black and
     * white images
     *
     * @param  destImg                image model where result image is to stored
     * @param  srcImg                 image for x axis of histogram
     * @param  baseImage              image for y axis of histogram
     * @param  mask                   mask for image - use is optional
     * @param  bin1                   histogram x axis bin number
     * @param  bin2                   histogram y axis bin number
     * @param  background1            minimum value for data in sourceImage
     * @param  background2            minimum value for data in baseImage
     * @param  leftPad                space to the left of the histogram bin area
     * @param  rightPad               space to the right of the histogram bin area
     * @param  bottomPad              space to the bottom of the histogram bin area
     * @param  topPad                 space to the top of the histogram bin area
     * @param  doColocWithThresholds  If true, calculate colocalization only using pixels >= threshold
     * @param  entireImage            If false, only use data from VOI region
     * @param  register               If true register 2 black and white images before colocalization
     * @param  cost                   Cost function used in registration
     * @param  doSecondIteration      If true, do second iteration excluding subthresholded region from first iteration
     * @param  doVOISubtraction       If true, create a new image with the average VOI background level subtracted out
     * @param  pointCalculation       If true, go to free range mode and set position at point1, point2
     * @param  point1                 DOCUMENT ME!
     * @param  point2                 DOCUMENT ME!
     */
    public AlgorithmColocalizationRegression(ModelImage destImg, ModelImage srcImg, ModelImage baseImage, BitSet mask,
                                             int bin1, int bin2, float background1, float background2, int leftPad,
                                             int rightPad, int bottomPad, int topPad, boolean doColocWithThresholds,
                                             boolean entireImage, boolean register, int cost, boolean doSecondIteration,
                                             boolean doVOISubtraction, boolean pointCalculation, float point1,
                                             float point2) {

        super(destImg, srcImg);
        this.baseImage = baseImage;
        this.mask = mask;
        this.bin1 = bin1;
        this.bin2 = bin2;
        this.background1 = background1;
        this.background2 = background2;
        this.leftPad = leftPad;
        this.rightPad = rightPad;
        this.bottomPad = bottomPad;
        this.topPad = topPad;
        this.doColocWithThresholds = doColocWithThresholds;
        this.entireImage = entireImage;
        this.register = register;
        this.cost = cost;
        this.doSecondIteration = doSecondIteration;
        this.doVOISubtraction = doVOISubtraction;
        this.pointCalculation = pointCalculation;
        this.point1 = point1;
        this.point2 = point2;

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();

        int i;

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - (2.0f / 3.0f))) < 0.0001f) {

                    if ((backgroundIndex == -1) && doVOISubtraction) {
                        backgroundIndex = i;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 background VOI");

                        return;
                    }
                } else {

                    if (regionIndex == -1) {
                        regionIndex = i;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 region VOI");

                        return;
                    }
                }

            }
        } // for (i = 0; i < nVOIs; i++)

        VOIs = baseImage.getVOIs();
        nVOIs = VOIs.size();

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - (2.0f / 3.0f))) < 0.0001f) {

                    if ((backgroundIndex2 == -1) && doVOISubtraction) {
                        backgroundIndex2 = i;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 background VOI");

                        return;
                    }
                }

            }
        } // for (i = 0; i < nVOIs; i++)

        if ((mask == null) && (regionIndex != -1)) {
            int length = srcImage.getExtents()[0];

            for (i = 1; i < srcImage.getNDims(); i++) {
                length *= srcImage.getExtents()[i];
            }

            shortMask = new short[length];

            for (i = 0; i < length; i++) {
                shortMask[i] = -1;
            }

            shortMask = srcImage.generateVOIMask(shortMask, regionIndex);
            this.mask = new BitSet(length);

            for (i = 0; i < length; i++) {

                if (shortMask[i] == regionIndex) {
                    this.mask.set(i);
                } else {
                    this.mask.clear(i);
                }
            }

            shortMask = null;
        } else if ((mask == null) && (regionIndex == -1)) {
            int length = srcImage.getExtents()[0];

            for (i = 1; i < srcImage.getNDims(); i++) {
                length *= srcImage.getExtents()[i];
            }

            this.mask = new BitSet(length);

            for (i = 0; i < length; i++) {
                this.mask.clear(i);
            }
        } else {
            maskSupplied = true;
        }

        if (backgroundIndex != -1) {
            int length = srcImage.getExtents()[0];

            for (i = 1; i < srcImage.getNDims(); i++) {
                length *= srcImage.getExtents()[i];
            }

            subtractionMask = new short[length];

            for (i = 0; i < length; i++) {
                subtractionMask[i] = -1;
            }

            subtractionMask = srcImage.generateVOIMask(subtractionMask, backgroundIndex);
        } // if (backgroundIndex != -1)

        if (backgroundIndex2 != -1) {
            int length = baseImage.getExtents()[0];

            for (i = 1; i < baseImage.getNDims(); i++) {
                length *= baseImage.getExtents()[i];
            }

            subtractionMask2 = new short[length];

            for (i = 0; i < length; i++) {
                subtractionMask2[i] = -1;
            }

            subtractionMask2 = srcImage.generateVOIMask(subtractionMask2, backgroundIndex2);
        } // if (backgroundIndex2 != -1)

    }

    /**
     * Constructor for images in which 2D histogram is placed in a predetermined destination image. Used for 1 color
     * image.
     *
     * @param  destImg                image model where result image is to stored
     * @param  srcImg                 source image model
     * @param  mask                   mask for image - use optional
     * @param  bin1                   histogram x axis bin number
     * @param  bin2                   histogram y axis bin number
     * @param  background1            minimum data value for first color
     * @param  background2            minimum data value for second color
     * @param  leftPad                space to the left of the histogram bin area
     * @param  rightPad               space to the right of the histogram bin area
     * @param  bottomPad              space to the bottom of the histogram bin area
     * @param  topPad                 space to the top of the histogram bin area
     * @param  useRed                 DOCUMENT ME!
     * @param  useGreen               DOCUMENT ME!
     * @param  useBlue                DOCUMENT ME!
     * @param  doColocWithThresholds  If true, calculate colocalization only using pixels >= threshold
     * @param  entireImage            If false, use only data from VOI region
     * @param  register               If true, register 2 colors before colocalization
     * @param  cost                   Cost function used in registration
     * @param  doSecondIteration      If true, do second iteration excluding subthresholded region from first iteration
     * @param  doVOISubtraction       If true, create a new image with the average VOI background level subtracted out
     * @param  pointCalculation       If true, go to free range mode and set position at point1, point2
     * @param  point1                 DOCUMENT ME!
     * @param  point2                 DOCUMENT ME!
     */
    public AlgorithmColocalizationRegression(ModelImage destImg, ModelImage srcImg, BitSet mask, int bin1, int bin2,
                                             float background1, float background2, int leftPad, int rightPad,
                                             int bottomPad, int topPad, boolean useRed, boolean useGreen,
                                             boolean useBlue, boolean doColocWithThresholds, boolean entireImage,
                                             boolean register, int cost, boolean doSecondIteration,
                                             boolean doVOISubtraction, boolean pointCalculation, float point1,
                                             float point2) {

        super(destImg, srcImg);
        this.mask = mask;
        this.bin1 = bin1;
        this.bin2 = bin2;
        this.background1 = background1;
        this.background2 = background2;
        this.leftPad = leftPad;
        this.rightPad = rightPad;
        this.bottomPad = bottomPad;
        this.topPad = topPad;
        this.useRed = useRed;
        this.useGreen = useGreen;
        this.useBlue = useBlue;
        this.doColocWithThresholds = doColocWithThresholds;
        this.entireImage = entireImage;
        this.register = register;
        this.cost = cost;
        this.doSecondIteration = doSecondIteration;
        this.doVOISubtraction = doVOISubtraction;
        this.pointCalculation = pointCalculation;
        this.point1 = point1;
        this.point2 = point2;

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();

        int i;

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - (2.0f / 3.0f))) < 0.0001f) {

                    if ((backgroundIndex == -1) && doVOISubtraction) {
                        backgroundIndex = i;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 background VOI");

                        return;
                    }
                } else {

                    if (regionIndex == -1) {
                        regionIndex = i;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 region VOI");

                        return;
                    }
                }

            }
        } // for (i = 0; i < nVOIs; i++)

        if ((mask == null) && (regionIndex != -1)) {
            int length = srcImage.getExtents()[0];

            for (i = 1; i < srcImage.getNDims(); i++) {
                length *= srcImage.getExtents()[i];
            }

            shortMask = new short[length];

            for (i = 0; i < length; i++) {
                shortMask[i] = -1;
            }

            shortMask = srcImage.generateVOIMask(shortMask, regionIndex);
            this.mask = new BitSet(length);

            for (i = 0; i < length; i++) {

                if (shortMask[i] == regionIndex) {
                    this.mask.set(i);
                } else {
                    this.mask.clear(i);
                }
            }

            shortMask = null;
        } else if ((mask == null) && (regionIndex == -1)) {
            int length = srcImage.getExtents()[0];

            for (i = 1; i < srcImage.getNDims(); i++) {
                length *= srcImage.getExtents()[i];
            }

            this.mask = new BitSet(length);

            for (i = 0; i < length; i++) {
                this.mask.clear(i);
            }
        } else {
            maskSupplied = true;
        }

        if (backgroundIndex != -1) {
            int length = srcImage.getExtents()[0];

            for (i = 1; i < srcImage.getNDims(); i++) {
                length *= srcImage.getExtents()[i];
            }

            subtractionMask = new short[length];

            for (i = 0; i < length; i++) {
                subtractionMask[i] = -1;
            }

            subtractionMask = srcImage.generateVOIMask(subtractionMask, backgroundIndex);
        } // if (backgroundIndex != -1)

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void calc2DBWStats() {
        int xDim, yDim, zDim;
        int length;
        int i, j;
        int ip;
        int z;
        BitSet inputSliceMask;
        float[] sliceBuffer;
        float[] secondSliceBuffer;
        int voiSliceSize;
        float voiSliceIntensity1;
        float voiSliceIntensity2;
        float[] tmpBuffer;
        float[] tmpSecondBuffer;
        double averagex, averagey;
        int count;
        double num, denom1, denom2, denom;
        float sliceR = 0.0f;
        double sumx, sumx2;
        double sumy, sumy2;
        double sumxy;
        double diffx, diffy;
        double varx, vary, covarxy;
        float mse; // mean square error
        float sliceA, sliceB;
        double min1, max1, min2, max2;
        float[] slRThreshold;
        float[] slColocSize;
        float[] slColocIntensity1;
        float[] slColocIntensity2;
        boolean[] slHaveThreshold;
        boolean slThresholdOn1;
        double slLineMin1, slLineMin2;
        double lineMax1, lineMax2;
        float threshold;
        float secondThreshold;
        float nonPositiveThreshold;
        float minPositiveThreshold;
        float lastPositive1, lastPositive2;
        float slt1, slt2;
        float firstNonPositive1, firstNonPositive2;
        float firstUndefined1, firstUndefined2;
        boolean transitionFound;
        int thresholdIncrement;
        boolean morex, morey;
        float lastx, lasty;
        float voiIntensity1Thr;
        float voiIntensity2Thr;
        float colocAreaPercent;
        float colocIntensityPercent1;
        float colocIntensityPercent2;
        String dataLine1;
        String dataLine2;
        NumberFormat nf;

        xDim = subtractedSrcImage.getExtents()[0];
        yDim = subtractedSrcImage.getExtents()[1];
        zDim = subtractedSrcImage.getExtents()[2];
        length = xDim * yDim;
        inputSliceMask = new BitSet(length);

        for (z = 0; z < zDim; z++) {
            Preferences.debug("Slice = " + (z + 1) + "\n", Preferences.DEBUG_ALGORITHM);
            ip = -1;
            sliceBuffer = new float[length];
            secondSliceBuffer = new float[length];

            for (i = 0; i < length; i++) {

                if (inputMask.get((z * length) + i)) {
                    inputSliceMask.set(i);
                } else {
                    inputSliceMask.clear(i);
                }
            }

            try {
                subtractedSrcImage.exportData(z * length, length, sliceBuffer);
                subtractedBaseImage.exportData(z * length, length, secondSliceBuffer);
            } catch (IOException e) {
                sliceBuffer = null;
                secondSliceBuffer = null;
                errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                return;
            }

            voiSliceSize = 0;
            voiSliceIntensity1 = 0.0f;
            voiSliceIntensity2 = 0.0f;

            for (i = 0; i < length; i++) {

                if (inputSliceMask.get(i)) {
                    voiSliceSize++;

                    if ((sliceBuffer[i] >= background1) || (secondSliceBuffer[i] >= background2)) {
                        voiSliceIntensity1 += sliceBuffer[i];
                        voiSliceIntensity2 += secondSliceBuffer[i];
                    }
                }
            }

            if (voiSliceSize == 0) {
                Preferences.debug("No VOI found", Preferences.DEBUG_ALGORITHM);
                dataLine1 = "slice #" + (z + 1) + " has no VOI\n";
                ViewUserInterface.getReference().setDataText(dataLine1);
                ViewUserInterface.getReference().setDataText("\n");

                continue;
            }

            min1 = Double.MAX_VALUE;
            max1 = -Double.MAX_VALUE;
            min2 = Double.MAX_VALUE;
            max2 = -Double.MAX_VALUE;

            for (i = 0; i < length; i++) {

                if (sliceBuffer[i] > max1) {
                    max1 = sliceBuffer[i];
                }

                if (sliceBuffer[i] < min1) {
                    min1 = sliceBuffer[i];
                }

                if (secondSliceBuffer[i] > max2) {
                    max2 = secondSliceBuffer[i];
                }

                if (secondSliceBuffer[i] < min2) {
                    min2 = secondSliceBuffer[i];
                }
            }

            // Reduce sliceBuffer and secondSliceBuffer so that they only contain the points
            // where (((sliceBuffer[i] >= background1) || (secondSliceBuffer[i] >= background2)) &&
            // inputMask.get(i))
            thrLength = 0;
            tmpBuffer = new float[length];
            tmpSecondBuffer = new float[length];

            for (i = 0; i < length; i++) {

                if (((sliceBuffer[i] >= background1) || (secondSliceBuffer[i] >= background2)) &&
                        inputSliceMask.get(i)) {
                    tmpBuffer[thrLength] = sliceBuffer[i];
                    tmpSecondBuffer[thrLength++] = secondSliceBuffer[i];
                }
            }

            sliceBuffer = new float[thrLength];
            secondSliceBuffer = new float[thrLength];

            for (i = 0; i < thrLength; i++) {
                sliceBuffer[i] = tmpBuffer[i];
                secondSliceBuffer[i] = tmpSecondBuffer[i];
            }

            tmpBuffer = null;
            tmpSecondBuffer = null;
            System.gc();

            averagex = 0.0;
            averagey = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {
                averagex += sliceBuffer[i];
                averagey += secondSliceBuffer[i];
                count++;
            }

            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (i = 0; i < thrLength; i++) {
                num += (sliceBuffer[i] - averagex) * (secondSliceBuffer[i] - averagey);
                denom1 += (sliceBuffer[i] - averagex) * (sliceBuffer[i] - averagex);
                denom2 += (secondSliceBuffer[i] - averagey) * (secondSliceBuffer[i] - averagey);
            }

            denom = Math.sqrt(denom1 * denom2);
            sliceR = (float) (num / denom);
            Preferences.debug("Linear corrleation coefficient = " + sliceR + "\n", Preferences.DEBUG_ALGORITHM);

            sumx = 0.0;
            sumy = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {
                sumx += sliceBuffer[i];
                sumy += secondSliceBuffer[i];
                count++;
            }

            averagex = sumx / count;
            averagey = sumy / count;
            sumx2 = 0.0;
            sumxy = 0.0;
            sumy2 = 0.0;

            for (i = 0; i < thrLength; i++) {
                diffx = sliceBuffer[i] - averagex;
                diffy = secondSliceBuffer[i] - averagey;
                sumx2 += diffx * diffx;
                sumxy += diffx * diffy;
                sumy2 += diffy * diffy;
            }

            float aa;
            varx = sumx2 / (count - 1);
            vary = sumy2 / (count - 1);
            covarxy = sumxy / (count - 1);
            sliceA = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                  (2 * covarxy));
            aa = sliceA * sliceA;
            sliceB = (float) (averagey - (sliceA * averagex));
            mse = (float) (varx + (2 * sliceA * covarxy) + (aa * vary)) / (1 + aa);
            // +-2 standard deviations on each side includes about 95%.

            Preferences.debug("First iteration\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug(baseImage.getImageName() + " = " + sliceA + "*" + srcImage.getImageName() + " + " +
                              sliceB + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

            slLineMin1 = min1;
            slLineMin2 = (sliceA * min1) + sliceB;

            if (slLineMin2 < min2) {
                slLineMin2 = min2;
                slLineMin1 = (min2 - sliceB) / sliceA;
            }

            lineMax1 = max1;
            lineMax2 = (sliceA * max1) + sliceB;

            if (lineMax2 > max2) {
                lineMax2 = max2;
                lineMax1 = (max2 - sliceB) / sliceA;
            }

            firstNonPositive1 = -1;
            firstNonPositive2 = -1;
            minPositiveThreshold = -1;
            nonPositiveThreshold = 1;
            lastPositive1 = (float) (Math.floor(lineMax1) + 1);
            lastPositive2 = (float) (Math.floor(lineMax2) + 1);
            firstUndefined1 = -1;
            firstUndefined2 = -1;

            // Calculate the linear correlation coefficients for all pixels
            // whose values are either below threshold in buffer or are below
            // a*threshold + b in secondBuffer.  The values in buffer range from
            // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
            // along the color that has the greatest range along the line
            // segment.
            if ((lineMax1 - slLineMin1) >= (lineMax2 - slLineMin2)) {
                slThresholdOn1 = true;
                slRThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(slLineMin1)) + 1];
                slColocSize = new float[slRThreshold.length];
                slColocIntensity1 = new float[slRThreshold.length];
                slColocIntensity2 = new float[slRThreshold.length];
                slHaveThreshold = new boolean[slRThreshold.length];

                for (i = 0; i < slRThreshold.length; i++) {
                    slRThreshold[i] = Float.NaN;
                    slColocSize[i] = 0.0f;
                    slColocIntensity1[i] = 0.0f;
                    slColocIntensity2[i] = 0.0f;
                    slHaveThreshold[i] = false;
                }

                transitionFound = false;
                i = slRThreshold.length / 2;
                thresholdIncrement = slRThreshold.length / 4;

                while (!transitionFound) {
                    threshold = (float) (i + Math.ceil(slLineMin1));
                    secondThreshold = (sliceA * threshold) + sliceB;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = sliceBuffer[0];
                    lasty = secondSliceBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {
                        float buff = sliceBuffer[j];
                        float secBuff = secondSliceBuffer[j];

                        if (buff >= threshold) {
                            voiIntensity1Thr += buff;
                        }

                        if (secBuff >= secondThreshold) {
                            voiIntensity2Thr += secBuff;
                        }

                        if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                            slColocSize[i] += 1.0f;
                            slColocIntensity1[i] += buff;
                            slColocIntensity2[i] += secBuff;
                        } else {
                            averagex += buff;
                            averagey += secBuff;
                            count++;

                            if (count == 1) {
                                lastx = buff;
                                lasty = secBuff;
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buff != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secBuff != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    slColocSize[i] = (slColocSize[i] / voiSliceSize) * 100.0f;

                    if (doColocWithThresholds) {
                        slColocIntensity1[i] = (slColocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        slColocIntensity2[i] = (slColocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        slColocIntensity1[i] = (slColocIntensity1[i] / voiSliceIntensity1) * 100.0f;
                        slColocIntensity2[i] = (slColocIntensity2[i] / voiSliceIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {
                            float buff = sliceBuffer[j];
                            float secBuff = secondSliceBuffer[j];

                            if ((buff < threshold) || (secBuff < secondThreshold)) {
                                num += (buff - averagex) * (secBuff - averagey);
                                denom1 += (buff - averagex) * (buff - averagex);
                                denom2 += (secBuff - averagey) * (secBuff - averagey);
                            }
                        } // for (j = 0; j < thrLength; j++)

                        slRThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        slHaveThreshold[i] = true;
                        Preferences.debug("i = " + i + " slRThreshold[i] = " + slRThreshold[i] + "\n",
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                (slRThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = slRThreshold[i - 1];
                            firstNonPositive1 = (float) ((i - 1) + Math.ceil(slLineMin1));
                            firstNonPositive2 = (sliceA * firstNonPositive1) + sliceB;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                            lastPositive2 = (sliceA * lastPositive1) + sliceB;
                        } else if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                       Float.isNaN(slRThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined1 = (float) ((i - 1) + Math.ceil(slLineMin1));
                            firstUndefined2 = (sliceA * firstUndefined1) + sliceB;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                            lastPositive2 = (sliceA * lastPositive1) + sliceB;
                        } else if ((i == 0) && (slRThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                            lastPositive2 = (sliceA * lastPositive1) + sliceB;
                        } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                       (slRThreshold[i] <= 0.0f) && (slRThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = slRThreshold[i];
                            firstNonPositive1 = (float) (i + Math.ceil(slLineMin1));
                            firstNonPositive2 = (sliceA * firstNonPositive1) + sliceB;
                            ip = i + 1;
                            minPositiveThreshold = slRThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(slLineMin1));
                            lastPositive2 = (sliceA * lastPositive1) + sliceB;
                        } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                       Float.isNaN(slRThreshold[i]) && (slRThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined1 = (float) (i + Math.ceil(slLineMin1));
                            firstUndefined2 = (sliceA * firstUndefined1) + sliceB;
                            ip = i + 1;
                            minPositiveThreshold = slRThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(slLineMin1));
                            lastPositive2 = (sliceA * lastPositive1) + sliceB;
                        } else if (slRThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (slRThreshold[i] > 0.0f)
                        else { // ((slRThreshold[i] <= 0.0f) || )Float.isNaN(slRThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= slRThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(slRThreshold[i])
                        slHaveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= slRThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(slRThreshold[i])
                } // while (!transitionFound)

            } // if ((lineMax1 - slLineMin1) >= (lineMax2 - slLineMin2))
            else { // ((lineMax1 - slLineMin1) < (lineMax2 - slLineMin2)
                slThresholdOn1 = false;
                slRThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(slLineMin2)) + 1];
                slColocSize = new float[slRThreshold.length];
                slColocIntensity1 = new float[slRThreshold.length];
                slColocIntensity2 = new float[slRThreshold.length];
                slHaveThreshold = new boolean[slRThreshold.length];

                for (i = 0; i < slRThreshold.length; i++) {
                    slRThreshold[i] = Float.NaN;
                    slColocSize[i] = 0.0f;
                    slColocIntensity1[i] = 0.0f;
                    slColocIntensity2[i] = 0.0f;
                    slHaveThreshold[i] = false;
                }

                transitionFound = false;
                i = slRThreshold.length / 2;
                thresholdIncrement = slRThreshold.length / 4;

                while (!transitionFound) {
                    secondThreshold = (float) (i + Math.ceil(slLineMin2));
                    threshold = (secondThreshold - sliceB) / sliceA;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = sliceBuffer[0];
                    lasty = secondSliceBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {
                        float buff = sliceBuffer[j];
                        float secBuff = secondSliceBuffer[j];

                        if (buff >= threshold) {
                            voiIntensity1Thr += buff;
                        }

                        if (secBuff >= secondThreshold) {
                            voiIntensity2Thr += secBuff;
                        }

                        if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                            slColocSize[i] += 1.0f;
                            slColocIntensity1[i] += buff;
                            slColocIntensity2[i] += secBuff;
                        } else {
                            averagex += buff;
                            averagey += secBuff;
                            count++;

                            if (count == 1) {
                                lastx = buff;
                                lasty = secBuff;
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buff != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secBuff != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    slColocSize[i] = (slColocSize[i] / voiSliceSize) * 100.0f;

                    if (doColocWithThresholds) {
                        slColocIntensity1[i] = (slColocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        slColocIntensity2[i] = (slColocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        slColocIntensity1[i] = (slColocIntensity1[i] / voiSliceIntensity1) * 100.0f;
                        slColocIntensity2[i] = (slColocIntensity2[i] / voiSliceIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {
                            float buff = sliceBuffer[j];
                            float secBuff = secondSliceBuffer[j];

                            if ((buff < threshold) || (secBuff < secondThreshold)) {
                                num += (buff - averagex) * (secBuff - averagey);
                                denom1 += (buff - averagex) * (buff - averagex);
                                denom2 += (secBuff - averagey) * (secBuff - averagey);
                            }
                        } // for (j = 0; j < thrLength; j++)

                        slRThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        slHaveThreshold[i] = true;
                        Preferences.debug("i = " + i + " slRThreshold[i] = " + slRThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                (slRThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = slRThreshold[i - 1];
                            firstNonPositive2 = (float) ((i - 1) + Math.ceil(slLineMin2));
                            firstNonPositive1 = (firstNonPositive2 - sliceB) / sliceA;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                            lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                        } else if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                       Float.isNaN(slRThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined2 = (float) ((i - 1) + Math.ceil(slLineMin2));
                            firstUndefined1 = (firstUndefined2 - sliceB) / sliceA;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                            lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                        } else if ((i == 0) && (slRThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                            lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                        } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                       (slRThreshold[i] <= 0.0f) && (slRThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = slRThreshold[i];
                            firstNonPositive2 = (float) (i + Math.ceil(slLineMin2));
                            firstNonPositive1 = (firstNonPositive2 - sliceB) / sliceA;
                            ip = i + 1;
                            minPositiveThreshold = slRThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(slLineMin2));
                            lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                        } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                       Float.isNaN(slRThreshold[i]) && (slRThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined2 = (float) (i + Math.ceil(slLineMin2));
                            firstUndefined1 = (firstUndefined2 - sliceB) / sliceA;
                            ip = i + 1;
                            minPositiveThreshold = slRThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(slLineMin2));
                            lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                        } else if (slRThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (slRThreshold[i] > 0.0f)
                        else { // ((slRThreshold[i] <= 0.0f) || )Float.isNaN(slRThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= slRThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(slRThreshold[i])
                        slHaveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= slRThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(slRThreshold[i])
                } // while (!transitionFound)

            } // else ((lineMax1 - slLineMin1) < (lineMax2 - slLineMin2)

            if (firstUndefined1 >= 0) {
                Preferences.debug("Cannot calculate linear correlation coefficient \n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstUndefined1 + " or " +
                                  baseImage.getImageName() + " < " + firstUndefined2 + "\n", Preferences.DEBUG_ALGORITHM);
            }

            if (nonPositiveThreshold <= 0) {
                Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstNonPositive1 + " or " +
                                  baseImage.getImageName() + " < " + firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + lastPositive1 + " or " +
                              baseImage.getImageName() + " < " + lastPositive2 + "\n", Preferences.DEBUG_ALGORITHM);

            if (minPositiveThreshold < 0) {
                Preferences.debug("No positive threshold found", Preferences.DEBUG_ALGORITHM);
                dataLine1 = "slice #" + (z + 1) + " has no positive threshold\n";
                ViewUserInterface.getReference().setDataText(dataLine1);
                ViewUserInterface.getReference().setDataText("\n");

                continue;
            }

            if (doSecondIteration) {
                Preferences.debug("Second iteration excluding subthresholded region\n", Preferences.DEBUG_ALGORITHM);
                fireProgressStateChanged("Second iteration excluding subthresholded region");
                fireProgressStateChanged(80);
                slt1 = lastPositive1;
                slt2 = lastPositive2;

                averagex = 0.0;
                averagey = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {

                    if ((sliceBuffer[i] >= slt1) && (secondSliceBuffer[i] >= slt2)) {
                        averagex += sliceBuffer[i];
                        averagey += secondSliceBuffer[i];
                        count++;
                    }
                }

                averagex /= count;
                averagey /= count;
                num = 0.0;
                denom1 = 0.0;
                denom2 = 0.0;

                for (i = 0; i < thrLength; i++) {

                    if ((sliceBuffer[i] >= slt1) && (secondSliceBuffer[i] >= slt2)) {
                        num += (sliceBuffer[i] - averagex) * (secondSliceBuffer[i] - averagey);
                        denom1 += (sliceBuffer[i] - averagex) * (sliceBuffer[i] - averagex);
                        denom2 += (secondSliceBuffer[i] - averagey) * (secondSliceBuffer[i] - averagey);
                    }
                }

                denom = Math.sqrt(denom1 * denom2);
                r = (float) (num / denom);
                Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);

                // Use linear least squares to calculate the best line fit
                // for secondBuffer = a*firstBuffer + b.  Use an orthogonal
                // line fit where the distance measurements are orthgonal to
                // the proposed line.  This is an orthogonal regression as opposed
                // to a traditional regression of dependent y variable green on
                // independent red variable x which would give measurements in the
                // y direction.  Note that a traditional regression of red on green
                // would be yet another line giving measurements in the x direction.
                // Don't include any point i with sliceBuffer[i] < lastPositive1 or
                // secondSliceBuffer[i] < lastPositive2 in the regression line.
                // This excludes the L shaped subthresholded region from the
                // first iteration.
                sumx = 0.0;
                sumy = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {

                    if ((sliceBuffer[i] >= slt1) && (secondSliceBuffer[i] >= slt2)) {
                        sumx += sliceBuffer[i];
                        sumy += secondSliceBuffer[i];
                        count++;
                    }
                }

                averagex = sumx / count;
                averagey = sumy / count;
                sumx2 = 0.0;
                sumxy = 0.0;
                sumy2 = 0.0;

                for (i = 0; i < thrLength; i++) {

                    if ((sliceBuffer[i] >= slt1) && (secondSliceBuffer[i] >= slt2)) {
                        diffx = sliceBuffer[i] - averagex;
                        diffy = secondSliceBuffer[i] - averagey;
                        sumx2 += diffx * diffx;
                        sumxy += diffx * diffy;
                        sumy2 += diffy * diffy;
                    }
                }

                varx = sumx2 / (count - 1);
                vary = sumy2 / (count - 1);
                covarxy = sumxy / (count - 1);
                sliceA = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                      (2 * covarxy));
                aa = sliceA * sliceA;
                sliceB = (float) (averagey - (sliceA * averagex));
                mse = (float) (varx + (2 * sliceA * covarxy) + (aa * vary)) / (1 + aa);
                // +-2 standard deviations on each side includes about 95%.

                Preferences.debug(baseImage.getImageName() + " = " + sliceA + "*" + srcImage.getImageName() + " + " +
                                  sliceB + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

                // secondSliceBuffer[i] = a*sliceBuffer[i] + b;
                // sliceBuffer[i] = (secondSliceBuffer[i] - b)/a;
                // Calculate (slLineMin1,slLineMin2) and (lineMax1,lineMax2),
                // the endpoints of the line segment
                slLineMin1 = min1;
                slLineMin2 = (sliceA * min1) + sliceB;

                if (slLineMin2 < min2) {
                    slLineMin2 = min2;
                    slLineMin1 = (min2 - sliceB) / sliceA;
                }

                lineMax1 = max1;
                lineMax2 = (sliceA * max1) + sliceB;

                if (lineMax2 > max2) {
                    lineMax2 = max2;
                    lineMax1 = (max2 - sliceB) / sliceA;
                }

                firstNonPositive1 = -1;
                firstNonPositive2 = -1;
                minPositiveThreshold = -1;
                nonPositiveThreshold = 1;
                lastPositive1 = (float) (Math.floor(lineMax1) + 1);
                lastPositive2 = (float) (Math.floor(lineMax2) + 1);
                firstUndefined1 = -1;
                firstUndefined2 = -1;

                // Calculate the linear correlation coefficients for all pixels
                // whose values are either below threshold in buffer or are below
                // a*threshold + b in secondBuffer.  The values in buffer range from
                // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
                // along the color that has the greatest range along the line
                // segment.
                if ((lineMax1 - slLineMin1) >= (lineMax2 - slLineMin2)) {
                    slThresholdOn1 = true;
                    slRThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(slLineMin1)) + 1];
                    slColocSize = new float[slRThreshold.length];
                    slColocIntensity1 = new float[slRThreshold.length];
                    slColocIntensity2 = new float[slRThreshold.length];
                    slHaveThreshold = new boolean[slRThreshold.length];

                    for (i = 0; i < slRThreshold.length; i++) {
                        slRThreshold[i] = Float.NaN;
                        slColocSize[i] = 0.0f;
                        slColocIntensity1[i] = 0.0f;
                        slColocIntensity2[i] = 0.0f;
                        slHaveThreshold[i] = false;
                    }

                    transitionFound = false;
                    i = slRThreshold.length / 2;
                    thresholdIncrement = slRThreshold.length / 4;

                    while (!transitionFound) {
                        threshold = (float) (i + Math.ceil(slLineMin1));
                        secondThreshold = (sliceA * threshold) + sliceB;
                        averagex = 0.0;
                        averagey = 0.0;
                        count = 0;
                        morex = false;
                        morey = false;
                        lastx = sliceBuffer[0];
                        lasty = secondSliceBuffer[0];
                        voiIntensity1Thr = 0.0f;
                        voiIntensity2Thr = 0.0f;

                        for (j = 0; j < thrLength; j++) {

                            if (sliceBuffer[j] >= threshold) {
                                voiIntensity1Thr += sliceBuffer[j];
                            }

                            if (secondSliceBuffer[j] >= secondThreshold) {
                                voiIntensity2Thr += secondSliceBuffer[j];
                            }

                            if ((sliceBuffer[j] >= threshold) && (secondSliceBuffer[j] >= secondThreshold)) {
                                slColocSize[i] += 1.0f;
                                slColocIntensity1[i] += sliceBuffer[j];
                                slColocIntensity2[i] += secondSliceBuffer[j];
                            } else {
                                averagex += sliceBuffer[j];
                                averagey += secondSliceBuffer[j];
                                count++;

                                if (count == 1) {
                                    lastx = sliceBuffer[j];
                                    lasty = secondSliceBuffer[j];
                                } else if (count >= 2) {

                                    if (!morex) {

                                        if (sliceBuffer[j] != lastx) {
                                            morex = true;
                                        }
                                    }

                                    if (!morey) {

                                        if (secondSliceBuffer[j] != lasty) {
                                            morey = true;
                                        }
                                    }
                                } // else if (count >= 2)
                            }
                        } // for (j = 0; j < thrLength; j++)

                        slColocSize[i] = (slColocSize[i] / voiSliceSize) * 100.0f;

                        if (doColocWithThresholds) {
                            slColocIntensity1[i] = (slColocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                            slColocIntensity2[i] = (slColocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                        } else {
                            slColocIntensity1[i] = (slColocIntensity1[i] / voiSliceIntensity1) * 100.0f;
                            slColocIntensity2[i] = (slColocIntensity2[i] / voiSliceIntensity2) * 100.0f;
                        }

                        if (morex && morey) {
                            averagex /= count;
                            averagey /= count;
                            num = 0.0;
                            denom1 = 0.0;
                            denom2 = 0.0;

                            for (j = 0; j < thrLength; j++) {

                                if ((sliceBuffer[j] < threshold) || (secondSliceBuffer[j] < secondThreshold)) {
                                    num += (sliceBuffer[j] - averagex) * (secondSliceBuffer[j] - averagey);
                                    denom1 += (sliceBuffer[j] - averagex) * (sliceBuffer[j] - averagex);
                                    denom2 += (secondSliceBuffer[j] - averagey) * (secondSliceBuffer[j] - averagey);
                                }
                            }

                            slRThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                            slHaveThreshold[i] = true;
                            Preferences.debug("i = " + i + " slRThreshold[i] = " + slRThreshold[i] + "\n", 
                            		Preferences.DEBUG_ALGORITHM);

                            if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                    (slRThreshold[i - 1] <= 0.0f)) {
                                transitionFound = true;
                                nonPositiveThreshold = slRThreshold[i - 1];
                                firstNonPositive1 = (float) ((i - 1) + Math.ceil(slLineMin1));
                                firstNonPositive2 = (sliceA * firstNonPositive1) + sliceB;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                                lastPositive2 = (sliceA * lastPositive1) + sliceB;
                            } else if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                           Float.isNaN(slRThreshold[i - 1])) {
                                transitionFound = true;
                                firstUndefined1 = (float) ((i - 1) + Math.ceil(slLineMin1));
                                firstUndefined2 = (sliceA * firstUndefined1) + sliceB;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                                lastPositive2 = (sliceA * lastPositive1) + sliceB;
                            } else if ((i == 0) && (slRThreshold[i] > 0.0f)) {
                                transitionFound = true;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                                lastPositive2 = (sliceA * lastPositive1) + sliceB;
                            } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                           (slRThreshold[i] <= 0.0f) && (slRThreshold[i + 1] > 0.0f)) {
                                transitionFound = true;
                                nonPositiveThreshold = slRThreshold[i];
                                firstNonPositive1 = (float) (i + Math.ceil(slLineMin1));
                                firstNonPositive2 = (sliceA * firstNonPositive1) + sliceB;
                                ip = i + 1;
                                minPositiveThreshold = slRThreshold[i + 1];
                                lastPositive1 = (float) ((i + 1) + Math.ceil(slLineMin1));
                                lastPositive2 = (sliceA * lastPositive1) + sliceB;
                            } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                           Float.isNaN(slRThreshold[i]) && (slRThreshold[i + 1] > 0.0f)) {
                                transitionFound = true;
                                firstUndefined1 = (float) (i + Math.ceil(slLineMin1));
                                firstUndefined2 = (sliceA * firstUndefined1) + sliceB;
                                ip = i + 1;
                                minPositiveThreshold = slRThreshold[i + 1];
                                lastPositive1 = (float) ((i + 1) + Math.ceil(slLineMin1));
                                lastPositive2 = (sliceA * lastPositive1) + sliceB;
                            } else if (slRThreshold[i] > 0.0f) {
                                i = i - thresholdIncrement;

                                if (i < 0) {
                                    transitionFound = true;
                                }

                                thresholdIncrement = thresholdIncrement / 2;

                                if (thresholdIncrement == 0) {
                                    thresholdIncrement = 1;
                                }
                            } // else if (slRThreshold[i] > 0.0f)
                            else { // ((slRThreshold[i] <= 0.0f) || )Float.isNaN(slRThreshold[i])
                                i = i + thresholdIncrement;

                                if (i >= slRThreshold.length) {
                                    transitionFound = true;
                                }

                                thresholdIncrement = thresholdIncrement / 2;

                                if (thresholdIncrement == 0) {
                                    thresholdIncrement = 1;
                                }
                            }
                        } // if (morex && morey)
                        else { // Float.isNaN(slRThreshold[i])
                            slHaveThreshold[i] = true;
                            i = i + thresholdIncrement;

                            if (i >= slRThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // Float.isNaN(slRThreshold[i])
                    } // while (!transitionFound)

                } // if ((lineMax1 - slLineMin1) >= (lineMax2 - slLineMin2))
                else { // ((lineMax1 - slLineMin1) < (lineMax2 - slLineMin2)
                    slThresholdOn1 = false;
                    slRThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(slLineMin2)) + 1];
                    slColocSize = new float[slRThreshold.length];
                    slColocIntensity1 = new float[slRThreshold.length];
                    slColocIntensity2 = new float[slRThreshold.length];

                    for (i = 0; i < slRThreshold.length; i++) {
                        slRThreshold[i] = Float.NaN;
                        slColocSize[i] = 0.0f;
                        slColocIntensity1[i] = 0.0f;
                        slColocIntensity2[i] = 0.0f;
                    }

                    transitionFound = false;
                    i = slRThreshold.length / 2;
                    thresholdIncrement = slRThreshold.length / 4;

                    while (!transitionFound) {
                        secondThreshold = (float) (i + Math.ceil(slLineMin2));
                        threshold = (secondThreshold - sliceB) / sliceA;
                        averagex = 0.0;
                        averagey = 0.0;
                        count = 0;
                        morex = false;
                        morey = false;
                        lastx = sliceBuffer[0];
                        lasty = secondSliceBuffer[0];
                        voiIntensity1Thr = 0.0f;
                        voiIntensity2Thr = 0.0f;

                        for (j = 0; j < thrLength; j++) {

                            if (sliceBuffer[j] >= threshold) {
                                voiIntensity1Thr += sliceBuffer[j];
                            }

                            if (secondSliceBuffer[j] >= secondThreshold) {
                                voiIntensity2Thr += secondSliceBuffer[j];
                            }

                            if ((sliceBuffer[j] >= threshold) && (secondSliceBuffer[j] >= secondThreshold)) {
                                slColocSize[i] += 1.0f;
                                slColocIntensity1[i] += sliceBuffer[j];
                                slColocIntensity2[i] += secondSliceBuffer[j];
                            } else {
                                averagex += sliceBuffer[j];
                                averagey += secondSliceBuffer[j];
                                count++;

                                if (count == 1) {
                                    lastx = sliceBuffer[j];
                                    lasty = secondSliceBuffer[j];
                                } else if (count >= 2) {

                                    if (!morex) {

                                        if (sliceBuffer[j] != lastx) {
                                            morex = true;
                                        }
                                    }

                                    if (!morey) {

                                        if (secondSliceBuffer[j] != lasty) {
                                            morey = true;
                                        }
                                    }
                                } // else if (count >= 2)
                            }
                        } // for (j = 0; j < thrLength; j++)

                        slColocSize[i] = (slColocSize[i] / voiSliceSize) * 100.0f;

                        if (doColocWithThresholds) {
                            slColocIntensity1[i] = (slColocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                            slColocIntensity2[i] = (slColocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                        } else {
                            slColocIntensity1[i] = (slColocIntensity1[i] / voiSliceIntensity1) * 100.0f;
                            slColocIntensity2[i] = (slColocIntensity2[i] / voiSliceIntensity2) * 100.0f;
                        }

                        if (morex && morey) {
                            averagex /= count;
                            averagey /= count;
                            num = 0.0;
                            denom1 = 0.0;
                            denom2 = 0.0;

                            for (j = 0; j < thrLength; j++) {

                                if ((sliceBuffer[j] < threshold) || (secondSliceBuffer[j] < secondThreshold)) {
                                    num += (sliceBuffer[j] - averagex) * (secondSliceBuffer[j] - averagey);
                                    denom1 += (sliceBuffer[j] - averagex) * (sliceBuffer[j] - averagex);
                                    denom2 += (secondSliceBuffer[j] - averagey) * (secondSliceBuffer[j] - averagey);
                                }
                            }

                            slRThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                            slHaveThreshold[i] = true;
                            Preferences.debug("i = " + i + " slRThreshold[i] = " + slRThreshold[i] + "\n", 
                            		Preferences.DEBUG_ALGORITHM);

                            if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                    (slRThreshold[i - 1] <= 0.0f)) {
                                transitionFound = true;
                                nonPositiveThreshold = slRThreshold[i - 1];
                                firstNonPositive2 = (float) ((i - 1) + Math.ceil(slLineMin2));
                                firstNonPositive1 = (firstNonPositive2 - sliceB) / sliceA;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                                lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                            } else if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                           Float.isNaN(slRThreshold[i - 1])) {
                                transitionFound = true;
                                firstUndefined2 = (float) ((i - 1) + Math.ceil(slLineMin2));
                                firstUndefined1 = (firstUndefined2 - sliceB) / sliceA;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                                lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                            } else if ((i == 0) && (slRThreshold[i] > 0.0f)) {
                                transitionFound = true;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                                lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                            } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                           (slRThreshold[i] <= 0.0f) && (slRThreshold[i + 1] > 0.0f)) {
                                transitionFound = true;
                                nonPositiveThreshold = slRThreshold[i];
                                firstNonPositive2 = (float) (i + Math.ceil(slLineMin2));
                                firstNonPositive1 = (firstNonPositive2 - sliceB) / sliceA;
                                ip = i + 1;
                                minPositiveThreshold = slRThreshold[i + 1];
                                lastPositive2 = (float) ((i + 1) + Math.ceil(slLineMin2));
                                lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                            } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                           Float.isNaN(slRThreshold[i]) && (slRThreshold[i + 1] > 0.0f)) {
                                transitionFound = true;
                                firstUndefined2 = (float) (i + Math.ceil(slLineMin2));
                                firstUndefined1 = (firstUndefined2 - sliceB) / sliceA;
                                ip = i + 1;
                                minPositiveThreshold = slRThreshold[i + 1];
                                lastPositive2 = (float) ((i + 1) + Math.ceil(slLineMin2));
                                lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                            } else if (slRThreshold[i] > 0.0f) {
                                i = i - thresholdIncrement;

                                if (i < 0) {
                                    transitionFound = true;
                                }

                                thresholdIncrement = thresholdIncrement / 2;

                                if (thresholdIncrement == 0) {
                                    thresholdIncrement = 1;
                                }
                            } // else if (slRThreshold[i] > 0.0f)
                            else { // ((slRThreshold[i] <= 0.0f) || )Float.isNaN(slRThreshold[i])
                                i = i + thresholdIncrement;

                                if (i >= slRThreshold.length) {
                                    transitionFound = true;
                                }

                                thresholdIncrement = thresholdIncrement / 2;

                                if (thresholdIncrement == 0) {
                                    thresholdIncrement = 1;
                                }
                            }
                        } // if (morex && morey)
                        else { // Float.isNaN(slRThreshold[i])
                            slHaveThreshold[i] = true;
                            i = i + thresholdIncrement;

                            if (i >= slRThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // Float.isNaN(slRThreshold[i])
                    } // while (!transitionFound)

                } // else ((lineMax1 - slLineMin1) < (lineMax2 - slLineMin2)

                if (firstUndefined1 >= 0) {
                    Preferences.debug("Cannot calculate linear correlation coefficient \n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstUndefined1 + " or " +
                                      baseImage.getImageName() + " < " + firstUndefined2 + "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (nonPositiveThreshold <= 0) {
                    Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstNonPositive1 +
                                      " or " + baseImage.getImageName() + " < " + firstNonPositive2 + "\n", 
                                      Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + lastPositive1 + " or " +
                                  baseImage.getImageName() + " < " + lastPositive2 + "\n", Preferences.DEBUG_ALGORITHM);

                if (minPositiveThreshold < 0) {
                    Preferences.debug("No positive threshold found on second iteration", Preferences.DEBUG_ALGORITHM);
                    dataLine1 = "slice #" + (z + 1) + " has no positive threshold on second iteration\n";
                    ViewUserInterface.getReference().setDataText(dataLine1);
                    ViewUserInterface.getReference().setDataText("\n");

                    continue;
                }
            } // if (doSecondIteration)

            if (ip >= 0) {

                if (slThresholdOn1) {
                    threshold = slRThreshold[ip];
                    colocAreaPercent = slColocSize[ip];
                    colocIntensityPercent1 = slColocIntensity1[ip];
                    colocIntensityPercent2 = slColocIntensity2[ip];
                } else {
                    threshold = slRThreshold[ip];
                    colocAreaPercent = slColocSize[ip];
                    colocIntensityPercent1 = slColocIntensity1[ip];
                    colocIntensityPercent2 = slColocIntensity2[ip];
                }

                dataLine1 = "slice #\t%coloc area\t";
                dataLine2 = (z + 1) + "\t" + colocAreaPercent + "\t";

                dataLine1 = dataLine1 + "% coloc1\t% coloc2\t";
                dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";

                if (doSecondIteration) {
                    dataLine1 = dataLine1 + "Linear correlation coefficient - 2 iters\n";
                    dataLine2 = dataLine2 + sliceR + "\n";
                } else {
                    dataLine1 = dataLine1 + "Linear correlation coefficient - 1 iter\n";
                    dataLine2 = dataLine2 + sliceR + "\n";
                }

                ViewUserInterface.getReference().setDataText(dataLine1);
                ViewUserInterface.getReference().setDataText(dataLine2);
                ViewUserInterface.getReference().setDataText("\n");

                nf = NumberFormat.getNumberInstance();
                nf.setMaximumFractionDigits(6);
            } // if (ip >= 0)
        } // for (z = 0; z < zDim; z++)
    }

    /**
     * DOCUMENT ME!
     */
    public void calc2DColorStats() {
        int xDim, yDim, zDim;
        int length;
        int i, j;
        int ip;
        int z;
        BitSet inputSliceMask;
        float[] sliceBuffer;
        float[] secondSliceBuffer;
        int voiSliceSize;
        float voiSliceIntensity1;
        float voiSliceIntensity2;
        float[] tmpBuffer;
        float[] tmpSecondBuffer;
        double averagex, averagey;
        int count;
        double num, denom1, denom2, denom;
        float sliceR = 0.0f;
        double sumx, sumx2;
        double sumy, sumy2;
        double sumxy;
        double diffx, diffy;
        double varx, vary, covarxy;
        float mse; // mean square error
        float sliceA, sliceB;
        double min1, max1, min2, max2;
        float[] slRThreshold;
        float[] slColocSize;
        float[] slColocIntensity1;
        float[] slColocIntensity2;
        boolean[] slHaveThreshold;
        boolean slThresholdOn1;
        double slLineMin1, slLineMin2;
        double lineMax1, lineMax2;
        float threshold;
        float secondThreshold;
        float nonPositiveThreshold;
        float minPositiveThreshold;
        float lastPositive1, lastPositive2;
        float slt1, slt2;
        float firstNonPositive1, firstNonPositive2;
        float firstUndefined1, firstUndefined2;
        boolean transitionFound;
        int thresholdIncrement;
        boolean morex, morey;
        float lastx, lasty;
        float voiIntensity1Thr;
        float voiIntensity2Thr;
        float colocAreaPercent;
        float colocIntensityPercent1;
        float colocIntensityPercent2;
        String dataLine1;
        String dataLine2;
        NumberFormat nf;

        xDim = subtractedSrcImage.getExtents()[0];
        yDim = subtractedSrcImage.getExtents()[1];
        zDim = subtractedSrcImage.getExtents()[2];
        length = xDim * yDim;
        inputSliceMask = new BitSet(length);

        for (z = 0; z < zDim; z++) {
            Preferences.debug("Slice = " + (z + 1) + "\n", Preferences.DEBUG_ALGORITHM);
            ip = -1;
            sliceBuffer = new float[length];
            secondSliceBuffer = new float[length];

            for (i = 0; i < length; i++) {

                if (inputMask.get((z * length) + i)) {
                    inputSliceMask.set(i);
                } else {
                    inputSliceMask.clear(i);
                }
            }

            try {
                subtractedSrcImage.exportRGBData(color, 4 * z * length, length, sliceBuffer);
                subtractedSrcImage.exportRGBData(secondColor, 4 * z * length, length, secondSliceBuffer);
            } catch (IOException e) {
                sliceBuffer = null;
                secondSliceBuffer = null;
                errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                return;
            }

            voiSliceSize = 0;
            voiSliceIntensity1 = 0.0f;
            voiSliceIntensity2 = 0.0f;

            for (i = 0; i < length; i++) {

                if (inputSliceMask.get(i)) {
                    voiSliceSize++;

                    if ((sliceBuffer[i] >= background1) || (secondSliceBuffer[i] >= background2)) {
                        voiSliceIntensity1 += sliceBuffer[i];
                        voiSliceIntensity2 += secondSliceBuffer[i];
                    }
                }
            }

            if (voiSliceSize == 0) {
                Preferences.debug("No VOI found", Preferences.DEBUG_ALGORITHM);
                dataLine1 = "slice #" + (z + 1) + " has no VOI\n";
                ViewUserInterface.getReference().setDataText(dataLine1);
                ViewUserInterface.getReference().setDataText("\n");

                continue;
            }

            min1 = Double.MAX_VALUE;
            max1 = -Double.MAX_VALUE;
            min2 = Double.MAX_VALUE;
            max2 = -Double.MAX_VALUE;

            for (i = 0; i < length; i++) {

                if (sliceBuffer[i] > max1) {
                    max1 = sliceBuffer[i];
                }

                if (sliceBuffer[i] < min1) {
                    min1 = sliceBuffer[i];
                }

                if (secondSliceBuffer[i] > max2) {
                    max2 = secondSliceBuffer[i];
                }

                if (secondSliceBuffer[i] < min2) {
                    min2 = secondSliceBuffer[i];
                }
            }

            // Reduce sliceBuffer and secondSliceBuffer so that they only contain the points
            // where (((sliceBuffer[i] >= background1) || (secondSliceBuffer[i] >= background2)) &&
            // inputMask.get(i))
            thrLength = 0;
            tmpBuffer = new float[length];
            tmpSecondBuffer = new float[length];

            for (i = 0; i < length; i++) {

                if (((sliceBuffer[i] >= background1) || (secondSliceBuffer[i] >= background2)) &&
                        inputSliceMask.get(i)) {
                    tmpBuffer[thrLength] = sliceBuffer[i];
                    tmpSecondBuffer[thrLength++] = secondSliceBuffer[i];
                }
            }

            sliceBuffer = new float[thrLength];
            secondSliceBuffer = new float[thrLength];

            for (i = 0; i < thrLength; i++) {
                sliceBuffer[i] = tmpBuffer[i];
                secondSliceBuffer[i] = tmpSecondBuffer[i];
            }

            tmpBuffer = null;
            tmpSecondBuffer = null;
            System.gc();

            averagex = 0.0;
            averagey = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {
                averagex += sliceBuffer[i];
                averagey += secondSliceBuffer[i];
                count++;
            }

            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (i = 0; i < thrLength; i++) {
                num += (sliceBuffer[i] - averagex) * (secondSliceBuffer[i] - averagey);
                denom1 += (sliceBuffer[i] - averagex) * (sliceBuffer[i] - averagex);
                denom2 += (secondSliceBuffer[i] - averagey) * (secondSliceBuffer[i] - averagey);
            }

            denom = Math.sqrt(denom1 * denom2);
            sliceR = (float) (num / denom);
            Preferences.debug("Linear corrleation coefficient = " + sliceR + "\n", Preferences.DEBUG_ALGORITHM);

            sumx = 0.0;
            sumy = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {
                sumx += sliceBuffer[i];
                sumy += secondSliceBuffer[i];
                count++;
            }

            averagex = sumx / count;
            averagey = sumy / count;
            sumx2 = 0.0;
            sumxy = 0.0;
            sumy2 = 0.0;

            for (i = 0; i < thrLength; i++) {
                diffx = sliceBuffer[i] - averagex;
                diffy = secondSliceBuffer[i] - averagey;
                sumx2 += diffx * diffx;
                sumxy += diffx * diffy;
                sumy2 += diffy * diffy;
            }

            float aa;
            varx = sumx2 / (count - 1);
            vary = sumy2 / (count - 1);
            covarxy = sumxy / (count - 1);
            sliceA = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                  (2 * covarxy));
            aa = sliceA * sliceA;
            sliceB = (float) (averagey - (sliceA * averagex));
            mse = (float) (varx + (2 * sliceA * covarxy) + (aa * vary)) / (1 + aa);
            // +-2 standard deviations on each side includes about 95%.

            Preferences.debug("First iteration\n", Preferences.DEBUG_ALGORITHM);

            if ((useRed) && (useGreen)) {
                Preferences.debug("green = " + sliceA + "*red" + " + " + sliceB + "\n", Preferences.DEBUG_ALGORITHM);
            } else if ((useRed) && (useBlue)) {
                Preferences.debug("blue = " + sliceA + "*red" + " + " + sliceB + "\n", Preferences.DEBUG_ALGORITHM);
            } else {
                Preferences.debug("blue = " + sliceA + "*green" + " + " + sliceB + "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

            slLineMin1 = min1;
            slLineMin2 = (sliceA * min1) + sliceB;

            if (slLineMin2 < min2) {
                slLineMin2 = min2;
                slLineMin1 = (min2 - sliceB) / sliceA;
            }

            lineMax1 = max1;
            lineMax2 = (sliceA * max1) + sliceB;

            if (lineMax2 > max2) {
                lineMax2 = max2;
                lineMax1 = (max2 - sliceB) / sliceA;
            }

            firstNonPositive1 = -1;
            firstNonPositive2 = -1;
            minPositiveThreshold = -1;
            nonPositiveThreshold = 1;
            lastPositive1 = (float) (Math.floor(lineMax1) + 1);
            lastPositive2 = (float) (Math.floor(lineMax2) + 1);
            firstUndefined1 = -1;
            firstUndefined2 = -1;

            // Calculate the linear correlation coefficients for all pixels
            // whose values are either below threshold in buffer or are below
            // a*threshold + b in secondBuffer.  The values in buffer range from
            // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
            // along the color that has the greatest range along the line
            // segment.
            if ((lineMax1 - slLineMin1) >= (lineMax2 - slLineMin2)) {
                slThresholdOn1 = true;
                slRThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(slLineMin1)) + 1];
                slColocSize = new float[slRThreshold.length];
                slColocIntensity1 = new float[slRThreshold.length];
                slColocIntensity2 = new float[slRThreshold.length];
                slHaveThreshold = new boolean[slRThreshold.length];

                for (i = 0; i < slRThreshold.length; i++) {
                    slRThreshold[i] = Float.NaN;
                    slColocSize[i] = 0.0f;
                    slColocIntensity1[i] = 0.0f;
                    slColocIntensity2[i] = 0.0f;
                    slHaveThreshold[i] = false;
                }

                transitionFound = false;
                i = slRThreshold.length / 2;
                thresholdIncrement = slRThreshold.length / 4;

                while (!transitionFound) {
                    threshold = (float) (i + Math.ceil(slLineMin1));
                    secondThreshold = (sliceA * threshold) + sliceB;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = sliceBuffer[0];
                    lasty = secondSliceBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {
                        float buff = sliceBuffer[j];
                        float secBuff = secondSliceBuffer[j];

                        if (buff >= threshold) {
                            voiIntensity1Thr += buff;
                        }

                        if (secBuff >= secondThreshold) {
                            voiIntensity2Thr += secBuff;
                        }

                        if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                            slColocSize[i] += 1.0f;
                            slColocIntensity1[i] += buff;
                            slColocIntensity2[i] += secBuff;
                        } else {
                            averagex += buff;
                            averagey += secBuff;
                            count++;

                            if (count == 1) {
                                lastx = buff;
                                lasty = secBuff;
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buff != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secBuff != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    slColocSize[i] = (slColocSize[i] / voiSliceSize) * 100.0f;

                    if (doColocWithThresholds) {
                        slColocIntensity1[i] = (slColocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        slColocIntensity2[i] = (slColocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        slColocIntensity1[i] = (slColocIntensity1[i] / voiSliceIntensity1) * 100.0f;
                        slColocIntensity2[i] = (slColocIntensity2[i] / voiSliceIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {
                            float buff = sliceBuffer[j];
                            float secBuff = secondSliceBuffer[j];

                            if ((buff < threshold) || (secBuff < secondThreshold)) {
                                num += (buff - averagex) * (secBuff - averagey);
                                denom1 += (buff - averagex) * (buff - averagex);
                                denom2 += (secBuff - averagey) * (secBuff - averagey);
                            }
                        } // for (j = 0; j < thrLength; j++)

                        slRThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        slHaveThreshold[i] = true;
                        Preferences.debug("i = " + i + " slRThreshold[i] = " + slRThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                (slRThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = slRThreshold[i - 1];
                            firstNonPositive1 = (float) ((i - 1) + Math.ceil(slLineMin1));
                            firstNonPositive2 = (sliceA * firstNonPositive1) + sliceB;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                            lastPositive2 = (sliceA * lastPositive1) + sliceB;
                        } else if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                       Float.isNaN(slRThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined1 = (float) ((i - 1) + Math.ceil(slLineMin1));
                            firstUndefined2 = (sliceA * firstUndefined1) + sliceB;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                            lastPositive2 = (sliceA * lastPositive1) + sliceB;
                        } else if ((i == 0) && (slRThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                            lastPositive2 = (sliceA * lastPositive1) + sliceB;
                        } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                       (slRThreshold[i] <= 0.0f) && (slRThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = slRThreshold[i];
                            firstNonPositive1 = (float) (i + Math.ceil(slLineMin1));
                            firstNonPositive2 = (sliceA * firstNonPositive1) + sliceB;
                            ip = i + 1;
                            minPositiveThreshold = slRThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(slLineMin1));
                            lastPositive2 = (sliceA * lastPositive1) + sliceB;
                        } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                       Float.isNaN(slRThreshold[i]) && (slRThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined1 = (float) (i + Math.ceil(slLineMin1));
                            firstUndefined2 = (sliceA * firstUndefined1) + sliceB;
                            ip = i + 1;
                            minPositiveThreshold = slRThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(slLineMin1));
                            lastPositive2 = (sliceA * lastPositive1) + sliceB;
                        } else if (slRThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (slRThreshold[i] > 0.0f)
                        else { // ((slRThreshold[i] <= 0.0f) || )Float.isNaN(slRThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= slRThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(slRThreshold[i])
                        slHaveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= slRThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(slRThreshold[i])
                } // while (!transitionFound)

            } // if ((lineMax1 - slLineMin1) >= (lineMax2 - slLineMin2))
            else { // ((lineMax1 - slLineMin1) < (lineMax2 - slLineMin2)
                slThresholdOn1 = false;
                slRThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(slLineMin2)) + 1];
                slColocSize = new float[slRThreshold.length];
                slColocIntensity1 = new float[slRThreshold.length];
                slColocIntensity2 = new float[slRThreshold.length];
                slHaveThreshold = new boolean[slRThreshold.length];

                for (i = 0; i < slRThreshold.length; i++) {
                    slRThreshold[i] = Float.NaN;
                    slColocSize[i] = 0.0f;
                    slColocIntensity1[i] = 0.0f;
                    slColocIntensity2[i] = 0.0f;
                    slHaveThreshold[i] = false;
                }

                transitionFound = false;
                i = slRThreshold.length / 2;
                thresholdIncrement = slRThreshold.length / 4;

                while (!transitionFound) {
                    secondThreshold = (float) (i + Math.ceil(slLineMin2));
                    threshold = (secondThreshold - sliceB) / sliceA;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = sliceBuffer[0];
                    lasty = secondSliceBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {
                        float buff = sliceBuffer[j];
                        float secBuff = secondSliceBuffer[j];

                        if (buff >= threshold) {
                            voiIntensity1Thr += buff;
                        }

                        if (secBuff >= secondThreshold) {
                            voiIntensity2Thr += secBuff;
                        }

                        if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                            slColocSize[i] += 1.0f;
                            slColocIntensity1[i] += buff;
                            slColocIntensity2[i] += secBuff;
                        } else {
                            averagex += buff;
                            averagey += secBuff;
                            count++;

                            if (count == 1) {
                                lastx = buff;
                                lasty = secBuff;
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buff != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secBuff != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    slColocSize[i] = (slColocSize[i] / voiSliceSize) * 100.0f;

                    if (doColocWithThresholds) {
                        slColocIntensity1[i] = (slColocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        slColocIntensity2[i] = (slColocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        slColocIntensity1[i] = (slColocIntensity1[i] / voiSliceIntensity1) * 100.0f;
                        slColocIntensity2[i] = (slColocIntensity2[i] / voiSliceIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {
                            float buff = sliceBuffer[j];
                            float secBuff = secondSliceBuffer[j];

                            if ((buff < threshold) || (secBuff < secondThreshold)) {
                                num += (buff - averagex) * (secBuff - averagey);
                                denom1 += (buff - averagex) * (buff - averagex);
                                denom2 += (secBuff - averagey) * (secBuff - averagey);
                            }
                        } // for (j = 0; j < thrLength; j++)

                        slRThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        slHaveThreshold[i] = true;
                        Preferences.debug("i = " + i + " slRThreshold[i] = " + slRThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                (slRThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = slRThreshold[i - 1];
                            firstNonPositive2 = (float) ((i - 1) + Math.ceil(slLineMin2));
                            firstNonPositive1 = (firstNonPositive2 - sliceB) / sliceA;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                            lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                        } else if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                       Float.isNaN(slRThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined2 = (float) ((i - 1) + Math.ceil(slLineMin2));
                            firstUndefined1 = (firstUndefined2 - sliceB) / sliceA;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                            lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                        } else if ((i == 0) && (slRThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            ip = i;
                            minPositiveThreshold = slRThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                            lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                        } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                       (slRThreshold[i] <= 0.0f) && (slRThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = slRThreshold[i];
                            firstNonPositive2 = (float) (i + Math.ceil(slLineMin2));
                            firstNonPositive1 = (firstNonPositive2 - sliceB) / sliceA;
                            ip = i + 1;
                            minPositiveThreshold = slRThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(slLineMin2));
                            lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                        } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                       Float.isNaN(slRThreshold[i]) && (slRThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined2 = (float) (i + Math.ceil(slLineMin2));
                            firstUndefined1 = (firstUndefined2 - sliceB) / sliceA;
                            ip = i + 1;
                            minPositiveThreshold = slRThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(slLineMin2));
                            lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                        } else if (slRThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (slRThreshold[i] > 0.0f)
                        else { // ((slRThreshold[i] <= 0.0f) || )Float.isNaN(slRThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= slRThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(slRThreshold[i])
                        slHaveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= slRThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(slRThreshold[i])
                } // while (!transitionFound)

            } // else ((lineMax1 - slLineMin1) < (lineMax2 - slLineMin2)

            if ((useRed) && (useGreen)) {

                if (firstUndefined1 >= 0) {
                    Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstUndefined1 + " or green < " + firstUndefined2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (nonPositiveThreshold <= 0) {
                    Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstNonPositive1 + " or green < " +
                                      firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + lastPositive1 + " or green < " + lastPositive2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);

            } // if ((useRed) && (useGreen))
            else if ((useRed) && (useBlue)) {

                if (firstUndefined1 >= 0) {
                    Preferences.debug("Cannot calculate linear correlation coefficient\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstUndefined1 + " or blue < " + firstUndefined2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (nonPositiveThreshold <= 0) {
                    Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstNonPositive1 + " or blue < " + firstNonPositive2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            } // else if ((useRed) && (useBlue))
            else {

                if (firstUndefined1 >= 0) {
                    Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with green < " + firstUndefined1 + " or blue < " + firstUndefined2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (nonPositiveThreshold <= 0) {
                    Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with green < " + firstNonPositive1 + " or blue < " +
                                      firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with green < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            }

            if (minPositiveThreshold < 0) {
                Preferences.debug("No positive threshold found", Preferences.DEBUG_ALGORITHM);
                dataLine1 = "slice #" + (z + 1) + " has no positive threshold\n";
                ViewUserInterface.getReference().setDataText(dataLine1);
                ViewUserInterface.getReference().setDataText("\n");

                continue;
            }

            if (doSecondIteration) {
                Preferences.debug("Second iteration excluding subthresholded region\n", Preferences.DEBUG_ALGORITHM);
                fireProgressStateChanged("Second iteration excluding subthresholded region");
                fireProgressStateChanged(80);
                slt1 = lastPositive1;
                slt2 = lastPositive2;

                averagex = 0.0;
                averagey = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {

                    if ((sliceBuffer[i] >= slt1) && (secondSliceBuffer[i] >= slt2)) {
                        averagex += sliceBuffer[i];
                        averagey += secondSliceBuffer[i];
                        count++;
                    }
                }

                averagex /= count;
                averagey /= count;
                num = 0.0;
                denom1 = 0.0;
                denom2 = 0.0;

                for (i = 0; i < thrLength; i++) {

                    if ((sliceBuffer[i] >= slt1) && (secondSliceBuffer[i] >= slt2)) {
                        num += (sliceBuffer[i] - averagex) * (secondSliceBuffer[i] - averagey);
                        denom1 += (sliceBuffer[i] - averagex) * (sliceBuffer[i] - averagex);
                        denom2 += (secondSliceBuffer[i] - averagey) * (secondSliceBuffer[i] - averagey);
                    }
                }

                denom = Math.sqrt(denom1 * denom2);
                r = (float) (num / denom);
                Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);

                // Use linear least squares to calculate the best line fit
                // for secondBuffer = a*firstBuffer + b.  Use an orthogonal
                // line fit where the distance measurements are orthgonal to
                // the proposed line.  This is an orthogonal regression as opposed
                // to a traditional regression of dependent y variable green on
                // independent red variable x which would give measurements in the
                // y direction.  Note that a traditional regression of red on green
                // would be yet another line giving measurements in the x direction.
                // Don't include any point i with sliceBuffer[i] < lastPositive1 or
                // secondSliceBuffer[i] < lastPositive2 in the regression line.
                // This excludes the L shaped subthresholded region from the
                // first iteration.
                sumx = 0.0;
                sumy = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {

                    if ((sliceBuffer[i] >= slt1) && (secondSliceBuffer[i] >= slt2)) {
                        sumx += sliceBuffer[i];
                        sumy += secondSliceBuffer[i];
                        count++;
                    }
                }

                averagex = sumx / count;
                averagey = sumy / count;
                sumx2 = 0.0;
                sumxy = 0.0;
                sumy2 = 0.0;

                for (i = 0; i < thrLength; i++) {

                    if ((sliceBuffer[i] >= slt1) && (secondSliceBuffer[i] >= slt2)) {
                        diffx = sliceBuffer[i] - averagex;
                        diffy = secondSliceBuffer[i] - averagey;
                        sumx2 += diffx * diffx;
                        sumxy += diffx * diffy;
                        sumy2 += diffy * diffy;
                    }
                }

                varx = sumx2 / (count - 1);
                vary = sumy2 / (count - 1);
                covarxy = sumxy / (count - 1);
                sliceA = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                      (2 * covarxy));
                aa = sliceA * sliceA;
                sliceB = (float) (averagey - (sliceA * averagex));
                mse = (float) (varx + (2 * sliceA * covarxy) + (aa * vary)) / (1 + aa);
                // +-2 standard deviations on each side includes about 95%.

                if ((useRed) && (useGreen)) {
                    Preferences.debug("green = " + sliceA + "*red" + " + " + sliceB + "\n", Preferences.DEBUG_ALGORITHM);
                } else if ((useRed) && (useBlue)) {
                    Preferences.debug("blue = " + sliceA + "*red" + " + " + sliceB + "\n", Preferences.DEBUG_ALGORITHM);
                } else {
                    Preferences.debug("blue = " + sliceA + "*green" + " + " + sliceB + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

                // secondSliceBuffer[i] = a*sliceBuffer[i] + b;
                // sliceBuffer[i] = (secondSliceBuffer[i] - b)/a;
                // Calculate (slLineMin1,slLineMin2) and (lineMax1,lineMax2),
                // the endpoints of the line segment
                slLineMin1 = min1;
                slLineMin2 = (sliceA * min1) + sliceB;

                if (slLineMin2 < min2) {
                    slLineMin2 = min2;
                    slLineMin1 = (min2 - sliceB) / sliceA;
                }

                lineMax1 = max1;
                lineMax2 = (sliceA * max1) + sliceB;

                if (lineMax2 > max2) {
                    lineMax2 = max2;
                    lineMax1 = (max2 - sliceB) / sliceA;
                }

                firstNonPositive1 = -1;
                firstNonPositive2 = -1;
                minPositiveThreshold = -1;
                nonPositiveThreshold = 1;
                lastPositive1 = (float) (Math.floor(lineMax1) + 1);
                lastPositive2 = (float) (Math.floor(lineMax2) + 1);
                firstUndefined1 = -1;
                firstUndefined2 = -1;

                // Calculate the linear correlation coefficients for all pixels
                // whose values are either below threshold in buffer or are below
                // a*threshold + b in secondBuffer.  The values in buffer range from
                // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
                // along the color that has the greatest range along the line
                // segment.
                if ((lineMax1 - slLineMin1) >= (lineMax2 - slLineMin2)) {
                    slThresholdOn1 = true;
                    slRThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(slLineMin1)) + 1];
                    slColocSize = new float[slRThreshold.length];
                    slColocIntensity1 = new float[slRThreshold.length];
                    slColocIntensity2 = new float[slRThreshold.length];
                    slHaveThreshold = new boolean[slRThreshold.length];

                    for (i = 0; i < slRThreshold.length; i++) {
                        slRThreshold[i] = Float.NaN;
                        slColocSize[i] = 0.0f;
                        slColocIntensity1[i] = 0.0f;
                        slColocIntensity2[i] = 0.0f;
                        slHaveThreshold[i] = false;
                    }

                    transitionFound = false;
                    i = slRThreshold.length / 2;
                    thresholdIncrement = slRThreshold.length / 4;

                    while (!transitionFound) {
                        threshold = (float) (i + Math.ceil(slLineMin1));
                        secondThreshold = (sliceA * threshold) + sliceB;
                        averagex = 0.0;
                        averagey = 0.0;
                        count = 0;
                        morex = false;
                        morey = false;
                        lastx = sliceBuffer[0];
                        lasty = secondSliceBuffer[0];
                        voiIntensity1Thr = 0.0f;
                        voiIntensity2Thr = 0.0f;

                        for (j = 0; j < thrLength; j++) {

                            if (sliceBuffer[j] >= threshold) {
                                voiIntensity1Thr += sliceBuffer[j];
                            }

                            if (secondSliceBuffer[j] >= secondThreshold) {
                                voiIntensity2Thr += secondSliceBuffer[j];
                            }

                            if ((sliceBuffer[j] >= threshold) && (secondSliceBuffer[j] >= secondThreshold)) {
                                slColocSize[i] += 1.0f;
                                slColocIntensity1[i] += sliceBuffer[j];
                                slColocIntensity2[i] += secondSliceBuffer[j];
                            } else {
                                averagex += sliceBuffer[j];
                                averagey += secondSliceBuffer[j];
                                count++;

                                if (count == 1) {
                                    lastx = sliceBuffer[j];
                                    lasty = secondSliceBuffer[j];
                                } else if (count >= 2) {

                                    if (!morex) {

                                        if (sliceBuffer[j] != lastx) {
                                            morex = true;
                                        }
                                    }

                                    if (!morey) {

                                        if (secondSliceBuffer[j] != lasty) {
                                            morey = true;
                                        }
                                    }
                                } // else if (count >= 2)
                            }
                        } // for (j = 0; j < thrLength; j++)

                        slColocSize[i] = (slColocSize[i] / voiSliceSize) * 100.0f;

                        if (doColocWithThresholds) {
                            slColocIntensity1[i] = (slColocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                            slColocIntensity2[i] = (slColocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                        } else {
                            slColocIntensity1[i] = (slColocIntensity1[i] / voiSliceIntensity1) * 100.0f;
                            slColocIntensity2[i] = (slColocIntensity2[i] / voiSliceIntensity2) * 100.0f;
                        }

                        if (morex && morey) {
                            averagex /= count;
                            averagey /= count;
                            num = 0.0;
                            denom1 = 0.0;
                            denom2 = 0.0;

                            for (j = 0; j < thrLength; j++) {

                                if ((sliceBuffer[j] < threshold) || (secondSliceBuffer[j] < secondThreshold)) {
                                    num += (sliceBuffer[j] - averagex) * (secondSliceBuffer[j] - averagey);
                                    denom1 += (sliceBuffer[j] - averagex) * (sliceBuffer[j] - averagex);
                                    denom2 += (secondSliceBuffer[j] - averagey) * (secondSliceBuffer[j] - averagey);
                                }
                            }

                            slRThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                            slHaveThreshold[i] = true;
                            Preferences.debug("i = " + i + " slRThreshold[i] = " + slRThreshold[i] + "\n", 
                            		Preferences.DEBUG_ALGORITHM);

                            if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                    (slRThreshold[i - 1] <= 0.0f)) {
                                transitionFound = true;
                                nonPositiveThreshold = slRThreshold[i - 1];
                                firstNonPositive1 = (float) ((i - 1) + Math.ceil(slLineMin1));
                                firstNonPositive2 = (sliceA * firstNonPositive1) + sliceB;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                                lastPositive2 = (sliceA * lastPositive1) + sliceB;
                            } else if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                           Float.isNaN(slRThreshold[i - 1])) {
                                transitionFound = true;
                                firstUndefined1 = (float) ((i - 1) + Math.ceil(slLineMin1));
                                firstUndefined2 = (sliceA * firstUndefined1) + sliceB;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                                lastPositive2 = (sliceA * lastPositive1) + sliceB;
                            } else if ((i == 0) && (slRThreshold[i] > 0.0f)) {
                                transitionFound = true;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive1 = (float) (i + Math.ceil(slLineMin1));
                                lastPositive2 = (sliceA * lastPositive1) + sliceB;
                            } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                           (slRThreshold[i] <= 0.0f) && (slRThreshold[i + 1] > 0.0f)) {
                                transitionFound = true;
                                nonPositiveThreshold = slRThreshold[i];
                                firstNonPositive1 = (float) (i + Math.ceil(slLineMin1));
                                firstNonPositive2 = (sliceA * firstNonPositive1) + sliceB;
                                ip = i + 1;
                                minPositiveThreshold = slRThreshold[i + 1];
                                lastPositive1 = (float) ((i + 1) + Math.ceil(slLineMin1));
                                lastPositive2 = (sliceA * lastPositive1) + sliceB;
                            } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                           Float.isNaN(slRThreshold[i]) && (slRThreshold[i + 1] > 0.0f)) {
                                transitionFound = true;
                                firstUndefined1 = (float) (i + Math.ceil(slLineMin1));
                                firstUndefined2 = (sliceA * firstUndefined1) + sliceB;
                                ip = i + 1;
                                minPositiveThreshold = slRThreshold[i + 1];
                                lastPositive1 = (float) ((i + 1) + Math.ceil(slLineMin1));
                                lastPositive2 = (sliceA * lastPositive1) + sliceB;
                            } else if (slRThreshold[i] > 0.0f) {
                                i = i - thresholdIncrement;

                                if (i < 0) {
                                    transitionFound = true;
                                }

                                thresholdIncrement = thresholdIncrement / 2;

                                if (thresholdIncrement == 0) {
                                    thresholdIncrement = 1;
                                }
                            } // else if (slRThreshold[i] > 0.0f)
                            else { // ((slRThreshold[i] <= 0.0f) || )Float.isNaN(slRThreshold[i])
                                i = i + thresholdIncrement;

                                if (i >= slRThreshold.length) {
                                    transitionFound = true;
                                }

                                thresholdIncrement = thresholdIncrement / 2;

                                if (thresholdIncrement == 0) {
                                    thresholdIncrement = 1;
                                }
                            }
                        } // if (morex && morey)
                        else { // Float.isNaN(slRThreshold[i])
                            slHaveThreshold[i] = true;
                            i = i + thresholdIncrement;

                            if (i >= slRThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // Float.isNaN(slRThreshold[i])
                    } // while (!transitionFound)

                } // if ((lineMax1 - slLineMin1) >= (lineMax2 - slLineMin2))
                else { // ((lineMax1 - slLineMin1) < (lineMax2 - slLineMin2)
                    slThresholdOn1 = false;
                    slRThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(slLineMin2)) + 1];
                    slColocSize = new float[slRThreshold.length];
                    slColocIntensity1 = new float[slRThreshold.length];
                    slColocIntensity2 = new float[slRThreshold.length];

                    for (i = 0; i < slRThreshold.length; i++) {
                        slRThreshold[i] = Float.NaN;
                        slColocSize[i] = 0.0f;
                        slColocIntensity1[i] = 0.0f;
                        slColocIntensity2[i] = 0.0f;
                    }

                    transitionFound = false;
                    i = slRThreshold.length / 2;
                    thresholdIncrement = slRThreshold.length / 4;

                    while (!transitionFound) {
                        secondThreshold = (float) (i + Math.ceil(slLineMin2));
                        threshold = (secondThreshold - sliceB) / sliceA;
                        averagex = 0.0;
                        averagey = 0.0;
                        count = 0;
                        morex = false;
                        morey = false;
                        lastx = sliceBuffer[0];
                        lasty = secondSliceBuffer[0];
                        voiIntensity1Thr = 0.0f;
                        voiIntensity2Thr = 0.0f;

                        for (j = 0; j < thrLength; j++) {

                            if (sliceBuffer[j] >= threshold) {
                                voiIntensity1Thr += sliceBuffer[j];
                            }

                            if (secondSliceBuffer[j] >= secondThreshold) {
                                voiIntensity2Thr += secondSliceBuffer[j];
                            }

                            if ((sliceBuffer[j] >= threshold) && (secondSliceBuffer[j] >= secondThreshold)) {
                                slColocSize[i] += 1.0f;
                                slColocIntensity1[i] += sliceBuffer[j];
                                slColocIntensity2[i] += secondSliceBuffer[j];
                            } else {
                                averagex += sliceBuffer[j];
                                averagey += secondSliceBuffer[j];
                                count++;

                                if (count == 1) {
                                    lastx = sliceBuffer[j];
                                    lasty = secondSliceBuffer[j];
                                } else if (count >= 2) {

                                    if (!morex) {

                                        if (sliceBuffer[j] != lastx) {
                                            morex = true;
                                        }
                                    }

                                    if (!morey) {

                                        if (secondSliceBuffer[j] != lasty) {
                                            morey = true;
                                        }
                                    }
                                } // else if (count >= 2)
                            }
                        } // for (j = 0; j < thrLength; j++)

                        slColocSize[i] = (slColocSize[i] / voiSliceSize) * 100.0f;

                        if (doColocWithThresholds) {
                            slColocIntensity1[i] = (slColocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                            slColocIntensity2[i] = (slColocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                        } else {
                            slColocIntensity1[i] = (slColocIntensity1[i] / voiSliceIntensity1) * 100.0f;
                            slColocIntensity2[i] = (slColocIntensity2[i] / voiSliceIntensity2) * 100.0f;
                        }

                        if (morex && morey) {
                            averagex /= count;
                            averagey /= count;
                            num = 0.0;
                            denom1 = 0.0;
                            denom2 = 0.0;

                            for (j = 0; j < thrLength; j++) {

                                if ((sliceBuffer[j] < threshold) || (secondSliceBuffer[j] < secondThreshold)) {
                                    num += (sliceBuffer[j] - averagex) * (secondSliceBuffer[j] - averagey);
                                    denom1 += (sliceBuffer[j] - averagex) * (sliceBuffer[j] - averagex);
                                    denom2 += (secondSliceBuffer[j] - averagey) * (secondSliceBuffer[j] - averagey);
                                }
                            }

                            slRThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                            slHaveThreshold[i] = true;
                            Preferences.debug("i = " + i + " slRThreshold[i] = " + slRThreshold[i] + "\n", 
                            		Preferences.DEBUG_ALGORITHM);

                            if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                    (slRThreshold[i - 1] <= 0.0f)) {
                                transitionFound = true;
                                nonPositiveThreshold = slRThreshold[i - 1];
                                firstNonPositive2 = (float) ((i - 1) + Math.ceil(slLineMin2));
                                firstNonPositive1 = (firstNonPositive2 - sliceB) / sliceA;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                                lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                            } else if ((i >= 1) && slHaveThreshold[i - 1] && (slRThreshold[i] > 0.0f) &&
                                           Float.isNaN(slRThreshold[i - 1])) {
                                transitionFound = true;
                                firstUndefined2 = (float) ((i - 1) + Math.ceil(slLineMin2));
                                firstUndefined1 = (firstUndefined2 - sliceB) / sliceA;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                                lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                            } else if ((i == 0) && (slRThreshold[i] > 0.0f)) {
                                transitionFound = true;
                                ip = i;
                                minPositiveThreshold = slRThreshold[i];
                                lastPositive2 = (float) (i + Math.ceil(slLineMin2));
                                lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                            } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                           (slRThreshold[i] <= 0.0f) && (slRThreshold[i + 1] > 0.0f)) {
                                transitionFound = true;
                                nonPositiveThreshold = slRThreshold[i];
                                firstNonPositive2 = (float) (i + Math.ceil(slLineMin2));
                                firstNonPositive1 = (firstNonPositive2 - sliceB) / sliceA;
                                ip = i + 1;
                                minPositiveThreshold = slRThreshold[i + 1];
                                lastPositive2 = (float) ((i + 1) + Math.ceil(slLineMin2));
                                lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                            } else if ((i < (slRThreshold.length - 1)) && slHaveThreshold[i + 1] &&
                                           Float.isNaN(slRThreshold[i]) && (slRThreshold[i + 1] > 0.0f)) {
                                transitionFound = true;
                                firstUndefined2 = (float) (i + Math.ceil(slLineMin2));
                                firstUndefined1 = (firstUndefined2 - sliceB) / sliceA;
                                ip = i + 1;
                                minPositiveThreshold = slRThreshold[i + 1];
                                lastPositive2 = (float) ((i + 1) + Math.ceil(slLineMin2));
                                lastPositive1 = (lastPositive2 - sliceB) / sliceA;
                            } else if (slRThreshold[i] > 0.0f) {
                                i = i - thresholdIncrement;

                                if (i < 0) {
                                    transitionFound = true;
                                }

                                thresholdIncrement = thresholdIncrement / 2;

                                if (thresholdIncrement == 0) {
                                    thresholdIncrement = 1;
                                }
                            } // else if (slRThreshold[i] > 0.0f)
                            else { // ((slRThreshold[i] <= 0.0f) || )Float.isNaN(slRThreshold[i])
                                i = i + thresholdIncrement;

                                if (i >= slRThreshold.length) {
                                    transitionFound = true;
                                }

                                thresholdIncrement = thresholdIncrement / 2;

                                if (thresholdIncrement == 0) {
                                    thresholdIncrement = 1;
                                }
                            }
                        } // if (morex && morey)
                        else { // Float.isNaN(slRThreshold[i])
                            slHaveThreshold[i] = true;
                            i = i + thresholdIncrement;

                            if (i >= slRThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // Float.isNaN(slRThreshold[i])
                    } // while (!transitionFound)

                } // else ((lineMax1 - slLineMin1) < (lineMax2 - slLineMin2)

                if ((useRed) && (useGreen)) {

                    if (firstUndefined1 >= 0) {
                        Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("for pixels with red < " + firstUndefined1 + " or green < " +
                                          firstUndefined2 + "\n", Preferences.DEBUG_ALGORITHM);
                    }

                    if (nonPositiveThreshold <= 0) {
                        Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold +
                                          "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("for pixels with red < " + firstNonPositive1 + " or green < " +
                                          firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
                    }

                    Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + lastPositive1 + " or green < " + lastPositive2 + "\n", 
                    		Preferences.DEBUG_ALGORITHM);

                } // if ((useRed) && (useGreen))
                else if ((useRed) && (useBlue)) {

                    if (firstUndefined1 >= 0) {
                        Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("for pixels with red < " + firstUndefined1 + " or blue < " + firstUndefined2 +
                                          "\n", Preferences.DEBUG_ALGORITHM);
                    }

                    if (nonPositiveThreshold <= 0) {
                        Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold +
                                          "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("for pixels with red < " + firstNonPositive1 + " or blue < " +
                                          firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
                    }

                    Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                } // else if ((useRed) && (useBlue))
                else {

                    if (firstUndefined1 >= 0) {
                        Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("for pixels with green < " + firstUndefined1 + " or blue < " +
                                          firstUndefined2 + "\n", Preferences.DEBUG_ALGORITHM);
                    }

                    if (nonPositiveThreshold <= 0) {
                        Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold +
                                          "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("for pixels with green < " + firstNonPositive1 + " or blue < " +
                                          firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
                    }

                    Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with green < " + lastPositive1 + " or blue < " + lastPositive2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (minPositiveThreshold < 0) {
                    Preferences.debug("No positive threshold found on second iteration", Preferences.DEBUG_ALGORITHM);
                    dataLine1 = "slice #" + (z + 1) + " has no positive threshold on second iteration\n";
                    ViewUserInterface.getReference().setDataText(dataLine1);
                    ViewUserInterface.getReference().setDataText("\n");

                    continue;
                }
            } // if (doSecondIteration)

            if (ip >= 0) {

                if (slThresholdOn1) {
                    threshold = slRThreshold[ip];
                    colocAreaPercent = slColocSize[ip];
                    colocIntensityPercent1 = slColocIntensity1[ip];
                    colocIntensityPercent2 = slColocIntensity2[ip];
                } else {
                    threshold = slRThreshold[ip];
                    colocAreaPercent = slColocSize[ip];
                    colocIntensityPercent1 = slColocIntensity1[ip];
                    colocIntensityPercent2 = slColocIntensity2[ip];
                }

                dataLine1 = "slice #\t%coloc area\t";
                dataLine2 = (z + 1) + "\t" + colocAreaPercent + "\t";

                if (useRed && useGreen) {
                    dataLine1 = dataLine1 + "% red coloc\t% green coloc\t";
                    dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
                } else if (useRed && useBlue) {
                    dataLine1 = dataLine1 + "% red coloc\t% blue coloc\t";
                    dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
                } else {
                    dataLine1 = dataLine1 + "% green coloc\t% blue coloc\t";
                    dataLine2 = dataLine2 + colocIntensityPercent1 + "\t" + colocIntensityPercent2 + "\t";
                }

                if (doSecondIteration) {
                    dataLine1 = dataLine1 + "Linear correlation coefficient - 2 iters\n";
                    dataLine2 = dataLine2 + sliceR + "\n";
                } else {
                    dataLine1 = dataLine1 + "Linear correlation coefficient - 1 iter\n";
                    dataLine2 = dataLine2 + sliceR + "\n";
                }

                ViewUserInterface.getReference().setDataText(dataLine1);
                ViewUserInterface.getReference().setDataText(dataLine2);
                ViewUserInterface.getReference().setDataText("\n");

                nf = NumberFormat.getNumberInstance();
                nf.setMaximumFractionDigits(6);
            } // if (ip >= 0)
        } // for (z = 0; z < zDim; z++)
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ch1  the bin1 channel used
     * @param  ch2  the bin2 channel used
     */
    public void calculateFreeRangeThreshold(int ch1, int ch2) {
        double averagex, averagey;
        int count;
        boolean morex, morey;
        int j;
        float lastx, lasty;
        double num, denom1, denom2;
        int index;
        float voiIntensity1Thr;
        float voiIntensity2Thr;
        int thresholdIndex1;
        int thresholdIndex2;

        index = ch1 + (bin1 * ch2);
        thresholdIndex1 = (int) Math.round((ch1 / scale1) + min1);
        thresholdIndex2 = (int) Math.round((ch2 / scale2) + min2);
        haveFreeRangeThreshold[index] = true;
        averagex = 0.0;
        averagey = 0.0;
        count = 0;
        morex = false;
        morey = false;
        lastx = buffer[0];
        lasty = secondBuffer[0];
        voiIntensity1Thr = 0.0f;
        voiIntensity2Thr = 0.0f;

        for (j = 0; j < thrLength; j++) {
            float buff = buffer[j];
            float secBuff = secondBuffer[j];

            if (buff >= thresholdIndex1) {
                voiIntensity1Thr += buff;
            }

            if (secBuff >= thresholdIndex2) {
                voiIntensity2Thr += secBuff;
            }

            if ((buff >= thresholdIndex1) && (secBuff >= thresholdIndex2)) {
                freeRangeColocSize[index] += 1.0f;
                freeRangeColocIntensity1[index] += buff;
                freeRangeColocIntensity2[index] += secBuff;
            } else {
                averagex += buff;
                averagey += secBuff;
                count++;

                if (count == 1) {
                    lastx = buff;
                    lasty = secBuff;
                } else if (count >= 2) {

                    if (!morex) {

                        if (buff != lastx) {
                            morex = true;
                        }
                    }

                    if (!morey) {

                        if (secBuff != lasty) {
                            morey = true;
                        }
                    }
                } // else if (count >= 2)
            }
        } // for (j = 0; j < thrLength; j++)

        if (morex && morey) {
            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (j = 0; j < thrLength; j++) {
                float buff = buffer[j];
                float secBuff = secondBuffer[j];

                if ((buff < thresholdIndex1) || (secBuff < thresholdIndex2)) {
                    num += (buff - averagex) * (secBuff - averagey);
                    denom1 += (buff - averagex) * (buff - averagex);
                    denom2 += (secBuff - averagey) * (secBuff - averagey);
                }
            } // for (j = 0; j < thrLength; j++)

            freeRangeRThreshold[index] = (float) (num / Math.sqrt(denom1 * denom2));
        } // if (moreX && morey)

        freeRangeColocSize[index] = (freeRangeColocSize[index] / voiSize) * 100.0f;

        if (doColocWithThresholds) {
            freeRangeColocIntensity1[index] = (freeRangeColocIntensity1[index] / voiIntensity1Thr) * 100.0f;
            freeRangeColocIntensity2[index] = (freeRangeColocIntensity2[index] / voiIntensity2Thr) * 100.0f;
        } else {
            freeRangeColocIntensity1[index] = (freeRangeColocIntensity1[index] / voiIntensity1) * 100.0f;
            freeRangeColocIntensity2[index] = (freeRangeColocIntensity2[index] / voiIntensity2) * 100.0f;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  thresholdIndex  the index along the line segment
     */
    public void calculateThreshold(int thresholdIndex) {
        float threshold, secondThreshold;
        double averagex, averagey;
        int count;
        boolean morex, morey;
        int j;
        float lastx, lasty;
        double num, denom1, denom2;
        float voiIntensity1Thr;
        float voiIntensity2Thr;

        haveThreshold[thresholdIndex] = true;

        if (thresholdOn1) {
            threshold = (float) (thresholdIndex + Math.ceil(lineMin1));
            secondThreshold = (a * threshold) + b;
        } else {
            secondThreshold = (float) (thresholdIndex + Math.ceil(lineMin2));
            threshold = (secondThreshold - b) / a;
        }

        averagex = 0.0;
        averagey = 0.0;
        count = 0;
        morex = false;
        morey = false;
        lastx = buffer[0];
        lasty = secondBuffer[0];
        voiIntensity1Thr = 0.0f;
        voiIntensity2Thr = 0.0f;

        for (j = 0; j < thrLength; j++) {
            float buff = buffer[j];
            float secBuff = secondBuffer[j];

            if (buff >= threshold) {
                voiIntensity1Thr += buff;
            }

            if (secBuff >= secondThreshold) {
                voiIntensity2Thr += secBuff;
            }

            if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                colocSize[thresholdIndex] += 1.0f;
                colocIntensity1[thresholdIndex] += buff;
                colocIntensity2[thresholdIndex] += secBuff;
            } else {
                averagex += buff;
                averagey += secBuff;
                count++;

                if (count == 1) {
                    lastx = buff;
                    lasty = secBuff;
                } else if (count >= 2) {

                    if (!morex) {

                        if (buff != lastx) {
                            morex = true;
                        }
                    }

                    if (!morey) {

                        if (secBuff != lasty) {
                            morey = true;
                        }
                    }
                } // else if (count >= 2)
            }
        } // for (j = 0; j < thrLength; j++)

        if (morex && morey) {
            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (j = 0; j < thrLength; j++) {
                float buff = buffer[j];
                float secBuff = secondBuffer[j];

                if ((buff < threshold) || (secBuff < secondThreshold)) {
                    num += (buff - averagex) * (secBuff - averagey);
                    denom1 += (buff - averagex) * (buff - averagex);
                    denom2 += (secBuff - averagey) * (secBuff - averagey);
                }
            } // for (j = 0; j < thrLength; j++)

            rThreshold[thresholdIndex] = (float) (num / Math.sqrt(denom1 * denom2));
        } // if (moreX && morey)

        colocSize[thresholdIndex] = (colocSize[thresholdIndex] / voiSize) * 100.0f;

        if (doColocWithThresholds) {
            colocIntensity1[thresholdIndex] = (colocIntensity1[thresholdIndex] / voiIntensity1Thr) * 100.0f;
            colocIntensity2[thresholdIndex] = (colocIntensity2[thresholdIndex] / voiIntensity2Thr) * 100.0f;
        } else {
            colocIntensity1[thresholdIndex] = (colocIntensity1[thresholdIndex] / voiIntensity1) * 100.0f;
            colocIntensity2[thresholdIndex] = (colocIntensity2[thresholdIndex] / voiIntensity2) * 100.0f;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     */
    public void createFreeRangeArrays() {
        int i;
        int freeRangeSize = bin1 * bin2;
        haveFreeRangeThreshold = new boolean[freeRangeSize];
        freeRangeRThreshold = new float[freeRangeSize];
        freeRangeColocSize = new float[freeRangeSize];
        freeRangeColocIntensity1 = new float[freeRangeSize];
        freeRangeColocIntensity2 = new float[freeRangeSize];

        for (i = 0; i < freeRangeSize; i++) {
            haveFreeRangeThreshold[i] = false;
            freeRangeRThreshold[i] = Float.NaN;
            freeRangeColocSize[i] = 0.0f;
            freeRangeColocIntensity1[i] = 0.0f;
            freeRangeColocIntensity2[i] = 0.0f;
        }

        frameColocalize.passFreeRangeArrays(haveFreeRangeThreshold, freeRangeRThreshold, freeRangeColocSize,
                                            freeRangeColocIntensity1, freeRangeColocIntensity2);
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        buffer = null;
        secondBuffer = null;

        srcImage.getParentFrame().getComponentImage().getVOIHandler().removeVOIUpdateListener(this);
        srcImage.clearMask();
        baseImage = null;
        destImage = null;
        srcImage = null;
        rThreshold = null;
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     */
    public void removeVOIUpdateListener() {

        if (subtractedSrcImage != null) {
            subtractedSrcImage.getParentFrame().getComponentImage().getVOIHandler().removeVOIUpdateListener(this);
        }
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
     * responds to the volume of interest (<code>VOI</code>) change events.
     *
     * <p>This method calls <code>updateVOI</code> using the <code>UpdateVOIEvent</code> changed <code>VOI</code>, and
     * retrieves the runningInSeparateThread out of the current image's frame.</p>
     *
     * @see  UpdateVOIEvent
     * @see  #updateVOI
     * @see  ViewJFrameBase#getActiveImage
     */
    public void selectionChanged(UpdateVOIEvent newVOIselection) {
        VOI voi = newVOIselection.getChangedVolumeOfInterest();

        if (voi.getCurveType() != VOI.CONTOUR) {
            return;
        }

        subtractedSrcImage.clearMask();
        subtractedSrcImage.notifyImageDisplayListeners();
        this.mask = subtractedSrcImage.generateVOIMask();
        redo = true;

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
     * values represented across the y axis. The total least squares line and a matrix of correlation coefficients for L
     * shaped subthreshold regions are also generated.
     */
    private void calc2D() {
        int i;
        int pos;
        float[] randomBuffer;
        double[] histBuffer;
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
        boolean transformVOI = true;
        boolean clip = true;
        TransMatrix xfrm;
        double max1, max2, range1, range2;
        double averagex, averagey;
        double diffx, diffy;
        double varx, vary, covarxy;
        float mse; // mean square error
        float halfWidth;
        float distance;
        int iter;
        boolean doAgain;
        float aLast;
        float bLast;
        boolean morex, morey;
        float lastx, lasty;

        // AlgorithmEigensolver kES;
        // double directionx;
        // double directiony;
        int ch1, ch2;
        float ch1f, ch2f;
        double lineMax1, lineMax2;
        float[] xArray;
        float[] yArray;
        float[] zArray;
        VOI fitLineVOI;
        VOI pointVOI;
        double num, denom1, denom2, denom;
        String name;
        ModelImage resultImage = null;
        AlgorithmAutoCovariance algoAutoCovariance;
        int fwhmS = Integer.MAX_VALUE;
        int fwhmB = Integer.MAX_VALUE;
        int fwhm = Integer.MAX_VALUE;
        boolean scrambleFirst;
        int xDim, yDim;
        int length;
        int x, y;
        int numSquares;
        Vector<Integer> vectRand;
        RandomNumberGen randomGen;
        int randNum;
        Integer randSquare;
        int s, j, k;
        int sx, sy;
        int numXValues;
        int numYValues;
        float[] rMat = new float[200];
        int numBins;
        float[] rCum;
        float[] pStatMat;
        boolean found;
        int xRemainder = 0;
        int yRemainder = 0;
        int xStart = 0;
        int yStart = 0;
        int xEnd;
        int yEnd;
        double sumx;
        double sumx2;
        double sumy;
        double sumxy;
        double sumy2;
        float threshold;
        float secondThreshold;
        int count;
        float nonPositiveThreshold;
        float minPositiveThreshold;
        float lastPositive1, lastPositive2;
        float firstNonPositive1, firstNonPositive2;
        float firstUndefined1, firstUndefined2;
        Vector<ViewImageUpdateInterface> frameList;
        ViewJFrameBase controlFrame = null;
        float[] destRes;
        boolean transitionFound;
        int thresholdIncrement;
        float[] tmpBuffer;
        float[] tmpSecondBuffer;
        float voiIntensity1Thr;
        float voiIntensity2Thr;
        float backC1 = 0.0f;
        float backC2 = 0.0f;

        try {

            fireProgressStateChanged(srcImage.getImageName(), "Creating 2D Colocolization Histogram ...");

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

            if ((backgroundIndex != -1) && (!redo)) {
                fireProgressStateChanged("Calculating backround average");
                fireProgressStateChanged(2);

                for (i = 0; i < length; i++) {

                    if (subtractionMask[i] == backgroundIndex) {
                        backC1 += srcImage.getFloat(i);
                    }
                }

                backC1 /= length;
            } // if ((backgroundIndex != -1) && (!redo))

            if ((backgroundIndex2 != -1) && (!redo)) {
                fireProgressStateChanged("Calculating backround average");
                fireProgressStateChanged(5);

                for (i = 0; i < length; i++) {

                    if (subtractionMask2[i] == backgroundIndex2) {
                        backC2 += baseImage.getFloat(i);
                    }
                }

                backC2 /= length;
            } // if ((backgroundIndex2 != -1) && (!redo))

            if ((register) && (!redo)) {
                fireProgressStateChanged("Registering images");
                fireProgressStateChanged(7);
                // Register each image half of the way to the same center point

                reg2 = new AlgorithmRegOAR2D(srcImage, baseImage, cost, DOF, interp, coarseBegin, coarseEnd, coarseRate,
                                             fineRate, doSubsample, doMultiThread);
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

                fireProgressStateChanged(10);
                transform = new AlgorithmTransform(baseImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA, xDim,
                                                   yDim, false, clip, false);
                transform.run();
                registeredBaseImage = transform.getTransformedImage();
                updateFileInfo(baseImage, registeredBaseImage);
                registeredBaseImage.calcMinMax();
                registeredBaseImage.setImageName(baseImage.getImageName() + "_register");
                transform.disposeLocal();
                transform = null;

                bin2 = Math.min(bin2,
                                (int) Math.round(registeredBaseImage.getMax() - registeredBaseImage.getMin() + 1));

                reg2 = new AlgorithmRegOAR2D(baseImage, srcImage, cost, DOF, interp, coarseBegin, coarseEnd, coarseRate,
                                             fineRate, doSubsample, doMultiThread);
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

                transform = new AlgorithmTransform(srcImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA, xDim,
                                                   yDim, transformVOI, clip, false);
                transform.run();
                registeredSrcImage = transform.getTransformedImage();
                updateFileInfo(srcImage, registeredSrcImage);
                registeredSrcImage.calcMinMax();
                registeredSrcImage.setImageName(srcImage.getImageName() + "_register");
                transform.disposeLocal();
                srcImage.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                srcImage.clearMask();
                srcImage.notifyImageDisplayListeners();
                transform = null;

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
            } // if ((register) && (!redo))
            else if (!redo) {
                registeredSrcImage = srcImage;
                registeredBaseImage = baseImage;
            } // else if (!redo)

            if ((backgroundIndex != -1) && (!redo)) {
                fireProgressStateChanged("Subtracting background from " + srcImage.getImageName());
                fireProgressStateChanged(12);
                subtractedSrcImage = new ModelImage(srcImage.getType(), srcImage.getExtents(),
                                                    registeredSrcImage.getImageName() + "_subtract");
                buffer = new float[length];

                try {
                    registeredSrcImage.exportData(0, length, buffer);
                } catch (IOException e) {
                    buffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                for (i = 0; i < length; i++) {
                    buffer[i] -= backC1;

                    if (buffer[i] < 0.0f) {
                        buffer[i] = 0.0f;
                    }
                }

                try {
                    subtractedSrcImage.importData(0, buffer, true);
                } catch (IOException e) {
                    buffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                updateFileInfo(registeredSrcImage, subtractedSrcImage);

                try {
                    imageFrame3 = new ViewJFrameImage(subtractedSrcImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Unable to open frame for subtracted image");
                    setCompleted(false);

                    return;
                }

                srcImage.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                srcImage.clearMask();
                srcImage.notifyImageDisplayListeners();

                bin1 = Math.min(bin1, (int) Math.round(subtractedSrcImage.getMax() - subtractedSrcImage.getMin() + 1));

                int[] newExtents = new int[2];
                newExtents[0] = bin1 + leftPad + rightPad;
                newExtents[1] = bin2 + bottomPad + topPad;
                destImage.changeExtents(newExtents);
            } // if (backgroundIndex != -1) && (!redo))
            else if (!redo) {
                subtractedSrcImage = registeredSrcImage;
            } // else if (!redo)

            if ((backgroundIndex2 != -1) && (!redo)) {
                fireProgressStateChanged("Subtracting background from " + baseImage.getImageName());
                fireProgressStateChanged(15);
                subtractedBaseImage = new ModelImage(baseImage.getType(), baseImage.getExtents(),
                                                     registeredBaseImage.getImageName() + "_subtract");
                buffer = new float[length];

                try {
                    registeredBaseImage.exportData(0, length, buffer);
                } catch (IOException e) {
                    buffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                for (i = 0; i < length; i++) {
                    buffer[i] -= backC1;

                    if (buffer[i] < 0.0f) {
                        buffer[i] = 0.0f;
                    }
                }

                try {
                    subtractedBaseImage.importData(0, buffer, true);
                } catch (IOException e) {
                    buffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                updateFileInfo(registeredBaseImage, subtractedBaseImage);

                try {
                    imageFrame4 = new ViewJFrameImage(subtractedBaseImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Unable to open frame for subtracted image");
                    setCompleted(false);

                    return;
                }

                baseImage.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                baseImage.clearMask();
                baseImage.notifyImageDisplayListeners();

                bin2 = Math.min(bin2,
                                (int) Math.round(subtractedBaseImage.getMax() - subtractedBaseImage.getMin() + 1));

                int[] newExtents = new int[2];
                newExtents[0] = bin1 + leftPad + rightPad;
                newExtents[1] = bin2 + bottomPad + topPad;
                destImage.changeExtents(newExtents);
            } // if (backgroundIndex2 != -1) && (!redo))
            else if (!redo) {
                subtractedBaseImage = registeredBaseImage;
            } // else if (!redo)

            if ((!entireImage) && (!maskSupplied) && (!redo)) {
                subtractedSrcImage.getParentFrame().getComponentImage().getVOIHandler().addVOIUpdateListener(this);
            }

            try {
                buffer = null;
                buffer = new float[length];
                secondBuffer = null;
                secondBuffer = new float[length];

                // Blank spaces at left and bottom
                histBuffer = null;
                histBuffer = new double[(bin1 + leftPad + rightPad) * (bin2 + bottomPad + topPad)];
            } catch (OutOfMemoryError oome) {
                buffer = null;
                secondBuffer = null;
                histBuffer = null;
                errorCleanUp("Algorithm ColocalizationRegression: Out of memory", true);

                return;
            }

            for (i = 0; i < histBuffer.length; i++) {
                histBuffer[i] = 0.0;
            }

            destImage.releaseLock(); // we need to be able to alter the dest image

            k = 0;

            for (j = 0; j <= (yDim - 1); j++) {

                for (i = 0; i <= (xDim - 1); i++) {
                    pos = i + (xDim * j);
                    buffer[k] = subtractedSrcImage.getFloat(pos);
                    secondBuffer[k++] = subtractedBaseImage.getFloat(pos);
                }
            }

            voiSize = 0;
            voiIntensity1 = 0.0f;
            voiIntensity2 = 0.0f;

            for (i = 0; i < length; i++) {

                if (inputMask.get(i)) {
                    voiSize++;

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        voiIntensity1 += buffer[i];
                        voiIntensity2 += secondBuffer[i];
                    }
                }
            }

            fireProgressStateChanged("Calculating overall linear correlation coefficient");
            fireProgressStateChanged(20);
            min1 = Double.MAX_VALUE;
            max1 = -Double.MAX_VALUE;
            min2 = Double.MAX_VALUE;
            max2 = -Double.MAX_VALUE;

            for (i = 0; i < length; i++) {

                if (buffer[i] > max1) {
                    max1 = buffer[i];
                }

                if (buffer[i] < min1) {
                    min1 = buffer[i];
                }

                if (secondBuffer[i] > max2) {
                    max2 = secondBuffer[i];
                }

                if (secondBuffer[i] < min2) {
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
            averagex = 0.0;
            averagey = 0.0;
            count = 0;

            for (i = 0; i < length; i++) {

                if (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) && inputMask.get(i)) {
                    averagex += buffer[i];
                    averagey += secondBuffer[i];
                    count++;
                }
            }

            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (i = 0; i < length; i++) {

                if (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) && inputMask.get(i)) {
                    num += (buffer[i] - averagex) * (secondBuffer[i] - averagey);
                    denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                    denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                }
            }

            denom = Math.sqrt(denom1 * denom2);
            r = (float) (num / denom);
            Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);

            if (doP) {
                fireProgressStateChanged("Calculating autocovariance");
                fireProgressStateChanged(10);

                name = srcImage.getImageName() + "_autocovarianceS";
                resultImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), name);

                algoAutoCovariance = new AlgorithmAutoCovariance(resultImage, subtractedSrcImage);
                algoAutoCovariance.run();

                fwhmS = algoAutoCovariance.getFWHM();
                fwhm = fwhmS;

                if (fwhmS != Integer.MAX_VALUE) {
                    ViewUserInterface.getReference().setDataText(srcImage.getImageName() +
                                                                 " auto covariance full width at half maximum = " +
                                                                 fwhmS + "\n");
                    Preferences.debug(srcImage.getImageName() + " auto covariance full width at half maximum = " +
                                      fwhmS + "\n", Preferences.DEBUG_ALGORITHM);
                } else {
                    ViewUserInterface.getReference().setDataText("Cannot determine " + srcImage.getImageName() +
                                                                 " auto covariance full width at half maximum\n");
                    Preferences.debug("Cannot determine " + srcImage.getImageName() +
                                      " auto covariance full width at half maximum\n", Preferences.DEBUG_ALGORITHM);
                }

                algoAutoCovariance.finalize();
                algoAutoCovariance = null;

                algoAutoCovariance = new AlgorithmAutoCovariance(resultImage, subtractedBaseImage);
                algoAutoCovariance.run();

                fwhmB = algoAutoCovariance.getFWHM();
                fwhm = Math.min(fwhmS, fwhmB);

                if (fwhmB != Integer.MAX_VALUE) {
                    ViewUserInterface.getReference().setDataText(baseImage.getImageName() +
                                                                 " auto covariance full width at half maximum = " +
                                                                 fwhmB + "\n");
                    Preferences.debug(baseImage.getImageName() + " auto covariance full width at half maximum = " +
                                      fwhmB + "\n", Preferences.DEBUG_ALGORITHM);
                } else {
                    ViewUserInterface.getReference().setDataText("Cannot determine " + baseImage.getImageName() +
                                                                 " auto covariance full width at half maximum\n");
                    Preferences.debug("Cannot determine " + baseImage.getImageName() +
                                      " auto covariance full width at half maximum\n", Preferences.DEBUG_ALGORITHM);
                }

                algoAutoCovariance.finalize();
                algoAutoCovariance = null;

                resultImage.disposeLocal();
                resultImage = null;

                if (fwhm == Integer.MAX_VALUE) {
                    finalize();
                    errorCleanUp("Algorithm ColocalizationRegression could not determine fwhm", true);

                    return;
                } else if (fwhm == fwhmS) {
                    scrambleFirst = true;
                } else {
                    scrambleFirst = false;
                }

                if (fwhm == 0) {
                    fwhm = 1;
                }

                fireProgressStateChanged("Calculating P-value");
                fireProgressStateChanged(15); // updateValue(15);

                // Calculate denom without inputMask
                averagex = 0.0;
                averagey = 0.0;
                count = 0;

                for (i = 0; i < length; i++) {

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        averagex += buffer[i];
                        averagey += secondBuffer[i];
                        count++;
                    }
                }

                averagex /= count;
                averagey /= count;
                denom1 = 0.0;
                denom2 = 0.0;

                for (i = 0; i < length; i++) {

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                        denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                    }
                }

                denom = Math.sqrt(denom1 * denom2);
                randomBuffer = null;
                randomBuffer = new float[length];
                numXValues = xDim / fwhm;
                xRemainder = xDim % fwhm;
                numYValues = yDim / fwhm;
                yRemainder = yDim % fwhm;
                numSquares = numXValues * numYValues;
                randomGen = new RandomNumberGen();
                vectRand = new Vector<Integer>();

                int idx;
                int end;

                for (i = 0; i < 200; i++) {

                    for (s = 0; s < numSquares; s++) {
                        vectRand.add(new Integer(s));
                    }

                    for (s = 0; s < numSquares; s++) {
                        randNum = randomGen.genUniformRandomNum(0, vectRand.size() - 1);
                        xStart = randomGen.genUniformRandomNum(0, xRemainder);
                        xEnd = xDim - (xRemainder - xStart);
                        yStart = randomGen.genUniformRandomNum(0, yRemainder);
                        yEnd = yDim - (yRemainder - yStart);
                        randSquare = vectRand.elementAt(randNum);
                        vectRand.removeElementAt(randNum);
                        y = (fwhm * (randSquare.intValue() / numXValues)) + yStart;
                        x = (fwhm * (randSquare.intValue() % numXValues)) + xStart;
                        sy = (fwhm * (s / numXValues)) + yStart;
                        sx = (fwhm * (s % numXValues)) + xStart;

                        for (k = 0; k < fwhm; k++) {

                            for (j = 0; j < fwhm; j++) {

                                if (scrambleFirst) {
                                    randomBuffer[sx + j + ((sy + k) * xDim)] = buffer[x + j + ((y + k) * xDim)];
                                } else {
                                    randomBuffer[sx + j + ((sy + k) * xDim)] = secondBuffer[x + j + ((y + k) * xDim)];
                                }
                            } // for (j = 0; j < fwhm; j++)
                        } // for (k = 0; k < fwhm; k++)

                        for (y = 0; y < yDim; y++) {
                            idx = y * xDim;
                            end = idx + xStart;

                            for (x = idx; x < end; x++) {

                                if (scrambleFirst) {
                                    randomBuffer[x] = buffer[x];
                                } else {
                                    randomBuffer[x] = secondBuffer[x];
                                }
                            }
                        }

                        for (y = 0; y < yDim; y++) {
                            idx = y * xDim;

                            for (x = xEnd; x < xDim; x++) {

                                if (scrambleFirst) {
                                    randomBuffer[x + idx] = buffer[x + idx];
                                } else {
                                    randomBuffer[x + idx] = secondBuffer[x + idx];
                                }
                            }
                        }

                        for (y = 0; y < yStart; y++) {
                            idx = y * xDim;
                            end = idx + xDim;

                            for (x = idx; x < end; x++) {

                                if (scrambleFirst) {
                                    randomBuffer[x] = buffer[x];
                                } else {
                                    randomBuffer[x] = secondBuffer[x];
                                }
                            }
                        }

                        for (y = yEnd; y < yDim; y++) {
                            idx = y * xDim;
                            end = idx + xDim;

                            for (x = idx; x < end; x++) {

                                if (scrambleFirst) {
                                    randomBuffer[x] = buffer[x];
                                } else {
                                    randomBuffer[x] = secondBuffer[x];
                                }
                            }
                        }
                    } // for (s = 0; s < numSquares; s++)

                    if (scrambleFirst) {
                        num = 0.0f;

                        for (j = 0; j < length; j++) {

                            if ((randomBuffer[j] >= background1) || (secondBuffer[j] >= background2)) {
                                num += (randomBuffer[j] - averagex) * (secondBuffer[j] - averagey);
                            }
                        }

                        rMat[i] = (float) (num / denom);
                    } else {
                        num = 0.0f;

                        for (j = 0; j < length; j++) {

                            if ((buffer[j] >= background1) || (randomBuffer[j] >= background2)) {
                                num += (buffer[j] - averagex) * (randomBuffer[j] - averagey);
                            }
                        }

                        rMat[i] = (float) (num / denom);
                    }
                } // for (i = 0; i < 200; i++)

                randomBuffer = null;

                Arrays.sort(rMat);
                numBins = 200;

                for (i = 1; i < 200; i++) {

                    if (rMat[i] == rMat[i - 1]) {
                        numBins--;
                    }
                }

                rCum = new float[numBins];
                pStatMat = new float[numBins];
                rCum[0] = rMat[0];
                pStatMat[0] = 0.0f;
                pStatMat[1] = 0.005f;
                j = 1;

                for (i = 1; i < 200; i++) {

                    if (rMat[i] == rMat[i - 1]) {
                        pStatMat[j] += 0.005f;
                    } else {
                        rCum[j++] = rMat[i];

                        if (j < numBins) {
                            pStatMat[j] = pStatMat[j - 1] + 0.005f;
                        }
                    }
                } // for (i = 1; i < 200; i++)

                PValue = 0.995f;
                found = false;

                for (i = 0; (i <= (numBins - 1)) && (!found); i++) {

                    if (r <= rCum[i]) {
                        PValue = pStatMat[i];
                        found = true;
                    }
                }

                Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("numBins = " + numBins + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("rCum[0] = " + rCum[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("rCum[numBins-1] = " + rCum[numBins - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("P-value = " + PValue + "\n", Preferences.DEBUG_ALGORITHM);

                if (threadStopped) {
                    finalize();

                    return;
                }

                if (PValue < 0.95f) {

                    setCompleted(true);

                    return;
                }

            } // if (doP)

            // Reduce buffer and secondBuffer so that they only contain the points
            // where (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) &&
            // inputMask.get(i))
            thrLength = 0;
            tmpBuffer = null;
            tmpBuffer = new float[length];

            tmpSecondBuffer = new float[length];

            for (i = 0; i < length; i++) {

                if (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) && inputMask.get(i)) {
                    tmpBuffer[thrLength] = buffer[i];
                    tmpSecondBuffer[thrLength++] = secondBuffer[i];
                }
            }

            buffer = null;
            buffer = new float[thrLength];

            secondBuffer = null;

            secondBuffer = new float[thrLength];

            for (i = 0; i < thrLength; i++) {
                buffer[i] = tmpBuffer[i];
                secondBuffer[i] = tmpSecondBuffer[i];
            }

            tmpBuffer = null;
            tmpSecondBuffer = null;
            System.gc();

            // Use linear least squares to calculate the best line fit
            // for secondBuffer = a*firstBuffer + b.  Use an orthogonal
            // line fit where the distance measurements are orthgonal to
            // the proposed line.  This is an orthogonal regression as opposed
            // to a traditional regression of dependent y variable green on
            // independent red variable x which would give measurements in the
            // y direction.  Note that a traditional regression of red on green
            // would be yet another line giving measurements in the x direction.
            // Don't include any point i with buffer[i] < background1 and
            // secondBuffer[i] < background2 in the regression line.
            // Don't want the regression line to be affected by the dataless
            // black background of the picture.
            fireProgressStateChanged("Calculating least squares fit line");
            fireProgressStateChanged(25);
            sumx = 0.0;
            sumy = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {
                sumx += buffer[i];
                sumy += secondBuffer[i];
                count++;
            }

            averagex = sumx / count;
            averagey = sumy / count;
            sumx2 = 0.0;
            sumxy = 0.0;
            sumy2 = 0.0;

            for (i = 0; i < thrLength; i++) {
                diffx = buffer[i] - averagex;
                diffy = secondBuffer[i] - averagey;
                sumx2 += diffx * diffx;
                sumxy += diffx * diffy;
                sumy2 += diffy * diffy;
            }

            /*kES = new AlgorithmEigensolver(2);
             * kES.setMatrix(0,0,sumy2); kES.setMatrix(0,1,-sumxy); kES.setMatrix(1,0,-sumxy); kES.setMatrix(1,1,sumx2);
             * kES.solve(); // Eigenvectors are stored in increasing order // Get the eigenvector for the smallest value
             * // The eigenvector of the ith eigenvalue is stored in // the ith column directionx =
             * kES.getEigenvector(0,0); directiony = kES.getEigenvector(1,0); delta = directiony/directionx; a =
             * (float)(delta);*/
            float aa;
            varx = sumx2 / (count - 1);
            vary = sumy2 / (count - 1);
            covarxy = sumxy / (count - 1);
            a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                             (2 * covarxy));
            aa = a * a;
            b = (float) (averagey - (a * averagex));
            mse = (float) (varx + (2 * a * covarxy) + (aa * vary)) / (1 + aa);

            // +-2 standard deviations on each side includes about 95%.
            halfWidth = (float) (2.0 * Math.sqrt(mse));

            Preferences.debug("First iteration\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug(baseImage.getImageName() + " = " + a + "*" + srcImage.getImageName() + " + " + b + "\n", 
            		Preferences.DEBUG_ALGORITHM);

            Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);
            doAgain = false;

            for (iter = 0; (iter < 10) && doAgain; iter++) {
                float invAAp1 = 1 / (aa + 1);
                doAgain = false;
                sumx = 0.0;
                sumy = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {
                    distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) * invAAp1;

                    if (distance <= halfWidth) {
                        sumx += buffer[i];
                        sumy += secondBuffer[i];
                        count++;
                    }
                }

                averagex = sumx / count;
                averagey = sumy / count;
                sumx2 = 0.0;
                sumxy = 0.0;
                sumy2 = 0.0;

                for (i = 0; i < thrLength; i++) {
                    distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) * invAAp1;

                    if (distance <= halfWidth) {
                        diffx = buffer[i] - averagex;
                        diffy = secondBuffer[i] - averagey;
                        sumx2 += diffx * diffx;
                        sumxy += diffx * diffy;
                        sumy2 += diffy * diffy;
                    }
                }

                varx = sumx2 / (count - 1);
                vary = sumy2 / (count - 1);
                covarxy = sumxy / (count - 1);
                aLast = a;
                bLast = b;
                a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                 (2 * covarxy));
                aa = a * a;
                b = (float) (averagey - (a * averagex));
                mse = (float) (varx + (2 * a * covarxy) + (aa * vary)) / (1 + aa);

                // +-2 standard deviations on each side includes about 95%.
                halfWidth = (float) (2.0 * Math.sqrt(mse));

                Preferences.debug("Iteration = " + (iter + 2) + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug(baseImage.getImageName() + " = " + a + "*" + srcImage.getImageName() + " + " + b +
                                  "\n", Preferences.DEBUG_ALGORITHM);

                Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

                if ((aLast != 0.0f) && ((Math.abs(a - aLast) / aLast) > 0.001)) {
                    doAgain = true;
                }

                if ((bLast != 0.0f) && ((Math.abs(b - bLast) / bLast) > 0.001)) {
                    doAgain = true;
                }
            } // for (iter = 0; (iter < 10) && doAgain; iter++)

            fireProgressStateChanged("Generating histogram buffer");
            fireProgressStateChanged(50);

            for (i = 0; i < thrLength; i++) {
                ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                // invert y
                histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
            }

            fireProgressStateChanged("Generating VOI for least squares line");
            fireProgressStateChanged(60);
            xArray = new float[2];
            yArray = new float[2];
            zArray = new float[2];
            zArray[0] = 0.0f;
            zArray[1] = 0.0f;

            // secondBuffer[i] = a*buffer[i] + b
            // ch1 = (buffer[i] - min1)*scale1 + leftPad
            // ch2 = (secondBuffer[i] - min2)*scale2 + topPad;
            // ch1 = (scale1/a)*[((ch2-topPad)/scale2) + min2 - a*min1 - b] +
            // leftPad
            // ch2 = scale2*[((a*(ch1-leftPad))/scale1) + a*min1 - min2 + b] +
            // topPad
            ch2f = topPad;
            ch1f = (float) ((scale1 / a) * (min2 - (a * min1) - b)) + leftPad;

            if (ch1f < leftPad) {
                ch1f = leftPad;
                ch2f = (float) (scale2 * ((a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[0] = ch1f;
            yArray[0] = ch2f;
            ch2f = topPad + bin2 - 1;
            ch1f = (float) ((scale1 / a) * (((ch2f - topPad) / scale2) + min2 - (a * min1) - b)) + leftPad;

            if (ch1f > (bin1 - 1 + leftPad)) {
                ch1f = bin1 - 1 + leftPad;
                ch2f = (float) (scale2 * (((a * (bin1 - 1)) / scale1) + (a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[1] = ch1f;
            yArray[1] = ch2f;

            if (!doSecondIteration) {
                fitLineVOI = new VOI((short) 0, "fitLine", VOI.LINE, -1.0f);
                fitLineVOI.importCurve(xArray, yArray, zArray);
                destImage.registerVOI(fitLineVOI);
                fitLineVOI.setFixed(true);
                fitLineVOI.setColor(Color.orange);
            } // if (!doSecondIteration)

            if (threadStopped) { // do before copying back into image
                finalize();

                return;
            }

            destImage.importData(0, histBuffer, true);
            destRes = new float[2];
            destRes[0] = 1.0f;
            destRes[1] = 1.0f;
            destImage.getFileInfo(0).setResolutions(destRes);

        } // try
        catch (IOException error) {
            displayError("Algorithm ColocalizationRegression reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            secondBuffer = null;
            displayError("Algorithm ColocalizationRegression reports: out of memory" + e);
            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Generating thresholded linear correlations");
        fireProgressStateChanged(70);

        // secondBuffer[i] = a*buffer[i] + b;
        // buffer[i] = (secondBuffer[i] - b)/a;
        // Calculate (lineMin1,lineMin2) and (lineMax1,lineMax2),
        // the endpoints of the line segment
        lineMin1 = min1;
        lineMin2 = (a * min1) + b;

        if (lineMin2 < min2) {
            lineMin2 = min2;
            lineMin1 = (min2 - b) / a;
        }

        lineMax1 = max1;
        lineMax2 = (a * max1) + b;

        if (lineMax2 > max2) {
            lineMax2 = max2;
            lineMax1 = (max2 - b) / a;
        }

        firstNonPositive1 = -1;
        firstNonPositive2 = -1;
        minPositiveThreshold = -1;
        nonPositiveThreshold = 1;
        lastPositive1 = (float) (Math.floor(lineMax1) + 1);
        lastPositive2 = (float) (Math.floor(lineMax2) + 1);
        firstUndefined1 = -1;
        firstUndefined2 = -1;

        // Calculate the linear correlation coefficients for all pixels
        // whose values are either below threshold in buffer or are below
        // a*threshold + b in secondBuffer.  The values in buffer range from
        // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
        // along the color that has the greatest range along the line
        // segment.
        if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2)) {
            thresholdOn1 = true;
            rThreshold = null;
            rThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(lineMin1)) + 1];
            colocSize = null;
            colocSize = new float[rThreshold.length];
            colocIntensity1 = null;
            colocIntensity1 = new float[rThreshold.length];
            colocIntensity2 = null;
            colocIntensity2 = new float[rThreshold.length];
            haveThreshold = new boolean[rThreshold.length];

            for (i = 0; i < rThreshold.length; i++) {
                rThreshold[i] = Float.NaN;
                colocSize[i] = 0.0f;
                colocIntensity1[i] = 0.0f;
                colocIntensity2[i] = 0.0f;
                haveThreshold[i] = false;
            }

            transitionFound = false;
            i = rThreshold.length / 2;
            thresholdIncrement = rThreshold.length / 4;

            while (!transitionFound) {
                threshold = (float) (i + Math.ceil(lineMin1));
                secondThreshold = (a * threshold) + b;
                averagex = 0.0;
                averagey = 0.0;
                count = 0;
                morex = false;
                morey = false;
                lastx = buffer[0];
                lasty = secondBuffer[0];
                voiIntensity1Thr = 0.0f;
                voiIntensity2Thr = 0.0f;

                for (j = 0; j < thrLength; j++) {
                    float buff = buffer[j];
                    float secBuff = secondBuffer[j];

                    if (buff >= threshold) {
                        voiIntensity1Thr += buff;
                    }

                    if (secBuff >= secondThreshold) {
                        voiIntensity2Thr += secBuff;
                    }

                    if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                        colocSize[i] += 1.0f;
                        colocIntensity1[i] += buff;
                        colocIntensity2[i] += secBuff;
                    } else {
                        averagex += buff;
                        averagey += secBuff;
                        count++;

                        if (count == 1) {
                            lastx = buff;
                            lasty = secBuff;
                        } else if (count >= 2) {

                            if (!morex) {

                                if (buff != lastx) {
                                    morex = true;
                                }
                            }

                            if (!morey) {

                                if (secBuff != lasty) {
                                    morey = true;
                                }
                            }
                        } // else if (count >= 2)
                    }
                } // for (j = 0; j < thrLength; j++)

                colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                if (doColocWithThresholds) {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                } else {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                }

                if (morex && morey) {
                    averagex /= count;
                    averagey /= count;
                    num = 0.0;
                    denom1 = 0.0;
                    denom2 = 0.0;

                    for (j = 0; j < thrLength; j++) {
                        float buff = buffer[j];
                        float secBuff = secondBuffer[j];

                        if ((buff < threshold) || (secBuff < secondThreshold)) {
                            num += (buff - averagex) * (secBuff - averagey);
                            denom1 += (buff - averagex) * (buff - averagex);
                            denom2 += (secBuff - averagey) * (secBuff - averagey);
                        }
                    } // for (j = 0; j < thrLength; j++)

                    rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                    haveThreshold[i] = true;
                    Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", Preferences.DEBUG_ALGORITHM);

                    if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i - 1];
                        firstNonPositive1 = (float) ((i - 1) + Math.ceil(lineMin1));
                        firstNonPositive2 = (a * firstNonPositive1) + b;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                   Float.isNaN(rThreshold[i - 1])) {
                        transitionFound = true;
                        firstUndefined1 = (float) ((i - 1) + Math.ceil(lineMin1));
                        firstUndefined2 = (a * firstUndefined1) + b;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                        transitionFound = true;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i];
                        firstNonPositive1 = (float) (i + Math.ceil(lineMin1));
                        firstNonPositive2 = (a * firstNonPositive1) + b;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && Float.isNaN(rThreshold[i]) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        firstUndefined1 = (float) (i + Math.ceil(lineMin1));
                        firstUndefined2 = (a * firstUndefined1) + b;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if (rThreshold[i] > 0.0f) {
                        i = i - thresholdIncrement;

                        if (i < 0) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // else if (rThreshold[i] > 0.0f)
                    else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    }
                } // if (morex && morey)
                else { // Float.isNaN(rThreshold[i])
                    haveThreshold[i] = true;
                    i = i + thresholdIncrement;

                    if (i >= rThreshold.length) {
                        transitionFound = true;
                    }

                    thresholdIncrement = thresholdIncrement / 2;

                    if (thresholdIncrement == 0) {
                        thresholdIncrement = 1;
                    }
                } // Float.isNaN(rThreshold[i])
            } // while (!transitionFound)

        } // if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2))
        else { // ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)
            thresholdOn1 = false;
            rThreshold = null;
            rThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(lineMin2)) + 1];
            colocSize = null;
            colocSize = new float[rThreshold.length];
            colocIntensity1 = null;
            colocIntensity1 = new float[rThreshold.length];
            colocIntensity2 = null;
            colocIntensity2 = new float[rThreshold.length];
            haveThreshold = new boolean[rThreshold.length];

            for (i = 0; i < rThreshold.length; i++) {
                rThreshold[i] = Float.NaN;
                colocSize[i] = 0.0f;
                colocIntensity1[i] = 0.0f;
                colocIntensity2[i] = 0.0f;
                haveThreshold[i] = false;
            }

            transitionFound = false;
            i = rThreshold.length / 2;
            thresholdIncrement = rThreshold.length / 4;

            while (!transitionFound) {
                secondThreshold = (float) (i + Math.ceil(lineMin2));
                threshold = (secondThreshold - b) / a;
                averagex = 0.0;
                averagey = 0.0;
                count = 0;
                morex = false;
                morey = false;
                lastx = buffer[0];
                lasty = secondBuffer[0];
                voiIntensity1Thr = 0.0f;
                voiIntensity2Thr = 0.0f;

                for (j = 0; j < thrLength; j++) {
                    float buff = buffer[j];
                    float secBuff = secondBuffer[j];

                    if (buff >= threshold) {
                        voiIntensity1Thr += buff;
                    }

                    if (secBuff >= secondThreshold) {
                        voiIntensity2Thr += secBuff;
                    }

                    if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                        colocSize[i] += 1.0f;
                        colocIntensity1[i] += buff;
                        colocIntensity2[i] += secBuff;
                    } else {
                        averagex += buff;
                        averagey += secBuff;
                        count++;

                        if (count == 1) {
                            lastx = buff;
                            lasty = secBuff;
                        } else if (count >= 2) {

                            if (!morex) {

                                if (buff != lastx) {
                                    morex = true;
                                }
                            }

                            if (!morey) {

                                if (secBuff != lasty) {
                                    morey = true;
                                }
                            }
                        } // else if (count >= 2)
                    }
                } // for (j = 0; j < thrLength; j++)

                colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                if (doColocWithThresholds) {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                } else {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                }

                if (morex && morey) {
                    averagex /= count;
                    averagey /= count;
                    num = 0.0;
                    denom1 = 0.0;
                    denom2 = 0.0;

                    for (j = 0; j < thrLength; j++) {
                        float buff = buffer[j];
                        float secBuff = secondBuffer[j];

                        if ((buff < threshold) || (secBuff < secondThreshold)) {
                            num += (buff - averagex) * (secBuff - averagey);
                            denom1 += (buff - averagex) * (buff - averagex);
                            denom2 += (secBuff - averagey) * (secBuff - averagey);
                        }
                    } // for (j = 0; j < thrLength; j++)

                    rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                    haveThreshold[i] = true;
                    Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", Preferences.DEBUG_ALGORITHM);

                    if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i - 1];
                        firstNonPositive2 = (float) ((i - 1) + Math.ceil(lineMin2));
                        firstNonPositive1 = (firstNonPositive2 - b) / a;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                   Float.isNaN(rThreshold[i - 1])) {
                        transitionFound = true;
                        firstUndefined2 = (float) ((i - 1) + Math.ceil(lineMin2));
                        firstUndefined1 = (firstUndefined2 - b) / a;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                        transitionFound = true;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i];
                        firstNonPositive2 = (float) (i + Math.ceil(lineMin2));
                        firstNonPositive1 = (firstNonPositive2 - b) / a;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && Float.isNaN(rThreshold[i]) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        firstUndefined2 = (float) (i + Math.ceil(lineMin2));
                        firstUndefined1 = (firstUndefined2 - b) / a;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if (rThreshold[i] > 0.0f) {
                        i = i - thresholdIncrement;

                        if (i < 0) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // else if (rThreshold[i] > 0.0f)
                    else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    }
                } // if (morex && morey)
                else { // Float.isNaN(rThreshold[i])
                    haveThreshold[i] = true;
                    i = i + thresholdIncrement;

                    if (i >= rThreshold.length) {
                        transitionFound = true;
                    }

                    thresholdIncrement = thresholdIncrement / 2;

                    if (thresholdIncrement == 0) {
                        thresholdIncrement = 1;
                    }
                } // Float.isNaN(rThreshold[i])
            } // while (!transitionFound)

        } // else ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)

        if (firstUndefined1 >= 0) {
            Preferences.debug("Cannot calculate linear correlation coefficient \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstUndefined1 + " or " +
                              baseImage.getImageName() + " < " + firstUndefined2 + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (nonPositiveThreshold <= 0) {
            Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstNonPositive1 + " or " +
                              baseImage.getImageName() + " < " + firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
        }

        Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + lastPositive1 + " or " +
                          baseImage.getImageName() + " < " + lastPositive2 + "\n", Preferences.DEBUG_ALGORITHM);

        if (minPositiveThreshold < 0) {
            removeVOIUpdateListener();
            MipavUtil.displayError("No positve threshold found");

            setCompleted(false);

            return;
        }

        if (doSecondIteration) {

            Preferences.debug("Second iteration excluding subthresholded region\n", Preferences.DEBUG_ALGORITHM);
            fireProgressStateChanged("Second iteration excluding subthresholded region");
            fireProgressStateChanged(80);
            t1 = lastPositive1;
            t2 = lastPositive2;

            averagex = 0.0;
            averagey = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    averagex += buffer[i];
                    averagey += secondBuffer[i];
                    count++;
                }
            }

            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    num += (buffer[i] - averagex) * (secondBuffer[i] - averagey);
                    denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                    denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                }
            }

            denom = Math.sqrt(denom1 * denom2);
            r = (float) (num / denom);
            Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);

            // Use linear least squares to calculate the best line fit
            // for secondBuffer = a*firstBuffer + b.  Use an orthogonal
            // line fit where the distance measurements are orthgonal to
            // the proposed line.  This is an orthogonal regression as opposed
            // to a traditional regression of dependent y variable green on
            // independent red variable x which would give measurements in the
            // y direction.  Note that a traditional regression of red on green
            // would be yet another line giving measurements in the x direction.
            // Don't include any point i with buffer[i] < lastPositive1 or
            // secondBuffer[i] < lastPositive2 in the regression line.
            // This excludes the L shaped subthresholded region from the
            // first iteration.
            sumx = 0.0;
            sumy = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    sumx += buffer[i];
                    sumy += secondBuffer[i];
                    count++;
                }
            }

            averagex = sumx / count;
            averagey = sumy / count;
            sumx2 = 0.0;
            sumxy = 0.0;
            sumy2 = 0.0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    diffx = buffer[i] - averagex;
                    diffy = secondBuffer[i] - averagey;
                    sumx2 += diffx * diffx;
                    sumxy += diffx * diffy;
                    sumy2 += diffy * diffy;
                }
            }

            /*kES = new AlgorithmEigensolver(2);
             * kES.setMatrix(0,0,sumy2); kES.setMatrix(0,1,-sumxy); kES.setMatrix(1,0,-sumxy); kES.setMatrix(1,1,sumx2);
             * kES.solve(); // Eigenvectors are stored in increasing order // Get the eigenvector for the smallest value
             * // The eigenvector of the ith eigenvalue is stored in // the ith column directionx =
             * kES.getEigenvector(0,0); directiony = kES.getEigenvector(1,0); delta = directiony/directionx; a =
             * (float)(delta);*/
            varx = sumx2 / (count - 1);
            vary = sumy2 / (count - 1);
            covarxy = sumxy / (count - 1);
            a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                             (2 * covarxy));
            b = (float) (averagey - (a * averagex));
            mse = (float) (varx + (2 * a * covarxy) + (a * a * vary)) / (1 + (a * a));

            // +-2 standard deviations on each side includes about 95%.
            halfWidth = (float) (2.0 * Math.sqrt(mse));

            Preferences.debug(baseImage.getImageName() + " = " + a + "*" + srcImage.getImageName() + " + " + b + "\n", 
            		Preferences.DEBUG_ALGORITHM);

            Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);
            doAgain = false;

            for (iter = 0; (iter < 10) && doAgain; iter++) {
                doAgain = false;
                sumx = 0.0;
                sumy = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {

                    if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                        distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) / ((a * a) + 1);

                        if (distance <= halfWidth) {
                            sumx += buffer[i];
                            sumy += secondBuffer[i];
                            count++;
                        }
                    }
                }

                averagex = sumx / count;
                averagey = sumy / count;
                sumx2 = 0.0;
                sumxy = 0.0;
                sumy2 = 0.0;

                for (i = 0; i < thrLength; i++) {

                    if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                        distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) / ((a * a) + 1);

                        if (distance <= halfWidth) {
                            diffx = buffer[i] - averagex;
                            diffy = secondBuffer[i] - averagey;
                            sumx2 += diffx * diffx;
                            sumxy += diffx * diffy;
                            sumy2 += diffy * diffy;
                        }
                    }
                }

                varx = sumx2 / (count - 1);
                vary = sumy2 / (count - 1);
                covarxy = sumxy / (count - 1);
                aLast = a;
                bLast = b;
                a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                 (2 * covarxy));
                b = (float) (averagey - (a * averagex));
                mse = (float) (varx + (2 * a * covarxy) + (a * a * vary)) / (1 + (a * a));

                // +-2 standard deviations on each side includes about 95%.
                halfWidth = (float) (2.0 * Math.sqrt(mse));

                Preferences.debug("Iteration = " + (iter + 2) + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug(baseImage.getImageName() + " = " + a + "*" + srcImage.getImageName() + " + " + b +
                                  "\n", Preferences.DEBUG_ALGORITHM);

                Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

                if ((aLast != 0.0f) && ((Math.abs(a - aLast) / aLast) > 0.001)) {
                    doAgain = true;
                }

                if ((bLast != 0.0f) && ((Math.abs(b - bLast) / bLast) > 0.001)) {
                    doAgain = true;
                }
            } // for (iter = 0; (iter < 10) && doAgain; iter++)

            // secondBuffer[i] = a*buffer[i] + b
            // ch1 = (buffer[i] - min1)*scale1 + leftPad
            // ch2 = (secondBuffer[i] - min2)*scale2 + topPad;
            // ch1 = (scale1/a)*[((ch2-topPad)/scale2) + min2 - a*min1 - b] +
            // leftPad
            // ch2 = scale2*[((a*(ch1-leftPad))/scale1) + a*min1 - min2 + b] +
            // topPad
            ch2f = topPad;
            ch1f = (float) ((scale1 / a) * (min2 - (a * min1) - b)) + leftPad;

            if (ch1f < leftPad) {
                ch1f = leftPad;
                ch2f = (float) (scale2 * ((a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[0] = ch1f;
            yArray[0] = ch2f;
            ch2f = topPad + bin2 - 1;
            ch1f = (float) ((scale1 / a) * (((ch2f - topPad) / scale2) + min2 - (a * min1) - b)) + leftPad;

            if (ch1f > (bin1 - 1 + leftPad)) {
                ch1f = bin1 - 1 + leftPad;
                ch2f = (float) (scale2 * (((a * (bin1 - 1)) / scale1) + (a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[1] = ch1f;
            yArray[1] = ch2f;
            fitLineVOI = new VOI((short) 0, "fitLine", VOI.LINE, -1.0f);
            fitLineVOI.importCurve(xArray, yArray, zArray);
            destImage.registerVOI(fitLineVOI);
            fitLineVOI.setFixed(true);
            fitLineVOI.setColor(Color.orange);

            // secondBuffer[i] = a*buffer[i] + b;
            // buffer[i] = (secondBuffer[i] - b)/a;
            // Calculate (lineMin1,lineMin2) and (lineMax1,lineMax2),
            // the endpoints of the line segment
            lineMin1 = min1;
            lineMin2 = (a * min1) + b;

            if (lineMin2 < min2) {
                lineMin2 = min2;
                lineMin1 = (min2 - b) / a;
            }

            lineMax1 = max1;
            lineMax2 = (a * max1) + b;

            if (lineMax2 > max2) {
                lineMax2 = max2;
                lineMax1 = (max2 - b) / a;
            }

            firstNonPositive1 = -1;
            firstNonPositive2 = -1;
            minPositiveThreshold = -1;
            nonPositiveThreshold = 1;
            lastPositive1 = (float) (Math.floor(lineMax1) + 1);
            lastPositive2 = (float) (Math.floor(lineMax2) + 1);
            firstUndefined1 = -1;
            firstUndefined2 = -1;

            // Calculate the linear correlation coefficients for all pixels
            // whose values are either below threshold in buffer or are below
            // a*threshold + b in secondBuffer.  The values in buffer range from
            // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
            // along the color that has the greatest range along the line
            // segment.
            if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2)) {
                thresholdOn1 = true;
                rThreshold = null;
                rThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(lineMin1)) + 1];
                colocSize = null;
                colocSize = new float[rThreshold.length];
                colocIntensity1 = null;
                colocIntensity1 = new float[rThreshold.length];
                colocIntensity2 = null;
                colocIntensity2 = new float[rThreshold.length];
                haveThreshold = new boolean[rThreshold.length];

                for (i = 0; i < rThreshold.length; i++) {
                    rThreshold[i] = Float.NaN;
                    colocSize[i] = 0.0f;
                    colocIntensity1[i] = 0.0f;
                    colocIntensity2[i] = 0.0f;
                    haveThreshold[i] = false;
                }

                transitionFound = false;
                i = rThreshold.length / 2;
                thresholdIncrement = rThreshold.length / 4;

                while (!transitionFound) {
                    threshold = (float) (i + Math.ceil(lineMin1));
                    secondThreshold = (a * threshold) + b;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = buffer[0];
                    lasty = secondBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {

                        if (buffer[j] >= threshold) {
                            voiIntensity1Thr += buffer[j];
                        }

                        if (secondBuffer[j] >= secondThreshold) {
                            voiIntensity2Thr += secondBuffer[j];
                        }

                        if ((buffer[j] >= threshold) && (secondBuffer[j] >= secondThreshold)) {
                            colocSize[i] += 1.0f;
                            colocIntensity1[i] += buffer[j];
                            colocIntensity2[i] += secondBuffer[j];
                        } else {
                            averagex += buffer[j];
                            averagey += secondBuffer[j];
                            count++;

                            if (count == 1) {
                                lastx = buffer[j];
                                lasty = secondBuffer[j];
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buffer[j] != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secondBuffer[j] != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                    if (doColocWithThresholds) {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {

                            if ((buffer[j] < threshold) || (secondBuffer[j] < secondThreshold)) {
                                num += (buffer[j] - averagex) * (secondBuffer[j] - averagey);
                                denom1 += (buffer[j] - averagex) * (buffer[j] - averagex);
                                denom2 += (secondBuffer[j] - averagey) * (secondBuffer[j] - averagey);
                            }
                        }

                        rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        haveThreshold[i] = true;
                        Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i - 1];
                            firstNonPositive1 = (float) ((i - 1) + Math.ceil(lineMin1));
                            firstNonPositive2 = (a * firstNonPositive1) + b;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                       Float.isNaN(rThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined1 = (float) ((i - 1) + Math.ceil(lineMin1));
                            firstUndefined2 = (a * firstUndefined1) + b;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                       (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i];
                            firstNonPositive1 = (float) (i + Math.ceil(lineMin1));
                            firstNonPositive2 = (a * firstNonPositive1) + b;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] &&
                                       Float.isNaN(rThreshold[i]) && (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined1 = (float) (i + Math.ceil(lineMin1));
                            firstUndefined2 = (a * firstUndefined1) + b;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if (rThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (rThreshold[i] > 0.0f)
                        else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= rThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(rThreshold[i])
                        haveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(rThreshold[i])
                } // while (!transitionFound)

            } // if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2))
            else { // ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)
                thresholdOn1 = false;
                rThreshold = null;
                rThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(lineMin2)) + 1];
                colocSize = null;
                colocSize = new float[rThreshold.length];
                colocIntensity1 = null;
                colocIntensity1 = new float[rThreshold.length];
                colocIntensity2 = null;
                colocIntensity2 = new float[rThreshold.length];

                for (i = 0; i < rThreshold.length; i++) {
                    rThreshold[i] = Float.NaN;
                    colocSize[i] = 0.0f;
                    colocIntensity1[i] = 0.0f;
                    colocIntensity2[i] = 0.0f;
                    haveThreshold[i] = false;
                }

                transitionFound = false;
                i = rThreshold.length / 2;
                thresholdIncrement = rThreshold.length / 4;

                while (!transitionFound) {
                    secondThreshold = (float) (i + Math.ceil(lineMin2));
                    threshold = (secondThreshold - b) / a;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = buffer[0];
                    lasty = secondBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {

                        if (buffer[j] >= threshold) {
                            voiIntensity1Thr += buffer[j];
                        }

                        if (secondBuffer[j] >= secondThreshold) {
                            voiIntensity2Thr += secondBuffer[j];
                        }

                        if ((buffer[j] >= threshold) && (secondBuffer[j] >= secondThreshold)) {
                            colocSize[i] += 1.0f;
                            colocIntensity1[i] += buffer[j];
                            colocIntensity2[i] += secondBuffer[j];
                        } else {
                            averagex += buffer[j];
                            averagey += secondBuffer[j];
                            count++;

                            if (count == 1) {
                                lastx = buffer[j];
                                lasty = secondBuffer[j];
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buffer[j] != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secondBuffer[j] != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                    if (doColocWithThresholds) {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {

                            if ((buffer[j] < threshold) || (secondBuffer[j] < secondThreshold)) {
                                num += (buffer[j] - averagex) * (secondBuffer[j] - averagey);
                                denom1 += (buffer[j] - averagex) * (buffer[j] - averagex);
                                denom2 += (secondBuffer[j] - averagey) * (secondBuffer[j] - averagey);
                            }
                        }

                        rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        haveThreshold[i] = true;
                        Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i - 1];
                            firstNonPositive2 = (float) ((i - 1) + Math.ceil(lineMin2));
                            firstNonPositive1 = (firstNonPositive2 - b) / a;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                       Float.isNaN(rThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined2 = (float) ((i - 1) + Math.ceil(lineMin2));
                            firstUndefined1 = (firstUndefined2 - b) / a;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                       (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i];
                            firstNonPositive2 = (float) (i + Math.ceil(lineMin2));
                            firstNonPositive1 = (firstNonPositive2 - b) / a;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] &&
                                       Float.isNaN(rThreshold[i]) && (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined2 = (float) (i + Math.ceil(lineMin2));
                            firstUndefined1 = (firstUndefined2 - b) / a;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if (rThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (rThreshold[i] > 0.0f)
                        else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= rThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(rThreshold[i])
                        haveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(rThreshold[i])
                } // while (!transitionFound)

            } // else ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)

            if (firstUndefined1 >= 0) {
                Preferences.debug("Cannot calculate linear correlation coefficient \n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstUndefined1 + " or " +
                                  baseImage.getImageName() + " < " + firstUndefined2 + "\n", Preferences.DEBUG_ALGORITHM);
            }

            if (nonPositiveThreshold <= 0) {
                Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstNonPositive1 + " or " +
                                  baseImage.getImageName() + " < " + firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + lastPositive1 + " or " +
                              baseImage.getImageName() + " < " + lastPositive2 + "\n", Preferences.DEBUG_ALGORITHM);

            if (minPositiveThreshold < 0) {
                removeVOIUpdateListener();
                MipavUtil.displayError("No positve threshold found on second iteration");

                setCompleted(false);

                return;
            }
        } // if (doSecondIteration)

        frameList = srcImage.getImageFrameVector();

        for (i = 0; i < frameList.size(); i++) {

            if ((((ViewJFrameBase) frameList.elementAt(i)).getControls()) != null) {
                controlFrame = ((ViewJFrameBase) frameList.elementAt(i));
            }
        }

        fireProgressStateChanged("Creating point VOI");
        fireProgressStateChanged(85);

        // position the point at the minimum positive threshold
        xArray = new float[1];
        yArray = new float[1];
        zArray = new float[1];

        // Allow blank spaces
        if (pointCalculation) {
            xArray[0] = (float) ((point1 - min1) * scale1) + leftPad;
            yArray[0] = (float) (bin2 - 1.0f - ((point2 - min2) * scale2)) + topPad;
        } else {
            xArray[0] = (float) ((lastPositive1 - min1) * scale1) + leftPad;
            yArray[0] = (float) (bin2 - 1.0f - ((lastPositive2 - min2) * scale2)) + topPad;
        }

        zArray[0] = 0.0f;
        pointVOI = new VOI((short) 1, "thresholdPoint", VOI.POINT, -1.0f);
        pointVOI.importCurve(xArray, yArray, zArray);
        destImage.registerVOI(pointVOI);
        pointVOI.setFixed(false);

        // Don't draw point - draw square with blue inside and yellow edge
        pointVOI.setColor(Color.green);

        /*UI.setDataText("Linear correlation coefficient = " + r + "\n");
         * UI.setDataText(baseImage.getImageName() + " = " + a + "*" + srcImage.getImageName()+ " + " + b + "\n");
         * UI.setDataText("Mean square error = " + mse + "\n"); if (firstUndefined1 >= 0) { UI.setDataText("Cannot
         * calculate linear correlation coefficient \n"); UI.setDataText("for pixels with " + srcImage.getImageName() +
         * " < " + firstUndefined1 + " or " + baseImage.getImageName() + " < " + firstUndefined2 + "\n"); } if
         * (nonPositiveThreshold <= 0) { UI.setDataText("Nonpositive linear correlation coefficient = " +
         * nonPositiveThreshold + "\n"); UI.setDataText("for pixels with " + srcImage.getImageName() + " < " +
         * firstNonPositive1 + " or " + baseImage.getImageName() + " < " + firstNonPositive2 + "\n"); }
         * UI.setDataText("Linear correlation coefficient = " + minPositiveThreshold + "\n"); UI.setDataText("for pixels
         * with " + srcImage.getImageName() + " < " + lastPositive1 + " or " + baseImage.getImageName() + " < " +
         * lastPositive2 + "\n");*/

        fireProgressStateChanged("Creating ColocalizationRegression frame");
        fireProgressStateChanged(90);

        if (redo) {
            haveFreeRangeThreshold = null;
            freeRangeRThreshold = null;
            freeRangeColocSize = null;
            freeRangeColocIntensity1 = null;
            freeRangeColocIntensity2 = null;
            frameColocalize.setNewVar(a, b, r, PValue, haveThreshold, rThreshold, colocSize, colocIntensity1,
                                      colocIntensity2, min1, max1, min2, max2, scale1, scale2, lineMin1, lineMax1,
                                      lineMin2, lineMax2, thresholdOn1);
        } else {

            // System.err.println("Constr1");
            frameColocalize = new ViewJFrameColocalizationRegression(this, subtractedSrcImage, null,
                                                                     subtractedBaseImage, null, null, destImage,
                                                                     controlFrame, useRed, useGreen, useBlue, a, b, r,
                                                                     PValue, haveThreshold, rThreshold, colocSize,
                                                                     colocIntensity1, colocIntensity2, min1, max1, min2,
                                                                     max2, scale1, scale2, lineMin1, lineMax1, lineMin2,
                                                                     lineMax2, thresholdOn1, leftPad, rightPad,
                                                                     bottomPad, topPad, doSecondIteration,
                                                                     pointCalculation);

            if (pointCalculation) {
                frameColocalize.pointCalculate();
            }
        }

        fireProgressStateChanged(100);


        setCompleted(true);
    }

    /**
     * This function produces a 2D histogram image with srcImage values represented across the x axis and baseImage
     * values represented across the y axis. The total least squares line and a matrix of correlation coefficients for L
     * shaped subthreshold regions are also generated.
     */
    private void calc3D() {
        int i;
        int pos;
        float[] randomBuffer;
        double[] histBuffer;
        int[] extents2D;
        ModelImage refImage;
        ModelImage inputImage;
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
        boolean transformVOI = false;
        boolean clip = true;
        TransMatrix xfrm;
        double min1, min2, max1, max2, range1, range2, scale1, scale2;
        double averagex, averagey;
        double diffx, diffy;
        double varx, vary, covarxy;
        float mse; // mean square error
        float halfWidth;
        float distance;
        int iter;
        boolean doAgain;
        float aLast;
        float bLast;
        boolean morex, morey;
        float lastx, lasty;

        // AlgorithmEigensolver kES;
        // double directionx;
        // double directiony;
        int ch1, ch2;
        float ch1f, ch2f;
        double lineMax1, lineMax2;
        float[] xArray;
        float[] yArray;
        float[] zArray;
        VOI fitLineVOI;
        VOI pointVOI;
        double num, denom1, denom2, denom;
        String name;
        ModelImage resultImage = null;
        AlgorithmAutoCovariance algoAutoCovariance;
        int fwhmS = Integer.MAX_VALUE;
        int fwhmB = Integer.MAX_VALUE;
        int fwhm = Integer.MAX_VALUE;
        boolean scrambleFirst;
        int xDim, yDim, zDim;
        int length;
        int volume;
        int zPos;
        int totPos;
        int x, y, z;
        int numCubes;
        Vector<Integer> vectRand;
        RandomNumberGen randomGen;
        int randNum;
        Integer randCube;
        int sliceCubes;
        int s, j, k, m;
        int sx, sy, sz;
        int numXValues;
        int numYValues;
        int numZValues;
        float[] rMat = new float[200];
        int numBins;
        float[] rCum;
        float[] pStatMat;
        boolean found;
        int xRemainder = 0;
        int yRemainder = 0;
        int zRemainder = 0;
        int xStart = 0;
        int yStart = 0;
        int zStart = 0;
        int xEnd;
        int yEnd;
        int zEnd;
        double sumx;
        double sumx2;
        double sumy;
        double sumxy;
        double sumy2;
        float threshold;
        float secondThreshold;
        int count;
        float nonPositiveThreshold;
        float minPositiveThreshold;
        float lastPositive1, lastPositive2;
        float firstNonPositive1, firstNonPositive2;
        float firstUndefined1, firstUndefined2;
        Vector<ViewImageUpdateInterface> frameList;
        ViewJFrameBase controlFrame = null;
        float[] destRes;
        boolean transitionFound;
        int thresholdIncrement;
        float[] tmpBuffer;
        float[] tmpSecondBuffer;
        float voiIntensity1Thr;
        float voiIntensity2Thr;
        float backC1 = 0.0f;
        float backC2 = 0.0f;

        try {

            fireProgressStateChanged(srcImage.getImageName(), "Creating 2D Colocolization Histogram ...");

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

            if ((backgroundIndex != -1) && (!redo)) {
                fireProgressStateChanged("Calculating backround average");
                fireProgressStateChanged(2);

                for (i = 0; i < volume; i++) {

                    if (subtractionMask[i] == backgroundIndex) {
                        backC1 += srcImage.getFloat(i);
                    }
                }

                backC1 /= volume;
            } // if ((backgroundIndex != -1) && (!redo))

            if ((backgroundIndex2 != -1) && (!redo)) {
                fireProgressStateChanged("Calculating backround average");
                fireProgressStateChanged(2);

                for (i = 0; i < volume; i++) {

                    if (subtractionMask2[i] == backgroundIndex2) {
                        backC2 += baseImage.getFloat(i);
                    }
                }

                backC2 /= volume;
            } // if ((backgroundIndex2 != -1) && (!redo))

            if ((register) && (!redo)) {
                fireProgressStateChanged("Registering images");
                fireProgressStateChanged(7);

                // Register each image half of the way to the same center point
                extents2D = new int[2];
                extents2D[0] = srcImage.getExtents()[0];
                extents2D[1] = srcImage.getExtents()[1];

                float xresA = baseImage.getFileInfo(0).getResolutions()[0];
                float yresA = baseImage.getFileInfo(0).getResolutions()[1];
                float xresB = srcImage.getFileInfo(0).getResolutions()[0];
                float yresB = srcImage.getFileInfo(0).getResolutions()[1];

                refImage = new ModelImage(srcImage.getType(), extents2D, "refImage");
                inputImage = new ModelImage(baseImage.getType(), extents2D, "inputImage");
                registeredSrcImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), "regSImage");
                registeredBaseImage = new ModelImage(baseImage.getType(), baseImage.getExtents(), "regBImage");

                for (z = 0; z < zDim; z++) {
                    fireProgressStateChanged("Registering slice = " + (z + 1));

                    zPos = z * length;

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

                    reg2 = new AlgorithmRegOAR2D(refImage, inputImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                                 coarseRate, fineRate, doSubsample, doMultiThread);
                    reg2.run();
                    xfrm = reg2.getTransform();
                    reg2.disposeLocal();
                    reg2 = null;
                    
                    xfrm.Set(0, 1, xfrm.Get(0, 1) * 0.5f);
                    xfrm.Set(0, 2, xfrm.Get(0, 2) * 0.5f);
                    xfrm.Set(1, 0, xfrm.Get(1, 0) * 0.5f);
                    xfrm.Set(1, 2, xfrm.Get(1, 2) * 0.5f);
                    
                    transform = new AlgorithmTransform(inputImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA,
                                                       xDim, yDim, transformVOI, clip, false);
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

                    reg2 = new AlgorithmRegOAR2D(inputImage, refImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                                 coarseRate, fineRate, doSubsample, doMultiThread);
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

                } // for (z = 0; z < zDim; z++)

                updateFileInfo(srcImage, registeredSrcImage);
                registeredSrcImage.calcMinMax();
                registeredSrcImage.setImageName(srcImage.getImageName() + "_register");
                registeredSrcImage.setVOIs(srcImage.getVOIs());
                srcImage.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                srcImage.clearMask();
                srcImage.notifyImageDisplayListeners();
                bin1 = Math.min(bin1, (int) Math.round(registeredSrcImage.getMax() - registeredSrcImage.getMin() + 1));
                updateFileInfo(baseImage, registeredBaseImage);
                registeredBaseImage.calcMinMax();
                registeredBaseImage.setImageName(baseImage.getImageName() + "_register");
                bin2 = Math.min(bin2,
                                (int) Math.round(registeredBaseImage.getMax() - registeredBaseImage.getMin() + 1));

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
            } // if ((register) && (!redo))
            else if (!redo) {
                registeredBaseImage = baseImage;
                registeredSrcImage = srcImage;
            } // else if (!redo)

            if ((backgroundIndex != -1) && (!redo)) {
                fireProgressStateChanged("Subtracting background from " + srcImage.getImageName());
                fireProgressStateChanged(12);
                subtractedSrcImage = new ModelImage(srcImage.getType(), srcImage.getExtents(),
                                                    registeredSrcImage.getImageName() + "_subtract");
                buffer = null;
                buffer = new float[length];

                for (j = 0; j < srcImage.getExtents()[2]; j++) {

                    try {
                        registeredSrcImage.exportData(j * length, length, buffer);
                    } catch (IOException e) {
                        buffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                        return;
                    }

                    for (i = 0; i < length; i++) {
                        buffer[i] -= backC1;

                        if (buffer[i] < 0.0f) {
                            buffer[i] = 0.0f;
                        }
                    }

                    try {
                        subtractedSrcImage.importData(j * length, buffer, true);
                    } catch (IOException e) {
                        buffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", false);

                        return;
                    }
                } // for (j = 0; j < srcImage.getExtents()[2]; j++)

                subtractedSrcImage.calcMinMax();
                updateFileInfo(registeredSrcImage, subtractedSrcImage);

                try {
                    imageFrame3 = new ViewJFrameImage(subtractedSrcImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Unable to open frame for subtracted image");
                    setCompleted(false);

                    return;
                }

                srcImage.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                srcImage.clearMask();
                srcImage.notifyImageDisplayListeners();

                bin1 = Math.min(bin1, (int) Math.round(subtractedSrcImage.getMax() - subtractedSrcImage.getMin() + 1));

                int[] newExtents = new int[2];
                newExtents[0] = bin1 + leftPad + rightPad;
                newExtents[1] = bin2 + bottomPad + topPad;
                destImage.changeExtents(newExtents);
            } // if (backgroundIndex != -1) && (!redo))
            else if (!redo) {
                subtractedSrcImage = registeredSrcImage;
            } // else if (!redo)

            if ((backgroundIndex2 != -1) && (!redo)) {
                fireProgressStateChanged("Subtracting background from " + baseImage.getImageName());
                fireProgressStateChanged(15);
                subtractedBaseImage = new ModelImage(baseImage.getType(), baseImage.getExtents(),
                                                     registeredBaseImage.getImageName() + "_subtract");
                buffer = null;
                buffer = new float[length];

                for (j = 0; j < baseImage.getExtents()[2]; j++) {

                    try {
                        registeredBaseImage.exportData(j * length, length, buffer);
                    } catch (IOException e) {
                        buffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                        return;
                    }

                    for (i = 0; i < length; i++) {
                        buffer[i] -= backC1;

                        if (buffer[i] < 0.0f) {
                            buffer[i] = 0.0f;
                        }
                    }

                    try {
                        subtractedBaseImage.importData(j * length, buffer, false);
                    } catch (IOException e) {
                        buffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", false);

                        return;
                    }
                } // for (j = 0; j < baseImage.getExtents()[2]; j++)

                subtractedBaseImage.calcMinMax();
                updateFileInfo(registeredBaseImage, subtractedBaseImage);

                try {
                    imageFrame4 = new ViewJFrameImage(subtractedBaseImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Unable to open frame for subtracted image");
                    setCompleted(false);

                    return;
                }

                baseImage.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                baseImage.clearMask();
                baseImage.notifyImageDisplayListeners();

                bin2 = Math.min(bin2,
                                (int) Math.round(subtractedBaseImage.getMax() - subtractedBaseImage.getMin() + 1));

                int[] newExtents = new int[2];
                newExtents[0] = bin1 + leftPad + rightPad;
                newExtents[1] = bin2 + bottomPad + topPad;
                destImage.changeExtents(newExtents);
            } // if (backgroundIndex2 != -1) && (!redo))
            else if (!redo) {
                subtractedBaseImage = registeredBaseImage;
            } // else if (!redo)

            if ((!entireImage) && (!maskSupplied) && (!redo)) {
                subtractedSrcImage.getParentFrame().getComponentImage().getVOIHandler().addVOIUpdateListener(this);
            }

            try {
                buffer = null;
                buffer = new float[volume];
                secondBuffer = null;
                secondBuffer = new float[volume];
                histBuffer = null;
                histBuffer = new double[(bin1 + leftPad + rightPad) * (bin2 + bottomPad + topPad)];
            } catch (OutOfMemoryError oome) {
                buffer = null;
                secondBuffer = null;
                histBuffer = null;
                errorCleanUp("Algorithm ColocalizationRegression: Out of memory", true);

                return;
            }

            for (i = 0; i < histBuffer.length; i++) {
                histBuffer[i] = 0.0;
            }

            destImage.releaseLock(); // we need to be able to alter the dest image

            m = 0;

            for (k = 0; k <= (zDim - 1); k++) {

                for (j = 0; j <= (yDim - 1); j++) {

                    for (i = 0; i <= (xDim - 1); i++) {
                        pos = i + (xDim * j) + (length * k);
                        buffer[m] = subtractedSrcImage.getFloat(pos);
                        secondBuffer[m++] = subtractedBaseImage.getFloat(pos);
                    }
                }
            }

            voiSize = 0;
            voiIntensity1 = 0.0f;
            voiIntensity2 = 0.0f;

            for (i = 0; i < volume; i++) {

                if (inputMask.get(i)) {
                    voiSize++;

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        voiIntensity1 += buffer[i];
                        voiIntensity2 += secondBuffer[i];
                    }
                }
            }

            fireProgressStateChanged("Calculating overall linear correlation coefficient");
            fireProgressStateChanged(20);
            min1 = Double.MAX_VALUE;
            max1 = -Double.MAX_VALUE;
            min2 = Double.MAX_VALUE;
            max2 = -Double.MAX_VALUE;

            for (i = 0; i < volume; i++) {

                if (buffer[i] > max1) {
                    max1 = buffer[i];
                }

                if (buffer[i] < min1) {
                    min1 = buffer[i];
                }

                if (secondBuffer[i] > max2) {
                    max2 = secondBuffer[i];
                }

                if (secondBuffer[i] < min2) {
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
            averagex = 0.0;
            averagey = 0.0;
            count = 0;

            for (i = 0; i < volume; i++) {

                if (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) && inputMask.get(i)) {
                    averagex += buffer[i];
                    averagey += secondBuffer[i];
                    count++;
                }
            }

            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (i = 0; i < volume; i++) {

                if (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) && inputMask.get(i)) {
                    num += (buffer[i] - averagex) * (secondBuffer[i] - averagey);
                    denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                    denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                }
            }

            denom = Math.sqrt(denom1 * denom2);
            r = (float) (num / denom);
            Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);

            if (doP) {
                fireProgressStateChanged("Calculating autocovariance");
                fireProgressStateChanged(10);
                name = srcImage.getImageName() + "_autocovarianceS";
                resultImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), name);

                algoAutoCovariance = new AlgorithmAutoCovariance(resultImage, subtractedSrcImage);
                algoAutoCovariance.run();

                fwhmS = algoAutoCovariance.getFWHM();
                fwhm = fwhmS;

                if (fwhmS != Integer.MAX_VALUE) {
                    ViewUserInterface.getReference().setDataText(srcImage.getImageName() +
                                                                 " auto covariance full width at half maximum = " +
                                                                 fwhmS + "\n");
                    Preferences.debug(srcImage.getImageName() + " auto covariance full width at half maximum = " +
                                      fwhmS + "\n", Preferences.DEBUG_ALGORITHM);
                } else {
                    ViewUserInterface.getReference().setDataText("Cannot determine " + srcImage.getImageName() +
                                                                 " auto covariance full width at half maximum\n");
                    Preferences.debug("Cannot determine " + srcImage.getImageName() +
                                      " auto covariance full width at half maximum\n", Preferences.DEBUG_ALGORITHM);
                }

                algoAutoCovariance.finalize();
                algoAutoCovariance = null;

                algoAutoCovariance = new AlgorithmAutoCovariance(resultImage, subtractedBaseImage);
                algoAutoCovariance.run();

                fwhmB = algoAutoCovariance.getFWHM();
                fwhm = Math.min(fwhmS, fwhmB);

                if (fwhmB != Integer.MAX_VALUE) {
                    ViewUserInterface.getReference().setDataText(baseImage.getImageName() +
                                                                 " auto covariance full width at half maximum = " +
                                                                 fwhmB + "\n");
                    Preferences.debug(baseImage.getImageName() + " auto covariance full width at half maximum = " +
                                      fwhmB + "\n", Preferences.DEBUG_ALGORITHM);
                } else {
                    ViewUserInterface.getReference().setDataText("Cannot determine " + baseImage.getImageName() +
                                                                 " auto covariance full width at half maximum\n");
                    Preferences.debug("Cannot determine " + baseImage.getImageName() +
                                      " auto covariance full width at half maximum\n", Preferences.DEBUG_ALGORITHM);
                }

                algoAutoCovariance.finalize();
                algoAutoCovariance = null;

                resultImage.disposeLocal();
                resultImage = null;

                if (fwhm == Integer.MAX_VALUE) {
                    finalize();
                    errorCleanUp("Algorithm ColocalizationRegression could not determine fwhm", true);

                    return;
                } else if (fwhm == fwhmS) {
                    scrambleFirst = true;
                } else {
                    scrambleFirst = false;
                }

                if (fwhm == 0) {
                    fwhm = 1;
                }

                fireProgressStateChanged("Calculating P-value");
                fireProgressStateChanged(15);

                // Calculate denom without inputMask
                averagex = 0.0;
                averagey = 0.0;
                count = 0;

                for (i = 0; i < volume; i++) {

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        averagex += buffer[i];
                        averagey += secondBuffer[i];
                        count++;
                    }
                }

                averagex /= count;
                averagey /= count;
                denom1 = 0.0;
                denom2 = 0.0;

                for (i = 0; i < volume; i++) {

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                        denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                    }
                }

                denom = Math.sqrt(denom1 * denom2);
                randomBuffer = null;
                randomBuffer = new float[volume];
                numXValues = xDim / fwhm;
                xRemainder = xDim % fwhm;
                numYValues = yDim / fwhm;
                yRemainder = yDim % fwhm;
                numZValues = zDim / fwhm;
                zRemainder = zDim % fwhm;
                sliceCubes = numXValues * numYValues;
                numCubes = sliceCubes * numZValues;
                randomGen = new RandomNumberGen();
                vectRand = new Vector<Integer>();

                for (i = 0; i < 200; i++) {

                    for (s = 0; s < numCubes; s++) {
                        vectRand.add(new Integer(s));
                    }

                    for (s = 0; s < numCubes; s++) {
                        randNum = randomGen.genUniformRandomNum(0, vectRand.size() - 1);
                        xStart = randomGen.genUniformRandomNum(0, xRemainder);
                        xEnd = xDim - (xRemainder - xStart);
                        yStart = randomGen.genUniformRandomNum(0, yRemainder);
                        yEnd = yDim - (yRemainder - yStart);
                        zStart = randomGen.genUniformRandomNum(0, zRemainder);
                        zEnd = zDim - (zRemainder - zStart);
                        randCube = vectRand.elementAt(randNum);
                        vectRand.removeElementAt(randNum);
                        z = (fwhm * (randCube.intValue() / sliceCubes)) + zStart;
                        y = (fwhm * ((randCube.intValue() % sliceCubes) / numXValues)) + yStart;
                        x = (fwhm * ((randCube.intValue() % sliceCubes) % numXValues)) + xStart;
                        sz = (fwhm * (s / sliceCubes)) + zStart;
                        sy = (fwhm * ((s % sliceCubes) / numXValues)) + yStart;
                        sx = (fwhm * ((s % sliceCubes) % numXValues)) + xStart;

                        for (m = 0; m < fwhm; m++) {

                            for (k = 0; k < fwhm; k++) {

                                for (j = 0; j < fwhm; j++) {

                                    if (scrambleFirst) {
                                        randomBuffer[sx + j + ((sy + k) * xDim) + ((sz + m) * length)] = buffer[x + j +
                                                                                                                ((y +
                                                                                                                  k) *
                                                                                                                     xDim) +
                                                                                                                ((z +
                                                                                                                  m) *
                                                                                                                     length)];
                                    } else {
                                        randomBuffer[sx + j + ((sy + k) * xDim) + ((sz + m) * length)] = secondBuffer[x +
                                                                                                                      j +
                                                                                                                      ((y +
                                                                                                                        k) *
                                                                                                                           xDim) +
                                                                                                                      ((z +
                                                                                                                        m) *
                                                                                                                           length)];
                                    }
                                } // for (j = 0; j < fwhm; j++)
                            } // for (k = 0; k < fwhm; k++)
                        } // for (m = 0; m < fwhm; m++)

                        for (z = 0; z < zDim; z++) {

                            for (y = 0; y < yDim; y++) {

                                for (x = 0; x < xStart; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + (y * xDim) + (z * length)] = buffer[x + (y * xDim) +
                                                                                             (z * length)];
                                    } else {
                                        randomBuffer[x + (y * xDim) + (z * length)] = secondBuffer[x + (y * xDim) +
                                                                                                   (z * length)];
                                    }
                                }
                            }
                        }

                        for (z = 0; z < zDim; z++) {

                            for (y = 0; y < yDim; y++) {

                                for (x = xEnd; x < xDim; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + (y * xDim) + (z * length)] = buffer[x + (y * xDim) +
                                                                                             (z * length)];
                                    } else {
                                        randomBuffer[x + (y * xDim) + (z * length)] = secondBuffer[x + (y * xDim) +
                                                                                                   (z * length)];
                                    }
                                }
                            }
                        }

                        for (z = 0; z < zDim; z++) {

                            for (y = 0; y < yStart; y++) {

                                for (x = 0; x < xDim; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + (y * xDim) + (z * length)] = buffer[x + (y * xDim) +
                                                                                             (z * length)];
                                    } else {
                                        randomBuffer[x + (y * xDim) + (z * length)] = secondBuffer[x + (y * xDim) +
                                                                                                   (z * length)];
                                    }
                                }
                            }
                        }

                        for (z = 0; z < zDim; z++) {

                            for (y = yEnd; y < yDim; y++) {

                                for (x = 0; x < xDim; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + (y * xDim) + (z * length)] = buffer[x + (y * xDim) +
                                                                                             (z * length)];
                                    } else {
                                        randomBuffer[x + (y * xDim) + (z * length)] = secondBuffer[x + (y * xDim) +
                                                                                                   (z * length)];
                                    }
                                }
                            }
                        }

                        for (z = 0; z < zStart; z++) {

                            for (y = 0; y < yDim; y++) {

                                for (x = 0; x < xDim; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + (y * xDim) + (z * length)] = buffer[x + (y * xDim) +
                                                                                             (z * length)];
                                    } else {
                                        randomBuffer[x + (y * xDim) + (z * length)] = secondBuffer[x + (y * xDim) +
                                                                                                   (z * length)];
                                    }
                                }
                            }
                        }

                        for (z = zEnd; z < zDim; z++) {

                            for (y = 0; y < yDim; y++) {

                                for (x = 0; x < xDim; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + (y * xDim) + (z * length)] = buffer[x + (y * xDim) +
                                                                                             (z * length)];
                                    } else {
                                        randomBuffer[x + (y * xDim) + (z * length)] = secondBuffer[x + (y * xDim) +
                                                                                                   (z * length)];
                                    }
                                }
                            }
                        }
                    } // for (s = 0; s < numCubes; s++)

                    if (scrambleFirst) {
                        num = 0.0;

                        for (j = 0; j < volume; j++) {

                            if ((randomBuffer[j] >= background1) || (secondBuffer[j] >= background2)) {
                                num += (randomBuffer[j] - averagex) * (secondBuffer[j] - averagey);
                            }
                        }

                        rMat[i] = (float) (num / denom);
                    } else {
                        num = 0.0;

                        for (j = 0; j < volume; j++) {

                            if ((buffer[j] >= background1) || (randomBuffer[j] >= background2)) {
                                num += (buffer[j] - averagex) * (randomBuffer[j] - averagey);
                            }
                        }

                        rMat[i] = (float) (num / denom);
                    }
                } // for (i = 0; i < 200; i++)

                Arrays.sort(rMat);
                numBins = 200;

                for (i = 1; i < 200; i++) {

                    if (rMat[i] == rMat[i - 1]) {
                        numBins--;
                    }
                }

                rCum = new float[numBins];
                pStatMat = new float[numBins];
                rCum[0] = rMat[0];
                pStatMat[0] = 0.0f;
                pStatMat[1] = 0.005f;
                j = 1;

                for (i = 1; i < 200; i++) {

                    if (rMat[i] == rMat[i - 1]) {
                        pStatMat[j] += 0.005f;
                    } else {
                        rCum[j++] = rMat[i];

                        if (j < numBins) {
                            pStatMat[j] = pStatMat[j - 1] + 0.005f;
                        }
                    }
                } // for (i = 1; i < 200; i++)

                PValue = 0.995f;
                found = false;

                for (i = 0; (i <= (numBins - 1)) && (!found); i++) {

                    if (r <= rCum[i]) {
                        PValue = pStatMat[i];
                        found = true;
                    }
                }

                Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("numBins = " + numBins + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("rCum[0] = " + rCum[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("rCum[numBins-1] = " + rCum[numBins - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("P-value = " + PValue + "\n", Preferences.DEBUG_ALGORITHM);

                if (threadStopped) {
                    finalize();

                    return;
                }

                if (PValue < 0.95f) {

                    setCompleted(true);

                    return;
                }

            } // if (doP)

            // Reduce buffer and secondBuffer so that they only contain the points
            // where (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) &&
            // inputMask.get(i))
            thrLength = 0;
            tmpBuffer = new float[volume];
            tmpSecondBuffer = new float[volume];

            for (i = 0; i < volume; i++) {

                if (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) && inputMask.get(i)) {
                    tmpBuffer[thrLength] = buffer[i];
                    tmpSecondBuffer[thrLength++] = secondBuffer[i];
                }
            }

            buffer = null;
            buffer = new float[thrLength];
            secondBuffer = null;
            secondBuffer = new float[thrLength];

            for (i = 0; i < thrLength; i++) {
                buffer[i] = tmpBuffer[i];
                secondBuffer[i] = tmpSecondBuffer[i];
            }

            tmpBuffer = null;
            tmpSecondBuffer = null;
            System.gc();

            // Use linear least squares to calculate the best line fit
            // for secondBuffer = a*firstBuffer + b.  Use an orthogonal
            // line fit where the distance measurements are orthgonal to
            // the proposed line.  This is an orthogonal regression as opposed
            // to a traditional regression of dependent y variable green on
            // independent red variable x which would give measurements in the
            // y direction.  Note that a traditional regression of red on green
            // would be yet another line giving measurements in the x direction.
            // Don't include any point i with buffer[i] < background1 and
            // secondBuffer[i] < background2 in the regression line.
            // Don't want the regression line to be affected by the dataless
            // black background of the picture.
            fireProgressStateChanged("Calculating least squares fit line");
            fireProgressStateChanged(25);
            sumx = 0.0;
            sumy = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {
                sumx += buffer[i];
                sumy += secondBuffer[i];
                count++;
            }

            averagex = sumx / count;
            averagey = sumy / count;
            sumx2 = 0.0;
            sumxy = 0.0;
            sumy2 = 0.0;

            for (i = 0; i < thrLength; i++) {
                diffx = buffer[i] - averagex;
                diffy = secondBuffer[i] - averagey;
                sumx2 += diffx * diffx;
                sumxy += diffx * diffy;
                sumy2 += diffy * diffy;
            }

            /*kES = new AlgorithmEigensolver(2);
             * kES.setMatrix(0,0,sumy2); kES.setMatrix(0,1,-sumxy); kES.setMatrix(1,0,-sumxy); kES.setMatrix(1,1,sumx2);
             * kES.solve(); // Eigenvectors are stored in increasing order // Get the eigenvector for the smallest value
             * // The eigenvector of the ith eigenvalue is stored in // the ith column directionx =
             * kES.getEigenvector(0,0); directiony = kES.getEigenvector(1,0); delta = directiony/directionx; a =
             * (float)(delta);*/
            varx = sumx2 / (count - 1);
            vary = sumy2 / (count - 1);
            covarxy = sumxy / (count - 1);
            a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                             (2 * covarxy));
            b = (float) (averagey - (a * averagex));
            mse = (float) (varx + (2 * a * covarxy) + (a * a * vary)) / (1 + (a * a));

            // +-2 standard deviations on each side includes about 95%.
            halfWidth = (float) (2.0 * Math.sqrt(mse));

            Preferences.debug("First iteration\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug(baseImage.getImageName() + " = " + a + "*" + srcImage.getImageName() + " + " + b + "\n", 
            		Preferences.DEBUG_ALGORITHM);

            Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);
            doAgain = false;

            for (iter = 0; (iter < 10) && doAgain; iter++) {
                doAgain = false;
                sumx = 0.0;
                sumy = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {
                    distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) / ((a * a) + 1);

                    if (distance <= halfWidth) {
                        sumx += buffer[i];
                        sumy += secondBuffer[i];
                        count++;
                    }
                }

                averagex = sumx / count;
                averagey = sumy / count;
                sumx2 = 0.0;
                sumxy = 0.0;
                sumy2 = 0.0;

                for (i = 0; i < thrLength; i++) {
                    distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) / ((a * a) + 1);

                    if (distance <= halfWidth) {
                        diffx = buffer[i] - averagex;
                        diffy = secondBuffer[i] - averagey;
                        sumx2 += diffx * diffx;
                        sumxy += diffx * diffy;
                        sumy2 += diffy * diffy;
                    }
                }

                varx = sumx2 / (count - 1);
                vary = sumy2 / (count - 1);
                covarxy = sumxy / (count - 1);
                aLast = a;
                bLast = b;
                a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                 (2 * covarxy));
                b = (float) (averagey - (a * averagex));
                mse = (float) (varx + (2 * a * covarxy) + (a * a * vary)) / (1 + (a * a));

                // +-2 standard deviations on each side includes about 95%.
                halfWidth = (float) (2.0 * Math.sqrt(mse));

                Preferences.debug("Iteration = " + (iter + 2) + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug(baseImage.getImageName() + " = " + a + "*" + srcImage.getImageName() + " + " + b +
                                  "\n", Preferences.DEBUG_ALGORITHM);

                Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

                if ((aLast != 0.0f) && ((Math.abs(a - aLast) / aLast) > 0.001)) {
                    doAgain = true;
                }

                if ((bLast != 0.0f) && ((Math.abs(b - bLast) / bLast) > 0.001)) {
                    doAgain = true;
                }
            } // for (iter = 0; (iter < 10) && doAgain; iter++)

            fireProgressStateChanged("Generating histogram buffer");
            fireProgressStateChanged(50);

            for (i = 0; i < thrLength; i++) {
                ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                // invert y
                histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
            }

            fireProgressStateChanged("Generating VOI for least squares line");
            fireProgressStateChanged(60);
            xArray = new float[2];
            yArray = new float[2];
            zArray = new float[2];
            zArray[0] = 0.0f;
            zArray[1] = 0.0f;

            // secondBuffer[i] = a*buffer[i] + b
            // ch1 = (buffer[i] - min1)*scale1 + leftPad
            // ch2 = (secondBuffer[i] - min2)*scale2 + topPad;
            // ch1 = (scale1/a)*[((ch2-topPad)/scale2) + min2 - a*min1 - b] + leftPad
            // ch2 = scale2*[((a*(ch1-leftPad))/scale1) + a*min1 - min2 + b] + topPad
            ch2f = topPad;
            ch1f = (float) ((scale1 / a) * (min2 - (a * min1) - b)) + leftPad;

            if (ch1f < leftPad) {
                ch1f = leftPad;
                ch2f = (float) (scale2 * ((a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[0] = ch1f;
            yArray[0] = ch2f;
            ch2f = topPad + bin2 - 1;
            ch1f = (float) ((scale1 / a) * (((ch2f - topPad) / scale2) + min2 - (a * min1) - b)) + leftPad;

            if (ch1f > (bin1 - 1 + leftPad)) {
                ch1f = bin1 - 1 + leftPad;
                ch2f = (float) (scale2 * (((a * (bin1 - 1)) / scale1) + (a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[1] = ch1f;
            yArray[1] = ch2f;

            if (!doSecondIteration) {
                fitLineVOI = new VOI((short) 0, "fitLine", VOI.LINE, -1.0f);
                fitLineVOI.importCurve(xArray, yArray, zArray);
                destImage.registerVOI(fitLineVOI);
                fitLineVOI.setFixed(true);
                fitLineVOI.setColor(Color.orange);
            } // if (!doSecondIteration)

            if (threadStopped) { // do before copying back into image
                finalize();

                return;
            }

            destImage.importData(0, histBuffer, true);
            destRes = new float[2];
            destRes[0] = 1.0f;
            destRes[1] = 1.0f;
            destImage.getFileInfo(0).setResolutions(destRes);

        } // try
        catch (IOException error) {
            displayError("Algorithm ColocalizationRegression reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            secondBuffer = null;
            displayError("Algorithm ColocalizationRegression reports: out of memory" + e);
            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Generating thresholded linear correlations");
        fireProgressStateChanged(70);

        // secondBuffer[i] = a*buffer[i] + b;
        // buffer[i] = (secondBuffer[i] - b)/a;
        // Calculate (lineMin1,lineMin2) and (lineMax1,lineMax2),
        // the endpoints of the line segment
        lineMin1 = min1;
        lineMin2 = (a * min1) + b;

        if (lineMin2 < min2) {
            lineMin2 = min2;
            lineMin1 = (min2 - b) / a;
        }

        lineMax1 = max1;
        lineMax2 = (a * max1) + b;

        if (lineMax2 > max2) {
            lineMax2 = max2;
            lineMax1 = (max2 - b) / a;
        }

        firstNonPositive1 = -1;
        firstNonPositive2 = -1;
        minPositiveThreshold = -1;
        nonPositiveThreshold = 1;
        lastPositive1 = (float) (Math.floor(lineMax1) + 1);
        lastPositive2 = (float) (Math.floor(lineMax2) + 1);
        firstUndefined1 = -1;
        firstUndefined2 = -1;

        // Calculate the linear correlation coefficients for all pixels
        // whose values are either below threshold in buffer or are below
        // a*threshold + b in secondBuffer.  The values in buffer range from
        // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
        // along the color that has the greatest range along the line
        // segment.
        if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2)) {
            thresholdOn1 = true;
            rThreshold = null;
            rThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(lineMin1)) + 1];
            colocSize = null;
            colocSize = new float[rThreshold.length];
            colocIntensity1 = null;
            colocIntensity1 = new float[rThreshold.length];
            colocIntensity2 = null;
            colocIntensity2 = new float[rThreshold.length];
            haveThreshold = new boolean[rThreshold.length];

            for (i = 0; i < rThreshold.length; i++) {
                rThreshold[i] = Float.NaN;
                colocSize[i] = 0.0f;
                colocIntensity1[i] = 0.0f;
                colocIntensity2[i] = 0.0f;
                haveThreshold[i] = false;
            }

            transitionFound = false;
            i = rThreshold.length / 2;
            thresholdIncrement = rThreshold.length / 4;

            while (!transitionFound) {
                threshold = (float) (i + Math.ceil(lineMin1));
                secondThreshold = (a * threshold) + b;
                averagex = 0.0;
                averagey = 0.0;
                count = 0;
                morex = false;
                morey = false;
                lastx = buffer[0];
                lasty = secondBuffer[0];
                voiIntensity1Thr = 0.0f;
                voiIntensity2Thr = 0.0f;

                for (j = 0; j < thrLength; j++) {
                    float buff = buffer[j];
                    float secBuff = secondBuffer[j];

                    if (buff >= threshold) {
                        voiIntensity1Thr += buff;
                    }

                    if (secBuff >= secondThreshold) {
                        voiIntensity2Thr += secBuff;
                    }

                    if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                        colocSize[i] += 1.0f;
                        colocIntensity1[i] += buff;
                        colocIntensity2[i] += secBuff;
                    } else {
                        averagex += buff;
                        averagey += secBuff;
                        count++;

                        if (count == 1) {
                            lastx = buff;
                            lasty = secBuff;
                        } else if (count >= 2) {

                            if (!morex) {

                                if (buff != lastx) {
                                    morex = true;
                                }
                            }

                            if (!morey) {

                                if (secBuff != lasty) {
                                    morey = true;
                                }
                            }
                        } // else if (count >= 2)
                    }
                } // for (j = 0; j < thrLength; j++)

                colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                if (doColocWithThresholds) {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                } else {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                }

                if (morex && morey) {
                    averagex /= count;
                    averagey /= count;
                    num = 0.0;
                    denom1 = 0.0;
                    denom2 = 0.0;

                    for (j = 0; j < thrLength; j++) {
                        float buff = buffer[j];
                        float secBuff = secondBuffer[j];

                        if ((buff < threshold) || (secBuff < secondThreshold)) {
                            num += (buff - averagex) * (secBuff - averagey);
                            denom1 += (buff - averagex) * (buff - averagex);
                            denom2 += (secBuff - averagey) * (secBuff - averagey);
                        }
                    } // for (j = 0; j < thrLength; j++)

                    rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                    haveThreshold[i] = true;
                    Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", Preferences.DEBUG_ALGORITHM);

                    if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i - 1];
                        firstNonPositive1 = (float) ((i - 1) + Math.ceil(lineMin1));
                        firstNonPositive2 = (a * firstNonPositive1) + b;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                   Float.isNaN(rThreshold[i - 1])) {
                        transitionFound = true;
                        firstUndefined1 = (float) ((i - 1) + Math.ceil(lineMin1));
                        firstUndefined2 = (a * firstUndefined1) + b;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                        transitionFound = true;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i];
                        firstNonPositive1 = (float) (i + Math.ceil(lineMin1));
                        firstNonPositive2 = (a * firstNonPositive1) + b;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && Float.isNaN(rThreshold[i]) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        firstUndefined1 = (float) (i + Math.ceil(lineMin1));
                        firstUndefined2 = (a * firstUndefined1) + b;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if (rThreshold[i] > 0.0f) {
                        i = i - thresholdIncrement;

                        if (i < 0) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // else if (rThreshold[i] > 0.0f)
                    else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    }
                } // if (morex && morey)
                else { // Float.isNaN(rThreshold[i])
                    haveThreshold[i] = true;
                    i = i + thresholdIncrement;

                    if (i >= rThreshold.length) {
                        transitionFound = true;
                    }

                    thresholdIncrement = thresholdIncrement / 2;

                    if (thresholdIncrement == 0) {
                        thresholdIncrement = 1;
                    }
                } // Float.isNaN(rThreshold[i])
            } // while (!transitionFound)

        } // if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2))
        else { // ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)
            thresholdOn1 = false;
            rThreshold = null;
            rThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(lineMin2)) + 1];
            colocSize = null;
            colocSize = new float[rThreshold.length];
            colocIntensity1 = null;
            colocIntensity1 = new float[rThreshold.length];
            colocIntensity2 = null;
            colocIntensity2 = new float[rThreshold.length];
            haveThreshold = new boolean[rThreshold.length];

            for (i = 0; i < rThreshold.length; i++) {
                rThreshold[i] = Float.NaN;
                colocSize[i] = 0.0f;
                colocIntensity1[i] = 0.0f;
                colocIntensity2[i] = 0.0f;
                haveThreshold[i] = false;
            }

            transitionFound = false;
            i = rThreshold.length / 2;
            thresholdIncrement = rThreshold.length / 4;

            while (!transitionFound) {
                secondThreshold = (float) (i + Math.ceil(lineMin2));
                threshold = (secondThreshold - b) / a;
                averagex = 0.0;
                averagey = 0.0;
                count = 0;
                morex = false;
                morey = false;
                lastx = buffer[0];
                lasty = secondBuffer[0];
                voiIntensity1Thr = 0.0f;
                voiIntensity2Thr = 0.0f;

                for (j = 0; j < thrLength; j++) {
                    float buff = buffer[j];
                    float secBuff = secondBuffer[j];

                    if (buff >= threshold) {
                        voiIntensity1Thr += buff;
                    }

                    if (secBuff >= secondThreshold) {
                        voiIntensity2Thr += secBuff;
                    }

                    if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                        colocSize[i] += 1.0f;
                        colocIntensity1[i] += buff;
                        colocIntensity2[i] += secBuff;
                    } else {
                        averagex += buff;
                        averagey += secBuff;
                        count++;

                        if (count == 1) {
                            lastx = buff;
                            lasty = secBuff;
                        } else if (count >= 2) {

                            if (!morex) {

                                if (buff != lastx) {
                                    morex = true;
                                }
                            }

                            if (!morey) {

                                if (secBuff != lasty) {
                                    morey = true;
                                }
                            }
                        } // else if (count >= 2)
                    }
                } // for (j = 0; j < thrLength; j++)

                colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                if (doColocWithThresholds) {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                } else {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                }

                if (morex && morey) {
                    averagex /= count;
                    averagey /= count;
                    num = 0.0;
                    denom1 = 0.0;
                    denom2 = 0.0;

                    for (j = 0; j < thrLength; j++) {
                        float buff = buffer[j];
                        float secBuff = secondBuffer[j];

                        if ((buff < threshold) || (secBuff < secondThreshold)) {
                            num += (buff - averagex) * (secBuff - averagey);
                            denom1 += (buff - averagex) * (buff - averagex);
                            denom2 += (secBuff - averagey) * (secBuff - averagey);
                        }
                    } // for (j = 0; j < thrLength; j++)

                    rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                    haveThreshold[i] = true;
                    Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", Preferences.DEBUG_ALGORITHM);

                    if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i - 1];
                        firstNonPositive2 = (float) ((i - 1) + Math.ceil(lineMin2));
                        firstNonPositive1 = (firstNonPositive2 - b) / a;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                   Float.isNaN(rThreshold[i - 1])) {
                        transitionFound = true;
                        firstUndefined2 = (float) ((i - 1) + Math.ceil(lineMin2));
                        firstUndefined1 = (firstUndefined2 - b) / a;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                        transitionFound = true;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i];
                        firstNonPositive2 = (float) (i + Math.ceil(lineMin2));
                        firstNonPositive1 = (firstNonPositive2 - b) / a;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && Float.isNaN(rThreshold[i]) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        firstUndefined2 = (float) (i + Math.ceil(lineMin2));
                        firstUndefined1 = (firstUndefined2 - b) / a;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if (rThreshold[i] > 0.0f) {
                        i = i - thresholdIncrement;

                        if (i < 0) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // else if (rThreshold[i] > 0.0f)
                    else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    }
                } // if (morex && morey)
                else { // Float.isNaN(rThreshold[i])
                    haveThreshold[i] = true;
                    i = i + thresholdIncrement;

                    if (i >= rThreshold.length) {
                        transitionFound = true;
                    }

                    thresholdIncrement = thresholdIncrement / 2;

                    if (thresholdIncrement == 0) {
                        thresholdIncrement = 1;
                    }
                } // Float.isNaN(rThreshold[i])
            } // while (!transitionFound)

        } // else ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)

        if (firstUndefined1 >= 0) {
            Preferences.debug("Cannot calculate linear correlation coefficient \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstUndefined1 + " or " +
                              baseImage.getImageName() + " < " + firstUndefined2 + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (nonPositiveThreshold <= 0) {
            Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstNonPositive1 + " or " +
                              baseImage.getImageName() + " < " + firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
        }

        Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + lastPositive1 + " or " +
                          baseImage.getImageName() + " < " + lastPositive2 + "\n", Preferences.DEBUG_ALGORITHM);

        if (minPositiveThreshold < 0) {
            removeVOIUpdateListener();
            MipavUtil.displayError("No positve threshold found");

            setCompleted(false);

            return;
        }

        if (doSecondIteration) {
            Preferences.debug("Second iteration excluding subthresholded region\n", Preferences.DEBUG_ALGORITHM);
            fireProgressStateChanged("Second iteration excluding subthresholded region");
            fireProgressStateChanged(80);
            t1 = lastPositive1;
            t2 = lastPositive2;

            averagex = 0.0;
            averagey = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    averagex += buffer[i];
                    averagey += secondBuffer[i];
                    count++;
                }
            }

            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    num += (buffer[i] - averagex) * (secondBuffer[i] - averagey);
                    denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                    denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                }
            }

            denom = Math.sqrt(denom1 * denom2);
            r = (float) (num / denom);
            Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);

            // Use linear least squares to calculate the best line fit
            // for secondBuffer = a*firstBuffer + b.  Use an orthogonal
            // line fit where the distance measurements are orthgonal to
            // the proposed line.  This is an orthogonal regression as opposed
            // to a traditional regression of dependent y variable green on
            // independent red variable x which would give measurements in the
            // y direction.  Note that a traditional regression of red on green
            // would be yet another line giving measurements in the x direction.
            // Don't include any point i with buffer[i] < lastPositive1 or
            // secondBuffer[i] < lastPositive2 in the regression line.
            // This excludes the L shaped subthresholded region from the
            // first iteration.
            sumx = 0.0;
            sumy = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    sumx += buffer[i];
                    sumy += secondBuffer[i];
                    count++;
                }
            }

            averagex = sumx / count;
            averagey = sumy / count;
            sumx2 = 0.0;
            sumxy = 0.0;
            sumy2 = 0.0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    diffx = buffer[i] - averagex;
                    diffy = secondBuffer[i] - averagey;
                    sumx2 += diffx * diffx;
                    sumxy += diffx * diffy;
                    sumy2 += diffy * diffy;
                }
            }

            varx = sumx2 / (count - 1);
            vary = sumy2 / (count - 1);
            covarxy = sumxy / (count - 1);
            a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                             (2 * covarxy));
            b = (float) (averagey - (a * averagex));
            mse = (float) (varx + (2 * a * covarxy) + (a * a * vary)) / (1 + (a * a));

            // +-2 standard deviations on each side includes about 95%.
            halfWidth = (float) (2.0 * Math.sqrt(mse));

            Preferences.debug(baseImage.getImageName() + " = " + a + "*" + srcImage.getImageName() + " + " + b + "\n",
            		Preferences.DEBUG_ALGORITHM);

            Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);
            doAgain = false;

            for (iter = 0; (iter < 10) && doAgain; iter++) {
                doAgain = false;
                sumx = 0.0;
                sumy = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {

                    if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                        distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) / ((a * a) + 1);

                        if (distance <= halfWidth) {
                            sumx += buffer[i];
                            sumy += secondBuffer[i];
                            count++;
                        }
                    }
                }

                averagex = sumx / count;
                averagey = sumy / count;
                sumx2 = 0.0;
                sumxy = 0.0;
                sumy2 = 0.0;

                for (i = 0; i < thrLength; i++) {

                    if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                        distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) / ((a * a) + 1);

                        if (distance <= halfWidth) {
                            diffx = buffer[i] - averagex;
                            diffy = secondBuffer[i] - averagey;
                            sumx2 += diffx * diffx;
                            sumxy += diffx * diffy;
                            sumy2 += diffy * diffy;
                        }
                    }
                }

                varx = sumx2 / (count - 1);
                vary = sumy2 / (count - 1);
                covarxy = sumxy / (count - 1);
                aLast = a;
                bLast = b;
                a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                 (2 * covarxy));
                b = (float) (averagey - (a * averagex));
                mse = (float) (varx + (2 * a * covarxy) + (a * a * vary)) / (1 + (a * a));

                // +-2 standard deviations on each side includes about 95%.
                halfWidth = (float) (2.0 * Math.sqrt(mse));

                Preferences.debug("Iteration = " + (iter + 2) + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug(baseImage.getImageName() + " = " + a + "*" + srcImage.getImageName() + " + " + b +
                                  "\n", Preferences.DEBUG_ALGORITHM);

                Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

                if ((aLast != 0.0f) && ((Math.abs(a - aLast) / aLast) > 0.001)) {
                    doAgain = true;
                }

                if ((bLast != 0.0f) && ((Math.abs(b - bLast) / bLast) > 0.001)) {
                    doAgain = true;
                }
            } // for (iter = 0; (iter < 10) && doAgain; iter++)

            // secondBuffer[i] = a*buffer[i] + b
            // ch1 = (buffer[i] - min1)*scale1 + leftPad
            // ch2 = (secondBuffer[i] - min2)*scale2 + topPad;
            // ch1 = (scale1/a)*[((ch2-topPad)/scale2) + min2 - a*min1 - b] + leftPad
            // ch2 = scale2*[((a*(ch1-leftPad))/scale1) + a*min1 - min2 + b] + topPad
            ch2f = topPad;
            ch1f = (float) ((scale1 / a) * (min2 - (a * min1) - b)) + leftPad;

            if (ch1f < leftPad) {
                ch1f = leftPad;
                ch2f = (float) (scale2 * ((a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[0] = ch1f;
            yArray[0] = ch2f;
            ch2f = topPad + bin2 - 1;
            ch1f = (float) ((scale1 / a) * (((ch2f - topPad) / scale2) + min2 - (a * min1) - b)) + leftPad;

            if (ch1f > (bin1 - 1 + leftPad)) {
                ch1f = bin1 - 1 + leftPad;
                ch2f = (float) (scale2 * (((a * (bin1 - 1)) / scale1) + (a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[1] = ch1f;
            yArray[1] = ch2f;
            fitLineVOI = new VOI((short) 0, "fitLine", VOI.LINE, -1.0f);
            fitLineVOI.importCurve(xArray, yArray, zArray);
            destImage.registerVOI(fitLineVOI);
            fitLineVOI.setFixed(true);
            fitLineVOI.setColor(Color.orange);

            // secondBuffer[i] = a*buffer[i] + b;
            // buffer[i] = (secondBuffer[i] - b)/a;
            // Calculate (lineMin1,lineMin2) and (lineMax1,lineMax2),
            // the endpoints of the line segment
            lineMin1 = min1;
            lineMin2 = (a * min1) + b;

            if (lineMin2 < min2) {
                lineMin2 = min2;
                lineMin1 = (min2 - b) / a;
            }

            lineMax1 = max1;
            lineMax2 = (a * max1) + b;

            if (lineMax2 > max2) {
                lineMax2 = max2;
                lineMax1 = (max2 - b) / a;
            }

            firstNonPositive1 = -1;
            firstNonPositive2 = -1;
            minPositiveThreshold = -1;
            nonPositiveThreshold = 1;
            lastPositive1 = (float) (Math.floor(lineMax1) + 1);
            lastPositive2 = (float) (Math.floor(lineMax2) + 1);
            firstUndefined1 = -1;
            firstUndefined2 = -1;

            // Calculate the linear correlation coefficients for all pixels
            // whose values are either below threshold in buffer or are below
            // a*threshold + b in secondBuffer.  The values in buffer range from
            // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
            // along the color that has the greatest range along the line
            // segment.
            if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2)) {
                thresholdOn1 = true;
                rThreshold = null;
                rThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(lineMin1)) + 1];
                colocSize = null;
                colocSize = new float[rThreshold.length];
                colocIntensity1 = null;
                colocIntensity1 = new float[rThreshold.length];
                colocIntensity2 = null;
                colocIntensity2 = new float[rThreshold.length];
                haveThreshold = new boolean[rThreshold.length];

                for (i = 0; i < rThreshold.length; i++) {
                    rThreshold[i] = Float.NaN;
                    colocSize[i] = 0.0f;
                    colocIntensity1[i] = 0.0f;
                    colocIntensity2[i] = 0.0f;
                    haveThreshold[i] = false;
                }

                transitionFound = false;
                i = rThreshold.length / 2;
                thresholdIncrement = rThreshold.length / 4;

                while (!transitionFound) {
                    threshold = (float) (i + Math.ceil(lineMin1));
                    secondThreshold = (a * threshold) + b;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = buffer[0];
                    lasty = secondBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {

                        if (buffer[j] >= threshold) {
                            voiIntensity1Thr += buffer[j];
                        }

                        if (secondBuffer[j] >= secondThreshold) {
                            voiIntensity2Thr += secondBuffer[j];
                        }

                        if ((buffer[j] >= threshold) && (secondBuffer[j] >= secondThreshold)) {
                            colocSize[i] += 1.0f;
                            colocIntensity1[i] += buffer[j];
                            colocIntensity2[i] += secondBuffer[j];
                        } else {
                            averagex += buffer[j];
                            averagey += secondBuffer[j];
                            count++;

                            if (count == 1) {
                                lastx = buffer[j];
                                lasty = secondBuffer[j];
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buffer[j] != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secondBuffer[j] != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                    if (doColocWithThresholds) {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {

                            if ((buffer[j] < threshold) || (secondBuffer[j] < secondThreshold)) {
                                num += (buffer[j] - averagex) * (secondBuffer[j] - averagey);
                                denom1 += (buffer[j] - averagex) * (buffer[j] - averagex);
                                denom2 += (secondBuffer[j] - averagey) * (secondBuffer[j] - averagey);
                            }
                        }

                        rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        haveThreshold[i] = true;
                        Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i - 1];
                            firstNonPositive1 = (float) ((i - 1) + Math.ceil(lineMin1));
                            firstNonPositive2 = (a * firstNonPositive1) + b;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                       Float.isNaN(rThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined1 = (float) ((i - 1) + Math.ceil(lineMin1));
                            firstUndefined2 = (a * firstUndefined1) + b;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                       (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i];
                            firstNonPositive1 = (float) (i + Math.ceil(lineMin1));
                            firstNonPositive2 = (a * firstNonPositive1) + b;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] &&
                                       Float.isNaN(rThreshold[i]) && (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined1 = (float) (i + Math.ceil(lineMin1));
                            firstUndefined2 = (a * firstUndefined1) + b;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if (rThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (rThreshold[i] > 0.0f)
                        else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= rThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(rThreshold[i])
                        haveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(rThreshold[i])
                } // while (!transitionFound)

            } // if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2))
            else { // ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)
                thresholdOn1 = false;
                rThreshold = null;
                rThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(lineMin2)) + 1];
                colocSize = null;
                colocSize = new float[rThreshold.length];
                colocIntensity1 = null;
                colocIntensity1 = new float[rThreshold.length];
                colocIntensity2 = null;
                colocIntensity2 = new float[rThreshold.length];

                for (i = 0; i < rThreshold.length; i++) {
                    rThreshold[i] = Float.NaN;
                    colocSize[i] = 0.0f;
                    colocIntensity1[i] = 0.0f;
                    colocIntensity2[i] = 0.0f;
                    haveThreshold[i] = false;
                }

                transitionFound = false;
                i = rThreshold.length / 2;
                thresholdIncrement = rThreshold.length / 4;

                while (!transitionFound) {
                    secondThreshold = (float) (i + Math.ceil(lineMin2));
                    threshold = (secondThreshold - b) / a;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = buffer[0];
                    lasty = secondBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {

                        if (buffer[j] >= threshold) {
                            voiIntensity1Thr += buffer[j];
                        }

                        if (secondBuffer[j] >= secondThreshold) {
                            voiIntensity2Thr += secondBuffer[j];
                        }

                        if ((buffer[j] >= threshold) && (secondBuffer[j] >= secondThreshold)) {
                            colocSize[i] += 1.0f;
                            colocIntensity1[i] += buffer[j];
                            colocIntensity2[i] += secondBuffer[j];
                        } else {
                            averagex += buffer[j];
                            averagey += secondBuffer[j];
                            count++;

                            if (count == 1) {
                                lastx = buffer[j];
                                lasty = secondBuffer[j];
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buffer[j] != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secondBuffer[j] != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                    if (doColocWithThresholds) {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {

                            if ((buffer[j] < threshold) || (secondBuffer[j] < secondThreshold)) {
                                num += (buffer[j] - averagex) * (secondBuffer[j] - averagey);
                                denom1 += (buffer[j] - averagex) * (buffer[j] - averagex);
                                denom2 += (secondBuffer[j] - averagey) * (secondBuffer[j] - averagey);
                            }
                        }

                        rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        haveThreshold[i] = true;
                        Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i - 1];
                            firstNonPositive2 = (float) ((i - 1) + Math.ceil(lineMin2));
                            firstNonPositive1 = (firstNonPositive2 - b) / a;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                       Float.isNaN(rThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined2 = (float) ((i - 1) + Math.ceil(lineMin2));
                            firstUndefined1 = (firstUndefined2 - b) / a;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                       (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i];
                            firstNonPositive2 = (float) (i + Math.ceil(lineMin2));
                            firstNonPositive1 = (firstNonPositive2 - b) / a;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] &&
                                       Float.isNaN(rThreshold[i]) && (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined2 = (float) (i + Math.ceil(lineMin2));
                            firstUndefined1 = (firstUndefined2 - b) / a;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if (rThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (rThreshold[i] > 0.0f)
                        else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= rThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(rThreshold[i])
                        haveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(rThreshold[i])
                } // while (!transitionFound)

            } // else ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)

            if (firstUndefined1 >= 0) {
                Preferences.debug("Cannot calculate linear correlation coefficient \n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstUndefined1 + " or " +
                                  baseImage.getImageName() + " < " + firstUndefined2 + "\n", Preferences.DEBUG_ALGORITHM);
            }

            if (nonPositiveThreshold <= 0) {
                Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + firstNonPositive1 + " or " +
                                  baseImage.getImageName() + " < " + firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with " + srcImage.getImageName() + " < " + lastPositive1 + " or " +
                              baseImage.getImageName() + " < " + lastPositive2 + "\n", Preferences.DEBUG_ALGORITHM);

            if (minPositiveThreshold < 0) {
                removeVOIUpdateListener();
                MipavUtil.displayError("No positve threshold found on second iteration");

                setCompleted(false);

                return;
            }
        } // if (doSecondIteration)

        frameList = srcImage.getImageFrameVector();

        for (i = 0; i < frameList.size(); i++) {

            if ((((ViewJFrameBase) frameList.elementAt(i)).getControls()) != null) {
                controlFrame = ((ViewJFrameBase) frameList.elementAt(i));
            }
        }

        fireProgressStateChanged("Creating point VOI");
        fireProgressStateChanged(85);

        // position the point at the minimum positive threshold
        xArray = new float[1];
        yArray = new float[1];
        zArray = new float[1];

        // Allow 10 blank spaces at top and 10 blank spaces at bottom
        if (pointCalculation) {
            xArray[0] = (float) ((point1 - min1) * scale1) + leftPad;
            yArray[0] = (float) (bin2 - 1.0f - ((point2 - min2) * scale2)) + topPad;

        } else {
            xArray[0] = (float) ((lastPositive1 - min1) * scale1) + leftPad;
            yArray[0] = (float) (bin2 - 1.0f - ((lastPositive2 - min2) * scale2)) + topPad;
        }

        zArray[0] = 0.0f;
        pointVOI = new VOI((short) 1, "thresholdPoint", VOI.POINT, -1.0f);
        pointVOI.importCurve(xArray, yArray, zArray);
        destImage.registerVOI(pointVOI);
        pointVOI.setFixed(false);

        // Don't draw point - draw square with blue inside and yellow edge
        pointVOI.setColor(Color.green);

        /*UI.setDataText("Linear correlation coefficient = " + r + "\n");
         * UI.setDataText(baseImage.getImageName() + " = " + a + "*" + srcImage.getImageName()+ " + " + b + "\n");
         * UI.setDataText("Mean square error = " + mse + "\n"); if (firstUndefined1 >= 0) { UI.setDataText("Cannot
         * calculate linear correlation coefficient \n"); UI.setDataText("for pixels with " + srcImage.getImageName() +
         * " < " + firstUndefined1 + " or " + baseImage.getImageName() + " < " + firstUndefined2 + "\n"); } if
         * (nonPositiveThreshold <= 0) { UI.setDataText("Nonpositive linear correlation coefficient = " +
         * nonPositiveThreshold + "\n"); UI.setDataText("for pixels with " + srcImage.getImageName() + " < " +
         * firstNonPositive1 + " or " + baseImage.getImageName() + " < " + firstNonPositive2 + "\n"); }
         * UI.setDataText("Linear correlation coefficient = " + minPositiveThreshold + "\n"); UI.setDataText("for pixels
         * with " + srcImage.getImageName() + " < " + lastPositive1 + " or " + baseImage.getImageName() + " < " +
         * lastPositive2 + "\n");*/

        fireProgressStateChanged("Creating ColocalizationRegression frame");
        fireProgressStateChanged(90);

        if (redo) {
            haveFreeRangeThreshold = null;
            freeRangeRThreshold = null;
            freeRangeColocSize = null;
            freeRangeColocIntensity1 = null;
            freeRangeColocIntensity2 = null;
            frameColocalize.setNewVar(a, b, r, PValue, haveThreshold, rThreshold, colocSize, colocIntensity1,
                                      colocIntensity2, min1, max1, min2, max2, scale1, scale2, lineMin1, lineMax1,
                                      lineMin2, lineMax2, thresholdOn1);
        } else {

            // System.err.println("Constr2");
            frameColocalize = new ViewJFrameColocalizationRegression(this, subtractedSrcImage, null,
                                                                     subtractedBaseImage, null, null, destImage,
                                                                     controlFrame, useRed, useGreen, useBlue, a, b, r,
                                                                     PValue, haveThreshold, rThreshold, colocSize,
                                                                     colocIntensity1, colocIntensity2, min1, max1, min2,
                                                                     max2, scale1, scale2, lineMin1, lineMax1, lineMin2,
                                                                     lineMax2, thresholdOn1, leftPad, rightPad,
                                                                     bottomPad, topPad, doSecondIteration,
                                                                     pointCalculation);

            if (pointCalculation) {
                frameColocalize.pointCalculate();
            }
        }

        fireProgressStateChanged(100);

        setCompleted(true);
    }

    /**
     * This function produces a 2D histogram image with first color values represented across the x axis and second
     * color values represented across the y axis. The total least squares line and a matrix of correlation coefficients
     * for L shaped subthreshold regions are also generated.
     */
    private void calcColor2D() {
        int i;
        int pos;
        float[] randomBuffer;
        double[] histBuffer;
        ModelImage refImage;
        ModelImage inputImage;
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
        boolean transformVOI = false;
        boolean clip = true;
        int c;
        TransMatrix xfrm;
        double max1, max2, range1, range2;
        double averagex, averagey;
        double diffx, diffy;
        double varx, vary, covarxy;
        float mse; // mean square error
        float halfWidth;
        float distance;
        int iter;
        boolean doAgain;
        float aLast;
        float bLast;
        boolean morex, morey;
        float lastx, lasty;

        int ch1, ch2;
        float ch1f, ch2f;
        double lineMax1, lineMax2;
        float[] xArray;
        float[] yArray;
        float[] zArray;
        VOI fitLineVOI;
        VOI pointVOI;
        double num, denom1, denom2, denom;
        String nameR, nameG, nameB;
        ModelImage resultImageR = null;
        ModelImage resultImageG = null;
        ModelImage resultImageB = null;
        AlgorithmAutoCovariance algoAutoCovariance;
        int fwhmR = Integer.MAX_VALUE;
        int fwhmG = Integer.MAX_VALUE;
        int fwhmB = Integer.MAX_VALUE;
        int fwhm = Integer.MAX_VALUE;
        boolean scrambleFirst;
        int xDim, yDim;
        int length;
        int x, y;
        int numSquares;
        Vector<Integer> vectRand;
        RandomNumberGen randomGen;
        int randNum;
        Integer randSquare;
        int s, j, k;
        int sx, sy;
        int numXValues;
        int numYValues;
        float[] rMat = new float[200];
        int numBins;
        float[] rCum;
        float[] pStatMat;
        boolean found;
        int xRemainder = 0;
        int yRemainder = 0;
        int xStart = 0;
        int yStart = 0;
        int xEnd;
        int yEnd;
        double sumx;
        double sumx2;
        double sumy;
        double sumxy;
        double sumy2;
        float threshold;
        float secondThreshold;
        int count;
        float nonPositiveThreshold;
        float minPositiveThreshold;
        float lastPositive1, lastPositive2;
        float firstNonPositive1, firstNonPositive2;
        float firstUndefined1, firstUndefined2;
        Vector<ViewImageUpdateInterface> frameList;
        ViewJFrameBase controlFrame = null;
        float[] destRes;
        boolean transitionFound;
        int thresholdIncrement;
        float[] tmpBuffer;
        float[] tmpSecondBuffer;
        float voiIntensity1Thr;
        float voiIntensity2Thr;
        float backC1 = 0.0f;
        float backC2 = 0.0f;
        float backC3 = 0.0f;

        try {

            fireProgressStateChanged(srcImage.getImageName(), "Creating 2D Colocolization Histogram ...");

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

            if (useRed && useGreen) {
                thirdColor = 3;
            } else if (useRed && useBlue) {
                thirdColor = 2;
            } else {
                thirdColor = 1;
            }

            if ((backgroundIndex != -1) && (!redo)) {
                fireProgressStateChanged("Calculating backround average");
                fireProgressStateChanged(5);
                buffer = null;
                buffer = new float[length];
                secondBuffer = null;
                secondBuffer = new float[length];

                try {
                    srcImage.exportRGBData(color, 0, length, buffer);
                    srcImage.exportRGBData(secondColor, 0, length, secondBuffer);
                } catch (IOException e) {
                    buffer = null;
                    secondBuffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                for (i = 0; i < length; i++) {
                    backC1 += buffer[i];
                    backC2 += secondBuffer[i];
                }

                backC1 /= length;
                backC2 /= length;

                try {
                    srcImage.exportRGBData(thirdColor, 0, length, buffer);
                } catch (IOException e) {
                    buffer = null;
                    secondBuffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                for (i = 0; i < length; i++) {
                    backC3 += buffer[i];
                }

                backC3 /= length;
            } // if ((backgroundIndex != -1) && (!redo))

            if ((register) && (!redo)) {
                fireProgressStateChanged("Registering colors");
                fireProgressStateChanged(10);

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
                // Always register based on the entire image

                reg2 = new AlgorithmRegOAR2D(refImage, inputImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                             coarseRate, fineRate, doSubsample, doMultiThread);
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

                transform = new AlgorithmTransform(inputImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA, xDim,
                                                   yDim, transformVOI, clip, false);
                transform.run();
                registeredImageY = transform.getTransformedImage();
                transform.disposeLocal();
                transform = null;

                registeredImageY.calcMinMax();
                bin2 = Math.min(bin2, (int) Math.round(registeredImageY.getMax() - registeredImageY.getMin() + 1));

                reg2 = new AlgorithmRegOAR2D(inputImage, refImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                             coarseRate, fineRate, doSubsample, doMultiThread);
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
                registeredSrcImage.setVOIs(srcImage.getVOIs());
                srcImage.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                srcImage.clearMask();
                srcImage.notifyImageDisplayListeners();

                try {
                    imageFrame = new ViewJFrameImage(registeredSrcImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Unable to open frame for registered image");
                    setCompleted(false);

                    return;
                }
            } // if ((register) && (!redo))
            else if (!redo) {
                registeredSrcImage = srcImage;
            } // else if (!redo)

            if ((backgroundIndex != -1) && (!redo)) {
                fireProgressStateChanged("Subtracting background from " + srcImage.getImageName());
                fireProgressStateChanged(15);
                subtractedSrcImage = new ModelImage(srcImage.getType(), srcImage.getExtents(),
                                                    registeredSrcImage.getImageName() + "_subtract");
                buffer = null;
                buffer = new float[length];

                try {
                    registeredSrcImage.exportRGBData(color, 0, length, buffer);
                } catch (IOException e) {
                    buffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                for (i = 0; i < length; i++) {
                    buffer[i] -= backC1;

                    if (buffer[i] < 0.0f) {
                        buffer[i] = 0.0f;
                    }
                }

                try {
                    subtractedSrcImage.importRGBData(color, 0, buffer, false);
                } catch (IOException e) {
                    buffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                try {
                    registeredSrcImage.exportRGBData(secondColor, 0, length, buffer);
                } catch (IOException e) {
                    buffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                for (i = 0; i < length; i++) {
                    buffer[i] -= backC2;

                    if (buffer[i] < 0.0f) {
                        buffer[i] = 0.0f;
                    }
                }

                try {
                    subtractedSrcImage.importRGBData(secondColor, 0, buffer, true);
                } catch (IOException e) {
                    buffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                try {
                    registeredSrcImage.exportRGBData(thirdColor, 0, length, buffer);
                } catch (IOException e) {
                    buffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                for (i = 0; i < length; i++) {
                    buffer[i] -= backC3;

                    if (buffer[i] < 0.0f) {
                        buffer[i] = 0.0f;
                    }
                }

                try {
                    subtractedSrcImage.importRGBData(thirdColor, 0, buffer, true);
                } catch (IOException e) {
                    buffer = null;
                    errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                    return;
                }

                updateFileInfo(registeredSrcImage, subtractedSrcImage);

                try {
                    imageFrame3 = new ViewJFrameImage(subtractedSrcImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Unable to open frame for subtracted image");
                    setCompleted(false);

                    return;
                }

                srcImage.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                srcImage.clearMask();
                srcImage.notifyImageDisplayListeners();

                if (color == 1) {
                    bin1 = Math.min(bin1,
                                    (int) Math.round(subtractedSrcImage.getMaxR() - subtractedSrcImage.getMinR() + 1));
                } else {
                    bin1 = Math.min(bin1,
                                    (int) Math.round(subtractedSrcImage.getMaxG() - subtractedSrcImage.getMinG() + 1));
                }

                if (secondColor == 2) {
                    bin2 = Math.min(bin2,
                                    (int) Math.round(subtractedSrcImage.getMaxG() - subtractedSrcImage.getMinG() + 1));
                } else {
                    bin2 = Math.min(bin2,
                                    (int) Math.round(subtractedSrcImage.getMaxB() - subtractedSrcImage.getMinB() + 1));
                }

                int[] newExtents = new int[2];
                newExtents[0] = bin1 + leftPad + rightPad;
                newExtents[1] = bin2 + bottomPad + topPad;
                destImage.changeExtents(newExtents);
            } // if (backgroundIndex != -1) && (!redo))
            else if (!redo) {
                subtractedSrcImage = registeredSrcImage;
            } // else if (!redo)

            if ((!entireImage) && (!maskSupplied) && (!redo)) {
                subtractedSrcImage.getParentFrame().getComponentImage().getVOIHandler().addVOIUpdateListener(this);
            }

            try {
                buffer = null;
                buffer = new float[length];
                secondBuffer = null;
                secondBuffer = new float[length];

                // Blank spaces at left and bottom
                histBuffer = null;
                histBuffer = new double[(bin1 + leftPad + rightPad) * (bin2 + bottomPad + topPad)];
            } catch (OutOfMemoryError oome) {
                buffer = null;
                secondBuffer = null;
                histBuffer = null;
                errorCleanUp("Algorithm ColocalizationRegression: Out of memory", true);

                return;
            }

            for (i = 0; i < histBuffer.length; i++) {
                histBuffer[i] = 0.0;
            }

            destImage.releaseLock(); // we need to be able to alter the dest image

            try {
                subtractedSrcImage.exportRGBData(color, 0, length, buffer);
                subtractedSrcImage.exportRGBData(secondColor, 0, length, secondBuffer);
            } catch (IOException e) {
                buffer = null;
                secondBuffer = null;
                histBuffer = null;
                errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                return;
            }

            voiSize = 0;
            voiIntensity1 = 0.0f;
            voiIntensity2 = 0.0f;

            for (i = 0; i < length; i++) {

                if (inputMask.get(i)) {
                    voiSize++;

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        voiIntensity1 += buffer[i];
                        voiIntensity2 += secondBuffer[i];
                    }
                }
            }

            fireProgressStateChanged("Calculating overall linear correlation coefficient");
            fireProgressStateChanged(20);
            min1 = Double.MAX_VALUE;
            max1 = -Double.MAX_VALUE;
            min2 = Double.MAX_VALUE;
            max2 = -Double.MAX_VALUE;

            for (i = 0; i < length; i++) {

                if (buffer[i] > max1) {
                    max1 = buffer[i];
                }

                if (buffer[i] < min1) {
                    min1 = buffer[i];
                }

                if (secondBuffer[i] > max2) {
                    max2 = secondBuffer[i];
                }

                if (secondBuffer[i] < min2) {
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
            averagex = 0.0;
            averagey = 0.0;
            count = 0;

            for (i = 0; i < length; i++) {

                if (inputMask.get(i) && ((buffer[i] >= background1) || (secondBuffer[i] >= background2))) {
                    averagex += buffer[i];
                    averagey += secondBuffer[i];
                    count++;
                }
            }

            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (i = 0; i < length; i++) {

                if (inputMask.get(i) && ((buffer[i] >= background1) || (secondBuffer[i] >= background2))) {
                    num += (buffer[i] - averagex) * (secondBuffer[i] - averagey);
                    denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                    denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                }
            }

            denom = Math.sqrt(denom1 * denom2);
            r = (float) (num / denom);
            Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);

            if (doP) {
                fireProgressStateChanged("Calculating autocovariance");
                fireProgressStateChanged(10);

                if (useRed) {
                    nameR = srcImage.getImageName() + "_autocovarianceR";
                    resultImageR = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), nameR);
                }

                if (useGreen) {
                    nameG = srcImage.getImageName() + "_autocovarianceG";
                    resultImageG = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), nameG);
                }

                if (useBlue) {
                    nameB = srcImage.getImageName() + "_autocovarianceB";
                    resultImageB = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), nameB);
                }

                algoAutoCovariance = new AlgorithmAutoCovariance(resultImageR, resultImageG, resultImageB,
                                                                 subtractedSrcImage);
                algoAutoCovariance.run();

                if (useRed) {
                    fwhmR = algoAutoCovariance.getFWHMR();
                    fwhm = fwhmR;

                    if (fwhmR != Integer.MAX_VALUE) {
                        Preferences.debug("Red auto covariance full width at half maximum = " + fwhmR + "\n", Preferences.DEBUG_ALGORITHM);
                    } else {
                        ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                        ViewUserInterface.getReference().setDataText("Cannot determine red auto covariance full width at half maximum\n");
                        Preferences.debug("Cannot determine red auto covariance full width at half maximum\n", 
                        		Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (useGreen) {
                    fwhmG = algoAutoCovariance.getFWHMG();
                    fwhm = Math.min(fwhm, fwhmG);

                    if (fwhmG != Integer.MAX_VALUE) {
                        Preferences.debug("Green auto covariance full width at half maximum = " + fwhmG + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                    } else {
                        ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                        ViewUserInterface.getReference().setDataText("Cannot determine green auto covariance full width at half maximum\n");
                        Preferences.debug("Cannot determine green auto covariance full width at half maximum\n", 
                        		Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (useBlue) {
                    fwhmB = algoAutoCovariance.getFWHMB();
                    fwhm = Math.min(fwhm, fwhmB);

                    if (fwhmB != Integer.MAX_VALUE) {
                        Preferences.debug("Blue auto covariance full width at half maximum = " + fwhmB + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                    } else {
                        ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                        ViewUserInterface.getReference().setDataText("Cannot determine blue auto covariance full width at half maximum\n");
                        Preferences.debug("Cannot determine blue auto covariance full width at half maximum\n", 
                        		Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (resultImageR != null) {
                    resultImageR.disposeLocal();
                    resultImageR = null;
                }

                if (resultImageG != null) {
                    resultImageG.disposeLocal();
                    resultImageG = null;
                }

                if (resultImageB != null) {
                    resultImageB.disposeLocal();
                    resultImageB = null;
                }

                algoAutoCovariance.finalize();
                algoAutoCovariance = null;

                if (fwhm == Integer.MAX_VALUE) {
                    finalize();
                    errorCleanUp("Algorithm ColocalizationRegression could not determine fwhm", true);

                    return;
                } else if (fwhm == fwhmR) {
                    scrambleFirst = true;
                } else if (fwhm == fwhmG) {

                    if (useRed) {
                        scrambleFirst = false;
                    } else {
                        scrambleFirst = true;
                    }
                } else {
                    scrambleFirst = false;
                }

                if (fwhm == 0) {
                    fwhm = 1;
                }

                fireProgressStateChanged("Calculating P-value");
                fireProgressStateChanged(15);

                // Calculate denom without inputMask
                averagex = 0.0;
                averagey = 0.0;
                count = 0;

                for (i = 0; i < length; i++) {

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        averagex += buffer[i];
                        averagey += secondBuffer[i];
                        count++;
                    }
                }

                averagex /= count;
                averagey /= count;
                num = 0.0;
                denom1 = 0.0;
                denom2 = 0.0;

                for (i = 0; i < length; i++) {

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        num += (buffer[i] - averagex) * (secondBuffer[i] - averagey);
                        denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                        denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                    }
                }

                denom = Math.sqrt(denom1 * denom2);
                randomBuffer = null;
                randomBuffer = new float[length];
                numXValues = xDim / fwhm;
                xRemainder = xDim % fwhm;
                numYValues = yDim / fwhm;
                yRemainder = yDim % fwhm;
                numSquares = numXValues * numYValues;
                randomGen = new RandomNumberGen();
                vectRand = new Vector<Integer>();

                int idx;
                int end;

                for (i = 0; i < 200; i++) {

                    for (s = 0; s < numSquares; s++) {
                        vectRand.add(new Integer(s));
                    }

                    for (s = 0; s < numSquares; s++) {
                        randNum = randomGen.genUniformRandomNum(0, vectRand.size() - 1);
                        xStart = randomGen.genUniformRandomNum(0, xRemainder);
                        xEnd = xDim - (xRemainder - xStart);
                        yStart = randomGen.genUniformRandomNum(0, yRemainder);
                        yEnd = yDim - (yRemainder - yStart);
                        randSquare = vectRand.elementAt(randNum);
                        vectRand.removeElementAt(randNum);
                        y = (fwhm * (randSquare.intValue() / numXValues)) + yStart;
                        x = (fwhm * (randSquare.intValue() % numXValues)) + xStart;
                        sy = (fwhm * (s / numXValues)) + yStart;
                        sx = (fwhm * (s % numXValues)) + xStart;

                        for (k = 0; k < fwhm; k++) {

                            for (j = 0; j < fwhm; j++) {

                                if (scrambleFirst) {
                                    randomBuffer[sx + j + ((sy + k) * xDim)] = buffer[x + j + ((y + k) * xDim)];
                                } else {
                                    randomBuffer[sx + j + ((sy + k) * xDim)] = secondBuffer[x + j + ((y + k) * xDim)];
                                }
                            } // for (j = 0; j < fwhm; j++)
                        } // for (k = 0; k < fwhm; k++)

                        for (y = 0; y < yDim; y++) {
                            idx = y * xDim;
                            end = idx + xStart;

                            for (x = idx; x < end; x++) {

                                if (scrambleFirst) {
                                    randomBuffer[x] = buffer[x];
                                } else {
                                    randomBuffer[x] = secondBuffer[x];
                                }
                            }
                        }

                        for (y = 0; y < yDim; y++) {
                            idx = y * xDim;

                            for (x = xEnd; x < xDim; x++) {

                                if (scrambleFirst) {
                                    randomBuffer[x + idx] = buffer[x + idx];
                                } else {
                                    randomBuffer[x + idx] = secondBuffer[x + idx];
                                }
                            }
                        }

                        for (y = 0; y < yStart; y++) {
                            idx = y * xDim;
                            end = idx + xDim;

                            for (x = idx; x < end; x++) {

                                if (scrambleFirst) {
                                    randomBuffer[x] = buffer[x];
                                } else {
                                    randomBuffer[x] = secondBuffer[x];
                                }
                            }
                        }

                        for (y = yEnd; y < yDim; y++) {
                            idx = y * xDim;
                            end = idx + xDim;

                            for (x = idx; x < end; x++) {

                                if (scrambleFirst) {
                                    randomBuffer[x] = buffer[x];
                                } else {
                                    randomBuffer[x] = secondBuffer[x];
                                }
                            }
                        }
                    } // for (s = 0; s < numSquares; s++)

                    if (scrambleFirst) {
                        num = 0.0f;

                        for (j = 0; j < length; j++) {

                            if ((randomBuffer[j] >= background1) || (secondBuffer[j] >= background2)) {
                                num += (randomBuffer[j] - averagex) * (secondBuffer[j] - averagey);
                            }
                        }

                        rMat[i] = (float) (num / denom);
                    } else {
                        num = 0.0f;

                        for (j = 0; j < length; j++) {

                            if ((buffer[j] >= background1) || (randomBuffer[j] >= background2)) {
                                num += (buffer[j] - averagex) * (randomBuffer[j] - averagey);
                            }
                        }

                        rMat[i] = (float) (num / denom);
                    }
                } // for (i = 0; i < 200; i++)

                randomBuffer = null;

                Arrays.sort(rMat);
                numBins = 200;

                for (i = 1; i < 200; i++) {

                    if (rMat[i] == rMat[i - 1]) {
                        numBins--;
                    }
                }

                rCum = new float[numBins];
                pStatMat = new float[numBins];
                rCum[0] = rMat[0];
                pStatMat[0] = 0.0f;
                pStatMat[1] = 0.005f;
                j = 1;

                for (i = 1; i < 200; i++) {

                    if (rMat[i] == rMat[i - 1]) {
                        pStatMat[j] += 0.005f;
                    } else {
                        rCum[j++] = rMat[i];

                        if (j < numBins) {
                            pStatMat[j] = pStatMat[j - 1] + 0.005f;
                        }
                    }
                } // for (i = 1; i < 200; i++)

                PValue = 0.995f;
                found = false;

                for (i = 0; (i <= (numBins - 1)) && (!found); i++) {

                    if (r <= rCum[i]) {
                        PValue = pStatMat[i];
                        found = true;
                    }
                }

                Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("numBins = " + numBins + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("rCum[0] = " + rCum[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("rCum[numBins-1] = " + rCum[numBins - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("P-value = " + PValue + "\n", Preferences.DEBUG_ALGORITHM);

                if (threadStopped) {
                    finalize();

                    return;
                }

                if (PValue < 0.95f) {

                    setCompleted(true);

                    return;
                }

            } // if (doP)

            // Reduce buffer and secondBuffer so that they only contain the points
            // where (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) &&
            // inputMask.get(i))
            thrLength = 0;
            tmpBuffer = new float[length];
            tmpSecondBuffer = new float[length];

            for (i = 0; i < length; i++) {

                if (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) && inputMask.get(i)) {
                    tmpBuffer[thrLength] = buffer[i];
                    tmpSecondBuffer[thrLength++] = secondBuffer[i];
                }
            }

            buffer = null;
            buffer = new float[thrLength];
            secondBuffer = null;
            secondBuffer = new float[thrLength];

            for (i = 0; i < thrLength; i++) {
                buffer[i] = tmpBuffer[i];
                secondBuffer[i] = tmpSecondBuffer[i];
            }

            tmpBuffer = null;
            tmpSecondBuffer = null;
            System.gc();

            // Use linear least squares to calculate the best line fit
            // for secondBuffer = a*firstBuffer + b.  Use an orthogonal
            // line fit where the distance measurements are orthgonal to
            // the proposed line.  This is an orthogonal regression as opposed
            // to a traditional regression of dependent y variable green on
            // independent red variable x which would give measurements in the
            // y direction.  Note that a traditional regression of red on green
            // would be yet another line giving measurements in the x direction.
            // Don't include any point i with buffer[i] < background1 and
            // secondBuffer[i] < background2 in the regression line.
            // Don't want the regression line to be affected by the dataless
            // black background of the picture.
            fireProgressStateChanged("Calculating least squares fit line");
            fireProgressStateChanged(25);
            sumx = 0.0;
            sumy = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {
                sumx += buffer[i];
                sumy += secondBuffer[i];
                count++;
            }

            averagex = sumx / count;
            averagey = sumy / count;
            sumx2 = 0.0;
            sumxy = 0.0;
            sumy2 = 0.0;

            for (i = 0; i < thrLength; i++) {
                diffx = buffer[i] - averagex;
                diffy = secondBuffer[i] - averagey;
                sumx2 += diffx * diffx;
                sumxy += diffx * diffy;
                sumy2 += diffy * diffy;
            }

            float aa;
            varx = sumx2 / (count - 1);
            vary = sumy2 / (count - 1);
            covarxy = sumxy / (count - 1);
            a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                             (2 * covarxy));
            aa = a * a;
            b = (float) (averagey - (a * averagex));
            mse = (float) (varx + (2 * a * covarxy) + (aa * vary)) / (1 + aa);

            // +-2 standard deviations on each side includes about 95%.
            halfWidth = (float) (2.0 * Math.sqrt(mse));

            Preferences.debug("First iteration\n", Preferences.DEBUG_ALGORITHM);

            if ((useRed) && (useGreen)) {
                Preferences.debug("green = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            } else if ((useRed) && (useBlue)) {
                Preferences.debug("blue = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            } else {
                Preferences.debug("blue = " + a + "*green" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);
            doAgain = false;

            for (iter = 0; (iter < 10) && doAgain; iter++) {
                float invAAp1 = 1 / (aa + 1);
                doAgain = false;
                sumx = 0.0;
                sumy = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {
                    distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) * invAAp1;

                    if (distance <= halfWidth) {
                        sumx += buffer[i];
                        sumy += secondBuffer[i];
                        count++;
                    }
                }

                averagex = sumx / count;
                averagey = sumy / count;
                sumx2 = 0.0;
                sumxy = 0.0;
                sumy2 = 0.0;

                for (i = 0; i < thrLength; i++) {
                    distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) * invAAp1;

                    if (distance <= halfWidth) {
                        diffx = buffer[i] - averagex;
                        diffy = secondBuffer[i] - averagey;
                        sumx2 += diffx * diffx;
                        sumxy += diffx * diffy;
                        sumy2 += diffy * diffy;
                    }
                }

                varx = sumx2 / (count - 1);
                vary = sumy2 / (count - 1);
                covarxy = sumxy / (count - 1);
                aLast = a;
                bLast = b;
                a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                 (2 * covarxy));
                aa = a * a;
                b = (float) (averagey - (a * averagex));
                mse = (float) (varx + (2 * a * covarxy) + (aa * vary)) / (1 + aa);

                // +-2 standard deviations on each side includes about 95%.
                halfWidth = (float) (2.0 * Math.sqrt(mse));

                Preferences.debug("Iteration = " + (iter + 2) + "\n", Preferences.DEBUG_ALGORITHM);

                if ((useRed) && (useGreen)) {
                    Preferences.debug("green = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                } else if ((useRed) && (useBlue)) {
                    Preferences.debug("blue = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                } else {
                    Preferences.debug("blue = " + a + "*green" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

                if ((aLast != 0.0f) && ((Math.abs(a - aLast) / aLast) > 0.001)) {
                    doAgain = true;
                }

                if ((bLast != 0.0f) && ((Math.abs(b - bLast) / bLast) > 0.001)) {
                    doAgain = true;
                }
            } // for (iter = 0; (iter < 10) && doAgain; iter++)

            fireProgressStateChanged("Generating histogram buffer");
            fireProgressStateChanged(50);

            for (i = 0; i < thrLength; i++) {
                ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                // invert y
                histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
            }

            fireProgressStateChanged("Generating VOI for least squares line");
            fireProgressStateChanged(60);
            xArray = new float[2];
            yArray = new float[2];
            zArray = new float[2];
            zArray[0] = 0.0f;
            zArray[1] = 0.0f;

            // secondBuffer[i] = a*buffer[i] + b
            // ch1 = (buffer[i] - min1)*scale1 + leftPad
            // ch2 = (secondBuffer[i] - min2)*scale2 + topPad;
            // ch1 = (scale1/a)*[((ch2-topPad)/scale2) + min2 - a*min1 - b]
            // + leftPad
            // ch2 = scale2*[((a*(ch1-leftPad))/scale1) + a*min1 - min2 + b]
            // + topPad
            ch2f = topPad;
            ch1f = (float) ((scale1 / a) * (min2 - (a * min1) - b)) + leftPad;

            if (ch1f < leftPad) {
                ch1f = leftPad;
                ch2f = (float) (scale2 * ((a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[0] = ch1f;
            yArray[0] = ch2f;
            ch2f = topPad + bin2 - 1;
            ch1f = (float) ((scale1 / a) * (((ch2f - topPad) / scale2) + min2 - (a * min1) - b)) + leftPad;

            if (ch1f > (bin1 - 1 + leftPad)) {
                ch1f = bin1 - 1 + leftPad;
                ch2f = (float) (scale2 * (((a * (bin1 - 1)) / scale1) + (a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[1] = ch1f;
            yArray[1] = ch2f;

            if (!doSecondIteration) {
                fitLineVOI = new VOI((short) 0, "fitLine", VOI.LINE, -1.0f);
                fitLineVOI.importCurve(xArray, yArray, zArray);
                destImage.registerVOI(fitLineVOI);
                fitLineVOI.setFixed(true);
                fitLineVOI.setColor(Color.orange);
            }

            if (threadStopped) { // do before copying back into image
                finalize();

                return;
            }

            destImage.importData(0, histBuffer, true);
            destRes = new float[2];
            destRes[0] = 1.0f;
            destRes[1] = 1.0f;
            destImage.getFileInfo(0).setResolutions(destRes);

        } // try
        catch (IOException error) {
            displayError("Algorithm ColocalizationRegression reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            secondBuffer = null;
            displayError("Algorithm ColocalizationRegression reports: out of memory" + e);
            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Generating thresholded linear correlations");
        fireProgressStateChanged(70);

        // secondBuffer[i] = a*buffer[i] + b;
        // buffer[i] = (secondBuffer[i] - b)/a;
        // Calculate (lineMin1,lineMin2) and (lineMax1,lineMax2),
        // the endpoints of the line segment
        lineMin1 = min1;
        lineMin2 = (a * min1) + b;

        if (lineMin2 < min2) {
            lineMin2 = min2;
            lineMin1 = (min2 - b) / a;
        }

        lineMax1 = max1;
        lineMax2 = (a * max1) + b;

        if (lineMax2 > max2) {
            lineMax2 = max2;
            lineMax1 = (max2 - b) / a;
        }

        firstNonPositive1 = -1;
        firstNonPositive2 = -1;
        minPositiveThreshold = -1;
        nonPositiveThreshold = 1;
        lastPositive1 = (float) (Math.floor(lineMax1) + 1);
        lastPositive2 = (float) (Math.floor(lineMax2) + 1);
        firstUndefined1 = -1;
        firstUndefined2 = -1;

        // Calculate the linear correlation coefficients for all pixels
        // whose values are either below threshold in buffer or are below
        // a*threshold + b in secondBuffer.  The values in buffer range from
        // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
        // along the color that has the greatest range along the line
        // segment.
        if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2)) {
            thresholdOn1 = true;
            rThreshold = null;
            rThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(lineMin1)) + 1];
            colocSize = null;
            colocSize = new float[rThreshold.length];
            colocIntensity1 = null;
            colocIntensity1 = new float[rThreshold.length];
            colocIntensity2 = null;
            colocIntensity2 = new float[rThreshold.length];
            haveThreshold = new boolean[rThreshold.length];

            for (i = 0; i < rThreshold.length; i++) {
                rThreshold[i] = Float.NaN;
                colocSize[i] = 0.0f;
                colocIntensity1[i] = 0.0f;
                colocIntensity2[i] = 0.0f;
                haveThreshold[i] = false;
            }

            transitionFound = false;
            i = rThreshold.length / 2;
            thresholdIncrement = rThreshold.length / 4;

            while (!transitionFound) {
                threshold = (float) (i + Math.ceil(lineMin1));
                secondThreshold = (a * threshold) + b;
                averagex = 0.0;
                averagey = 0.0;
                count = 0;
                morex = false;
                morey = false;
                lastx = buffer[0];
                lasty = secondBuffer[0];
                voiIntensity1Thr = 0.0f;
                voiIntensity2Thr = 0.0f;

                for (j = 0; j < thrLength; j++) {
                    float buff = buffer[j];
                    float secBuff = secondBuffer[j];

                    if (buff >= threshold) {
                        voiIntensity1Thr += buff;
                    }

                    if (secBuff >= secondThreshold) {
                        voiIntensity2Thr += secBuff;
                    }

                    if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                        colocSize[i] += 1.0f;
                        colocIntensity1[i] += buff;
                        colocIntensity2[i] += secBuff;
                    } else {
                        averagex += buff;
                        averagey += secBuff;
                        count++;

                        if (count == 1) {
                            lastx = buff;
                            lasty = secBuff;
                        } else if (count >= 2) {

                            if (!morex) {

                                if (buff != lastx) {
                                    morex = true;
                                }
                            }

                            if (!morey) {

                                if (secBuff != lasty) {
                                    morey = true;
                                }
                            }
                        } // else if (count >= 2)
                    }
                } // for (j = 0; j < thrLength; j++)

                colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                if (doColocWithThresholds) {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                } else {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                }

                if (morex && morey) {
                    averagex /= count;
                    averagey /= count;
                    num = 0.0;
                    denom1 = 0.0;
                    denom2 = 0.0;

                    for (j = 0; j < thrLength; j++) {
                        float buff = buffer[j];
                        float secBuff = secondBuffer[j];

                        if ((buff < threshold) || (secBuff < secondThreshold)) {
                            num += (buff - averagex) * (secBuff - averagey);
                            denom1 += (buff - averagex) * (buff - averagex);
                            denom2 += (secBuff - averagey) * (secBuff - averagey);
                        }
                    } // for (j = 0; j < thrLength; j++)

                    rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                    haveThreshold[i] = true;
                    Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", Preferences.DEBUG_ALGORITHM);

                    if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i - 1];
                        firstNonPositive1 = (float) ((i - 1) + Math.ceil(lineMin1));
                        firstNonPositive2 = (a * firstNonPositive1) + b;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                   Float.isNaN(rThreshold[i - 1])) {
                        transitionFound = true;
                        firstUndefined1 = (float) ((i - 1) + Math.ceil(lineMin1));
                        firstUndefined2 = (a * firstUndefined1) + b;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                        transitionFound = true;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i];
                        firstNonPositive1 = (float) (i + Math.ceil(lineMin1));
                        firstNonPositive2 = (a * firstNonPositive1) + b;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && Float.isNaN(rThreshold[i]) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        firstUndefined1 = (float) (i + Math.ceil(lineMin1));
                        firstUndefined2 = (a * firstUndefined1) + b;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if (rThreshold[i] > 0.0f) {
                        i = i - thresholdIncrement;

                        if (i < 0) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // else if (rThreshold[i] > 0.0f)
                    else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    }
                } // if (morex && morey)
                else { // Float.isNaN(rThreshold[i])
                    haveThreshold[i] = true;
                    i = i + thresholdIncrement;

                    if (i >= rThreshold.length) {
                        transitionFound = true;
                    }

                    thresholdIncrement = thresholdIncrement / 2;

                    if (thresholdIncrement == 0) {
                        thresholdIncrement = 1;
                    }
                } // Float.isNaN(rThreshold[i])
            } // while (!transitionFound)

        } // if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2))
        else { // ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)
            thresholdOn1 = false;
            rThreshold = null;
            rThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(lineMin2)) + 1];
            colocSize = null;
            colocSize = new float[rThreshold.length];

            colocIntensity1 = null;
            colocIntensity1 = new float[rThreshold.length];
            colocIntensity2 = null;
            colocIntensity2 = new float[rThreshold.length];
            haveThreshold = new boolean[rThreshold.length];

            for (i = 0; i < rThreshold.length; i++) {
                rThreshold[i] = Float.NaN;
                colocSize[i] = 0.0f;
                colocIntensity1[i] = 0.0f;
                colocIntensity2[i] = 0.0f;
                haveThreshold[i] = false;
            }

            transitionFound = false;
            i = rThreshold.length / 2;
            thresholdIncrement = rThreshold.length / 4;

            while (!transitionFound) {
                secondThreshold = (float) (i + Math.ceil(lineMin2));
                threshold = (secondThreshold - b) / a;
                averagex = 0.0;
                averagey = 0.0;
                count = 0;
                morex = false;
                morey = false;
                lastx = buffer[0];
                lasty = secondBuffer[0];
                voiIntensity1Thr = 0.0f;
                voiIntensity2Thr = 0.0f;

                for (j = 0; j < thrLength; j++) {
                    float buff = buffer[j];
                    float secBuff = secondBuffer[j];

                    if (buff >= threshold) {
                        voiIntensity1Thr += buff;
                    }

                    if (secBuff >= secondThreshold) {
                        voiIntensity2Thr += secBuff;
                    }

                    if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                        colocSize[i] += 1.0f;
                        colocIntensity1[i] += buff;
                        colocIntensity2[i] += secBuff;
                    } else {
                        averagex += buff;
                        averagey += secBuff;
                        count++;

                        if (count == 1) {
                            lastx = buff;
                            lasty = secBuff;
                        } else if (count >= 2) {

                            if (!morex) {

                                if (buff != lastx) {
                                    morex = true;
                                }
                            }

                            if (!morey) {

                                if (secBuff != lasty) {
                                    morey = true;
                                }
                            }
                        } // else if (count >= 2)
                    }
                } // for (j = 0; j < thrLength; j++)

                colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                if (doColocWithThresholds) {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                } else {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                }

                if (morex && morey) {
                    averagex /= count;
                    averagey /= count;
                    num = 0.0;
                    denom1 = 0.0;
                    denom2 = 0.0;

                    for (j = 0; j < thrLength; j++) {
                        float buff = buffer[j];
                        float secBuff = secondBuffer[j];

                        if ((buff < threshold) || (secBuff < secondThreshold)) {
                            num += (buff - averagex) * (secBuff - averagey);
                            denom1 += (buff - averagex) * (buff - averagex);
                            denom2 += (secBuff - averagey) * (secBuff - averagey);
                        }
                    } // for (j = 0; j < thrLength; j++)

                    rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                    haveThreshold[i] = true;
                    Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", Preferences.DEBUG_ALGORITHM);

                    if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i - 1];
                        firstNonPositive2 = (float) ((i - 1) + Math.ceil(lineMin2));
                        firstNonPositive1 = (firstNonPositive2 - b) / a;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                   Float.isNaN(rThreshold[i - 1])) {
                        transitionFound = true;
                        firstUndefined2 = (float) ((i - 1) + Math.ceil(lineMin2));
                        firstUndefined1 = (firstUndefined2 - b) / a;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                        transitionFound = true;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i];
                        firstNonPositive2 = (float) (i + Math.ceil(lineMin2));
                        firstNonPositive1 = (firstNonPositive2 - b) / a;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && Float.isNaN(rThreshold[i]) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        firstUndefined2 = (float) (i + Math.ceil(lineMin2));
                        firstUndefined1 = (firstUndefined2 - b) / a;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if (rThreshold[i] > 0.0f) {
                        i = i - thresholdIncrement;

                        if (i < 0) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // else if (rThreshold[i] > 0.0f)
                    else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    }
                } // if (morex && morey)
                else { // Float.isNaN(rThreshold[i])
                    haveThreshold[i] = true;
                    i = i + thresholdIncrement;

                    if (i >= rThreshold.length) {
                        transitionFound = true;
                    }

                    thresholdIncrement = thresholdIncrement / 2;

                    if (thresholdIncrement == 0) {
                        thresholdIncrement = 1;
                    }
                } // Float.isNaN(rThreshold[i])
            } // while (!transitionFound)

        } // else ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)

        if ((useRed) && (useGreen)) {

            if (firstUndefined1 >= 0) {
                Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + firstUndefined1 + " or green < " + firstUndefined2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            }

            if (nonPositiveThreshold <= 0) {
                Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + firstNonPositive1 + " or green < " + firstNonPositive2 +
                                  "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with red < " + lastPositive1 + " or green < " + lastPositive2 + "\n", 
            		Preferences.DEBUG_ALGORITHM);

        } // if ((useRed) && (useGreen))
        else if ((useRed) && (useBlue)) {

            if (firstUndefined1 >= 0) {
                Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + firstUndefined1 + " or blue < " + firstUndefined2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            }

            if (nonPositiveThreshold <= 0) {
                Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + firstNonPositive1 + " or blue < " + firstNonPositive2 +
                                  "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with red < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
            		Preferences.DEBUG_ALGORITHM);
        } // else if ((useRed) && (useBlue))
        else {

            if (firstUndefined1 >= 0) {
                Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with green < " + firstUndefined1 + " or blue < " + firstUndefined2 +
                                  "\n", Preferences.DEBUG_ALGORITHM);
            }

            if (nonPositiveThreshold <= 0) {
                Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with green < " + firstNonPositive1 + " or blue < " + firstNonPositive2 +
                                  "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with green < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
            		Preferences.DEBUG_ALGORITHM);
        }

        if (minPositiveThreshold < 0) {
            removeVOIUpdateListener();
            subtractedSrcImage.clearMask();
            subtractedSrcImage.notifyImageDisplayListeners();
            MipavUtil.displayError("No positive threshold found on first iteration");

            setCompleted(false);

            return;
        }

        if (doSecondIteration) {
            Preferences.debug("Second iteration excluding subthresholded region\n", Preferences.DEBUG_ALGORITHM);
            fireProgressStateChanged("Second iteration excluding subthresholded region");
            fireProgressStateChanged(80);
            t1 = lastPositive1;
            t2 = lastPositive2;

            averagex = 0.0;
            averagey = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    averagex += buffer[i];
                    averagey += secondBuffer[i];
                    count++;
                }
            }

            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    num += (buffer[i] - averagex) * (secondBuffer[i] - averagey);
                    denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                    denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                }
            }

            denom = Math.sqrt(denom1 * denom2);
            r = (float) (num / denom);
            Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);

            // Use linear least squares to calculate the best line fit
            // for secondBuffer = a*firstBuffer + b.  Use an orthogonal
            // line fit where the distance measurements are orthgonal to
            // the proposed line.  This is an orthogonal regression as opposed
            // to a traditional regression of dependent y variable green on
            // independent red variable x which would give measurements in the
            // y direction.  Note that a traditional regression of red on green
            // would be yet another line giving measurements in the x direction.
            // Don't include any point i with buffer[i] < lastPositive1 or
            // secondBuffer[i] < lastPositive2 in the regression line.
            // This excludes the L shaped subthresholded region from the
            // first iteration.
            sumx = 0.0;
            sumy = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    sumx += buffer[i];
                    sumy += secondBuffer[i];
                    count++;
                }
            }

            averagex = sumx / count;
            averagey = sumy / count;
            sumx2 = 0.0;
            sumxy = 0.0;
            sumy2 = 0.0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    diffx = buffer[i] - averagex;
                    diffy = secondBuffer[i] - averagey;
                    sumx2 += diffx * diffx;
                    sumxy += diffx * diffy;
                    sumy2 += diffy * diffy;
                }
            }


            varx = sumx2 / (count - 1);
            vary = sumy2 / (count - 1);
            covarxy = sumxy / (count - 1);
            a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                             (2 * covarxy));
            b = (float) (averagey - (a * averagex));
            mse = (float) (varx + (2 * a * covarxy) + (a * a * vary)) / (1 + (a * a));

            // +-2 standard deviations on each side includes about 95%.
            halfWidth = (float) (2.0 * Math.sqrt(mse));

            if ((useRed) && (useGreen)) {
                Preferences.debug("green = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            } else if ((useRed) && (useBlue)) {
                Preferences.debug("blue = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            } else {
                Preferences.debug("blue = " + a + "*green" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);
            doAgain = false;

            for (iter = 0; (iter < 10) && doAgain; iter++) {
                doAgain = false;
                sumx = 0.0;
                sumy = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {

                    if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                        distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) / ((a * a) + 1);

                        if (distance <= halfWidth) {
                            sumx += buffer[i];
                            sumy += secondBuffer[i];
                            count++;
                        }
                    }
                }

                averagex = sumx / count;
                averagey = sumy / count;
                sumx2 = 0.0;
                sumxy = 0.0;
                sumy2 = 0.0;

                for (i = 0; i < thrLength; i++) {

                    if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                        distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) / ((a * a) + 1);

                        if (distance <= halfWidth) {
                            diffx = buffer[i] - averagex;
                            diffy = secondBuffer[i] - averagey;
                            sumx2 += diffx * diffx;
                            sumxy += diffx * diffy;
                            sumy2 += diffy * diffy;
                        }
                    }
                }

                varx = sumx2 / (count - 1);
                vary = sumy2 / (count - 1);
                covarxy = sumxy / (count - 1);
                aLast = a;
                bLast = b;
                a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                 (2 * covarxy));
                b = (float) (averagey - (a * averagex));
                mse = (float) (varx + (2 * a * covarxy) + (a * a * vary)) / (1 + (a * a));

                // +-2 standard deviations on each side includes about 95%.
                halfWidth = (float) (2.0 * Math.sqrt(mse));

                Preferences.debug("Iteration = " + (iter + 2) + "\n", Preferences.DEBUG_ALGORITHM);

                if ((useRed) && (useGreen)) {
                    Preferences.debug("green = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                } else if ((useRed) && (useBlue)) {
                    Preferences.debug("blue = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                } else {
                    Preferences.debug("blue = " + a + "*green" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

                if ((aLast != 0.0f) && ((Math.abs(a - aLast) / aLast) > 0.001)) {
                    doAgain = true;
                }

                if ((bLast != 0.0f) && ((Math.abs(b - bLast) / bLast) > 0.001)) {
                    doAgain = true;
                }
            } // for (iter = 0; (iter < 10) && doAgain; iter++)

            // secondBuffer[i] = a*buffer[i] + b
            // ch1 = (buffer[i] - min1)*scale1 + leftPad
            // ch2 = (secondBuffer[i] - min2)*scale2 + topPad;
            // ch1 = (scale1/a)*[((ch2-topPad)/scale2) + min2 - a*min1 - b]
            // + leftPad
            // ch2 = scale2*[((a*(ch1-leftPad))/scale1) + a*min1 - min2 + b]
            // + topPad
            ch2f = topPad;
            ch1f = (float) ((scale1 / a) * (min2 - (a * min1) - b)) + leftPad;

            if (ch1f < leftPad) {
                ch1f = leftPad;
                ch2f = (float) (scale2 * ((a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[0] = ch1f;
            yArray[0] = ch2f;
            ch2f = topPad + bin2 - 1;
            ch1f = (float) ((scale1 / a) * (((ch2f - topPad) / scale2) + min2 - (a * min1) - b)) + leftPad;

            if (ch1f > (bin1 - 1 + leftPad)) {
                ch1f = bin1 - 1 + leftPad;
                ch2f = (float) (scale2 * (((a * (bin1 - 1)) / scale1) + (a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[1] = ch1f;
            yArray[1] = ch2f;
            fitLineVOI = new VOI((short) 0, "fitLine", VOI.LINE, -1.0f);
            fitLineVOI.importCurve(xArray, yArray, zArray);
            destImage.registerVOI(fitLineVOI);
            fitLineVOI.setFixed(true);
            fitLineVOI.setColor(Color.orange);

            // secondBuffer[i] = a*buffer[i] + b;
            // buffer[i] = (secondBuffer[i] - b)/a;
            // Calculate (lineMin1,lineMin2) and (lineMax1,lineMax2),
            // the endpoints of the line segment
            lineMin1 = min1;
            lineMin2 = (a * min1) + b;

            if (lineMin2 < min2) {
                lineMin2 = min2;
                lineMin1 = (min2 - b) / a;
            }

            lineMax1 = max1;
            lineMax2 = (a * max1) + b;

            if (lineMax2 > max2) {
                lineMax2 = max2;
                lineMax1 = (max2 - b) / a;
            }

            firstNonPositive1 = -1;
            firstNonPositive2 = -1;
            minPositiveThreshold = -1;
            nonPositiveThreshold = 1;
            lastPositive1 = (float) (Math.floor(lineMax1) + 1);
            lastPositive2 = (float) (Math.floor(lineMax2) + 1);
            firstUndefined1 = -1;
            firstUndefined2 = -1;

            // Calculate the linear correlation coefficients for all pixels
            // whose values are either below threshold in buffer or are below
            // a*threshold + b in secondBuffer.  The values in buffer range from
            // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
            // along the color that has the greatest range along the line
            // segment.
            if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2)) {
                thresholdOn1 = true;
                rThreshold = null;
                rThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(lineMin1)) + 1];
                colocSize = null;
                colocSize = new float[rThreshold.length];
                colocIntensity1 = null;
                colocIntensity1 = new float[rThreshold.length];
                colocIntensity2 = null;
                colocIntensity2 = new float[rThreshold.length];
                haveThreshold = new boolean[rThreshold.length];

                for (i = 0; i < rThreshold.length; i++) {
                    rThreshold[i] = Float.NaN;
                    colocSize[i] = 0.0f;
                    colocIntensity1[i] = 0.0f;
                    colocIntensity2[i] = 0.0f;
                    haveThreshold[i] = false;
                }

                transitionFound = false;
                i = rThreshold.length / 2;
                thresholdIncrement = rThreshold.length / 4;

                while (!transitionFound) {
                    threshold = (float) (i + Math.ceil(lineMin1));
                    secondThreshold = (a * threshold) + b;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = buffer[0];
                    lasty = secondBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {

                        if (buffer[j] >= threshold) {
                            voiIntensity1Thr += buffer[j];
                        }

                        if (secondBuffer[j] >= secondThreshold) {
                            voiIntensity2Thr += secondBuffer[j];
                        }

                        if ((buffer[j] >= threshold) && (secondBuffer[j] >= secondThreshold)) {
                            colocSize[i] += 1.0f;
                            colocIntensity1[i] += buffer[j];
                            colocIntensity2[i] += secondBuffer[j];
                        } else {
                            averagex += buffer[j];
                            averagey += secondBuffer[j];
                            count++;

                            if (count == 1) {
                                lastx = buffer[j];
                                lasty = secondBuffer[j];
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buffer[j] != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secondBuffer[j] != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                    if (doColocWithThresholds) {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {

                            if ((buffer[j] < threshold) || (secondBuffer[j] < secondThreshold)) {
                                num += (buffer[j] - averagex) * (secondBuffer[j] - averagey);
                                denom1 += (buffer[j] - averagex) * (buffer[j] - averagex);
                                denom2 += (secondBuffer[j] - averagey) * (secondBuffer[j] - averagey);
                            }
                        }

                        rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        haveThreshold[i] = true;
                        Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i - 1];
                            firstNonPositive1 = (float) ((i - 1) + Math.ceil(lineMin1));
                            firstNonPositive2 = (a * firstNonPositive1) + b;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                       Float.isNaN(rThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined1 = (float) ((i - 1) + Math.ceil(lineMin1));
                            firstUndefined2 = (a * firstUndefined1) + b;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                       (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i];
                            firstNonPositive1 = (float) (i + Math.ceil(lineMin1));
                            firstNonPositive2 = (a * firstNonPositive1) + b;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] &&
                                       Float.isNaN(rThreshold[i]) && (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined1 = (float) (i + Math.ceil(lineMin1));
                            firstUndefined2 = (a * firstUndefined1) + b;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if (rThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (rThreshold[i] > 0.0f)
                        else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= rThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(rThreshold[i])
                        haveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(rThreshold[i])
                } // while (!transitionFound)

            } // if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2))
            else { // ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)
                thresholdOn1 = false;
                rThreshold = null;
                rThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(lineMin2)) + 1];
                colocSize = null;
                colocSize = new float[rThreshold.length];
                colocIntensity1 = null;
                colocIntensity1 = new float[rThreshold.length];
                colocIntensity2 = null;
                colocIntensity2 = new float[rThreshold.length];

                for (i = 0; i < rThreshold.length; i++) {
                    rThreshold[i] = Float.NaN;
                    colocSize[i] = 0.0f;
                    colocIntensity1[i] = 0.0f;
                    colocIntensity2[i] = 0.0f;
                    haveThreshold[i] = false;
                }

                transitionFound = false;
                i = rThreshold.length / 2;
                thresholdIncrement = rThreshold.length / 4;

                while (!transitionFound) {
                    secondThreshold = (float) (i + Math.ceil(lineMin2));
                    threshold = (secondThreshold - b) / a;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = buffer[0];
                    lasty = secondBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {

                        if (buffer[j] >= threshold) {
                            voiIntensity1Thr += buffer[j];
                        }

                        if (secondBuffer[j] >= secondThreshold) {
                            voiIntensity2Thr += secondBuffer[j];
                        }

                        if ((buffer[j] >= threshold) && (secondBuffer[j] >= secondThreshold)) {
                            colocSize[i] += 1.0f;
                            colocIntensity1[i] += buffer[j];
                            colocIntensity2[i] += secondBuffer[j];
                        } else {
                            averagex += buffer[j];
                            averagey += secondBuffer[j];
                            count++;

                            if (count == 1) {
                                lastx = buffer[j];
                                lasty = secondBuffer[j];
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buffer[j] != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secondBuffer[j] != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                    if (doColocWithThresholds) {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {

                            if ((buffer[j] < threshold) || (secondBuffer[j] < secondThreshold)) {
                                num += (buffer[j] - averagex) * (secondBuffer[j] - averagey);
                                denom1 += (buffer[j] - averagex) * (buffer[j] - averagex);
                                denom2 += (secondBuffer[j] - averagey) * (secondBuffer[j] - averagey);
                            }
                        }

                        rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        haveThreshold[i] = true;
                        Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i - 1];
                            firstNonPositive2 = (float) ((i - 1) + Math.ceil(lineMin2));
                            firstNonPositive1 = (firstNonPositive2 - b) / a;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                       Float.isNaN(rThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined2 = (float) ((i - 1) + Math.ceil(lineMin2));
                            firstUndefined1 = (firstUndefined2 - b) / a;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                       (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i];
                            firstNonPositive2 = (float) (i + Math.ceil(lineMin2));
                            firstNonPositive1 = (firstNonPositive2 - b) / a;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] &&
                                       Float.isNaN(rThreshold[i]) && (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined2 = (float) (i + Math.ceil(lineMin2));
                            firstUndefined1 = (firstUndefined2 - b) / a;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if (rThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (rThreshold[i] > 0.0f)
                        else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= rThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(rThreshold[i])
                        haveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(rThreshold[i])
                } // while (!transitionFound)

            } // else ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)

            if ((useRed) && (useGreen)) {

                if (firstUndefined1 >= 0) {
                    Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstUndefined1 + " or green < " + firstUndefined2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (nonPositiveThreshold <= 0) {
                    Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstNonPositive1 + " or green < " +
                                      firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + lastPositive1 + " or green < " + lastPositive2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);

            } // if ((useRed) && (useGreen))
            else if ((useRed) && (useBlue)) {

                if (firstUndefined1 >= 0) {
                    Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstUndefined1 + " or blue < " + firstUndefined2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (nonPositiveThreshold <= 0) {
                    Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstNonPositive1 + " or blue < " + firstNonPositive2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            } // else if ((useRed) && (useBlue))
            else {

                if (firstUndefined1 >= 0) {
                    Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with green < " + firstUndefined1 + " or blue < " + firstUndefined2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (nonPositiveThreshold <= 0) {
                    Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with green < " + firstNonPositive1 + " or blue < " +
                                      firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with green < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            }

            if (minPositiveThreshold < 0) {
                removeVOIUpdateListener();
                MipavUtil.displayError("No positve threshold found on second iteration");

                setCompleted(false);

                return;
            }
        } // if (doSecondIteration)

        frameList = srcImage.getImageFrameVector();

        for (i = 0; i < frameList.size(); i++) {

            if ((((ViewJFrameBase) frameList.elementAt(i)).getControls()) != null) {
                controlFrame = ((ViewJFrameBase) frameList.elementAt(i));
            }
        }

        fireProgressStateChanged("Creating point VOI");
        fireProgressStateChanged(85);

        // position the point at the minimum positive threshold
        xArray = new float[1];
        yArray = new float[1];
        zArray = new float[1];

        // Allow blank spaces
        if (pointCalculation) {
            xArray[0] = (float) ((point1 - min1) * scale1) + leftPad;
            yArray[0] = (float) (bin2 - 1.0f - ((point2 - min2) * scale2)) + topPad;
        } else {
            xArray[0] = (float) ((lastPositive1 - min1) * scale1) + leftPad;
            yArray[0] = (float) (bin2 - 1.0f - ((lastPositive2 - min2) * scale2)) + topPad;
        }

        zArray[0] = 0.0f;
        pointVOI = new VOI((short) 1, "thresholdPoint", VOI.POINT, -1.0f);
        pointVOI.importCurve(xArray, yArray, zArray);
        destImage.registerVOI(pointVOI);
        pointVOI.setFixed(false);

        // Don't draw point - draw square with blue inside and yellow edge
        pointVOI.setColor(Color.green);

        /*UI.setDataText("Linear correlation coefficient = " + r + "\n");
         * if ((useRed) && (useGreen)) { UI.setDataText("green = " + a + "*red" + " + " + b + "\n"); } else if ((useRed)
         * && (useBlue)) { UI.setDataText("blue = " + a + "*red" + " + " + b + "\n"); } else { UI.setDataText("blue = "
         * + a + "*green" + " + " + b + "\n"); } UI.setDataText("Mean square error = " + mse + "\n"); if ((useRed) &&
         * (useGreen)) { if (firstUndefined1 >= 0) { UI.setDataText("Cannot calculate linear correlation
         * coefficient\n"); UI.setDataText("for pixels with red < " + firstUndefined1 + " or green < " + firstUndefined2
         * +"\n"); } if (nonPositiveThreshold <= 0) { UI.setDataText("Nonpositive linear correlation coefficient = " +
         * nonPositiveThreshold + "\n"); UI.setDataText("for pixels with red < " + firstNonPositive1 + " or green < " +
         * firstNonPositive2 + "\n"); } UI.setDataText("Linear correlation coefficient = " + minPositiveThreshold +
         * "\n"); UI.setDataText("for pixels with red < " + lastPositive1 + " or green < " + lastPositive2 + "\n"); } //
         * if ((useRed) && (useGreen)) else if ((useRed) && (useBlue)) { if (firstUndefined1 >= 0) {
         * UI.setDataText("Cannot calculate linear correlation coefficient\n"); UI.setDataText("for pixels with red < "
         * + firstUndefined1 + " or blue < " + firstUndefined2 +"\n"); } if (nonPositiveThreshold <= 0) {
         * UI.setDataText("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n");
         * UI.setDataText("for pixels with red < " + firstNonPositive1 + " or blue < " + firstNonPositive2 + "\n"); }
         * UI.setDataText("Linear correlation coefficient = " + minPositiveThreshold + "\n"); UI.setDataText("for pixels
         * with red < " + lastPositive1 + " or blue < " + lastPositive2 + "\n"); } // else if ((useRed) && (useBlue))
         * else { if (firstUndefined1 >= 0) { UI.setDataText("Cannot calculate linear correlation coefficient\n");
         * UI.setDataText("for pixels with green < " + firstUndefined1 + " or blue < " + firstUndefined2 +"\n"); } if
         * (nonPositiveThreshold <= 0) { UI.setDataText("Nonpositive linear correlation coefficient = " +
         * nonPositiveThreshold + "\n"); UI.setDataText("for pixels with green < " + firstNonPositive1 + " or blue < " +
         * firstNonPositive2 + "\n"); } UI.setDataText("Linear correlation coefficient = " + minPositiveThreshold +
         * "\n"); UI.setDataText("for pixels with green < " + lastPositive1 + " or blue < " + lastPositive2 + "\n"); }*/

        fireProgressStateChanged("Creating ColocalizationRegression frame");
        fireProgressStateChanged(90);

        if (redo) {
            haveFreeRangeThreshold = null;
            freeRangeRThreshold = null;
            freeRangeColocSize = null;
            freeRangeColocIntensity1 = null;
            freeRangeColocIntensity2 = null;
            frameColocalize.setNewVar(a, b, r, PValue, haveThreshold, rThreshold, colocSize, colocIntensity1,
                                      colocIntensity2, min1, max1, min2, max2, scale1, scale2, lineMin1, lineMax1,
                                      lineMin2, lineMax2, thresholdOn1);
        } else {

            // System.err.println("Constr3");
            frameColocalize = new ViewJFrameColocalizationRegression(this, subtractedSrcImage, null, baseImage, null,
                                                                     null, destImage, controlFrame, useRed, useGreen,
                                                                     useBlue, a, b, r, PValue, haveThreshold,
                                                                     rThreshold, colocSize, colocIntensity1,
                                                                     colocIntensity2, min1, max1, min2, max2, scale1,
                                                                     scale2, lineMin1, lineMax1, lineMin2, lineMax2,
                                                                     thresholdOn1, leftPad, rightPad, bottomPad, topPad,
                                                                     doSecondIteration, pointCalculation);

            if (pointCalculation) {
                frameColocalize.pointCalculate();
            }
        }

        fireProgressStateChanged(100);


        setCompleted(true);
    }

    /**
     * This function produces a 2D histogram image with first color values represented across the x axis and second
     * color values represented across the y axis. The total least squares line and a matrix of correlation coefficients
     * for L shaped subthreshold regions are also generated.
     */
    private void calcColor3D() {
        int i;
        int pos;
        float[] randomBuffer;
        double[] histBuffer;
        int[] extents2D;
        ModelImage refImage;
        ModelImage inputImage;
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
        boolean transformVOI = false;
        boolean clip = true;
        int c;
        TransMatrix xfrm;
        double max1, max2, range1, range2;
        double averagex, averagey;
        double diffx, diffy;
        double varx, vary, covarxy;
        float mse; // mean square error
        float halfWidth;
        float distance;
        int iter;
        boolean doAgain;
        float aLast;
        float bLast;
        boolean morex, morey;
        float lastx, lasty;

        int ch1, ch2;
        float ch1f, ch2f;
        double lineMax1, lineMax2;
        float[] xArray;
        float[] yArray;
        float[] zArray;
        VOI fitLineVOI;
        VOI pointVOI;
        double num, denom1, denom2, denom;
        String nameR, nameG, nameB;
        ModelImage resultImageR = null;
        ModelImage resultImageG = null;
        ModelImage resultImageB = null;
        AlgorithmAutoCovariance algoAutoCovariance;
        int fwhmR = Integer.MAX_VALUE;
        int fwhmG = Integer.MAX_VALUE;
        int fwhmB = Integer.MAX_VALUE;
        int fwhm = Integer.MAX_VALUE;
        boolean scrambleFirst;
        int xDim, yDim, zDim;
        int length;
        int volume;
        int zPos;
        int totPos;
        int x, y, z;
        int numCubes;
        Vector<Integer> vectRand;
        RandomNumberGen randomGen;
        int randNum;
        Integer randCube;
        int sliceCubes;
        int s, j, k, m;
        int sx, sy, sz;
        int numXValues, numYValues, numZValues;
        float[] rMat = new float[200];
        int numBins;
        float[] rCum;
        float[] pStatMat;
        boolean found;
        int xRemainder = 0, yRemainder = 0, zRemainder = 0;
        int xStart = 0;
        int yStart = 0;
        int zStart = 0;
        int xEnd, yEnd, zEnd;
        double sumx, sumx2;
        double sumy, sumy2;
        double sumxy;
        float threshold;
        float secondThreshold;
        int count;
        float nonPositiveThreshold;
        float minPositiveThreshold;
        float lastPositive1, lastPositive2;
        float firstNonPositive1, firstNonPositive2;
        float firstUndefined1, firstUndefined2;
        Vector<ViewImageUpdateInterface> frameList;
        ViewJFrameBase controlFrame = null;
        float[] destRes;
        boolean transitionFound;
        int thresholdIncrement;
        float[] tmpBuffer;
        float[] tmpSecondBuffer;
        float voiIntensity1Thr;
        float voiIntensity2Thr;
        float backC1 = 0.0f;
        float backC2 = 0.0f;
        float backC3 = 0.0f;

        try {

            fireProgressStateChanged(srcImage.getImageName(), "Creating 2D Colocolization Histogram ...");

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

            if (useRed && useGreen) {
                thirdColor = 3;
            } else if (useRed && useBlue) {
                thirdColor = 2;
            } else {
                thirdColor = 1;
            }

            if ((backgroundIndex != -1) && (!redo)) {
                fireProgressStateChanged("Calculating backround average");
                fireProgressStateChanged(5);
                buffer = null;
                buffer = new float[length];
                secondBuffer = null;
                secondBuffer = new float[length];

                for (j = 0; j < srcImage.getExtents()[2]; j++) {

                    try {
                        srcImage.exportRGBData(color, 4 * j * length, length, buffer);
                        srcImage.exportRGBData(secondColor, 4 * j * length, length, secondBuffer);
                    } catch (IOException e) {
                        buffer = null;
                        secondBuffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                        return;
                    }

                    for (i = 0; i < length; i++) {
                        backC1 += buffer[i];
                        backC2 += secondBuffer[i];
                    }
                } // for (j = 0; j < srcImage.getExtents()[2]; j++)

                backC1 /= volume;
                backC2 /= volume;

                for (j = 0; j < srcImage.getExtents()[2]; j++) {

                    try {
                        srcImage.exportRGBData(thirdColor, 4 * j * length, length, buffer);
                    } catch (IOException e) {
                        buffer = null;
                        secondBuffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                        return;
                    }

                    for (i = 0; i < length; i++) {
                        backC3 += buffer[i];
                    }
                } // for (j = 0; j < srcImage.getExtents()[2]; j++)

                backC3 /= volume;
            } // if ((backgroundIndex != -1) && (!redo))

            if ((register) && (!redo)) {
                fireProgressStateChanged("Registering colors");
                fireProgressStateChanged(10);
                extents2D = new int[2];
                extents2D[0] = srcImage.getExtents()[0];
                extents2D[1] = srcImage.getExtents()[1];

                float xresA = srcImage.getFileInfo(0).getResolutions()[0];
                float yresA = srcImage.getFileInfo(0).getResolutions()[1];

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
                    fireProgressStateChanged("Registering slice = " + (z + 1));

                    zPos = z * length;

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

                    reg2 = new AlgorithmRegOAR2D(refImage, inputImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                                 coarseRate, fineRate, doSubsample, doMultiThread);
                    reg2.run();
                    xfrm = reg2.getTransform();
                    reg2.disposeLocal();
                    reg2 = null;
                    
                    xfrm.Set(0, 1, xfrm.Get(0, 1) * 0.5f);
                    xfrm.Set(0, 2, xfrm.Get(0, 2) * 0.5f);
                    xfrm.Set(1, 0, xfrm.Get(1, 0) * 0.5f);
                    xfrm.Set(1, 2, xfrm.Get(1, 2) * 0.5f);
                           
                    transform = new AlgorithmTransform(inputImage, xfrm, AlgorithmTransform.BILINEAR, xresA, yresA,
                                                       xDim, yDim, transformVOI, clip, false);
                    transform.run();
                    registeredImageY = transform.getTransformedImage();
                    transform.disposeLocal();
                    transform = null;

                    reg2 = new AlgorithmRegOAR2D(inputImage, refImage, cost, DOF, interp, coarseBegin, coarseEnd,
                                                 coarseRate, fineRate, doSubsample, doMultiThread);
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
                                        registeredSrcImage.setUByte((4 * totPos) + c,
                                                                    srcImage.getUByte((4 * totPos) + c));
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
                } // for (z = 0; z < zDim; z++) End of loop through the image!!!!

                updateFileInfo(srcImage, registeredSrcImage);
                registeredSrcImage.calcMinMax();
                registeredSrcImage.setVOIs(srcImage.getVOIs());
                srcImage.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                srcImage.clearMask();
                srcImage.notifyImageDisplayListeners();

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
            } // if ((register) && (!redo))
            else if (!redo) {
                registeredSrcImage = srcImage;
            } // else if (!redo)

            if ((backgroundIndex != -1) && (!redo)) {
                fireProgressStateChanged("Subtracting background from " + srcImage.getImageName());
                fireProgressStateChanged(15);
                subtractedSrcImage = new ModelImage(srcImage.getType(), srcImage.getExtents(),
                                                    registeredSrcImage.getImageName() + "_subtract");
                buffer = null;
                buffer = new float[length];

                for (j = 0; j < srcImage.getExtents()[2]; j++) {

                    try {
                        registeredSrcImage.exportRGBData(color, 4 * j * length, length, buffer);
                    } catch (IOException e) {
                        buffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                        return;
                    }

                    for (i = 0; i < length; i++) {
                        buffer[i] -= backC1;

                        if (buffer[i] < 0.0f) {
                            buffer[i] = 0.0f;
                        }
                    }

                    try {
                        subtractedSrcImage.importRGBData(color, 4 * j * length, buffer, false);
                    } catch (IOException e) {
                        buffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                        return;
                    }

                    try {
                        registeredSrcImage.exportRGBData(secondColor, 4 * j * length, length, buffer);
                    } catch (IOException e) {
                        buffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                        return;
                    }

                    for (i = 0; i < length; i++) {
                        buffer[i] -= backC2;

                        if (buffer[i] < 0.0f) {
                            buffer[i] = 0.0f;
                        }
                    }

                    try {
                        subtractedSrcImage.importRGBData(secondColor, 4 * j * length, buffer, false);
                    } catch (IOException e) {
                        buffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                        return;
                    }

                    try {
                        registeredSrcImage.exportRGBData(thirdColor, 4 * j * length, length, buffer);
                    } catch (IOException e) {
                        buffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                        return;
                    }

                    for (i = 0; i < length; i++) {
                        buffer[i] -= backC3;

                        if (buffer[i] < 0.0f) {
                            buffer[i] = 0.0f;
                        }
                    }

                    try {
                        subtractedSrcImage.importRGBData(thirdColor, 4 * j * length, buffer, true);
                    } catch (IOException e) {
                        buffer = null;
                        errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                        return;
                    }
                } // for (j = 0; j < srcImage.getExtents()[2]; j++)

                subtractedSrcImage.calcMinMax();
                updateFileInfo(registeredSrcImage, subtractedSrcImage);

                try {
                    imageFrame3 = new ViewJFrameImage(subtractedSrcImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Unable to open frame for subtracted image");
                    setCompleted(false);

                    return;
                }

                srcImage.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                srcImage.clearMask();
                srcImage.notifyImageDisplayListeners();

                if (color == 1) {
                    bin1 = Math.min(bin1,
                                    (int) Math.round(subtractedSrcImage.getMaxR() - subtractedSrcImage.getMinR() + 1));
                } else {
                    bin1 = Math.min(bin1,
                                    (int) Math.round(subtractedSrcImage.getMaxG() - subtractedSrcImage.getMinG() + 1));
                }

                if (secondColor == 2) {
                    bin2 = Math.min(bin2,
                                    (int) Math.round(subtractedSrcImage.getMaxG() - subtractedSrcImage.getMinG() + 1));
                } else {
                    bin2 = Math.min(bin2,
                                    (int) Math.round(subtractedSrcImage.getMaxB() - subtractedSrcImage.getMinB() + 1));
                }

                int[] newExtents = new int[2];
                newExtents[0] = bin1 + leftPad + rightPad;
                newExtents[1] = bin2 + bottomPad + topPad;
                destImage.changeExtents(newExtents);
            } // if (backgroundIndex != -1) && (!redo))
            else if (!redo) {
                subtractedSrcImage = registeredSrcImage;
            } // else if (!redo)

            if ((!entireImage) && (!maskSupplied) && (!redo)) {
                subtractedSrcImage.getParentFrame().getComponentImage().getVOIHandler().addVOIUpdateListener(this);
            }

            try {
                buffer = null;
                buffer = new float[volume];
                secondBuffer = null;
                secondBuffer = new float[volume];
                histBuffer = null;
                histBuffer = new double[(bin1 + leftPad + rightPad) * (bin2 + bottomPad + topPad)];
            } catch (OutOfMemoryError oome) {
                buffer = null;
                secondBuffer = null;
                histBuffer = null;
                errorCleanUp("Algorithm ColocalizationRegression: Out of memory", true);

                return;
            }

            for (i = 0; i < histBuffer.length; i++) {
                histBuffer[i] = 0.0;
            }

            destImage.releaseLock(); // we need to be able to alter the dest image

            try {
                subtractedSrcImage.exportRGBData(color, 0, volume, buffer);
                subtractedSrcImage.exportRGBData(secondColor, 0, volume, secondBuffer);
            } catch (IOException e) {
                buffer = null;
                secondBuffer = null;
                histBuffer = null;
                errorCleanUp("Algorithm ColocalizationRegression: IOException", true);

                return;
            }

            voiSize = 0;
            voiIntensity1 = 0.0f;
            voiIntensity2 = 0.0f;

            for (i = 0; i < volume; i++) {

                if (inputMask.get(i)) {
                    voiSize++;

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        voiIntensity1 += buffer[i];
                        voiIntensity2 += secondBuffer[i];
                    }
                }
            }

            fireProgressStateChanged("Calculating overall linear correlation coefficient");
            fireProgressStateChanged(20);
            min1 = Double.MAX_VALUE;
            max1 = -Double.MAX_VALUE;
            min2 = Double.MAX_VALUE;
            max2 = -Double.MAX_VALUE;

            for (i = 0; i < volume; i++) {

                if (buffer[i] > max1) {
                    max1 = buffer[i];
                }

                if (buffer[i] < min1) {
                    min1 = buffer[i];
                }

                if (secondBuffer[i] > max2) {
                    max2 = secondBuffer[i];
                }

                if (secondBuffer[i] < min2) {
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
            averagex = 0.0;
            averagey = 0.0;
            count = 0;

            for (i = 0; i < volume; i++) {

                if (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) && inputMask.get(i)) {
                    averagex += buffer[i];
                    averagey += secondBuffer[i];
                    count++;
                }
            }

            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (i = 0; i < volume; i++) {

                if (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) && inputMask.get(i)) {
                    num += (buffer[i] - averagex) * (secondBuffer[i] - averagey);
                    denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                    denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                }
            }

            denom = Math.sqrt(denom1 * denom2);
            r = (float) (num / denom);
            Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);

            if (doP) {
                fireProgressStateChanged("Calculating autocovariance");
                fireProgressStateChanged(10);

                if (useRed) {
                    nameR = srcImage.getImageName() + "_autocovarianceR";
                    resultImageR = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), nameR);
                }

                if (useGreen) {
                    nameG = srcImage.getImageName() + "_autocovarianceG";
                    resultImageG = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), nameG);
                }

                if (useBlue) {
                    nameB = srcImage.getImageName() + "_autocovarianceB";
                    resultImageB = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), nameB);
                }

                algoAutoCovariance = new AlgorithmAutoCovariance(resultImageR, resultImageG, resultImageB,
                                                                 subtractedSrcImage);
                algoAutoCovariance.run();

                if (useRed) {
                    fwhmR = algoAutoCovariance.getFWHMR();
                    fwhm = fwhmR;

                    if (fwhmR != Integer.MAX_VALUE) {
                        Preferences.debug("Red auto covariance full width at half maximum = " + fwhmR + "\n",
                        		Preferences.DEBUG_ALGORITHM);
                    } else {
                        ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                        ViewUserInterface.getReference().setDataText("Cannot determine red auto covariance full width at half maximum");
                        Preferences.debug("Cannot determine red auto covariance full width at half maximum\n", 
                        		Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (useGreen) {
                    fwhmG = algoAutoCovariance.getFWHMG();
                    fwhm = Math.min(fwhm, fwhmG);

                    if (fwhmG != Integer.MAX_VALUE) {
                        Preferences.debug("Green auto covariance full width at half maximum = " + fwhmG + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                    } else {
                        ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                        ViewUserInterface.getReference().setDataText("Cannot determine green auto covariance full width at half maximum");
                        Preferences.debug("Cannot determine green auto covariance full width at half maximum\n", 
                        		Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (useBlue) {
                    fwhmB = algoAutoCovariance.getFWHMB();
                    fwhm = Math.min(fwhm, fwhmB);

                    if (fwhmB != Integer.MAX_VALUE) {
                        Preferences.debug("Blue auto covariance full width at half maximum = " + fwhmB + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                    } else {
                        ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                        ViewUserInterface.getReference().setDataText("Cannot determine blue auto covariance full width at half maximum");
                        Preferences.debug("Cannot determine blue auto covariance full width at half maximum\n", 
                        		Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (resultImageR != null) {
                    resultImageR.disposeLocal();
                    resultImageR = null;
                }

                if (resultImageG != null) {
                    resultImageG.disposeLocal();
                    resultImageG = null;
                }

                if (resultImageB != null) {
                    resultImageB.disposeLocal();
                    resultImageB = null;
                }

                algoAutoCovariance.finalize();
                algoAutoCovariance = null;

                if (fwhm == Integer.MAX_VALUE) {
                    finalize();
                    errorCleanUp("Algorithm ColocalizationRegression could not determine fwhm", true);

                    return;
                } else if (fwhm == fwhmR) {
                    scrambleFirst = true;
                } else if (fwhm == fwhmG) {

                    if (useRed) {
                        scrambleFirst = false;
                    } else {
                        scrambleFirst = true;
                    }
                } else {
                    scrambleFirst = false;
                }

                if (fwhm == 0) {
                    fwhm = 1;
                }

                fireProgressStateChanged("Calculating P-value");
                fireProgressStateChanged(15);

                // Calculate denom without inputMask
                averagex = 0.0;
                averagey = 0.0;
                count = 0;

                for (i = 0; i < volume; i++) {

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        averagex += buffer[i];
                        averagey += secondBuffer[i];
                        count++;
                    }
                }

                averagex /= count;
                averagey /= count;
                num = 0.0;
                denom1 = 0.0;
                denom2 = 0.0;

                for (i = 0; i < volume; i++) {

                    if ((buffer[i] >= background1) || (secondBuffer[i] >= background2)) {
                        num += (buffer[i] - averagex) * (secondBuffer[i] - averagey);
                        denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                        denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                    }
                }

                denom = Math.sqrt(denom1 * denom2);
                randomBuffer = null;
                randomBuffer = new float[volume];
                numXValues = xDim / fwhm;
                xRemainder = xDim % fwhm;
                numYValues = yDim / fwhm;
                yRemainder = yDim % fwhm;
                numZValues = zDim / fwhm;
                zRemainder = zDim % fwhm;
                sliceCubes = numXValues * numYValues;
                numCubes = sliceCubes * numZValues;
                randomGen = new RandomNumberGen();
                vectRand = new Vector<Integer>();

                int idxZ, idx;

                for (i = 0; i < 200; i++) {

                    for (s = 0; s < numCubes; s++) {
                        vectRand.add(new Integer(s));
                    }

                    for (s = 0; s < numCubes; s++) {
                        randNum = randomGen.genUniformRandomNum(0, vectRand.size() - 1);
                        xStart = randomGen.genUniformRandomNum(0, xRemainder);
                        xEnd = xDim - (xRemainder - xStart);
                        yStart = randomGen.genUniformRandomNum(0, yRemainder);
                        yEnd = yDim - (yRemainder - yStart);
                        zStart = randomGen.genUniformRandomNum(0, zRemainder);
                        zEnd = zDim - (zRemainder - zStart);
                        randCube = vectRand.elementAt(randNum);
                        vectRand.removeElementAt(randNum);
                        z = (fwhm * (randCube.intValue() / sliceCubes)) + zStart;
                        y = (fwhm * ((randCube.intValue() % sliceCubes) / numXValues)) + yStart;
                        x = (fwhm * ((randCube.intValue() % sliceCubes) % numXValues)) + xStart;
                        sz = (fwhm * (s / sliceCubes)) + zStart;
                        sy = (fwhm * ((s % sliceCubes) / numXValues)) + yStart;
                        sx = (fwhm * ((s % sliceCubes) % numXValues)) + xStart;

                        for (m = 0; m < fwhm; m++) {

                            for (k = 0; k < fwhm; k++) {

                                for (j = 0; j < fwhm; j++) {

                                    if (scrambleFirst) {
                                        randomBuffer[sx + j + ((sy + k) * xDim) + ((sz + m) * length)] = buffer[x + j +
                                                                                                                ((y +
                                                                                                                  k) *
                                                                                                                     xDim) +
                                                                                                                ((z +
                                                                                                                  m) *
                                                                                                                     length)];
                                    } else {
                                        randomBuffer[sx + j + ((sy + k) * xDim) + ((sz + m) * length)] = secondBuffer[x +
                                                                                                                      j +
                                                                                                                      ((y +
                                                                                                                        k) *
                                                                                                                           xDim) +
                                                                                                                      ((z +
                                                                                                                        m) *
                                                                                                                           length)];
                                    }
                                } // for (j = 0; j < fwhm; j++)
                            } // for (k = 0; k < fwhm; k++)
                        } // for (m = 0; m < fwhm; m++)

                        for (z = 0; z < zDim; z++) {
                            idxZ = z * length;

                            for (y = 0; y < yDim; y++) {
                                idx = (y * xDim) + idxZ;

                                for (x = 0; x < xStart; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + idx] = buffer[x + idx];
                                    } else {
                                        randomBuffer[x + idx] = secondBuffer[x + idx];
                                    }
                                }
                            }
                        }

                        for (z = 0; z < zDim; z++) {
                            idxZ = z * length;

                            for (y = 0; y < yDim; y++) {
                                idx = (y * xDim) + idxZ;

                                for (x = xEnd; x < xDim; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + idx] = buffer[x + idx];
                                    } else {
                                        randomBuffer[x + idx] = secondBuffer[x + idx];
                                    }
                                }
                            }
                        }

                        for (z = 0; z < zDim; z++) {
                            idxZ = z * length;

                            for (y = 0; y < yStart; y++) {
                                idx = (y * xDim) + idxZ;

                                for (x = 0; x < xDim; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + idx] = buffer[x + idx];
                                    } else {
                                        randomBuffer[x + idx] = secondBuffer[x + idx];
                                    }
                                }
                            }
                        }

                        for (z = 0; z < zDim; z++) {
                            idxZ = z * length;

                            for (y = yEnd; y < yDim; y++) {
                                idx = (y * xDim) + idxZ;

                                for (x = 0; x < xDim; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + idx] = buffer[x + idx];
                                    } else {
                                        randomBuffer[x + idx] = secondBuffer[x + idx];
                                    }
                                }
                            }
                        }

                        for (z = 0; z < zStart; z++) {
                            idxZ = z * length;

                            for (y = 0; y < yDim; y++) {
                                idx = (y * xDim) + idxZ;

                                for (x = 0; x < xDim; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + idx] = buffer[x + idx];
                                    } else {
                                        randomBuffer[x + idx] = secondBuffer[x + idx];
                                    }
                                }
                            }
                        }

                        for (z = zEnd; z < zDim; z++) {
                            idxZ = z * length;

                            for (y = 0; y < yDim; y++) {
                                idx = (y * xDim) + idxZ;

                                for (x = 0; x < xDim; x++) {

                                    if (scrambleFirst) {
                                        randomBuffer[x + idx] = buffer[x + idx];
                                    } else {
                                        randomBuffer[x + idx] = secondBuffer[x + idx];
                                    }
                                }
                            }
                        }
                    } // for (s = 0; s < numCubes; s++)

                    if (scrambleFirst) {
                        num = 0.0;

                        for (j = 0; j < volume; j++) {

                            if ((randomBuffer[j] >= background1) || (secondBuffer[j] >= background2)) {
                                num += (randomBuffer[j] - averagex) * (secondBuffer[j] - averagey);
                            }
                        }

                        rMat[i] = (float) (num / denom);
                    } else {
                        num = 0.0;

                        for (j = 0; j < volume; j++) {

                            if ((buffer[j] >= background1) || (randomBuffer[j] >= background2)) {
                                num += (buffer[j] - averagex) * (randomBuffer[j] - averagey);
                            }
                        }

                        rMat[i] = (float) (num / denom);
                    }
                } // for (i = 0; i < 200; i++)

                Arrays.sort(rMat);
                numBins = 200;

                for (i = 1; i < 200; i++) {

                    if (rMat[i] == rMat[i - 1]) {
                        numBins--;
                    }
                }

                rCum = new float[numBins];
                pStatMat = new float[numBins];
                rCum[0] = rMat[0];
                pStatMat[0] = 0.0f;
                pStatMat[1] = 0.005f;
                j = 1;

                for (i = 1; i < 200; i++) {

                    if (rMat[i] == rMat[i - 1]) {
                        pStatMat[j] += 0.005f;
                    } else {
                        rCum[j++] = rMat[i];

                        if (j < numBins) {
                            pStatMat[j] = pStatMat[j - 1] + 0.005f;
                        }
                    }
                } // for (i = 1; i < 200; i++)

                PValue = 0.995f;
                found = false;

                for (i = 0; (i <= (numBins - 1)) && (!found); i++) {

                    if (r <= rCum[i]) {
                        PValue = pStatMat[i];
                        found = true;
                    }
                }

                Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("numBins = " + numBins + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("rCum[0] = " + rCum[0] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("rCum[numBins-1] = " + rCum[numBins - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("P-value = " + PValue + "\n", Preferences.DEBUG_ALGORITHM);

                if (threadStopped) {
                    finalize();

                    return;
                }

                if (PValue < 0.95f) {

                    setCompleted(true);

                    return;
                }

            } // if (doP)

            // Reduce buffer and secondBuffer so that they only contain the points
            // where (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) &&
            // inputMask.get(i))
            thrLength = 0;
            tmpBuffer = new float[volume];
            tmpSecondBuffer = new float[volume];

            for (i = 0; i < volume; i++) {

                if (((buffer[i] >= background1) || (secondBuffer[i] >= background2)) && inputMask.get(i)) {
                    tmpBuffer[thrLength] = buffer[i];
                    tmpSecondBuffer[thrLength++] = secondBuffer[i];
                }
            }

            buffer = null;
            buffer = new float[thrLength];

            secondBuffer = null;
            secondBuffer = new float[thrLength];

            for (i = 0; i < thrLength; i++) {
                buffer[i] = tmpBuffer[i];
                secondBuffer[i] = tmpSecondBuffer[i];
            }

            tmpBuffer = null;
            tmpSecondBuffer = null;
            System.gc();

            // Use linear least squares to calculate the best line fit
            // for secondBuffer = a*firstBuffer + b.  Use an orthogonal
            // line fit where the distance measurements are orthgonal to
            // the proposed line.  This is an orthogonal regression as opposed
            // to a traditional regression of dependent y variable green on
            // independent red variable x which would give measurements in the
            // y direction.  Note that a traditional regression of red on green
            // would be yet another line giving measurements in the x direction.
            // Don't include any point i with buffer[i] < background1 and
            // secondBuffer[i] < background2 in the regression line.
            // Don't want the regression line to be affected by the dataless
            // black background of the picture.
            fireProgressStateChanged("Calculating least squares fit line");
            fireProgressStateChanged(25);
            sumx = 0.0;
            sumy = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {
                sumx += buffer[i];
                sumy += secondBuffer[i];
                count++;
            }

            averagex = sumx / count;
            averagey = sumy / count;
            sumx2 = 0.0;
            sumxy = 0.0;
            sumy2 = 0.0;

            for (i = 0; i < thrLength; i++) {
                diffx = buffer[i] - averagex;
                diffy = secondBuffer[i] - averagey;
                sumx2 += diffx * diffx;
                sumxy += diffx * diffy;
                sumy2 += diffy * diffy;
            }


            float aa;
            varx = sumx2 / (count - 1);
            vary = sumy2 / (count - 1);
            covarxy = sumxy / (count - 1);
            a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                             (2 * covarxy));
            aa = a * a;
            b = (float) (averagey - (a * averagex));
            mse = (float) (varx + (2 * a * covarxy) + (aa * vary)) / (1 + aa);

            // +-2 standard deviations on each side includes about 95%.
            halfWidth = (float) (2.0 * Math.sqrt(mse));

            Preferences.debug("First iteration\n", Preferences.DEBUG_ALGORITHM);

            if ((useRed) && (useGreen)) {
                Preferences.debug("green = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            } else if ((useRed) && (useBlue)) {
                Preferences.debug("blue = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            } else {
                Preferences.debug("blue = " + a + "*green" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);
            doAgain = false;

            for (iter = 0; (iter < 10) && doAgain; iter++) {
                float invAAp1 = 1.0f / (aa + 1);
                doAgain = false;
                sumx = 0.0;
                sumy = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {
                    distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) * invAAp1;

                    if (distance <= halfWidth) {
                        sumx += buffer[i];
                        sumy += secondBuffer[i];
                        count++;
                    }
                }

                averagex = sumx / count;
                averagey = sumy / count;
                sumx2 = 0.0;
                sumxy = 0.0;
                sumy2 = 0.0;

                for (i = 0; i < thrLength; i++) {
                    distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) * invAAp1;

                    if (distance <= halfWidth) {
                        diffx = buffer[i] - averagex;
                        diffy = secondBuffer[i] - averagey;
                        sumx2 += diffx * diffx;
                        sumxy += diffx * diffy;
                        sumy2 += diffy * diffy;
                    }
                }

                varx = sumx2 / (count - 1);
                vary = sumy2 / (count - 1);
                covarxy = sumxy / (count - 1);
                aLast = a;
                bLast = b;
                a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                 (2 * covarxy));
                aa = a * a;
                b = (float) (averagey - (a * averagex));
                mse = (float) (varx + (2 * a * covarxy) + (aa * vary)) / (1 + aa);

                // +-2 standard deviations on each side includes about 95%.
                halfWidth = (float) (2.0 * Math.sqrt(mse));

                Preferences.debug("Iteration = " + (iter + 2) + "\n", Preferences.DEBUG_ALGORITHM);

                if ((useRed) && (useGreen)) {
                    Preferences.debug("green = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                } else if ((useRed) && (useBlue)) {
                    Preferences.debug("blue = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                } else {
                    Preferences.debug("blue = " + a + "*green" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

                if ((aLast != 0.0f) && ((Math.abs(a - aLast) / aLast) > 0.001)) {
                    doAgain = true;
                }

                if ((bLast != 0.0f) && ((Math.abs(b - bLast) / bLast) > 0.001)) {
                    doAgain = true;
                }
            } // for (iter = 0; (iter < 10) && doAgain; iter++)

            fireProgressStateChanged("Generating histogram buffer");
            fireProgressStateChanged(50);

            for (i = 0; i < thrLength; i++) {
                ch1 = (int) Math.round((buffer[i] - min1) * scale1);
                ch2 = (int) Math.round((secondBuffer[i] - min2) * scale2);

                // invert y
                histBuffer[ch1 + leftPad + ((bin1 + leftPad + rightPad) * (topPad + bin2 - 1 - ch2))]++;
            }

            fireProgressStateChanged("Generating VOI for least squares line");
            fireProgressStateChanged(60);
            xArray = new float[2];
            yArray = new float[2];
            zArray = new float[2];
            zArray[0] = 0.0f;
            zArray[1] = 0.0f;

            // secondBuffer[i] = a*buffer[i] + b
            // ch1 = (buffer[i] - min1)*scale1 + leftPad
            // ch2 = (secondBuffer[i] - min2)*scale2 + topPad;
            // ch1 = (scale1/a)*[((ch2-topPad)/scale2) + min2 - a*min1 - b] + leftPad
            // ch2 = scale2*[((a*(ch1-leftPad))/scale1) + a*min1 - min2 + b] + topPad
            ch2f = topPad;
            ch1f = (float) ((scale1 / a) * (min2 - (a * min1) - b)) + leftPad;

            if (ch1f < leftPad) {
                ch1f = leftPad;
                ch2f = (float) (scale2 * ((a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[0] = ch1f;
            yArray[0] = ch2f;
            ch2f = topPad + bin2 - 1;
            ch1f = (float) ((scale1 / a) * (((ch2f - topPad) / scale2) + min2 - (a * min1) - b)) + leftPad;

            if (ch1f > (bin1 - 1 + leftPad)) {
                ch1f = bin1 - 1 + leftPad;
                ch2f = (float) (scale2 * (((a * (bin1 - 1)) / scale1) + (a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[1] = ch1f;
            yArray[1] = ch2f;

            if (!doSecondIteration) {
                fitLineVOI = new VOI((short) 0, "fitLine", VOI.LINE, -1.0f);
                fitLineVOI.importCurve(xArray, yArray, zArray);
                destImage.registerVOI(fitLineVOI);
                fitLineVOI.setFixed(true);
                fitLineVOI.setColor(Color.orange);
            } // if (!doSecondIteration)

            if (threadStopped) { // do before copying back into image
                finalize();

                return;
            }

            destImage.importData(0, histBuffer, true);
            destRes = new float[2];
            destRes[0] = 1.0f;
            destRes[1] = 1.0f;
            destImage.getFileInfo(0).setResolutions(destRes);

        } // try
        catch (IOException error) {
            displayError("Algorithm ColocalizationRegression reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            secondBuffer = null;
            displayError("Algorithm ColocalizationRegression reports: out of memory" + e);
            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Generating thresholded linear correlations");
        fireProgressStateChanged(70);

        // secondBuffer[i] = a*buffer[i] + b;
        // buffer[i] = (secondBuffer[i] - b)/a;
        // Calculate (lineMin1,lineMin2) and (lineMax1,lineMax2),
        // the endpoints of the line segment
        lineMin1 = min1;
        lineMin2 = (a * min1) + b;

        if (lineMin2 < min2) {
            lineMin2 = min2;
            lineMin1 = (min2 - b) / a;
        }

        lineMax1 = max1;
        lineMax2 = (a * max1) + b;

        if (lineMax2 > max2) {
            lineMax2 = max2;
            lineMax1 = (max2 - b) / a;
        }

        firstNonPositive1 = -1;
        firstNonPositive2 = -1;
        minPositiveThreshold = -1;
        nonPositiveThreshold = 1;
        lastPositive1 = (float) (Math.floor(lineMax1) + 1);
        lastPositive2 = (float) (Math.floor(lineMax2) + 1);
        firstUndefined1 = -1;
        firstUndefined2 = -1;

        // Calculate the linear correlation coefficients for all pixels
        // whose values are either below threshold in buffer or are below
        // a*threshold + b in secondBuffer.  The values in buffer range from
        // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
        // along the color that has the greatest range along the line
        // segment.
        if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2)) {
            thresholdOn1 = true;
            rThreshold = null;
            rThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(lineMin1)) + 1];
            colocSize = null;
            colocSize = new float[rThreshold.length];
            colocIntensity1 = null;
            colocIntensity1 = new float[rThreshold.length];
            colocIntensity2 = null;
            colocIntensity2 = new float[rThreshold.length];
            haveThreshold = new boolean[rThreshold.length];

            for (i = 0; i < rThreshold.length; i++) {
                rThreshold[i] = Float.NaN;
                colocSize[i] = 0.0f;
                colocIntensity1[i] = 0.0f;
                colocIntensity2[i] = 0.0f;
                haveThreshold[i] = false;
            }

            transitionFound = false;
            i = rThreshold.length / 2;
            thresholdIncrement = rThreshold.length / 4;

            while (!transitionFound) {
                threshold = (float) (i + Math.ceil(lineMin1));
                secondThreshold = (a * threshold) + b;
                averagex = 0.0;
                averagey = 0.0;
                count = 0;
                morex = false;
                morey = false;
                lastx = buffer[0];
                lasty = secondBuffer[0];
                voiIntensity1Thr = 0.0f;
                voiIntensity2Thr = 0.0f;

                for (j = 0; j < thrLength; j++) {
                    float buff = buffer[j];
                    float secBuff = secondBuffer[j];

                    if (buff >= threshold) {
                        voiIntensity1Thr += buff;
                    }

                    if (secBuff >= secondThreshold) {
                        voiIntensity2Thr += secBuff;
                    }

                    if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                        colocSize[i] += 1.0f;
                        colocIntensity1[i] += buff;
                        colocIntensity2[i] += secBuff;
                    } else {
                        averagex += buff;
                        averagey += secBuff;
                        count++;

                        if (count == 1) {
                            lastx = buff;
                            lasty = secBuff;
                        } else if (count >= 2) {

                            if (!morex) {

                                if (buff != lastx) {
                                    morex = true;
                                }
                            }

                            if (!morey) {

                                if (secBuff != lasty) {
                                    morey = true;
                                }
                            }
                        } // else if (count >= 2)
                    }
                } // for (j = 0; j < thrLength; j++)

                colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                if (doColocWithThresholds) {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                } else {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                }

                if (morex && morey) {
                    averagex /= count;
                    averagey /= count;
                    num = 0.0;
                    denom1 = 0.0;
                    denom2 = 0.0;

                    for (j = 0; j < thrLength; j++) {
                        float buff = buffer[j];
                        float secBuff = secondBuffer[j];

                        if ((buff < threshold) || (secBuff < secondThreshold)) {
                            num += (buff - averagex) * (secBuff - averagey);
                            denom1 += (buff - averagex) * (buff - averagex);
                            denom2 += (secBuff - averagey) * (secBuff - averagey);
                        }
                    } // for (j = 0; j < thrLength; j++)

                    rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                    haveThreshold[i] = true;
                    Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", Preferences.DEBUG_ALGORITHM);

                    if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i - 1];
                        firstNonPositive1 = (float) ((i - 1) + Math.ceil(lineMin1));
                        firstNonPositive2 = (a * firstNonPositive1) + b;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                   Float.isNaN(rThreshold[i - 1])) {
                        transitionFound = true;
                        firstUndefined1 = (float) ((i - 1) + Math.ceil(lineMin1));
                        firstUndefined2 = (a * firstUndefined1) + b;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                        transitionFound = true;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive1 = (float) (i + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i];
                        firstNonPositive1 = (float) (i + Math.ceil(lineMin1));
                        firstNonPositive2 = (a * firstNonPositive1) + b;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && Float.isNaN(rThreshold[i]) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        firstUndefined1 = (float) (i + Math.ceil(lineMin1));
                        firstUndefined2 = (a * firstUndefined1) + b;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                        lastPositive2 = (a * lastPositive1) + b;
                    } else if (rThreshold[i] > 0.0f) {
                        i = i - thresholdIncrement;

                        if (i < 0) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // else if (rThreshold[i] > 0.0f)
                    else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    }
                } // if (morex && morey)
                else { // Float.isNaN(rThreshold[i])
                    haveThreshold[i] = true;
                    i = i + thresholdIncrement;

                    if (i >= rThreshold.length) {
                        transitionFound = true;
                    }

                    thresholdIncrement = thresholdIncrement / 2;

                    if (thresholdIncrement == 0) {
                        thresholdIncrement = 1;
                    }
                } // Float.isNaN(rThreshold[i])
            } // while (!transitionFound)

        } // if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2))
        else { // ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)
            thresholdOn1 = false;
            rThreshold = null;
            rThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(lineMin2)) + 1];
            colocSize = null;
            colocSize = new float[rThreshold.length];
            colocIntensity1 = null;
            colocIntensity1 = new float[rThreshold.length];
            colocIntensity2 = null;
            colocIntensity2 = new float[rThreshold.length];
            haveThreshold = new boolean[rThreshold.length];

            for (i = 0; i < rThreshold.length; i++) {
                rThreshold[i] = Float.NaN;
                colocSize[i] = 0.0f;
                colocIntensity1[i] = 0.0f;
                colocIntensity2[i] = 0.0f;
                haveThreshold[i] = false;
            }

            transitionFound = false;
            i = rThreshold.length / 2;
            thresholdIncrement = rThreshold.length / 4;

            while (!transitionFound) {
                secondThreshold = (float) (i + Math.ceil(lineMin2));
                threshold = (secondThreshold - b) / a;
                averagex = 0.0;
                averagey = 0.0;
                count = 0;
                morex = false;
                morey = false;
                lastx = buffer[0];
                lasty = secondBuffer[0];
                voiIntensity1Thr = 0.0f;
                voiIntensity2Thr = 0.0f;

                for (j = 0; j < thrLength; j++) {
                    float buff = buffer[j];
                    float secBuff = secondBuffer[j];

                    if (buff >= threshold) {
                        voiIntensity1Thr += buff;
                    }

                    if (secBuff >= secondThreshold) {
                        voiIntensity2Thr += secBuff;
                    }

                    if ((buff >= threshold) && (secBuff >= secondThreshold)) {
                        colocSize[i] += 1.0f;
                        colocIntensity1[i] += buff;
                        colocIntensity2[i] += secBuff;
                    } else {
                        averagex += buff;
                        averagey += secBuff;
                        count++;

                        if (count == 1) {
                            lastx = buff;
                            lasty = secBuff;
                        } else if (count >= 2) {

                            if (!morex) {

                                if (buff != lastx) {
                                    morex = true;
                                }
                            }

                            if (!morey) {

                                if (secBuff != lasty) {
                                    morey = true;
                                }
                            }
                        } // else if (count >= 2)
                    }
                } // for (j = 0; j < thrLength; j++)

                colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                if (doColocWithThresholds) {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                } else {
                    colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                    colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                }

                if (morex && morey) {
                    averagex /= count;
                    averagey /= count;
                    num = 0.0;
                    denom1 = 0.0;
                    denom2 = 0.0;

                    for (j = 0; j < thrLength; j++) {
                        float buff = buffer[j];
                        float secBuff = secondBuffer[j];

                        if ((buff < threshold) || (secBuff < secondThreshold)) {
                            num += (buff - averagex) * (secBuff - averagey);
                            denom1 += (buff - averagex) * (buff - averagex);
                            denom2 += (secBuff - averagey) * (secBuff - averagey);
                        }
                    } // for (j = 0; j < thrLength; j++)

                    rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                    haveThreshold[i] = true;
                    Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", Preferences.DEBUG_ALGORITHM);

                    if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i - 1];
                        firstNonPositive2 = (float) ((i - 1) + Math.ceil(lineMin2));
                        firstNonPositive1 = (firstNonPositive2 - b) / a;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                   Float.isNaN(rThreshold[i - 1])) {
                        transitionFound = true;
                        firstUndefined2 = (float) ((i - 1) + Math.ceil(lineMin2));
                        firstUndefined1 = (firstUndefined2 - b) / a;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                        transitionFound = true;
                        minPositiveThreshold = rThreshold[i];
                        lastPositive2 = (float) (i + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        nonPositiveThreshold = rThreshold[i];
                        firstNonPositive2 = (float) (i + Math.ceil(lineMin2));
                        firstNonPositive1 = (firstNonPositive2 - b) / a;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && Float.isNaN(rThreshold[i]) &&
                                   (rThreshold[i + 1] > 0.0f)) {
                        transitionFound = true;
                        firstUndefined2 = (float) (i + Math.ceil(lineMin2));
                        firstUndefined1 = (firstUndefined2 - b) / a;
                        minPositiveThreshold = rThreshold[i + 1];
                        lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                        lastPositive1 = (lastPositive2 - b) / a;
                    } else if (rThreshold[i] > 0.0f) {
                        i = i - thresholdIncrement;

                        if (i < 0) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // else if (rThreshold[i] > 0.0f)
                    else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    }
                } // if (morex && morey)
                else { // Float.isNaN(rThreshold[i])
                    haveThreshold[i] = true;
                    i = i + thresholdIncrement;

                    if (i >= rThreshold.length) {
                        transitionFound = true;
                    }

                    thresholdIncrement = thresholdIncrement / 2;

                    if (thresholdIncrement == 0) {
                        thresholdIncrement = 1;
                    }
                } // Float.isNaN(rThreshold[i])
            } // while (!transitionFound)

        } // else ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)

        if ((useRed) && (useGreen)) {

            if (firstUndefined1 >= 0) {
                Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + firstUndefined1 + " or green < " + firstUndefined2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            }

            if (nonPositiveThreshold <= 0) {
                Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + firstNonPositive1 + " or green < " + firstNonPositive2 +
                                  "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with red < " + lastPositive1 + " or green < " + lastPositive2 + "\n", 
            		Preferences.DEBUG_ALGORITHM);

        } // if ((useRed) && (useGreen))
        else if ((useRed) && (useBlue)) {

            if (firstUndefined1 >= 0) {
                Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + firstUndefined1 + " or blue < " + firstUndefined2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            }

            if (nonPositiveThreshold <= 0) {
                Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + firstNonPositive1 + " or blue < " + firstNonPositive2 +
                                  "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with red < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
            		Preferences.DEBUG_ALGORITHM);
        } // else if ((useRed) && (useBlue))
        else {

            if (firstUndefined1 >= 0) {
                Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with green < " + firstUndefined1 + " or blue < " + firstUndefined2 +
                                  "\n", Preferences.DEBUG_ALGORITHM);
            }

            if (nonPositiveThreshold <= 0) {
                Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with green < " + firstNonPositive1 + " or blue < " + firstNonPositive2 +
                                  "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("for pixels with green < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
            		Preferences.DEBUG_ALGORITHM);
        }

        if (minPositiveThreshold < 0) {
            removeVOIUpdateListener();
            subtractedSrcImage.clearMask();
            subtractedSrcImage.notifyImageDisplayListeners();
            MipavUtil.displayError("No positive threshold found");

            setCompleted(false);

            return;
        }

        if (doSecondIteration) {
            Preferences.debug("Second iteration excluding subthresholded region\n", Preferences.DEBUG_ALGORITHM);
            fireProgressStateChanged("Second iteration excluding subthresholded region");
            fireProgressStateChanged(80);
            t1 = lastPositive1;
            t2 = lastPositive2;

            averagex = 0.0;
            averagey = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    averagex += buffer[i];
                    averagey += secondBuffer[i];
                    count++;
                }
            }

            averagex /= count;
            averagey /= count;
            num = 0.0;
            denom1 = 0.0;
            denom2 = 0.0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    num += (buffer[i] - averagex) * (secondBuffer[i] - averagey);
                    denom1 += (buffer[i] - averagex) * (buffer[i] - averagex);
                    denom2 += (secondBuffer[i] - averagey) * (secondBuffer[i] - averagey);
                }
            }

            denom = Math.sqrt(denom1 * denom2);
            r = (float) (num / denom);
            Preferences.debug("Linear correlation coefficient = " + r + "\n", Preferences.DEBUG_ALGORITHM);

            // Use linear least squares to calculate the best line fit
            // for secondBuffer = a*firstBuffer + b.  Use an orthogonal
            // line fit where the distance measurements are orthgonal to
            // the proposed line.  This is an orthogonal regression as opposed
            // to a traditional regression of dependent y variable green on
            // independent red variable x which would give measurements in the
            // y direction.  Note that a traditional regression of red on green
            // would be yet another line giving measurements in the x direction.
            // Don't include any point i with buffer[i] < lastPositive1 or
            // secondBuffer[i] < lastPositive2 in the regression line.
            // This excludes the L shaped subthresholded region from the
            // first iteration.
            sumx = 0.0;
            sumy = 0.0;
            count = 0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    sumx += buffer[i];
                    sumy += secondBuffer[i];
                    count++;
                }
            }

            averagex = sumx / count;
            averagey = sumy / count;
            sumx2 = 0.0;
            sumxy = 0.0;
            sumy2 = 0.0;

            for (i = 0; i < thrLength; i++) {

                if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                    diffx = buffer[i] - averagex;
                    diffy = secondBuffer[i] - averagey;
                    sumx2 += diffx * diffx;
                    sumxy += diffx * diffy;
                    sumy2 += diffy * diffy;
                }
            }


            float aa;
            varx = sumx2 / (count - 1);
            vary = sumy2 / (count - 1);
            covarxy = sumxy / (count - 1);
            a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                             (2 * covarxy));
            aa = a * a;
            b = (float) (averagey - (a * averagex));
            mse = (float) (varx + (2 * a * covarxy) + (aa * vary)) / (1 + aa);

            // +-2 standard deviations on each side includes about 95%.
            halfWidth = (float) (2.0 * Math.sqrt(mse));

            if ((useRed) && (useGreen)) {
                Preferences.debug("green = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            } else if ((useRed) && (useBlue)) {
                Preferences.debug("blue = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            } else {
                Preferences.debug("blue = " + a + "*green" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);
            doAgain = false;

            for (iter = 0; (iter < 10) && doAgain; iter++) {
                float invAAp1 = 1.0f / (aa + 1);
                doAgain = false;
                sumx = 0.0;
                sumy = 0.0;
                count = 0;

                for (i = 0; i < thrLength; i++) {

                    if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                        distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) * invAAp1;

                        if (distance <= halfWidth) {
                            sumx += buffer[i];
                            sumy += secondBuffer[i];
                            count++;
                        }
                    }
                }

                averagex = sumx / count;
                averagey = sumy / count;
                sumx2 = 0.0;
                sumxy = 0.0;
                sumy2 = 0.0;

                for (i = 0; i < thrLength; i++) {

                    if ((buffer[i] >= t1) && (secondBuffer[i] >= t2)) {
                        distance = Math.abs((a * buffer[i]) - secondBuffer[i] + b) * invAAp1;

                        if (distance <= halfWidth) {
                            diffx = buffer[i] - averagex;
                            diffy = secondBuffer[i] - averagey;
                            sumx2 += diffx * diffx;
                            sumxy += diffx * diffy;
                            sumy2 += diffy * diffy;
                        }
                    }
                }

                varx = sumx2 / (count - 1);
                vary = sumy2 / (count - 1);
                covarxy = sumxy / (count - 1);
                aLast = a;
                bLast = b;
                a = (float) ((vary - varx + Math.sqrt(((vary - varx) * (vary - varx)) + (4 * covarxy * covarxy))) /
                                 (2 * covarxy));
                aa = a * a;
                b = (float) (averagey - (a * averagex));
                mse = (float) (varx + (2 * a * covarxy) + (aa * vary)) / (1 + aa);

                // +-2 standard deviations on each side includes about 95%.
                halfWidth = (float) (2.0 * Math.sqrt(mse));

                Preferences.debug("Iteration = " + (iter + 2) + "\n", Preferences.DEBUG_ALGORITHM);

                if ((useRed) && (useGreen)) {
                    Preferences.debug("green = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                } else if ((useRed) && (useBlue)) {
                    Preferences.debug("blue = " + a + "*red" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                } else {
                    Preferences.debug("blue = " + a + "*green" + " + " + b + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Mean square error = " + mse + "\n", Preferences.DEBUG_ALGORITHM);

                if ((aLast != 0.0f) && ((Math.abs(a - aLast) / aLast) > 0.001)) {
                    doAgain = true;
                }

                if ((bLast != 0.0f) && ((Math.abs(b - bLast) / bLast) > 0.001)) {
                    doAgain = true;
                }
            } // for (iter = 0; (iter < 10) && doAgain; iter++)

            xArray = new float[2];
            yArray = new float[2];
            zArray = new float[2];
            zArray[0] = 0.0f;
            zArray[1] = 0.0f;

            // secondBuffer[i] = a*buffer[i] + b
            // ch1 = (buffer[i] - min1)*scale1 + leftPad
            // ch2 = (secondBuffer[i] - min2)*scale2 + topPad;
            // ch1 = (scale1/a)*[((ch2-topPad)/scale2) + min2 - a*min1 - b] + leftPad
            // ch2 = scale2*[((a*(ch1-leftPad))/scale1) + a*min1 - min2 + b] + topPad
            ch2f = topPad;
            ch1f = (float) ((scale1 / a) * (min2 - (a * min1) - b)) + leftPad;

            if (ch1f < leftPad) {
                ch1f = leftPad;
                ch2f = (float) (scale2 * ((a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[0] = ch1f;
            yArray[0] = ch2f;
            ch2f = topPad + bin2 - 1;
            ch1f = (float) ((scale1 / a) * (((ch2f - topPad) / scale2) + min2 - (a * min1) - b)) + leftPad;

            if (ch1f > (bin1 - 1 + leftPad)) {
                ch1f = bin1 - 1 + leftPad;
                ch2f = (float) (scale2 * (((a * (bin1 - 1)) / scale1) + (a * min1) - min2 + b)) + topPad;
            }

            // invert ch2f
            ch2f = (2 * topPad) + bin2 - 1.0f - ch2f;
            xArray[1] = ch1f;
            yArray[1] = ch2f;
            fitLineVOI = new VOI((short) 0, "fitLine", VOI.LINE, -1.0f);
            fitLineVOI.importCurve(xArray, yArray, zArray);
            destImage.registerVOI(fitLineVOI);
            fitLineVOI.setFixed(true);
            fitLineVOI.setColor(Color.orange);

            // secondBuffer[i] = a*buffer[i] + b;
            // buffer[i] = (secondBuffer[i] - b)/a;
            // Calculate (lineMin1,lineMin2) and (lineMax1,lineMax2),
            // the endpoints of the line segment
            lineMin1 = min1;
            lineMin2 = (a * min1) + b;

            if (lineMin2 < min2) {
                lineMin2 = min2;
                lineMin1 = (min2 - b) / a;
            }

            lineMax1 = max1;
            lineMax2 = (a * max1) + b;

            if (lineMax2 > max2) {
                lineMax2 = max2;
                lineMax1 = (max2 - b) / a;
            }

            firstNonPositive1 = -1;
            firstNonPositive2 = -1;
            minPositiveThreshold = -1;
            nonPositiveThreshold = 1;
            lastPositive1 = (float) (Math.floor(lineMax1) + 1);
            lastPositive2 = (float) (Math.floor(lineMax2) + 1);
            firstUndefined1 = -1;
            firstUndefined2 = -1;

            // Calculate the linear correlation coefficients for all pixels
            // whose values are either below threshold in buffer or are below
            // a*threshold + b in secondBuffer.  The values in buffer range from
            // min1 to max1 and in secondBuffer range from min2 to max2.  Calculate
            // along the color that has the greatest range along the line
            // segment.
            if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2)) {
                thresholdOn1 = true;
                rThreshold = null;
                rThreshold = new float[(int) Math.round(Math.floor(lineMax1) - Math.ceil(lineMin1)) + 1];
                colocSize = null;
                colocSize = new float[rThreshold.length];
                colocIntensity1 = null;
                colocIntensity1 = new float[rThreshold.length];
                colocIntensity2 = null;
                colocIntensity2 = new float[rThreshold.length];
                haveThreshold = new boolean[rThreshold.length];

                for (i = 0; i < rThreshold.length; i++) {
                    rThreshold[i] = Float.NaN;
                    colocSize[i] = 0.0f;
                    colocIntensity1[i] = 0.0f;
                    colocIntensity2[i] = 0.0f;
                    haveThreshold[i] = false;
                }

                transitionFound = false;
                i = rThreshold.length / 2;
                thresholdIncrement = rThreshold.length / 4;

                while (!transitionFound) {
                    threshold = (float) (i + Math.ceil(lineMin1));
                    secondThreshold = (a * threshold) + b;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = buffer[0];
                    lasty = secondBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {

                        if (buffer[j] >= threshold) {
                            voiIntensity1Thr += buffer[j];
                        }

                        if (secondBuffer[j] >= secondThreshold) {
                            voiIntensity2Thr += secondBuffer[j];
                        }

                        if ((buffer[j] >= threshold) && (secondBuffer[j] >= secondThreshold)) {
                            colocSize[i] += 1.0f;
                            colocIntensity1[i] += buffer[j];
                            colocIntensity2[i] += secondBuffer[j];
                        } else {
                            averagex += buffer[j];
                            averagey += secondBuffer[j];
                            count++;

                            if (count == 1) {
                                lastx = buffer[j];
                                lasty = secondBuffer[j];
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buffer[j] != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secondBuffer[j] != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                    if (doColocWithThresholds) {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {

                            if ((buffer[j] < threshold) || (secondBuffer[j] < secondThreshold)) {
                                num += (buffer[j] - averagex) * (secondBuffer[j] - averagey);
                                denom1 += (buffer[j] - averagex) * (buffer[j] - averagex);
                                denom2 += (secondBuffer[j] - averagey) * (secondBuffer[j] - averagey);
                            }
                        }

                        rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        haveThreshold[i] = true;
                        Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i - 1];
                            firstNonPositive1 = (float) ((i - 1) + Math.ceil(lineMin1));
                            firstNonPositive2 = (a * firstNonPositive1) + b;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                       Float.isNaN(rThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined1 = (float) ((i - 1) + Math.ceil(lineMin1));
                            firstUndefined2 = (a * firstUndefined1) + b;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive1 = (float) (i + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                       (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i];
                            firstNonPositive1 = (float) (i + Math.ceil(lineMin1));
                            firstNonPositive2 = (a * firstNonPositive1) + b;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] &&
                                       Float.isNaN(rThreshold[i]) && (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined1 = (float) (i + Math.ceil(lineMin1));
                            firstUndefined2 = (a * firstUndefined1) + b;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive1 = (float) ((i + 1) + Math.ceil(lineMin1));
                            lastPositive2 = (a * lastPositive1) + b;
                        } else if (rThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (rThreshold[i] > 0.0f)
                        else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= rThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(rThreshold[i])
                        haveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(rThreshold[i])
                } // while (!transitionFound)

            } // if ((lineMax1 - lineMin1) >= (lineMax2 - lineMin2))
            else { // ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)
                thresholdOn1 = false;
                rThreshold = new float[(int) Math.round(Math.floor(lineMax2) - Math.ceil(lineMin2)) + 1];
                colocSize = new float[rThreshold.length];
                colocIntensity1 = new float[rThreshold.length];
                colocIntensity2 = new float[rThreshold.length];

                for (i = 0; i < rThreshold.length; i++) {
                    rThreshold[i] = Float.NaN;
                    colocSize[i] = 0.0f;
                    colocIntensity1[i] = 0.0f;
                    colocIntensity2[i] = 0.0f;
                    haveThreshold[i] = false;
                }

                transitionFound = false;
                i = rThreshold.length / 2;
                thresholdIncrement = rThreshold.length / 4;

                while (!transitionFound) {
                    secondThreshold = (float) (i + Math.ceil(lineMin2));
                    threshold = (secondThreshold - b) / a;
                    averagex = 0.0;
                    averagey = 0.0;
                    count = 0;
                    morex = false;
                    morey = false;
                    lastx = buffer[0];
                    lasty = secondBuffer[0];
                    voiIntensity1Thr = 0.0f;
                    voiIntensity2Thr = 0.0f;

                    for (j = 0; j < thrLength; j++) {

                        if (buffer[j] >= threshold) {
                            voiIntensity1Thr += buffer[j];
                        }

                        if (secondBuffer[j] >= secondThreshold) {
                            voiIntensity2Thr += secondBuffer[j];
                        }

                        if ((buffer[j] >= threshold) && (secondBuffer[j] >= secondThreshold)) {
                            colocSize[i] += 1.0f;
                            colocIntensity1[i] += buffer[j];
                            colocIntensity2[i] += secondBuffer[j];
                        } else {
                            averagex += buffer[j];
                            averagey += secondBuffer[j];
                            count++;

                            if (count == 1) {
                                lastx = buffer[j];
                                lasty = secondBuffer[j];
                            } else if (count >= 2) {

                                if (!morex) {

                                    if (buffer[j] != lastx) {
                                        morex = true;
                                    }
                                }

                                if (!morey) {

                                    if (secondBuffer[j] != lasty) {
                                        morey = true;
                                    }
                                }
                            } // else if (count >= 2)
                        }
                    } // for (j = 0; j < thrLength; j++)

                    colocSize[i] = (colocSize[i] / voiSize) * 100.0f;

                    if (doColocWithThresholds) {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1Thr) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2Thr) * 100.0f;
                    } else {
                        colocIntensity1[i] = (colocIntensity1[i] / voiIntensity1) * 100.0f;
                        colocIntensity2[i] = (colocIntensity2[i] / voiIntensity2) * 100.0f;
                    }

                    if (morex && morey) {
                        averagex /= count;
                        averagey /= count;
                        num = 0.0;
                        denom1 = 0.0;
                        denom2 = 0.0;

                        for (j = 0; j < thrLength; j++) {

                            if ((buffer[j] < threshold) || (secondBuffer[j] < secondThreshold)) {
                                num += (buffer[j] - averagex) * (secondBuffer[j] - averagey);
                                denom1 += (buffer[j] - averagex) * (buffer[j] - averagex);
                                denom2 += (secondBuffer[j] - averagey) * (secondBuffer[j] - averagey);
                            }
                        }

                        rThreshold[i] = (float) (num / Math.sqrt(denom1 * denom2));
                        haveThreshold[i] = true;
                        Preferences.debug("i = " + i + " rThreshold[i] = " + rThreshold[i] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);

                        if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) && (rThreshold[i - 1] <= 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i - 1];
                            firstNonPositive2 = (float) ((i - 1) + Math.ceil(lineMin2));
                            firstNonPositive1 = (firstNonPositive2 - b) / a;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i >= 1) && haveThreshold[i - 1] && (rThreshold[i] > 0.0f) &&
                                       Float.isNaN(rThreshold[i - 1])) {
                            transitionFound = true;
                            firstUndefined2 = (float) ((i - 1) + Math.ceil(lineMin2));
                            firstUndefined1 = (firstUndefined2 - b) / a;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i == 0) && (rThreshold[i] > 0.0f)) {
                            transitionFound = true;
                            minPositiveThreshold = rThreshold[i];
                            lastPositive2 = (float) (i + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] && (rThreshold[i] <= 0.0f) &&
                                       (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            nonPositiveThreshold = rThreshold[i];
                            firstNonPositive2 = (float) (i + Math.ceil(lineMin2));
                            firstNonPositive1 = (firstNonPositive2 - b) / a;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if ((i < (rThreshold.length - 1)) && haveThreshold[i + 1] &&
                                       Float.isNaN(rThreshold[i]) && (rThreshold[i + 1] > 0.0f)) {
                            transitionFound = true;
                            firstUndefined2 = (float) (i + Math.ceil(lineMin2));
                            firstUndefined1 = (firstUndefined2 - b) / a;
                            minPositiveThreshold = rThreshold[i + 1];
                            lastPositive2 = (float) ((i + 1) + Math.ceil(lineMin2));
                            lastPositive1 = (lastPositive2 - b) / a;
                        } else if (rThreshold[i] > 0.0f) {
                            i = i - thresholdIncrement;

                            if (i < 0) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        } // else if (rThreshold[i] > 0.0f)
                        else { // ((rThreshold[i] <= 0.0f) || )Float.isNaN(rThreshold[i])
                            i = i + thresholdIncrement;

                            if (i >= rThreshold.length) {
                                transitionFound = true;
                            }

                            thresholdIncrement = thresholdIncrement / 2;

                            if (thresholdIncrement == 0) {
                                thresholdIncrement = 1;
                            }
                        }
                    } // if (morex && morey)
                    else { // Float.isNaN(rThreshold[i])
                        haveThreshold[i] = true;
                        i = i + thresholdIncrement;

                        if (i >= rThreshold.length) {
                            transitionFound = true;
                        }

                        thresholdIncrement = thresholdIncrement / 2;

                        if (thresholdIncrement == 0) {
                            thresholdIncrement = 1;
                        }
                    } // Float.isNaN(rThreshold[i])
                } // while (!transitionFound)

            } // else ((lineMax1 - lineMin1) < (lineMax2 - lineMin2)

            if ((useRed) && (useGreen)) {

                if (firstUndefined1 >= 0) {
                    Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstUndefined1 + " or green < " + firstUndefined2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (nonPositiveThreshold <= 0) {
                    Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstNonPositive1 + " or green < " +
                                      firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + lastPositive1 + " or green < " + lastPositive2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);

            } // if ((useRed) && (useGreen))
            else if ((useRed) && (useBlue)) {

                if (firstUndefined1 >= 0) {
                    Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstUndefined1 + " or blue < " + firstUndefined2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (nonPositiveThreshold <= 0) {
                    Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with red < " + firstNonPositive1 + " or blue < " + firstNonPositive2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with red < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            } // else if ((useRed) && (useBlue))
            else {

                if (firstUndefined1 >= 0) {
                    Preferences.debug("Cannot calculate linear correlation coefficient\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with green < " + firstUndefined1 + " or blue < " + firstUndefined2 +
                                      "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (nonPositiveThreshold <= 0) {
                    Preferences.debug("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("for pixels with green < " + firstNonPositive1 + " or blue < " +
                                      firstNonPositive2 + "\n", Preferences.DEBUG_ALGORITHM);
                }

                Preferences.debug("Linear correlation coefficient = " + minPositiveThreshold + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("for pixels with green < " + lastPositive1 + " or blue < " + lastPositive2 + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            }

            if (minPositiveThreshold < 0) {
                removeVOIUpdateListener();
                MipavUtil.displayError("No positve threshold found on second iteration");

                setCompleted(false);

                return;
            }
        } // if (doSecondIteration)

        frameList = srcImage.getImageFrameVector();

        for (i = 0; i < frameList.size(); i++) {

            if ((((ViewJFrameBase) frameList.elementAt(i)).getControls()) != null) {
                controlFrame = ((ViewJFrameBase) frameList.elementAt(i));
            }
        }

        fireProgressStateChanged("Creating point VOI");
        fireProgressStateChanged(85);

        // position the point at the minimum positive threshold
        xArray = new float[1];
        yArray = new float[1];
        zArray = new float[1];

        // Allow 10 blank spaces at top and 10 blank spaces at bottom
        if (pointCalculation) {
            xArray[0] = (float) ((point1 - min1) * scale1) + leftPad;
            yArray[0] = (float) (bin2 - 1.0f - ((point2 - min2) * scale2)) + topPad;
        } else {
            xArray[0] = (float) ((lastPositive1 - min1) * scale1) + leftPad;
            yArray[0] = (float) (bin2 - 1.0f - ((lastPositive2 - min2) * scale2)) + topPad;
        }

        zArray[0] = 0.0f;
        pointVOI = new VOI((short) 1, "thresholdPoint", VOI.POINT, -1.0f);
        pointVOI.importCurve(xArray, yArray, zArray);
        destImage.registerVOI(pointVOI);
        pointVOI.setFixed(false);

        // Don't draw point - draw square with blue inside and yellow edge
        pointVOI.setColor(Color.green);

        /*UI.setDataText("Linear correlation coefficient = " + r + "\n");
         * if ((useRed) && (useGreen)) { UI.setDataText("green = " + a + "*red" + " + " + b + "\n"); } else if ((useRed)
         * && (useBlue)) { UI.setDataText("blue = " + a + "*red" + " + " + b + "\n"); } else { UI.setDataText("blue = "
         * + a + "*green" + " + " + b + "\n"); } UI.setDataText("Mean square error = " + mse + "\n"); if ((useRed) &&
         * (useGreen)) { if (firstUndefined1 >= 0) { UI.setDataText("Cannot calculate linear correlation
         * coefficient\n"); UI.setDataText("for pixels with red < " + firstUndefined1 + " or green < " + firstUndefined2
         * +"\n"); } if (nonPositiveThreshold <= 0) { UI.setDataText("Nonpositive linear correlation coefficient = " +
         * nonPositiveThreshold + "\n"); UI.setDataText("for pixels with red < " + firstNonPositive1 + " or green < " +
         * firstNonPositive2 + "\n"); } UI.setDataText("Linear correlation coefficient = " + minPositiveThreshold +
         * "\n"); UI.setDataText("for pixels with red < " + lastPositive1 + " or green < " + lastPositive2 + "\n"); } //
         * if ((useRed) && (useGreen)) else if ((useRed) && (useBlue)) { if (firstUndefined1 >= 0) {
         * UI.setDataText("Cannot calculate linear correlation coefficient\n"); UI.setDataText("for pixels with red < "
         * + firstUndefined1 + " or blue < " + firstUndefined2 +"\n"); } if (nonPositiveThreshold <= 0) {
         * UI.setDataText("Nonpositive linear correlation coefficient = " + nonPositiveThreshold + "\n");
         * UI.setDataText("for pixels with red < " + firstNonPositive1 + " or blue < " + firstNonPositive2 + "\n"); }
         * UI.setDataText("Linear correlation coefficient = " + minPositiveThreshold + "\n"); UI.setDataText("for pixels
         * with red < " + lastPositive1 + " or blue < " + lastPositive2 + "\n"); } // else if ((useRed) && (useBlue))
         * else { if (firstUndefined1 >= 0) { UI.setDataText("Cannot calculate linear correlation coefficient\n");
         * UI.setDataText("for pixels with green < " + firstUndefined1 + " or blue < " + firstUndefined2 +"\n"); } if
         * (nonPositiveThreshold <= 0) { UI.setDataText("Nonpositive linear correlation coefficient = " +
         * nonPositiveThreshold + "\n"); UI.setDataText("for pixels with green < " + firstNonPositive1 + " or blue < " +
         * firstNonPositive2 + "\n"); } UI.setDataText("Linear correlation coefficient = " + minPositiveThreshold +
         * "\n"); UI.setDataText("for pixels with green < " + lastPositive1 + " or blue < " + lastPositive2 + "\n"); }*/

        fireProgressStateChanged("Creating ColocalizationRegression frame");
        fireProgressStateChanged(90);

        if (redo) {
            haveFreeRangeThreshold = null;
            freeRangeRThreshold = null;
            freeRangeColocSize = null;
            freeRangeColocIntensity1 = null;
            freeRangeColocIntensity2 = null;
            frameColocalize.setNewVar(a, b, r, PValue, haveThreshold, rThreshold, colocSize, colocIntensity1,
                                      colocIntensity2, min1, max1, min2, max2, scale1, scale2, lineMin1, lineMax1,
                                      lineMin2, lineMax2, thresholdOn1);
        } else {
            // System.err.println("Constr4");

            frameColocalize = new ViewJFrameColocalizationRegression(this, subtractedSrcImage, null, baseImage, null,
                                                                     null, destImage, controlFrame, useRed, useGreen,
                                                                     useBlue, a, b, r, PValue, haveThreshold,
                                                                     rThreshold, colocSize, colocIntensity1,
                                                                     colocIntensity2, min1, max1, min2, max2, scale1,
                                                                     scale2, lineMin1, lineMax1, lineMin2, lineMax2,
                                                                     thresholdOn1, leftPad, rightPad, bottomPad, topPad,
                                                                     doSecondIteration, pointCalculation);

            if (pointCalculation) {
                frameColocalize.pointCalculate();
            }
        }

        fireProgressStateChanged(100);

        setCompleted(true);
    }
    
   
}
