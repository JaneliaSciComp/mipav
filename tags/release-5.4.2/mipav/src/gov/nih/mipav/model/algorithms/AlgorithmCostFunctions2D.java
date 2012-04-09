package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.ImageRegistrationGPU;

import java.awt.*;

import WildMagic.LibFoundation.Mathematics.Matrix4f;


/**
 * CostFunction - class for specifying optimization function.
 */
public class AlgorithmCostFunctions2D implements AlgorithmOptimizeFunctionBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int CORRELATION_RATIO_SMOOTHED_WGT = 0;

    /** DOCUMENT ME! */
    public static final int CORRELATION_RATIO_SMOOTHED = 1;

    /** DOCUMENT ME! */
    public static final int CORRELATION_RATIO = 2;

    /** DOCUMENT ME! */
    public static final int LEAST_SQUARES_SMOOTHED_WGT = 3;

    /** DOCUMENT ME! */
    public static final int LEAST_SQUARES_SMOOTHED = 4;

    /** DOCUMENT ME! */
    public static final int LEAST_SQUARES = 5;

    /** DOCUMENT ME! */
    public static final int MUTUAL_INFORMATION_SMOOTHED_WGT = 6;

    /** DOCUMENT ME! */
    public static final int MUTUAL_INFORMATION_SMOOTHED = 7;

    /** DOCUMENT ME! */
    public static final int MUTUAL_INFORMATION = 8;

    /** DOCUMENT ME! */
    public static final int NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT = 9;

    /** DOCUMENT ME! */
    public static final int NORMALIZED_MUTUAL_INFORMATION_SMOOTHED = 10;

    /** DOCUMENT ME! */
    public static final int NORMALIZED_MUTUAL_INFORMATION = 11;

    /** DOCUMENT ME! */
    public static final int NORMALIZED_XCORRELATION_SMOOTHED_WGT = 12;

    /** DOCUMENT ME! */
    public static final int NORMALIZED_XCORRELATION_SINC = 13;

    /** DOCUMENT ME! */
    public static final int NORMALIZED_XCORRELATION_SMOOTHED = 14;

    /** DOCUMENT ME! */
    public static final int NORMALIZED_XCORRELATION = 15;

    /** DOCUMENT ME! */
    public static final int LEAST_SQUARES_SMOOTHED_WGT_COLOR = 16;

    /** DOCUMENT ME! */
    public static final int LEAST_SQUARES_SMOOTHED_COLOR = 17;

    /** DOCUMENT ME! */
    public static final int LEAST_SQUARES_COLOR = 18;
    public static final int NORMALIZED_MUTUAL_INFORMATION_GPU = 19;
    public static final int NORMALIZED_MUTUAL_INFORMATION_GPU_LM = 20;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
//    private double aT00, aT10;

    /** DOCUMENT ME! */
    private int costCalled = 0;

    /** DOCUMENT ME! */
    private int costFunctID = CORRELATION_RATIO_SMOOTHED;

    /** DOCUMENT ME! */
    private ModelSimpleImage inputImage;

    /** DOCUMENT ME! */
    private ModelSimpleImage inputWgtImage = null;

    /** DOCUMENT ME! */
//    private double iT00, iT10;

    /** DOCUMENT ME! */
//    private double[] jointHist;

    /** DOCUMENT ME! */
//    private double[] margHistI;

    /** DOCUMENT ME! */
//    private double[] margHistR;

    /** DOCUMENT ME! */
//    private Point minMaxPt = new Point();

    /** DOCUMENT ME! */
    private int nBins;

    /** DOCUMENT ME! */
//    private double newPtX, newPtY;

    /** DOCUMENT ME! */
//    private double[] numY;

    /** DOCUMENT ME! */
    private double[] pLogP;

    /** DOCUMENT ME! */
    private ModelSimpleImage refImage;

    /** DOCUMENT ME! */
    private ModelSimpleImage refWgtImage = null;

    /** DOCUMENT ME! */
    private float smoothSize;

    /** DOCUMENT ME! */
//    private double[] sumY;

    /** DOCUMENT ME! */
//    private double[] sumY2;

    /** DOCUMENT ME! */
//    private double T00, T01, T02, T10, T11, T12;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private int xEnd;

    /** DOCUMENT ME! */
    private double xEnd2;

    /** DOCUMENT ME! */
//    private double[][] xfrm;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private int yEnd;

    /** DOCUMENT ME! */
    private double yEnd2;

    private ImageRegistrationGPU m_kGPUCost = null;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmCostFunctions2D object.
     *
     * @param  rImage      DOCUMENT ME!
     * @param  iImage      DOCUMENT ME!
     * @param  functionID  DOCUMENT ME!
     * @param  nBins       DOCUMENT ME!
     * @param  smoothSize  DOCUMENT ME!
     */
    public AlgorithmCostFunctions2D(ModelSimpleImage rImage, ModelSimpleImage iImage, int functionID, int nBins,
                                    float smoothSize) {

        costFunctID = functionID;
        refImage = rImage;
        inputImage = iImage;
        this.nBins = nBins;
        this.smoothSize = smoothSize;

        refImage.calcMinMax();
        inputImage.calcMinMax();

//        setNBins(nBins);

        xDim = inputImage.xDim;
        yDim = inputImage.yDim;

        xEnd2 = xDim - 1.0001;
        yEnd2 = yDim - 1.0001;

        xEnd = refImage.xDim - 1;
        yEnd = refImage.yDim - 1;

        if ((costFunctID >= MUTUAL_INFORMATION_SMOOTHED) && (costFunctID <= NORMALIZED_MUTUAL_INFORMATION)) {
            setPLogP(nBins); // precalculate
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    public int getCostFunction()
    {
        return costFunctID;
    }
    
    
    /**
     * Not implemented in this class.
     *
     * @param   data  Array of data.
     *
     * @return  Double.MAX_VALUE
     */
    public double cost(double[] data) {
        return Double.MAX_VALUE;
    }

    /**
     * Calculates the cost (dependent on the selected cost function) based on the reference image and the input image.
     *
     * @param   affMatrix  Transformation matrix to test cost of.
     *
     * @return  Cost at a supplied transformation.
     */
    public double cost(TransMatrix affMatrix) {

        costCalled++; // global debuggin variable to keep track of how many times cost function was called.
        
        //System.err.println( "Cost fn: " + affMatrix.ToString() );
        double value = 0;

        switch (costFunctID) {

            case CORRELATION_RATIO_SMOOTHED_WGT:
                value = correlationRatioSmoothedWgt(affMatrix);
                break;

            case CORRELATION_RATIO_SMOOTHED:
                value = correlationRatioSmoothed(affMatrix);
                break;

            case CORRELATION_RATIO:
                value = correlationRatio(affMatrix);
                break;

            case LEAST_SQUARES:
                value = leastSquares(affMatrix);
                break;

            case LEAST_SQUARES_SMOOTHED:
                value = leastSquaresSmoothed(affMatrix);
                break;

            case LEAST_SQUARES_SMOOTHED_WGT:
                value = leastSquaresSmoothedWgt(affMatrix);
                break;

            case LEAST_SQUARES_COLOR:
                value = leastSquaresColor(affMatrix);
                break;

            case LEAST_SQUARES_SMOOTHED_COLOR:
                value = leastSquaresSmoothedColor(affMatrix);
                break;

            case LEAST_SQUARES_SMOOTHED_WGT_COLOR:
                value = leastSquaresSmoothedWgtColor(affMatrix);
                break;

            case NORMALIZED_XCORRELATION_SMOOTHED_WGT:
                value = normalizedXCorrelationSmoothedWgt(affMatrix);
                break;

            case NORMALIZED_XCORRELATION_SMOOTHED:
                value = normalizedXCorrelationSmoothed(affMatrix);
                break;

            case NORMALIZED_XCORRELATION:
                value = normalizedXCorrelation(affMatrix);
                break;

            case MUTUAL_INFORMATION:
                value = mutualInformation(affMatrix);
                break;

            case NORMALIZED_MUTUAL_INFORMATION:
                value = normalizedMutualInformation(affMatrix);
                break;

            case MUTUAL_INFORMATION_SMOOTHED:
                value = mutualInformationSmoothed(affMatrix);
                break;

            case NORMALIZED_MUTUAL_INFORMATION_SMOOTHED:
                value = normalizedMutualInformationSmoothed(affMatrix);
                if ( m_kGPUCost != null )
                {
                    value = normalizedMutualInformation(affMatrix);
                    m_kGPUCost.calcError(affMatrix);
                    double valueGPU = m_kGPUCost.getError();
                    if ( Math.abs( valueGPU - value ) > 0.01 )
                    {
                        //System.err.println( "CPU: " + value + "   GPU: " + valueGPU );
                    }
                    //value = valueGPU;

                }
                break;

            case NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT:
                value = normalizedMutualInformationSmoothedWgt(affMatrix);
                break;                
            case NORMALIZED_MUTUAL_INFORMATION_GPU:
                if ( m_kGPUCost != null )
                {
                    m_kGPUCost.calcError(affMatrix);
                    value = m_kGPUCost.getError();   
                }
                break;
            case NORMALIZED_MUTUAL_INFORMATION_GPU_LM:
                if ( m_kGPUCost != null )
                {
                    m_kGPUCost.calcError(affMatrix);
                    value = m_kGPUCost.getError();   
                }
                break;
        }

        return value;
    }
    
    /**
     * Calculates the cost (dependent on the selected cost function) based on the reference image and the input image.
     *
     * @param   affMatrix  Transformation matrix to test cost of.
     *
     * @return  Cost at a supplied transformation.
     */
    public double cost(TransMatrixd affMatrix) {

        costCalled++; // global debuggin variable to keep track of how many times cost function was called.
        
        //System.err.println( "Cost fn: " + affMatrix.ToString() );
        double value = 0;

        switch (costFunctID) {

            case CORRELATION_RATIO_SMOOTHED_WGT:
                value = correlationRatioSmoothedWgt(affMatrix);
                break;

            case CORRELATION_RATIO_SMOOTHED:
                value = correlationRatioSmoothed(affMatrix);
                break;

            case CORRELATION_RATIO:
                value = correlationRatio(affMatrix);
                break;

            case LEAST_SQUARES:
                value = leastSquares(affMatrix);
                break;

            case LEAST_SQUARES_SMOOTHED:
                value = leastSquaresSmoothed(affMatrix);
                break;

            case LEAST_SQUARES_SMOOTHED_WGT:
                value = leastSquaresSmoothedWgt(affMatrix);
                break;

            case LEAST_SQUARES_COLOR:
                value = leastSquaresColor(affMatrix);
                break;

            case LEAST_SQUARES_SMOOTHED_COLOR:
                value = leastSquaresSmoothedColor(affMatrix);
                break;

            case LEAST_SQUARES_SMOOTHED_WGT_COLOR:
                value = leastSquaresSmoothedWgtColor(affMatrix);
                break;

            case NORMALIZED_XCORRELATION_SMOOTHED_WGT:
                value = normalizedXCorrelationSmoothedWgt(affMatrix);
                break;

            case NORMALIZED_XCORRELATION_SMOOTHED:
                value = normalizedXCorrelationSmoothed(affMatrix);
                break;

            case NORMALIZED_XCORRELATION:
                value = normalizedXCorrelation(affMatrix);
                break;

            case MUTUAL_INFORMATION:
                value = mutualInformation(affMatrix);
                break;

            case NORMALIZED_MUTUAL_INFORMATION:
                value = normalizedMutualInformation(affMatrix);
                break;

            case MUTUAL_INFORMATION_SMOOTHED:
                value = mutualInformationSmoothed(affMatrix);
                break;

            case NORMALIZED_MUTUAL_INFORMATION_SMOOTHED:
                value = normalizedMutualInformationSmoothed(affMatrix);
                if ( m_kGPUCost != null )
                {
                    value = normalizedMutualInformation(affMatrix);
                    m_kGPUCost.calcError(affMatrix);
                    double valueGPU = m_kGPUCost.getError();
                    if ( Math.abs( valueGPU - value ) > 0.01 )
                    {
                        //System.err.println( "CPU: " + value + "   GPU: " + valueGPU );
                    }
                    //value = valueGPU;

                }
                break;

            case NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT:
                value = normalizedMutualInformationSmoothedWgt(affMatrix);
                break;                
            case NORMALIZED_MUTUAL_INFORMATION_GPU:
                if ( m_kGPUCost != null )
                {
                    m_kGPUCost.calcError(affMatrix);
                    value = m_kGPUCost.getError();   
                }
                break;
            case NORMALIZED_MUTUAL_INFORMATION_GPU_LM:
                if ( m_kGPUCost != null )
                {
                    m_kGPUCost.calcError(affMatrix);
                    value = m_kGPUCost.getError();   
                }
                break;
        }

        return value;
    }

    /**
     * Sets class global array variables to null and calls the garbage collector.
     */
    public void disposeLocal() {

        refImage = null;
        inputImage = null;
        refWgtImage = null;
        inputWgtImage = null;

        m_kGPUCost = null;
        //System.err.println( "AlgorithmCostFuntions2D.disposeLocal(): " + nBins + " " + costCalled );
//        sumY = null;
//        sumY2 = null;
//        numY = null;
//
//        jointHist = null;
//        margHistR = null;
//        margHistI = null;
        pLogP = null;
        //System.gc();
    }

    /**
     * Calls disposeLocal of this class to ensure this class nulls the references to global class variables so that
     * memory will be recovered.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    public void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }
    
    public void setGPUCost( ImageRegistrationGPU kGPUCost )
    {
        m_kGPUCost = kGPUCost;
    }
    
    public boolean isGPULineMin()
    {
        if ( m_kGPUCost != null )
            return true;
        return false;
    }

    public float[] lineMin(Matrix4f kToOrigin, Matrix4f kFromOrigin,
            float rigid, float dim, double[] startPoint, double[] pt, int ptLength,
            double[] unitDirections, double unit_tolerance, double fMinDist,
            double bracketA, double functionA,
            double bracketB, double functionB,
            double bracketC, double functionC
            )
    {
        m_kGPUCost.initLineMin( kToOrigin,  kFromOrigin,
                rigid,  dim, startPoint, pt, ptLength,
                unitDirections, unit_tolerance, fMinDist,
                bracketA, functionA,
                bracketB, functionB,
                bracketC, functionC);
        m_kGPUCost.calcLineMinimization();
        return m_kGPUCost.getBracketB();
    }
    
    /**
     * Accessor that returns how many times the cost function has been called.
     *
     * @return  The number of times the cost function has been called.
     */
    public int getCostCalled() {
        return costCalled;
    }

    /**
     * Sets the input weight image. If the weight values are outside the range [0:1] then the weigthts will be remapped
     * to be between 0:1.
     *
     * @param  inputWgtImg  the input weight image
     */
    public void setInputWgtImage(ModelSimpleImage inputWgtImg) {
        inputWgtImage = inputWgtImg;

        float diff;

        if (inputWgtImage == null) {
            return;
        }

        inputWgtImage.calcMinMax();

        if ((inputWgtImage.min < 0) || (inputWgtImage.max > 1)) {

            // remap data - normalize data between 0 and 1
            if (inputWgtImage.min != inputWgtImage.max) {
                diff = inputWgtImage.max - inputWgtImage.min;

                for (int i = 0; i < inputWgtImage.data.length; i++) {
                    inputWgtImage.data[i] = (inputWgtImage.data[i] - inputWgtImage.min) / diff;
                }
            }

            inputWgtImage.calcMinMax();
        }

    }

    /**
     * Sets the number of bins used in remapping statistical costs.
     *
     * @param  nBins  DOCUMENT ME!
     */
//    public void setNBins(int nBins) {
//
//        sumY = null;
//        sumY2 = null;
//        numY = null;
//        System.gc();
//
//        try {
//            sumY = new double[nBins];
//            sumY2 = new double[nBins];
//            numY = new double[nBins];
//        } catch (OutOfMemoryError error) {
//            System.gc();
//            MipavUtil.displayError("Out of memory: CostFunctions.setBins");
//
//            return;
//        }
//    }

    /**
     * Sets the reference weight image. If the weight values are outside the range [0:1] then the weigthts will be
     * remapped to be between 0:1.
     *
     * @param  refWgtImg  the reference weight image
     */
    public void setRefWgtImage(ModelSimpleImage refWgtImg) {
        refWgtImage = refWgtImg;

        float diff;

        if (refWgtImage == null) {
            return;
        }

        refWgtImage.calcMinMax();

        if ((refWgtImage.min < 0) || (refWgtImage.max > 1)) {

            // remap data - normalize data between 0 and 1
            if (refWgtImage.min != refWgtImage.max) {
                diff = refWgtImage.max - refWgtImage.min;

                for (int i = 0; i < refWgtImage.data.length; i++) {
                    refWgtImage.data[i] = (refWgtImage.data[i] - refWgtImage.min) / diff;
                }
            }

            refWgtImage.calcMinMax();
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  tMatrix       DOCUMENT ME!
     * @param  jointEntropy  DOCUMENT ME!
     * @param  margEntropyR  DOCUMENT ME!
     * @param  margEntropyI  DOCUMENT ME!
     */
    private void calcEntropy(TransMatrix tMatrix, double[] jointEntropy, double[] margEntropyR, double[] margEntropyI) {

        int x, y;
        int index, indexValueR, indexValueI;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;
        double value;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }
        double[] jointHist = new double[nBins*nBins];
        double[] margHistI = new double[nBins];
        double[] margHistR = new double[nBins];
        for (int i = 0; i < (nBins * nBins); i++) {
            jointHist[i] = 0;
        }

        for (int i = 0; i < nBins; i++) {
            margHistI[i] = 0;
            margHistR[i] = 0;
        }

        double constantR = 1;

        if ((refImage.max - refImage.min) != 0) {
            constantR = (nBins - 1) / (refImage.max - refImage.min);
        }

        double constantI = 1;

        if ((inputImage.max - inputImage.min) != 0) {
            constantI = (nBins - 1) / (inputImage.max - inputImage.min);
        }
        
        //System.err.println( refImage.min + " " + constantR );

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                indexValueI = (int) ((value - inputImage.min) * constantI);
                indexValueR = (int) ((refImage.data[index] - refImage.min) * constantR);
                jointHist[(indexValueR * nBins) + indexValueI] += 1;
                margHistR[indexValueR] += 1;
                margHistI[indexValueI] += 1;

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        // note that the pLogP values indexed by integers such that: pLogP(n) = n/N * log(n/N)
        double p = 0.0;
        int n = 0;
        int pSize = pLogP.length;
        int nVoxels = refImage.data.length;
        int jLength = (nBins * nBins);

        for (int i = 0; i < jLength; i++) {
            n = (int) MipavMath.round(jointHist[i]);

            if (n > 0) {

                if (n < pSize) {
                    jointEntropy[0] += pLogP[n];
                } else {
                    p = ((double) n) / ((double) nVoxels);
                    jointEntropy[0] += -p * Math.log(p);
                }
            }
        }

        //System.err.println( "");
        //System.err.println( "");
        //System.err.println( "");
        //System.err.println( "Reference Image: ");
        int iSum = 0;
        for (int i = 0; i < nBins; i++) {
            n = (int) MipavMath.round(margHistR[i]);
            //System.err.println( i + " " + margHistR[i] );
            if (n > 0) {

                iSum += n;
                if (n < pSize) {
                    margEntropyR[0] += pLogP[n];
                } else {
                    p = ((double) n) / ((double) nVoxels);
                    margEntropyR[0] += -p * Math.log(p);
                }
            }
        }
        //System.err.println( "DONE " + iSum);

        int nOverlap = 0;


        //System.err.println( "");
        //System.err.println( "");
        //System.err.println( "");
        //System.err.println( "Moving Image: ");
        for (int i = 0; i < nBins; i++) {
            n = (int) MipavMath.round(margHistI[i]);
            //System.err.println( i + " " + margHistI[i] );

            if (n > 0) {
                nOverlap += n;

                if (n < pSize) {
                    margEntropyI[0] += pLogP[n];
                } else {
                    p = ((double) n) / ((double) nVoxels);
                    margEntropyI[0] += -p * Math.log(p);
                }
            }
        }
        //System.err.println( "DONE " + nOverlap);

        //System.err.println( "CPU: " + nBins + " " + nVoxels + " " + nOverlap + " " + margEntropyR[0] + " " + margEntropyI[0] + " " + jointEntropy[0] );
        
        // correct for difference in total histogram size
        // that is: nOverlap vs nVoxels
        // H_1 = N_0/N_1 * H_0 + log(N_1/N_0)
        // = N_0/N_1 * H_0 - log(N_0/N_1)
        if (nOverlap > 1000) {
            double nRatio = ((double) nVoxels) / ((double) nOverlap);

            jointEntropy[0] = (nRatio * jointEntropy[0]) - Math.log(nRatio);
            margEntropyR[0] = (nRatio * margEntropyR[0]) - Math.log(nRatio);
            margEntropyI[0] = (nRatio * margEntropyI[0]) - Math.log(nRatio);
        } else {

            // Put in maximum entropy values as base cases = BAD registration
            jointEntropy[0] = 2.0 * Math.log(nBins);
            margEntropyR[0] = Math.log(nBins);
            margEntropyI[0] = Math.log(nBins);
        }
        return;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  tMatrix       DOCUMENT ME!
     * @param  jointEntropy  DOCUMENT ME!
     * @param  margEntropyR  DOCUMENT ME!
     * @param  margEntropyI  DOCUMENT ME!
     */
    private void calcEntropy(TransMatrixd tMatrix, double[] jointEntropy, double[] margEntropyR, double[] margEntropyI) {

        int x, y;
        int index, indexValueR, indexValueI;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;
        double value;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }
        double[] jointHist = new double[nBins*nBins];
        double[] margHistI = new double[nBins];
        double[] margHistR = new double[nBins];
        for (int i = 0; i < (nBins * nBins); i++) {
            jointHist[i] = 0;
        }

        for (int i = 0; i < nBins; i++) {
            margHistI[i] = 0;
            margHistR[i] = 0;
        }

        double constantR = 1;

        if ((refImage.max - refImage.min) != 0) {
            constantR = (nBins - 1) / (refImage.max - refImage.min);
        }

        double constantI = 1;

        if ((inputImage.max - inputImage.min) != 0) {
            constantI = (nBins - 1) / (inputImage.max - inputImage.min);
        }
        
        //System.err.println( refImage.min + " " + constantR );

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                indexValueI = (int) ((value - inputImage.min) * constantI);
                indexValueR = (int) ((refImage.data[index] - refImage.min) * constantR);
                jointHist[(indexValueR * nBins) + indexValueI] += 1;
                margHistR[indexValueR] += 1;
                margHistI[indexValueI] += 1;

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        // note that the pLogP values indexed by integers such that: pLogP(n) = n/N * log(n/N)
        double p = 0.0;
        int n = 0;
        int pSize = pLogP.length;
        int nVoxels = refImage.data.length;
        int jLength = (nBins * nBins);

        for (int i = 0; i < jLength; i++) {
            n = (int) MipavMath.round(jointHist[i]);

            if (n > 0) {

                if (n < pSize) {
                    jointEntropy[0] += pLogP[n];
                } else {
                    p = ((double) n) / ((double) nVoxels);
                    jointEntropy[0] += -p * Math.log(p);
                }
            }
        }

        //System.err.println( "");
        //System.err.println( "");
        //System.err.println( "");
        //System.err.println( "Reference Image: ");
        int iSum = 0;
        for (int i = 0; i < nBins; i++) {
            n = (int) MipavMath.round(margHistR[i]);
            //System.err.println( i + " " + margHistR[i] );
            if (n > 0) {

                iSum += n;
                if (n < pSize) {
                    margEntropyR[0] += pLogP[n];
                } else {
                    p = ((double) n) / ((double) nVoxels);
                    margEntropyR[0] += -p * Math.log(p);
                }
            }
        }
        //System.err.println( "DONE " + iSum);

        int nOverlap = 0;


        //System.err.println( "");
        //System.err.println( "");
        //System.err.println( "");
        //System.err.println( "Moving Image: ");
        for (int i = 0; i < nBins; i++) {
            n = (int) MipavMath.round(margHistI[i]);
            //System.err.println( i + " " + margHistI[i] );

            if (n > 0) {
                nOverlap += n;

                if (n < pSize) {
                    margEntropyI[0] += pLogP[n];
                } else {
                    p = ((double) n) / ((double) nVoxels);
                    margEntropyI[0] += -p * Math.log(p);
                }
            }
        }
        //System.err.println( "DONE " + nOverlap);

        //System.err.println( "CPU: " + nBins + " " + nVoxels + " " + nOverlap + " " + margEntropyR[0] + " " + margEntropyI[0] + " " + jointEntropy[0] );
        
        // correct for difference in total histogram size
        // that is: nOverlap vs nVoxels
        // H_1 = N_0/N_1 * H_0 + log(N_1/N_0)
        // = N_0/N_1 * H_0 - log(N_0/N_1)
        if (nOverlap > 1000) {
            double nRatio = ((double) nVoxels) / ((double) nOverlap);

            jointEntropy[0] = (nRatio * jointEntropy[0]) - Math.log(nRatio);
            margEntropyR[0] = (nRatio * margEntropyR[0]) - Math.log(nRatio);
            margEntropyI[0] = (nRatio * margEntropyI[0]) - Math.log(nRatio);
        } else {

            // Put in maximum entropy values as base cases = BAD registration
            jointEntropy[0] = 2.0 * Math.log(nBins);
            margEntropyR[0] = Math.log(nBins);
            margEntropyI[0] = Math.log(nBins);
        }
        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tMatrix       DOCUMENT ME!
     * @param  jointEntropy  DOCUMENT ME!
     * @param  margEntropyR  DOCUMENT ME!
     * @param  margEntropyI  DOCUMENT ME!
     */
    private void calcEntropySmoothed(TransMatrix tMatrix, double[] jointEntropy, double[] margEntropyR,
                                     double[] margEntropyI) {

        int x, y;
        int index, indexValueR;
        int iCenter;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;
        double value;

        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        double[] jointHist = new double[nBins*nBins];
        double[] margHistI = new double[nBins];
        double[] margHistR = new double[nBins];

        for (int i = 0; i < (nBins * nBins); i++) {
            jointHist[i] = 0;
        }

        for (int i = 0; i < nBins; i++) {
            margHistI[i] = 0;
            margHistR[i] = 0;
        }

        double constantR = 1;

        if ((refImage.max - refImage.min) != 0) {
            constantR = (nBins - 1) / (refImage.max - refImage.min);
        }

        double constantI = 1;

        if ((inputImage.max - inputImage.min) != 0) {
            constantI = (nBins - 1) / (inputImage.max - inputImage.min);
        }

        Point minMaxPt = new Point();

        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                weight = 1.0;
                iCenter = (int) ((value - inputImage.min) * constantI);
                indexValueR = (int) ((refImage.data[index] - refImage.min) * constantR);

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                if (iCenter >= nBins) {
                    iCenter = nBins - 1;
                }

                if (iCenter < 0) {
                    iCenter = 0;
                }

                jointHist[(indexValueR * nBins) + iCenter] += weight;
                margHistI[iCenter] += weight;
                margHistR[indexValueR] += weight;

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double p = 0.0;
        double n = 0.0;
        int nVoxels = refImage.data.length;

        for (int i = 0; i < (nBins * nBins); i++) {
            n = jointHist[i];

            if (n > 0.0) {
                p = n / ((double) nVoxels);
                jointEntropy[0] += -p * Math.log(p);
            }
        }

        for (int i = 0; i < nBins; i++) {
            n = margHistR[i];

            if (n > 0.0) {
                p = n / ((double) nVoxels);
                margEntropyR[0] += -p * Math.log(p);
            }
        }

        double nOverlap = 0.0;

        for (int i = 0; i < nBins; i++) {
            n = margHistI[i];

            if (n > 0.0) {
                nOverlap += n;
                p = n / ((double) nVoxels);
                margEntropyI[0] += -p * Math.log(p);
            }
        }

        // correct for difference in total histogram size
        // that is: nOverlap vs nVoxels
        // H_1 = N_0/N_1 * H_0 + log(N_1/N_0)
        // = N_0/N_1 * H_0 - log(N_0/N_1)
        if (nOverlap > 1000) {
            double nRatio = ((double) nVoxels) / ((double) nOverlap);

            jointEntropy[0] = (nRatio * jointEntropy[0]) - Math.log(nRatio);
            margEntropyR[0] = (nRatio * margEntropyR[0]) - Math.log(nRatio);
            margEntropyI[0] = (nRatio * margEntropyI[0]) - Math.log(nRatio);
        } else {

            // Put in maximum entropy values as base cases = BAD registration
            jointEntropy[0] = 2.0 * Math.log(nBins);
            margEntropyR[0] = Math.log(nBins);
            margEntropyI[0] = Math.log(nBins);
        }

        return;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  tMatrix       DOCUMENT ME!
     * @param  jointEntropy  DOCUMENT ME!
     * @param  margEntropyR  DOCUMENT ME!
     * @param  margEntropyI  DOCUMENT ME!
     */
    private void calcEntropySmoothed(TransMatrixd tMatrix, double[] jointEntropy, double[] margEntropyR,
                                     double[] margEntropyI) {

        int x, y;
        int index, indexValueR;
        int iCenter;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;
        double value;

        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        double[] jointHist = new double[nBins*nBins];
        double[] margHistI = new double[nBins];
        double[] margHistR = new double[nBins];

        for (int i = 0; i < (nBins * nBins); i++) {
            jointHist[i] = 0;
        }

        for (int i = 0; i < nBins; i++) {
            margHistI[i] = 0;
            margHistR[i] = 0;
        }

        double constantR = 1;

        if ((refImage.max - refImage.min) != 0) {
            constantR = (nBins - 1) / (refImage.max - refImage.min);
        }

        double constantI = 1;

        if ((inputImage.max - inputImage.min) != 0) {
            constantI = (nBins - 1) / (inputImage.max - inputImage.min);
        }

        Point minMaxPt = new Point();

        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                weight = 1.0;
                iCenter = (int) ((value - inputImage.min) * constantI);
                indexValueR = (int) ((refImage.data[index] - refImage.min) * constantR);

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                if (iCenter >= nBins) {
                    iCenter = nBins - 1;
                }

                if (iCenter < 0) {
                    iCenter = 0;
                }

                jointHist[(indexValueR * nBins) + iCenter] += weight;
                margHistI[iCenter] += weight;
                margHistR[indexValueR] += weight;

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double p = 0.0;
        double n = 0.0;
        int nVoxels = refImage.data.length;

        for (int i = 0; i < (nBins * nBins); i++) {
            n = jointHist[i];

            if (n > 0.0) {
                p = n / ((double) nVoxels);
                jointEntropy[0] += -p * Math.log(p);
            }
        }

        for (int i = 0; i < nBins; i++) {
            n = margHistR[i];

            if (n > 0.0) {
                p = n / ((double) nVoxels);
                margEntropyR[0] += -p * Math.log(p);
            }
        }

        double nOverlap = 0.0;

        for (int i = 0; i < nBins; i++) {
            n = margHistI[i];

            if (n > 0.0) {
                nOverlap += n;
                p = n / ((double) nVoxels);
                margEntropyI[0] += -p * Math.log(p);
            }
        }

        // correct for difference in total histogram size
        // that is: nOverlap vs nVoxels
        // H_1 = N_0/N_1 * H_0 + log(N_1/N_0)
        // = N_0/N_1 * H_0 - log(N_0/N_1)
        if (nOverlap > 1000) {
            double nRatio = ((double) nVoxels) / ((double) nOverlap);

            jointEntropy[0] = (nRatio * jointEntropy[0]) - Math.log(nRatio);
            margEntropyR[0] = (nRatio * margEntropyR[0]) - Math.log(nRatio);
            margEntropyI[0] = (nRatio * margEntropyI[0]) - Math.log(nRatio);
        } else {

            // Put in maximum entropy values as base cases = BAD registration
            jointEntropy[0] = 2.0 * Math.log(nBins);
            margEntropyR[0] = Math.log(nBins);
            margEntropyI[0] = Math.log(nBins);
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tMatrix       DOCUMENT ME!
     * @param  jointEntropy  DOCUMENT ME!
     * @param  margEntropyR  DOCUMENT ME!
     * @param  margEntropyI  DOCUMENT ME!
     */
    private void calcEntropySmoothedWgt(TransMatrix tMatrix, double[] jointEntropy, double[] margEntropyR,
                                        double[] margEntropyI) {

        int x, y;
        int index, indexValueR;
        int iCenter;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;
        double value, wValue;

        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        double[] jointHist = new double[nBins * nBins];
        double[] margHistR = new double[nBins];
        double[] margHistI = new double[nBins];
 
        for (int i = 0; i < (nBins * nBins); i++) {
            jointHist[i] = 0;
        }

        for (int i = 0; i < nBins; i++) {
            margHistI[i] = 0;
            margHistR[i] = 0;
        }

        double constantR = 1;

        if ((refImage.max - refImage.min) != 0) {
            constantR = (nBins - 1) / (refImage.max - refImage.min);
        }

        double constantI = 1;

        if ((inputImage.max - inputImage.min) != 0) {
            constantI = (nBins - 1) / (inputImage.max - inputImage.min);
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                b1 = (dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]);
                b2 = (dx1 * inputWgtImage.data[position1 + xDim]) + (dx * inputWgtImage.data[position11 + xDim]);
                wValue = (dy1 * b1) + (dy * b2);

                weight = wValue * refWgtImage.data[index];

                iCenter = (int) ((value - inputImage.min) * constantI);
                indexValueR = (int) ((refImage.data[index] - refImage.min) * constantR);

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                if (iCenter >= nBins) {
                    iCenter = nBins - 1;
                }

                if (iCenter < 0) {
                    iCenter = 0;
                }

                jointHist[(indexValueR * nBins) + iCenter] += weight;
                margHistI[iCenter] += weight;
                margHistR[indexValueR] += weight;

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double p = 0.0;
        double n = 0.0;
        int nVoxels = refImage.data.length;

        for (int i = 0; i < (nBins * nBins); i++) {
            n = jointHist[i];

            if (n > 0.0) {
                p = n / nVoxels;
                jointEntropy[0] += -p * Math.log(p);
            }
        }

        for (int i = 0; i < nBins; i++) {
            n = margHistR[i];

            if (n > 0.0) {
                p = n / nVoxels;
                margEntropyR[0] += -p * Math.log(p);
            }
        }

        double nOverlap = 0.0;

        for (int i = 0; i < nBins; i++) {
            n = margHistI[i];

            if (n > 0.0) {
                nOverlap += n;
                p = n / nVoxels;
                margEntropyI[0] += -p * Math.log(p);
            }
        }

        // correct for difference in total histogram size
        // that is: nOverlap vs nVoxels
        // H_1 = N_0/N_1 * H_0 + log(N_1/N_0)
        // = N_0/N_1 * H_0 - log(N_0/N_1)
        // This separation number of 5.0 may have to be tweaked depending
        // on the size of the weighted areas.

        if (nOverlap > 5.0) {
            double nRatio = ((double) nVoxels) / ((double) nOverlap);

            jointEntropy[0] = (nRatio * jointEntropy[0]) - Math.log(nRatio);
            margEntropyR[0] = (nRatio * margEntropyR[0]) - Math.log(nRatio);
            margEntropyI[0] = (nRatio * margEntropyI[0]) - Math.log(nRatio);
        } else {

            // Put in maximum entropy values as base cases = BAD registration
            jointEntropy[0] = 2.0 * Math.log(nBins);
            margEntropyR[0] = Math.log(nBins);
            margEntropyI[0] = Math.log(nBins);
        }

        return;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  tMatrix       DOCUMENT ME!
     * @param  jointEntropy  DOCUMENT ME!
     * @param  margEntropyR  DOCUMENT ME!
     * @param  margEntropyI  DOCUMENT ME!
     */
    private void calcEntropySmoothedWgt(TransMatrixd tMatrix, double[] jointEntropy, double[] margEntropyR,
                                        double[] margEntropyI) {

        int x, y;
        int index, indexValueR;
        int iCenter;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;
        double value, wValue;

        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        double[] jointHist = new double[nBins * nBins];
        double[] margHistR = new double[nBins];
        double[] margHistI = new double[nBins];
 
        for (int i = 0; i < (nBins * nBins); i++) {
            jointHist[i] = 0;
        }

        for (int i = 0; i < nBins; i++) {
            margHistI[i] = 0;
            margHistR[i] = 0;
        }

        double constantR = 1;

        if ((refImage.max - refImage.min) != 0) {
            constantR = (nBins - 1) / (refImage.max - refImage.min);
        }

        double constantI = 1;

        if ((inputImage.max - inputImage.min) != 0) {
            constantI = (nBins - 1) / (inputImage.max - inputImage.min);
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                b1 = (dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]);
                b2 = (dx1 * inputWgtImage.data[position1 + xDim]) + (dx * inputWgtImage.data[position11 + xDim]);
                wValue = (dy1 * b1) + (dy * b2);

                weight = wValue * refWgtImage.data[index];

                iCenter = (int) ((value - inputImage.min) * constantI);
                indexValueR = (int) ((refImage.data[index] - refImage.min) * constantR);

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                if (iCenter >= nBins) {
                    iCenter = nBins - 1;
                }

                if (iCenter < 0) {
                    iCenter = 0;
                }

                jointHist[(indexValueR * nBins) + iCenter] += weight;
                margHistI[iCenter] += weight;
                margHistR[indexValueR] += weight;

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double p = 0.0;
        double n = 0.0;
        int nVoxels = refImage.data.length;

        for (int i = 0; i < (nBins * nBins); i++) {
            n = jointHist[i];

            if (n > 0.0) {
                p = n / nVoxels;
                jointEntropy[0] += -p * Math.log(p);
            }
        }

        for (int i = 0; i < nBins; i++) {
            n = margHistR[i];

            if (n > 0.0) {
                p = n / nVoxels;
                margEntropyR[0] += -p * Math.log(p);
            }
        }

        double nOverlap = 0.0;

        for (int i = 0; i < nBins; i++) {
            n = margHistI[i];

            if (n > 0.0) {
                nOverlap += n;
                p = n / nVoxels;
                margEntropyI[0] += -p * Math.log(p);
            }
        }

        // correct for difference in total histogram size
        // that is: nOverlap vs nVoxels
        // H_1 = N_0/N_1 * H_0 + log(N_1/N_0)
        // = N_0/N_1 * H_0 - log(N_0/N_1)
        // This separation number of 5.0 may have to be tweaked depending
        // on the size of the weighted areas.

        if (nOverlap > 5.0) {
            double nRatio = ((double) nVoxels) / ((double) nOverlap);

            jointEntropy[0] = (nRatio * jointEntropy[0]) - Math.log(nRatio);
            margEntropyR[0] = (nRatio * margEntropyR[0]) - Math.log(nRatio);
            margEntropyI[0] = (nRatio * margEntropyI[0]) - Math.log(nRatio);
        } else {

            // Put in maximum entropy values as base cases = BAD registration
            jointEntropy[0] = 2.0 * Math.log(nBins);
            margEntropyR[0] = Math.log(nBins);
            margEntropyI[0] = Math.log(nBins);
        }

        return;
    }

    /**
     * Correlation ratio cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double correlationRatio(TransMatrix tMatrix) {

        int x, y;
        int index, indexValue;
        double value;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double[] numY = new double[nBins];
        double[] sumY = new double[nBins];
        double[] sumY2 = new double[nBins];

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        double constant = 1;

        if ((refImage.max - refImage.min) != 0) {
            constant = (nBins - 1) / (refImage.max - refImage.min);
        }

        int nCalcs = 0;

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {
                nCalcs++;
                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                indexValue = (int) ((refImage.data[index] - refImage.min) * constant);
                numY[indexValue] += 1;
                sumY[indexValue] += (value - inputImage.min);
                sumY2[indexValue] += (value - inputImage.min) * (value - inputImage.min);

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double corrRatio = 0.0;
        double variance = 0.0;
        double totSumY = 0.0;
        double totSumY2 = 0.0;
        double numTotY = 0.0;

        // now calculate the individual variances for each iso-set
        // weighting them by the number of pixels from Image x that contribute
        for (int b = 0; b < nBins; b++) {

            if (numY[b] > 2.0) {
                numTotY += numY[b];
                totSumY += sumY[b];
                totSumY2 += sumY2[b];

                // the following should be the variance of the bth iso-subset
                variance = (sumY2[b] - (sumY[b] * sumY[b] / numY[b])) / (numY[b] - 1);
                corrRatio += variance * numY[b];
            }
        }

        // normalise the weighting of numy[]
        if (numTotY > 0) {
            corrRatio /= numTotY;
        }

        // penalizes cost based on the number of cost calculations over the
        // total possible if the number of cost calculation is less than 1000.
        int totCalcs = yEnd * xEnd;

        if (nCalcs < (0.25 * totCalcs)) {
            corrRatio = corrRatio + ((1.0 - corrRatio) * ((totCalcs - nCalcs) / (double) totCalcs));
        }

        // calculate the total variance of Image y and then normalise by this
        if (numTotY > 1) {
            variance = (totSumY2 - (totSumY * totSumY / numTotY)) / (numTotY - 1);
        }

        if (variance > 0.0) {
            corrRatio /= variance;
        }

        if ((numTotY <= 1) || (variance <= 0.0)) {
            return 1.0;
        } // the totally uncorrelated condition
        else {
            return (corrRatio);
        }
    }
    
    /**
     * Correlation ratio cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double correlationRatio(TransMatrixd tMatrix) {

        int x, y;
        int index, indexValue;
        double value;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double[] numY = new double[nBins];
        double[] sumY = new double[nBins];
        double[] sumY2 = new double[nBins];

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        double constant = 1;

        if ((refImage.max - refImage.min) != 0) {
            constant = (nBins - 1) / (refImage.max - refImage.min);
        }

        int nCalcs = 0;

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {
                nCalcs++;
                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                indexValue = (int) ((refImage.data[index] - refImage.min) * constant);
                numY[indexValue] += 1;
                sumY[indexValue] += (value - inputImage.min);
                sumY2[indexValue] += (value - inputImage.min) * (value - inputImage.min);

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double corrRatio = 0.0;
        double variance = 0.0;
        double totSumY = 0.0;
        double totSumY2 = 0.0;
        double numTotY = 0.0;

        // now calculate the individual variances for each iso-set
        // weighting them by the number of pixels from Image x that contribute
        for (int b = 0; b < nBins; b++) {

            if (numY[b] > 2.0) {
                numTotY += numY[b];
                totSumY += sumY[b];
                totSumY2 += sumY2[b];

                // the following should be the variance of the bth iso-subset
                variance = (sumY2[b] - (sumY[b] * sumY[b] / numY[b])) / (numY[b] - 1);
                corrRatio += variance * numY[b];
            }
        }

        // normalise the weighting of numy[]
        if (numTotY > 0) {
            corrRatio /= numTotY;
        }

        // penalizes cost based on the number of cost calculations over the
        // total possible if the number of cost calculation is less than 1000.
        int totCalcs = yEnd * xEnd;

        if (nCalcs < (0.25 * totCalcs)) {
            corrRatio = corrRatio + ((1.0 - corrRatio) * ((totCalcs - nCalcs) / (double) totCalcs));
        }

        // calculate the total variance of Image y and then normalise by this
        if (numTotY > 1) {
            variance = (totSumY2 - (totSumY * totSumY / numTotY)) / (numTotY - 1);
        }

        if (variance > 0.0) {
            corrRatio /= variance;
        }

        if ((numTotY <= 1) || (variance <= 0.0)) {
            return 1.0;
        } // the totally uncorrelated condition
        else {
            return (corrRatio);
        }
    }

    /**
     * Correlation ratio cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double correlationRatioSmoothed(TransMatrix tMatrix) {

        int x, y;
        int index, indexValue;
        double value;
        double weight;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;
        double tmp;

        // setup
        smoothX = smoothSize;
        smoothY = smoothSize;

        // setup so that inner loops use multiples (faster) instead of divides.
        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        // clear out some variables

        double[] numY = new double[nBins];
        double[] sumY = new double[nBins];
        double[] sumY2 = new double[nBins];

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        // precalculates this constant for rebinning
        double constant = 1;

        if ((refImage.max - refImage.min) != 0) {
            constant = (nBins - 1) / (refImage.max - refImage.min);
        }

        int nCalcs = 0;

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += (minMaxPt.x * T00);
            newPtY += (minMaxPt.x * T10);

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                // make function call to getTrilinear or move code here.
                nCalcs++;
                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = b2 = 0;

                try {
                    b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                    b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                } catch (IndexOutOfBoundsException error) {
                    System.out.println(error);
                    System.out.println(" position1 = " + position1 + " xDim = " + xDim);
                    System.out.println(" imageImage data.length = " + inputImage.data.length);
                    System.out.println("  minMaxPt.x = " + minMaxPt.x);
                    System.out.println("  minMaxPt.y = " + minMaxPt.y);
                    System.out.println("  int X = " + intX + "  int Y " + intY);
                    System.out.println(" x = " + x);
                    System.out.println(" y = " + y);
                    System.out.println(" T00 = " + T00);
                    System.out.println(" T10 = " + T10);

                }

                value = (dy1 * b1) + (dy * b2);

                weight = 1.0;

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                // could move remapping (rebinning) of refImage to the calling function. that way it is only
                // done once - savings - one mult, sub and convert to int. per loop.
                indexValue = (int) ((refImage.data[index] - refImage.min) * constant);
                tmp = weight * (value - inputImage.min); // Added by Matt to handle image with neg values
                numY[indexValue] += weight;
                sumY[indexValue] += tmp;
                sumY2[indexValue] += tmp * (value - inputImage.min);

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double corrRatio = 0.0;
        double variance = 0.0;
        double totSumY = 0.0;
        double totSumY2 = 0.0;
        double numTotY = 0.0;

        // now calculate the individual variances for each iso-set
        // weighting them by the number of pixels from Image x that contribute
        for (int b = 0; b < nBins; b++) {

            if (numY[b] > 2.0) {
                numTotY += numY[b];
                totSumY += sumY[b];
                totSumY2 += sumY2[b];

                // the following should be the variance of the bth iso-subset
                variance = (sumY2[b] - (sumY[b] * sumY[b] / numY[b])) / (numY[b] - 1);
                corrRatio += variance * numY[b];
            }
        }

        // normalise the weighting of numy[]
        if (numTotY > 0) {
            corrRatio /= numTotY;
        }

        // calculate the total variance of Image y and then normalise by this
        if (numTotY > 1) {
            variance = (totSumY2 - (totSumY * totSumY / numTotY)) / (numTotY - 1);
        }

        if (variance > 0.0) {
            corrRatio /= variance;
        }

        // penalizes cost based on the number of cost calculations over the
        // total possible if the number of cost calculation is less than 1000.
        int totCalcs = yEnd * xEnd;

        if (nCalcs < (0.25 * totCalcs)) {
            corrRatio = corrRatio + ((1.0 - corrRatio) * ((totCalcs - nCalcs) / (double) totCalcs));
        }

        if ((numTotY <= 1) || (variance <= 0.0)) {
            return 1.0;
        } // the totally uncorrelated condition
        else {
            return (corrRatio);
        }

        // an alternative is to return 1.0/corr_ratio (=1/(1-correlation ratio))
        // which may be better at rewarding gains near the best solution
    }
    
    /**
     * Correlation ratio cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double correlationRatioSmoothed(TransMatrixd tMatrix) {

        int x, y;
        int index, indexValue;
        double value;
        double weight;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;
        double tmp;

        // setup
        smoothX = smoothSize;
        smoothY = smoothSize;

        // setup so that inner loops use multiples (faster) instead of divides.
        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        // clear out some variables

        double[] numY = new double[nBins];
        double[] sumY = new double[nBins];
        double[] sumY2 = new double[nBins];

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        // precalculates this constant for rebinning
        double constant = 1;

        if ((refImage.max - refImage.min) != 0) {
            constant = (nBins - 1) / (refImage.max - refImage.min);
        }

        int nCalcs = 0;

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += (minMaxPt.x * T00);
            newPtY += (minMaxPt.x * T10);

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                // make function call to getTrilinear or move code here.
                nCalcs++;
                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = b2 = 0;

                try {
                    b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                    b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                } catch (IndexOutOfBoundsException error) {
                    System.out.println(error);
                    System.out.println(" position1 = " + position1 + " xDim = " + xDim);
                    System.out.println(" imageImage data.length = " + inputImage.data.length);
                    System.out.println("  minMaxPt.x = " + minMaxPt.x);
                    System.out.println("  minMaxPt.y = " + minMaxPt.y);
                    System.out.println("  int X = " + intX + "  int Y " + intY);
                    System.out.println(" x = " + x);
                    System.out.println(" y = " + y);
                    System.out.println(" T00 = " + T00);
                    System.out.println(" T10 = " + T10);

                }

                value = (dy1 * b1) + (dy * b2);

                weight = 1.0;

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                // could move remapping (rebinning) of refImage to the calling function. that way it is only
                // done once - savings - one mult, sub and convert to int. per loop.
                indexValue = (int) ((refImage.data[index] - refImage.min) * constant);
                tmp = weight * (value - inputImage.min); // Added by Matt to handle image with neg values
                numY[indexValue] += weight;
                sumY[indexValue] += tmp;
                sumY2[indexValue] += tmp * (value - inputImage.min);

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double corrRatio = 0.0;
        double variance = 0.0;
        double totSumY = 0.0;
        double totSumY2 = 0.0;
        double numTotY = 0.0;

        // now calculate the individual variances for each iso-set
        // weighting them by the number of pixels from Image x that contribute
        for (int b = 0; b < nBins; b++) {

            if (numY[b] > 2.0) {
                numTotY += numY[b];
                totSumY += sumY[b];
                totSumY2 += sumY2[b];

                // the following should be the variance of the bth iso-subset
                variance = (sumY2[b] - (sumY[b] * sumY[b] / numY[b])) / (numY[b] - 1);
                corrRatio += variance * numY[b];
            }
        }

        // normalise the weighting of numy[]
        if (numTotY > 0) {
            corrRatio /= numTotY;
        }

        // calculate the total variance of Image y and then normalise by this
        if (numTotY > 1) {
            variance = (totSumY2 - (totSumY * totSumY / numTotY)) / (numTotY - 1);
        }

        if (variance > 0.0) {
            corrRatio /= variance;
        }

        // penalizes cost based on the number of cost calculations over the
        // total possible if the number of cost calculation is less than 1000.
        int totCalcs = yEnd * xEnd;

        if (nCalcs < (0.25 * totCalcs)) {
            corrRatio = corrRatio + ((1.0 - corrRatio) * ((totCalcs - nCalcs) / (double) totCalcs));
        }

        if ((numTotY <= 1) || (variance <= 0.0)) {
            return 1.0;
        } // the totally uncorrelated condition
        else {
            return (corrRatio);
        }

        // an alternative is to return 1.0/corr_ratio (=1/(1-correlation ratio))
        // which may be better at rewarding gains near the best solution
    }

    /**
     * Correlation ratio cost function using weighting functions to mask out areas that should not be included in the
     * cost function calculations.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double correlationRatioSmoothedWgt(TransMatrix tMatrix) {

        int x, y;
        int index, indexValue;
        double value, wValue;
        double weight;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;
        double tmp;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double[] numY = new double[nBins];
        double[] sumY = new double[nBins];
        double[] sumY2 = new double[nBins];

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        // Something to think about - could we use shear-warping to speed this process ???(Paeth, Levoy)
        // Volumes must be of the same resolution.
        // Although I could correct for this it would slow the process down - maybe two version are needed.

        double constant = 1;

        if ((refImage.max - refImage.min) != 0) {
            constant = (nBins - 1) / (refImage.max - refImage.min);
        }

        int nCalcs = 0;

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {
                nCalcs++;
                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                b1 = (dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]);
                b2 = (dx1 * inputWgtImage.data[position1 + xDim]) + (dx * inputWgtImage.data[position11 + xDim]);
                wValue = (dy1 * b1) + (dy * b2);

                weight = wValue * refWgtImage.data[index];

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                indexValue = (int) ((refImage.data[index] - refImage.min) * constant);
                tmp = weight * (value - inputImage.min); // Added by Matt to handle image with neg values
                numY[indexValue] += weight;
                sumY[indexValue] += tmp;
                sumY2[indexValue] += tmp * (value - inputImage.min);

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double corrRatio = 0.0;
        double variance = 0.0;
        double totSumY = 0.0;
        double totSumY2 = 0.0;
        double numTotY = 0.0;

        // now calculate the individual variances for each iso-set
        // weighting them by the number of pixels from Image x that contribute
        int binsUsed = 0;

        for (int b = 0; b < nBins; b++) {

            if (numY[b] > 2.0) {
                binsUsed++;
                numTotY += numY[b];
                totSumY += sumY[b];
                totSumY2 += sumY2[b];

                // the following should be the variance of the bth iso-subset
                variance = (sumY2[b] - (sumY[b] * sumY[b] / numY[b])) / (numY[b] - 1);
                corrRatio += variance * numY[b];
            }
        }

        // normalise the weighting of numY[]
        if (numTotY > 0) {
            corrRatio /= numTotY;
        }

        // calculate the total variance of Image y and then normalise by this
        if (numTotY > 1) {
            variance = (totSumY2 - (totSumY * totSumY / numTotY)) / (numTotY - 1);
        }

        if (variance > 0.0) {
            corrRatio /= variance;
        }

        if ((numTotY <= 1) || (variance <= 0.0)) {
            return 1.0;
        } // the totally uncorrelated condition
        else {
            return (corrRatio);
        }
    }
    
    /**
     * Correlation ratio cost function using weighting functions to mask out areas that should not be included in the
     * cost function calculations.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double correlationRatioSmoothedWgt(TransMatrixd tMatrix) {

        int x, y;
        int index, indexValue;
        double value, wValue;
        double weight;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;
        double tmp;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double[] numY = new double[nBins];
        double[] sumY = new double[nBins];
        double[] sumY2 = new double[nBins];

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        // Something to think about - could we use shear-warping to speed this process ???(Paeth, Levoy)
        // Volumes must be of the same resolution.
        // Although I could correct for this it would slow the process down - maybe two version are needed.

        double constant = 1;

        if ((refImage.max - refImage.min) != 0) {
            constant = (nBins - 1) / (refImage.max - refImage.min);
        }

        int nCalcs = 0;

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {
                nCalcs++;
                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                b1 = (dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]);
                b2 = (dx1 * inputWgtImage.data[position1 + xDim]) + (dx * inputWgtImage.data[position11 + xDim]);
                wValue = (dy1 * b1) + (dy * b2);

                weight = wValue * refWgtImage.data[index];

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                indexValue = (int) ((refImage.data[index] - refImage.min) * constant);
                tmp = weight * (value - inputImage.min); // Added by Matt to handle image with neg values
                numY[indexValue] += weight;
                sumY[indexValue] += tmp;
                sumY2[indexValue] += tmp * (value - inputImage.min);

                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double corrRatio = 0.0;
        double variance = 0.0;
        double totSumY = 0.0;
        double totSumY2 = 0.0;
        double numTotY = 0.0;

        // now calculate the individual variances for each iso-set
        // weighting them by the number of pixels from Image x that contribute
        int binsUsed = 0;

        for (int b = 0; b < nBins; b++) {

            if (numY[b] > 2.0) {
                binsUsed++;
                numTotY += numY[b];
                totSumY += sumY[b];
                totSumY2 += sumY2[b];

                // the following should be the variance of the bth iso-subset
                variance = (sumY2[b] - (sumY[b] * sumY[b] / numY[b])) / (numY[b] - 1);
                corrRatio += variance * numY[b];
            }
        }

        // normalise the weighting of numY[]
        if (numTotY > 0) {
            corrRatio /= numTotY;
        }

        // calculate the total variance of Image y and then normalise by this
        if (numTotY > 1) {
            variance = (totSumY2 - (totSumY * totSumY / numTotY)) / (numTotY - 1);
        }

        if (variance > 0.0) {
            corrRatio /= variance;
        }

        if ((numTotY <= 1) || (variance <= 0.0)) {
            return 1.0;
        } // the totally uncorrelated condition
        else {
            return (corrRatio);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  minMaxPt  DOCUMENT ME!
     */
    private void findRangeX(Point minMaxPt, double newPtX, double newPtY, double aT00, double aT10, double iT00, double iT10) {
        double x1, x2, xMin, xMax, xMin0, xMax0;

        xMin0 = 0;
        xMax0 = xEnd;

        if (aT00 < 1.0e-8) {

            if ((0.0 <= newPtX) && (newPtX <= xEnd2)) {
                x1 = -1.0e8;
                x2 = 1.0e8;
            } else {
                x1 = -1.0e8;
                x2 = -1.0e8;
            }
        } else {
            x1 = -newPtX * iT00;
            x2 = (xEnd2 - newPtX) * iT00;
        }

        xMin = x1;
        xMax = x2;

        if (x2 < x1) {
            xMin = x2;
            xMax = x1;
        }

        if (xMin > xMin0) {
            xMin0 = xMin;
        }

        if (xMax < xMax0) {
            xMax0 = xMax;
        }

        if (aT10 < 1.0e-8) {

            if ((0.0 <= newPtY) && (newPtY <= yEnd2)) {
                x1 = -1.0e8;
                x2 = 1.0e8;
            } else {
                x1 = -1.0e8;
                x2 = -1.0e8;
            }
        } else {
            x1 = -newPtY * iT10;
            x2 = (yEnd2 - newPtY) * iT10;
        }

        xMin = x1;
        xMax = x2;

        if (x2 < x1) {
            xMin = x2;
            xMax = x1;
        }

        if (xMin > xMin0) {
            xMin0 = xMin;
        }

        if (xMax < xMax0) {
            xMax0 = xMax;
        }

        if (xMax0 < xMin0) {
            minMaxPt.y = 0;
            minMaxPt.x = 1;
        } else {
            minMaxPt.x = (int) Math.ceil(xMin0);
            minMaxPt.y = (int) Math.floor(xMax0);
        }
    }

    /**
     * Least squares cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquares(TransMatrix tMatrix) {

        int x, y;
        int index;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, valueR, valueI;
        double sum = 0.0;
        long count = 0;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                valueR = refImage.data[index] - refImage.min;
                valueI = value - inputImage.min;
                sum += (valueR - valueI) * (valueR - valueI);

                count++;
                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1000) {
            lsq = sum / ((double) count);
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }
    
    /**
     * Least squares cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquares(TransMatrixd tMatrix) {

        int x, y;
        int index;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, valueR, valueI;
        double sum = 0.0;
        long count = 0;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                valueR = refImage.data[index] - refImage.min;
                valueI = value - inputImage.min;
                sum += (valueR - valueI) * (valueR - valueI);

                count++;
                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1000) {
            lsq = sum / ((double) count);
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }

    /**
     * Least squares cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquaresColor(TransMatrix tMatrix) {

        int x, y, c;
        int index;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double valueR, valueI;
        double sum = 0.0;
        long count = 0;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = 4 * ((y * (xEnd + 1)) + minMaxPt.x);

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = 4 * ((intY * xDim) + intX);
                position11 = position1 + 4;

                for (c = 1; c <= 3; c++) {
                    b1 = (dx1 * inputImage.data[position1 + c]) + (dx * inputImage.data[position11 + c]);
                    b2 = (dx1 * inputImage.data[position1 + (4 * xDim) + c]) +
                         (dx * inputImage.data[position11 + (4 * xDim) + c]);
                    valueI = (dy1 * b1) + (dy * b2);

                    valueR = refImage.data[index + c];
                    sum += (valueR - valueI) * (valueR - valueI);
                } // for (c = 1; c <= 3; c++)

                count++;
                index += 4;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1000) {
            lsq = sum / ((double) count);
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }
    
    /**
     * Least squares cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquaresColor(TransMatrixd tMatrix) {

        int x, y, c;
        int index;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double valueR, valueI;
        double sum = 0.0;
        long count = 0;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = 4 * ((y * (xEnd + 1)) + minMaxPt.x);

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = 4 * ((intY * xDim) + intX);
                position11 = position1 + 4;

                for (c = 1; c <= 3; c++) {
                    b1 = (dx1 * inputImage.data[position1 + c]) + (dx * inputImage.data[position11 + c]);
                    b2 = (dx1 * inputImage.data[position1 + (4 * xDim) + c]) +
                         (dx * inputImage.data[position11 + (4 * xDim) + c]);
                    valueI = (dy1 * b1) + (dy * b2);

                    valueR = refImage.data[index + c];
                    sum += (valueR - valueI) * (valueR - valueI);
                } // for (c = 1; c <= 3; c++)

                count++;
                index += 4;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1000) {
            lsq = sum / ((double) count);
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }

    /**
     * Least squares cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquaresSmoothed(TransMatrix tMatrix) {

        int x, y;
        int index;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                weight = 1.0;

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                valueR = refImage.data[index] - refImage.min;
                index++;
                valueI = value - inputImage.min;
                count += weight;
                sum += weight * (valueR - valueI) * (valueR - valueI);

                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1000) {
            lsq = sum / count;
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }
    
    /**
     * Least squares cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquaresSmoothed(TransMatrixd tMatrix) {

        int x, y;
        int index;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                weight = 1.0;

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                valueR = refImage.data[index] - refImage.min;
                index++;
                valueI = value - inputImage.min;
                count += weight;
                sum += weight * (valueR - valueI) * (valueR - valueI);

                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1000) {
            lsq = sum / count;
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }

    /**
     * Least squares cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquaresSmoothedColor(TransMatrix tMatrix) {

        int x, y, c;
        int index;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;


        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = 4 * ((y * (xEnd + 1)) + minMaxPt.x);

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = 4 * ((intY * xDim) + intX);
                position11 = position1 + 4;

                weight = 1.0;

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                for (c = 1; c <= 3; c++) {
                    b1 = (dx1 * inputImage.data[position1 + c]) + (dx * inputImage.data[position11 + c]);
                    b2 = (dx1 * inputImage.data[position1 + (4 * xDim) + c]) +
                         (dx * inputImage.data[position11 + (4 * xDim) + c]);
                    valueI = (dy1 * b1) + (dy * b2);

                    valueR = refImage.data[index + c];
                    sum += weight * (valueR - valueI) * (valueR - valueI);
                } // for (c = 1; c <= 3; c++)

                index += 4;
                count += weight;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1000) {
            lsq = sum / count;
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }
    
    /**
     * Least squares cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquaresSmoothedColor(TransMatrixd tMatrix) {

        int x, y, c;
        int index;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;


        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = 4 * ((y * (xEnd + 1)) + minMaxPt.x);

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = 4 * ((intY * xDim) + intX);
                position11 = position1 + 4;

                weight = 1.0;

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                for (c = 1; c <= 3; c++) {
                    b1 = (dx1 * inputImage.data[position1 + c]) + (dx * inputImage.data[position11 + c]);
                    b2 = (dx1 * inputImage.data[position1 + (4 * xDim) + c]) +
                         (dx * inputImage.data[position11 + (4 * xDim) + c]);
                    valueI = (dy1 * b1) + (dy * b2);

                    valueR = refImage.data[index + c];
                    sum += weight * (valueR - valueI) * (valueR - valueI);
                } // for (c = 1; c <= 3; c++)

                index += 4;
                count += weight;
                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1000) {
            lsq = sum / count;
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }

    /**
     * Least squares cost function with weighting.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquaresSmoothedWgt(TransMatrix tMatrix) {

        int x, y;
        int index;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, wValue, valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {
                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                b1 = (dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]);
                b2 = (dx1 * inputWgtImage.data[position1 + xDim]) + (dx * inputWgtImage.data[position11 + xDim]);
                wValue = (dy1 * b1) + (dy * b2);

                weight = wValue * refWgtImage.data[index];

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                valueR = refImage.data[index] - refImage.min;
                index++;
                valueI = value - inputImage.min;
                count += weight;
                sum += weight * (valueR - valueI) * (valueR - valueI);

                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1.0) {
            lsq = sum / count;
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }
    
    /**
     * Least squares cost function with weighting.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquaresSmoothedWgt(TransMatrixd tMatrix) {

        int x, y;
        int index;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, wValue, valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {
                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                b1 = (dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]);
                b2 = (dx1 * inputWgtImage.data[position1 + xDim]) + (dx * inputWgtImage.data[position11 + xDim]);
                wValue = (dy1 * b1) + (dy * b2);

                weight = wValue * refWgtImage.data[index];

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                valueR = refImage.data[index] - refImage.min;
                index++;
                valueI = value - inputImage.min;
                count += weight;
                sum += weight * (valueR - valueI) * (valueR - valueI);

                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1.0) {
            lsq = sum / count;
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }

    /**
     * Least squares cost function with weighting.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquaresSmoothedWgtColor(TransMatrix tMatrix) {

        int x, y, c;
        int index;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double wValue, valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {
                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                b1 = (dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]);
                b2 = (dx1 * inputWgtImage.data[position1 + xDim]) + (dx * inputWgtImage.data[position11 + xDim]);
                wValue = (dy1 * b1) + (dy * b2);

                weight = wValue * refWgtImage.data[index];

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                for (c = 1; c <= 3; c++) {

                    // bilinear interpolation
                    b1 = (dx1 * inputImage.data[(4 * position1) + c]) + (dx * inputImage.data[(4 * position11) + c]);
                    b2 = (dx1 * inputImage.data[(4 * (position1 + xDim)) + c]) +
                         (dx * inputImage.data[(4 * (position11 + xDim)) + c]);
                    valueI = (dy1 * b1) + (dy * b2);

                    valueR = refImage.data[(4 * index) + c];
                    sum += weight * (valueR - valueI) * (valueR - valueI);
                } // for (c = 1; c <= 3; c++)

                count += weight;
                index++;

                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1.0) {
            lsq = sum / count;
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }
    
    /**
     * Least squares cost function with weighting.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquaresSmoothedWgtColor(TransMatrixd tMatrix) {

        int x, y, c;
        int index;
        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double wValue, valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {
                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                b1 = (dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]);
                b2 = (dx1 * inputWgtImage.data[position1 + xDim]) + (dx * inputWgtImage.data[position11 + xDim]);
                wValue = (dy1 * b1) + (dy * b2);

                weight = wValue * refWgtImage.data[index];

                if (newPtX < smoothX) {
                    weight *= newPtX * invSmoothX;
                } else if ((xEnd2 - newPtX) < smoothX) {
                    weight *= (xEnd2 - newPtX) * invSmoothX;
                }

                if (newPtY < smoothY) {
                    weight *= newPtY * invSmoothY;
                } else if ((yEnd2 - newPtY) < smoothY) {
                    weight *= (yEnd2 - newPtY) * invSmoothY;
                }

                if (weight < 0.0) {
                    weight = 0.0;
                }

                for (c = 1; c <= 3; c++) {

                    // bilinear interpolation
                    b1 = (dx1 * inputImage.data[(4 * position1) + c]) + (dx * inputImage.data[(4 * position11) + c]);
                    b2 = (dx1 * inputImage.data[(4 * (position1 + xDim)) + c]) +
                         (dx * inputImage.data[(4 * (position11 + xDim)) + c]);
                    valueI = (dy1 * b1) + (dy * b2);

                    valueR = refImage.data[(4 * index) + c];
                    sum += weight * (valueR - valueI) * (valueR - valueI);
                } // for (c = 1; c <= 3; c++)

                count += weight;
                index++;

                newPtX += T00;
                newPtY += T10;
            }
        }

        double lsq = 0;

        if (count > 1.0) {
            lsq = sum / count;
        } else {
            lsq = Double.MAX_VALUE;
        }

        return lsq;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double mutualInformation(TransMatrix tMatrix) {

        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropy(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        return ((1 - (margEntropyR[0] + margEntropyI[0] - jointEntropy[0])));
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double mutualInformation(TransMatrixd tMatrix) {

        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropy(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        return ((1 - (margEntropyR[0] + margEntropyI[0] - jointEntropy[0])));
    }

    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double mutualInformationSmoothed(TransMatrix tMatrix) {

        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropySmoothed(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        return ((1 - (margEntropyR[0] + margEntropyI[0] - jointEntropy[0])));
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double mutualInformationSmoothed(TransMatrixd tMatrix) {

        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropySmoothed(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        return ((1 - (margEntropyR[0] + margEntropyI[0] - jointEntropy[0])));
    }

    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedMutualInformation(TransMatrix tMatrix) {

        double normalizedMI;
        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropy(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        if (Math.abs(jointEntropy[0]) < 1e-9) {
            normalizedMI = 1.0;
        } else {
            normalizedMI = jointEntropy[0] / (margEntropyR[0] + margEntropyI[0]);
        }

        return (normalizedMI);
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedMutualInformation(TransMatrixd tMatrix) {

        double normalizedMI;
        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropy(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        if (Math.abs(jointEntropy[0]) < 1e-9) {
            normalizedMI = 1.0;
        } else {
            normalizedMI = jointEntropy[0] / (margEntropyR[0] + margEntropyI[0]);
        }

        return (normalizedMI);
    }

    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedMutualInformationSmoothed(TransMatrix tMatrix) {

        double normalizedMI;
        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropySmoothed(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        if (Math.abs(jointEntropy[0]) < 1e-9) {
            normalizedMI = 1.0;
        } else {
            normalizedMI = jointEntropy[0] / (margEntropyR[0] + margEntropyI[0]);
        }

        return (normalizedMI);
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedMutualInformationSmoothed(TransMatrixd tMatrix) {

        double normalizedMI;
        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropySmoothed(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        if (Math.abs(jointEntropy[0]) < 1e-9) {
            normalizedMI = 1.0;
        } else {
            normalizedMI = jointEntropy[0] / (margEntropyR[0] + margEntropyI[0]);
        }

        return (normalizedMI);
    }

    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedMutualInformationSmoothedWgt(TransMatrix tMatrix) {

        double normalizedMI;
        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropySmoothedWgt(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        if (Math.abs(jointEntropy[0]) < 1e-9) {
            normalizedMI = 1.0;
        } else {
            normalizedMI = jointEntropy[0] / (margEntropyR[0] + margEntropyI[0]);
        }

        return (normalizedMI);
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedMutualInformationSmoothedWgt(TransMatrixd tMatrix) {

        double normalizedMI;
        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropySmoothedWgt(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        if (Math.abs(jointEntropy[0]) < 1e-9) {
            normalizedMI = 1.0;
        } else {
            normalizedMI = jointEntropy[0] / (margEntropyR[0] + margEntropyI[0]);
        }

        return (normalizedMI);
    }

    /**
     * Normalized cross-correlation cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedXCorrelation(TransMatrix tMatrix) {

        int x, y;
        int index;
        double correlation;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, valueR, valueI;
        double sumX = 0.0;
        double sumX2 = 0.0;
        double sumY = 0.0;
        double sumY2 = 0.0;
        double sumXY = 0.0;
        long count = 0;
        double countSqr = 0;
        double varX, varY, varXY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                valueR = refImage.data[index] - refImage.min;
                valueI = value - inputImage.min;

                sumX += valueR;
                sumX2 += valueR * valueR;
                sumY += valueI;
                sumY2 += valueI * valueI;
                sumXY += valueR * valueI;

                count++;
                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        correlation = 0.0; // uncorrelated (worst) case

        if (count > 1000) {
            countSqr = (count - 1) * count;
            varXY = (sumXY / (count - 1)) - ((sumX * sumY) / countSqr);
            varX = (sumX2 / (count - 1)) - ((sumX * sumX) / countSqr);
            varY = (sumY2 / (count - 1)) - ((sumY * sumY) / countSqr);

            if ((varX > 0.0) && (varY > 0.0)) {
                correlation = Math.abs(varXY) / Math.sqrt(varX * varY);
            }
        }

        return (1 - correlation);
    }
    
    /**
     * Normalized cross-correlation cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedXCorrelation(TransMatrixd tMatrix) {

        int x, y;
        int index;
        double correlation;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, valueR, valueI;
        double sumX = 0.0;
        double sumX2 = 0.0;
        double sumY = 0.0;
        double sumY2 = 0.0;
        double sumXY = 0.0;
        long count = 0;
        double countSqr = 0;
        double varX, varY, varXY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (y = 0; y <= yEnd; y++) {

            newPtX = (y * T01) + T02;
            newPtY = (y * T11) + T12;

            // determine range
            findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

            newPtX += minMaxPt.x * T00;
            newPtY += minMaxPt.x * T10;

            index = (y * (xEnd + 1)) + minMaxPt.x;

            for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                intX = (int) newPtX;
                intY = (int) newPtY;

                dx = newPtX - intX;
                dy = newPtY - intY;

                dx1 = 1 - dx;
                dy1 = 1 - dy;

                position1 = (intY * xDim) + intX;
                position11 = position1 + 1;

                // bilinear interpolation
                b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                value = (dy1 * b1) + (dy * b2);

                valueR = refImage.data[index] - refImage.min;
                valueI = value - inputImage.min;

                sumX += valueR;
                sumX2 += valueR * valueR;
                sumY += valueI;
                sumY2 += valueI * valueI;
                sumXY += valueR * valueI;

                count++;
                index++;
                newPtX += T00;
                newPtY += T10;
            }
        }

        correlation = 0.0; // uncorrelated (worst) case

        if (count > 1000) {
            countSqr = (count - 1) * count;
            varXY = (sumXY / (count - 1)) - ((sumX * sumY) / countSqr);
            varX = (sumX2 / (count - 1)) - ((sumX * sumX) / countSqr);
            varY = (sumY2 / (count - 1)) - ((sumY * sumY) / countSqr);

            if ((varX > 0.0) && (varY > 0.0)) {
                correlation = Math.abs(varXY) / Math.sqrt(varX * varY);
            }
        }

        return (1 - correlation);
    }

    /**
     * Normalized cross-correlation cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedXCorrelationSmoothed(TransMatrix tMatrix) {

        int x, y, iter;

        int index;
        int numValues = 0;
        double correlation;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, valueR, valueI;
        double sumX = 0.0;
        double sumX2 = 0.0;
        double sumY = 0.0;
        double sumY2 = 0.0;
        double sumXY = 0.0;
        double count = 0.0;
        double countFactor = 0.0;
        double varX, varY, varXY;
        double xAverage = 0.0;
        double yAverage = 0.0;

        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (iter = 0; iter <= 1; iter++) {

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + T02;
                newPtY = (y * T11) + T12;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;

                index = (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;

                    dx = newPtX - intX;
                    dy = newPtY - intY;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intY * xDim) + intX;
                    position11 = position1 + 1;

                    // bilinear interpolation
                    b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                    b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                    value = (dy1 * b1) + (dy * b2);

                    weight = 1.0;

                    if (newPtX < smoothX) {
                        weight *= newPtX * invSmoothX;
                    } else if ((xEnd2 - newPtX) < smoothX) {
                        weight *= (xEnd2 - newPtX) * invSmoothX;
                    }

                    if (newPtY < smoothY) {
                        weight *= newPtY * invSmoothY;
                    } else if ((yEnd2 - newPtY) < smoothY) {
                        weight *= (yEnd2 - newPtY) * invSmoothY;
                    }

                    if (weight < 0.0) {
                        weight = 0.0;
                    }

                    valueR = refImage.data[index] - refImage.min;
                    valueI = value - inputImage.min;

                    if (iter == 0) {
                        sumX += weight * valueR;
                        sumY += weight * valueI;
                        count += weight;
                        numValues++;
                    } else if (iter == 1) {
                        sumX2 += weight * (valueR - xAverage) * (valueR - xAverage);
                        sumY2 += weight * (valueI - yAverage) * (valueI - yAverage);
                        sumXY += weight * (valueR - xAverage) * (valueI - yAverage);
                    }

                    index++;
                    newPtX += T00;
                    newPtY += T10;
                }
            }

            if (iter == 0) {
                xAverage = sumX / count;
                yAverage = sumY / count;
            }
        } // for (iter = 0; iter <= 1; iter++)

        correlation = 0.0; // uncorrelated (worst) case

        if (count > 1000) {
            countFactor = numValues / ((numValues - 1) * count);
            varXY = sumXY * countFactor;
            varX = sumX2 * countFactor;
            varY = sumY2 * countFactor;

            if ((varX > 0.0) && (varY > 0.0)) {
                correlation = Math.abs(varXY) / Math.sqrt(varX * varY);
            }
        }

        return (1 - correlation);
    }
    
    /**
     * Normalized cross-correlation cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedXCorrelationSmoothed(TransMatrixd tMatrix) {

        int x, y, iter;

        int index;
        int numValues = 0;
        double correlation;
        int position1, position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, valueR, valueI;
        double sumX = 0.0;
        double sumX2 = 0.0;
        double sumY = 0.0;
        double sumY2 = 0.0;
        double sumXY = 0.0;
        double count = 0.0;
        double countFactor = 0.0;
        double varX, varY, varXY;
        double xAverage = 0.0;
        double yAverage = 0.0;

        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (iter = 0; iter <= 1; iter++) {

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + T02;
                newPtY = (y * T11) + T12;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;

                index = (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;

                    dx = newPtX - intX;
                    dy = newPtY - intY;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intY * xDim) + intX;
                    position11 = position1 + 1;

                    // bilinear interpolation
                    b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                    b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                    value = (dy1 * b1) + (dy * b2);

                    weight = 1.0;

                    if (newPtX < smoothX) {
                        weight *= newPtX * invSmoothX;
                    } else if ((xEnd2 - newPtX) < smoothX) {
                        weight *= (xEnd2 - newPtX) * invSmoothX;
                    }

                    if (newPtY < smoothY) {
                        weight *= newPtY * invSmoothY;
                    } else if ((yEnd2 - newPtY) < smoothY) {
                        weight *= (yEnd2 - newPtY) * invSmoothY;
                    }

                    if (weight < 0.0) {
                        weight = 0.0;
                    }

                    valueR = refImage.data[index] - refImage.min;
                    valueI = value - inputImage.min;

                    if (iter == 0) {
                        sumX += weight * valueR;
                        sumY += weight * valueI;
                        count += weight;
                        numValues++;
                    } else if (iter == 1) {
                        sumX2 += weight * (valueR - xAverage) * (valueR - xAverage);
                        sumY2 += weight * (valueI - yAverage) * (valueI - yAverage);
                        sumXY += weight * (valueR - xAverage) * (valueI - yAverage);
                    }

                    index++;
                    newPtX += T00;
                    newPtY += T10;
                }
            }

            if (iter == 0) {
                xAverage = sumX / count;
                yAverage = sumY / count;
            }
        } // for (iter = 0; iter <= 1; iter++)

        correlation = 0.0; // uncorrelated (worst) case

        if (count > 1000) {
            countFactor = numValues / ((numValues - 1) * count);
            varXY = sumXY * countFactor;
            varX = sumX2 * countFactor;
            varY = sumY2 * countFactor;

            if ((varX > 0.0) && (varY > 0.0)) {
                correlation = Math.abs(varXY) / Math.sqrt(varX * varY);
            }
        }

        return (1 - correlation);
    }

    /**
     * Normalized cross-correlation cost function with weighting.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedXCorrelationSmoothedWgt(TransMatrix tMatrix) {

        int x, y, iter;
        int index;
        int numValues = 0;
        double correlation;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, wValue, valueR, valueI;
        double sumX = 0.0;
        double sumX2 = 0.0;
        double sumY = 0.0;
        double sumY2 = 0.0;
        double sumXY = 0.0;
        double count = 0.0;
        double countFactor = 0.0;
        double varX, varY, varXY;
        double xAverage = 0.0;
        double yAverage = 0.0;

        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (iter = 0; iter <= 1; iter++) {

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + T02;
                newPtY = (y * T11) + T12;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;

                index = (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;

                    dx = newPtX - intX;
                    dy = newPtY - intY;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intY * xDim) + intX;
                    position11 = position1 + 1;

                    // bilinear interpolation
                    b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                    b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                    value = (dy1 * b1) + (dy * b2);

                    b1 = (dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]);
                    b2 = (dx1 * inputWgtImage.data[position1 + xDim]) + (dx * inputWgtImage.data[position11 + xDim]);
                    wValue = (dy1 * b1) + (dy * b2);

                    weight = wValue * refWgtImage.data[index];

                    if (newPtX < smoothX) {
                        weight *= newPtX * invSmoothX;
                    } else if ((xEnd2 - newPtX) < smoothX) {
                        weight *= (xEnd2 - newPtX) * invSmoothX;
                    }

                    if (newPtY < smoothY) {
                        weight *= newPtY * invSmoothY;
                    } else if ((yEnd2 - newPtY) < smoothY) {
                        weight *= (yEnd2 - newPtY) * invSmoothY;
                    }

                    if (weight < 0.0) {
                        weight = 0.0;
                    }

                    valueR = refImage.data[index] - refImage.min;
                    valueI = value - inputImage.min;

                    if (iter == 0) {
                        sumX += weight * valueR;
                        sumY += weight * valueI;
                        count += weight;
                        numValues++;
                    } else if (iter == 1) {
                        sumX2 += weight * (valueR - xAverage) * (valueR - xAverage);
                        sumY2 += weight * (valueI - yAverage) * (valueI - yAverage);
                        sumXY += weight * (valueR - xAverage) * (valueI - yAverage);
                    }

                    index++;
                    newPtX += T00;
                    newPtY += T10;
                }
            }

            if (iter == 0) {
                xAverage = sumX / count;
                yAverage = sumY / count;
            }
        } // for (iter = 0; iter <= 1; iter++)

        correlation = 0.0; // uncorrelated (worst) case

        if (count > 2.0) {
            countFactor = numValues / ((numValues - 1) * count);
            varXY = sumXY * countFactor;
            varX = sumX2 * countFactor;
            varY = sumY2 * countFactor;

            if ((varX > 0.0) && (varY > 0.0)) {
                correlation = Math.abs(varXY) / Math.sqrt(varX * varY);
            }
        }

        return (1 - correlation);
    }
    
    /**
     * Normalized cross-correlation cost function with weighting.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedXCorrelationSmoothedWgt(TransMatrixd tMatrix) {

        int x, y, iter;
        int index;
        int numValues = 0;
        double correlation;
        int position1;
        int position11;
        int intX, intY;
        double dx, dy, dx1, dy1;
        double b1, b2;

        double value, wValue, valueR, valueI;
        double sumX = 0.0;
        double sumX2 = 0.0;
        double sumY = 0.0;
        double sumY2 = 0.0;
        double sumXY = 0.0;
        double count = 0.0;
        double countFactor = 0.0;
        double varX, varY, varXY;
        double xAverage = 0.0;
        double yAverage = 0.0;

        double smoothX, smoothY;
        double invSmoothX, invSmoothY;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;

        double aT00 = T00;
        double aT10 = T10;

        double iT00, iT10;
        double newPtX, newPtY;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT00 >= 1.0e-8) {
            iT00 = 1 / T00;
        }else{
        	iT00 = Double.MAX_VALUE;
        }

        if (aT10 >= 1.0e-8) {
            iT10 = 1 / T10;
        }else{
        	iT10 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (iter = 0; iter <= 1; iter++) {

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + T02;
                newPtY = (y * T11) + T12;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, aT00, aT10, iT00, iT10);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;

                index = (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;

                    dx = newPtX - intX;
                    dy = newPtY - intY;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intY * xDim) + intX;
                    position11 = position1 + 1;

                    // bilinear interpolation
                    b1 = (dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]);
                    b2 = (dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim]);
                    value = (dy1 * b1) + (dy * b2);

                    b1 = (dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]);
                    b2 = (dx1 * inputWgtImage.data[position1 + xDim]) + (dx * inputWgtImage.data[position11 + xDim]);
                    wValue = (dy1 * b1) + (dy * b2);

                    weight = wValue * refWgtImage.data[index];

                    if (newPtX < smoothX) {
                        weight *= newPtX * invSmoothX;
                    } else if ((xEnd2 - newPtX) < smoothX) {
                        weight *= (xEnd2 - newPtX) * invSmoothX;
                    }

                    if (newPtY < smoothY) {
                        weight *= newPtY * invSmoothY;
                    } else if ((yEnd2 - newPtY) < smoothY) {
                        weight *= (yEnd2 - newPtY) * invSmoothY;
                    }

                    if (weight < 0.0) {
                        weight = 0.0;
                    }

                    valueR = refImage.data[index] - refImage.min;
                    valueI = value - inputImage.min;

                    if (iter == 0) {
                        sumX += weight * valueR;
                        sumY += weight * valueI;
                        count += weight;
                        numValues++;
                    } else if (iter == 1) {
                        sumX2 += weight * (valueR - xAverage) * (valueR - xAverage);
                        sumY2 += weight * (valueI - yAverage) * (valueI - yAverage);
                        sumXY += weight * (valueR - xAverage) * (valueI - yAverage);
                    }

                    index++;
                    newPtX += T00;
                    newPtY += T10;
                }
            }

            if (iter == 0) {
                xAverage = sumX / count;
                yAverage = sumY / count;
            }
        } // for (iter = 0; iter <= 1; iter++)

        correlation = 0.0; // uncorrelated (worst) case

        if (count > 2.0) {
            countFactor = numValues / ((numValues - 1) * count);
            varXY = sumXY * countFactor;
            varX = sumX2 * countFactor;
            varY = sumY2 * countFactor;

            if ((varX > 0.0) && (varY > 0.0)) {
                correlation = Math.abs(varXY) / Math.sqrt(varX * varY);
            }
        }

        return (1 - correlation);
    }

    /**
     * Precalculates information and allocates buffers used in the calculation of mutual information statistics.
     *
     * @param  nBins  the number of bins
     */
    private void setPLogP(int nBins) {
        int N = refImage.data.length;
        double p = 0;

        try {
            pLogP = new double[Math.min(10000, (10 * N) / nBins)];

            for (int num = 0; num < pLogP.length; num++) {
                p = ((double) num) / ((double) N);
                pLogP[num] = -p * Math.log(p);
            }

//            jointHist = new double[nBins * nBins];
//            margHistR = new double[nBins];
//            margHistI = new double[nBins];

        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: CostFunctions.setBins");

            return;
        }
    }

}
