package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.*;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.ImageRegistrationGPU;

import java.awt.*;

import WildMagic.LibFoundation.Mathematics.Matrix4f;


/**
 * CostFunction - class for specifying optimization function.
 */
public class AlgorithmCostFunctions implements AlgorithmOptimizeFunctionBase {

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
//    private double aT00, aT10, aT20;

    /** DOCUMENT ME! */
    private int costCalled = 0;

    /** DOCUMENT ME! */
    private int costFunctID = CORRELATION_RATIO_SMOOTHED;

    /** DOCUMENT ME! */
    private ModelSimpleImage inputImage;

    /** DOCUMENT ME! */
    private ModelSimpleImage inputWgtImage = null;

    /** DOCUMENT ME! */
//    private double iT00, iT10, iT20;

    /** DOCUMENT ME! */
//    private double[] jointHist;

    /** DOCUMENT ME! */
    private double M_PI = Math.PI;

    /** DOCUMENT ME! */
//    private double[] margHistI;

    /** DOCUMENT ME! */
//    private double[] margHistR;

    /** DOCUMENT ME! */
//    private Point minMaxPt = new Point();

    /** DOCUMENT ME! */
    private int nBins;

    /** DOCUMENT ME! */
//    private double newPtX, newPtY, newPtZ;

    /** DOCUMENT ME! */
//    private double nOverlap;

    /** DOCUMENT ME! */
//    private double[] numY;

    /** DOCUMENT ME! */
    private int nVoxels;

    /** DOCUMENT ME! */
    private double[] pLogP;

    /** DOCUMENT ME! */
    private ModelSimpleImage refImage;

    /** DOCUMENT ME! */
    private int refSliceSize;

    /** DOCUMENT ME! */
    private ModelSimpleImage refWgtImage = null;

    /** DOCUMENT ME! */
    private double[] sincKernel = new double[201];

    /** DOCUMENT ME! */
    private int sincWidth = 3;

    /** DOCUMENT ME! */
    private double[] sincx = new double[(2 * sincWidth) + 1];

    /** DOCUMENT ME! */
    private double[] sincy = new double[(2 * sincWidth) + 1];

    /** DOCUMENT ME! */
    private double[] sincz = new double[(2 * sincWidth) + 1];

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private float smoothSize;

    /** DOCUMENT ME! */
//    private double[] sumY;

    /** DOCUMENT ME! */
//    private double[] sumY2;

    /** DOCUMENT ME! */
//    private double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

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

    /** DOCUMENT ME! */
    private int zDim;

    /** DOCUMENT ME! */
    private int zEnd;

    /** DOCUMENT ME! */
    private double zEnd2;
    
    private float[] m_afJointHisto;
    private boolean m_bPrint = false;
    
    private ImageRegistrationGPU m_kGPUCost = null;;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmCostFunctions object.
     *
     * @param  rImage      DOCUMENT ME!
     * @param  iImage      DOCUMENT ME!
     * @param  functionID  DOCUMENT ME!
     * @param  nBins       DOCUMENT ME!
     * @param  smoothSize  DOCUMENT ME!
     */
    public AlgorithmCostFunctions(ModelSimpleImage rImage, ModelSimpleImage iImage, int functionID, int nBins,
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
        zDim = inputImage.zDim;
        sliceSize = xDim * yDim;

        xEnd2 = xDim - 1.0001;
        yEnd2 = yDim - 1.0001;
        zEnd2 = zDim - 1.0001;

        xEnd = refImage.xDim - 1;
        yEnd = refImage.yDim - 1;
        zEnd = refImage.zDim - 1;
        refSliceSize = refImage.xDim * refImage.yDim;

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
        //affMatrix = new TransMatrix(4,4);
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

            case NORMALIZED_XCORRELATION_SINC:
                value = normalizedXCorrelationSinc(affMatrix);
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
                /*
                if ( m_kGPUCost != null )
                {
                    value = normalizedMutualInformation(affMatrix);
                    m_kGPUCost.setJoint( m_afJointHisto );
                    double valueGPU = m_kGPUCost.getError(affMatrix);
                    if ( Math.abs( valueGPU - value ) > 0.01 )
                    {
                        System.err.println( "CPU: " + value + "   GPU: " + valueGPU );
                        m_bPrint = true;
                        value = normalizedMutualInformation(affMatrix);
                        m_kGPUCost.Print(true);
                        m_kGPUCost.setJoint( m_afJointHisto );
                        valueGPU = m_kGPUCost.getError(affMatrix);
                        System.err.println( "22222222 CPU: " + value + "   GPU: " + valueGPU );
                    }
                    m_bPrint = false;
                    m_kGPUCost.Print(false);
                    value = valueGPU;
                }
                */
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
        
        //System.err.println( "AlgorithmCostFuntions2D.disposeLocal(): " + this + " " + nBins + " " + costCalled );

//        sumY = null;
//        sumY2 = null;
//        numY = null;

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

    /**
     * Accessor that returns how many times the cost function has been called.
     *
     * @return  The number of times the cost function has been called.
     */
    public int getCostCalled() {
        return costCalled;
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
     * Accessor that returns the amount of overlap, the total voxels, and the percentage.
     *
     * @return  The number of times the cost function has been called.
     */
//    public double[] getOverlap() {
//        double[] toReturn = new double[] { 0, 0, 0 };
//
//        if ((costFunctID < MUTUAL_INFORMATION_SMOOTHED) || (costFunctID > NORMALIZED_MUTUAL_INFORMATION)) {
//            return toReturn;
//        } else {
//            toReturn[0] = (double) nOverlap;
//            toReturn[1] = (double) nVoxels;
//            toReturn[2] = 100 * toReturn[0] / toReturn[1];
//
//            return toReturn;
//        }
//    }

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

        if ((inputWgtImage.min < 0) || (inputWgtImage.max > 1)) {

            // remap data - normalize data between 0 and 1
            if (inputWgtImage.min != inputWgtImage.max) {
                diff = inputWgtImage.max - inputWgtImage.min;

                for (int i = 0; i < inputWgtImage.data.length; i++) {
                    inputWgtImage.data[i] = (inputWgtImage.data[i] - inputWgtImage.min) / diff;
                }
            }
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

        if ((refWgtImage.min < 0) || (refWgtImage.max > 1)) {

            // remap data - normalize data between 0 and 1
            if (refWgtImage.min != refWgtImage.max) {
                diff = refWgtImage.max - refWgtImage.min;

                for (int i = 0; i < refWgtImage.data.length; i++) {
                    refWgtImage.data[i] = (refWgtImage.data[i] - refWgtImage.min) / diff;
                }
            }
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

        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index, indexValueR, indexValueI;
        int position1, position2;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;
        double value;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;
        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
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
        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position2 = position1 + sliceSize;

                    b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position1 + 1]))) +
                         (dy *
                              ((dx1 * inputImage.data[position1 + xDim]) +
                                   (dx * inputImage.data[position1 + xDim + 1])));

                    b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position2 + 1]))) +
                         (dy *
                              ((dx1 * inputImage.data[position2 + xDim]) +
                                   (dx * inputImage.data[position2 + xDim + 1])));

                    value = ((1 - dz) * b1) + (dz * b2);

                    indexValueI = (int) ((value - inputImage.min) * constantI);
                    indexValueR = (int) ((refImage.data[index] - refImage.min) * constantR);
                    
                    //if ( z == 1 )
                    {
                        jointHist[(indexValueR * nBins) + indexValueI] += 1;
                        margHistR[indexValueR] += 1;
                        margHistI[indexValueI] += 1;
                    }
                    index++;
                    newPtX += T00;
                    newPtY += T10;
                    newPtZ += T20;
                }
            }
        }

        // note that the pLogP values indexed by integers such that: pLogP(n) = n/N * log(n/N)
        double p = 0.0;
        int n = 0;
        int pSize = pLogP.length;

        nVoxels = refImage.data.length;
        
        m_afJointHisto = new float[nBins*nBins*4];

        for (int i = 0; i < (nBins * nBins); i++) {
            n = (int) MipavMath.round(jointHist[i]);

            if (n > 0) {

                if (n < pSize) {
                    jointEntropy[0] += pLogP[n];
                } else {
                    p = ((double) n) / ((double) nVoxels);
                    jointEntropy[0] += -p * Math.log(p);
                }
            }
            m_afJointHisto[i*4] = (float)jointHist[i];
            m_afJointHisto[i*4+1] = 0f;
            m_afJointHisto[i*4+2] = 0f;
            m_afJointHisto[i*4+3] = (float)jointHist[i];
        }
        //System.err.println("");
        //System.err.println("");
        //System.err.println("");
        //System.err.println("Reference");
        for (int i = 0; i < nBins; i++) {
            n = (int) MipavMath.round(margHistR[i]);
            //System.err.println ( i + " " + n );
            if (n > 0) {

                if (n < pSize) {
                    margEntropyR[0] += pLogP[n];
                } else {
                    p = ((double) n) / ((double) nVoxels);
                    margEntropyR[0] += -p * Math.log(p);
                }
            }
        }
        //System.err.println("Done");
        //System.err.println("");
        //System.err.println("");
        //System.err.println("");

        double nOverlap = 0.0;

        for (int i = 0; i < nBins; i++) {
            n = (int) MipavMath.round(margHistI[i]);

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

        //System.err.println( "CPU: " + nBins + " " + nVoxels + " " + nOverlap + " " + margEntropyR[0] + " " + margEntropyI[0] + " " + jointEntropy[0] );
        
        // correct for difference in total histogram size
        // that is: nOverlap vs nVoxels
        // H_1 = N_0/N_1 * H_0 + log(N_1/N_0)
        // = N_0/N_1 * H_0 - log(N_0/N_1)
        if (nOverlap > (0.15 * nVoxels)) {
            double nRatio = ((double) nVoxels) / ((double) nOverlap);

            margEntropyR[0] = (nRatio * margEntropyR[0]) - Math.log(nRatio);
            margEntropyI[0] = (nRatio * margEntropyI[0]) - Math.log(nRatio);
            jointEntropy[0] = (nRatio * jointEntropy[0]) - Math.log(nRatio);
            if ( m_bPrint )
            {
                System.err.println( "CPU: " + margEntropyR[0] + " " + margEntropyI[0] + " " + jointEntropy[0] );
            }
            } else {

            // System.out.println("nOvelap not high enough, less than 15% of voxels.");
            // Put in maximum entropy values as base cases = BAD registration
            jointEntropy[0] = 2.0 * Math.log(nBins);
            margEntropyR[0] = Math.log(nBins);
            margEntropyI[0] = Math.log(nBins);
            if ( m_bPrint )
            {
                System.out.println("nOvelap not high enough, less than 15% of voxels.");
                System.err.println( "CPU: " + margEntropyR[0] + " " + margEntropyI[0] + " " + jointEntropy[0] );
            }
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

        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index, indexValueR;
        int iCenter;
        int position1, position2;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;
        double value;

        double smoothX, smoothY, smoothZ;
        double invSmoothX, invSmoothY, invSmoothZ;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;
        smoothZ = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;
        invSmoothZ = 1.0 / smoothZ;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
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
        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position2 = position1 + sliceSize;

                    b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position1 + 1]))) +
                         (dy *
                              ((dx1 * inputImage.data[position1 + xDim]) +
                                   (dx * inputImage.data[position1 + xDim + 1])));

                    b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position2 + 1]))) +
                         (dy *
                              ((dx1 * inputImage.data[position2 + xDim]) +
                                   (dx * inputImage.data[position2 + xDim + 1])));

                    value = ((1 - dz) * b1) + (dz * b2);

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

                    if (newPtZ < smoothZ) {
                        weight *= newPtZ * invSmoothZ;
                    } else if ((zEnd2 - newPtZ) < smoothZ) {
                        weight *= (zEnd2 - newPtZ) * invSmoothZ;
                    }

                    if (weight < 0.0) {
                        weight = 0.0;
                    }

                    if (iCenter >= nBins) {
                        System.out.println("Testing:  iCenter is greater than nBins " + iCenter + " " + nBins);
                        iCenter = nBins - 1;
                    }

                    if (iCenter < 0) {
                        System.out.println("Testing:  iCenter is less than 0 " + iCenter);
                        iCenter = 0;
                    }

                    jointHist[(indexValueR * nBins) + iCenter] += weight;
                    margHistI[iCenter] += weight;
                    margHistR[indexValueR] += weight;

                    index++;
                    newPtX += T00;
                    newPtY += T10;
                    newPtZ += T20;
                }
            }
        }

        // Calculate joint entropy and marginal entropies.
        double p = 0.0;
        double nJ = 0.0, nR = 0.0, nI = 0.0;
        int k;

        nVoxels = refImage.data.length;
        double nOverlap = 0.0;

        for (int i = 0; i < nBins; i++) {
            nI = margHistI[i];

            if (nI > 0.0) {
                nOverlap += nI;
            }
        }

        if (nOverlap > (.15 * nVoxels)) {

            for (int i = 0; i < nBins; i++) {
                nR = margHistR[i];

                if (nR > 0.0) {
                    p = nR / ((double) nOverlap);
                    margEntropyR[0] += -p * Math.log(p);
                }

                nI = margHistI[i];

                if (nI > 0.0) {
                    p = nI / ((double) nOverlap);
                    margEntropyI[0] += -p * Math.log(p);
                }

                for (int j = 0; j < nBins; j++) {
                    k = (i * nBins) + j;
                    nJ = jointHist[k];

                    if (nJ > 0.0) {
                        p = nJ / ((double) nOverlap);
                        jointEntropy[0] += -p * Math.log(p);
                    }
                }
            }
        } else {
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
        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index, indexValueR;
        int iCenter;
        int position1, position2, position1x, position2x;
        int position11, position21, position11x, position21x;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;
        double value, wValue;

        double smoothX, smoothY, smoothZ;
        double invSmoothX, invSmoothY, invSmoothZ;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;
        smoothZ = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;
        invSmoothZ = 1.0 / smoothZ;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        // The variables aT00, aT10, aT20, iT00... etc. will be used for findRangeX, but calculated before
        // entering the loop for efficiency.
        // save the x multipliers to new variable names and then if negative, make them positive.
        aT00 = T00;
        aT10 = T10;
        aT20 = T20;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
        }

        // Also take the inverse of each multiplier, unless it is smaller than 1.0e-8
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
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

        // Define constantR and constantI, where they each represent the number of bins per intensity
        // value (less than one).  They will be used later to multiply the intensity value of the reference
        // image to find the appropriate bin.
        double constantR = 1;

        if ((refImage.max - refImage.min) != 0) {
            constantR = (nBins - 1) / (refImage.max - refImage.min);
        }

        double constantI = 1;

        if ((inputImage.max - inputImage.min) != 0) {
            constantI = (nBins - 1) / (inputImage.max - inputImage.min);
        }
        Point minMaxPt = new Point();

        // zEnd has been previously defined to be refImage.zDim-1 (likewise xEnd and yEnd)
        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range of x values for the current y value, so don't have to loop through all x's
                // and always check that it's in bounds
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                // Full formula for newPt's:
                // newPtX = minMaxPt.x*T00 + y*T01 + z*T02 + T03
                // newPtY = minMaxPt.x*T01 + y*T11 + z*T12 + T13
                // newPtZ = minMaxPt.x*T20 + y*T21 + z*T22 + T23

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                // index keeps track of where we are in the reference image
                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    // newPt's aren't integers, but we can only index images with integers.
                    // Interpolate (trilinear) between image value at neighboring indices.
                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position1x = position1 + xDim;
                    position2 = position1 + sliceSize;
                    position2x = position2 + xDim;
                    position11 = position1 + 1;
                    position11x = position11 + xDim;
                    position21 = position2 + 1;
                    position21x = position21 + xDim;

                    // Get value at current x,y, z for the inputImage.
                    // b1 is value interplated between x values and y values at rounded-down z slice
                    b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]))) +
                         (dy * ((dx1 * inputImage.data[position1x]) + (dx * inputImage.data[position11x])));

                    // b2 is value interpolated between x value and y values at rounded-up z slice
                    b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position21]))) +
                         (dy * ((dx1 * inputImage.data[position2x]) + (dx * inputImage.data[position21x])));

                    // value is interpolation between b1 and b2 and exact z location
                    value = ((1 - dz) * b1) + (dz * b2);

                    // Get value at current x,y, z for the inputWgtImage - weighted image!!
                    // b1, b2 are same as above; wValue is now interpolation between b1 and b2
                    b1 = (dy1 * ((dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]))) +
                         (dy * ((dx1 * inputWgtImage.data[position1x]) + (dx * inputWgtImage.data[position11x])));

                    b2 = (dy1 * ((dx1 * inputWgtImage.data[position2]) + (dx * inputWgtImage.data[position21]))) +
                         (dy * ((dx1 * inputWgtImage.data[position2x]) + (dx * inputWgtImage.data[position21x])));
                    wValue = ((1 - dz) * b1) + (dz * b2);

                    // Weight assigned wValue (weight of input image at current loc) times the Reference image weight.
                    weight = wValue * refWgtImage.data[index];

                    // Find iCenter and indexValueR.  iCenter is bin number for this pixel in the Input image (y
                    // coordinate of joint histogram image).  indexValueR in the bin number for this pixel in the
                    // Reference image (x coordiante of joint histogram image).
                    iCenter = (int) ((value - inputImage.min) * constantI);
                    indexValueR = (int) ((refImage.data[index] - refImage.min) * constantR);

                    // Smooth out the edges.
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

                    if (newPtZ < smoothZ) {
                        weight *= newPtZ * invSmoothZ;
                    } else if ((zEnd2 - newPtZ) < smoothZ) {
                        weight *= (zEnd2 - newPtZ) * invSmoothZ;
                    }

                    if (weight < 0.0) {
                        weight = 0.0;
                    }

                    if (iCenter >= nBins) {
                        System.out.println("Testing.  iCenter is greater than nBins.");
                        iCenter = nBins - 1;
                    }

                    if (iCenter < 0) {
                        System.out.println("Testing. iCenter is less than zero.");
                        iCenter = 0;
                    }

                    // Add "weight" to the histograms.  Index the jointHistogram (saved as 1D array) assuming width of
                    // nBins, so indexValueR*nBins + iCenter.
                    jointHist[(indexValueR * nBins) + iCenter] += weight;
                    margHistI[iCenter] += weight;
                    margHistR[indexValueR] += weight;

                    // Increment index.  Go to next point.
                    index++;
                    newPtX += T00;
                    newPtY += T10;
                    newPtZ += T20;

                } // end of x loop
            } // end of y loop
        } // end of z loop

        // Up until this point we've generated a jointHistogram "image" and histogram images for the two
        // input images.  Now, to calculate the Normalized Mutual Information, we'll calculate the entropy of
        // the histogram images by summing -plogp over all bins.  p is the probability for a given
        // intensity value - equal to the value in the histogram image divided by the total # of voxels.
        double p = 0.0;
        double n = 0.0;

        nVoxels = refImage.data.length;

        // Joint entropy H(A,B)
        for (int i = 0; i < (nBins * nBins); i++) {
            n = jointHist[i];

            if (n > 0.0) {
                p = n / ((double) nVoxels);
                jointEntropy[0] += -p * Math.log(p);
            }
        }

        // Marginal entropy of tranformed reference image H(B)
        for (int i = 0; i < nBins; i++) {
            n = margHistR[i];

            if (n > 0.0) {
                p = n / ((double) nVoxels);
                margEntropyR[0] += -p * Math.log(p);
            }
        }

        // Marginal entropy of input image H(A)
        double nOverlap = 0.0;

        for (int i = 0; i < nBins; i++) {
            n = margHistI[i];

            if (n > 0.0) {
                nOverlap += n; // Calculate the region of overlap while we're going through Input image voxels.
                p = n / ((double) nVoxels);
                margEntropyI[0] += -p * Math.log(p);
            }
        }

        // correct for difference in total histogram size
        // that is: nOverlap vs nVoxels
        // H_1 = N_0/N_1 * H_0 + log(N_1/N_0)
        // = N_0/N_1 * H_0 - log(N_0/N_1)
        // This separation number of 5.0 may have to be tweaked depending on
        // the size of the weighted volumes
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

        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index, indexValue;
        double value;
        int position1, position2;
        int position11, position21;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;

        double[] numY = new double[nBins];
        double[] sumY = new double[nBins];
        double[] sumY2 = new double[nBins];

        // get transformation matrix into quick access variables.
        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        // precalculates this constant for rebinning
        double constant = 1;

        if ((refImage.max - refImage.min) != 0) {
            constant = (nBins - 1) / (refImage.max - refImage.min);
        }

        int nCalcs = 0;
        Point minMaxPt = new Point();

        for (z = 0; z <= zEnd; z++) { // zEnd = the bound of ref image
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x < minMaxPt.y; x++) {

                    // make function call to getTrilinear or move code here.
                    // value = inputImage.getTriLinear(newPtX,newPtY,newPtZ);
                    nCalcs++;
                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position2 = position1 + sliceSize;
                    position11 = position1 + 1;
                    position21 = position2 + 1;

                    // trilinear interpolation
                    b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]))) +
                         (dy * ((dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim])));

                    b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position21]))) +
                         (dy * ((dx1 * inputImage.data[position2 + xDim]) + (dx * inputImage.data[position21 + xDim])));

                    value = ((1 - dz) * b1) + (dz * b2);

                    // This was actually slower -> b1 + (b2-b1)*dz ??????
                    indexValue = (int) ((refImage.data[index] - refImage.min) * constant);
                    numY[indexValue] += 1;
                    sumY[indexValue] += value - inputImage.min;
                    sumY2[indexValue] += (value - inputImage.min) * (value - inputImage.min);

                    index++;
                    newPtX += T00;
                    newPtY += T10;
                    newPtZ += T20;
                }
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
        int totCalcs = zEnd * yEnd * xEnd;

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
    private double correlationRatioSmoothed(TransMatrix tMatrix) {

        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index, indexValue;
        double value;
        double weight;
        double smoothX, smoothY, smoothZ;
        double invSmoothX, invSmoothY, invSmoothZ;
        int position1, position2;
        int position11, position21;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;
        double tmp;

        // setup
        smoothX = smoothSize;
        smoothY = smoothSize;
        smoothZ = smoothSize;

        // setup so that inner loops use multiples (faster) instead of divides.
        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;
        invSmoothZ = 1.0 / smoothZ;

        double[] numY = new double[nBins];
        double[] sumY = new double[nBins];
        double[] sumY2 = new double[nBins];

        // get transformation matrix into quick access variables.
        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;
        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        // precalculates this constant for rebinning
        double constant = 1;

        if ((refImage.max - refImage.min) != 0) {
            constant = (nBins - 1) / (refImage.max - refImage.min);
        }

        int nCalcs = 0;
        Point minMaxPt = new Point();

        for (z = 0; z <= zEnd; z++) { // zEnd = the bound of ref image
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x < minMaxPt.y; x++) {

                    // make function call to getTrilinear or move code here.
                    nCalcs++;
                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position2 = position1 + sliceSize;
                    position11 = position1 + 1;
                    position21 = position2 + 1;

                    // trilinear interpolation
                    b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]))) +
                         (dy * ((dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim])));

                    b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position21]))) +
                         (dy * ((dx1 * inputImage.data[position2 + xDim]) + (dx * inputImage.data[position21 + xDim])));

                    value = ((1 - dz) * b1) + (dz * b2);
                    // This was actually slower -> b1 + (b2-b1)*dz ??????

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

                    if (newPtZ < smoothZ) {
                        weight *= newPtZ * invSmoothZ;
                    } else if ((zEnd2 - newPtZ) < smoothZ) {
                        weight *= (zEnd2 - newPtZ) * invSmoothZ;
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
                    newPtZ += T20;
                }
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
        int totCalcs = zEnd * yEnd * xEnd;

        if (nCalcs < (0.15 * totCalcs)) {
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

        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index, indexValue;
        double value, wValue;
        double weight;
        double smoothX, smoothY, smoothZ;
        double invSmoothX, invSmoothY, invSmoothZ;
        int position1, position2, position1x, position2x;
        int position11, position21, position11x, position21x;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;
        double tmp;

        smoothX = smoothSize;
        smoothY = smoothSize;
        smoothZ = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;
        invSmoothZ = 1.0 / smoothZ;

        double[] numY = new double[nBins];
        double[] sumY = new double[nBins];
        double[] sumY2 = new double[nBins];

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
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

        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {
                    nCalcs++;
                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position1x = position1 + xDim;
                    position2 = position1 + sliceSize;
                    position2x = position2 + xDim;
                    position11 = position1 + 1;
                    position11x = position11 + xDim;
                    position21 = position2 + 1;
                    position21x = position21 + xDim;

                    b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]))) +
                         (dy * ((dx1 * inputImage.data[position1x]) + (dx * inputImage.data[position11x])));

                    b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position21]))) +
                         (dy * ((dx1 * inputImage.data[position2x]) + (dx * inputImage.data[position21x])));
                    value = ((1 - dz) * b1) + (dz * b2);

                    b1 = (dy1 * ((dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]))) +
                         (dy * ((dx1 * inputWgtImage.data[position1x]) + (dx * inputWgtImage.data[position11x])));

                    b2 = (dy1 * ((dx1 * inputWgtImage.data[position2]) + (dx * inputWgtImage.data[position21]))) +
                         (dy * ((dx1 * inputWgtImage.data[position2x]) + (dx * inputWgtImage.data[position21x])));
                    wValue = ((1 - dz) * b1) + (dz * b2);

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

                    if (newPtZ < smoothZ) {
                        weight *= newPtZ * invSmoothZ;
                    } else if ((zEnd2 - newPtZ) < smoothZ) {
                        weight *= (zEnd2 - newPtZ) * invSmoothZ;
                    }

                    if (weight < 0.0) {
                        weight = 0.0;
                    }

                    indexValue = (int) ((refImage.data[index] - refImage.min) * constant);
                    tmp = weight * (value - inputImage.min);
                    numY[indexValue] += weight;
                    sumY[indexValue] += tmp;
                    sumY2[indexValue] += tmp * (value - inputImage.min);

                    index++;
                    newPtX += T00;
                    newPtY += T10;
                    newPtZ += T20;
                }
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
        // penalizes cost based on the number of cost calculations over the
        // total possible if the number of cost calculation is less than 10000.

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
    private void findRangeX(Point minMaxPt, double newPtX, double newPtY, 
    		double newPtZ, double aT00, double aT10, double aT20, 
    		double iT00, double iT10, double iT20) {
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

        if (aT20 < 1.0e-8) {

            if ((0.0 <= newPtZ) && (newPtZ <= zEnd2)) {
                x1 = -1.0e8;
                x2 = 1.0e8;
            } else {
                x1 = -1.0e8;
                x2 = -1.0e8;
            }
        } else {
            x1 = -newPtZ * iT20;
            x2 = (zEnd2 - newPtZ) * iT20;
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
     * DOCUMENT ME!
     *
     * @param   data  DOCUMENT ME!
     * @param   x     DOCUMENT ME!
     * @param   y     DOCUMENT ME!
     * @param   z     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getSinc(float[] data, double x, double y, double z) {

        // kernel half-width  (i.e. range is +/- w)
        int w = sincWidth;
        int ix0, iy0, iz0;

        ix0 = (int) Math.floor(x);
        iy0 = (int) Math.floor(y);
        iz0 = (int) Math.floor(z);

        double convsum = 0.0, interpval = 0.0;
        double kersum = 0.0;

        for (int d = -w; d <= w; d++) {
            sincz[d + w] = kernelVal((z - iz0 + d), w);
            sincy[d + w] = kernelVal((y - iy0 + d), w);
            sincx[d + w] = kernelVal((x - ix0 + d), w);
        }

        int xj, yj, zj;

        for (int z1 = iz0 - w; z1 <= (iz0 + w); z1++) {
            zj = iz0 - z1 + w;

            for (int y1 = iy0 - w; y1 <= (iy0 + w); y1++) {
                yj = iy0 - y1 + w;

                for (int x1 = ix0 - w; x1 <= (ix0 + w); x1++) {
                    int index = (z1 * sliceSize) + (y1 * xDim) + x1;

                    if ((index >= 0) && (index < data.length)) {
                        xj = ix0 - x1 + w;

                        double sincfac = sincx[xj] * sincy[yj] * sincz[zj];

                        convsum += (data[index] * sincfac);
                        kersum += sincfac;
                    }
                }
            }
        }

        if (Math.abs(kersum) > 1e-9) {
            interpval = (convsum / kersum);
        } else {
            return 0;
        }

        return interpval;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     * @param   w  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double hanning(double x, int w) {

        if (Math.abs(x) > w) {
            return 0.0;
        } else {
            return (0.5 + (0.5 * Math.cos(M_PI * x / w)));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     * @param   w  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double kernelVal(double x, int w) {

        // effectively returns  sinc(x)*hanning(x,w);
        if (Math.abs(x) > w) {
            return 0.0f;
        }

        double dn = (x / w * 100.0) + 100;
        int n = (int) Math.floor(dn);

        dn -= n;

        if (n >= 200) {
            return 0.0f;
        }

        if (n < 0) {
            return 0.0f;
        }

        return ((sincKernel[n] * (1.0 - dn)) + (sincKernel[n + 1] * dn));
    }

    /**
     * Least squares cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double leastSquares(TransMatrix tMatrix) {

        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index;
        int position1, position2, position11, position21;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;

        double value, valueR, valueI;
        double sum = 0.0;
        long count = 0;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();

        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position2 = position1 + sliceSize;
                    position11 = position1 + 1;
                    position21 = position2 + 1;

                    b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]))) +
                         (dy * ((dx1 * inputImage.data[position1 + xDim]) + (dx * inputImage.data[position11 + xDim])));

                    b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position21]))) +
                         (dy * ((dx1 * inputImage.data[position2 + xDim]) + (dx * inputImage.data[position21 + xDim])));

                    value = ((1 - dz) * b1) + (dz * b2);

                    valueR = refImage.data[index] - refImage.min;
                    valueI = value - inputImage.min;
                    ;
                    sum += (valueR - valueI) * (valueR - valueI);

                    count++;
                    index++;
                    newPtX += T00;
                    newPtY += T10;
                    newPtZ += T20;
                }
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

        int x, y, z, c;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index;
        int position1, position2, position11, position21;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;

        double valueR, valueI;
        double sum = 0.0;
        long count = 0;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();

        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = 4 * (indexZ + (y * (xEnd + 1)) + minMaxPt.x);

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = 4 * ((intZ * sliceSize) + (intY * xDim) + intX);
                    position2 = position1 + (4 * sliceSize);
                    position11 = position1 + 4;
                    position21 = position2 + 4;

                    for (c = 1; c <= 3; c++) {

                        b1 = (dy1 * ((dx1 * inputImage.data[position1 + c]) + (dx * inputImage.data[position11 + c]))) +
                             (dy *
                                  ((dx1 * inputImage.data[position1 + (4 * xDim) + c]) +
                                       (dx * inputImage.data[position11 + (4 * xDim) + c])));

                        b2 = (dy1 * ((dx1 * inputImage.data[position2 + c]) + (dx * inputImage.data[position21 + c]))) +
                             (dy *
                                  ((dx1 * inputImage.data[position2 + (4 * xDim) + c]) +
                                       (dx * inputImage.data[position21 + (4 * xDim) + c])));

                        valueI = ((1 - dz) * b1) + (dz * b2);

                        valueR = refImage.data[index + c];
                        sum += (valueR - valueI) * (valueR - valueI);
                    } // for (c = 1; c <= 3; c++)

                    count++;
                    index += 4;
                    newPtX += T00;
                    newPtY += T10;
                    newPtZ += T20;
                }
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

        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index;
        double smoothX, smoothY, smoothZ;
        double invSmoothX, invSmoothY, invSmoothZ;
        double weight;
        int position1, position2;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;

        double value, valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;
        smoothZ = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;
        invSmoothZ = 1.0 / smoothZ;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();
        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position2 = position1 + sliceSize;

                    b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position1 + 1]))) +
                         (dy *
                              ((dx1 * inputImage.data[position1 + xDim]) +
                                   (dx * inputImage.data[position1 + xDim + 1])));

                    b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position2 + 1]))) +
                         (dy *
                              ((dx1 * inputImage.data[position2 + xDim]) +
                                   (dx * inputImage.data[position2 + xDim + 1])));

                    value = ((1 - dz) * b1) + (dz * b2);

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

                    if (newPtZ < smoothZ) {
                        weight *= newPtZ * invSmoothZ;
                    } else if ((zEnd2 - newPtZ) < smoothZ) {
                        weight *= (zEnd2 - newPtZ) * invSmoothZ;
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
                    newPtZ += T20;
                }
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

        int x, y, z, c;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index;
        double smoothX, smoothY, smoothZ;
        double invSmoothX, invSmoothY, invSmoothZ;
        double weight;
        int position1, position2;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;

        double valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;
        smoothZ = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;
        invSmoothZ = 1.0 / smoothZ;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();

        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = 4 * (indexZ + (y * (xEnd + 1)) + minMaxPt.x);

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = 4 * ((intZ * sliceSize) + (intY * xDim) + intX);
                    position2 = position1 + (4 * sliceSize);

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

                    if (newPtZ < smoothZ) {
                        weight *= newPtZ * invSmoothZ;
                    } else if ((zEnd2 - newPtZ) < smoothZ) {
                        weight *= (zEnd2 - newPtZ) * invSmoothZ;
                    }

                    if (weight < 0.0) {
                        weight = 0.0;
                    }

                    for (c = 1; c <= 3; c++) {
                        b1 = (dy1 *
                                  ((dx1 * inputImage.data[position1 + c]) + (dx * inputImage.data[position1 + 4 + c]))) +
                             (dy *
                                  ((dx1 * inputImage.data[position1 + (4 * xDim) + c]) +
                                       (dx * inputImage.data[position1 + (4 * xDim) + 4 + c])));

                        b2 = (dy1 *
                                  ((dx1 * inputImage.data[position2 + c]) + (dx * inputImage.data[position2 + 4 + c]))) +
                             (dy *
                                  ((dx1 * inputImage.data[position2 + (4 * xDim) + c]) +
                                       (dx * inputImage.data[position2 + (4 * xDim) + 4 + c])));

                        valueI = ((1 - dz) * b1) + (dz * b2);

                        valueR = refImage.data[index + c];

                        sum += weight * (valueR - valueI) * (valueR - valueI);
                    } // for (c = 1; c <= 3; c++)

                    index += 4;
                    count += weight;

                    newPtX += T00;
                    newPtY += T10;
                    newPtZ += T20;
                }
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

        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index;
        double smoothX, smoothY, smoothZ;
        double invSmoothX, invSmoothY, invSmoothZ;
        double weight;
        int position1, position2, position1x, position2x;
        int position11, position21, position11x, position21x;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;

        double value, wValue, valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;
        smoothZ = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;
        invSmoothZ = 1.0 / smoothZ;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();

        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position1x = position1 + xDim;
                    position2 = position1 + sliceSize;
                    position2x = position2 + xDim;
                    position11 = position1 + 1;
                    position11x = position11 + xDim;
                    position21 = position2 + 1;
                    position21x = position21 + xDim;

                    b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]))) +
                         (dy * ((dx1 * inputImage.data[position1x]) + (dx * inputImage.data[position11x])));

                    b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position21]))) +
                         (dy * ((dx1 * inputImage.data[position2x]) + (dx * inputImage.data[position21x])));
                    value = ((1 - dz) * b1) + (dz * b2);

                    b1 = (dy1 * ((dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]))) +
                         (dy * ((dx1 * inputWgtImage.data[position1x]) + (dx * inputWgtImage.data[position11x])));

                    b2 = (dy1 * ((dx1 * inputWgtImage.data[position2]) + (dx * inputWgtImage.data[position21]))) +
                         (dy * ((dx1 * inputWgtImage.data[position2x]) + (dx * inputWgtImage.data[position21x])));
                    wValue = ((1 - dz) * b1) + (dz * b2);

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

                    if (newPtZ < smoothZ) {
                        weight *= newPtZ * invSmoothZ;
                    } else if ((zEnd2 - newPtZ) < smoothZ) {
                        weight *= (zEnd2 - newPtZ) * invSmoothZ;
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
                    newPtZ += T20;
                }
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

        int x, y, z, c;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index;
        double smoothX, smoothY, smoothZ;
        double invSmoothX, invSmoothY, invSmoothZ;
        double weight;
        int position1, position2, position1x, position2x;
        int position11, position21, position11x, position21x;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
        double b1, b2;

        double wValue, valueR, valueI;
        double sum = 0.0;
        double count = 0.0;

        smoothX = smoothSize;
        smoothY = smoothSize;
        smoothZ = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;
        invSmoothZ = 1.0 / smoothZ;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();

        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position1x = position1 + xDim;
                    position2 = position1 + sliceSize;
                    position2x = position2 + xDim;
                    position11 = position1 + 1;
                    position11x = position11 + xDim;
                    position21 = position2 + 1;
                    position21x = position21 + xDim;

                    b1 = (dy1 * ((dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]))) +
                         (dy * ((dx1 * inputWgtImage.data[position1x]) + (dx * inputWgtImage.data[position11x])));

                    b2 = (dy1 * ((dx1 * inputWgtImage.data[position2]) + (dx * inputWgtImage.data[position21]))) +
                         (dy * ((dx1 * inputWgtImage.data[position2x]) + (dx * inputWgtImage.data[position21x])));
                    wValue = ((1 - dz) * b1) + (dz * b2);

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

                    if (newPtZ < smoothZ) {
                        weight *= newPtZ * invSmoothZ;
                    } else if ((zEnd2 - newPtZ) < smoothZ) {
                        weight *= (zEnd2 - newPtZ) * invSmoothZ;
                    }

                    if (weight < 0.0) {
                        weight = 0.0;
                    }

                    for (c = 1; c <= 3; c++) {
                        b1 = (dy1 *
                                  ((dx1 * inputImage.data[(4 * position1) + c]) +
                                       (dx * inputImage.data[(4 * position11) + c]))) +
                             (dy *
                                  ((dx1 * inputImage.data[(4 * position1x) + c]) +
                                       (dx * inputImage.data[(4 * position11x) + c])));

                        b2 = (dy1 *
                                  ((dx1 * inputImage.data[(4 * position2) + c]) +
                                       (dx * inputImage.data[(4 * position21) + c]))) +
                             (dy *
                                  ((dx1 * inputImage.data[(4 * position2x) + c]) +
                                       (dx * inputImage.data[(4 * position21x) + c])));
                        valueI = ((1 - dz) * b1) + (dz * b2);

                        valueR = refImage.data[(4 * index) + c];
                        sum += weight * (valueR - valueI) * (valueR - valueI);
                    } // for (c = 1; c <= 3; c++)

                    count += weight;
                    index++;

                    newPtX += T00;
                    newPtY += T10;
                    newPtZ += T20;
                }
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
    private double normalizedMutualInformationSmoothed(TransMatrix tMatrix) {

        double normalizedMI;
        double[] jointEntropy = { 0.0 };
        double[] margEntropyR = { 0.0 };
        double[] margEntropyI = { 0.0 };

        calcEntropySmoothed(tMatrix, jointEntropy, margEntropyR, margEntropyI);

        if (Math.abs(jointEntropy[0]) < 1e-9) { // if joint entropy is tiny, assume work cost function value, 1
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
     * Normalized cross-correlation cost function.
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double normalizedXCorrelation(TransMatrix tMatrix) {

        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index;
        double correlation;
        int position1, position2;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
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
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();

        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                    intX = (int) newPtX;
                    intY = (int) newPtY;
                    intZ = (int) newPtZ;

                    dx = newPtX - intX;
                    dy = newPtY - intY;
                    dz = newPtZ - intZ;

                    dx1 = 1 - dx;
                    dy1 = 1 - dy;

                    position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                    position2 = position1 + sliceSize;

                    b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position1 + 1]))) +
                         (dy *
                              ((dx1 * inputImage.data[position1 + xDim]) +
                                   (dx * inputImage.data[position1 + xDim + 1])));

                    b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position2 + 1]))) +
                         (dy *
                              ((dx1 * inputImage.data[position2 + xDim]) +
                                   (dx * inputImage.data[position2 + xDim + 1])));

                    value = ((1 - dz) * b1) + (dz * b2);

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
                    newPtZ += T20;
                }
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
    private double normalizedXCorrelationSinc(TransMatrix tMatrix) {

        int x, y, z;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index;
        double correlation;

        double value, valueR, valueI;
        double sumX = 0.0;
        double sumX2 = 0.0;
        double sumY = 0.0;
        double sumY2 = 0.0;
        double sumXY = 0.0;
        long count = 0;

        double varX, varY, varXY;
        double countSqr = 0;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        setupKernel(); // setups sinc kernal
        Point minMaxPt = new Point();

        for (z = 0; z <= zEnd; z++) {
            tmpZ1 = (z * T02) + T03;
            tmpZ2 = (z * T12) + T13;
            tmpZ3 = (z * T22) + T23;
            indexZ = z * refSliceSize;

            for (y = 0; y <= yEnd; y++) {

                newPtX = (y * T01) + tmpZ1;
                newPtY = (y * T11) + tmpZ2;
                newPtZ = (y * T21) + tmpZ3;

                // determine range
                findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                newPtX += minMaxPt.x * T00;
                newPtY += minMaxPt.x * T10;
                newPtZ += minMaxPt.x * T20;

                index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                for (x = minMaxPt.x; x < minMaxPt.y; x++) {

                    // value = inputImage.getTriLinear(newPtX,newPtY,newPtZ);
                    value = getSinc(inputImage.data, newPtX, newPtY, newPtZ);

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
                    newPtZ += T20;
                }
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

        int x, y, z, iter;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index;
        int numValues = 0;
        double correlation;
        int position1, position2;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
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

        double smoothX, smoothY, smoothZ;
        double invSmoothX, invSmoothY, invSmoothZ;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;
        smoothZ = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;
        invSmoothZ = 1.0 / smoothZ;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();

        for (iter = 0; iter <= 1; iter++) {

            for (z = 0; z <= zEnd; z++) {
                tmpZ1 = (z * T02) + T03;
                tmpZ2 = (z * T12) + T13;
                tmpZ3 = (z * T22) + T23;
                indexZ = z * refSliceSize;

                for (y = 0; y <= yEnd; y++) {

                    newPtX = (y * T01) + tmpZ1;
                    newPtY = (y * T11) + tmpZ2;
                    newPtZ = (y * T21) + tmpZ3;

                    // determine range
                    findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                    newPtX += minMaxPt.x * T00;
                    newPtY += minMaxPt.x * T10;
                    newPtZ += minMaxPt.x * T20;

                    index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                    for (x = minMaxPt.x; x < minMaxPt.y; x++) {

                        intX = (int) newPtX;
                        intY = (int) newPtY;
                        intZ = (int) newPtZ;

                        dx = newPtX - intX;
                        dy = newPtY - intY;
                        dz = newPtZ - intZ;

                        dx1 = 1 - dx;
                        dy1 = 1 - dy;

                        position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                        position2 = position1 + sliceSize;

                        b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position1 + 1]))) +
                             (dy *
                                  ((dx1 * inputImage.data[position1 + xDim]) +
                                       (dx * inputImage.data[position1 + xDim + 1])));

                        b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position2 + 1]))) +
                             (dy *
                                  ((dx1 * inputImage.data[position2 + xDim]) +
                                       (dx * inputImage.data[position2 + xDim + 1])));

                        value = ((1 - dz) * b1) + (dz * b2);

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

                        if (newPtZ < smoothZ) {
                            weight *= newPtZ * invSmoothZ;
                        } else if ((zEnd2 - newPtZ) < smoothZ) {
                            weight *= (zEnd2 - newPtZ) * invSmoothZ;
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
                        newPtZ += T20;
                    }
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

        int x, y, z, iter;
        double tmpZ1, tmpZ2, tmpZ3;
        int indexZ;
        int index;
        int numValues = 0;
        double correlation;
        int position1, position2, position1x, position2x;
        int position11, position21, position11x, position21x;
        int intX, intY, intZ;
        double dx, dy, dz, dx1, dy1;
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

        double smoothX, smoothY, smoothZ;
        double invSmoothX, invSmoothY, invSmoothZ;
        double weight;

        smoothX = smoothSize;
        smoothY = smoothSize;
        smoothZ = smoothSize;

        invSmoothX = 1.0 / smoothX;
        invSmoothY = 1.0 / smoothY;
        invSmoothZ = 1.0 / smoothZ;

        double T00 = tMatrix.M00;
        double T01 = tMatrix.M01;
        double T02 = tMatrix.M02;
        double T03 = tMatrix.M03;
        double T10 = tMatrix.M10;
        double T11 = tMatrix.M11;
        double T12 = tMatrix.M12;
        double T13 = tMatrix.M13;
        double T20 = tMatrix.M20;
        double T21 = tMatrix.M21;
        double T22 = tMatrix.M22;
        double T23 = tMatrix.M23;

        // the next variable are used in the "findRangeX" method. They are calculated only once to
        // speed up the cost function calculation
        double aT00 = T00;
        double aT10 = T10;
        double aT20 = T20;
        double iT00, iT10, iT20;
        double newPtX, newPtY, newPtZ;

        if (aT00 < 0) {
            aT00 = -aT00;
        }

        if (aT10 < 0) {
            aT10 = -aT10;
        }

        if (aT20 < 0) {
            aT20 = -aT20;
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

        if (aT20 >= 1.0e-8) {
            iT20 = 1 / T20;
        }else{
        	iT20 = Double.MAX_VALUE;
        }

        Point minMaxPt = new Point();

        for (iter = 0; iter <= 1; iter++) {

            for (z = 0; z <= zEnd; z++) {
                tmpZ1 = (z * T02) + T03;
                tmpZ2 = (z * T12) + T13;
                tmpZ3 = (z * T22) + T23;
                indexZ = z * refSliceSize;

                for (y = 0; y <= yEnd; y++) {

                    newPtX = (y * T01) + tmpZ1;
                    newPtY = (y * T11) + tmpZ2;
                    newPtZ = (y * T21) + tmpZ3;

                    // determine range
                    findRangeX(minMaxPt, newPtX, newPtY, newPtZ, aT00, aT10, aT20, iT00, iT10, iT20);

                    newPtX += minMaxPt.x * T00;
                    newPtY += minMaxPt.x * T10;
                    newPtZ += minMaxPt.x * T20;

                    index = indexZ + (y * (xEnd + 1)) + minMaxPt.x;

                    for (x = minMaxPt.x; x <= minMaxPt.y; x++) {

                        intX = (int) newPtX;
                        intY = (int) newPtY;
                        intZ = (int) newPtZ;

                        dx = newPtX - intX;
                        dy = newPtY - intY;
                        dz = newPtZ - intZ;

                        dx1 = 1 - dx;
                        dy1 = 1 - dy;

                        position1 = (intZ * sliceSize) + (intY * xDim) + intX;
                        position1x = position1 + xDim;
                        position2 = position1 + sliceSize;
                        position2x = position2 + xDim;
                        position11 = position1 + 1;
                        position11x = position11 + xDim;
                        position21 = position2 + 1;
                        position21x = position21 + xDim;

                        b1 = (dy1 * ((dx1 * inputImage.data[position1]) + (dx * inputImage.data[position11]))) +
                             (dy * ((dx1 * inputImage.data[position1x]) + (dx * inputImage.data[position11x])));

                        b2 = (dy1 * ((dx1 * inputImage.data[position2]) + (dx * inputImage.data[position21]))) +
                             (dy * ((dx1 * inputImage.data[position2x]) + (dx * inputImage.data[position21x])));
                        value = ((1 - dz) * b1) + (dz * b2);

                        b1 = (dy1 * ((dx1 * inputWgtImage.data[position1]) + (dx * inputWgtImage.data[position11]))) +
                             (dy * ((dx1 * inputWgtImage.data[position1x]) + (dx * inputWgtImage.data[position11x])));

                        b2 = (dy1 * ((dx1 * inputWgtImage.data[position2]) + (dx * inputWgtImage.data[position21]))) +
                             (dy * ((dx1 * inputWgtImage.data[position2x]) + (dx * inputWgtImage.data[position21x])));
                        wValue = ((1 - dz) * b1) + (dz * b2);

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

                        if (newPtZ < smoothZ) {
                            weight *= newPtZ * invSmoothZ;
                        } else if ((zEnd2 - newPtZ) < smoothZ) {
                            weight *= (zEnd2 - newPtZ) * invSmoothZ;
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
                        newPtZ += T20;
                    }
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
        double p = 0.0;

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

    /**
     * DOCUMENT ME!
     */
    private void setupKernel() {

        // set x between +/- kernelwidth
        for (int n = 0; n <= 200; n++) {
            double x = (n - 100) / 100.0f * sincWidth;

            sincKernel[n] = (sinc(x) * hanning(x, sincWidth));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double sinc(double x) {

        if (Math.abs(x) < 1e-7) {
            return 1.0 - Math.abs(x);
        }

        double y = M_PI * x;

        return Math.sin(y) / y;
    }

}
