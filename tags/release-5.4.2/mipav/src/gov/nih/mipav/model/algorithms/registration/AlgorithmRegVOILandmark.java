package gov.nih.mipav.model.algorithms.registration;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import de.jtem.numericalMethods.calculus.function.RealFunctionOfSeveralVariables;
import de.jtem.numericalMethods.calculus.minimizing.NelderMead;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * AlgorithmRegVOILandmark First slice is template (base image) to which subsequent slices are registered. Trace kidney
 * in 1st slice using polygon VOI. Minimize cost = -sum of Gradient magnitude under propagated ROI in match image
 *
 * @version  1.0 June, 2000
 * @author   Delia McGarry
 */
public class AlgorithmRegVOILandmark extends AlgorithmBase implements RealFunctionOfSeveralVariables {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int SIMPLEXOPT = 0;

    /** DOCUMENT ME! */
    public static final int EXHAUSTIVEOPT = 10;

    /** DOCUMENT ME! */
    public static final int MINDIFF = 20;

    /** DOCUMENT ME! */
    public static final int MAXSUM = 30;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected float baseVOIGradMagSum;

    /** DOCUMENT ME! */
    private AlgorithmGradientMagnitude gradientMagAlgo;

    /** DOCUMENT ME! */
    protected float[] gradMagBuf = null;

    /** DOCUMENT ME! */
    private double minTx, maxTx, minTy, maxTy, minRz, maxRz, step;

    /** DOCUMENT ME! */
    protected int opt, costFunc;

    /** DOCUMENT ME! */
    private int simplexDim;

    /** DOCUMENT ME! */
    protected int VOIlength;

    /** positions (x,y) of VOI in first slice */
    protected VOIBase VOIposition = null;

    /** DOCUMENT ME! */
    protected int volLength, sliceSize;

    /** DOCUMENT ME! */
    protected ModelImage volume, gradMagVol;

    /** DOCUMENT ME! */
    protected int xdim, ydim, tdim;

    private float [] sigmas;
    private boolean maskFlag;
    private CostFunction func = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  volume      DOCUMENT ME!
     * @param  gradMagVol  DOCUMENT ME!
     * @param  sigmas      DOCUMENT ME!
     * @param  maskFlag    DOCUMENT ME!
     * @param  position    DOCUMENT ME!
     * @param  minTx       DOCUMENT ME!
     * @param  maxTx       DOCUMENT ME!
     * @param  minTy       DOCUMENT ME!
     * @param  maxTy       DOCUMENT ME!
     * @param  minRz       DOCUMENT ME!
     * @param  maxRz       DOCUMENT ME!
     * @param  step        DOCUMENT ME!
     * @param  opt         DOCUMENT ME!
     * @param  costFunc    DOCUMENT ME!
     */
    public AlgorithmRegVOILandmark(ModelImage volume, ModelImage gradMagVol, float[] sigmas, boolean maskFlag,
            VOIBase position, double minTx, double maxTx, double minTy, double maxTy,
            double minRz, double maxRz, double step, int opt, int costFunc) {
        super(volume, gradMagVol);
        this.volume = volume;
        this.gradMagVol = gradMagVol;
        this.opt = opt;
        this.costFunc = costFunc;
        this.step = step;
        this.minTx = minTx;
        this.minTy = minTy;
        this.maxTx = maxTx;
        this.maxTy = maxTy;
        this.minRz = minRz;
        this.maxRz = maxRz;
        this.sigmas = sigmas;
        this.maskFlag = maskFlag;
        xdim = volume.getExtents()[0];
        ydim = volume.getExtents()[1];
        tdim = volume.getExtents()[2];
        VOIlength = position.size();
        VOIposition = position;

        // VOIintensity = intensity;
        baseVOIGradMagSum = 0f;
        sliceSize = xdim * ydim;
        volLength = sliceSize * tdim;
        simplexDim = 3;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize - prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int i;
        gradientMagAlgo = new AlgorithmGradientMagnitude(gradMagVol, volume, sigmas, maskFlag, true);
        gradientMagAlgo.setProgressValues(generateProgressValues(0,10));

        try {
            gradientMagAlgo.setRunningInSeparateThread(runningInSeparateThread);
            gradientMagAlgo.run();
            gradMagBuf = new float[volLength];
            gradMagVol.exportData(0, volLength, gradMagBuf); // copy volume into 1D array
        } catch (IOException error) {
            displayError("Algorithm: Image(s) locked");
            setCompleted(false);
            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm: Out of memory");
            setCompleted(false);
            return;
        }

        for (i = 0; i < VOIlength; i++) {
            baseVOIGradMagSum += gradMagBuf[(int) (VOIposition.elementAt(i).X + (VOIposition.elementAt(i).Y * xdim))];
        }
        Preferences.debug("baseVOIGradMagSum = " + baseVOIGradMagSum + "\n",Preferences.DEBUG_ALGORITHM);
        search();
        if ( gradMagVol != null ) {
        	gradMagVol.disposeLocal();
        	gradMagVol = null;
        }
        new ViewJFrameImage(volume);
        setCompleted(true);
    }

    /**
     * Exhaustive search for global minimum.
     *
     * @param  p     DOCUMENT ME!
     * @param  func  DOCUMENT ME!
     */
    private void exhaustiveSearch(double[] p, CostFunction func) {
        double minCost = Double.MAX_VALUE;
        double cost = 0f;
        double X, Y, R;
        double minX = 0f;
        double minY = 0f;
        double minR = 0f;

        for (int i = 0; i < simplexDim; i++) {
            p[i] = 0f;
        }

        for (Y = minTy; Y <= maxTy; Y += step) {
            for (X = minTx; X <= maxTx; X += step) {
                for (R = minRz; R <= maxRz; R++) {
                    p[0] = X;
                    p[1] = Y;
                    p[2] = R;                    
                    cost = func.cost(p);
                    if (cost < minCost) {
                        minCost = cost;
                        minX = X;
                        minY = Y;
                        minR = R;
                    }
                }
            }
        }
        p[0] = minX;
        p[1] = minY;
        p[2] = minR;
        Preferences.debug(" mincost = " + minCost + "\n",Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Converts row of p into 3x3 matrix.
     *
     * @param   x  = row of p[][]
     *
     * @return  DOCUMENT ME!
     */
    protected TransMatrix getTransform(double[] x) {
        TransMatrix xfrm = new TransMatrix(3);
        xfrm.setTransform(x[0], x[1], x[2]);
        return xfrm;
    }

    /**
     * Search.
     */
    private void search() {
        Preferences.debug("Search:\n",Preferences.DEBUG_ALGORITHM);

        TransMatrix xfrm;
        float increment;
        float[] imgBuf = null;
        float[] tImgBuf = null;

        try {
            func = new CostFunction();
            imgBuf = new float[sliceSize];
            tImgBuf = new float[sliceSize];
            increment = 90.0f / (tdim - 2);

        } catch (OutOfMemoryError e) {
            displayError("Algorithm Reg Kidney:  Out of Memory");
            setCompleted(false);
            return;
        }

        fireProgressStateChanged(-1, null, "Registering ...");

        for (int t = 1; t < tdim; t++) {

            fireProgressStateChanged( ((increment * t)/(increment * tdim)), null, null);

            func.setSlice(t);

            double[] initialPoint = new double[]{0,0,0};
            if (opt == EXHAUSTIVEOPT) {
                exhaustiveSearch(initialPoint, func);
            } else {
                double scale = 1.0/0.00024;
                double[][] xi = new double[][]{{scale * (maxTx - minTx),0,0},
                        {0,scale*(maxTy-minTy),0},{0,0,scale*(maxRz-minRz)}};
                double dCost = NelderMead.search(initialPoint, xi,
                        0.0000001, this, 50000, null);
                Preferences.debug("cost = " + dCost + "\n",Preferences.DEBUG_ALGORITHM);
            }

            xfrm = getTransform(initialPoint);            
            xfrm.Inverse();                     
            transformSlice(t, xfrm, imgBuf, tImgBuf, volume.getImageCenter());
            Preferences.debug(xfrm.toString(),Preferences.DEBUG_ALGORITHM);

            // Preferences.debug( "Transformed slice "+t+"\n");
            Preferences.debug("*******************************************\n",Preferences.DEBUG_ALGORITHM);
        }
    }

    /**
     * Transforms a slice.
     *
     * @param  slice    DOCUMENT ME!
     * @param  xfrm     DOCUMENT ME!
     * @param  imgBuf   DOCUMENT ME!
     * @param  tImgBuf  DOCUMENT ME!
     */
    private void transformSlice(int slice, TransMatrix xfrm, float[] imgBuf, float[] tImgBuf, Vector3f center) {
        int sliceIndex = slice * sliceSize;

        try {
            volume.exportData(sliceIndex, sliceSize, imgBuf); // copy volume into 1D array
            int[] extentsTemp = new int[]{xdim,ydim};
            ModelImage volumeTemp = new ModelImage( volume.getType(), extentsTemp, volume.getImageName()+"_"+slice );
            volumeTemp.importData(imgBuf);

            AlgorithmTransform algoTrans = new AlgorithmTransform(volumeTemp, xfrm, AlgorithmTransform.BILINEAR, 
                    volume.getFileInfo(0).getResolutions()[0], volume.getFileInfo(0).getResolutions()[1], 
                    xdim, ydim, volume.getFileInfo(0).getUnitsOfMeasure(), false, true,
                    false, true, center);
            algoTrans.setFillValue((float)volume.getMin());
            algoTrans.setUpdateOriginFlag(true);
            algoTrans.run();
            ModelImage resultImage = algoTrans.getTransformedImage();
            resultImage.exportData(0, sliceSize, tImgBuf);

            volume.importData(sliceIndex, tImgBuf, false); // copy imgBuff back into image

            resultImage.disposeLocal();
            volumeTemp.disposeLocal();
            System.gc();
        } catch (IOException error) {
            displayError("Algorithm: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            imgBuf = null;
            System.gc();
            displayError("Algorithm: Out of memory");
            setCompleted(false);
            return;
        }
        volume.calcMinMax();
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * CostFunction - class for specifying optimization function for AlgorithmSimplexOpt slice - current fluoroscopy
     * slice being operated on.
     */
    private class CostFunction implements AlgorithmOptimizeFunctionBase {

        /** DOCUMENT ME! */
        int slice;

        /**
         * CostFunction constructor.
         */
        public CostFunction() {
            slice = 0;
        }
        public int getCostFunction()
        {
            return 0;
        }
        /**
         * DOCUMENT ME!
         *
         * @param   tMatrix  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double cost(TransMatrix tMatrix) {

            // Filler to implement cost functions from AlgorithmOptimizeFunctionBase
            return -1;
        }
        
        /**
         * DOCUMENT ME!
         *
         * @param   tMatrix  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double cost(TransMatrixd tMatrix) {

            // Filler to implement cost functions from AlgorithmOptimizeFunctionBase
            return -1;
        }

        /**
         * Cost cost = intensity sum of errors = sum_i(I[i] - I'[i]) x[] = row of p[][].
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double cost(double[] x) {
            int i;
            int slicePos = slice * sliceSize;
            int X0pos, Y0pos;
            int X1pos, Y1pos;
            float X, Y;
            float x0, y0;
            float x1, y1;
            float value;
            double Cost = 0.0f;
            TransMatrix xfrm;
            xfrm = getTransform(x);

            Vector3f[] matchVOIposition = new Vector3f[VOIlength];
            Vector3f[] VOIpositionTemp = new Vector3f[VOIlength];

            Vector3f gcPt = volume.getImageCenter();
            // Translate to image center:
            for (i = 0; i < VOIlength; i++) {
                matchVOIposition[i] = new Vector3f();
                VOIpositionTemp[i] = new Vector3f(VOIposition.elementAt(i));
                VOIpositionTemp[i].Sub(gcPt);
            }
            // Transform VOI based on new transform
            xfrm.transformAsVector3Df(VOIpositionTemp, matchVOIposition);
            // Translate from image center:
            for (i = 0; i < VOIlength; i++) {
                matchVOIposition[i].Add(gcPt);
                // The Z should be the current slice 
                matchVOIposition[i].Z = slice;
            }
            // Compare the gradient magnitude values for each point on the original contour with the
            // transformed contour. Take the sum of squares of differences as the error:
            for (i = 0; i < VOIlength; i++) {
                value = 0;
                X = matchVOIposition[i].X;

                if ((X >= 0) && (X < (xdim - 1))) {
                    Y = matchVOIposition[i].Y;

                    if ((Y >= 0) && (Y < (ydim - 1))) {
                        x0 = X - (int) X; // bilinear interp
                        y0 = Y - (int) Y;
                        x1 = 1 - x0;
                        y1 = 1 - y0;
                        X0pos = (int) X;
                        Y0pos = (int) Y * xdim;
                        X1pos = X0pos + 1;
                        Y1pos = Y0pos + xdim;
                        float transVal = (x1 * y1 * gradMagBuf[Y0pos + X0pos + slicePos]) +
                        (x0 * y1 * gradMagBuf[Y0pos + X1pos + slicePos]) +
                        (x1 * y0 * gradMagBuf[Y1pos + X0pos + slicePos]) +
                        (x0 * y0 * gradMagBuf[Y1pos + X1pos + slicePos]);

                        float origVal = gradMagBuf[(int) (VOIposition.elementAt(i).X + (VOIposition.elementAt(i).Y * xdim))];
                        // Get the difference for each point on the original contour with the transformed contour:
                        value = origVal - transVal;
                        // square the difference:
                        value *= value;
                    }
                }
                // Return sum of squares:
                Cost += value; 
            }
            return Cost;
        }


        /**
         * DOCUMENT ME!
         *
         * @param  t  DOCUMENT ME!
         */
        protected void setSlice(int t) {
            slice = t;
        }
    }

    @Override
    public double eval(double[] x) {
        return func.cost(x);
    }

    @Override
    public int getNumberOfVariables() {
        return simplexDim;
    }

}
