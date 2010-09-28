package gov.nih.mipav.model.algorithms.registration;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.NumericalAnalysis.function.RealFunctionOfSeveralVariables;
import WildMagic.LibFoundation.NumericalAnalysis.minimizing.NelderMead;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import javax.swing.*;


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
    private float baseVOIGradMagSum;

    /** DOCUMENT ME! */
    private AlgorithmGradientMagnitude gradientMagAlgo;

    /** DOCUMENT ME! */
    private float[] gradMagBuf = null;

    /** DOCUMENT ME! */
    private double minTx, maxTx, minTy, maxTy, minRz, maxRz, step;

    /** DOCUMENT ME! */
    private int opt, costFunc;

    /** DOCUMENT ME! */
    //private AlgorithmSimplexOpt simplex;

    /** DOCUMENT ME! */
    private int simplexDim;

    /** DOCUMENT ME! */
    private int VOIlength;

    /** DOCUMENT ME! */
    private Vector3f[] VOIposition = null; // positions (x,y) of VOI in first slice

    /** DOCUMENT ME! */
    private int volLength, sliceSize;

    /** DOCUMENT ME! */
    private ModelImage volume, gradMagVol;

    /** DOCUMENT ME! */
    private int xdim, ydim, tdim;

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
    		Vector3f[] position, double minTx, double maxTx, double minTy, double maxTy,
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
        xdim = volume.getExtents()[0];
        ydim = volume.getExtents()[1];
        tdim = volume.getExtents()[2];
        VOIlength = position.length;
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
            gradMagVol.convertToFloat();
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
            baseVOIGradMagSum += gradMagBuf[(int) (VOIposition[i].X + (VOIposition[i].Y * xdim))];
        }

        Preferences.debug("baseVOIGradMagSum = " + baseVOIGradMagSum + "\n");
        Search();
        if(gradMagVol != null) {
        	gradMagVol.disposeLocal();
        	gradMagVol = null;
        }
        new ViewJFrameImage(volume);
        setCompleted(true);
    }

    /**
     * displayError - displays the error.
     *
     * @param  error  the error
     * @param  title  DOCUMENT ME!
     */
    private void displayError(String error, String title) {
        JOptionPane.showMessageDialog(null, error, title, JOptionPane.ERROR_MESSAGE);

        return;
    }

    /**
     * Exhaustive search for global minimum.
     *
     * @param  p     DOCUMENT ME!
     * @param  func  DOCUMENT ME!
     */
    private void exhaustiveSearch(double[][] p, CostFunction func) {
        int i, j;
        double minCost = Double.MAX_VALUE;
        double cost = 0f;
        double X, Y, R;
        double minX = 0f;
        double minY = 0f;
        double minR = 0f;

        for (i = 0; i < (simplexDim + 1); i++) { // initialize p matrix to 0

            for (j = 0; j < simplexDim; j++) {
                p[i][j] = 0f;
            }
        }

        for (Y = minTy; Y < maxTy; Y += step) {

            for (X = minTx; X < maxTx; X += step) {

                for (R = minRz; R < maxRz; R++) {
                    p[0][0] = X;
                    p[0][1] = Y;
                    p[0][2] = R;
                    cost = func.cost(p[0]);

                    if (cost < minCost) {
                        minCost = cost;
                        minX = X;
                        minY = Y;
                        minR = R;
                    }
                }
            }
        }

        p[0][0] = minX;
        p[0][1] = minY;
        p[0][2] = minR;
        Preferences.debug(" mincost = " + minCost + "\n");
    }

    /**
     * Converts row of p into 3x3 matrix.
     *
     * @param   x  = row of p[][]
     *
     * @return  DOCUMENT ME!
     */
    private TransMatrix getTransform(double[] x) {
        TransMatrix xfrm = new TransMatrix(3);
        xfrm.setTransform(x[0], x[1], x[2]);

        // xfrm.setTranslate(x[0], x[1]);
        // xfrm.setRotate(x[2]);
        // xfrm.print();
        return xfrm;
    }

    /**
     * InitializePandY p[i][j] = each i is a simplex vertex; each j is a parameter tx, ty, or r y[i] = cost of position
     * corresponding to row i of p[][].
     *
     * @param  p     DOCUMENT ME!
     * @param  y     DOCUMENT ME!
     * @param  func  DOCUMENT ME!
     */
    private void InitializePandY(double[][] p, double[] y, CostFunction func) {
        Preferences.debug("InitializePandY:\n");

        int i, j;

        for (i = 0; i < (simplexDim + 1); i++) {

            for (j = 0; j < (simplexDim + 1); j++) {
                p[i][j] = 0f;
            }
        }

        p[0][0] = 5f; // Tx
        p[1][1] = 5f; // Ty

        // p[2][0] = -5f; //Tx again
        p[2][2] = 5f; // Rot about z-axis (degrees)

        for (i = 0; i < (simplexDim + 1); i++) {

            for (j = 0; j < (simplexDim + 1); j++) {
                Preferences.debug("p[" + i + "][" + j + "]=" + p[i][j] + "\n");
            }
        }

        for (i = 0; i < (simplexDim + 1); i++) {
            y[i] = func.cost(p[i]);
            Preferences.debug("y[" + i + "] = " + y[i] + "\n");
        }

        Preferences.debug("InitializePandY done\n");
    }

    /**
     * Search.
     */
    private void Search() {
        Preferences.debug("Search:\n");

        TransMatrix xfrm;
        float increment;
        float[] imgBuf = null;
        float[] tImgBuf = null;
        double[][] p = null;
        double[] y = null;
        // Matrix inverseXfrm = new Matrix(3,3);

        try {
            func = new CostFunction();
            imgBuf = new float[sliceSize];
            tImgBuf = new float[sliceSize];
            p = new double[simplexDim + 1][simplexDim];
            y = new double[simplexDim + 1];
            increment = 90.0f / (tdim - 2);
            //simplex = new AlgorithmSimplexOpt(p, y, simplexDim, func);
           
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Reg Kidney:  Out of Memory");
            setCompleted(false);
            return;
        }

        fireProgressStateChanged(-1, null, "Registering ...");
        
        for (int t = 1; t < tdim; t++) {

            fireProgressStateChanged( ((increment * t)/(increment * tdim)), null, null);
        

            func.setSlice(t);

            if (opt == EXHAUSTIVEOPT) {
                exhaustiveSearch(p, func);
            } else {
                //p = new double[simplexDim + 1][simplexDim + 1];
                //y = new double[simplexDim + 1];
                //InitializePandY(p, y, func);
                double[] initialPoint = new double[simplexDim];
                double dCost = NelderMead.search( initialPoint, 0.0000001, this );
                Preferences.debug("cost = " + dCost + "\n");
                p[0] = initialPoint;
                //NelderMead.search( y, p, 0.0000001, this, 5000, null );
                //simplex.setP(p);
                //simplex.setY(y);
                //simplex.setRunningInSeparateThread(runningInSeparateThread);
                //simplex.run();
                //Preferences.debug("cost = " + y[0] + "\n");
            }

            xfrm = getTransform(p[0]); // lowest cost row
            xfrm.Inverse();
            Preferences.debug("Transformation for slice " + t + " =\n");

            // xfrm.print()
            transformSlice(t, xfrm, imgBuf, tImgBuf);
            Preferences.debug(xfrm.toString());

            // Preferences.debug( "Transformed slice "+t+"\n");
            Preferences.debug("*******************************************\n");
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
    private void transformSlice(int slice, TransMatrix xfrm, float[] imgBuf, float[] tImgBuf) {
        // float xres = volume.getFileInfo(0).getResolutions()[0]; float yres =
        // volume.getFileInfo(0).getResolutions()[1]; float tres = volume.getFileInfo(0).getResolutions()[2];

        int sliceIndex = slice * sliceSize;

        try {
            volume.exportData(sliceIndex, sliceSize, imgBuf); // copy volume into 1D array

            AlgorithmTransform.transformBilinear(imgBuf, tImgBuf, xfrm, xdim, ydim,
                                                 volume.getFileInfo(0).getResolutions()[0],
                                                 volume.getFileInfo(0).getResolutions()[1], xdim, ydim,
                                                 volume.getFileInfo(0).getResolutions()[0],
                                                 volume.getFileInfo(0).getResolutions()[1], null);
            volume.importData(sliceIndex, tImgBuf, false); // copy imgBuff back into image
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

            for (i = 0; i < VOIlength; i++) {
                matchVOIposition[i] = new Vector3f();
            }

            xfrm.transformAsVector3Df(VOIposition, matchVOIposition);

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
                        value = (x1 * y1 * gradMagBuf[Y0pos + X0pos + slicePos]) +
                                (x0 * y1 * gradMagBuf[Y0pos + X1pos + slicePos]) +
                                (x1 * y0 * gradMagBuf[Y1pos + X0pos + slicePos]) +
                                (x0 * y0 * gradMagBuf[Y1pos + X1pos + slicePos]);
                        //  volIndex = (int)matchVOIposition[i].x +        (int)matchVOIposition[i].y*xdim +
                        // slice*sliceSize;
                    }
                }

                Cost -= value; // cost = -(sum of Grad. Mag. intensities under transformed VOI)
            }

            if (costFunc == MINDIFF) {
                Cost = Math.abs(baseVOIGradMagSum + Cost);
            } // minimize difference between sums under VOI

            return Cost;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   es  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double cost(double[][] es) {
            double value = 0;

            return value;
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
