package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.util.DoubleDouble;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.BitSet;
import java.util.concurrent.*;

import de.jtem.numericalMethods.calculus.function.RealFunctionOfOneVariable;
import de.jtem.numericalMethods.calculus.integration.RungeKuttaFehlbergIntegrator;


/**
 * Based on the document provided by Daniel Reich: Notes on DCE with SM2 (standard model, aka Tofts model,
 * 2-compartment) 3 model parameters are fit for each voxel in 3D: 1) K_trans in [1.0E-5, 5.0] in /min On input ktrans
 * is converted from /min to /sec and on output ktrans is converted from /sec to /min. 2) ve in [1.0E-5, 0.99] 3) f_vp
 * in [0, 0.99]
 * 
 * srcImage is a dynamic "4D volume" of MRI signal (3D over time). The equation used here is virtually identical to
 * equation 10.3 in T1-w DCE-MRI. Ct(t) = vpCp(t) + ktrans*integral from 0 to t of Cp(t') * exp[-ktrans*(t - t')/ve]dt'
 * (10.3) Cp is the concentration of agent in the vascular plasma space, vp. ve is the volume of the extravascular
 * extracellular space per unit volume of tissue. ktrans is the volume transfer constant between vp and ve. Ct is the
 * tracer concentration in the tissue as a whole. Our equation is: R1(t) - R10 = (vp * [R1,p(t) - R10,p] +
 * ktrans*integral from 0 to t of [R1,p(tau) - R10,p]*exp[-ktrans*(t - tau)/ve]d(tau))/(1 - h) where R10 is the tissue
 * R1 map in the absence of contrast material, a 3D volume R1(tj) is the tissue R1 map during contrast infusion, a 4D
 * data set. h = hematocrit with a default value of 0.4. With the sagittal sinus VOI: Average over this VOI to extract
 * the constant R10,p from the R10 map. Average over this VOI to extract the time series R1,p(tj) from the R1(tj) 4D
 * data set. tj is a vector of the center times for each volume, where j = 1,...,n and n is the number of post-contrast
 * volumes.
 * 
 * Solve the equation numerically assuming piecewise linearity and using the trapezoid rule.
 * 
 * The pre-injection volumes are actually averaged together to generate the R10 map. So you're right, they wouldn't
 * provide any additional information, and we don't need to fit them. After injection (t=t1), the first volume usually
 * has no contrast in it. That's because the contrast has to circulate to the brain. So R1,p(t1) should be the same as
 * R10,p, but in our scheme R1,p(t1) wasn't used to generate the R10 map. Any differences should be due to noise. Does
 * that help explain things?
 * 
 * For monomeric gadolinium chelates, Ktrans for the normal blood brain barrier is approximately 10^-4 min^-1 (from the
 * legend to Fig 5 of Li, Rooney, and Springer (MRM, 2005)). So a lower bound of 10^-5 should be ok. If R1,p(t1) = R10,p
 * except for noise, then R1,p(t1) - R10,p is purely noise and contains no useful information
 * 
 * 
 * 
 * References: 1.) "A Unified Magnetic Resonance Imaging Pharmacokinetic Theory: Intravascular and Extracellular
 * Contrast Reagents" by Xin Li, William D. Rooney, and Charles S. Springer, Jr., Magnetic Resonance in Medicine, Vol.
 * 54, 2005, pp. 1351-1359. 2.) Erratum: Magnetic Resonance in Medicine, Vol. 55, 2006, p.1217. 3.) Quantitative MRI of
 * the Brain, Edited by Paul Tofts, 2003, John Wiley & Sons, Ltd., ISBN: 0-47084721-2, Chapter 10, T1-w DCE-MRI:
 * T1-weighted Dynamic Contrast-enhanced MRI by Geoff J. M. Parker and Anwar R. Padhani, pp. 341-364. 4.) Larsson HBW,
 * Courivaud F, Rostrup E, Hansen AE. Measurement of brain perfusion, blood volume, and blood-brain barrier
 * permeability, using dynamic contrast-enhanced T1-weighted MRI at 3 tesla. Magnetic Resonance in Medicine 2009;
 * 62(5):1270-1281. 5.) Li X, Rooney WD, Springer CS. A unified magnetic resonance imaging pharmacokinetic theory:
 * intravascular and extracellular contrast reagents. Magnetic Resonance in Medicine 2005 Dec; 54(6): 1351-1359. 6.)
 * Tofts PS, Modeling tracer kinetics in dynamic Gd-DPTA MR imaging. Journal of Magnetic Resonance Imaging, 1997, 7(1),
 * pp. 91-101. 7.) Tofts PS, Brix G, Buckley DL, Evelhoch JL, Henderson E, Knopp MV, Larsson HB, Mayr NA, Parker GJ,
 * Port RE, Taylor J, Weisskoff RM. Estimating kinetic parameters from dynamic contrast-enhanced T(1)-weighted MRI of a
 * diffusable tracer: standardized quantitites and symbols. J. Magn. Reson Imaging 1999 Sep; 10(3), pp. 223-232.
 */
public class AlgorithmSM2 extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private ModelImage destImage[] = null;

    /** A vector of center times for each volume */
    private double timeVals[] = null;

    private double r1t0[];

    private double r1tj[];

    private double r1pt0;

    private double r1ptj[];

    private double min_constr[];

    private double max_constr[];

    // hematocrit
    private final double h;

    private final int sagittalSinusIndex;

    private int processRegionIndex;

    private double ymodel[];

    private int i;

    private int xDim;

    private int yDim;

    private int zDim;

    private int tDim;

    private int volSize;

    private double initial[];

    private double trapezoidMidpoint[];

    private double trapezoidConstant[];

    private double trapezoidSlope[];

    private double ktransDivVe;

    private double exparray[][];

    private int[] exitStatus;

    private final int[] paramNaN = new int[3];

    private final int[] paramInf = new int[3];

    private final double[] paramMin = new double[3];

    private final double[] paramMax = new double[3];

    private int processors;

    private float destArray[][];

    private int destExitStatusArray[];

    private long voxelsProcessed = 0;

    private int barMarker = 0;

    private int oldBarMarker = 0;

    private ModelImage tissueImage;

    private BitSet bitMask = null;

    private int validVoxels = 0;

    private double ktransTotal = 0.0;

    private double veTotal = 0.0;

    private double vpTotal = 0.0;

    private boolean wholeImage = true;

    private final int elsuncEngine = 1;

    private final int nl2solEngine = 2;

    // removed DQED class for licensing reasons
    // private final int dqedEngine = 3;

    private final int fittingEngine = elsuncEngine;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmDEMRI3 object.
     * 
     */
    public AlgorithmSM2(final ModelImage destImage[], final ModelImage srcImage, final double min_constr[],
            final double max_constr[], final double initial[], final ModelImage tissueImage, final double timeVals[],
            final double h, final int sagittalSinusIndex, final int processRegionIndex) {

        super(null, srcImage);
        this.destImage = destImage;
        this.min_constr = min_constr;
        this.max_constr = max_constr;
        this.initial = initial;
        this.tissueImage = tissueImage;
        this.timeVals = timeVals;
        this.h = h;
        this.sagittalSinusIndex = sagittalSinusIndex;
        this.processRegionIndex = processRegionIndex;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        int i;
        srcImage = null;
        for (i = 0; i < 4; i++) {
            destImage[i] = null;
        }
        destImage = null;
        min_constr = null;
        max_constr = null;
        if (tissueImage != null) {
            tissueImage.disposeLocal();
            tissueImage = null;
        }
        initial = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {
        ViewVOIVector VOIs;
        short mask[];
        int t;
        double y_array[];
        int size4D;
        FitSM2ConstrainedModel dModel;
        FitSM2nl2solModel nl2Model;
        double[] params;
        float chi_squared;
        int j;
        // If true, run a self test of NLConstrainedEngineEP.java
        final boolean nlConstrainedEngineEPTest = false;
        // If true, run a self test of NLConstrainedEngine.java
        final boolean nlConstrainedEngineTest = false;
        final boolean selfTest = false;
        final boolean selfTest2 = false;
        final boolean nl2solTest = false;
        // final boolean dqedTest = false;
        final boolean Integration2Test = false;
        final boolean Integration2EPTest = false;
        int voiCount;
        double delT;
        long normalTerminations = 0;
        long abnormalTerminations = 0;
        double paramSecs0;
        double initial0_original;
        double min0_original;
        double max0_original;
        double ktransAverage;
        double veAverage;
        double vpAverage;

        if (selfTest) {
            int i;
            final String fileList[] = new String[661];
            String seq;
            final String zero = "0";
            final String baseString = "dcemri_testversion4_";
            final String ext = ".dcm";
            String selectedFileName;
            final boolean performSort = false;
            FileIO fileIO;
            ModelImage dceImage;
            double dceData[];
            int sliceSize;
            int dataSize;
            int x;
            int y;
            final double ktransArray[] = new double[] {0.0, 0.01, 0.02, 0.05, 0.1, 0.2};
            final double veArray[] = new double[] {0.1, 0.2, 0.5};
            final double vpArray[] = new double[] {0.001, 0.005, 0.01, 0.02, 0.05, 0.1};
            int ktransIndex;
            int yIndex;
            int veIndex;
            int vpIndex;
            double ktransActual;
            double veActual;
            double vpActual;
            String timeString;
            int status;
            int firstColonIndex;
            int lastColonIndex;
            String hourString;
            String minuteString;
            String secondString;

            xDim = 60;
            yDim = 200;
            sliceSize = xDim * yDim;
            tDim = 661;
            dataSize = sliceSize * tDim;
            dceData = new double[dataSize];
            for (i = 1; i <= 661; i++) {
                seq = String.valueOf(i);
                while (seq.length() < 3) {
                    seq = zero.concat(seq);
                }
                fileList[i - 1] = baseString + seq + ext;
            } // for (i = 1; i <= 661; i++)
            selectedFileName = fileList[0];
            fileIO = new FileIO();
            fileIO.setFileDir("C:" + File.separator + "DCE_MRI" + File.separator + "QIBA_v4" + File.separator
                    + "Dynamic_v4" + File.separator + "DICOM" + File.separator);
            dceImage = fileIO.readDicom(selectedFileName, fileList, performSort);
            final FileInfoBase[] fileInfo = dceImage.getFileInfo();
            timeVals = new double[tDim];
            for (t = 0; t < tDim; t++) {
                timeString = ((String) ((FileInfoDicom) fileInfo[t]).getTagTable().getValue("0008,0032"));
                firstColonIndex = timeString.indexOf(":", 0);
                lastColonIndex = timeString.lastIndexOf(":", timeString.length() - 1);
                hourString = timeString.substring(0, firstColonIndex);
                minuteString = timeString.substring(firstColonIndex + 1, lastColonIndex);
                secondString = timeString.substring(lastColonIndex + 1);
                timeVals[t] = 3600.0 * Double.valueOf(hourString).doubleValue() + 60.0
                        * Double.valueOf(minuteString).doubleValue() + Double.valueOf(secondString).doubleValue();
            }

            try {
                dceImage.exportData(0, dataSize, dceData);
            } catch (final IOException e) {
                MipavUtil.displayError("IOException on dceImage.exportData(0, dataSize, decData)");
                setCompleted(false);
                return;
            }
            // dceImage.calcMinMax();
            // new ViewJFrameImage(dceImage);
            dceImage.disposeLocal();
            dceImage = null;

            r1ptj = new double[tDim];
            r1pt0 = dceData[190 * 60];
            for (t = 0; t < tDim; t++) {
                r1ptj[t] = dceData[t * sliceSize + 190 * 60] - r1pt0;
            }

            trapezoidMidpoint = new double[tDim - 1];
            for (t = 0; t < tDim - 1; t++) {
                trapezoidMidpoint[t] = 0.5 * (r1ptj[t] + r1ptj[t + 1]);
            }
            trapezoidSlope = new double[tDim - 1];
            trapezoidConstant = new double[tDim - 1];
            for (t = 0; t < tDim - 1; t++) {
                delT = timeVals[t + 1] - timeVals[t];
                trapezoidSlope[t] = (r1ptj[t + 1] - r1ptj[t]) / delT;
                trapezoidConstant[t] = (r1ptj[t] * timeVals[t + 1] - r1ptj[t + 1] * timeVals[t]) / delT;
            }

            ymodel = new double[tDim - 1];
            exparray = new double[tDim][tDim];

            // ktrans takes 6 values along the x axis (0, 0.01, 0.02, 0.05, 0.1, 0.2)
            // Along the y axis ve varies first. Then vp varies.
            // ve takes 3 values (0.1, 0.2, 0.5)
            // vp takes 6 values (0.01, 0.05, 0.01, 0.02, 0.05, 0.1)
            y_array = new double[tDim - 1];
            for (yIndex = 0; yIndex < 18; yIndex++) {
                veIndex = yIndex % 3;
                veActual = veArray[veIndex];
                // initial[1] = veActual;
                vpIndex = yIndex / 3;
                vpActual = vpArray[vpIndex];
                // initial[2] = vpActual;
                y = 5 + 10 * yIndex;
                for (ktransIndex = 0; ktransIndex < 6; ktransIndex++) {
                    ktransActual = ktransArray[ktransIndex];
                    // initial[0] = ktransActual;
                    x = 5 + 10 * ktransIndex;
                    for (t = 1; t < tDim; t++) {
                        y_array[t - 1] = dceData[t * sliceSize + y * xDim + x] - dceData[y * xDim + x];
                    }
                    // Note that the nPts, tDim-1, is the number of points in the y_array.
                    dModel = new FitSM2ConstrainedModel(tDim - 1, y_array, initial);
                    dModel.driver();
                    // dModel.dumpResults();
                    params = dModel.getParameters();
                    Preferences.debug("Actual ktrans = " + ktransActual + " Calculated ktrans = " + params[0] + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Actual ve = " + veActual + " Calculated ve = " + params[1] + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Actual vp = " + vpActual + " Calculated vp = " + params[2] + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Number of iterations: " + String.valueOf(dModel.iters) + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Chi-squared: " + String.valueOf(dModel.getChiSquared()) + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    status = dModel.getExitStatus();
                    statusMessage(status);
                } // for (ktransIndex = 0; ktransIndex < 6; ktransIndex++)
            } // for (yIndex = 0; yIndex < 18; yIndex++)

            setCompleted(false);
            return;
        } // if (selfTest)

        if (selfTest2) {
            // If your initial guesses are 0.495, 0.495, 0.495, then the 90 out of 108
            // runs where ktrans does not equal zero are correct. The model cannot
            // presently converge to ktrans equals zero in 18 out of the 108 cases.
            // Constrained ELSUNC works in all 90 cases where ktrans is nonzero.
            // Unconstrained ELSUNC works in 0 cases where ktrans is nonzero.
            // Unconstrained NL2sol works in 81 of 90 cases where ktrans is nonzero.
            final double ktransArray[] = new double[] {0.0, 0.01 / 60.0, 0.02 / 60.0, 0.05 / 60.0, 0.1 / 60.0,
                    0.2 / 60.0};
            final double veArray[] = new double[] {0.1, 0.2, 0.5};
            final double vpArray[] = new double[] {0.001, 0.005, 0.01, 0.02, 0.05, 0.1};
            int ktransIndex;
            int yIndex;
            int veIndex;
            int vpIndex;
            double ktransActual;
            double veActual;
            double vpActual;
            int status;
            int m;
            int success = 0;
            int failure = 0;
            int notADescentDirection = 0;
            int ktransequalszero = 0;
            tDim = 100;
            timeVals = new double[tDim];
            double x[] = null;
            int iv[] = null;
            int vLength;
            double v[] = null;
            final boolean useAnalyticJacobian = true;
            int singularConvergence = 0;
            int falseConvergence = 0;
            int functionEvaluationLimit = 0;
            int iterationLimit = 0;
            // Below variables used in DQED
            /*
             * int mcon = 0; int npmax = 5; int nvars = 3; int nall = mcon + 2*nvars + npmax + 1; int mequa = tDim - 1;
             * int nplus; if (mcon == 0) { nplus = 0; } else { nplus = 3*nall+2; } int liwork = 3*mcon + 9*nvars +
             * 4*npmax + nall + 11; int lwork = nall*nall + 4*nall + nvars*npmax + 33*nvars + mequa*npmax +
             * Math.max(mequa, nvars)*npmax + 13*npmax + 9*mcon + 26 + nplus; int ldfj = mcon + mequa; double bl[] =
             * null; double bu[] = null; double fj[][] = null; double fnorm[] = null; int igo[] = null; int ind[] =
             * null; int iopt[] = null; int iwork[] = null; int niters; double ropt[] = null; double work[] = null;
             * final int dqedReverseCase = 105;
             */
            if (fittingEngine == nl2solEngine) {
                x = new double[4];
                // 61 + number of coefficients
                iv = new int[64];
                vLength = 94 + (tDim - 1) * 3 + 3 * (tDim - 1) + 3 * (3 * 3 + 33) / 2;
                v = new double[vLength];
            }
            /*
             * if (fittingEngine == dqedEngine) { bl = new double[nvars+mcon+1]; bu = new double[nvars+mcon+1]; fj = new
             * double[ldfj+1][nvars+2]; fnorm = new double[1]; igo = new int[1]; ind = new int[nvars+mcon+1]; iopt = new
             * int[25]; iwork = new int[liwork+1]; ropt = new double[2]; work = new double[lwork+1]; ind[1] = 3; // 3
             * means upper and lower bounds bl[1] = min_constr[0]; bu[1] = max_constr[0]; ind[2] = 3; bl[2] =
             * min_constr[1]; bu[2] = max_constr[1]; ind[3] = 3; bl[3] = min_constr[2]; bu[3] = max_constr[2]; x = new
             * double[4]; }
             */
            for (i = 0; i < 100; i++) {
                timeVals[i] = i;
            }
            r1ptj = new double[tDim];
            for (i = 0; i < 100; i++) {
                r1ptj[i] = 2.0 * i;
            }

            trapezoidMidpoint = new double[tDim - 1];
            for (t = 0; t < tDim - 1; t++) {
                trapezoidMidpoint[t] = 0.5 * (r1ptj[t] + r1ptj[t + 1]);
            }
            trapezoidSlope = new double[tDim - 1];
            trapezoidConstant = new double[tDim - 1];
            for (t = 0; t < tDim - 1; t++) {
                delT = timeVals[t + 1] - timeVals[t];
                trapezoidSlope[t] = (r1ptj[t + 1] - r1ptj[t]) / delT;
                trapezoidConstant[t] = (r1ptj[t] * timeVals[t + 1] - r1ptj[t + 1] * timeVals[t]) / delT;
            }

            ymodel = new double[tDim - 1];
            exparray = new double[tDim][tDim];

            // ktrans takes 6 values along the x axis (0, 0.01/60.0, 0.02/60.0, 0.05/60.0, 0.1/60.0, 0.2/60.0)
            // Along the y axis ve varies first. Then vp varies.
            // ve takes 3 values (0.1, 0.2, 0.5)
            // vp takes 6 values (0.01, 0.05, 0.01, 0.02, 0.05, 0.1)
            y_array = new double[tDim - 1];
            for (yIndex = 0; yIndex < 18; yIndex++) {
                veIndex = yIndex % 3;
                veActual = veArray[veIndex];
                // initial[1] = veActual;
                if (fittingEngine == elsuncEngine) {
                    initial[1] = 0.495;
                }
                vpIndex = yIndex / 3;
                vpActual = vpArray[vpIndex];
                // initial[2] = vpActual;
                if (fittingEngine == elsuncEngine) {
                    initial[2] = 0.495;
                }
                for (ktransIndex = 0; ktransIndex < 6; ktransIndex++) {
                    ktransActual = ktransArray[ktransIndex];
                    Preferences.debug("ktrans = " + ktransActual + " ve = " + veActual + " vp = " + vpActual + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    ktransDivVe = ktransActual / veActual;
                    for (j = 0; j <= tDim - 1; j++) {
                        for (m = 0; m <= tDim - 1; m++) {
                            exparray[j][m] = Math.exp( (timeVals[j] - timeVals[m]) * ktransDivVe);
                        }
                    }
                    // initial[0] = ktransActual;
                    if (fittingEngine == elsuncEngine) {
                        initial[0] = 0.495;
                    } else if (fittingEngine == nl2solEngine) {
                        x[1] = 0.495;
                        x[2] = 0.495;
                        x[3] = 0.495;
                    }
                    for (t = 1; t < tDim; t++) {
                        IntModel imod;
                        final int steps;
                        double numInt;
                        final double eps = 1.0e-8;
                        final double intSum;
                        imod = new IntModel(timeVals[t], ktransActual, veActual);
                        final RungeKuttaFehlbergIntegrator kIntegrator = new RungeKuttaFehlbergIntegrator(imod);
                        kIntegrator.setEps(eps);
                        numInt = kIntegrator.integrate(0.0, timeVals[t]);

                        y_array[t - 1] = (ktransActual * numInt + vpActual * r1ptj[t]) / (1.0 - h);

                        /*
                         * intSum = 0.0; for (j = 1; j <= t; j++) { //intSum += trapezoidMidpoint[j-1]*(exparray[j][t] -
                         * exparray[j-1][t])*veActual; intSum += trapezoidConstant[j-1]*(exparray[j][t] -
                         * exparray[j-1][t])*veActual; intSum += trapezoidSlope[j-1]* ((exparray[j][t]*(timeVals[j] -
                         * 1.0/ktransDivVe)) - (exparray[j-1][t]*(timeVals[j-1] - 1.0/ktransDivVe)))*veActual; } // for
                         * (j = 2; j <= m; j++) ymodel[t-1] = intSum + vpActual * r1ptj[t]; Preferences.debug("t = " + t +
                         * "y_array = " + y_array[t-1] + " ymodel = " + ymodel[t-1] + "\n", Preferences.DEBUG_ALGORITHM);
                         */
                    } // for (t = 1; t < tDim; t++)
                    // Note that the nPts, tDim-1, is the number of points in the y_array.
                    if (fittingEngine == elsuncEngine) {
                        dModel = new FitSM2ConstrainedModel(tDim - 1, y_array, initial);
                        dModel.driver();
                        // dModel.dumpResults();
                        params = dModel.getParameters();
                        Preferences.debug("Actual ktrans = " + ktransActual + " Calculated ktrans = " + params[0]
                                + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("Actual ve = " + veActual + " Calculated ve = " + params[1] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("Actual vp = " + vpActual + " Calculated vp = " + params[2] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("Number of iterations: " + String.valueOf(dModel.iters) + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("Chi-squared: " + String.valueOf(dModel.getChiSquared()) + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                        status = dModel.getExitStatus();
                        statusMessage(status);
                        if ( (Math.abs(ktransActual - params[0]) <= 1.0E-7)
                                && (Math.abs(veActual - params[1]) <= 1.0E-5)
                                && (Math.abs(vpActual - params[2]) <= 1.0E-5)) {
                            success++;
                        } else {
                            failure++;
                            if (status == -6) {
                                notADescentDirection++;
                            } else if (ktransActual == 0.0) {
                                ktransequalszero++;
                            }
                        }
                    } // if (fittingEngine == elsuncEngine)
                    else if (fittingEngine == nl2solEngine) {
                        nl2Model = new FitSM2nl2solModel(tDim - 1, y_array, x, iv, v, useAnalyticJacobian);
                        nl2Model.driver();
                        Preferences.debug("Actual ktrans = " + ktransActual + " Calculated ktrans = " + x[1] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("Actual ve = " + veActual + " Calculated ve = " + x[2] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("Actual vp = " + vpActual + " Calculated vp = " + x[3] + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("Number of iterations: " + String.valueOf(iv[31]) + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("Chi-squared: " + String.valueOf(2.0 * v[10]) + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                        status = iv[1];
                        statusMessageNL2sol(status, 3);
                        if ( (Math.abs(ktransActual - x[1]) <= 1.0E-7) && (Math.abs(veActual - x[2]) <= 1.0E-5)
                                && (Math.abs(vpActual - x[3]) <= 1.0E-5)) {
                            success++;
                        } else {
                            failure++;
                            if (ktransActual == 0.0) {
                                ktransequalszero++;
                            }
                            if (status == 7) {
                                singularConvergence++;
                            } else if (status == 8) {
                                falseConvergence++;
                            } else if (status == 9) {
                                functionEvaluationLimit++;
                            } else if (status == 10) {
                                iterationLimit++;
                            }
                        }
                    } // else if (fittingEngine == nl2solEngine)
                    /*
                     * else if (fittingEngine == dqedEngine) { // Set initial values of variables x[1] = 0.495; x[2] =
                     * 0.495; x[3] = 0.495; // Tell how much storage we gave the solver iwork[1] = lwork; iwork[2] =
                     * liwork; // Initialize the call counter niters = 0; // User reverse communication to evaluate the
                     * derivatives. iopt[1] = 16; iopt[2] = 1; // No more options iopt[3] = 99; boolean dqedSelfTest =
                     * false; dq = new DQED (dqedSelfTest); do {
                     * 
                     * dq.dqed( dqedReverseCase, mequa, nvars, mcon, ind, bl, bu, x, fj, ldfj, fnorm, igo, iopt, ropt,
                     * iwork, work ); if ( 1 < igo[0] ) { break; } // // Count function evaluations. // niters = niters +
                     * 1;
                     * 
                     * double ktrans; double ve; double vp; double intSum;
                     * 
                     * ktrans = x[1]; ve = x[2]; vp = x[3]; ktransDivVe = ktrans / ve; for (j = 0; j <= tDim - 1; j++) {
                     * for (m = 0; m <= tDim - 1; m++) { exparray[j][m] = Math.exp( (timeVals[j] - timeVals[m]) *
                     * ktransDivVe); } }
                     * 
                     * for (m = 2; m <= tDim; m++) { intSum = 0.0; for (j = 2; j <= m; j++) { intSum +=
                     * trapezoidConstant[j - 2] * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]) * ve; intSum +=
                     * trapezoidSlope[j - 2] ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - 1.0 / ktransDivVe)) -
                     * (exparray[j - 2][m - 1] * (timeVals[j - 2] - 1.0 / ktransDivVe))) ve; } // for (j = 2; j <= m;
                     * j++) ymodel[m - 2] = (intSum + vp * r1ptj[m - 1]) / (1.0 - h); } // for (m = 2; m <= tDim; m++) //
                     * evaluate the residuals[j] = ymodel[j] - ySeries[j] for (j = 1; j <= mequa; j++) { fj[mcon+j][4] =
                     * ymodel[j-1] - y_array[j-1]; }
                     * 
                     * if (igo[0] != 0) { double intSumDerivKtrans; double intSumDerivVe; // Calculate the Jacobian
                     * analytically for (m = 2; m <= tDim; m++) { intSumDerivKtrans = 0.0; intSumDerivVe = 0.0; for (j =
                     * 2; j <= m; j++) { intSumDerivKtrans += trapezoidConstant[j - 2] ( (timeVals[j - 1] - timeVals[m -
                     * 1]) * exparray[j - 1][m - 1] - (timeVals[j - 2] - timeVals[m - 1]) exparray[j - 2][m - 1]);
                     * intSumDerivKtrans += trapezoidSlope[j - 2] ( (exparray[j - 1][m - 1] * (timeVals[j - 1] -
                     * timeVals[m - 1]) * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1] (timeVals[j -
                     * 2] - timeVals[m - 1]) * (timeVals[j - 2] - 1.0 / ktransDivVe))); intSumDerivKtrans +=
                     * trapezoidSlope[j - 2] (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]) * ve * ve / (ktrans *
                     * ktrans); intSumDerivVe += trapezoidConstant[j - 2] ( (timeVals[j - 1] - timeVals[m - 1]) *
                     * exparray[j - 1][m - 1] - (timeVals[j - 2] - timeVals[m - 1]) exparray[j - 2][m - 1]) * ( -ktrans /
                     * ve); intSumDerivVe += trapezoidConstant[j - 2] (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]);
                     * intSumDerivVe += trapezoidSlope[j - 2] ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - timeVals[m -
                     * 1]) * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1] (timeVals[j - 2] -
                     * timeVals[m - 1]) * (timeVals[j - 2] - 1.0 / ktransDivVe))) ( -ktrans / ve); intSumDerivVe +=
                     * trapezoidSlope[j - 2] * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]) ( -2.0 * ve / ktrans);
                     * intSumDerivVe += trapezoidSlope[j - 2] ( (exparray[j - 1][m - 1] * timeVals[j - 1]) - (exparray[j -
                     * 2][m - 1] * timeVals[j - 2])); } // for (j = 2; j <= m; j++) fj[mcon + m - 1][1] =
                     * intSumDerivKtrans / (1.0 - h); fj[mcon + m - 1][2] = intSumDerivVe / (1.0 - h); fj[mcon + m -
                     * 1][3] = r1ptj[m - 1] / (1.0 - h); } } // if (igo[0] != 0) } while(true);
                     * Preferences.debug("Actual ktrans = " + ktransActual + " Calculated ktrans = " + x[1] + "\n", 
                     * Preferences.DEBUG_ALGORITHM);
                     * Preferences.debug("Actual ve = " + veActual + " Calculated ve = " + x[2] + "\n", 
                     * Preferences.DEBUG_ALGORITHM);
                     * Preferences.debug("Actual vp = " + vpActual + " Calculated vp = " + x[3] + "\n", 
                     * Preferences.DEBUG_ALGORITHM);
                     * Preferences.debug("Number of iterations: " + String.valueOf(niters) + "\n", 
                     * Preferences.DEBUG_ALGORITHM);
                     * Preferences.debug("Residual after the fit fnorm[0] = " + fnorm[0] + "\n", 
                     * Preferences.DEBUG_ALGORITHM);
                     * Preferences.debug("DQED output flag igo[0] = " + igo[0] + "\n", 
                     * Preferences.DEBUG_ALGORITHM); if ( (Math.abs(ktransActual -
                     * x[1]) <= 1.0E-7) && (Math.abs(veActual - x[2]) <= 1.0E-5) && (Math.abs(vpActual - x[3]) <=
                     * 1.0E-5)) { success++; } else { failure++; if (ktransActual == 0.0) { ktransequalszero++; } } } //
                     * else if (fittingEngine == dqedEngine)
                     */
                } // for (ktransIndex = 0; ktransIndex < 6; ktransIndex++)
            } // for (yIndex = 0; yIndex < 18; yIndex++)
            Preferences.debug("Number of successes = " + success + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of failures = " + failure + "\n", Preferences.DEBUG_ALGORITHM);
            if (fittingEngine == elsuncEngine) {
                Preferences
                        .debug("Number failing with abnormal termination because the latest search direction computed using subspace minimization\n", 
                        		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("was not a descent direction (probably caused by a wrongly computed Jacobian) = "
                        + notADescentDirection + "\n", Preferences.DEBUG_ALGORITHM);

            } else if (fittingEngine == nl2solEngine) {
                Preferences.debug("Number failing with singular convergence = " + singularConvergence + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Number failing with false convergence = " + falseConvergence + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Number failing with function evaluation limit = " + functionEvaluationLimit + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Number failing with iteration limit = " + iterationLimit + "\n", 
                		Preferences.DEBUG_ALGORITHM);
            }
            Preferences.debug("Number failing because cannot handle ktrans equals zero = " + ktransequalszero + "\n", 
            		Preferences.DEBUG_ALGORITHM);
            setCompleted(false);
            return;
        } // if (selfTest2)

        if (nlConstrainedEngineEPTest) {
            new FitAllEP();
            setCompleted(false);
            return;
        }

        if (nlConstrainedEngineTest) {
            new FitAll();
            setCompleted(false);
            return;
        }

        if (nl2solTest) {
            new FitAllNL2();
            setCompleted(false);
            return;
        }

        /*
         * if (dqedTest) { boolean dqedSelfTest = true; new DQED(dqedSelfTest); setCompleted(false); return; }
         */

        if (Integration2Test) {
            new Integration2All();
            setCompleted(false);
            return;
        }

        if (Integration2EPTest) {
            new Integration2EPAll();
            setCompleted(false);
            return;
        }

        if (srcImage.getNDims() != 4) {
            MipavUtil.displayError("srcImage must be 4D");
            setCompleted(false);
            return;
        }

        processors = Runtime.getRuntime().availableProcessors();
        Preferences.debug("Available processors = " + processors + "\n", Preferences.DEBUG_ALGORITHM);

        // Convert dialog ktrans units from /min to /sec
        // Multiply /min by 1 min/60 seconds
        initial0_original = initial[0];
        min0_original = min_constr[0];
        max0_original = max_constr[0];
        initial[0] = initial[0] / 60.0;
        min_constr[0] = min_constr[0] / 60.0;
        max_constr[0] = max_constr[0] / 60.0;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        tDim = srcImage.getExtents()[3];
        volSize = xDim * yDim * zDim;
        size4D = volSize * tDim;
        destArray = new float[4][volSize];
        destExitStatusArray = new int[volSize];
        for (i = 0; i < 4; i++) {
            for (j = 0; j < volSize; j++) {
                destArray[i][j] = Float.NaN;
            }
        }

        r1t0 = new double[volSize];
        r1tj = new double[size4D];
        r1ptj = new double[tDim];

        try {
            tissueImage.exportData(0, volSize, r1t0);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on tissueImage.exportData(0, volSize, r1t0)");
            setCompleted(false);
            return;
        }
        tissueImage.disposeLocal();
        tissueImage = null;

        VOIs = srcImage.getVOIs();

        if (sagittalSinusIndex < 0) {
            MipavUtil.displayError("No bounding vois around sagittal sinus");
            setCompleted(false);
            return;
        }

        if ( (VOIs.VOIAt(sagittalSinusIndex).getCurveType() != VOI.CONTOUR)
                && (VOIs.VOIAt(sagittalSinusIndex).getCurveType() != VOI.POLYLINE)) {
            MipavUtil.displayError("No bounding vois around sagittal sinus");
            setCompleted(false);
            return;
        }

        mask = new short[volSize];
        for (i = 0; i < volSize; i++) {
            mask[i] = -1;
        }
        mask = srcImage.generateVOIMask(mask, sagittalSinusIndex);

        try {
            srcImage.exportData(0, size4D, r1tj);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on srcImage.exportData(0, size4D, r1tj");
            setCompleted(false);
            return;
        }

        voiCount = 0;
        r1pt0 = 0.0;
        // Actually looking from t = t1 to t = tlast
        for (t = 0; t < tDim; t++) {
            for (i = 0; i < volSize; i++) {
                if (mask[i] != -1) {
                    r1ptj[t] += r1tj[i + t * volSize];
                    if (t == 0) {
                        r1pt0 += r1t0[i];
                        voiCount++;
                    }
                }
            }
            if (t == 0) {
                r1pt0 = r1pt0 / voiCount;
            }
            r1ptj[t] = r1ptj[t] / voiCount;
        } // for (t = 0; t < tDim; t++)

        // Actually looking from t = t1 to t = tlast
        for (t = 0; t < tDim; t++) {
            // For t = t1, looking at R1,p(t1) - R10,p, but R1,p(t1) and R10,p are identical except for noise
            r1ptj[t] = r1ptj[t] - r1pt0;
            for (i = 0; i < volSize; i++) {
                r1tj[i + t * volSize] = r1tj[i + t * volSize] - r1t0[i];
            }
        } // for (t = 0; t < tDim; t++)
        r1t0 = null;

        if (processRegionIndex >= 0) {
            if ( (VOIs.VOIAt(processRegionIndex).getCurveType() != VOI.CONTOUR)
                    && (VOIs.VOIAt(processRegionIndex).getCurveType() != VOI.POLYLINE)) {
                processRegionIndex = -1;
            }
        }

        bitMask = new BitSet(volSize);
        if (processRegionIndex < 0) {
            mask = null;
            for (i = 0; i < volSize; i++) {
                bitMask.set(i);
            }
            wholeImage = true;
        } else {
            for (i = 0; i < volSize; i++) {
                mask[i] = -1;
            }
            mask = srcImage.generateVOIMask(mask, processRegionIndex);
            for (i = 0; i < volSize; i++) {
                if (mask[i] != -1) {
                    bitMask.set(i);
                }
            }
            mask = null;
            wholeImage = false;
        }

        trapezoidMidpoint = new double[tDim - 1];
        for (t = 0; t < tDim - 1; t++) {
            trapezoidMidpoint[t] = 0.5 * (r1ptj[t] + r1ptj[t + 1]);
        }
        trapezoidSlope = new double[tDim - 1];
        trapezoidConstant = new double[tDim - 1];
        for (t = 0; t < tDim - 1; t++) {
            delT = timeVals[t + 1] - timeVals[t];
            trapezoidSlope[t] = (r1ptj[t + 1] - r1ptj[t]) / delT;
            trapezoidConstant[t] = (r1ptj[t] * timeVals[t + 1] - r1ptj[t + 1] * timeVals[t]) / delT;
        }

        y_array = new double[tDim - 1];
        ymodel = new double[tDim - 1];
        exparray = new double[tDim][tDim];
        exitStatus = new int[12356];

        for (i = 0; i < 3; i++) {
            paramMin[i] = Double.MAX_VALUE;
            paramMax[i] = -Double.MAX_VALUE;
        }

        if ((processors > 1) && (Preferences.isMultiThreadingEnabled())) {
            int start;
            int end;
            final ExecutorService application = Executors.newCachedThreadPool();
            for (i = 0; i < processors; i++) {
                start = (i * volSize) / processors;
                end = ( (i + 1) * volSize) / processors;
                application.execute(new sm2Task(start, end, tDim, r1ptj, initial, trapezoidConstant, trapezoidSlope,
                        timeVals));
            }
            application.shutdown();
            try {
                boolean tasksEnded = application.awaitTermination(24, TimeUnit.HOURS);
                if ( !tasksEnded) {
                    System.err.println("Timed out while waiting for tasks to finish");
                    Preferences.debug("Timed out while waiting for tasks to finish\n", Preferences.DEBUG_ALGORITHM);
                    setCompleted(false);
                    return;
                }

            } catch (final InterruptedException ex) {
                System.err.println("Interrupted while waiting for tasks to finish");
                Preferences.debug("Interrupted while waiting for tasks to finish\n", Preferences.DEBUG_ALGORITHM);
                setCompleted(false);
                return;
            }
        } else { // processors == 1 || !Preferences.isMultiThreadingEnabled()
            for (i = 0; i < volSize; i++) {
                voxelsProcessed++;
                final long vt100 = voxelsProcessed * 100L;
                barMarker = (int) (vt100 / volSize);
                if (barMarker > oldBarMarker) {
                    oldBarMarker = barMarker;
                    fireProgressStateChanged(barMarker);
                }
                if (bitMask.get(i)) {
                    for (t = 1; t < tDim; t++) {
                        y_array[t - 1] = r1tj[t * volSize + i];
                    }
                    // Note that the nPts, tDim-1, is the number of points in the y_array.
                    dModel = new FitSM2ConstrainedModel(tDim - 1, y_array, initial);
                    dModel.driver();
                    // dModel.dumpResults();
                    params = dModel.getParameters();
                    chi_squared = (float) dModel.getChiSquared();
                    // Convert ktrans from /sec to /min
                    // Mutliply /sec by 60 seconds/minute
                    paramSecs0 = params[0];
                    params[0] = 60.0 * params[0];
                    for (j = 0; j < 3; j++) {
                        destArray[j][i] = (float) params[j];
                        if (Double.isNaN(params[j])) {
                            paramNaN[j]++;
                        } else if (Double.isInfinite(params[j])) {
                            paramInf[j]++;
                        } else {
                            if (params[j] < paramMin[j]) {
                                paramMin[j] = params[j];
                            }
                            if (params[j] > paramMax[j]) {
                                paramMax[j] = params[j];
                            }
                        }
                    }
                    // Set values that come out at the extreme values of the allowed intervals to NaN rather
                    // than to the extreme values. Those values are invariabley wrong, and the images
                    // become very difficult to analyze.
                    if ( (paramSecs0 <= min_constr[0]) || (paramSecs0 >= max_constr[0]) || (params[1] <= min_constr[1])
                            || (params[1] >= max_constr[1]) || (params[2] <= min_constr[2])
                            || (params[2] >= max_constr[2])) {
                        destArray[0][i] = Float.NaN;
                        destArray[1][i] = Float.NaN;
                        destArray[2][i] = Float.NaN;
                        bitMask.clear(i);
                    } else if (dModel.getExitStatus() > 0) {
                        validVoxels++;
                        ktransTotal += destArray[0][i];
                        veTotal += destArray[1][i];
                        vpTotal += destArray[2][i];
                    } else if (dModel.getExitStatus() <= 0) {
                        bitMask.clear(i);
                    }
                    destArray[3][i] = chi_squared;
                    destExitStatusArray[i] = dModel.getExitStatus();
                    exitStatus[ (dModel.getExitStatus() + 11)]++;
                } // if (bitMask.get(i)
            } // for (i = 0; i < volSize; i++)
        } // else processors == 1

        if (paramNaN[0] > 0) {
            System.out.println(paramNaN[0] + " of ktrans values are NaN");
            Preferences.debug(paramNaN[0] + " of ktrans values are NaN\n", Preferences.DEBUG_ALGORITHM);
        }

        if (paramNaN[1] > 0) {
            System.out.println(paramNaN[1] + " of ve values are NaN");
            Preferences.debug(paramNaN[1] + " of ve values are NaN\n", Preferences.DEBUG_ALGORITHM);
        }

        if (paramNaN[2] > 0) {
            System.out.println(paramNaN[2] + " of vp values are NaN");
            Preferences.debug(paramNaN[2] + " of vp values are NaN\n", Preferences.DEBUG_ALGORITHM);
        }

        if (paramInf[0] > 0) {
            System.out.println(paramInf[0] + " of ktrans values are infinite");
            Preferences.debug(paramInf[0] + " of ktrans values are infinite\n", Preferences.DEBUG_ALGORITHM);
        }

        if (paramInf[1] > 0) {
            System.out.println(paramInf[1] + " of ve values are infinite");
            Preferences.debug(paramInf[1] + " of ve values are infinite\n", Preferences.DEBUG_ALGORITHM);
        }

        if (paramInf[2] > 0) {
            System.out.println(paramInf[2] + " of vp values are infinite");
            Preferences.debug(paramInf[2] + " of vp values are infinite\n", Preferences.DEBUG_ALGORITHM);
        }

        System.out.println("Valid voxels = " + validVoxels);
        Preferences.debug("Valid voxels = " + validVoxels + "\n", Preferences.DEBUG_ALGORITHM);
        ktransAverage = ktransTotal / validVoxels;
        veAverage = veTotal / validVoxels;
        vpAverage = vpTotal / validVoxels;

        System.out.println("ktrans minimum value = " + paramMin[0]);
        Preferences.debug("ktrans minimum value = " + paramMin[0] + "\n", Preferences.DEBUG_ALGORITHM);
        System.out.println("ktrans average value = " + ktransAverage);
        Preferences.debug("ktrans average value = " + ktransAverage + "\n", Preferences.DEBUG_ALGORITHM);
        System.out.println("ktrans maximum value = " + paramMax[0]);
        Preferences.debug("ktrans maximum value = " + paramMax[0] + "\n", Preferences.DEBUG_ALGORITHM);

        System.out.println("ve minimum value = " + paramMin[1]);
        Preferences.debug("ve minimum value = " + paramMin[1] + "\n", Preferences.DEBUG_ALGORITHM);
        System.out.println("ve average value = " + veAverage);
        Preferences.debug("ve average value = " + veAverage + "\n", Preferences.DEBUG_ALGORITHM);
        System.out.println("ve maximum value = " + paramMax[1]);
        Preferences.debug("ve maximum value = " + paramMax[1] + "\n", Preferences.DEBUG_ALGORITHM);

        System.out.println("vp minimum value = " + paramMin[2]);
        Preferences.debug("vp minimum value = " + paramMin[2] + "\n", Preferences.DEBUG_ALGORITHM);
        System.out.println("vp average value = " + vpAverage);
        Preferences.debug("vp average value = " + vpAverage + "\n", Preferences.DEBUG_ALGORITHM);
        System.out.println("vp maximum value = " + paramMax[2]);
        Preferences.debug("vp maximum value = " + paramMax[2] + "\n", Preferences.DEBUG_ALGORITHM);

        if (exitStatus[12351] > 0) {
            normalTerminations += exitStatus[12351];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12351] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12352] > 0) {
            normalTerminations += exitStatus[12352];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12352] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12353] > 0) {
            normalTerminations += exitStatus[12353];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12353] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12354] > 0) {
            normalTerminations += exitStatus[12354];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12354] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12355] > 0) {
            normalTerminations += exitStatus[12355];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12355] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12311] > 0) {
            normalTerminations += exitStatus[12311];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12311] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12312] > 0) {
            normalTerminations += exitStatus[12312];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12312] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12313] > 0) {
            normalTerminations += exitStatus[12313];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12313] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12314] > 0) {
            normalTerminations += exitStatus[12314];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12314] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12315] > 0) {
            normalTerminations += exitStatus[12315];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12315] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12051] > 0) {
            normalTerminations += exitStatus[12051];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12051] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12052] > 0) {
            normalTerminations += exitStatus[12052];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12052] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12053] > 0) {
            normalTerminations += exitStatus[12053];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12053] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12054] > 0) {
            normalTerminations += exitStatus[12054];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12054] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12055] > 0) {
            normalTerminations += exitStatus[12055];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12055] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12011] > 0) {
            normalTerminations += exitStatus[12011];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12011] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12012] > 0) {
            normalTerminations += exitStatus[12012];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12012] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12013] > 0) {
            normalTerminations += exitStatus[12013];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12013] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12014] > 0) {
            normalTerminations += exitStatus[12014];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12014] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[12015] > 0) {
            normalTerminations += exitStatus[12015];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[12015] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10351] > 0) {
            normalTerminations += exitStatus[10351];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10351] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10352] > 0) {
            normalTerminations += exitStatus[10352];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10352] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10353] > 0) {
            normalTerminations += exitStatus[10353];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10353] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10354] > 0) {
            normalTerminations += exitStatus[10354];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10354] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10355] > 0) {
            normalTerminations += exitStatus[10355];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10355] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10311] > 0) {
            normalTerminations += exitStatus[10311];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10311] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10312] > 0) {
            normalTerminations += exitStatus[10312];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10312] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10313] > 0) {
            normalTerminations += exitStatus[10313];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10313] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10314] > 0) {
            normalTerminations += exitStatus[10314];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10314] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10315] > 0) {
            normalTerminations += exitStatus[10315];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10315] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10051] > 0) {
            normalTerminations += exitStatus[10051];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10051] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10052] > 0) {
            normalTerminations += exitStatus[10052];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10052] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10053] > 0) {
            normalTerminations += exitStatus[10053];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10053] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10054] > 0) {
            normalTerminations += exitStatus[10054];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10054] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10055] > 0) {
            normalTerminations += exitStatus[10055];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10055] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10011] > 0) {
            normalTerminations += exitStatus[10011];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10011] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10012] > 0) {
            normalTerminations += exitStatus[10012];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10012] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10013] > 0) {
            normalTerminations += exitStatus[10013];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10013] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10014] > 0) {
            normalTerminations += exitStatus[10014];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10014] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10015] > 0) {
            normalTerminations += exitStatus[10015];
            Preferences
                    .debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[10015] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2351] > 0) {
            normalTerminations += exitStatus[2351];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2351] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2352] > 0) {
            normalTerminations += exitStatus[2352];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2352] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2353] > 0) {
            normalTerminations += exitStatus[2353];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2353] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2354] > 0) {
            normalTerminations += exitStatus[2354];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2354] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2355] > 0) {
            normalTerminations += exitStatus[2355];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2355] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2311] > 0) {
            normalTerminations += exitStatus[2311];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2311] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2312] > 0) {
            normalTerminations += exitStatus[2312];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2312] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2313] > 0) {
            normalTerminations += exitStatus[2313];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2313] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2314] > 0) {
            normalTerminations += exitStatus[2314];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2314] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2315] > 0) {
            normalTerminations += exitStatus[2315];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2315] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2051] > 0) {
            normalTerminations += exitStatus[2051];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2051] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2052] > 0) {
            normalTerminations += exitStatus[2052];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2052] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2053] > 0) {
            normalTerminations += exitStatus[2053];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2053] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2054] > 0) {
            normalTerminations += exitStatus[2054];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2054] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2055] > 0) {
            normalTerminations += exitStatus[2055];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2055] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2011] > 0) {
            normalTerminations += exitStatus[2011];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2011] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2012] > 0) {
            normalTerminations += exitStatus[2012];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2012] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2013] > 0) {
            normalTerminations += exitStatus[2013];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2013] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2014] > 0) {
            normalTerminations += exitStatus[2014];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2014] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[2015] > 0) {
            normalTerminations += exitStatus[2015];
            Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[2015] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[351] > 0) {
            normalTerminations += exitStatus[351];
            Preferences.debug("Normal terminations because the relative change in x is less than epsx and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[351] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[352] > 0) {
            normalTerminations += exitStatus[352];
            Preferences.debug("Normal terminations because the relative change in x is less than epsx and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[352] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[353] > 0) {
            normalTerminations += exitStatus[353];
            Preferences.debug("Normal terminations because the relative change in x is less than epsx and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[353] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[354] > 0) {
            normalTerminations += exitStatus[354];
            Preferences.debug("Normal terminations because the relative change in x is less than epsx and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[354] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[355] > 0) {
            normalTerminations += exitStatus[355];
            Preferences.debug("Normal terminations because the relative change in x is less than epsx and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[355] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[311] > 0) {
            normalTerminations += exitStatus[311];
            Preferences.debug("Normal terminations because the relative change in x is less than epsx\n",
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[311] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[312] > 0) {
            normalTerminations += exitStatus[312];
            Preferences.debug("Normal terminations because the relative change in x is less than epsx\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[312] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[313] > 0) {
            normalTerminations += exitStatus[313];
            Preferences.debug("Normal terminations because the relative change in x is less than epsx\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[313] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[314] > 0) {
            normalTerminations += exitStatus[314];
            Preferences.debug("Normal terminations because the relative change in x is less than epsx\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[314] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[315] > 0) {
            normalTerminations += exitStatus[315];
            Preferences.debug("Normal terminations because the relative change in x is less than epsx\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[315] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[51] > 0) {
            normalTerminations += exitStatus[51];
            Preferences.debug("Normal terminations because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[51] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[52] > 0) {
            normalTerminations += exitStatus[52];
            Preferences.debug("Normal terminations because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[52] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[53] > 0) {
            normalTerminations += exitStatus[53];
            Preferences.debug("Normal terminations because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[53] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[54] > 0) {
            normalTerminations += exitStatus[54];
            Preferences.debug("Normal terminations because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[54] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[55] > 0) {
            normalTerminations += exitStatus[55];
            Preferences.debug("Normal terminations because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number = " + exitStatus[55] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[11] > 0) {
            abnormalTerminations += exitStatus[11];
            Preferences.debug("Abnormal terminations with no termination bits set = " + exitStatus[11] + "\n", 
            		Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[10] > 0) {
            abnormalTerminations += exitStatus[10];
            Preferences
                    .debug("Abnormal terminations because m < n or n <= 0 or m <= 0 or mdc < m or mdw < n*n + 5*n + 3*m + 6 or\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences
                    .debug("maxit <= 0 or epsrel < 0 or epsabs < 0 or epsx < 0 or invalid starting point on entry = "
                            + exitStatus[10] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[9] > 0) {
            abnormalTerminations += exitStatus[9];
            Preferences
                    .debug("Abnormal terminations because the number of iterations has exceeded the maximum allowed iterations = "
                            + exitStatus[9] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[8] > 0) {
            abnormalTerminations += exitStatus[8];
            Preferences
                    .debug("Abnormal terminations because the Hessian emanating from the 2nd order method is not positive definite = "
                            + exitStatus[8] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[7] > 0) {
            abnormalTerminations += exitStatus[7];
            Preferences
                    .debug("Abnormal terminations because the algorithm would like to use 2nd derivatives but is not allowed to do that = "
                            + exitStatus[7] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[6] > 0) {
            abnormalTerminations += exitStatus[6];
            Preferences.debug("Abnormal terminations because an undamped step with Newtons method is a failure = "
                    + exitStatus[6] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[5] > 0) {
            abnormalTerminations += exitStatus[5];
            Preferences
                    .debug("Abnormal terminations because the latest search direction computed using subspace minimization\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("was not a descent direction (probably caused by a wrongly computed Jacobian) = "
                    + exitStatus[5] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[4] > 0) {
            abnormalTerminations += exitStatus[4];
            Preferences.debug("Abnormal terminations because there is only one feasible point,\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("namely X(I) = BL(I) = BU(I), I = 1,2,...,N = " + exitStatus[4] + "\n", 
            		Preferences.DEBUG_ALGORITHM);
        }

        if (exitStatus[3] > 0) {
            abnormalTerminations += exitStatus[3];
            Preferences.debug("Abnormal terminations because of NLConstrainedEngine driver error = " + exitStatus[3]
                    + "\n", Preferences.DEBUG_ALGORITHM);
        }

        System.out.println("\nTotal normal terminations = " + normalTerminations);
        Preferences.debug("\nTotal normal terminations = " + normalTerminations + "\n", Preferences.DEBUG_ALGORITHM);
        System.out.println("Total abnormal terminations = " + abnormalTerminations);
        Preferences.debug("Total abnormal terminations = " + abnormalTerminations + "\n", Preferences.DEBUG_ALGORITHM);

        for (i = 0; i < 4; i++) {
            try {
                destImage[i].importData(0, destArray[i], true);
            } catch (final IOException e) {
                MipavUtil.displayError("IOException on destImage[" + i + "].importData(0, destArray[" + i + "], true)");
                setCompleted(false);
                return;
            }
        }

        try {
            destImage[4].importData(0, destExitStatusArray, true);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on destImage[4].importData(0, destExitStatusArray, true)");
            setCompleted(false);
            return;
        }

        if ( !wholeImage) {
            for (t = 1; t < tDim; t++) {
                y_array[t - 1] = 0.0;
                for (i = 0; i < volSize; i++) {
                    if (bitMask.get(i)) {
                        y_array[t - 1] += r1tj[t * volSize + i];
                    }
                } // for (i = 0; i < volSize; i++)
                y_array[t - 1] = y_array[t - 1] / validVoxels;
            } // for (t = 1; t < tDim; t++)
            // Note that the nPts, tDim-1, is the number of points in the y_array.
            dModel = new FitSM2ConstrainedModel(tDim - 1, y_array, initial);
            dModel.driver();
            // dModel.dumpResults();
            params = dModel.getParameters();
            chi_squared = (float) dModel.getChiSquared();
            // Convert ktrans from /sec to /min
            // Mutliply /sec by 60 seconds/minute
            params[0] = 60.0 * params[0];
            System.out.println("ktrans spatial average = " + params[0]);
            Preferences.debug("ktrans spatial average = " + params[0] + "\n", Preferences.DEBUG_ALGORITHM);
            System.out.println("ve spatial average = " + params[1]);
            Preferences.debug("ve spatial average = " + params[1] + "\n", Preferences.DEBUG_ALGORITHM);
            System.out.println("vp spatial average = " + params[2]);
            Preferences.debug("vp spatial average = " + params[2] + "\n", Preferences.DEBUG_ALGORITHM);
            System.out.println("chi-squared spatial average = " + chi_squared);
            Preferences.debug("chi-squared spatial average = " + chi_squared + "\n", Preferences.DEBUG_ALGORITHM);
            if (dModel.getExitStatus() > 0) {
                System.out.println("Normal termination on spatial average");
                Preferences.debug("Normal termination on spatial average\n", Preferences.DEBUG_ALGORITHM);
            } else {
                System.out.println("Abnormal termination on spatial average");
                Preferences.debug("Abnormal termination on spatial average\n", Preferences.DEBUG_ALGORITHM);
                if (dModel.getExitStatus() == -1) {
                    System.out
                            .println("Abnormal termination because m < n or n <= 0 or m <= 0 or mdc < m or mdw < n*n + 5*n + 3*m + 6 or");
                    System.out
                            .println("maxit <= 0 or epsrel < 0 or epsabs < 0 or epsx < 0 or invalid starting point on entry");
                    Preferences
                            .debug("Abnormal termination because m < n or n <= 0 or m <= 0 or mdc < m or mdw < n*n + 5*n + 3*m + 6 or\n", 
                            		Preferences.DEBUG_ALGORITHM);
                    Preferences
                            .debug("maxit <= 0 or epsrel < 0 or epsabs < 0 or epsx < 0 or invalid starting point on entry\n", 
                            		Preferences.DEBUG_ALGORITHM);
                } else if (dModel.getExitStatus() == -2) {
                    System.out
                            .println("Abnormal termination because the number of iterations has exceeded the maximum allowed iterations");
                    Preferences
                            .debug("Abnormal termination because the number of iterations has exceeded the maximum allowed iterations\n", 
                            		Preferences.DEBUG_ALGORITHM);
                } else if (dModel.getExitStatus() == -3) {
                    System.out
                            .println("Abnormal termination because the Hessian emanating from the 2nd order method is not positive definite");
                    Preferences
                            .debug("Abnormal termination because the Hessian emanating from the 2nd order method is not positive definite\n",
                            		Preferences.DEBUG_ALGORITHM);
                } else if (dModel.getExitStatus() == -4) {
                    System.out
                            .println("Abnormal termination because the algorithm would like to use 2nd derivatives but is not allowed to do that");
                    Preferences
                            .debug("Abnormal termination because the algorithm would like to use 2nd derivatives but is not allowed to do that\n", 
                            		Preferences.DEBUG_ALGORITHM);
                } else if (dModel.getExitStatus() == -5) {
                    System.out
                            .println("Abnormal termination because an undamped step with Newtons method is a failure");
                    Preferences
                            .debug("Abnormal termination because an undamped step with Newtons method is a failure\n", 
                            		Preferences.DEBUG_ALGORITHM);
                } else if (dModel.getExitStatus() == -6) {
                    System.out
                            .println("Abnormal termination because the latest search direction computed using subspace minimization");
                    System.out.println("was not a descent direction (probably caused by a wrongly computed Jacobian)");
                    Preferences
                            .debug("Abnormal termination because the latest search direction computed using subspace minimization\n", 
                            		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("was not a descent direction (probably caused by a wrongly computed Jacobian)\n", 
                    		Preferences.DEBUG_ALGORITHM);
                } else if (dModel.getExitStatus() == -7) {
                    System.out.println("Abnormal termination because there is only one feasible point,");
                    System.out.println("namely X(I) = BL(I) = BU(I), I = 1,2,...,N");
                    Preferences.debug("Abnormal termination because there is only one feasible point,\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("namely X(I) = BL(I) = BU(I), I = 1,2,...,N\n", Preferences.DEBUG_ALGORITHM);
                } else if (dModel.getExitStatus() == -8) {
                    System.out.println("Abnormal termination due to NLConstrainedEngine driver error");
                    Preferences.debug("Abnormal termination due to NLConstrainedEngine driver error\n", 
                    		Preferences.DEBUG_ALGORITHM);
                }
            }
        } // if (!wholeImage)

        // Restore original ktrans values in /min for scripting in JDialogSM2
        initial[0] = initial0_original;
        min_constr[0] = min0_original;
        max_constr[0] = max0_original;
        setCompleted(true);
    }

    private void statusMessage(final int status) {
        if (status == 12340) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 12341) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 12342) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12343) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12344) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12300) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 12301) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 12302) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12303) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12304) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12040) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 12041) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 12042) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12043) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12044) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12000) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 12001) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 12002) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12003) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 12004) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10340) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 10341) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 10342) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10343) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10344) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10300) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 10301) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 10302) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10303) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10304) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10040) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 10041) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 10042) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10043) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10044) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10000) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 10001) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 10002) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10003) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 10004) {
            Preferences
                    .debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2340) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 2341) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2  and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 2342) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2343) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2344) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2300) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 2301) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 2302) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2303) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2304) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2040) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 2041) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2  and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 2042) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2043) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2044) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2000) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 2001) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 2002) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2003) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 2004) {
            Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 340) {
            Preferences.debug("Normal termination because the relative change in x is less than epsx and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 341) {
            Preferences.debug("Normal termination because the relative change in x is less than epsx  and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 342) {
            Preferences.debug("Normal termination because the relative change in x is less than epsx and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 343) {
            Preferences.debug("Normal termination because the relative change in x is less than epsx and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 344) {
            Preferences.debug("Normal termination because the relative change in x is less than epsx and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 300) {
            Preferences.debug("Normal termination because the relative change in x is less than epsx\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 301) {
            Preferences.debug("Normal termination because the relative change in x is less than epsx\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 302) {
            Preferences.debug("Normal termination because the relative change in x is less than epsx\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 303) {
            Preferences.debug("Normal termination because the relative change in x is less than epsx\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 304) {
            Preferences.debug("Normal termination because the relative change in x is less than epsx\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 40) {
            Preferences.debug("Normal termination because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 41) {
            Preferences.debug("Normal termination because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 42) {
            Preferences.debug("Normal termination because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 43) {
            Preferences.debug("Normal termination because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 44) {
            Preferences.debug("Normal termination because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == -1) {
            Preferences
                    .debug("Abnormal termination because m < n or n <= 0 or m <= 0 or mdc < m or mdw < n*n + 5*n + 3*m + 6 or\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences
                    .debug("maxit <= 0 or epsrel < 0 or epsabs < 0 or epsx < 0 or invalid starting point on entry\n", 
                    		Preferences.DEBUG_ALGORITHM);
        } else if (status == -2) {
            Preferences
                    .debug("Abnormal termination because the number of iterations has exceeded the maximum allowed iterations\n", 
                    		Preferences.DEBUG_ALGORITHM);
        } else if (status == -3) {
            Preferences
                    .debug("Abnormal termination because the Hessian emanating from the 2nd order method is not positive definite\n", 
                    		Preferences.DEBUG_ALGORITHM);
        } else if (status == -4) {
            Preferences
                    .debug("Abnormal termination because the algorithm would like to use 2nd derivatives but is not allowed to do that\n", 
                    		Preferences.DEBUG_ALGORITHM);
        } else if (status == -5) {
            Preferences.debug("Abnormal termination because an undamped step with Newtons method is a failure\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == -6) {
            Preferences
                    .debug("Abnormal termination because the latest search direction computed using subspace minimization\n", 
                    		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("was not a descent direction (probably caused by a wrongly computed Jacobian)\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == -7) {
            Preferences.debug("Abnormal termination because there is only one feasible point,\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("namely X(I) = BL(I) = BU(I), I = 1,2,...,N\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == -8) {
            Preferences.debug("Abnormal termination due to NLConstrainedEngine driver error\n", Preferences.DEBUG_ALGORITHM);
        } else {
            Preferences.debug("Exit status = " + status + "\n", Preferences.DEBUG_ALGORITHM);
        }
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    }

    private void statusMessageNL2sol(final int status, final int numParam) {
        if (status == 3) {
            Preferences.debug("X-convergence.  The scaled relative difference between the current parameter\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("vector x and a locally optimal parameter vector is very likely at most v[xctol].\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 4) {
            Preferences.debug("Relative function convergence.  The relative difference between the current\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("function value and its locally optimal value is very likely at most v(rfctol).\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 5) {
            Preferences.debug("Both x- and relative function convergence (i.e., the conditions for iv(1) = 3 and\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("iv(1) = 4 both hold).\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 6) {
            Preferences.debug("Absolute function convergence.  the current function value is at most v(afctol)\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("in absolute value.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 7) {
            Preferences.debug("Singular convergence.  The hessian near the current iterate appears to be singular\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("or nearly so, and a step of length at most v(lmax0) is unlikely to yield a relative\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("function decrease of more than v(rfctol).\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 8) {
            Preferences.debug("False convergence.  The iterates appear to be converging to a noncritical point.\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("This may mean that the convergence tolerances (v(afctol), v(rfctol), v(xctol)) are\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("too small for the accuracy to which the function and gradient are being computed,\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("that there is an error in computing the gradient, or that the function or gradient\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("is discontinuous near x.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 9) {
            Preferences.debug("Function evaluation limit reached without other convergence (see iv(mxfcal)).\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 10) {
            Preferences.debug("Iteration limit reached without other convergence (see iv(mxiter)).\n", 
            		Preferences.DEBUG_ALGORITHM);
        } else if (status == 11) {
            Preferences.debug("stopx returned .true. (external interrupt).\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 13) {
            Preferences.debug("f(x) cannot be computed at the initial x.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 14) {
            Preferences.debug("Bad parameters passed to assess (which should not occur).\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 15) {
            Preferences.debug("The jacobian could not be computed at x\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 16) {
            Preferences.debug("n or p (or parameter nn to nl2itr) out of range --\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("p <= 0 or n < p or nn < n.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 17) {
            Preferences.debug("Restart attempted with n or p (or par. nn to nl2itr) changed.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 18) {
            Preferences.debug("iv(inits) is out of range.\n", Preferences.DEBUG_ALGORITHM);
        } else if ( (status >= 19) && (status <= 45)) {
            Preferences.debug("v(iv(1)) is out of range.\n", Preferences.DEBUG_ALGORITHM);
        } else if (status == 50) {
            Preferences.debug("iv(1) was out of range.\n", Preferences.DEBUG_ALGORITHM);
        } else if ( (status >= 87) && (status <= 86 + numParam)) {
            Preferences.debug("jtol(iv(1)-86) (i.e., v(iv(1)) is not positive\n", Preferences.DEBUG_ALGORITHM);
        }
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    }

    public class sm2Task implements Runnable {
        private final int start;

        private final int end;

        private final int tDim;

        private final double r1ptj[];

        private final double initial[];

        private final double trapezoidConstant[];

        private final double trapezoidSlope[];

        private final double timeVals[];

        public sm2Task(final int start, final int end, final int tDim, final double r1ptj[], final double initial[],
                final double trapezoidConstant[], final double trapezoidSlope[], final double timeVals[]) {
            this.start = start;
            this.end = end;
            this.tDim = tDim;
            this.r1ptj = r1ptj.clone();
            this.initial = initial.clone();
            this.trapezoidConstant = trapezoidConstant.clone();
            this.trapezoidSlope = trapezoidSlope.clone();
            this.timeVals = timeVals.clone();
        }

        public void run() {
            int i;
            final double y_array[] = new double[tDim - 1];
            final double exparray[][] = new double[tDim][tDim];
            final double ymodel[] = new double[tDim - 1];
            double params[];
            float chi_squared;
            FitSM2ConstrainedModelC dModel;
            double paramSecs0;
            for (i = start; i < end; i++) {
                // fireProgressStateChanged(i * 100/volSize);
                if (bitMask.get(i)) {
                    input(y_array, i);
                    // Note that the nPts, tDim-1, is the number of points in the y_array.
                    dModel = new FitSM2ConstrainedModelC(tDim - 1, y_array, initial, exparray, trapezoidConstant,
                            trapezoidSlope, timeVals, r1ptj, ymodel);
                    dModel.driver();
                    // dModel.dumpResults();
                    params = dModel.getParameters();
                    chi_squared = (float) dModel.getChiSquared();
                    // Convert ktrans from /sec to /min
                    // Mutliply /sec by 60 seconds/minute
                    paramSecs0 = params[0];
                    params[0] = 60.0 * params[0];
                    output(params, paramSecs0, i, dModel.getExitStatus(), chi_squared);
                } // if (bitMask.get(i)
                else {
                    output(null, 0.0, -1, 0, 0.0f);
                }
            } // for (i = start; i < end; i++)
        }

    } // class sm2Task implements Runnable

    public synchronized void input(final double y_array[], final int i) {
        int t;
        for (t = 1; t < tDim; t++) {
            y_array[t - 1] = r1tj[t * volSize + i];
        }
    }

    public synchronized void output(final double params[], final double paramSecs0, final int i, final int exitBits,
            final float chi_squared) {
        int j;
        voxelsProcessed++;
        final long vt100 = voxelsProcessed * 100L;
        barMarker = (int) (vt100 / volSize);
        if (barMarker > oldBarMarker) {
            oldBarMarker = barMarker;
            fireProgressStateChanged(barMarker);
        }
        if (i >= 0) {
            for (j = 0; j < 3; j++) {
                destArray[j][i] = (float) params[j];
                if (Double.isNaN(params[j])) {
                    paramNaN[j]++;
                } else if (Double.isInfinite(params[j])) {
                    paramInf[j]++;
                } else {
                    if (params[j] < paramMin[j]) {
                        paramMin[j] = params[j];
                    }
                    if (params[j] > paramMax[j]) {
                        paramMax[j] = params[j];
                    }
                }
            }
            // Set values that come out at the extreme values of the allowed intervals to NaN rather
            // than to the extreme values. Those values are invariabley wrong, and the images
            // become very difficult to analyze.
            if ( (paramSecs0 <= min_constr[0]) || (paramSecs0 >= max_constr[0]) || (params[1] <= min_constr[1])
                    || (params[1] >= max_constr[1]) || (params[2] <= min_constr[2]) || (params[2] >= max_constr[2])) {
                destArray[0][i] = Float.NaN;
                destArray[1][i] = Float.NaN;
                destArray[2][i] = Float.NaN;
                bitMask.clear(i);
            } else if (exitBits > 0) {
                validVoxels++;
                ktransTotal += destArray[0][i];
                veTotal += destArray[1][i];
                vpTotal += destArray[2][i];
            } else if (exitBits <= 0) {
                bitMask.clear(i);
            }
            destArray[3][i] = chi_squared;
            destExitStatusArray[i] = exitBits;
            exitStatus[exitBits + 11]++;
        } // if (i >= 0)
    }

    class FitSM2ConstrainedModelC extends NLConstrainedEngine {
        private final double yData[];

        private final double exparray[][];

        private final int tDim;

        private final double trapezoidConstant[];

        private final double trapezoidSlope[];

        private final double timeVals[];

        private final double r1ptj[];

        private final double ymodel[];

        /**
         * Creates a new FitDEMRI3ConstrainedModel object.
         * 
         * @param nPoints DOCUMENT ME!
         * @param yData DOCUMENT ME!
         * @param initial DOCUMENT ME!
         */
        public FitSM2ConstrainedModelC(final int nPoints, final double[] yData, final double[] initial,
                final double[][] exparray, final double trapezoidConstant[], final double trapezoidSlope[],
                final double timeVals[], final double r1ptj[], final double ymodel[]) {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3);
            tDim = nPoints + 1;
            this.yData = yData;
            this.exparray = exparray;
            this.trapezoidConstant = trapezoidConstant;
            this.trapezoidSlope = trapezoidSlope;
            this.timeVals = timeVals;
            this.r1ptj = r1ptj;
            this.ymodel = ymodel;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain parameter 0
            bl[0] = min_constr[0];
            bu[0] = max_constr[0];

            // Constrain parameter 1
            bl[1] = min_constr[1];
            bu[1] = max_constr[1];

            // Constrain parameter 2
            bl[2] = min_constr[2];
            bu[2] = max_constr[2];

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
         * Display results of displaying SM2 fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitSM2ConstrainedModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * 
         * 
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j;
            double ktrans;
            double ve;
            double vp;
            int m;
            double intSum;
            double intSumDerivKtrans;
            double intSumDerivVe;
            double ktransDivVe;

            try {
                ctrl = ctrlMat[0];
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] +
                // "\n", Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    ktrans = a[0];
                    ve = a[1];
                    vp = a[2];
                    ktransDivVe = ktrans / ve;
                    for (j = 0; j <= tDim - 1; j++) {
                        for (m = 0; m <= tDim - 1; m++) {
                            exparray[j][m] = Math.exp( (timeVals[j] - timeVals[m]) * ktransDivVe);
                        }
                    }

                    for (m = 2; m <= tDim; m++) {
                        intSum = 0.0;
                        for (j = 2; j <= m; j++) {
                            intSum += trapezoidConstant[j - 2] * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]) * ve;
                            intSum += trapezoidSlope[j - 2]
                                    * ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1] * (timeVals[j - 2] - 1.0 / ktransDivVe)))
                                    * ve;
                        } // for (j = 2; j <= m; j++)
                        ymodel[m - 2] = (intSum + vp * r1ptj[m - 1]) / (1.0 - h);
                    } // for (m = 2; m <= tDim; m++)
                    // evaluate the residuals[j] = ymodel[j] - ySeries[j]
                    for (j = 0; j < nPts; j++) {
                        residuals[j] = ymodel[j] - yData[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    ktrans = a[0];
                    ve = a[1];
                    vp = a[2];
                    ktransDivVe = ktrans / ve;
                    for (j = 0; j <= tDim - 1; j++) {
                        for (m = 0; m <= tDim - 1; m++) {
                            exparray[j][m] = Math.exp( (timeVals[j] - timeVals[m]) * ktransDivVe);
                        }
                    }
                    for (m = 2; m <= tDim; m++) {
                        intSumDerivKtrans = 0.0;
                        intSumDerivVe = 0.0;
                        for (j = 2; j <= m; j++) {
                            intSumDerivKtrans += trapezoidConstant[j - 2]
                                    * ( (timeVals[j - 1] - timeVals[m - 1]) * exparray[j - 1][m - 1] - (timeVals[j - 2] - timeVals[m - 1])
                                            * exparray[j - 2][m - 1]);
                            intSumDerivKtrans += trapezoidSlope[j - 2]
                                    * ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - timeVals[m - 1]) * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1]
                                            * (timeVals[j - 2] - timeVals[m - 1]) * (timeVals[j - 2] - 1.0 / ktransDivVe)));
                            intSumDerivKtrans += trapezoidSlope[j - 2]
                                    * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]) * ve * ve / (ktrans * ktrans);
                            intSumDerivVe += trapezoidConstant[j - 2]
                                    * ( (timeVals[j - 1] - timeVals[m - 1]) * exparray[j - 1][m - 1] - (timeVals[j - 2] - timeVals[m - 1])
                                            * exparray[j - 2][m - 1]) * ( -ktrans / ve);
                            intSumDerivVe += trapezoidConstant[j - 2]
                                    * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]);
                            intSumDerivVe += trapezoidSlope[j - 2]
                                    * ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - timeVals[m - 1]) * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1]
                                            * (timeVals[j - 2] - timeVals[m - 1]) * (timeVals[j - 2] - 1.0 / ktransDivVe)))
                                    * ( -ktrans / ve);
                            intSumDerivVe += trapezoidSlope[j - 2] * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1])
                                    * ( -2.0 * ve / ktrans);
                            intSumDerivVe += trapezoidSlope[j - 2]
                                    * ( (exparray[j - 1][m - 1] * timeVals[j - 1]) - (exparray[j - 2][m - 1] * timeVals[j - 2]));
                        } // for (j = 2; j <= m; j++)
                        covarMat[m - 2][0] = intSumDerivKtrans / (1.0 - h);
                        covarMat[m - 2][1] = intSumDerivVe / (1.0 - h);
                        covarMat[m - 2][2] = r1ptj[m - 1] / (1.0 - h);
                        // Preferences.debug("covarMat[" + (m-2) + "][0] = " + covarMat[m-2][0] + "\n",
                        // Preferences.DEBUG_ALGORITHM);
                        // Preferences.debug("covarMat[" + (m-2) + "][1] = " + covarMat[m-2][1] + "\n", 
                        // Preferences.DEBUG_ALGORITHM);
                        // Preferences.debug("covarMat[" + (m-2) + "][2] = " + covarMat[m-2][2] + "\n", 
                        // Preferences.DEBUG_ALGORITHM);
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

    class FitSM2ConstrainedModel extends NLConstrainedEngine {
        private final double yData[];

        /**
         * Creates a new FitDEMRI3ConstrainedModel object.
         * 
         * @param nPoints DOCUMENT ME!
         * @param yData DOCUMENT ME!
         * @param initial DOCUMENT ME!
         */
        public FitSM2ConstrainedModel(final int nPoints, final double[] yData, final double[] initial) {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3);
            this.yData = yData;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain parameter 0
            bl[0] = min_constr[0];
            bu[0] = max_constr[0];

            // Constrain parameter 1
            bl[1] = min_constr[1];
            bu[1] = max_constr[1];

            // Constrain parameter 2
            bl[2] = min_constr[2];
            bu[2] = max_constr[2];

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
         * Display results of displaying SM2 fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitSM2ConstrainedModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * 
         * 
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j;
            double ktrans;
            double ve;
            double vp;
            int m;
            double intSum;
            double intSumDerivKtrans;
            double intSumDerivVe;

            try {
                ctrl = ctrlMat[0];
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] +
                // "\n", Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    ktrans = a[0];
                    ve = a[1];
                    vp = a[2];
                    ktransDivVe = ktrans / ve;
                    for (j = 0; j <= tDim - 1; j++) {
                        for (m = 0; m <= tDim - 1; m++) {
                            exparray[j][m] = Math.exp( (timeVals[j] - timeVals[m]) * ktransDivVe);
                        }
                    }

                    for (m = 2; m <= tDim; m++) {
                        intSum = 0.0;
                        for (j = 2; j <= m; j++) {
                            intSum += trapezoidConstant[j - 2] * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]) * ve;
                            intSum += trapezoidSlope[j - 2]
                                    * ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1] * (timeVals[j - 2] - 1.0 / ktransDivVe)))
                                    * ve;
                        } // for (j = 2; j <= m; j++)
                        ymodel[m - 2] = (intSum + vp * r1ptj[m - 1]) / (1.0 - h);
                    } // for (m = 2; m <= tDim; m++)
                    // evaluate the residuals[j] = ymodel[j] - yData[j]
                    for (j = 0; j < nPts; j++) {
                        residuals[j] = ymodel[j] - yData[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    ktrans = a[0];
                    ve = a[1];
                    vp = a[2];
                    ktransDivVe = ktrans / ve;
                    for (j = 0; j <= tDim - 1; j++) {
                        for (m = 0; m <= tDim - 1; m++) {
                            exparray[j][m] = Math.exp( (timeVals[j] - timeVals[m]) * ktransDivVe);
                        }
                    }
                    for (m = 2; m <= tDim; m++) {
                        intSumDerivKtrans = 0.0;
                        intSumDerivVe = 0.0;
                        for (j = 2; j <= m; j++) {
                            intSumDerivKtrans += trapezoidConstant[j - 2]
                                    * ( (timeVals[j - 1] - timeVals[m - 1]) * exparray[j - 1][m - 1] - (timeVals[j - 2] - timeVals[m - 1])
                                            * exparray[j - 2][m - 1]);
                            intSumDerivKtrans += trapezoidSlope[j - 2]
                                    * ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - timeVals[m - 1]) * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1]
                                            * (timeVals[j - 2] - timeVals[m - 1]) * (timeVals[j - 2] - 1.0 / ktransDivVe)));
                            intSumDerivKtrans += trapezoidSlope[j - 2]
                                    * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]) * ve * ve / (ktrans * ktrans);
                            intSumDerivVe += trapezoidConstant[j - 2]
                                    * ( (timeVals[j - 1] - timeVals[m - 1]) * exparray[j - 1][m - 1] - (timeVals[j - 2] - timeVals[m - 1])
                                            * exparray[j - 2][m - 1]) * ( -ktrans / ve);
                            intSumDerivVe += trapezoidConstant[j - 2]
                                    * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]);
                            intSumDerivVe += trapezoidSlope[j - 2]
                                    * ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - timeVals[m - 1]) * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1]
                                            * (timeVals[j - 2] - timeVals[m - 1]) * (timeVals[j - 2] - 1.0 / ktransDivVe)))
                                    * ( -ktrans / ve);
                            intSumDerivVe += trapezoidSlope[j - 2] * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1])
                                    * ( -2.0 * ve / ktrans);
                            intSumDerivVe += trapezoidSlope[j - 2]
                                    * ( (exparray[j - 1][m - 1] * timeVals[j - 1]) - (exparray[j - 2][m - 1] * timeVals[j - 2]));
                        } // for (j = 2; j <= m; j++)
                        covarMat[m - 2][0] = intSumDerivKtrans / (1.0 - h);
                        covarMat[m - 2][1] = intSumDerivVe / (1.0 - h);
                        covarMat[m - 2][2] = r1ptj[m - 1] / (1.0 - h);
                        // Preferences.debug("covarMat[" + (m-2) + "][0] = " + covarMat[m-2][0] + "\n", 
                        // Preferences.DEBUG_ALGORITHM);
                        // Preferences.debug("covarMat[" + (m-2) + "][1] = " + covarMat[m-2][1] + "\n", 
                        // Preferences.DEBUG_ALGORITHM);
                        // Preferences.debug("covarMat[" + (m-2) + "][2] = " + covarMat[m-2][2] + "\n", 
                        // Preferences.DEBUG_ALGORITHM);
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

    class FitSM2nl2solModel extends NL2sol {
        int iv[];

        double v[];

        double x[];

        double yData[];

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
        public FitSM2nl2solModel(final int nPoints, final double[] yData, final double[] x, final int iv[],
                final double v[], final boolean useAnalyticJacobian) {

            // nPoints data points
            // 3 coefficients
            // x[] is a length 4 initial guess at input and best estimate at output
            // data starts at x[1]
            // iv[] has length 61 + number of coefficients = 64
            // v[] has length at least 94 + n*p + 3*n + p*(3*p+33)/2
            // uiparm, integer parameter array = null
            // urparm, double parameter array = null
            super(nPoints, 3, x, iv, v, useAnalyticJacobian, null, null);
            this.x = x;
            this.iv = iv;
            this.v = v;
            this.yData = yData;
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying SM2 fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitSM2nl2solModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iv[31]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(2.0 * v[10]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(x[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(x[2]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(x[3]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        public void calcj(final int meqn, final int nvar, final double x[], final int nf, final double jac[][],
                final int uiparm[], final double urparm[]) {

            int j;
            double ktrans;
            double ve;
            // double vp;
            int m;
            double intSumDerivKtrans;
            double intSumDerivVe;
            double ktransDivVe;
            // Calculate the Jacobian analytically
            ktrans = x[1];
            ve = x[2];
            // vp = x[3];
            ktransDivVe = ktrans / ve;
            for (j = 0; j <= tDim - 1; j++) {
                for (m = 0; m <= tDim - 1; m++) {
                    exparray[j][m] = Math.exp( (timeVals[j] - timeVals[m]) * ktransDivVe);
                }
            }
            for (m = 2; m <= tDim; m++) {
                intSumDerivKtrans = 0.0;
                intSumDerivVe = 0.0;
                for (j = 2; j <= m; j++) {
                    intSumDerivKtrans += trapezoidConstant[j - 2]
                            * ( (timeVals[j - 1] - timeVals[m - 1]) * exparray[j - 1][m - 1] - (timeVals[j - 2] - timeVals[m - 1])
                                    * exparray[j - 2][m - 1]);
                    intSumDerivKtrans += trapezoidSlope[j - 2]
                            * ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - timeVals[m - 1]) * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1]
                                    * (timeVals[j - 2] - timeVals[m - 1]) * (timeVals[j - 2] - 1.0 / ktransDivVe)));
                    intSumDerivKtrans += trapezoidSlope[j - 2] * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]) * ve
                            * ve / (ktrans * ktrans);
                    intSumDerivVe += trapezoidConstant[j - 2]
                            * ( (timeVals[j - 1] - timeVals[m - 1]) * exparray[j - 1][m - 1] - (timeVals[j - 2] - timeVals[m - 1])
                                    * exparray[j - 2][m - 1]) * ( -ktrans / ve);
                    intSumDerivVe += trapezoidConstant[j - 2] * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]);
                    intSumDerivVe += trapezoidSlope[j - 2]
                            * ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - timeVals[m - 1]) * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1]
                                    * (timeVals[j - 2] - timeVals[m - 1]) * (timeVals[j - 2] - 1.0 / ktransDivVe)))
                            * ( -ktrans / ve);
                    intSumDerivVe += trapezoidSlope[j - 2] * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1])
                            * ( -2.0 * ve / ktrans);
                    intSumDerivVe += trapezoidSlope[j - 2]
                            * ( (exparray[j - 1][m - 1] * timeVals[j - 1]) - (exparray[j - 2][m - 1] * timeVals[j - 2]));
                } // for (j = 2; j <= m; j++)
                jac[m - 1][1] = intSumDerivKtrans / (1.0 - h);
                jac[m - 1][2] = intSumDerivVe / (1.0 - h);
                jac[m - 1][3] = r1ptj[m - 1] / (1.0 - h);
                // Preferences.debug("jac[" + (m-1) + "][1] = " + jac[m-1][1] + "\n", Preferences.DEBUG_ALGORITHM);
                // Preferences.debug("jac[" + (m-1) + "][2] = " + jac[m-1][2] + "\n", Preferences.DEBUG_ALGORITHM);
                // Preferences.debug("jac[" + (m-1) + "][3] = " + jac[m-1][3] + "\n", Preferences.DEBUG_ALGORITHM);
            }

        }

        public void calcr(final int meqn, final int nvar, final double x[], final int nf, final double r[],
                final int uiparm[], final double urparm[]) {
            double ktrans;
            double ve;
            double vp;
            int j;
            int m;
            double intSum;

            ktrans = x[1];
            ve = x[2];
            vp = x[3];
            ktransDivVe = ktrans / ve;
            for (j = 0; j <= tDim - 1; j++) {
                for (m = 0; m <= tDim - 1; m++) {
                    exparray[j][m] = Math.exp( (timeVals[j] - timeVals[m]) * ktransDivVe);
                }
            }

            for (m = 2; m <= tDim; m++) {
                intSum = 0.0;
                for (j = 2; j <= m; j++) {
                    intSum += trapezoidConstant[j - 2] * (exparray[j - 1][m - 1] - exparray[j - 2][m - 1]) * ve;
                    intSum += trapezoidSlope[j - 2]
                            * ( (exparray[j - 1][m - 1] * (timeVals[j - 1] - 1.0 / ktransDivVe)) - (exparray[j - 2][m - 1] * (timeVals[j - 2] - 1.0 / ktransDivVe)))
                            * ve;
                } // for (j = 2; j <= m; j++)
                ymodel[m - 2] = (intSum + vp * r1ptj[m - 1]) / (1.0 - h);
            } // for (m = 2; m <= tDim; m++)
            // evaluate the residuals[j] = ymodel[j] - ySeries[j]
            for (j = 0; j < meqn; j++) {
                r[j + 1] = ymodel[j] - yData[j];
                // Preferences.debug("residuals["+ (j+1) + "] = " + r[j+1] + "\n", Preferences.DEBUG_ALGORITHM);
            }

        }

    }

    /**
     * 
     * 
     * @param a The best guess parameter values.
     * @param residuals ymodel - yData.
     * @param covarMat The derivative values of y with respect to fitting parameters.
     */
    /*
     * public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) { int ctrl; int j; double ktrans;
     * double ve; double vp; int m; double intSum; double intSumDerivKtrans; double intSumDerivVe;
     * 
     * try { ctrl = ctrlMat[0]; //Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " +
     * a[2] + "\n", Preferences.DEBUG_ALGORITHM); 
     * if ((ctrl == -1) || (ctrl == 1)) { ktrans = a[0]; ve = a[1]; vp = a[2]; ktransDivVe = ktrans/ve;
     * for (j = 0; j <= tDim-1; j++) { for (m = 0; m <= tDim-1; m++) { exparray[j][m] = Math.exp((timeVals[j] -
     * timeVals[m])*ktransDivVe); } }
     * 
     * for (m = 2; m <= tDim; m++) { intSum = 0.0; for (j = 2; j <= m; j++) { intSum +=
     * trapezoidMidpoint[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1])*ve; } // for (j = 2; j <= m; j++) ymodel[m-2] =
     * intSum + vp * r1ptj[m-1]; } // for (m = 2; m <= tDim; m++) // evaluate the residuals[j] = ymodel[j] - ySeries[j]
     * for (j = 0; j < nPts; j++) { residuals[j] = ymodel[j] - ySeries[j]; //Preferences.debug("residuals["+ j + "] = " +
     * residuals[j] + "\n", Preferences.DEBUG_ALGORITHM); } } 
     * // if ((ctrl == -1) || (ctrl == 1)) else if (ctrl == 2) { // Calculate the Jacobian
     * analytically ktrans = a[0]; ve = a[1]; vp = a[2]; ktransDivVe = ktrans/ve; for (j = 0; j <= tDim-1; j++) { for (m =
     * 0; m <= tDim-1; m++) { exparray[j][m] = Math.exp((timeVals[j] - timeVals[m])*ktransDivVe); } } for (m = 2; m <=
     * tDim; m++) { intSumDerivKtrans = 0.0; intSumDerivVe = 0.0; for (j = 2; j <= m; j++) { intSumDerivKtrans +=
     * trapezoidMidpoint[j-2]*((timeVals[j-1]-timeVals[m-1])*exparray[j-1][m-1] - (timeVals[j-2] -
     * timeVals[m-1])*exparray[j-2][m-1]); intSumDerivVe +=
     * trapezoidMidpoint[j-2]*((timeVals[j-1]-timeVals[m-1])*exparray[j-1][m-1] -
     * (timeVals[j-2]-timeVals[m-1])*exparray[j-2][m-1])*(-ktrans/ve); intSumDerivVe +=
     * trapezoidMidpoint[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1]); } // for (j = 2; j <= m; j++) covarMat[m-2][0] =
     * intSumDerivKtrans; covarMat[m-2][1] = intSumDerivVe; covarMat[m-2][2] = r1ptj[m-1];
     * //Preferences.debug("covarMat[" + (m-2) + "][0] = " + covarMat[m-2][0] + "\n", Preferences.DEBUG_ALGORITHM); 
     * //Preferences.debug("covarMat[" +
     * (m-2) + "][1] = " + covarMat[m-2][1] + "\n", Preferences.DEBUG_ALGORITHM); 
     * //Preferences.debug("covarMat[" + (m-2) + "][2] = " +
     * covarMat[m-2][2] + "\n", Preferences.DEBUG_ALGORITHM); } } 
     * // Calculate the Jacobian numerically //else if (ctrl == 2) { //ctrlMat[0] = 0; //} }
     * catch (Exception exc) { Preferences.debug("function error: " + exc.getMessage() + "\n", Preferences.DEBUG_ALGORITHM); }
     * 
     * return; } }
     */

    class Integration2All extends Integration2 {
        public Integration2All() {
            super();
        }

        public double intFunc(final double x) {
            final double function = 0.0;
            return function;
        }
    }

    class Integration2EPAll extends Integration2EP {
        public Integration2EPAll() {
            super();
        }

        public DoubleDouble intFunc(final DoubleDouble x) {
            final DoubleDouble function = DoubleDouble.valueOf(0.0);
            return function;
        }
    }

    class FitAllNL2 extends NL2sol {
        public FitAllNL2() {
            super();
        }

        public void calcj(final int meqn, final int nvar, final double x[], final int nf, final double jac[][],
                final int uiparm[], final double urparm[]) {

        }

        public void calcr(final int meqn, final int nvar, final double x[], final int nf, final double r[],
                final int uiparm[], final double urparm[]) {

        }
    }

    class FitAllEP extends NLConstrainedEngineEP {

        /**
         * Creates a new Fit24DModel object.
         * 
         * @param nPoints DOCUMENT ME!
         * @param xData DOCUMENT ME!
         * @param yData DOCUMENT ME!
         * @param initial DOCUMENT ME!
         */
        public FitAllEP() {

            // nPoints data points, 3 coefficients, and exponential fitting
            super();

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

        }

        /**
         * Fit to function - a0 - a1*(a2**x).
         * 
         * @param a The x value of the data point.
         * @param residuals The best guess parameter values.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final DoubleDouble[] a, final DoubleDouble[] residuals,
                final DoubleDouble[][] covarMat) {

            return;
        }
    }

    class FitAll extends NLConstrainedEngine {

        /**
         * Creates a new Fit24DModel object.
         * 
         * @param nPoints DOCUMENT ME!
         * @param xData DOCUMENT ME!
         * @param yData DOCUMENT ME!
         * @param initial DOCUMENT ME!
         */
        public FitAll() {

            // nPoints data points, 3 coefficients, and exponential fitting
            super();

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

        }

        /**
         * Fit to function - a0 - a1*(a2**x).
         * 
         * @param a The x value of the data point.
         * @param residuals The best guess parameter values.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {

            return;
        }
    }

    class IntModel implements RealFunctionOfOneVariable {
        double ktrans;

        double upper;

        double ve;

        /**
         * Creates a new IntModel object.
         */
        public IntModel(final double upper, final double ktrans, final double ve) {
            this.ktrans = ktrans;
            this.upper = upper;
            this.ve = ve;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param x DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double intFunc(final double x) {
            double function;
            function = 2.0 * x * Math.exp( -ktrans * (upper - x) / ve);

            return function;
        }

        @Override
        public double eval(final double x) {
            return intFunc(x);
        }

    }

}
