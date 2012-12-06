package gov.nih.mipav.model.algorithms;



import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.BitSet;
import java.util.concurrent.*;



public class AlgorithmTimeFitting extends AlgorithmBase {
    private static final int LINEAR_FIT = 0;
    
    private static final int EXPONENTIAL_FIT = 1;
    
    private static final int GAUSSIAN_FIT = 2;
    
    private static final int LAPLACE_FIT = 3;
    
    private static final int LORENTZ_FIT = 4;
    
    /**
     * From Exponential analysis in physical phenomena by Andrei A. Istratov and Oleg F. Vyvenko:
     * Note that for a multiexponential fit nonlinear least squares analysis as used here can handle nonzero baseline offsets.
     * However, for nonlinear least squares initial guesses are needed for the values of unknown decay parameters and, if
     * these initial guesses are poor, the iteration may converge to a local minimum rather than to the absolute minimum.
     * 
     * With Prony's method the baseline offset should be removed first.  
     * 
     * The differentiation of transients only allows the exponent time constants to be determined.  It does not allow the
     * amplitudes by which the exponents are multiplied to be determined. Also experimental transients are noisy, and large
     * errors would result from their numerical differentiation. 
     * 
     * The method of modulating functions does not enable one to determine the amplitude of the transients and is not tolerant
     * to nonzero baseline offsets. 
     * 
     * The integration method allows one to determine the baseline.  However, the solution is more stable and exact if the
     * baseline is subtracted from the raw signal before the analysis.  For the integration method, on computed two-component
     * decays with tau1/tau2 = 2.5 the reliable separation of components is impossible if the SNR is less than 30.  For 
     * tau1/tau2, two components can be separated for a SNR of about 10.  When two-component analysis is performed with a baseline
     * determination the mean error of the calculated time constants increases by a factor of about 3-10.
     * 
     * Before the method of moments can be applied, the baseline offset must be removed from the data.
     * 
     * The Laplace-Pade approximation is not applicable to decays containing baseline offset.
     * 
     * Tolerance of the method to baseline offsets may be very important, depending on whether a baseline offset can be
     * encountered in the experiment.  Few of the discussed techniques are tolerable of nonzero baseline offsets.  Such
     * commonly used methods as Prony's or method of moments will provide wrong results or even crash if a decay contains
     * an offset.  Unfortunately, algorithms for extrapolation of baseline offsets are poorly developed and are not
     * always sufficiently exact.  Therefore, we would recommend algorithms that do not require baseline corrections
     * and can accommodate transients with a baseline.
     * 
     * Taking into account stability for wide range changes in parameters and insensitivity to baseline offsets, the 
     * authors select nonlinear least squares as the best fitting method for multiexponential fits.
     * 
     * Fitting routines are only accurate if the number of components is correct and the initial approximation is close
     * to the true solution.  A way to obtain this initial approximation is to extract it from a spectroscopic method, 
     * as was done by Provencher and Mazzola et al.
     */
    private static final int MULTIEXPONENTIAL_FIT = 5;
    
    private static final int RAYLEIGH_FIT = 6;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** A vector of center times for each volume */
    private double timeVals[] = null;
    
    private double srcArray[] = null;

    private int i;

    private int xDim;

    private int yDim;

    private int zDim;

    private int tDim;

    private int volSize;
    
    private boolean findInitialFromData = true;

    private double initial[];
    
    private boolean useBounds[];
    
    private double lowBounds[];
    
    private double highBounds[];

    private int[] exitStatus;

    private int[] paramNaN;

    private int[] paramInf;

    private double[] paramMin;

    private double[] paramMax;
    
    private double[] paramTotal;
    
    private double[] paramAverage;

    private int processors;

    private double destArray[];

    private int destExitStatusArray[];

    private long voxelsProcessed = 0;

    private int barMarker = 0;

    private int oldBarMarker = 0;

    private BitSet bitMask = null;

    private int validVoxels = 0;

    private boolean wholeImage = true;
    
    private ModelImage exitStatusImage;
    
    private boolean useLog = false;
    
    private int functionFit;
    
    private int numVariables;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmTimeFitting object.
     * 
     */
    public AlgorithmTimeFitting(final ModelImage destImage, final ModelImage srcImage, final ModelImage exitStatusImage, final boolean useLog,
            final int functionFit, final int numVariables, boolean findInitialFromData, double initial[], boolean useBounds[],
            double lowBounds[], double highBounds[]) {

        super(destImage, srcImage);
        this.exitStatusImage = exitStatusImage;
        this.useLog = useLog;
        this.functionFit = functionFit;
        this.numVariables =  numVariables;
        this.findInitialFromData = findInitialFromData;
        this.initial = initial;
        this.useBounds = useBounds;
        this.lowBounds = lowBounds;
        this.highBounds = highBounds;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        destImage = null;
        initial = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {
        boolean  selfTest = false;
        int functionArray[] = null;
        int numVariablesArray[] = null;
        double initialArray[][] = null;
        double a0;
        double a1;
        double a2;
        double a3;
        double a4;
        ViewVOIVector VOIs;
        short mask[];
        int t;
        double y_array[];
        int srcSize4D;
        int destSize4D;
        FitLine lineModel;
        FitExponential expModel;
        FitGaussian gaussianModel;
        FitLaplace laplaceModel;
        FitLorentz lorentzModel;
        FitMultiExponential multiExponentialModel;
        FitRayleigh rayleighModel;
        double[] params = null;
        double chi_squared = 0.0;
        int status = 0;
        int j;
        long normalTerminations = 0;
        long abnormalTerminations = 0;
        int index = 0;
        String timeString;
        int firstColonIndex;
        int lastColonIndex;
        String hourString;
        String minuteString;
        String secondString;
        double delt;
        double minDiff;
        double diff;
        int tMid;
        double m;
        double val;
        double maxVal;
        boolean beenHalved;
        int tHalved = 0;
        double deltSquared;
        double a2Squared;
        double ratio;
        double mlower;
        double mupper;
        double sqrta2;
        double sqrt2 = Math.sqrt(2.0);
        double sqrtpoint5 = Math.sqrt(0.5);
        double exppoint5 = Math.exp(0.5);
        double expminuspoint5 = Math.exp(-0.5);
        double B = 0.0;
        AlgorithmMultiExponentialFitting multiExponentialAlg;
        int numExponentials = (numVariables - 1)/2;
        // the normal (unweighted) case of unit least squares weights w[k]
        int iwt = 1;
        // mtry = the maximum number of tries that will be made to find a solution for a single value of nlambda
        int mtry = 15;
        // t values are specified rather than calculated from tstart and tend.
        boolean regint = false;
        // Do not assume a zero baseline
        boolean nobase = false;
        // coefficients multiplying exponentials can be negative
        boolean nonneg = false;
        // No Preferences.debug from AlgorithmMultiExponentialFitting
        boolean showDebug = false;
        // Don't output t[] and y[] values
        boolean pry = false;
        // Don't output each iteration of preliminary analysis
        boolean prprel = false;
        // Don't output each iteration of final analysis
        boolean prfinl = false;
        // Don't plot fit of final solution
        boolean plotrs = false;
        // Don't print final summary of results a second time
        boolean repeat = false;
        // Number of time intervals
        int nint = 1;
        double tstart[] = new double[1];
        double tend[] = null;
        int nt[] = new int[1];
        double sqrtw[] = null;

        processors = Runtime.getRuntime().availableProcessors();
        Preferences.debug("Available processors = " + processors + "\n", Preferences.DEBUG_ALGORITHM);
        if (selfTest) {
            processors = 1;
        }

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        tDim = srcImage.getExtents()[3];
        volSize = xDim * yDim * zDim;
        srcSize4D = volSize * tDim;
        srcArray = new double[srcSize4D];
        // Have the variables followed by chi-squared
        destSize4D = volSize * (numVariables+1);
        destArray = new double[destSize4D];
        destExitStatusArray = new int[volSize];
        for (i = 0; i < destSize4D; i++) {
            destArray[i] = Double.NaN;
        }
        y_array = new double[tDim];
        paramNaN = new int[numVariables];
        paramInf = new int[numVariables];
        paramMin = new double[numVariables];
        paramMax = new double[numVariables];
        paramTotal = new double[numVariables];
        paramAverage = new double[numVariables];
        
        VOIs = srcImage.getVOIs();
        int nVOIs = VOIs.size();
        int nBoundingVOIs = 0;
        
        for (int i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
                index = i;
            }
        } // for (i = 0; i < nVOIs; i++)
        
        if (nBoundingVOIs > 1) {
            MipavUtil.displayError("Should only have 1 bounding VOI");
            setCompleted(false);
            return;
        }
        else if (nBoundingVOIs == 1) {
            mask = new short[volSize];
            for (i = 0; i < volSize; i++) {
                mask[i] = -1;
            }
            mask = srcImage.generateVOIMask(mask, index); 
            bitMask = new BitSet(volSize);
            for (i = 0; i < volSize; i++) {
                if (mask[i] != -1) {
                    bitMask.set(i);
                }
            }
            mask = null;
            wholeImage = false;
        }
        else {
            wholeImage = true;
        }

        
        try {
            srcImage.exportData(0, srcSize4D, srcArray);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on srcImage.exportData(0, srcSize4D, srcArray");
            setCompleted(false);
            return;
        }
        
        if (useLog) {
            for (i = 0; i < volSize; i++) {
                srcArray[i] = Math.log10(srcArray[i]);
            }
        } // if (useLog)

        timeVals = new double[tDim];
        for (t = 0; t < tDim; t++) {
            timeVals[t] = t * srcImage.getFileInfo()[0].getResolutions()[3];
            if (srcImage.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                if ( ((FileInfoDicom) srcImage.getFileInfo(t)).getTagTable() !=  null) {
                    // Acquisition time
                    timeString = ((String) ((FileInfoDicom) srcImage.getFileInfo(t)).getTagTable().getValue("0008,0032"));
                    if (timeString != null) {
                        firstColonIndex = timeString.indexOf(":", 0);
                        lastColonIndex = timeString.lastIndexOf(":", timeString.length() - 1);
                        hourString = timeString.substring(0, firstColonIndex);
                        minuteString = timeString.substring(firstColonIndex + 1, lastColonIndex);
                        secondString = timeString.substring(lastColonIndex + 1);
                        timeVals[t] = 3600.0 * Double.valueOf(hourString).doubleValue() + 60.0
                                * Double.valueOf(minuteString).doubleValue() + Double.valueOf(secondString).doubleValue(); 
                    } // if (timeString != null)
                }
            }
        }
        exitStatus = new int[12356];
        
        for (i = 0; i < numVariables; i++) {
            paramMin[i] = Double.MAX_VALUE;
            paramMax[i] = -Double.MAX_VALUE;
        }
        
        if (selfTest) {
            volSize = 9;
            tDim = 100;
            srcArray = new double[tDim * volSize];
            y_array = new double[tDim];
            functionArray = new int[volSize];
            numVariablesArray = new int[volSize];
            initialArray = new double[volSize][9];
            timeVals = new double[tDim];
            for (t = 0; t < tDim; t++) {
                timeVals[t] = t;
            }
            // linear a0 + a1*t
            functionArray[0] = LINEAR_FIT;
            numVariablesArray[0] = 2;
            initialArray[0][0] = 0.0; // intercept
            initialArray[0][1] = -1.0; // slope;
            a0 = 100.0;
            a1 = -10.0;
            for (t = 0; t < tDim; t++) {
                srcArray[t * volSize] = a0 + a1 * t;
            }
            functionArray[1] = LINEAR_FIT;
            numVariablesArray[1] = 2;
            initialArray[1][0] = 0.0; // intercept
            initialArray[1][1] = -1.0; // slope;
            a0 = -50.0;
            a1 = 2.5;
            for (t = 0; t < tDim; t++) {
                srcArray[t * volSize + 1] = a0 + a1 * t;
            }
            functionArray[2] = EXPONENTIAL_FIT;
            numVariablesArray[2] = 3;
            initialArray[2][0] = 50.0;
            initialArray[2][1] = 50.0;
            initialArray[2][2] = 1.0E-2;
            a0 = 100.0;
            a1 = 10.0;
            a2 = 1.0E-3;
            for (t = 0; t < tDim; t++) {
                srcArray[t * volSize + 2] = a0 + a1 * Math.exp(a2 * t);
            }
            functionArray[3] = EXPONENTIAL_FIT;
            numVariablesArray[3] = 3;
            initialArray[3][0] = -50.0;
            initialArray[3][1] = -5.0;
            initialArray[3][2] = -5.0E-3;
            a0 = -85.0;
            a1 = -10.0;
            a2 = -1.0E-3;
            for (t = 0; t < tDim; t++) {
                srcArray[t * volSize + 3] = a0 + a1 * Math.exp(a2 * t);
            } 
            functionArray[4] = GAUSSIAN_FIT;
            numVariablesArray[4] = 3;
            initialArray[4][0] = 2.5;
            initialArray[4][1] = 35.0;
            initialArray[4][2] = 20.0;
            a0 = 5.0;
            a1 = 30.0;
            a2 = 10.0;
            for (t = 0; t < tDim; t++) {
                srcArray[t * volSize + 4] = a0* Math.exp(-((t-a1)*(t-a1))/(2.0 * a2 * a2));
            }
            functionArray[5] = LAPLACE_FIT;
            numVariablesArray[5] = 3;
            initialArray[5][0] = 2.5;
            initialArray[5][1] = 35.0;
            initialArray[5][2] = 20.0;
            a0 = 5.0;
            a1 = 30.0;
            a2 = 10.0;
            for (t = 0; t < tDim; t++) {
                srcArray[t * volSize + 5] = a0*Math.exp(-Math.abs(t - a1)/a2);
            }
            functionArray[6] = LORENTZ_FIT; 
            numVariablesArray[6] = 3;
            initialArray[6][0] = 2.5;
            initialArray[6][1] = 35.0;
            initialArray[6][2] = 20.0;
            a0 = 5.0;
            a1 = 30.0;
            a2 = 10.0;
            for (t = 0; t < tDim; t++) {
                srcArray[t * volSize + 6] = a0/((t-a1)*(t-a1) + a2*a2);
            }
            numExponentials = 2;
            functionArray[7] = MULTIEXPONENTIAL_FIT;
            numVariablesArray[7] = 5;
            initialArray[7][0] = -80.0;
            initialArray[7][1] = -22.0;
            initialArray[7][2] = -2.9E-3;
            initialArray[7][3] = -90.0;
            initialArray[7][4] = -1.1E-2;
            a0 = -85.0;
            a1 = -25.0;
            a2 = -2.5E-2;
            a3 = -100.0;
            a4 = -2.0E-1;
            for (t = 0; t < tDim; t++) {
                srcArray[t * volSize + 7] = a0 + a1 * Math.exp(a2 * t) + a3 * Math.exp(a4 * t);
            } 
            functionArray[8] = RAYLEIGH_FIT; 
            numVariablesArray[8] = 3;
            initialArray[8][0] = 15.0;
            initialArray[8][1] = 48.0;
            initialArray[8][2] = 32.0;
            a0 = 10.0;
            a1 = 50.5;
            a2 = 30.0;
            for (t = 0; t < tDim; t++) {
                if (t >= 50.5) {
                    srcArray[t * volSize + 8] = a0 * (t-a1)*Math.exp(-(t-a1)*(t-a1)/a2);
                }
                else {
                    srcArray[t * volSize + 8] = 0.0;
                }
            }
        }
        
        if ((processors > 1) && (Preferences.isMultiThreadingEnabled())) {
            int start;
            int end;
            final ExecutorService application = Executors.newCachedThreadPool();
            for (i = 0; i < processors; i++) {
                start = (i * volSize) / processors;
                end = ( (i + 1) * volSize) / processors;
                application.execute(new fittingTask(start, end, tDim, initial, timeVals, useBounds, lowBounds, highBounds));
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
                if (wholeImage || bitMask.get(i)) {
                    for (t = 0; t < tDim; t++) {
                        y_array[t] = srcArray[t * volSize + i];
                    }
                    if (selfTest) {
                        functionFit = functionArray[i];
                        numVariables = numVariablesArray[i];
                        for (j = 0; j < numVariables; j++) {
                            initial[j] = initialArray[i][j];
                        }
                    }
                    switch(functionFit) {
                        case LINEAR_FIT:
                            if (findInitialFromData) {
                                // a0 + a1 * timeVals[tDim-1] = y_array[tDim-1]
                                // a0 + a1 * timeVals[0] = y_array[0]
                                // a1 * (timeVals[tDim-1] - timeVals[0]) = (y_array[tDim-1] - y_array[0])
                                initial[1] = (y_array[tDim-1]- y_array[0])/(timeVals[tDim-1] - timeVals[0]);
                                initial[0] = y_array[0] - initial[1] * timeVals[0];
                            }
                            lineModel = new FitLine(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            lineModel.driver();
                            params = lineModel.getParameters();
                            chi_squared = lineModel.getChiSquared();
                            status = lineModel.getExitStatus();
                            break;
                        case EXPONENTIAL_FIT:
                            if (findInitialFromData) {
                                // a0 + a1 * exp(a2 * timeVals[tDim-1]) = y_array[tDim-1]
                                // Let timeVals[tDim-1] - timeVals[0] = delt
                                // Find tMid such that timesVals[tMid] - timeVals[0] is closest to delt/2.
                                // a0 + a1 * exp(a2 * timeVals[tMid]) = y_array[tMid]
                                // a0 + a1 * exp(a2 * timeVals[0]) = y_array[0]
                                // a1*(exp(a2 * timeVals[tDim-1]) - exp(a2 * timeVals[0]) = (y_array[tDim-1] - y_array[0])
                                // a1*(exp(a2 * timeVals[tMid]) - exp(a2 * timeVals[0]) = (y_array[tMid] - y_array[0])
                                //
                                // ((exp(a2 * timeVals[tDim-1]) - exp(a2 * timeVals[0]))/(exp(a2 * timeVals[tMid]) - exp(a2 * timeVals[0])) =
                                // (y_array[tDim-1] - y_array[0])/(y_array[tMid] - y_array[0]) = m
                                
                                // Dividing numerator and denominator by exp(a2 * timeVals[0]):
                                // ((exp(a2 * delt) - 1)/(exp(0.5 * a2 * delt) - 1) = m
                                // exp(a2 * delt) - 1 = m*exp(0.5 * a2 * delt)  - m
                                // exp(a2 * delt) - m*exp(0.5 * a2 * delt) + (m - 1) = 0
                                // exp(0.5 * a2 * delt) = (m +- sqrt(m*m - 4*(m-1))/2 = m-1, 1
                                // m - 1 is the correct answer
                                // exp(a2 * delt) = (m-1)**2
                                // a2 = ln((m-1)**2)/delt
                                delt = timeVals[tDim-1] - timeVals[0];
                                minDiff = Double.MAX_VALUE;
                                tMid = tDim/2;
                                for (j = 0; j < tDim-1; j++) {
                                    diff = Math.abs(timeVals[j] - timeVals[0] - delt/2.0);
                                    if (diff < minDiff) {
                                        minDiff = diff;
                                        tMid = j;
                                    }
                                } // for (j = 0; j < tDim-1; j++)
                                m = (y_array[tDim-1] - y_array[0])/(y_array[tMid] - y_array[0]);
                                diff = m - 1.0;
                                initial[2] = Math.log(diff*diff)/delt;
                                initial[1] = (y_array[tDim-1] - y_array[0])/
                                        (Math.exp(initial[2] * timeVals[tDim-1]) - Math.exp(initial[2] * timeVals[0]));
                                initial[0] = y_array[tDim-1] - initial[1] - Math.exp(initial[2] * timeVals[tDim-1]);
                                // Note that Exponential analysis in physical phenomena by Andrei A. Istratov and
                                // Oleg F. Vyvenko lists the baseline obtained from 3 points as 
                                // B = (Y1Y3 - Y2**2)/(Y1 + Y3 - 2Y2)
                                // where Y1 corresponds to y_array[0], Y2 corresponds to y_array[tMid], and 
                                // Y3 corresponds to y_array[tDim-1]
                                if (selfTest) {
                                    B = (y_array[0]*y_array[tDim-1] - y_array[tMid]*y_array[tMid])/
                                            (y_array[0] + y_array[tDim-1] - 2.0 * y_array[tMid]);
                                }
                            } // if (findInitialFromData)
                            expModel = new FitExponential(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            expModel.driver();
                            params = expModel.getParameters();
                            chi_squared = expModel.getChiSquared();
                            status = expModel.getExitStatus();
                            break;
                        case GAUSSIAN_FIT:
                            if (findInitialFromData) {
                                // Gaussian (a0*exp(-(t-a1)^2/(2*(a2)^2))
                                maxVal = 0.0;
                                tMid = tDim/2;
                                for (j = 0; j < tDim; j++) {
                                    val = Math.abs(y_array[j]);
                                    if (val > maxVal) {
                                        maxVal = val;
                                        tMid = j;
                                    }
                                } // for (j = 0; j < tDim; j++)
                                initial[1] = timeVals[tMid];
                                initial[0] = y_array[tMid];
                                beenHalved = false;
                                for (j = tMid -1; (j >= 0) && (!beenHalved); j--) {
                                    if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                        beenHalved = true;
                                        tHalved = j;
                                    }
                                }
                                if (!beenHalved) {
                                    for (j = tMid + 1; (j <= tDim-1) && (!beenHalved); j++) {
                                        if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                            beenHalved = true;
                                            tHalved = j;
                                        }
                                    }
                                } // if (!beenHalved)
                                if (!beenHalved) {
                                    if (y_array[0] < y_array[tDim-1]) {
                                        tHalved = 0;
                                    }
                                    else {
                                        tHalved = tDim-1;
                                    }
                                } // if (!beenHalved)
                                initial[2] = Math.abs((timeVals[tHalved] - timeVals[tMid]) * 
                                        Math.sqrt(0.5 / Math.log(initial[0]/y_array[tHalved])));     
                            } // if (findInitialFromData)
                            gaussianModel = new FitGaussian(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            gaussianModel.driver();
                            params = gaussianModel.getParameters();
                            chi_squared = gaussianModel.getChiSquared();
                            status = gaussianModel.getExitStatus();
                            break;
                        case LAPLACE_FIT:
                            if (findInitialFromData) {
                                // Laplace (a0*exp(-|t-a1|/a2))
                                maxVal = 0.0;
                                tMid = tDim/2;
                                for (j = 0; j < tDim; j++) {
                                    val = Math.abs(y_array[j]);
                                    if (val > maxVal) {
                                        maxVal = val;
                                        tMid = j;
                                    }
                                } // for (j = 0; j < tDim; j++)
                                initial[1] = timeVals[tMid];
                                initial[0] = y_array[tMid];
                                beenHalved = false;
                                for (j = tMid -1; (j >= 0) && (!beenHalved); j--) {
                                    if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                        beenHalved = true;
                                        tHalved = j;
                                    }
                                }
                                if (!beenHalved) {
                                    for (j = tMid + 1; (j <= tDim-1) && (!beenHalved); j++) {
                                        if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                            beenHalved = true;
                                            tHalved = j;
                                        }
                                    }
                                } // if (!beenHalved)
                                if (!beenHalved) {
                                    if (y_array[0] < y_array[tDim-1]) {
                                        tHalved = 0;
                                    }
                                    else {
                                        tHalved = tDim-1;
                                    }
                                } // if (!beenHalved)
                                initial[2] = Math.abs(timeVals[tHalved] - timeVals[tMid]) / Math.log(initial[0]/y_array[tHalved]);      
                            } // if (findInitialFromData)
                            laplaceModel = new FitLaplace(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            laplaceModel.driver();
                            params = laplaceModel.getParameters();
                            chi_squared = laplaceModel.getChiSquared();
                            status = laplaceModel.getExitStatus();
                            break;
                        case LORENTZ_FIT:
                            if (findInitialFromData) {
                                // Lorentz (a0/((t-a1)*(t-a1) + a2*a2)
                                maxVal = 0.0;
                                tMid = tDim/2;
                                for (j = 0; j < tDim; j++) {
                                    val = Math.abs(y_array[j]);
                                    if (val > maxVal) {
                                        maxVal = val;
                                        tMid = j;
                                    }
                                } // for (j = 0; j < tDim; j++)
                                initial[1] = timeVals[tMid];
                                // a0/a2**2 = y_array[tMid]
                                beenHalved = false;
                                for (j = tMid -1; (j >= 0) && (!beenHalved); j--) {
                                    if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                        beenHalved = true;
                                        tHalved = j;
                                    }
                                }
                                if (!beenHalved) {
                                    for (j = tMid + 1; (j <= tDim-1) && (!beenHalved); j++) {
                                        if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                            beenHalved = true;
                                            tHalved = j;
                                        }
                                    }
                                } // if (!beenHalved)
                                if (!beenHalved) {
                                    if (y_array[0] < y_array[tDim-1]) {
                                        tHalved = 0;
                                    }
                                    else {
                                        tHalved = tDim-1;
                                    }
                                } // if (!beenHalved)
                                delt = (timeVals[tHalved] - initial[1]);
                                deltSquared = delt * delt;
                                // a0/(deltSquared + a2**2) = y_array[tHalved]
                                // (deltSquared + a2**2)/(a2**2) = y_array[tMid]/y_array[tHalved] = m
                                m = y_array[tMid]/y_array[tHalved];
                                // a2 = sqrt(deltSquared/(m-1))
                                a2Squared = deltSquared/(m - 1.0);
                                initial[2] = Math.sqrt(a2Squared);
                                // a0 = (a2**2) * y_array[tMid]
                                initial[0] = a2Squared * y_array[tMid];
                            } // if (findInitialFromData)
                            lorentzModel = new FitLorentz(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            lorentzModel.driver();
                            params = lorentzModel.getParameters();
                            chi_squared = lorentzModel.getChiSquared();
                            status = lorentzModel.getExitStatus();
                            break;
                        case MULTIEXPONENTIAL_FIT:
                            if (findInitialFromData) {
                                multiExponentialAlg =  new AlgorithmMultiExponentialFitting(numExponentials, iwt, mtry, regint,
                                        nobase, nonneg, showDebug, pry, prprel, prfinl, plotrs, repeat, tDim, timeVals, nint,
                                        tstart, tend, nt, y_array, sqrtw);  
                                multiExponentialAlg.run();
                                params = multiExponentialAlg.getParameters();
                            }
                            else {
                                multiExponentialModel = new FitMultiExponential(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                                multiExponentialModel.driver();
                                params = multiExponentialModel.getParameters();
                                chi_squared = multiExponentialModel.getChiSquared();
                                status = multiExponentialModel.getExitStatus();
                            }
                            break;
                        case RAYLEIGH_FIT:
                            if (findInitialFromData) {
                                // Rayleigh Distribution a0 *(t-a1)*exp(-(t-a1)*(t-a1)/a2)*u(t-a1) 
                                // Derivative with respect to t = a0 * exp(-(t-a1)*(t-a1)/a2) * (1 - 2*(t-a1)*(t-a1)/a2)
                                // Maximum where derivative = 0 or maximum at t = a1 + sqrt(a2/2).
                                // Here ymax = a0 * sqrt(a2/2) * exp(-0.5)
                                // At th = a1 + sqrt(a2), yh = a0 * sqrt(a2) * exp(-1)
                                // ymax/yh = sqrt(1/2) * exp(0.5) = 0.7071067811865475 * 1.6487 = 1.1658
                                // yh/ymax = 0.85777
                                // In general at t = a1 + m*sqrt(a2), for m > sqrt(1/2), y/ymax = sqrt(2) * exp(0.5) * m * exp(-m**2)
                                maxVal = 0.0;
                                tMid = tDim/2;
                                for (j = 0; j < tDim; j++) {
                                    val = Math.abs(y_array[j]);
                                    if (val > maxVal) {
                                        maxVal = val;
                                        tMid = j;
                                    }
                                } // for (j = 0; j < tDim; j++)
                                beenHalved = false;
                                for (j = tMid + 1; (j <= tDim-1) && (!beenHalved); j++) {
                                    if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                        beenHalved = true;
                                        tHalved = j;
                                    }
                                }
                                if (!beenHalved) { 
                                    tHalved = tDim-1;
                                } // if (!beenHalved)
                                ratio = y_array[tHalved]/(y_array[tMid] * sqrt2 * exppoint5);
                                // ratio = m * exp(-m**2)
                                mlower = sqrtpoint5;
                                mupper = 5.0;
                                m = (mlower + mupper)/2.0;
                                diff = mupper - mlower;
                                while (Math.abs(diff) > 1.0E-5) {
                                    m = (mlower + mupper)/2.0;
                                    diff = ratio - (m * Math.exp(-m*m));
                                    if (diff > 0.0) {
                                         mupper = m;    
                                    }
                                    else if (diff < 0.0) {
                                        mlower = m;
                                    }
                                } // while (diff > 1.0E-5)
                                // timeVals[tHalved] - timeVals[tMid] = a1 + m * sqrt(a2) - (a1 + sqrt(0.5)*sqrt(a2)) = 
                                //                                    (m - sqrt(0.5)) * sqrt(a2)
                                sqrta2 = (timeVals[tHalved] - timeVals[tMid])/(m - sqrtpoint5);
                                initial[2] = sqrta2 * sqrta2;
                                initial[0] = y_array[tMid]/(sqrta2 * sqrtpoint5 * expminuspoint5);
                                initial[1] = timeVals[tHalved] - m * sqrta2;
                            } // if (findInitialFromData)
                            rayleighModel = new FitRayleigh(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            rayleighModel.driver();
                            params = rayleighModel.getParameters();
                            chi_squared = rayleighModel.getChiSquared();
                            status = rayleighModel.getExitStatus();
                            break;
                    } // switch(functionFit)
                    if (selfTest) {
                        Preferences.debug("i = " + i + "\n", Preferences.DEBUG_ALGORITHM);
                        switch (functionFit) {
                            case LINEAR_FIT:
                                Preferences.debug("LINEAR\n", Preferences.DEBUG_ALGORITHM);
                                break;
                            case EXPONENTIAL_FIT:
                                Preferences.debug("EXPONENTIAL\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("B = " + B + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("initial[0] = " + initial[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                // Gave B = -78.435 and initial[0] = -78.436
                                // Gave B = 103.04 and initial[0] = 102.93
                                break;
                            case GAUSSIAN_FIT:
                                Preferences.debug("GAUSSIAN\n", Preferences.DEBUG_ALGORITHM);
                                break;
                            case LAPLACE_FIT:
                                Preferences.debug("LAPLACE\n", Preferences.DEBUG_ALGORITHM);
                                break;
                            case LORENTZ_FIT:
                                Preferences.debug("LORENTZ\n", Preferences.DEBUG_ALGORITHM);
                                break;
                            case MULTIEXPONENTIAL_FIT:
                                Preferences.debug("MULTIEXPONENTIAL\n", Preferences.DEBUG_ALGORITHM);
                                break;
                            case RAYLEIGH_FIT:
                                Preferences.debug("RAYLEIGH\n", Preferences.DEBUG_ALGORITHM);
                                break;
                        }
                        for (j = 0; j < numVariables; j++) {
                            Preferences.debug("a["+j+"] = " + params[j] + "\n", Preferences.DEBUG_ALGORITHM);
                        }
                        Preferences.debug("chi_squared = " + chi_squared + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("status = " + status + "\n", Preferences.DEBUG_ALGORITHM);
                        continue;
                    } // if (selfTest)
                    for (j = 0; j < numVariables; j++) {
                        destArray[j*volSize +i] = params[j];
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
                    if (status > 0) {
                        validVoxels++;
                        for (j = 0; j < numVariables; j++) {
                            paramTotal[j] += params[j];
                        }
                    } else {
                        bitMask.clear(i);
                    }
                    destArray[(numVariables * volSize) + i] = chi_squared;
                    destExitStatusArray[i] = status;
                    exitStatus[status + 11]++;
                } // if (wholeImage || bitMask.get(i))
            } // for (i = 0; i < volSize; i++)
        } // else processors == 1
            
        if (selfTest) {
            setCompleted(true);
            return;
        }
        
        for (i = 0; i < numVariables; i++) {
            if (paramNaN[i] > 0) {
                System.out.println(paramNaN[i] + " of a" + i + " values are NaN");
                Preferences.debug(paramNaN[i] + " of a" + i + " values are NaN\n", Preferences.DEBUG_ALGORITHM);    
            }
        }
        
        for (i = 0; i < numVariables; i++) {
            if (paramInf[i] > 0) {
                System.out.println(paramInf[i] + " of a" + i + " values are infinite");
                Preferences.debug(paramInf[i] + " of a" + i + " values are infinite\n", Preferences.DEBUG_ALGORITHM);    
            }
        }

        System.out.println("Valid voxels = " + validVoxels);
        Preferences.debug("Valid voxels = " + validVoxels + "\n", Preferences.DEBUG_ALGORITHM);
        
        for (i = 0; i < numVariables; i++) {
            paramAverage[i] = paramTotal[i] / validVoxels;    
        }
        
        for (i = 0; i < numVariables; i++) {
            System.out.println("a" + i + " minimum value = " + paramMin[i]);
            Preferences.debug("a" + i + " minimum value = " + paramMin[i] + "\n", Preferences.DEBUG_ALGORITHM);
            System.out.println("a" + i + " average value = " + paramAverage[i]);
            Preferences.debug("a" + i + " average value = " + paramAverage[i] + "\n", Preferences.DEBUG_ALGORITHM);
            System.out.println("a" + i + " maximum value = " + paramMax[i]);
            Preferences.debug("a" + i + " maximum value = " + paramMax[i] + "\n", Preferences.DEBUG_ALGORITHM);    
        }

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

        try {
            destImage.importData(0, destArray, true);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on destImage.importData(0, destArray, true)");
            setCompleted(false);
            return;
        }

        try {
            exitStatusImage.importData(0, destExitStatusArray, true);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on exitStatusImage.importData(0, destExitStatusArray, true)");
            setCompleted(false);
            return;
        }

        setCompleted(true);
    }
    
    public class fittingTask implements Runnable {
        private final int start;

        private final int end;

        private final int tDim;

        private final double initial[];

        private final double timeVals[];
        
        private final boolean useBounds[];
        
        private final double lowBounds[];
        
        private final double highBounds[];

        public fittingTask(final int start, final int end, final int tDim, final double initial[], final double timeVals[],
                           final boolean useBounds[], final double lowBounds[], final double highBounds[]) {
            this.start = start;
            this.end = end;
            this.tDim = tDim;
            this.initial = initial.clone();
            this.timeVals = timeVals.clone();
            this.useBounds = useBounds.clone();
            this.lowBounds = lowBounds.clone();
            this.highBounds = highBounds.clone();
        }

        public void run() {
            int i;
            int j;
            final double y_array[] = new double[tDim];
            double params[] = null;
            double chi_squared = 0.0;
            FitLine lineModel;
            FitExponential expModel;
            FitGaussian gaussianModel;
            FitLaplace laplaceModel;
            FitLorentz lorentzModel;
            FitMultiExponential multiExponentialModel;
            FitRayleigh rayleighModel;
            int status = 0;
            double delt;
            double minDiff;
            int tMid;
            double diff;
            double m;
            double maxVal;
            double val;
            boolean beenHalved;
            int tHalved = 0;
            double deltSquared;
            double a2Squared;
            double ratio;
            double mlower;
            double mupper;
            double sqrta2;
            double sqrt2 = Math.sqrt(2.0);
            double sqrtpoint5 = Math.sqrt(0.5);
            double exppoint5 = Math.exp(0.5);
            double expminuspoint5 = Math.exp(-0.5);
            AlgorithmMultiExponentialFitting multiExponentialAlg;
            int numExponentials = (numVariables - 1)/2;
            // the normal (unweighted) case of unit least squares weights w[k]
            int iwt = 1;
            // mtry = the maximum number of tries that will be made to find a solution for a single value of nlambda
            int mtry = 15;
            // t values are specified rather than calculated from tstart and tend.
            boolean regint = false;
            // Do not assume a zero baseline
            boolean nobase = false;
            // coefficients multiplying exponentials can be negative
            boolean nonneg = false;
            // No Preferences.debug from AlgorithmMultiExponentialFitting
            boolean showDebug = false;
            // Don't output t[] and y[] values
            boolean pry = false;
            // Don't output each iteration of preliminary analysis
            boolean prprel = false;
            // Don't output each iteration of final analysis
            boolean prfinl = false;
            // Don't plot fit of final solution
            boolean plotrs = false;
            // Don't print final summary of results a second time
            boolean repeat = false;
            // Number of time intervals
            int nint = 1;
            double tstart[] = new double[1];
            double tend[] = null;
            int nt[] = new int[1];
            double sqrtw[] = null;
            for (i = start; i < end; i++) {
                // fireProgressStateChanged(i * 100/volSize);
                if (wholeImage || bitMask.get(i)) {
                    input(y_array, i);
                    switch(functionFit) {
                        case LINEAR_FIT:
                            if (findInitialFromData) {
                                // a0 + a1 * timeVals[tDim-1] = y_array[tDim-1]
                                // a0 + a1 * timeVals[0] = y_array[0]
                                // a1 * (timeVals[tDim-1] - timeVals[0]) = (y_array[tDim-1] - y_array[0])
                                initial[1] = (y_array[tDim-1]- y_array[0])/(timeVals[tDim-1] - timeVals[0]);
                                initial[0] = y_array[0] - initial[1] * timeVals[0];
                            }
                            lineModel = new FitLine(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            lineModel.driver();
                            params = lineModel.getParameters();
                            chi_squared = lineModel.getChiSquared();
                            status = lineModel.getExitStatus();
                            break;
                        case EXPONENTIAL_FIT:
                            if (findInitialFromData) {
                                // a0 + a1 * exp(a2 * timeVals[tDim-1]) = y_array[tDim-1]
                                // Let timeVals[tDim-1] - timeVals[0] = delt
                                // Find tMid such that timesVals[tMid] - timeVals[0] is closest to delt/2.
                                // a0 + a1 * exp(a2 * timeVals[tMid]) = y_array[tMid]
                                // a0 + a1 * exp(a2 * timeVals[0]) = y_array[0]
                                // a1*(exp(a2 * timeVals[tDim-1]) - exp(a2 * timeVals[0]) = (y_array[tDim-1] - y_array[0])
                                // a1*(exp(a2 * timeVals[tMid]) - exp(a2 * timeVals[0]) = (y_array[tMid] - y_array[0])
                                //
                                // ((exp(a2 * timeVals[tDim-1]) - exp(a2 * timeVals[0]))/(exp(a2 * timeVals[tMid]) - exp(a2 * timeVals[0])) =
                                // (y_array[tDim-1] - y_array[0])/(y_array[tMid] - y_array[0]) = m
                                
                                // Dividing numerator and denominator by exp(a2 * timeVals[0]):
                                // ((exp(a2 * delt) - 1)/(exp(0.5 * a2 * delt) - 1) = m
                                // exp(a2 * delt) - 1 = m*exp(0.5 * a2 * delt)  - m
                                // exp(a2 * delt) - m*exp(0.5 * a2 * delt) + (m - 1) = 0
                                // exp(0.5 * a2 * delt) = (m +- sqrt(m*m - 4*(m-1))/2 = m-1, 1
                                // m - 1 is the correct answer
                                // exp(a2 * delt) = (m-1)**2
                                // a2 = ln((m-1)**2)/delt
                                delt = timeVals[tDim-1] - timeVals[0];
                                minDiff = Double.MAX_VALUE;
                                tMid = tDim/2;
                                for (j = 0; j < tDim-1; j++) {
                                    diff = Math.abs(timeVals[j] - timeVals[0] - delt/2.0);
                                    if (diff < minDiff) {
                                        minDiff = diff;
                                        tMid = j;
                                    }
                                } // for (j = 0; j < tDim-1; j++)
                                m = (y_array[tDim-1] - y_array[0])/(y_array[tMid] - y_array[0]);
                                diff = m - 1.0;
                                initial[2] = Math.log(diff*diff)/delt;
                                initial[1] = (y_array[tDim-1] - y_array[0])/
                                        (Math.exp(initial[2] * timeVals[tDim-1]) - Math.exp(initial[2] * timeVals[0]));
                                initial[0] = y_array[tDim-1] - initial[1] - Math.exp(initial[2] * timeVals[tDim-1]);
                            } // if (findInitialFromData)
                            expModel = new FitExponential(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            expModel.driver();
                            params = expModel.getParameters();
                            chi_squared = expModel.getChiSquared();
                            status = expModel.getExitStatus();
                            break;
                        case GAUSSIAN_FIT:
                            if (findInitialFromData) {
                                // Gaussian (a0*exp(-(t-a1)^2/(2*(a2)^2))
                                maxVal = 0.0;
                                tMid = tDim/2;
                                for (j = 0; j < tDim; j++) {
                                    val = Math.abs(y_array[j]);
                                    if (val > maxVal) {
                                        maxVal = val;
                                        tMid = j;
                                    }
                                } // for (j = 0; j < tDim; j++)
                                initial[1] = timeVals[tMid];
                                initial[0] = y_array[tMid];
                                beenHalved = false;
                                for (j = tMid -1; (j >= 0) && (!beenHalved); j--) {
                                    if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                        beenHalved = true;
                                        tHalved = j;
                                    }
                                }
                                if (!beenHalved) {
                                    for (j = tMid + 1; (j <= tDim-1) && (!beenHalved); j++) {
                                        if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                            beenHalved = true;
                                            tHalved = j;
                                        }
                                    }
                                } // if (!beenHalved)
                                if (!beenHalved) {
                                    if (y_array[0] < y_array[tDim-1]) {
                                        tHalved = 0;
                                    }
                                    else {
                                        tHalved = tDim-1;
                                    }
                                } // if (!beenHalved)
                                initial[2] = Math.abs((timeVals[tHalved] - timeVals[tMid]) * 
                                        Math.sqrt(0.5 / Math.log(initial[0]/y_array[tHalved])));     
                            } // if (findInitialFromData)
                            gaussianModel = new FitGaussian(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            gaussianModel.driver();
                            params = gaussianModel.getParameters();
                            chi_squared = gaussianModel.getChiSquared();
                            status = gaussianModel.getExitStatus();
                            break;
                        case LAPLACE_FIT:
                            if (findInitialFromData) {
                                // Laplace (a0*exp(-|t-a1|/a2))
                                maxVal = 0.0;
                                tMid = tDim/2;
                                for (j = 0; j < tDim; j++) {
                                    val = Math.abs(y_array[j]);
                                    if (val > maxVal) {
                                        maxVal = val;
                                        tMid = j;
                                    }
                                } // for (j = 0; j < tDim; j++)
                                initial[1] = timeVals[tMid];
                                initial[0] = y_array[tMid];
                                beenHalved = false;
                                for (j = tMid -1; (j >= 0) && (!beenHalved); j--) {
                                    if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                        beenHalved = true;
                                        tHalved = j;
                                    }
                                }
                                if (!beenHalved) {
                                    for (j = tMid + 1; (j <= tDim-1) && (!beenHalved); j++) {
                                        if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                            beenHalved = true;
                                            tHalved = j;
                                        }
                                    }
                                } // if (!beenHalved)
                                if (!beenHalved) {
                                    if (y_array[0] < y_array[tDim-1]) {
                                        tHalved = 0;
                                    }
                                    else {
                                        tHalved = tDim-1;
                                    }
                                } // if (!beenHalved)
                                initial[2] = Math.abs(timeVals[tHalved] - timeVals[tMid]) / Math.log(initial[0]/y_array[tHalved]);      
                            } // if (findInitialFromData)
                            laplaceModel = new FitLaplace(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            laplaceModel.driver();
                            params = laplaceModel.getParameters();
                            chi_squared = laplaceModel.getChiSquared();
                            status = laplaceModel.getExitStatus();
                            break;
                        case LORENTZ_FIT:
                            if (findInitialFromData) {
                                // Lorentz (a0/((t-a1)*(t-a1) + a2*a2)
                                maxVal = 0.0;
                                tMid = tDim/2;
                                for (j = 0; j < tDim; j++) {
                                    val = Math.abs(y_array[j]);
                                    if (val > maxVal) {
                                        maxVal = val;
                                        tMid = j;
                                    }
                                } // for (j = 0; j < tDim; j++)
                                initial[1] = timeVals[tMid];
                                // a0/a2**2 = y_array[tMid]
                                beenHalved = false;
                                for (j = tMid -1; (j >= 0) && (!beenHalved); j--) {
                                    if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                        beenHalved = true;
                                        tHalved = j;
                                    }
                                }
                                if (!beenHalved) {
                                    for (j = tMid + 1; (j <= tDim-1) && (!beenHalved); j++) {
                                        if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                            beenHalved = true;
                                            tHalved = j;
                                        }
                                    }
                                } // if (!beenHalved)
                                if (!beenHalved) {
                                    if (y_array[0] < y_array[tDim-1]) {
                                        tHalved = 0;
                                    }
                                    else {
                                        tHalved = tDim-1;
                                    }
                                } // if (!beenHalved)
                                delt = (timeVals[tHalved] - initial[1]);
                                deltSquared = delt * delt;
                                // a0/(deltSquared + a2**2) = y_array[tHalved]
                                // (deltSquared + a2**2)/(a2**2) = y_array[tMid]/y_array[tHalved] = m
                                m = y_array[tMid]/y_array[tHalved];
                                // a2 = sqrt(deltSquared/(m-1))
                                a2Squared = deltSquared/(m - 1.0);
                                initial[2] = Math.sqrt(a2Squared);
                                // a0 = (a2**2) * y_array[tMid]
                                initial[0] = a2Squared * y_array[tMid];
                            } // if (findInitialFromData)
                            lorentzModel = new FitLorentz(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            lorentzModel.driver();
                            params = lorentzModel.getParameters();
                            chi_squared = lorentzModel.getChiSquared();
                            status = lorentzModel.getExitStatus();
                            break;
                        case MULTIEXPONENTIAL_FIT:
                            if (findInitialFromData) {
                                multiExponentialAlg =  new AlgorithmMultiExponentialFitting(numExponentials, iwt, mtry, regint,
                                        nobase, nonneg, showDebug, pry, prprel, prfinl, plotrs, repeat, tDim, timeVals, nint,
                                        tstart, tend, nt, y_array, sqrtw);   
                                multiExponentialAlg.run();
                                params = multiExponentialAlg.getParameters();
                            }
                            else {
                                multiExponentialModel = new FitMultiExponential(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                                multiExponentialModel.driver();
                                params = multiExponentialModel.getParameters();
                                chi_squared = multiExponentialModel.getChiSquared();
                                status = multiExponentialModel.getExitStatus();
                            }
                            break;
                        case RAYLEIGH_FIT:
                            if (findInitialFromData) {
                                // Rayleigh Distribution a0 *(t-a1)*exp(-(t-a1)*(t-a1)/a2)*u(t-a1) 
                                // Derivative with respect to t = a0 * exp(-(t-a1)*(t-a1)/a2) * (1 - 2*(t-a1)*(t-a1)/a2)
                                // Maximum where derivative = 0 or maximum at t = a1 + sqrt(a2/2).
                                // Here ymax = a0 * sqrt(a2/2) * exp(-0.5)
                                // At th = a1 + sqrt(a2), yh = a0 * sqrt(a2) * exp(-1)
                                // ymax/yh = sqrt(1/2) * exp(0.5) = 0.7071067811865475 * 1.6487 = 1.1658
                                // yh/ymax = 0.85777
                                // In general at t = a1 + m*sqrt(a2), for m > sqrt(1/2), y/ymax = sqrt(2) * exp(0.5) * m * exp(-m**2)
                                maxVal = 0.0;
                                tMid = tDim/2;
                                for (j = 0; j < tDim; j++) {
                                    val = Math.abs(y_array[j]);
                                    if (val > maxVal) {
                                        maxVal = val;
                                        tMid = j;
                                    }
                                } // for (j = 0; j < tDim; j++)
                                beenHalved = false;
                                for (j = tMid + 1; (j <= tDim-1) && (!beenHalved); j++) {
                                    if (Math.abs(y_array[j]) <= 0.5*Math.abs(y_array[tMid])) {
                                        beenHalved = true;
                                        tHalved = j;
                                    }
                                }
                                if (!beenHalved) { 
                                    tHalved = tDim-1;
                                } // if (!beenHalved)
                                ratio = y_array[tHalved]/(y_array[tMid] * sqrt2 * exppoint5);
                                // ratio = m * exp(-m**2)
                                mlower = sqrtpoint5;
                                mupper = 5.0;
                                m = (mlower + mupper)/2.0;
                                diff = mupper - mlower;
                                while (Math.abs(diff) > 1.0E-5) {
                                    m = (mlower + mupper)/2.0;
                                    diff = ratio - (m * Math.exp(-m*m));
                                    if (diff > 0.0) {
                                         mupper = m;    
                                    }
                                    else if (diff < 0.0) {
                                        mlower = m;
                                    }
                                } // while (diff > 1.0E-5)
                                // timeVals[tHalved] - timeVals[tMid] = a1 + m * sqrt(a2) - (a1 + sqrt(0.5)*sqrt(a2)) = 
                                //                                    (m - sqrt(0.5)) * sqrt(a2)
                                sqrta2 = (timeVals[tHalved] - timeVals[tMid])/(m - sqrtpoint5);
                                initial[2] = sqrta2 * sqrta2;
                                initial[0] = y_array[tMid]/(sqrta2 * sqrtpoint5 * expminuspoint5);
                                initial[1] = timeVals[tHalved] - m * sqrta2;
                            } // if (findInitialFromData)
                            rayleighModel = new FitRayleigh(tDim, y_array, initial, useBounds, lowBounds, highBounds);
                            rayleighModel.driver();
                            params = rayleighModel.getParameters();
                            chi_squared = rayleighModel.getChiSquared();
                            status = rayleighModel.getExitStatus();
                            break;
                    } // switch(functionFit)
                    
                    output(params, i, status, chi_squared);
                } // if (bitMask.get(i)
                else {
                    output(null, -1, 0, 0.0);
                }
            } // for (i = start; i < end; i++)
        }

    } // class sm2Task implements Runnable
    
    public synchronized void input(final double y_array[], final int i) {
        int t;
        for (t = 0; t < tDim; t++) {
            y_array[t] = srcArray[t * volSize + i];
        }
    }

    public synchronized void output(final double params[], final int i, final int status, final double chi_squared) {
        int j;
        voxelsProcessed++;
        final long vt100 = voxelsProcessed * 100L;
        barMarker = (int) (vt100 / volSize);
        if (barMarker > oldBarMarker) {
            oldBarMarker = barMarker;
            fireProgressStateChanged(barMarker);
        }
        if (i >= 0) {
            for (j = 0; j < numVariables; j++) {
                destArray[j*volSize +i] = params[j];
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
            if (status > 0) {
                validVoxels++;
                for (j = 0; j < numVariables; j++) {
                    paramTotal[j] += params[j];
                }
            } else {
                bitMask.clear(i);
            }
            destArray[(numVariables * volSize) + i] = chi_squared;
            destExitStatusArray[i] = status;
            exitStatus[status + 11]++;
        } // if (i >= 0)
    }
    
    class FitLine extends NLConstrainedEngine {
        final double ydata[];
        
        public FitLine(final int tDim, final double ydata[], final double initial[], final boolean useBounds[], final double lowBounds[],
                       final double highBounds[]) {
            // tDim data points, 2 coefficients, and linear fitting
            super(tDim, 2);
            this.ydata = ydata;
            
            if ((useBounds[0] == false) && (useBounds[1] == false)) {
                bounds = 0; 
            }
            else {
                bounds = 2;
                bl = new double[2];
                bu = new double[2];
                for (int i = 0; i < 2; i++) {
                    if (!useBounds[i]) {
                        bl[i] = -Double.MAX_VALUE;
                        bu[i] = Double.MAX_VALUE;
                    }
                    else {
                        bl[i] = lowBounds[i];
                        bu[i] = highBounds[i];
                    }
                }
            }

            // bounds = 0 means unconstrained
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
        }
        
        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying line fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitLine ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /** 
         * Fit line to function.
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
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + "\n", Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                        ymod = a[1] * timeVals[j] + a[0];
                        residuals[j] = ymod - ydata[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                        covarMat[j][0] = 1; // a0 partial derivative
                        covarMat[j][1] = timeVals[j]; // a1 partial derivative
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
    
    class FitExponential extends NLConstrainedEngine {
        final double ydata[];
        
        public FitExponential(final int tDim, final double ydata[], final double initial[], final boolean useBounds[],
                              final double lowBounds[], final double highBounds[]) {
         // tDim data points, 3 coefficients, and exponential fitting
            super(tDim, 3);
            this.ydata = ydata;
            
            if ((useBounds[0] == false) && (useBounds[1] == false) && (useBounds[2] == false)) {
                bounds = 0; 
            }
            else {
                bounds = 2;
                bl = new double[3];
                bu = new double[3];
                for (int i = 0; i < 3; i++) {
                    if (!useBounds[i]) {
                        bl[i] = -Double.MAX_VALUE;
                        bu[i] = Double.MAX_VALUE;
                    }
                    else {
                        bl[i] = lowBounds[i];
                        bu[i] = highBounds[i];
                    }
                }
            }
            
            // bounds = 0 means unconstrained
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
            Preferences.debug(" ******* FitExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /** 
         * Fit to function - a0 + a1*exp(a2*x).
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
                        ymod = a[0] + (a[1] * Math.exp(a[2] * timeVals[j]));
                        residuals[j] = ymod - ydata[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                        covarMat[j][0] = 1; // a0 partial derivative
                        covarMat[j][1] = Math.exp(a[2] * timeVals[j]); // a1 partial derivative
                        covarMat[j][2] = a[1] * timeVals[j] * Math.exp(a[2] * timeVals[j]); // a2 partial derivative
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
    
    class FitGaussian extends NLConstrainedEngine {
        final double ydata[];
        
        public FitGaussian(final int tDim, final double ydata[], final double initial[], final boolean useBounds[],
                           final double lowBounds[], final double highBounds[]) {
            // tDim data points, 3 coefficients, and Gaussian fitting
            super(tDim, 3);
            this.ydata = ydata;
            
            bounds = 2;
            bl = new double[3];
            bu = new double[3];
            for (int i = 0; i < 3; i++) {
                if (!useBounds[i]) {
                    bl[i] = -Double.MAX_VALUE;
                    bu[i] = Double.MAX_VALUE;
                }
                else {
                    bl[i] = lowBounds[i];
                    bu[i] = highBounds[i];
                }
                if (i == 2) {
                    // a2 must be positive
                    bl[2] = Math.max(bl[2], Double.MIN_VALUE);
                }
            }
            
            // bounds = 0 means unconstrained
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
         * Display results of displaying Gaussian fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitGaussian ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /** 
         * Fit to function - a0*exp(-(t-a1)^2/(2*(a2)^2))
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j;
            double ymod = 0;
            double diff;
            double diffSq;
            double a2Sq;

            try {
                ctrl = ctrlMat[0];
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n", 
                // Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                        diff = timeVals[j] - a[1];
                        ymod = a[0] * Math.exp(-(diff*diff)/(2.0*a[2]*a[2]));
                        residuals[j] = ymod - ydata[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                        diff = timeVals[j] - a[1];
                        diffSq = diff * diff;
                        a2Sq = a[2] * a[2];
                        covarMat[j][0] = Math.exp(-(diffSq)/(2.0*a2Sq)); // a0 partial derivative
                        covarMat[j][1] = (a[0] * diff * covarMat[j][0])/a2Sq; // a1 partial derivative
                        covarMat[j][2] = (a[0] * diffSq * covarMat[j][0])/(a2Sq * a[2]); // a2 partial derivative
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
    
    class FitLaplace extends NLConstrainedEngine {
        final double ydata[];
        
        public FitLaplace(final int tDim, final double ydata[], final double intial[], final boolean useBounds[],
                          final double lowBounds[], final double highBounds[]) {
            // tDim data points, 3 coefficients, and Laplace fitting
            super(tDim, 3);
            this.ydata = ydata; 
            
            bounds = 2;
            bl = new double[3];
            bu = new double[3];
            for (int i = 0; i < 3; i++) {
                if (!useBounds[i]) {
                    bl[i] = -Double.MAX_VALUE;
                    bu[i] = Double.MAX_VALUE;
                }
                else {
                    bl[i] = lowBounds[i];
                    bu[i] = highBounds[i];
                }
                
                if (i == 2) {
                    // a2 must be positive
                    bl[2] = Math.max(bl[2], Double.MIN_VALUE);
                }
            }
            
            // bounds = 0 means unconstrained
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
         * Display results of displaying Laplace fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitLaplace ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /** 
         * Fit to function - a0*exp(-|t-a1|/a2)
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j;
            double ymod = 0;
            double diff;

            try {
                ctrl = ctrlMat[0];
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n", 
                // Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                        diff = timeVals[j] - a[1];
                        ymod = a[0] * Math.exp(-Math.abs(diff)/a[2]);
                        residuals[j] = ymod - ydata[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                        diff = timeVals[j] - a[1];
                        covarMat[j][0] = Math.exp(-Math.abs(diff)/a[2]); // a0 partial derivative
                        covarMat[j][1] = -(a[0] * Math.signum(-diff) * covarMat[j][0])/a[2]; // a1 partial derivative
                        covarMat[j][2] = (a[0] * Math.abs(diff) * covarMat[j][0])/(a[2] * a[2]); // a2 partial derivative
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
    
    class FitLorentz extends NLConstrainedEngine {
        final double ydata[];
        
        public FitLorentz(final int tDim, final double ydata[], final double initial[], final boolean useBounds[],
                          final double lowBounds[], final double highBounds[]) {
            // tDim data points, 3 coefficients, and Lorentz fitting
            super(tDim, 3);
            this.ydata = ydata;
            
            bounds = 2; 
            // bounds = 0 means unconstrained
            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            bl = new double[3];
            bu = new double[3];
            for (int i = 0; i < 3; i++) {
                if (!useBounds[i]) {
                    bl[i] = -Double.MAX_VALUE;
                    bu[i] = Double.MAX_VALUE;
                }
                else {
                    bl[i] = lowBounds[i];
                    bu[i] = highBounds[i];
                }
                
                if (i == 2) {
                    // a2, half-width at half-maximum must be positive
                    bl[2] = Math.max(bl[2], Double.MIN_VALUE);
                }
            }
            
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
         * Display results of displaying Lorentz fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitLorentz ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /** 
         * Fit to function - a0/((t-a1)*(t-a1) + a2*a2)
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j;
            double ymod = 0;
            double diff;
            double diffSq;
            double a2Sq;
            double num;
            double denom;

            try {
                ctrl = ctrlMat[0];
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n", 
                // Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                        diff = timeVals[j] - a[1];
                        diffSq = diff * diff;
                        a2Sq = a[2]*a[2];
                        ymod = a[0]/(diffSq + a2Sq);
                        residuals[j] = ymod - ydata[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                        diff = timeVals[j] - a[1];
                        diffSq = diff * diff;
                        a2Sq = a[2]*a[2];
                        covarMat[j][0] = 1.0/(diffSq + a2Sq); // a0 partial derivative
                        num = 2.0 * a[0] * diff;
                        denom = a2Sq + diffSq;
                        covarMat[j][1] = num/(denom * denom); // a1 partial derivative
                        num = -2.0 * a[0] * a[2];
                        covarMat[j][2] = num/(denom * denom); // a2 partial derivative
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
    
    
    class FitMultiExponential extends NLConstrainedEngine {
        final double ydata[];
        
        public FitMultiExponential(final int tDim, final double ydata[], final double initial[], final boolean useBounds[],
                                   final double lowBounds[], final double highBounds[]) {
            // tDim data points, numVariable coefficients, and multiexponential fitting
            super(tDim, numVariables);
            this.ydata = ydata;
            
            bounds = 2; 
            // bounds = 0 means unconstrained
            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            bl = new double[numVariables];
            bu = new double[numVariables];
            
            if (useBounds[0]) {
                bl[0] = lowBounds[0];
                bu[0] = highBounds[0];
            }
            else {
                bl[0] = -Double.MAX_VALUE;
                bu[0] = Double.MAX_VALUE;
            }
            // All exponentials are decaying negative exponentials by default
            for (int k = 0; k < (numVariables - 1)/2; k++) {
                if (useBounds[2*k+1]) {
                    bl[2*k+1] = lowBounds[2*k+1];
                    bu[2*k+1] = highBounds[2*k+1];
                }
                else {
                    bl[2*k+1] = -Double.MAX_VALUE;
                    bu[2*k+1] = Double.MAX_VALUE;
                }
                if (useBounds[2*k+2]) {
                    bl[2*k+2] = lowBounds[2*k+2];
                    bu[2*k+2] = highBounds[2*k+2];
                }
                else {
                    bl[2*k+2] = -Double.MAX_VALUE;
                    bu[2*k+2] = 0.0;
                }
            }
            
            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;
            
            for (int i = 0; i < numVariables; i++) {
                gues[i] = initial[i];
            }

            
        }
        
        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying multiExponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitMultiExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            for (int k = 0; k < (numVariables - 1)/2; k++) {
                Preferences.debug("a" + (2*k+1) + " " + String.valueOf(a[2*k+1]) + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("a" + (2*k+2) + " " + String.valueOf(a[2*k+2]) + "\n", Preferences.DEBUG_ALGORITHM);
            }
        }
        
        /** 
         * Fit to function - a0 + a1*exp(a2*t) + a3*exp(a4*t) + ...
         * where all the exponentials are negative decaying exponentials.
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j, k;
            double ymod = 0;

            try {
                ctrl = ctrlMat[0];
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0]  + "\n", 
                // Preferences.DEBUG_ALGORITHM);
                // for (k = 1; k < a.length; k++) {
                //    Preferences.debug("a[k] = " + a[k] + "\n", Preferences.DEBUG_ALGORITHM);
                // }
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                        ymod = a[0];
                        for (k = 0; k < (a.length - 1)/2; k++) {
                            ymod += (a[2*k+1] * Math.exp(a[2*k+2] * timeVals[j]));
                        }
                        residuals[j] = ymod - ydata[j];
                        //Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                        covarMat[j][0] = 1; // a0 partial derivative
                        for (k = 0; k < (a.length - 1)/2; k++) {
                            covarMat[j][2*k+1] = Math.exp(a[2*k+2] * timeVals[j]); // a[2*k+1] partial derivative
                            covarMat[j][2*k+2] = a[2*k+1] * timeVals[j] * Math.exp(a[2*k+2] * timeVals[j]); // a[2*k+2] partial derivative
                        }
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
    
    class FitRayleigh extends NLConstrainedEngine {
        final double ydata[];
        
        public FitRayleigh(final int tDim, final double ydata[], final double initial[], final boolean useBounds[],
                           final double lowBounds[], final double highBounds[]) {
            // tDim data points, 3 coefficients, and Rayleigh fitting
            super(tDim, 3);
            this.ydata = ydata;
            
            bounds = 2; 
            // bounds = 0 means unconstrained
            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            bl = new double[3];
            bu = new double[3];
            for (int i = 0; i < 3; i++) {
                if (!useBounds[i]) {
                    bl[i] = -Double.MAX_VALUE;
                    bu[i] = Double.MAX_VALUE;
                }
                else {
                    bl[i] = lowBounds[i];
                    bu[i] = highBounds[i];
                }
                
                if (i == 2) {
                    // a2 must be positive
                    bl[2] = Math.max(bl[2], Double.MIN_VALUE);
                }
            }
            
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
         * Display results of displaying Rayleigh fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitRayleigh ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /** 
         * Fit to function - a0 * (t-a1)*exp(-(t-a1)*(t-a1)/a2)*u(t - a1) with a2 > 0
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j;
            double ymod = 0;
            double diff;
            double expon;
            double diffSquared;

            try {
                ctrl = ctrlMat[0];
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                        if (timeVals[j] >= a[1]) {
                            diff = timeVals[j] - a[1];
                            ymod = a[0]*diff*Math.exp(-diff*diff/a[2]); 
                        }
                        else {
                            ymod = 0.0;
                        }
                        residuals[j] = ymod - ydata[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                        if (timeVals[j] >= a[1]) {
                            diff = timeVals[j] - a[1];
                            diffSquared = diff * diff;
                            expon = Math.exp(-diffSquared/a[2]);
                            covarMat[j][0] = diff * expon;
                            covarMat[j][1] = a[0]*expon*((2.0*diffSquared/a[2]) - 1.0);
                            covarMat[j][2] = a[0]*((diff * diffSquared)/(a[2]*a[2]))*expon;
                        }
                        else {
                            covarMat[j][0] = 0.0;
                            covarMat[j][1] = 0.0;
                            covarMat[j][2] = 0.0;
                        }
                    }
                }
                // Calculate the Jacobian numerically
                 else if (ctrl == 2) {
                 ctrlMat[0] = 0;
                 }
            } catch (final Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
        
        
        
    }

    

}
