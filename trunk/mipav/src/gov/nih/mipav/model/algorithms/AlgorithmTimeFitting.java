package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.util.DoubleDouble;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.*;
import java.util.BitSet;
import java.util.concurrent.*;

import de.jtem.numericalMethods.calculus.function.RealFunctionOfOneVariable;
import de.jtem.numericalMethods.calculus.integration.RungeKuttaFehlbergIntegrator;



public class AlgorithmTimeFitting extends AlgorithmBase {
    private static final int LINEAR_FIT = 0;
    
    private static final int EXPONENTIAL_FIT = 1;
    
    private static final int GAUSSIAN_FIT = 2;
    
    private static final int LAPLACE_FIT = 3;
    
    private static final int LORENTZ_FIT = 4;
    
    private static final int MULTIEXPONENTIAL_FIT = 5;
    
    private static final int RAYLEIGH_FIT = 6;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** A vector of center times for each volume */
    private double timeVals[] = null;

    private int i;

    private int xDim;

    private int yDim;

    private int zDim;

    private int tDim;

    private int volSize;

    private double initial[];

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
    
    private double srcArray[];

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmTimeFitting object.
     * 
     */
    public AlgorithmTimeFitting(final ModelImage destImage, final ModelImage srcImage, final ModelImage exitStatusImage, final boolean useLog,
            final int functionFit, final int numVariables) {

        super(destImage, srcImage);
        this.exitStatusImage = exitStatusImage;
        this.useLog = useLog;
        this.functionFit = functionFit;
        this.numVariables =  numVariables;
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
        ViewVOIVector VOIs;
        short mask[];
        int t;
        double y_array[];
        int srcSize4D;
        int destSize4D;
        FitLine lineModel;
        double[] params;
        double chi_squared;
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
        

        processors = Runtime.getRuntime().availableProcessors();
        Preferences.debug("Available processors = " + processors + "\n", Preferences.DEBUG_ALGORITHM);

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        tDim = srcImage.getExtents()[3];
        volSize = xDim * yDim * zDim;
        srcSize4D = volSize * tDim;
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
        
        initial = new double[numVariables];
        switch(functionFit) {
            case LINEAR_FIT:
                initial[0] = 0; // y intercept
                initial[1] = -1; // slope
                break;    
        }
        
        for (i = 0; i < numVariables; i++) {
            paramMin[i] = Double.MAX_VALUE;
            paramMax[i] = -Double.MAX_VALUE;
        }
        
        //if (processors > 1) {
            
        //} else { // processors == 1
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
                    switch(functionFit) {
                        case LINEAR_FIT:
                            lineModel = new FitLine(y_array, initial);
                            lineModel.driver();
                            params = lineModel.getParameters();
                            chi_squared = lineModel.getChiSquared();
                            for (j = 0; j < numVariables; j++) {
                                destArray[j*volSize +i] = params[j];
                            }
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
                            if (lineModel.getExitStatus() > 0) {
                                validVoxels++;
                                for (j = 0; j < numVariables; j++) {
                                    paramTotal[j] += params[j];
                                }
                            } else if (lineModel.getExitStatus() <= 0) {
                                bitMask.clear(i);
                            }
                            destArray[(numVariables * volSize) + i] = chi_squared;
                            destExitStatusArray[i] = lineModel.getExitStatus();
                            exitStatus[ (lineModel.getExitStatus() + 11)]++;
                            break;
                    }
                } // if (wholeImage || bitMask.get(i))
            } // for (i = 0; i < volSize; i++)
        //} // else processors == 1
        
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

    
    
    class FitLine extends NLConstrainedEngine {
        final double ydata[];
        
        public FitLine(final double ydata[], final double intial[]) {
         // tDim data points, 2 coefficients, and linear fitting
            super(tDim, 2);
            this.ydata = ydata;
            
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

    

}
