package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.AlgorithmFRAP.IntModel;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.DoubleDouble;

import gov.nih.mipav.view.*;

import java.util.BitSet;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import java.awt.*;

import java.io.*;

import java.text.*;


/**
 * Based on the document provided by Daniel Reich:
 * Notes on DCE with SM2 (standard model, aka Tofts model, 2-compartment)
 * 3 model parameters are fit for each voxel in 3D:
 * 1) K_trans in [1.0E-5, 5.0] in /min
 * On input ktrans is converted from /min to /sec and on output ktrans is converted
 * from /sec to /min.
 * 2) ve in [1.0E-5, 0.99]
 * 3) f_vp in [0, 0.99]
 
 * srcImage is a dynamic "4D volume" of MRI signal (3D over time).
 * The equation used here is virtually identical to equation 10.3 in T1-w DCE-MRI.
 * Ct(t) = vpCp(t) + ktrans*integral from 0 to t of 
 * Cp(t') * exp[-ktrans*(t - t')/ve]dt' (10.3)
 * Cp is the concentration of agent in the vascular plasma space, vp.
 * ve is the volume of the extravascular extracellular space per unit volume of tissue.
 * ktrans is the volume transfer constant between vp and ve.
 * Ct is the tracer concentration in the tissue as a whole.
 * Our equation is:
 * R1(t) - R10 = (vp * [R1,p(t) - R10,p] + ktrans*integral from 0 to t of
 * [R1,p(tau) - R10,p]*exp[-ktrans*(t - tau)/ve]d(tau))/(1 - h)
 * where R10 is the tissue R1 map in the absence of contrast material, a 3D volume
 * R1(tj) is the tissue R1 map during contrast infusion, a 4D data set.
 * h = hematocrit with a default value of 0.4.
 * With the sagittal sinus VOI:
 * Average over this VOI to extract the constant R10,p from the R10 map.
 * Average over this VOI to extract the time series R1,p(tj) from the
 * R1(tj) 4D data set.
 * tj is a vector of the center times for each volume, where j = 1,...,n and 
 * n is the number of post-contrast volumes.
 * 
 * Solve the equation numerically assuming piecewise linearity and using the
 * trapezoid rule.
 * 
The pre-injection volumes are actually averaged together to generate the R10 map. So you're right, they wouldn't provide any additional information,
 and we don't need to fit them.
After injection (t=t1), the first volume usually has no contrast in it. That's because the contrast has to circulate to the brain. 
So R1,p(t1) should be the same as R10,p, but in our scheme R1,p(t1) wasn't used to generate the R10 map. Any differences should be due to noise.
 Does that help explain things?
 
For monomeric gadolinium chelates, Ktrans for the normal blood brain barrier is approximately 10^-4 min^-1 
(from the legend to Fig 5 of Li, Rooney, and Springer (MRM, 2005)). So a lower bound of 10^-5 should be ok.
If R1,p(t1) = R10,p except for noise, then R1,p(t1) - R10,p is purely noise and contains no useful information
 
 
 
 References:
 1.) "A Unified Magnetic Resonance Imaging Pharmacokinetic Theory: Intravascular and Extracellular Contrast
 Reagents" by Xin Li, William D. Rooney, and Charles S. Springer, Jr., Magnetic Resonance in Medicine,
 Vol. 54, 2005, pp. 1351-1359.
 2.) Erratum: Magnetic Resonance in Medicine, Vol. 55, 2006, p.1217.
 3.) Quantitative MRI of the Brain, Edited by Paul Tofts, 2003, John Wiley & Sons, Ltd.,
 ISBN: 0-47084721-2, Chapter 10, T1-w DCE-MRI: T1-weighted Dynamic Contrast-enhanced MRI by
 Geoff J. M. Parker and Anwar R. Padhani, pp. 341-364.
 4.) Larsson HBW, Courivaud F, Rostrup E, Hansen AE.  Measurement of brain perfusion, blood volume, and 
 blood-brain barrier permeability, using dynamic contrast-enhanced T1-weighted MRI at 3 tesla.
 Magnetic Resonance in Medicine 2009; 62(5):1270-1281.
 5.) Li X, Rooney WD, Springer CS.  A unified magnetic resonance imaging pharmacokinetic theory:
 intravascular and extracellular contrast reagents.  Magnetic Resonance in Medicine
 2005 Dec; 54(6): 1351-1359.
 6.) Tofts PS, Modeling tracer kinetics in dynamic Gd-DPTA MR imaging.  Journal of Magnetic
 Resonance Imaging, 1997, 7(1), pp. 91-101.
 7.) Tofts PS, Brix G, Buckley DL, Evelhoch JL, Henderson E, Knopp MV, Larsson HB, Mayr NA,
 Parker GJ, Port RE, Taylor J, Weisskoff RM.  Estimating kinetic parameters from dynamic 
 contrast-enhanced T(1)-weighted MRI of a diffusable tracer: standardized quantitites and
 symbols.  J. Magn. Reson Imaging 1999 Sep; 10(3), pp. 223-232. 
 */
public class AlgorithmSM2 extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
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
    private double h;
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
    private int[] paramNaN = new int[3];
    private int[] paramInf = new int[3];
    private double[] paramMin = new double[3];
    private double[] paramMax = new double[3];
    private int processors;
    private float destArray[][];
    private int destExitStatusArray[];
    private long voxelsProcessed = 0;
    private int barMarker = 0;
    private int oldBarMarker = 0;
    
    private ModelImage tissueImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmDEMRI3 object.
     
     */
    public AlgorithmSM2(ModelImage destImage[], ModelImage srcImage, double min_constr[], double max_constr[],
    		               double initial[], ModelImage tissueImage, double timeVals[], double h) {

        super(null, srcImage);
        this.destImage = destImage;
        this.min_constr = min_constr;
        this.max_constr = max_constr;
        this.initial = initial;
        this.tissueImage = tissueImage;
        this.timeVals = timeVals;
        this.h = h;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
        BitSet mask;
        int t;
        double y_array[];
        int size4D;
        FitSM2ConstrainedModel dModel;
        double[] params;
        float chi_squared;
        int j;
        // If true, run a self test of NLConstrainedEngineEP.java
        boolean nlConstrainedEngineEPTest = false;
        // If true, run a self test of NLConstrainedEngine.java
        boolean nlConstrainedEngineTest = false;
        boolean selfTest = false;
        boolean selfTest2 = false;
        boolean levmarBoxConstraintTest = false;
        boolean ddrvbdTest = false;
        int voiCount;
        double delT;
        long normalTerminations = 0;
        long abnormalTerminations = 0;
        
        if (selfTest) {
        	int i;
            String fileList[] = new String[661];
            String seq;
            String zero = "0";
            String baseString = "dcemri_testversion4_";
            String ext = ".dcm";
            String selectedFileName;
            boolean performSort = false;
            FileIO fileIO;
            ModelImage dceImage;
            double dceData[];
            int sliceSize;
            int dataSize;
            int x;
            int y;
            double ktransArray[] = new double[]{0.0, 0.01, 0.02, 0.05, 0.1, 0.2};
            double veArray[] = new double[]{0.1, 0.2, 0.5};
            double vpArray[] = new double[]{0.001, 0.005, 0.01, 0.02, 0.05, 0.1};
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
               fileList[i-1] = baseString + seq + ext;
            } // for (i = 1; i <= 661; i++)
            selectedFileName = fileList[0];
            fileIO = new FileIO();
            fileIO.setFileDir("C:" + File.separator + "DCE_MRI" + File.separator + "QIBA_v4" + File.separator +
                    "Dynamic_v4" + File.separator + "DICOM" + File.separator);
            dceImage = fileIO.readDicom(selectedFileName, fileList, performSort);
            FileInfoBase[] fileInfo = dceImage.getFileInfo();
            timeVals = new double[tDim];
            for (t = 0; t < tDim; t++) {
            	timeString = ((String) ((FileInfoDicom) fileInfo[t]).getTagTable().getValue("0008,0032"));
            	firstColonIndex = timeString.indexOf(":", 0);
            	lastColonIndex = timeString.lastIndexOf(":", timeString.length()-1);
            	hourString = timeString.substring(0,firstColonIndex);
            	minuteString = timeString.substring(firstColonIndex+1, lastColonIndex);
            	secondString = timeString.substring(lastColonIndex+1);
            	timeVals[t] = 3600.0 * Double.valueOf(hourString).doubleValue() + 60.0 * Double.valueOf(minuteString).doubleValue()
            	              + Double.valueOf(secondString).doubleValue();
            }
            
            try {
            	dceImage.exportData(0, dataSize, dceData);
            }
            catch(IOException e) {
            	MipavUtil.displayError("IOException on dceImage.exportData(0, dataSize, decData)");
        		setCompleted(false);
        		return;	
            }
            //dceImage.calcMinMax();
            //new ViewJFrameImage(dceImage);
            dceImage.disposeLocal();
            dceImage = null;
            
            r1ptj = new double[tDim];
            r1pt0 = dceData[190*60];
            for (t = 0; t < tDim; t++) {
              r1ptj[t] = dceData[t*sliceSize + 190*60] - r1pt0;	
            }
            
            trapezoidMidpoint = new double[tDim-1];
            for (t = 0; t < tDim - 1; t++) {
            	trapezoidMidpoint[t] = 0.5*(r1ptj[t]+r1ptj[t+1]);
            }
            trapezoidSlope = new double[tDim-1];
            trapezoidConstant = new double[tDim-1];
            for (t = 0; t < tDim - 1; t++) {
            	delT = timeVals[t+1] - timeVals[t];
            	trapezoidSlope[t] = (r1ptj[t+1]-r1ptj[t])/delT;
            	trapezoidConstant[t] = (r1ptj[t]*timeVals[t+1] - r1ptj[t+1]*timeVals[t])/delT;
            }
            
            ymodel = new double[tDim-1];
            exparray = new double[tDim][tDim];
            
            // ktrans takes 6 values along the x axis (0, 0.01, 0.02, 0.05, 0.1, 0.2)
            // Along the y axis ve varies first.  Then vp varies.
            // ve takes 3 values (0.1, 0.2, 0.5)
            // vp takes 6 values (0.01, 0.05, 0.01, 0.02, 0.05, 0.1)
            y_array = new double[tDim-1];
            for (yIndex = 0; yIndex < 18; yIndex++) {
            	veIndex = yIndex % 3;
            	veActual = veArray[veIndex];
            	//initial[1] = veActual;
            	vpIndex = yIndex/3;
            	vpActual = vpArray[vpIndex];
            	//initial[2] = vpActual;
            	y = 5 + 10 * yIndex;
                for (ktransIndex = 0; ktransIndex < 6; ktransIndex++) {
                    ktransActual = ktransArray[ktransIndex];
                    //initial[0] = ktransActual;
                    x = 5 + 10 * ktransIndex;
                    for (t = 1; t < tDim; t++) {
                    	y_array[t-1] = dceData[t*sliceSize + y*xDim + x] - dceData[y*xDim + x];
                    }
                    // Note that the nPts, tDim-1, is the number of points in the y_array.
                    dModel = new FitSM2ConstrainedModel(tDim-1, r1ptj, y_array, initial);
                    dModel.driver();
                    //dModel.dumpResults();
                    params = dModel.getParameters();
                    Preferences.debug("Actual ktrans = " + ktransActual + " Calculated ktrans = " + params[0] + "\n");
                    Preferences.debug("Actual ve = " + veActual + " Calculated ve = " + params[1] + "\n");
                    Preferences.debug("Actual vp = " + vpActual + " Calculated vp = " + params[2] + "\n");
                    Preferences.debug("Number of iterations: " + String.valueOf(dModel.iters) + "\n");
                    Preferences.debug("Chi-squared: " + String.valueOf(dModel.getChiSquared()) + "\n");
                    status = dModel.getExitStatus();
                    statusMessage(status);
                } // for (ktransIndex = 0; ktransIndex < 6; ktransIndex++)
            } // for (yIndex = 0; yIndex < 18; yIndex++)
            
            setCompleted(false);
            return;
        } // if (selfTest)
        
        if (selfTest2) {
        	// If your initial guesses are 0.495, 0.495, 0.495, then the 90 out of 108 
        	// runs where ktrans does not equal zero are correct.  The model cannot 
        	// presently converge to ktrans equals zero in 18 out of the 108 cases.
        	double ktransArray[] = new double[]{0.0, 0.01/60.0, 0.02/60.0, 0.05/60.0, 0.1/60.0, 0.2/60.0};
            double veArray[] = new double[]{0.1, 0.2, 0.5};
            double vpArray[] = new double[]{0.001, 0.005, 0.01, 0.02, 0.05, 0.1};
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
            for (i = 0; i < 100; i++) {
            	timeVals[i] = i;
            }
            r1ptj = new double[tDim];
            for (i = 0; i < 100; i++) {
            	r1ptj[i] = 2.0*i;
            }
            
            trapezoidMidpoint = new double[tDim-1];
            for (t = 0; t < tDim - 1; t++) {
            	trapezoidMidpoint[t] = 0.5*(r1ptj[t]+r1ptj[t+1]);
            }
            trapezoidSlope = new double[tDim-1];
            trapezoidConstant = new double[tDim-1];
            for (t = 0; t < tDim - 1; t++) {
            	delT = timeVals[t+1] - timeVals[t];
            	trapezoidSlope[t] = (r1ptj[t+1]-r1ptj[t])/delT;
            	trapezoidConstant[t] = (r1ptj[t]*timeVals[t+1] - r1ptj[t+1]*timeVals[t])/delT;
            }
            
            ymodel = new double[tDim-1];
            exparray = new double[tDim][tDim];
            
            // ktrans takes 6 values along the x axis (0, 0.01, 0.02, 0.05, 0.1, 0.2)
            // Along the y axis ve varies first.  Then vp varies.
            // ve takes 3 values (0.1, 0.2, 0.5)
            // vp takes 6 values (0.01, 0.05, 0.01, 0.02, 0.05, 0.1)
            y_array = new double[tDim-1];
            for (yIndex = 0; yIndex < 18; yIndex++) {
            	veIndex = yIndex % 3;
            	veActual = veArray[veIndex];
            	//initial[1] = veActual;
            	initial[1] = 0.495;
            	vpIndex = yIndex/3;
            	vpActual = vpArray[vpIndex];
            	//initial[2] = vpActual;
            	initial[2] = 0.495;
                for (ktransIndex = 0; ktransIndex < 6; ktransIndex++) {
                    ktransActual = ktransArray[ktransIndex];
                    Preferences.debug("ktrans = " + ktransActual + " ve = " + veActual + " vp = " + vpActual + "\n");
                    ktransDivVe = ktransActual/veActual;
                	for (j = 0; j <= tDim-1; j++) {
                		for (m = 0; m <= tDim-1; m++) {
                	        exparray[j][m] = Math.exp((timeVals[j] - timeVals[m])*ktransDivVe);
                		}
                	}
                    //initial[0] = ktransActual;
                	initial[0] = 0.495;
                    for (t = 1; t < tDim; t++) {
                    	IntModel imod;
                        int steps;
                        double numInt;
                        double eps = 1.0e-8;
                        double intSum;
                        imod = new IntModel(0.0, timeVals[t], Integration.TRAPZD, eps, ktransActual, veActual);
                        imod.driver();
                        steps = imod.getStepsUsed();
                        numInt = imod.getIntegral();
                        y_array[t-1] = (ktransActual * numInt + vpActual * r1ptj[t])/(1.0 - h);
                        
                    	
                    		/*intSum = 0.0;
                    		for (j = 1; j <= t; j++) {
                    			//intSum += trapezoidMidpoint[j-1]*(exparray[j][t] - exparray[j-1][t])*veActual;
                    			intSum += trapezoidConstant[j-1]*(exparray[j][t] - exparray[j-1][t])*veActual;
                    			intSum += trapezoidSlope[j-1]* ((exparray[j][t]*(timeVals[j] - 1.0/ktransDivVe)) -
                                        (exparray[j-1][t]*(timeVals[j-1] - 1.0/ktransDivVe)))*veActual;
                    		} // for (j = 2; j <= m; j++)
                    		ymodel[t-1] = intSum + vpActual * r1ptj[t];
                        Preferences.debug("t = " + t +  "y_array = " + y_array[t-1] + " ymodel = " + ymodel[t-1] + "\n");*/
                    } // for (t = 1; t < tDim; t++)
                    // Note that the nPts, tDim-1, is the number of points in the y_array.
                    dModel = new FitSM2ConstrainedModel(tDim-1, r1ptj, y_array, initial);
                    dModel.driver();
                    //dModel.dumpResults();
                    params = dModel.getParameters();
                    Preferences.debug("Actual ktrans = " + ktransActual + " Calculated ktrans = " + params[0] + "\n");
                    Preferences.debug("Actual ve = " + veActual + " Calculated ve = " + params[1] + "\n");
                    Preferences.debug("Actual vp = " + vpActual + " Calculated vp = " + params[2] + "\n");
                    Preferences.debug("Number of iterations: " + String.valueOf(dModel.iters) + "\n");
                    Preferences.debug("Chi-squared: " + String.valueOf(dModel.getChiSquared()) + "\n");
                    status = dModel.getExitStatus();
                    statusMessage(status);
                    if ((Math.abs(ktransActual - params[0]) <= 1.0E-7) && (Math.abs(veActual - params[1]) <= 1.0E-5) &&
                            (Math.abs(vpActual - params[2]) <= 1.0E-5)) {
                        success++;
                    }
                    else {
                        failure++;
                        if (status == -6) {
                        	notADescentDirection++;
                        }
                        else if (ktransActual == 0.0) {
                        	ktransequalszero++;
                        }
                    }
                } // for (ktransIndex = 0; ktransIndex < 6; ktransIndex++)
            } // for (yIndex = 0; yIndex < 18; yIndex++)
            Preferences.debug("Number of successes = " + success + "\n");
            Preferences.debug("Number of failures = " + failure + "\n");
            Preferences.debug("Number failing with abnormal termination because the latest search direction computed using subspace minimization\n");
        	Preferences.debug("was not a descent direction (probably caused by a wrongly computed Jacobian) = " + notADescentDirection + "\n");
        	Preferences.debug("Number failing because cannot handle ktrans equals zero = " + ktransequalszero + "\n");
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
        
        if (levmarBoxConstraintTest) {
        	new FitAllBC();
        	setCompleted(false);
        	return;
        }
        
        if (ddrvbdTest) {
        	new FitAllBC2();
        	setCompleted(false);
        	return;	
        }
        
        if (srcImage.getNDims() != 4) {
            MipavUtil.displayError("srcImage must be 4D");
            setCompleted(false);
            return;
        }
        
        processors = Runtime.getRuntime().availableProcessors();
        Preferences.debug("Available processors = " + processors + "\n");
        
        // Convert dialog ktrans units from /min to /sec
        // Multiply /min by 1 min/60 seconds
        initial[0] = initial[0]/60.0;
        min_constr[0] = min_constr[0]/60.0;
        max_constr[0] = max_constr[0]/60.0;
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        tDim = srcImage.getExtents()[3];
        volSize = xDim * yDim * zDim;
        size4D = volSize * tDim;
        destArray = new float[4][volSize];
        destExitStatusArray = new int[volSize];
       
        r1t0 = new double[volSize];
        r1tj = new double[size4D];
        r1ptj = new double[tDim];
        
        
    	try {
    	    tissueImage.exportData(0, volSize, r1t0);
    	}
    	catch(IOException e) {
    		MipavUtil.displayError("IOException on tissueImage.exportData(0, volSize, r1t0)");
    		setCompleted(false);
    		return;
    	}
    	tissueImage.disposeLocal();
    	tissueImage = null;
        
       
        VOIs = srcImage.getVOIs();
        int nVOIs = VOIs.size();
        int nBoundingVOIs = 0;

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
            }
        }
        
        if (nBoundingVOIs == 0) {
            MipavUtil.displayError("No bounding vois around sagittal sinus");
            setCompleted(false);
            return;
        }
        
        if (nBoundingVOIs > 1) {
            MipavUtil.displayError(nBoundingVOIs + " bounding vois around sagittal sinus instead of the expected 1");
            setCompleted(false);
            return;
        }
        
        mask = srcImage.generateVOIMask();
        
        try {
            srcImage.exportData(0, size4D, r1tj);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException on srcImage.exportData(0, size4D, r1tj");
            setCompleted(false);
            return;
        }
        
        voiCount = 0;
        r1pt0 = 0.0;
        // Actually looking from t = t1 to t = tlast
        for (t = 0; t < tDim; t++) {
            for (i = 0; i < volSize; i++) {
                if (mask.get(i)) {
                    r1ptj[t] += r1tj[i + t*volSize];
                    if (t == 0) {
                    	r1pt0 += r1t0[i];
                        voiCount++;	
                    }
                }  
            }
            if (t == 0) {
            	r1pt0 = r1pt0/voiCount;
            }
            r1ptj[t] = r1ptj[t]/voiCount;
        } // for (t = 0; t < tDim; t++)
        
        // Actually looking from t = t1 to t = tlast
        for (t = 0; t < tDim; t++) {
        	// For t = t1, looking at R1,p(t1) - R10,p, but R1,p(t1) and R10,p are identical except for noise
        	r1ptj[t] = r1ptj[t] - r1pt0;
        	for (i = 0; i < volSize; i++) {
        	    r1tj[i + t*volSize] = r1tj[i + t*volSize] - r1t0[i];	
        	}
        } // for (t = 0; t < tDim; t++)
        r1t0 = null;
        
        trapezoidMidpoint = new double[tDim-1];
        for (t = 0; t < tDim - 1; t++) {
        	trapezoidMidpoint[t] = 0.5*(r1ptj[t]+r1ptj[t+1]);
        }
        trapezoidSlope = new double[tDim-1];
        trapezoidConstant = new double[tDim-1];
        for (t = 0; t < tDim - 1; t++) {
        	delT = timeVals[t+1] - timeVals[t];
        	trapezoidSlope[t] = (r1ptj[t+1]-r1ptj[t])/delT;
        	trapezoidConstant[t] = (r1ptj[t]*timeVals[t+1] - r1ptj[t+1]*timeVals[t])/delT;
        }
        
        y_array = new double[tDim-1];
        ymodel = new double[tDim-1];
        exparray = new double[tDim][tDim];
        exitStatus = new int[12356];
        
        for (i = 0; i < 3; i++) {
        	paramMin[i] = Double.MAX_VALUE;
        	paramMax[i] = -Double.MAX_VALUE;
        }
        
        if (processors > 1) {
        	int start;
        	int end;
            ExecutorService application = Executors.newCachedThreadPool();
            for (i = 0; i < processors; i++) {
                start = (i * volSize)/processors;
                end = ((i+1) * volSize)/processors;
                application.execute(new sm2Task(start, end, tDim, r1ptj, initial,
                		                        trapezoidConstant, trapezoidSlope, timeVals));
            }
            application.shutdown();
            try {
            	boolean tasksEnded = application.awaitTermination(24, TimeUnit.HOURS);
            	if (!tasksEnded) {
            		System.err.println("Timed out while waiting for tasks to finish");
            		Preferences.debug("Timed out while waiting for tasks to finish\n");
            		setCompleted(false);
            		return;
            	}
            		
            }
            catch (InterruptedException ex) {
            	System.err.println("Interrupted while waiting for tasks to finish");
        		Preferences.debug("Interrupted while waiting for tasks to finish\n");
        		setCompleted(false);
        		return;	
            }
        } 
        else { // processors == 1
	        for (i = 0; i < volSize; i++) {
	        	voxelsProcessed++;
	        	long vt100 = voxelsProcessed * 100L;
	        	barMarker = (int)(vt100/volSize);
	        	if (barMarker > oldBarMarker) {
	        		oldBarMarker = barMarker;
	        		fireProgressStateChanged(barMarker);
	        	}
	            for (t = 1; t < tDim; t++) {
	            	y_array[t-1] = r1tj[t*volSize + i];
	            }
	            // Note that the nPts, tDim-1, is the number of points in the y_array.
	            dModel = new FitSM2ConstrainedModel(tDim-1, r1ptj, y_array, initial);
	            dModel.driver();
	            //dModel.dumpResults();
	            params = dModel.getParameters();
	            chi_squared = (float)dModel.getChiSquared();
	            // Convert ktrans from /sec to /min
	            // Mutliply /sec by 60 seconds/minute
	            params[0] = 60.0 * params[0];
	            for (j = 0; j < 3; j++) {
	            	destArray[j][i] = (float)params[j];
	            	if (Double.isNaN(params[j])) {
	            	    paramNaN[j]++;	
	            	}
	            	else if (Double.isInfinite(params[j])) {
	            		paramInf[j]++;
	            	}
	            	else {
		            	if (params[j] < paramMin[j]) {
		            		paramMin[j] = params[j];
		            	}
		            	if (params[j] > paramMax[j]) {
		            		paramMax[j] = params[j];
		            	}
	            	}
	            }
	            destArray[3][i] = chi_squared;
	            destExitStatusArray[i] = dModel.getExitStatus();
	            exitStatus[(dModel.getExitStatus() + 11)]++;
            } // for (i = 0; i < volSize; i++)
        } // else processors == 1
        
        if (paramNaN[0] > 0) {
        	System.out.println(paramNaN[0] + " of ktrans values are NaN");
        	Preferences.debug(paramNaN[0] + " of ktrans values are NaN\n");
        }
        
        if (paramNaN[1] > 0) {
        	System.out.println(paramNaN[1] + " of ve values are NaN");
        	Preferences.debug(paramNaN[1] + " of ve values are NaN\n");
        }
        
        if (paramNaN[2] > 0) {
        	System.out.println(paramNaN[2] + " of vp values are NaN");
        	Preferences.debug(paramNaN[2] + " of vp values are NaN\n");
        }
        
        if (paramInf[0] > 0) {
        	System.out.println(paramInf[0] + " of ktrans values are infinite");
        	Preferences.debug(paramInf[0] + " of ktrans values are infinite\n");
        }
        
        if (paramInf[1] > 0) {
        	System.out.println(paramInf[1] + " of ve values are infinite");
        	Preferences.debug(paramInf[1] + " of ve values are infinite\n");
        }
        
        if (paramInf[2] > 0) {
        	System.out.println(paramInf[2] + " of vp values are infinite");
        	Preferences.debug(paramInf[2] + " of vp values are infinite\n");
        }
        
        System.out.println("ktrans minimum value = " + paramMin[0]);
        Preferences.debug("ktrans minimum value = " + paramMin[0] + "\n");
        System.out.println("ktrans maximum value = " + paramMax[0]);
        Preferences.debug("ktrans maximum value = " + paramMax[0] + "\n");
        
        System.out.println("ve minimum value = " + paramMin[1]);
        Preferences.debug("ve minimum value = " + paramMin[1] + "\n");
        System.out.println("ve maximum value = " + paramMax[1]);
        Preferences.debug("ve maximum value = " + paramMax[1] + "\n");
        
        System.out.println("vp minimum value = " + paramMin[2]);
        Preferences.debug("vp minimum value = " + paramMin[2] + "\n");
        System.out.println("vp maximum value = " + paramMax[2]);
        Preferences.debug("vp maximum value = " + paramMax[2] + "\n");
        
        if (exitStatus[12351] > 0) {
        	normalTerminations += exitStatus[12351];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[12351] + "\n");
        }
        
        if (exitStatus[12352] > 0) {
        	normalTerminations += exitStatus[12352];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[12352] + "\n");
        }
        
        if (exitStatus[12353] > 0) {
        	normalTerminations += exitStatus[12353];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[12353] + "\n");
        }
        
        if (exitStatus[12354] > 0) {
        	normalTerminations += exitStatus[12354];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
        	Preferences.debug("because we are computing at noise level\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[12354] + "\n");
        }
        
        if (exitStatus[12355] > 0) {
        	normalTerminations += exitStatus[12355];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[12355] + "\n");
        }
        
        if (exitStatus[12311] > 0) {
        	normalTerminations += exitStatus[12311];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[12311] + "\n");
        }
        
        if (exitStatus[12312] > 0) {
        	normalTerminations += exitStatus[12312];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[12312] + "\n");	
        }
        
        if (exitStatus[12313] > 0) {
        	normalTerminations += exitStatus[12313];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[12313] + "\n");	
        }
        
        if (exitStatus[12314] > 0) {
        	normalTerminations += exitStatus[12314];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[12314] + "\n");	
        }
        
        if (exitStatus[12315] > 0) {
        	normalTerminations += exitStatus[12315];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[12315] + "\n");	
        }
        
        if (exitStatus[12051] > 0) {
        	normalTerminations += exitStatus[12051];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[12051] + "\n");
        }
        
        if (exitStatus[12052] > 0) {
        	normalTerminations += exitStatus[12052];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[12052] + "\n");
        }
        
        if (exitStatus[12053] > 0) {
        	normalTerminations += exitStatus[12053];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[12053] + "\n");
        }
        
        if (exitStatus[12054] > 0) {
        	normalTerminations += exitStatus[12054];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because we are computing at noise level\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[12054] + "\n");
        }
        
        if (exitStatus[12055] > 0) {
        	normalTerminations += exitStatus[12055];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[12055] + "\n");
        }
        
        if (exitStatus[12011] > 0) {
        	normalTerminations += exitStatus[12011];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2\n");
        	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[12011] + "\n");
        }
        
        if (exitStatus[12012] > 0) {
        	normalTerminations += exitStatus[12012];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2\n");
        	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[12012] + "\n");	
        }
        
        if (exitStatus[12013] > 0) {
        	normalTerminations += exitStatus[12013];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2\n");
        	Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[12013] + "\n");	
        }
        
        if (exitStatus[12014] > 0) {
        	normalTerminations += exitStatus[12014];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[12014] + "\n");	
        }
        
        if (exitStatus[12015] > 0) {
        	normalTerminations += exitStatus[12015];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the sum of squares is less than epsabs**2\n");
        	Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[12015] + "\n");	
        }
        
        if (exitStatus[10351] > 0) {
        	normalTerminations += exitStatus[10351];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[10351] + "\n");
        }
        
        if (exitStatus[10352] > 0) {
        	normalTerminations += exitStatus[10352];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[10352] + "\n");	
        }
        
        if (exitStatus[10353] > 0) {
        	normalTerminations += exitStatus[10353];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[10353] + "\n");		
        }
        
        if (exitStatus[10354] > 0) {
        	normalTerminations += exitStatus[10354];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[10354] + "\n");			
        }
        
        if (exitStatus[10355] > 0) {
        	normalTerminations += exitStatus[10355];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");
            Preferences.debug("Number = " + exitStatus[10355] + "\n");				
        }
        
        if (exitStatus[10311] > 0) {
        	normalTerminations += exitStatus[10311];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[10311] + "\n");
        }
        
        if (exitStatus[10312] > 0) {
        	normalTerminations += exitStatus[10312];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[10312] + "\n");	
        }
        
        if (exitStatus[10313] > 0) {
        	normalTerminations += exitStatus[10313];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[10313] + "\n");	
        }
        
        if (exitStatus[10314] > 0) {
        	normalTerminations += exitStatus[10314];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[10314] + "\n");	
        }
        
        if (exitStatus[10315] > 0) {
        	normalTerminations += exitStatus[10315];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[10315] + "\n");	
        }
        
        if (exitStatus[10051] > 0) {
        	normalTerminations += exitStatus[10051];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[10051] + "\n");
        }
        
        if (exitStatus[10052] > 0) {
        	normalTerminations += exitStatus[10052];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[10052] + "\n");
        }
        
        if (exitStatus[10053] > 0) {
        	normalTerminations += exitStatus[10053];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[10053] + "\n");
        }
        
        if (exitStatus[10054] > 0) {
        	normalTerminations += exitStatus[10054];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        	Preferences.debug("because we are computing at noise level\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[10054] + "\n");
        }
        
        if (exitStatus[10055] > 0) {
        	normalTerminations += exitStatus[10055];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[10055] + "\n");
        }
        
        if (exitStatus[10011] > 0) {
        	normalTerminations += exitStatus[10011];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[10011] + "\n");
        }
        
        if (exitStatus[10012] > 0) {
        	normalTerminations += exitStatus[10012];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2\n");
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[10012] + "\n");
        }
        
        if (exitStatus[10013] > 0) {
        	normalTerminations += exitStatus[10013];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2\n");
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[10013] + "\n");
        }
        
        if (exitStatus[10014] > 0) {
        	normalTerminations += exitStatus[10014];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[10014] + "\n");
        }
        
        if (exitStatus[10015] > 0) {
        	normalTerminations += exitStatus[10015];
        	Preferences.debug("Normal terminations because the relative predicted reduction in the objective function is less than epsrel**2\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[10015] + "\n");
        }
        
        if (exitStatus[2351] > 0) {
        	normalTerminations += exitStatus[2351];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[2351] + "\n");
        }
        
        if (exitStatus[2352] > 0) {
        	normalTerminations += exitStatus[2352];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[2352] + "\n");
        }
        
        if (exitStatus[2353] > 0) {
        	normalTerminations += exitStatus[2353];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[2353] + "\n");
        }
        
        if (exitStatus[2354] > 0) {
        	normalTerminations += exitStatus[2354];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
        	Preferences.debug("because we are computing at noise level\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[2354] + "\n");
        }
        
        if (exitStatus[2355] > 0) {
        	normalTerminations += exitStatus[2355];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[2355] + "\n");
        }
        
        if (exitStatus[2311] > 0) {
        	normalTerminations += exitStatus[2311];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[2311] + "\n");
        }
        
        if (exitStatus[2312] > 0) {
        	normalTerminations += exitStatus[2312];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[2312] + "\n");
        }
        
        if (exitStatus[2313] > 0) {
        	normalTerminations += exitStatus[2313];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[2313] + "\n");
        }
        
        if (exitStatus[2314] > 0) {
        	normalTerminations += exitStatus[2314];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[2314] + "\n");
        }
        
        if (exitStatus[2315] > 0) {
        	normalTerminations += exitStatus[2315];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because the relative change in x is less than epsx\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[2315] + "\n");
        }
        
        if (exitStatus[2051] > 0) {
        	normalTerminations += exitStatus[2051];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[2051] + "\n");
        }
        
        if (exitStatus[2052] > 0) {
        	normalTerminations += exitStatus[2052];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[2052] + "\n");
        }
        
        if (exitStatus[2053] > 0) {
        	normalTerminations += exitStatus[2053];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[2053] + "\n");
        }
        
        if (exitStatus[2054] > 0) {
        	normalTerminations += exitStatus[2054];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
        	Preferences.debug("because we are computing at noise level\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[2054] + "\n");
        }
        
        if (exitStatus[2055] > 0) {
        	normalTerminations += exitStatus[2055];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2 and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[2055] + "\n");
        }
        
        if (exitStatus[2011] > 0) {
        	normalTerminations += exitStatus[2011];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[2011] + "\n");
        }
        
        if (exitStatus[2012] > 0) {
        	normalTerminations += exitStatus[2012];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2\n");
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[2012] + "\n");
        }
        
        if (exitStatus[2013] > 0) {
        	normalTerminations += exitStatus[2013];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2\n");
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[2013] + "\n");
        }
        
        if (exitStatus[2014] > 0) {
        	normalTerminations += exitStatus[2014];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[2014] + "\n");
        }
        
        if (exitStatus[2015] > 0) {
        	normalTerminations += exitStatus[2015];
        	Preferences.debug("Normal terminations because the sum of squares is less than epsabs**2\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[2015] + "\n");
        }
        
        if (exitStatus[351] > 0) {
        	normalTerminations += exitStatus[351];
        	Preferences.debug("Normal terminations because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[351] + "\n");
        }
        
        if (exitStatus[352] > 0) {
        	normalTerminations += exitStatus[352];
        	Preferences.debug("Normal terminations because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[352] + "\n");
        }
        
        if (exitStatus[353] > 0) {
        	normalTerminations += exitStatus[353];
        	Preferences.debug("Normal terminations because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");	
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[353] + "\n");
        }
        
        if (exitStatus[354] > 0) {
        	normalTerminations += exitStatus[354];
        	Preferences.debug("Normal terminations because the relative change in x is less than epsx and\n");
        	Preferences.debug("because we are computing at noise level\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[354] + "\n");
        }
        
        if (exitStatus[355] > 0) {
        	normalTerminations += exitStatus[355];
        	Preferences.debug("Normal terminations because the relative change in x is less than epsx and\n");
            Preferences.debug("because we are computing at noise level\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[355] + "\n");
        }
        
        if (exitStatus[311] > 0) {
        	normalTerminations += exitStatus[311];
        	Preferences.debug("Normal terminations because the relative change in x is less than epsx\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[311] + "\n");
        }
        
        if (exitStatus[312] > 0) {
        	normalTerminations += exitStatus[312];
        	Preferences.debug("Normal terminations because the relative change in x is less than epsx\n");	
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[312] + "\n");
        }
        
        if (exitStatus[313] > 0) {
        	normalTerminations += exitStatus[313];
        	Preferences.debug("Normal terminations because the relative change in x is less than epsx\n");
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[313] + "\n");
        }
        
        if (exitStatus[314] > 0) {
        	normalTerminations += exitStatus[314];
        	Preferences.debug("Normal terminations because the relative change in x is less than epsx\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[314] + "\n");
        }
        
        if (exitStatus[315] > 0) {
        	normalTerminations += exitStatus[315];
        	Preferences.debug("Normal terminations because the relative change in x is less than epsx\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[315] + "\n");
        }
        
        if (exitStatus[51] > 0) {
        	normalTerminations += exitStatus[51];
            Preferences.debug("Normal terminations because we are computing at noise level\n");
            Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
            Preferences.debug("Number = " + exitStatus[51] + "\n");
        }
        
        if (exitStatus[52] > 0) {
        	normalTerminations += exitStatus[52];
            Preferences.debug("Normal terminations because we are computing at noise level\n");	
            Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
            Preferences.debug("Number = " + exitStatus[52] + "\n");
        }
        
        if (exitStatus[53] > 0) {
        	normalTerminations += exitStatus[53];
            Preferences.debug("Normal terminations because we are computing at noise level\n");	
            Preferences.debug("The method of Newton was used (at least) in the last step\n");
            Preferences.debug("Number = " + exitStatus[53] + "\n");
        }
        
        if (exitStatus[54] > 0) {
        	normalTerminations += exitStatus[54];
        	Preferences.debug("Normal terminations because we are computing at noise level\n");
        	Preferences.debug("The 2nd but last step was subspace minimization but\n");
            Preferences.debug("the last two were Gauss-Newton steps \n");
            Preferences.debug("Number = " + exitStatus[54] + "\n");
        }
        
        if (exitStatus[55] > 0) {
        	normalTerminations += exitStatus[55];
            Preferences.debug("Normal terminations because we are computing at noise level\n");
            Preferences.debug("The steplength was not unit in both the last two steps\n");	
            Preferences.debug("Number = " + exitStatus[55] + "\n");
        }
        
        if (exitStatus[11] > 0) {
        	abnormalTerminations += exitStatus[11];
        	Preferences.debug("Abnormal terminations with no termination bits set = " + exitStatus[11] + "\n");
        }
        
        if (exitStatus[10] > 0) {
        	abnormalTerminations += exitStatus[10];
            Preferences.debug("Abnormal terminations because m < n or n <= 0 or m <= 0 or mdc < m or mdw < n*n + 5*n + 3*m + 6 or\n");
            Preferences.debug("maxit <= 0 or epsrel < 0 or epsabs < 0 or epsx < 0 or invalid starting point on entry = " + exitStatus[10] + "\n");
        }
        
        if (exitStatus[9] > 0) {
        	abnormalTerminations += exitStatus[9];
        	Preferences.debug("Abnormal terminations because the number of iterations has exceeded the maximum allowed iterations = "
        			           + exitStatus[9] + "\n");
        }
        
        if (exitStatus[8] > 0) {
        	abnormalTerminations += exitStatus[8];
        	Preferences.debug("Abnormal terminations because the Hessian emanating from the 2nd order method is not positive definite = "
        			            + exitStatus[8] + "\n");
        }
        
        if (exitStatus[7] > 0) {
        	abnormalTerminations += exitStatus[7];
        	Preferences.debug("Abnormal terminations because the algorithm would like to use 2nd derivatives but is not allowed to do that = "
        			           + exitStatus[7] + "\n");
        }
        
        if (exitStatus[6] > 0) {
        	abnormalTerminations += exitStatus[6];
        	Preferences.debug("Abnormal terminations because an undamped step with Newtons method is a failure = " + exitStatus[6] + "\n");
        }
        
        if (exitStatus[5] > 0) {
        	abnormalTerminations += exitStatus[5];
        	Preferences.debug("Abnormal terminations because the latest search direction computed using subspace minimization\n");
        	Preferences.debug("was not a descent direction (probably caused by a wrongly computed Jacobian) = " + exitStatus[5] + "\n");
        }
        
        if (exitStatus[4] > 0) {
        	abnormalTerminations += exitStatus[4];
        	Preferences.debug("Abnormal terminations because there is only one feasible point,\n");
        	Preferences.debug("namely X(I) = BL(I) = BU(I), I = 1,2,...,N = " + exitStatus[4] + "\n");
        }
        
        if (exitStatus[3] > 0) {
        	abnormalTerminations += exitStatus[3];
        	Preferences.debug("Abnormal terminations because of NLConstrainedEngine driver error = " + exitStatus[3] + "\n");
        }
        
        System.out.println("\nTotal normal terminations = " + normalTerminations);
        Preferences.debug("\nTotal normal terminations = " + normalTerminations + "\n");
        System.out.println("Total abnormal terminations = " + abnormalTerminations);
        Preferences.debug("Total abnormal terminations = " + abnormalTerminations + "\n");
       
        
        for (i = 0; i < 4; i++) {
	        try {
	        	destImage[i].importData(0, destArray[i], true);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on destImage["+ i + "].importData(0, destArray["+i+"], true)");
	        	setCompleted(false);
	        	return;
	        }
        }
        
        try {
        	destImage[4].importData(0, destExitStatusArray, true);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException on destImage[4].importData(0, destExitStatusArray, true)");
        	setCompleted(false);
        	return;
        }
        
        setCompleted(true);
    }
    
    private void statusMessage(int status) {
    if (status == 12340) { 	
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    }
    else if (status == 12341) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The last steps were computed with prank <> n at the termination point\n");	
    }
    else if (status == 12342) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The method of Newton was used (at least) in the last step\n");	
    }
    else if (status == 12343) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
    	Preferences.debug("because we are computing at noise level\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }
    else if (status == 12344) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The steplength was not unit in both the last two steps\n");	
    }
    else if (status == 12300) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    }
    else if (status == 12301) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");	
    }
    else if (status == 12302) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The method of Newton was used (at least) in the last step\n");	
    }
    else if (status == 12303) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }
    else if (status == 12304) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");	
    	Preferences.debug("The steplength was not unit in both the last two steps\n");	
    }
    else if (status == 12040) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");	
    }
    else if (status == 12041) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The last steps were computed with prank <> n at the termination point\n");	
    }
    else if (status == 12042) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 12043) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }
    else if (status == 12044) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2 and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The steplength was not unit in both the last two steps\n");	
    }
    else if (status == 12000) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2\n");
    	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    }
    else if (status == 12001) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2\n");
    	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }
    else if (status == 12002) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2\n");
    	Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 12003) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }
    else if (status == 12004) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the sum of squares is less than epsabs**2\n");
    	Preferences.debug("The steplength was not unit in both the last two steps\n");	
    }
    else if (status == 10340) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    }
    else if (status == 10341) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }
    else if (status == 10342) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 10343) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
    	Preferences.debug("because we are computing at noise level\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    } 
    else if (status == 10344) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The steplength was not unit in both the last two steps\n");
    }
    else if (status == 10300) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    }
    else if (status == 10301) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }
    else if (status == 10302) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 10303) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }
    else if (status == 10304) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The steplength was not unit in both the last two steps\n");
    }
    else if (status == 10040) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    } 
    else if (status == 10041) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }
    else if (status == 10042) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 10043) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
    	Preferences.debug("because we are computing at noise level\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }  
    else if (status == 10044) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The steplength was not unit in both the last two steps\n");
    }
    else if (status == 10000) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n");
    	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    }
    else if (status == 10001) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n");
    	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }
    else if (status == 10002) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n");
    	Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 10003) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }
    else if (status == 10004) {
    	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n");
    	Preferences.debug("The steplength was not unit in both the last two steps\n");
    }
    else if (status == 2340) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    }
    else if (status == 2341) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2  and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }  
    else if (status == 2342) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }  
    else if (status == 2343) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
    	Preferences.debug("because we are computing at noise level\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }
    else if (status == 2344) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The steplength was not unit in both the last two steps\n");
    } 
    else if (status == 2300) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");	
    	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    }
    else if (status == 2301) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }
    else if (status == 2302) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 2303) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }
    else if (status == 2304) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because the relative change in x is less than epsx\n");	
    	Preferences.debug("The steplength was not unit in both the last two steps\n");
    }
    else if (status == 2040) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    }   
    else if (status == 2041) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2  and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    } 
    else if (status == 2042) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 2043) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
    	Preferences.debug("because we are computing at noise level\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }
    else if (status == 2044) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The steplength was not unit in both the last two steps\n");
    }
    else if (status == 2000) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n");
    	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    } 
    else if (status == 2001) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n");
    	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }
    else if (status == 2002) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n");
    	Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 2003) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n");	
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");
    }
    else if (status == 2004) {
    	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n");
    	Preferences.debug("The steplength was not unit in both the last two steps\n");
    }
    else if (status == 340) {
    	Preferences.debug("Normal termination because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    }  
    else if (status == 341) {
    	Preferences.debug("Normal termination because the relative change in x is less than epsx  and\n");
        Preferences.debug("because we are computing at noise level\n");	
        Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }
    else if (status == 342) {
    	Preferences.debug("Normal termination because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 343) {
    	Preferences.debug("Normal termination because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");  
    }
    else if (status == 344) {
    	Preferences.debug("Normal termination because the relative change in x is less than epsx and\n");
        Preferences.debug("because we are computing at noise level\n");
        Preferences.debug("The steplength was not unit in both the last two steps\n");
    }
    else if (status == 300) {
    	Preferences.debug("Normal termination because the relative change in x is less than epsx\n");
    	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    } 
    else if (status == 301) {
    	Preferences.debug("Normal termination because the relative change in x is less than epsx\n");
    	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }
    else if (status == 302) {
    	Preferences.debug("Normal termination because the relative change in x is less than epsx\n");
    	Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 303) {
    	Preferences.debug("Normal termination because the relative change in x is less than epsx\n");	
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");  
    }
    else if (status == 304) {
    	Preferences.debug("Normal termination because the relative change in x is less than epsx\n");
        Preferences.debug("The steplength was not unit in both the last two steps\n");
    }
    else if (status == 40) {
    	Preferences.debug("Normal termination because we are computing at noise level\n");
    	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n");
    } 
    else if (status == 41) {
    	Preferences.debug("Normal termination because we are computing at noise level\n");
    	Preferences.debug("The last steps were computed with prank <> n at the termination point\n");
    }
    else if (status == 42) {
    	Preferences.debug("Normal termination because we are computing at noise level\n");
    	Preferences.debug("The method of Newton was used (at least) in the last step\n");
    }
    else if (status == 43) {
    	Preferences.debug("Normal termination because we are computing at noise level\n");
    	Preferences.debug("The 2nd but last step was subspace minimization but\n");
        Preferences.debug("the last two were Gauss-Newton steps \n");  
    }
    else if (status == 44) {
    	Preferences.debug("Normal termination because we are computing at noise level\n");
    	Preferences.debug("The steplength was not unit in both the last two steps\n");
    } 
    else if (status == -1) {
        Preferences.debug("Abnormal termination because m < n or n <= 0 or m <= 0 or mdc < m or mdw < n*n + 5*n + 3*m + 6 or\n");
        Preferences.debug("maxit <= 0 or epsrel < 0 or epsabs < 0 or epsx < 0 or invalid starting point on entry\n");
    } 
    else if (status == -2) {
    	Preferences.debug("Abnormal termination because the number of iterations has exceeded the maximum allowed iterations\n");
    }
    else if (status == -3) {
    	Preferences.debug("Abnormal termination because the Hessian emanating from the 2nd order method is not positive definite\n");
    }
    else if (status == -4) {
    	Preferences.debug("Abnormal termination because the algorithm would like to use 2nd derivatives but is not allowed to do that\n");
    }
    else if (status == -5) {
    	Preferences.debug("Abnormal termination because an undamped step with Newtons method is a failure\n");
    }
    else if (status == -6) {
    	Preferences.debug("Abnormal termination because the latest search direction computed using subspace minimization\n");
    	Preferences.debug("was not a descent direction (probably caused by a wrongly computed Jacobian)\n");
    }
    else if (status == -7) {
    	Preferences.debug("Abnormal termination because there is only one feasible point,\n");
    	Preferences.debug("namely X(I) = BL(I) = BU(I), I = 1,2,...,N\n");
    }
    else {
    	Preferences.debug("Exit status = " + status + "\n");
    }
    Preferences.debug("\n");
    }
    
    public class sm2Task implements Runnable {
    	private final int start;
    	private final int end;
    	private final int tDim;
    	private final double r1ptj[];
    	private double initial[];
    	private final double trapezoidConstant[];
    	private final double trapezoidSlope[];
    	private final double timeVals[];
    	
    	public sm2Task(int start, int end, int tDim, double r1ptj[], double initial[],
    			       double trapezoidConstant[], double trapezoidSlope[], double timeVals[]) {
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
    		double y_array[] = new double[tDim-1];
    		double exparray[][] = new double[tDim][tDim];
    		double ymodel[] = new double[tDim-1];
    		double params[];
    		float chi_squared;
    		FitSM2ConstrainedModelC dModel;
    		for (i = start; i < end; i++) {
	        	//fireProgressStateChanged(i * 100/volSize);
	            input(y_array, i);
	            // Note that the nPts, tDim-1, is the number of points in the y_array.
	            dModel = new FitSM2ConstrainedModelC(tDim-1, r1ptj, y_array, initial, exparray, trapezoidConstant,
	            		                             trapezoidSlope, timeVals, r1ptj, ymodel);
	            dModel.driver();
	            //dModel.dumpResults();
	            params = dModel.getParameters();
	            chi_squared = (float)dModel.getChiSquared();
	            // Convert ktrans from /sec to /min
	            // Mutliply /sec by 60 seconds/minute
	            params[0] = 60.0 * params[0];
	            output(params, i, dModel.getExitStatus(), chi_squared);
            } // for (i = start; i < end; i++)	
    	}
    	
    } // class sm2Task implements Runnable
    
    public synchronized void input(double y_array[], int i) {
    	int t;
    	for (t = 1; t < tDim; t++) {
    		y_array[t-1] = r1tj[t*volSize + i];
    	}
    }
    
    public synchronized void output(double params[], int i, int exitBits, float chi_squared) {
    	int j;
    	voxelsProcessed++;
    	long vt100 = voxelsProcessed * 100L;
    	barMarker = (int)(vt100/volSize);
    	if (barMarker > oldBarMarker) {
    		oldBarMarker = barMarker;
    		fireProgressStateChanged(barMarker);
    	}
    	for (j = 0; j < 3; j++) {
    		destArray[j][i] = (float)params[j];
        	if (Double.isNaN(params[j])) {
        	    paramNaN[j]++;	
        	}
        	else if (Double.isInfinite(params[j])) {
        		paramInf[j]++;
        	}
        	else {
            	if (params[j] < paramMin[j]) {
            		paramMin[j] = params[j];
            	}
            	if (params[j] > paramMax[j]) {
            		paramMax[j] = params[j];
            	}
        	}
        }
    	destArray[3][i] = chi_squared;
    	destExitStatusArray[i] = exitBits;
        exitStatus[exitBits + 11]++;	
    }
    
    class FitSM2ConstrainedModelC extends NLConstrainedEngine {
        private double exparray[][];
        private final int tDim;
        private final double trapezoidConstant[];
        private final double trapezoidSlope[];
        private final double timeVals[];
        private final double r1ptj[];
        private double ymodel[];
        /**
         * Creates a new FitDEMRI3ConstrainedModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitSM2ConstrainedModelC(int nPoints, double[] xData, double[] yData, double[] initial,
        		                       double[][] exparray, double trapezoidConstant[],
        		                       double trapezoidSlope[], double timeVals[], double r1ptj[],
        		                       double ymodel[]) {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3, xData, yData);
            tDim = nPoints + 1;
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
            //internalScaling = true;
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
            Preferences.debug(" ******* FitSM2ConstrainedModel ********* \n\n");
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n");
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n");
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n");
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n");
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n");
        }
        
        /**
         * 
         *
         * @param  a          The best guess parameter values.
         * @param  residuals  ymodel - yData.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
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
                //Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n");
                if ((ctrl == -1) || (ctrl == 1)) {
                	ktrans = a[0];
                	ve = a[1];
                	vp = a[2];
                	ktransDivVe = ktrans/ve;
                	for (j = 0; j <= tDim-1; j++) {
                		for (m = 0; m <= tDim-1; m++) {
                	        exparray[j][m] = Math.exp((timeVals[j] - timeVals[m])*ktransDivVe);
                		}
                	}
                	
                	for (m = 2; m <= tDim; m++) {
                		intSum = 0.0;
                		for (j = 2; j <= m; j++) {
                			intSum += trapezoidConstant[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1])*ve;
	                        intSum += trapezoidSlope[j-2]* ((exparray[j-1][m-1]*(timeVals[j-1] - 1.0/ktransDivVe)) -
	                                                                (exparray[j-2][m-1]*(timeVals[j-2] - 1.0/ktransDivVe)))*ve;
                		} // for (j = 2; j <= m; j++)
                		ymodel[m-2] = (intSum + vp * r1ptj[m-1])/(1.0 - h);
                	} // for (m = 2; m <= tDim; m++)
                    // evaluate the residuals[j] = ymodel[j] - ySeries[j]
                    for (j = 0; j < nPts; j++) {
                        residuals[j] = ymodel[j] - ySeries[j];
                        //Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n");
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                	// Calculate the Jacobian analytically
                	ktrans = a[0];
                	ve = a[1];
                	vp = a[2];
                	ktransDivVe = ktrans/ve;
                	for (j = 0; j <= tDim-1; j++) {
                		for (m = 0; m <= tDim-1; m++) {
                	        exparray[j][m] = Math.exp((timeVals[j] - timeVals[m])*ktransDivVe);
                		}
                	}
                	for (m = 2; m <= tDim; m++) {
                		intSumDerivKtrans = 0.0;
                		intSumDerivVe = 0.0;
                		for (j = 2; j <= m; j++) {
	                        intSumDerivKtrans += trapezoidConstant[j-2]*((timeVals[j-1]-timeVals[m-1])*exparray[j-1][m-1] 
	                                                                   - (timeVals[j-2] - timeVals[m-1])*exparray[j-2][m-1]);
	                        intSumDerivKtrans += trapezoidSlope[j-2]*((exparray[j-1][m-1]*(timeVals[j-1]-timeVals[m-1])*(timeVals[j-1] - 1.0/ktransDivVe)) -
	                        		                                  (exparray[j-2][m-1]*(timeVals[j-2]-timeVals[m-1])*(timeVals[j-2] - 1.0/ktransDivVe)));
	                        intSumDerivKtrans += trapezoidSlope[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1])*ve*ve/(ktrans*ktrans);
	                        intSumDerivVe += trapezoidConstant[j-2]*((timeVals[j-1]-timeVals[m-1])*exparray[j-1][m-1] 
	                                                              - (timeVals[j-2]-timeVals[m-1])*exparray[j-2][m-1])*(-ktrans/ve);
	                        intSumDerivVe += trapezoidConstant[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1]);
	                        intSumDerivVe += trapezoidSlope[j-2]*((exparray[j-1][m-1]*(timeVals[j-1]-timeVals[m-1])*(timeVals[j-1] - 1.0/ktransDivVe)) -
	                                  (exparray[j-2][m-1]*(timeVals[j-2]-timeVals[m-1])*(timeVals[j-2] - 1.0/ktransDivVe)))*(-ktrans/ve);
	                        intSumDerivVe += trapezoidSlope[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1])*(-2.0*ve/ktrans);
	                        intSumDerivVe += trapezoidSlope[j-2]* ((exparray[j-1][m-1]*timeVals[j-1]) -
                                    (exparray[j-2][m-1]*timeVals[j-2]));
                		} // for (j = 2; j <= m; j++)
                		covarMat[m-2][0] = intSumDerivKtrans/(1.0 - h);
                		covarMat[m-2][1] = intSumDerivVe/(1.0 - h);
                		covarMat[m-2][2] = r1ptj[m-1]/(1.0 - h);
                		//Preferences.debug("covarMat[" + (m-2) + "][0] = " + covarMat[m-2][0] + "\n");
                		//Preferences.debug("covarMat[" + (m-2) + "][1] = " + covarMat[m-2][1] + "\n");
                		//Preferences.debug("covarMat[" + (m-2) + "][2] = " + covarMat[m-2][2] + "\n");
                	}
                }
                // Calculate the Jacobian numerically
                //else if (ctrl == 2) {
                    //ctrlMat[0] = 0;
                //}
            } catch (Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n");
            }

            return;
        }
    }
    
    class FitSM2ConstrainedModel extends NLConstrainedEngine {

        /**
         * Creates a new FitDEMRI3ConstrainedModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitSM2ConstrainedModel(int nPoints, double[] xData, double[] yData,
                                                           double[] initial) {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3, xData, yData);

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
            //internalScaling = true;
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
            Preferences.debug(" ******* FitSM2ConstrainedModel ********* \n\n");
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n");
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n");
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n");
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n");
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n");
        }
        
        /**
         * 
         *
         * @param  a          The best guess parameter values.
         * @param  residuals  ymodel - yData.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
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
                //Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n");
                if ((ctrl == -1) || (ctrl == 1)) {
                	ktrans = a[0];
                	ve = a[1];
                	vp = a[2];
                	ktransDivVe = ktrans/ve;
                	for (j = 0; j <= tDim-1; j++) {
                		for (m = 0; m <= tDim-1; m++) {
                	        exparray[j][m] = Math.exp((timeVals[j] - timeVals[m])*ktransDivVe);
                		}
                	}
                	
                	for (m = 2; m <= tDim; m++) {
                		intSum = 0.0;
                		for (j = 2; j <= m; j++) {
                			intSum += trapezoidConstant[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1])*ve;
	                        intSum += trapezoidSlope[j-2]* ((exparray[j-1][m-1]*(timeVals[j-1] - 1.0/ktransDivVe)) -
	                                                                (exparray[j-2][m-1]*(timeVals[j-2] - 1.0/ktransDivVe)))*ve;
                		} // for (j = 2; j <= m; j++)
                		ymodel[m-2] = (intSum + vp * r1ptj[m-1])/(1.0 - h);
                	} // for (m = 2; m <= tDim; m++)
                    // evaluate the residuals[j] = ymodel[j] - ySeries[j]
                    for (j = 0; j < nPts; j++) {
                        residuals[j] = ymodel[j] - ySeries[j];
                        //Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n");
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                	// Calculate the Jacobian analytically
                	ktrans = a[0];
                	ve = a[1];
                	vp = a[2];
                	ktransDivVe = ktrans/ve;
                	for (j = 0; j <= tDim-1; j++) {
                		for (m = 0; m <= tDim-1; m++) {
                	        exparray[j][m] = Math.exp((timeVals[j] - timeVals[m])*ktransDivVe);
                		}
                	}
                	for (m = 2; m <= tDim; m++) {
                		intSumDerivKtrans = 0.0;
                		intSumDerivVe = 0.0;
                		for (j = 2; j <= m; j++) {
	                        intSumDerivKtrans += trapezoidConstant[j-2]*((timeVals[j-1]-timeVals[m-1])*exparray[j-1][m-1] 
	                                                                   - (timeVals[j-2] - timeVals[m-1])*exparray[j-2][m-1]);
	                        intSumDerivKtrans += trapezoidSlope[j-2]*((exparray[j-1][m-1]*(timeVals[j-1]-timeVals[m-1])*(timeVals[j-1] - 1.0/ktransDivVe)) -
	                        		                                  (exparray[j-2][m-1]*(timeVals[j-2]-timeVals[m-1])*(timeVals[j-2] - 1.0/ktransDivVe)));
	                        intSumDerivKtrans += trapezoidSlope[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1])*ve*ve/(ktrans*ktrans);
	                        intSumDerivVe += trapezoidConstant[j-2]*((timeVals[j-1]-timeVals[m-1])*exparray[j-1][m-1] 
	                                                              - (timeVals[j-2]-timeVals[m-1])*exparray[j-2][m-1])*(-ktrans/ve);
	                        intSumDerivVe += trapezoidConstant[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1]);
	                        intSumDerivVe += trapezoidSlope[j-2]*((exparray[j-1][m-1]*(timeVals[j-1]-timeVals[m-1])*(timeVals[j-1] - 1.0/ktransDivVe)) -
	                                  (exparray[j-2][m-1]*(timeVals[j-2]-timeVals[m-1])*(timeVals[j-2] - 1.0/ktransDivVe)))*(-ktrans/ve);
	                        intSumDerivVe += trapezoidSlope[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1])*(-2.0*ve/ktrans);
	                        intSumDerivVe += trapezoidSlope[j-2]* ((exparray[j-1][m-1]*timeVals[j-1]) -
                                    (exparray[j-2][m-1]*timeVals[j-2]));
                		} // for (j = 2; j <= m; j++)
                		covarMat[m-2][0] = intSumDerivKtrans/(1.0-h);
                		covarMat[m-2][1] = intSumDerivVe/(1.0-h);
                		covarMat[m-2][2] = r1ptj[m-1]/(1.0-h);
                		//Preferences.debug("covarMat[" + (m-2) + "][0] = " + covarMat[m-2][0] + "\n");
                		//Preferences.debug("covarMat[" + (m-2) + "][1] = " + covarMat[m-2][1] + "\n");
                		//Preferences.debug("covarMat[" + (m-2) + "][2] = " + covarMat[m-2][2] + "\n");
                	}
                }
                // Calculate the Jacobian numerically
                //else if (ctrl == 2) {
                    //ctrlMat[0] = 0;
                //}
            } catch (Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n");
            }

            return;
        }
    }

        /**
         * 
         *
         * @param  a          The best guess parameter values.
         * @param  residuals  ymodel - yData.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        /*public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
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
                //Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n");
                if ((ctrl == -1) || (ctrl == 1)) {
                	ktrans = a[0];
                	ve = a[1];
                	vp = a[2];
                	ktransDivVe = ktrans/ve;
                	for (j = 0; j <= tDim-1; j++) {
                		for (m = 0; m <= tDim-1; m++) {
                	        exparray[j][m] = Math.exp((timeVals[j] - timeVals[m])*ktransDivVe);
                		}
                	}
                	
                	for (m = 2; m <= tDim; m++) {
                		intSum = 0.0;
                		for (j = 2; j <= m; j++) {
                			intSum += trapezoidMidpoint[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1])*ve;
                		} // for (j = 2; j <= m; j++)
                		ymodel[m-2] = intSum + vp * r1ptj[m-1];
                	} // for (m = 2; m <= tDim; m++)
                    // evaluate the residuals[j] = ymodel[j] - ySeries[j]
                    for (j = 0; j < nPts; j++) {
                        residuals[j] = ymodel[j] - ySeries[j];
                        //Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n");
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                	// Calculate the Jacobian analytically
                	ktrans = a[0];
                	ve = a[1];
                	vp = a[2];
                	ktransDivVe = ktrans/ve;
                	for (j = 0; j <= tDim-1; j++) {
                		for (m = 0; m <= tDim-1; m++) {
                	        exparray[j][m] = Math.exp((timeVals[j] - timeVals[m])*ktransDivVe);
                		}
                	}
                	for (m = 2; m <= tDim; m++) {
                		intSumDerivKtrans = 0.0;
                		intSumDerivVe = 0.0;
                		for (j = 2; j <= m; j++) {
	                        intSumDerivKtrans += trapezoidMidpoint[j-2]*((timeVals[j-1]-timeVals[m-1])*exparray[j-1][m-1] 
	                                                                   - (timeVals[j-2] - timeVals[m-1])*exparray[j-2][m-1]);
	                        intSumDerivVe += trapezoidMidpoint[j-2]*((timeVals[j-1]-timeVals[m-1])*exparray[j-1][m-1] 
	                                                              - (timeVals[j-2]-timeVals[m-1])*exparray[j-2][m-1])*(-ktrans/ve);
	                        intSumDerivVe += trapezoidMidpoint[j-2]*(exparray[j-1][m-1] - exparray[j-2][m-1]);
                		} // for (j = 2; j <= m; j++)
                		covarMat[m-2][0] = intSumDerivKtrans;
                		covarMat[m-2][1] = intSumDerivVe;
                		covarMat[m-2][2] = r1ptj[m-1];
                		//Preferences.debug("covarMat[" + (m-2) + "][0] = " + covarMat[m-2][0] + "\n");
                		//Preferences.debug("covarMat[" + (m-2) + "][1] = " + covarMat[m-2][1] + "\n");
                		//Preferences.debug("covarMat[" + (m-2) + "][2] = " + covarMat[m-2][2] + "\n");
                	}
                }
                // Calculate the Jacobian numerically
                //else if (ctrl == 2) {
                    //ctrlMat[0] = 0;
                //}
            } catch (Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n");
            }

            return;
        }
    }*/
    
    class FitAllEP extends NLConstrainedEngineEP {

        /**
         * Creates a new Fit24DModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
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
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(DoubleDouble[] a, DoubleDouble[] residuals, DoubleDouble[][] covarMat) {
            

            return;
        }
    }
    
    class FitAll extends NLConstrainedEngine {

        /**
         * Creates a new Fit24DModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
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
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            

            return;
        }
    }
    
    class FitAllBC extends LevmarBoxConstraint {

        /**
         * Creates a new Fit24DModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitAllBC() {

            // nPoints data points, 3 coefficients, and exponential fitting
            super();

            
        }

        /**
         * Starts the analysis.
         */
        public int driver() {
            return super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            
        }

        /**
         * Fit to function - a0 - a1*(a2**x).
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] param, double[] hx, int paramNum, int nPts) {
            

            return;
        }
        
        public void fitToJacobian(double[] param, double[] jac, int paramNum, int nPts) {
        	return;
        }
    }
    
    class FitAllBC2 extends LevmarBoxConstraint {

        /**
         * Creates a new Fit24DModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitAllBC2() {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(1);

            
        }

        /**
         * Starts the analysis.
         */
        public int driver() {
            return super.driver();
        }

        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            
        }

        /**
         * Fit to function - a0 - a1*(a2**x).
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] param, double[] hx, int paramNum, int nPts) {
            

            return;
        }
        
        public void fitToJacobian(double[] param, double[] jac, int paramNum, int nPts) {
        	return;
        }
    }
    
    class IntModel extends Integration {
        double ktrans;
        double ve;
        /**
         * Creates a new IntModel object.
         *
         * @param  lower    DOCUMENT ME!
         * @param  upper    DOCUMENT ME!
         * @param  routine  DOCUMENT ME!
         * @param  eps      DOCUMENT ME!
         */
        public IntModel(double lower, double upper, int routine, double eps, double ktrans, double ve) {
            super(lower, upper, routine, eps);
            this.ktrans = ktrans;
            this.ve = ve;
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
            function = 2.0 * x * Math.exp(-ktrans*(upper - x)/ve);

            return function;
        }

    }
    
   
}
