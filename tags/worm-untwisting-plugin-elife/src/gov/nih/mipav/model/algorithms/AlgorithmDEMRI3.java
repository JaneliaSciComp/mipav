package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.util.BitSet;
import java.io.*;



/**
 * 3 model parameters are fit for each voxel in 3D:
 * 1) K_trans in [0, 0.99]
 * 2) User choice of k_ep in [0, 0.99] or ve
 * 3) f_vp in [0, 0.99]
 * K_trans and k_ep default to rates per second, but the user changed select rates per minute.
 * srcImage is a dynamic "4D volume" of MRI signal (3D over time).
 * An optional intrinsic relaxivity map may be obtained by acquiring two acquisition scans at low and high
 * flip angles; otherwise, a single tissue relaxivity can be assumed for the whole brain.
 
 References:
 1.) "A Unified Magnetic Resonance Imaging Pharmacokinetic Theory: Intravascular and Extracellular Contrast
 Reagents" by Xin Li, William D. Rooney, and Charles S. Springer, Jr., Magnetic Resonance in Medicine,
 Vol. 54, 2005, pp. 1351-1359.
 2.) Erratum: Magnetic Resonance in Medicine, Vol. 55, 2006, p.1217.
 3.) Quantitative MRI of the Brain, Edited by Paul Tofts, 2003, John Wiley & Sons, Ltd.,
 ISBN: 0-47084721-2, Chapter 10, T1-w DCE-MRI: T1-weighted Dynamic Contrast-enhanced MRI by
 Geoff J. M. Parker and Anwar R. Padhani.
 */
public class AlgorithmDEMRI3 extends AlgorithmBase {
    
    private int FIRST_VOLUME_TISSUE = 2;
    
    private int SEPARATE_VOLUME_TISSUE = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** Contrast relaxivity rate in 1/(mMol*sec) (0.0 - 1000.0) */
    private double r1;
    
    /** If true, user specifed rib, otherwise, voi specified rib */
    private boolean userSpecifiedBlood;
    
    /** blood intrinsic relaxivity rate in 1/(mMol * sec)
     *  input as reciprocal, in seconds (0.001 - 10000.0)*/
    private double rib;
    
    private int tissueSource;
    
    /** tissue intrinsic relaxivity rate in 1/(mMol * sec)
     *  specified as reciprocal, in seconds (0.001 - 10000.0)*/
    private double rit;
    
    /** non-uniform tissue intrinsic relaxivity map from input 3d + t = 4D image */
    private double r1i[] = null;
    
    /** Flip angle in degrees (0.0 - 90.0) */
    private double theta;
    
    /** Time between shots in seconds (0.0 - 10000.0) */
    private double tr;
    
    /** Time between frames (volumes) in seconds (0.1 - 30.0) */
    private double tf;
    
    /** If perMinute == false, K_trans and k_ep are per second
     *  If perMinute == true, K_trans and k_ep are per minute
     */
    private boolean perMinute;
    
    /** 1D Mp(t) data from sagittal sinus VOI */
    private double mp[] = null;
    
    /** nfirst injection TR index of input dataset (0 - 1000) 
     *  Number of TRs before Gd injection */
    private int nFirst;
    
    /** If false, second parameter is back-transfer rate (k_ep)
     *  If true, Second parameter is external celluar volume fraction (ve)
     */
    private boolean useVe;
    
    private double epsilon = 1.0E-4;
    
    private double min_constr[];
    private double max_constr[];
    private double comp[];
    private double elist[];
    private double ymodel[];
    private int i;
    private int xDim;
    private int yDim;
    private int zDim;
    private int tDim;
    private double cos0;
    double initial[] = new double[3];
    
    private ModelImage tissueImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmDEMRI3 object.
     
     */
    public AlgorithmDEMRI3(ModelImage destImage, ModelImage srcImage, double min_constr[], double max_constr[],
    		               double r1, boolean userSpecifiedBlood, double rib, int tissueSource, double rit, ModelImage tissueImage, double theta,
                           double tr, boolean perMinute, int nFirst, boolean useVe) {

        super(destImage, srcImage);
        this.min_constr = min_constr;
        this.max_constr = max_constr;
        this.r1 = r1;
        this.userSpecifiedBlood = userSpecifiedBlood;
        this.rib = rib;
        this.tissueSource = tissueSource;
        this.rit = rit;
        this.tissueImage = tissueImage;
        this.theta = theta;
        this.tr = tr;
        this.perMinute = perMinute;
        this.nFirst = nFirst;
        this.useVe = useVe;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        destImage = null;
        r1i = null;
        mp = null;
        min_constr = null;
        max_constr = null;
        if (tissueImage != null) {
        	tissueImage.disposeLocal();
        	tissueImage = null;
        }
        comp = null;
        elist = null;
        initial = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {
        int c;
        double m0;
        double rTR;
        double R_r1;
        double ertr;
        double ertr_c0;
        double c0_ertr_c0;
        double dval;
        ViewVOIVector VOIs;
        BitSet mask;
        double volume[];
        int volSize;
        int t;
        int timeUnits;
        double ts_array[];
        double x_array[];
        int size4D;
        double buf4D[];
        FitDEMRI3ConstrainedModel dModel;
        double[] params;
        float destArray[];
        int j;
        boolean testDraper = false;
        boolean testHock25 = false;
        int voiCount;
        
        if (testDraper) {
        	// Below is an example used to fit y = alpha - beta*(rho**x)
        	// This example implements the solution of problem D of chapter 24 of Applied Regression Analysis, Third Edition by
        	// Norman R. Draper and Harry Smith */
        	// The correct answer is a0 = 72.4326,  a1 = 28.2519, a2 = 0.5968
        	// Works for 4 possibilities of internalScaling = true, false; Jacobian calculation = numerical, analytical
            double[] xSeries = new double[5];
            float[] ySeries = new float[5];
            double[] initial = new double[3];
            int nPoints = 5;
            Fit24DModel fmod;
            xSeries[0] = 0.0;
            xSeries[1] = 1.0;
            xSeries[2] = 2.0;
            xSeries[3] = 3.0;
            xSeries[4] = 4.0;
            ySeries[0] = 44.4f;
            ySeries[1] = 54.6f;
            ySeries[2] = 63.8f;
            ySeries[3] = 65.7f;
            ySeries[4] = 68.9f;
            initial[0] = 0.0;
            initial[1] = 10.0;
            initial[2] = 0.2;
            fmod = new Fit24DModel(nPoints, xSeries, ySeries, initial);
            fmod.driver();
            fmod.dumpResults();
            return;
        }
        
        if (testHock25) {
        	// Below is an example used to fit y = (-50.0 * log(0.01*i)**(2.0/3.0) + 25.0
        	// where a0 = -50, a1 = 2.0/3.0, a3 = 25.0
        	// Variant of test example 25 from Hock and Schittkowski
        	// Works for 4 possibilities of internalScaling = true, false; Jacobian calculation = numerical, analytical
        	double xSeries[] = new double[99];
        	double ySeries[] = new double[99];
        	double initial[] = new double[3];
        	int nPoints = 99;
        	Fit25HModel fmod;
        	for (i = 1; i <= 99; i++) {
        		xSeries[i-1] = 0.01 * i;
        		ySeries[i-1] = Math.pow((-50.0 * Math.log(xSeries[i-1])),2.0/3.0) + 25.0;
        	}
        	initial[0] = -100.0;
        	initial[1] = 1.0/3.0;
        	initial[2] = 12.5;
        	fmod = new Fit25HModel(nPoints, xSeries, ySeries, initial);
        	fmod.driver();
            fmod.dumpResults();
            return;
        }
        
        if (srcImage.getNDims() != 4) {
            MipavUtil.displayError("srcImage must be 4D");
            setCompleted(false);
            return;
        }
        
        cos0 = Math.cos(theta * Math.PI/180.0);
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        tDim = srcImage.getExtents()[3];
        volSize = xDim * yDim * zDim;
        size4D = volSize * (tDim - nFirst);
        destArray = new float[3 * volSize];
        
        if ((tissueSource  == FIRST_VOLUME_TISSUE) || (tissueSource == SEPARATE_VOLUME_TISSUE)) {
        	r1i = new double[volSize];
        }
        
        if (tissueSource == FIRST_VOLUME_TISSUE) {
        	try {
        	    srcImage.exportData(0, volSize, r1i);
        	}
        	catch(IOException e) {
        		MipavUtil.displayError("IOException on srcImage.exportData(0, volSize, r1i)");
        		setCompleted(false);
        		return;
        	}
        }
        
        if (tissueSource == SEPARATE_VOLUME_TISSUE) {
        	try {
        	    tissueImage.exportData(0, volSize, r1i);
        	}
        	catch(IOException e) {
        		MipavUtil.displayError("IOException on tissueImage.exportData(0, volSize, r1i)");
        		setCompleted(false);
        		return;
        	}
        	tissueImage.disposeLocal();
        	tissueImage = null;
        }
        
        tf = srcImage.getFileInfo()[0].getResolutions()[3];
        timeUnits = srcImage.getFileInfo()[0].getUnitsOfMeasure()[3];
        if (!perMinute) { // perSecond
           switch (Unit.getUnitFromLegacyNum(timeUnits)) {
               case NANOSEC:
                   tf = 1.0E-9 * tf;
                   break;
               case MICROSEC:
            	   tf = 1.0E-6 * tf;
            	   break;
               case MILLISEC:
            	   tf = 1.0E-3 * tf;
            	   break;
               case SECONDS:
               case UNKNOWN_MEASURE:
            	   break;
               case MINUTES:
            	   tf = 60.0 * tf;
            	   break;
               case HOURS:
            	   tf = 3600.0 * tf;
            	   break;
               default:
            	   MipavUtil.displayError("Illegal time units = " + timeUnits);
            	   setCompleted(false);
            	   return;
           }
        }
        else { // perMinute
        	switch (Unit.getUnitFromLegacyNum(timeUnits)) {
	            case NANOSEC:
	                tf = 1.0E-9 * tf/60.0;
	                break;
	            case MICROSEC:
	         	   tf = 1.0E-6 * tf/60.0;
	         	   break;
	            case MILLISEC:
	         	   tf = 1.0E-3 * tf/60.0;
	         	   break;
	            case SECONDS:
	            case UNKNOWN_MEASURE:
	               tf = tf/60.0;
	         	   break;
	            case MINUTES:
	         	   break;
	            case HOURS:
	         	   tf = 60.0 * tf;
	         	   break;
	            default:
	         	   MipavUtil.displayError("Illegal time units = " + timeUnits);
	         	   setCompleted(false);
	         	   return;
            }
        }
        
        ts_array = new double[tDim-nFirst];
        x_array = new double[tDim-nFirst];
        ymodel = new double[tDim-nFirst];
        for (t = nFirst; t < tDim; t++) {
        	x_array[t-nFirst] = (t-nFirst) * tf;
        }
        
        comp = new double[tDim];
        elist = new double[tDim];
        mp = new double[tDim];
        
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
        volume = new double[volSize];
        
        voiCount = 0;
        for (t = 0; t < tDim; t++) {
            try {
                srcImage.exportData(t*volSize, volSize, volume);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException on srcImage.exportData(t*volSize, volSize, volume");
                setCompleted(false);
                return;
            }
            
            for (i = 0; i < volSize; i++) {
                if (mask.get(i)) {
                    mp[t] += volume[i];
                    if (t == 0  && (!userSpecifiedBlood)) {
                        voiCount++;	
                    }
                }  
            }
        } // for (t = 0; t < tDim; t++)
        
        if (!userSpecifiedBlood) {
        	rib = mp[0]/voiCount;
        }
        mask = null;
        volume = null;
        
        /*  convert Mp array to Cp(t) array

        Cp(t) =   1    * ln[ 1-exp(-RIB*TR)cos0 - (1-exp(-RIB*TR))cos0*Mp(t)/Mp(0) ]
                -----      [ ----------------------------------------------------  ]
                r1*TR      [  1-exp(-RIB*TR)cos0 - (1-exp(-RIB*TR))*Mp(t)/Mp(0)    ]

                       - RIB/r1

        subject to Cp(t) >= 0
    */

        /* Compute m0 equal to mean of first 'nFirst + 1' values */
        dval = 0.0;
        for (c = 0; c <= nFirst; c++) {
            dval += mp[c];
        }
        m0 = dval /(nFirst + 1);
        
        if (m0 < epsilon) {
            MipavUtil.displayError("m0 = " + m0 + " is smaller than epsilon = " + epsilon);
            setCompleted(false);
            return;
        }
        
        /* Simple terms */
        rTR = 1.0/(r1 * tr);
        R_r1 = rib/r1;
        
        /* exponential terms */
        ertr = 1.0 - Math.exp(-rib * tr);
        ertr_c0 = 1.0 - Math.exp(-rib * tr) * cos0;
        c0_ertr_c0 = (1.0 - Math.exp(-rib * tr)) * cos0;
        
        Preferences.debug("tDim = " + tDim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("m0 = " + m0 + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rTR = " + rTR + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("R_r1 = " + R_r1 + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("ertr = " + ertr + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("ertr_c0 = " + ertr_c0 + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("c0_ertr_c0 = " + c0_ertr_c0 + "\n", Preferences.DEBUG_ALGORITHM);
        
        /* start with setting the first nFirst + 1 terms to 0 */
        for (c = 0; c <= nFirst; c++) {
            mp[c] = 0.0;
        }
        
        /* and compute the remainder of the array */
        for (; c < tDim; c++) {
            /* start with ratio */
            dval = (ertr_c0 - c0_ertr_c0 * mp[c]/ m0)/ (ertr_c0 - ertr *mp[c] / m0);
            
            if (dval < 1.0) {
                mp[c] = 0.0; // If ln < 0, then c < 0, so skip
            }
            else {
                mp[c] = rTR * Math.log(dval) - R_r1;
            }
            
            if (mp[c] < 0.0) {
                mp[c] = 0.0; /* Don't allow result < 0 */
            }
        }
        
        Preferences.debug("Cp = " + "\n", Preferences.DEBUG_ALGORITHM);
        for (c = 0; c < tDim; c++) {
            Preferences.debug("mp[c] = " + mp[c] + "\n", Preferences.DEBUG_ALGORITHM);
        }
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        
        buf4D = new double[size4D];
        
        try {
            srcImage.exportData(nFirst*volSize, size4D, buf4D);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException on srcImage.exportData(nFirst*volSize, size4D, buf4D");
            setCompleted(false);
            return;
        }
        for (i = 0; i < 3; i++) {
        	initial[i] = (min_constr[i] + max_constr[i])/2.0;
        }
        
        for (i = 0; i < volSize; i++) {
        	fireProgressStateChanged(i * 100/volSize);
            for (t = nFirst; t < tDim; t++) {
            	ts_array[t-nFirst] = buf4D[(t-nFirst)*volSize + i];
            }
            dModel = new FitDEMRI3ConstrainedModel(tDim-nFirst, x_array, ts_array, initial);
            dModel.driver();
            //dModel.dumpResults();
            params = dModel.getParameters();
            for (j = 0; j < 3; j++) {
            	destArray[j*volSize + i] = (float)params[j];
            }
        } // for (i = 0; i < volSize; i++)
        
        try {
        	destImage.importData(0, destArray, true);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException on destImage.importData(0, destArray, true)");
        	setCompleted(false);
        	return;
        }
        
        setCompleted(true);
    }
    
    class FitDEMRI3ConstrainedModel extends NLConstrainedEngine {
    	@SuppressWarnings("unused")
    	private double xData[];
    	private double yData[];

        /**
         * Creates a new FitDEMRI3ConstrainedModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitDEMRI3ConstrainedModel(int nPoints, double[] xData, double[] yData,
                                                           double[] initial) {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3);
            this.xData = xData;
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
         * Display results of displaying DEMRI3 fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitDEMRI3ConstrainedModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
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
            double K;
            double fvp;
            double ve;
            double kep;
            int n;
            double P12;
            double P3;
            double dval;
            double P14;
            double P1;
            double P1c;
            double e;

            try {
            	K = a[0];
            	fvp = a[2];
            	
            	if (perMinute) {
            		K = K/60.0;
            	} // if (perMinute)
            	
            	if (useVe) {
            		ve = a[1];
            		kep = K/ve;
            		if (kep > 10.0) {
            			Preferences.debug("Warning: Voxel = " + i + " ve = " + ve + " K = " + K + " kep = " + kep + "\n", 
            					Preferences.DEBUG_ALGORITHM);
            			return;
            		}
            	} // if (useVe)
            	else {
            		kep = a[1];
            		if (perMinute) {
            			kep = kep/60.0;
            		}
            	}
            	
            	if (r1i != null) {
            	    rit = r1i[i];
            	    if ((rit < 0.02) || (rit > 20.0)) {
            	    	Preferences.debug("Warning: Voxel = " + i + " rit = " + rit + "\n", Preferences.DEBUG_ALGORITHM);
            	    	return;
            	    }
            	} // if (r1i != null)
            	
            	/*----------------------------------------------------------------------
            	  compute Ct from Cp and params

            	    Ct[n] = Ktrans * ( sum [ Cp[k] * exp( -kep(n-k)*TF )
            	            ------                 * ( exp(kep*TF/2) - exp(-kep*TF/2) ) ]
            	             kep
            	                       + (Cp[m] + Cp[n]) * (1 - exp(-kep*TF/2)) )

            	    where sum is over k=m+1..n-1 (m is nfirst), and n is over m..len-1
            	    note: Cp[n] = Ct[n] = 0, for n in {0..m}  (m also, since that is time=0)
            	    note: (Cp[m]+Cp[n])*... reflects the first and last half intervals,
            	          while the sum reflects all the interior, full intervals

            	        Let P1  = Ktrans / kep
            	            P2  = exp(kep*TF/2) - exp(-kep*TF/2)
            	                = exp(-P3/2) - exp(P3/2)
            	            P3  = -kep*TF
            	            P4  = 1 - exp(-kep*TF/2)
            	                = 1 - exp(P3/2)

            	    Ct[n] = P1*P2 * sum [ Cp[k] * exp(P3*(n-k)) ] + P1*P4 * (Cp[m]+Cp[n])
            	    note: in exp_list, max power is (P3*(len-1-m)), m is nfirst
            	    note: Ct is stored in P->comp
            	    
            	    ** This is the only place that the dataset TR (which we label as TF,
            	       the inter-frame TR) is used in this model.
            	*/

            	/* init first elements to 0 */
            	for (n = 0; n < nFirst; n++) {
            		comp[n] = 0.0;
            	}
            	
            	// Assign P*, and then fill elist[] from P3 */
            	P12 = K/kep;
            	P3 = -kep * tf;
            	dval = Math.exp(P3/2.0);
            	P14 = P12 * (1.0 - dval);
            	P12 = P12 * (1.0/dval - dval);
            	
            	elist[0] = 1.0;
            	dval = Math.exp(P3);
            	for (j = 1; j < tDim - nFirst; j++) {
            	    elist[j] = dval * elist[j-1];	
            	}
            	
            	for (n = nFirst; n < tDim; n++) {
            		dval = 0.0;
            		for (j = nFirst+1; j < n; j++) {
            			dval += mp[j] * elist[n-j];
            		}
            		comp[n] = P12 * dval + P14 * (mp[n] + mp[nFirst]);
            	}
            	
            	/*----------------------------------------------------------------------
                C[n] = { 0, for n < nfirst
                       { Ct[n] + fpv * Cp[n], n = nfirst..len-1

                note: C will replace Ct in P->comp
               */

            	for (n = nFirst; n < tDim; n++) {
            		comp[n] += fvp * mp[n];
            	}
            	
            	/*----------------------------------------------------------------------
                R1[n] = RIT + r1 * C[n]

                note: R1 will replace C in P->comp
                      R1[i] will be constant P->RIT over i=0..nfirst-1
                */

            	
            	for (n = nFirst; n < tDim; n++) {
            		comp[n] = rit + r1 * comp[n];
            	}
            	
            	/*----------------------------------------------------------------------
            	  compute the final time series, M_transpose, from the R1 array

            	    Mx[n] = 1 * (1 - exp(-R1[n] * TR)) * (1 - exp(-RIT*TR)cos0)
            	                ---------------------------------------------
            	                (1 - exp(-R1[n] * TR)cos0) * (1 - exp(-RIT*TR))

            	          =     (1 - e[n]) * P1c) / [(1 - cos0*e[n]) * P1]

            	        where P1   = 1 - exp(-RIT*TR)
            	              P1c  = 1 - exp(-RIT*TR)*cos0
            	              e[n] = exp(-R1[n] * TR)

            	    notes:  R1 is in P->comp
            	            The '1' is a placeholder for Mx(t=0), which should have
            	              been factored out of the input time series.
            	            Mx[i] is identically 1, for i = 0..nfirst-1 .
            	            I can see no speed-up for e[n].  :'(
            	*/

            	
            	P1 = Math.exp(-rit * tr);
            	P1c = 1.0 - P1 * cos0;
            	P1 = 1.0 - P1;
            	
            	/*for (n = 0; n < nFirst; n++) {
            		ymodel[n] = 1.0;
            	}*/
            	
            	for (n = nFirst; n < tDim; n++) {
            		e = Math.exp(-comp[n] * tr);
            		ymodel[n-nFirst] = (1 - e)*P1c / ((1 - cos0*e) * P1);
            	}
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {

                    // evaluate the residuals[j] = ymodel[j] - yData[j]
                    for (j = 0; j < nPts; j++) {
                        residuals[j] = ymodel[j] - yData[j];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                // Calculate the Jacobian numerically
                else if (ctrl == 2) {
                    ctrlMat[0] = 0;
                }
            } catch (Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
    }
    
    /**
     * DOCUMENT ME!
     */
    class Fit24DModel extends NLConstrainedEngine {
    	private double xData[];
    	private float yData[];

        /**
         * Creates a new Fit24DModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public Fit24DModel(int nPoints, double[] xData, float[] yData, double[] initial) {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3);
            this.xData = xData;
            this.yData = yData;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain alpha
            bl[0] = -1000.0;
            bu[0] = 1000.0;

            // Constrain beta
            bl[1] = -1000.0;
            bu[1] = 1000.0;

            // Constrain rho
            bl[2] = 0.0;
            bu[2] = 1.0;

            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            internalScaling = true;

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
            Preferences.debug(" ******* FitDoubleExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * Fit to function - a0 - a1*(a2**x).
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int i;
            double ymodel = 0.0;

            try {
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {

                    // evaluate the residuals[i] = ymodel[i] - yData[i]
                    for (i = 0; i < nPts; i++) {
                        ymodel = a[0] - (a[1] * Math.pow(a[2], xData[i]));
                        residuals[i] = ymodel - yData[i];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {

                    // Calculate the Jacobian analytically
                    for (i = 0; i < nPts; i++) {
                        covarMat[i][0] = 1.0;
                        covarMat[i][1] = -Math.pow(a[2], xData[i]);
                        covarMat[i][2] = -xData[i] * a[1] * Math.pow(a[2], xData[i] - 1.0);
                    }
                } // else if (ctrl == 2)
                // If the user wishes to calculate the Jacobian numerically
                /* else if (ctrl == 2) {
                 * ctrlMat[0] = 0; } */
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
    }
    
    /**
     * DOCUMENT ME!
     */
    class Fit25HModel extends NLConstrainedEngine {
    	private double xData[];
    	private double yData[];

        /**
         * Creates a new Fit25HModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public Fit25HModel(int nPoints, double[] xData, double[] yData, double[] initial) {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3);
            this.xData = xData;
            this.yData = yData;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain a[0]
            bl[0] = -100.0;
            bu[0] = -0.1;

            // Constrain a[1]
            bl[1] = 0.0;
            bu[1] = 5.0;

            // Constrain a[2]
            bl[2] = 0.0;
            bu[2] = 25.6;

            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            //internalScaling = true;

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
            Preferences.debug(" ******* FitDoubleExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * Fit to function - Math.pow((a[0] * Math.log(xSeries[i])),a[1]) + a[2]
         *
         * @param  a          The x value of the data point.
         * @param  residuals  The best guess parameter values.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int i;
            double ymodel = 0.0;

            try {
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {

                    // evaluate the residuals[i] = ymodel[i] - yData[i]
                    for (i = 0; i < nPts; i++) {
                        ymodel = Math.pow((a[0] * Math.log(xData[i])),a[1]) + a[2];
                        residuals[i] = ymodel - yData[i];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {

                    // Calculate the Jacobian analytically
                    for (i = 0; i < nPts; i++) {
                        covarMat[i][0] = a[1]*Math.pow((a[0] * Math.log(xData[i])),a[1]-1.0) * Math.log(xData[i]);
                        covarMat[i][1] = Math.log(a[0] * Math.log(xData[i])) * Math.pow((a[0] * Math.log(xData[i])),a[1]);
                        covarMat[i][2] = 1.0;
                    }
                	// Calculate the Jacobian numerically
                	//ctrlMat[0] = 0;
                } // else if (ctrl == 2)
                // If the user wishes to calculate the Jacobian numerically
                /* else if (ctrl == 2) {
                 * ctrlMat[0] = 0; } */
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
    }
}
