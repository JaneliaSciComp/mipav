package gov.nih.mipav.model.algorithms;
import gov.nih.mipav.model.file.FileInfoBase;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.util.BitSet;

import java.awt.*;

import java.io.*;

import java.text.*;


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

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ViewUserInterface UI;
    
    /** Contrast relaxivity rate in 1/(mMol*sec) (0.0 - 1000.0) */
    private double r1;
    
    /** blood intrinsic relaxivity rate in 1/(mMol * sec)
     *  input as reciprocal, in seconds (0.001 - 10000.0)*/
    private double rib;
    
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
    
    private double min_constr[] = new double[3];
    private double max_constr[] = new double[3];
    private double comp[];
    private double elist[];
    private int i;
    private int xDim;
    private int yDim;
    private int zDim;
    private int tDim;
    private double cos0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmDEMRI3 object.
     
     */
    public AlgorithmDEMRI3(ModelImage srcImage, double r1, double rib, double rit, double r1i[], double theta,
                           double tr, boolean perMinute, int nFirst, boolean useVe) {

        super(null, srcImage);
        this.r1 = r1;
        this.rib = rib;
        this.rit = rit;
        this.r1i = r1i;
        this.theta = theta;
        this.tr = tr;
        this.perMinute = perMinute;
        this.nFirst = nFirst;
        this.useVe = useVe;
        
        UI = ViewUserInterface.getReference();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
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
        double x_array[][];
        int size4D;
        double buf4D[];
        
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
        size4D = volSize * tDim;
        
        tf = srcImage.getFileInfo()[0].getResolutions()[3];
        timeUnits = srcImage.getFileInfo()[0].getUnitsOfMeasure()[3];
        if (!perMinute) { // perSecond
           switch (timeUnits) {
               case FileInfoBase.NANOSEC:
                   tf = 1.0E-9 * tf;
                   break;
               case FileInfoBase.MICROSEC:
            	   tf = 1.0E-6 * tf;
            	   break;
               case FileInfoBase.MILLISEC:
            	   tf = 1.0E-3 * tf;
            	   break;
               case FileInfoBase.SECONDS:
               case FileInfoBase.UNKNOWN_MEASURE:
            	   break;
               case FileInfoBase.MINUTES:
            	   tf = 60.0 * tf;
            	   break;
               case FileInfoBase.HOURS:
            	   tf = 3600.0 * tf;
            	   break;
               default:
            	   MipavUtil.displayError("Illegal time units = " + timeUnits);
            	   setCompleted(false);
            	   return;
           }
        }
        else { // perMinute
        	switch (timeUnits) {
	            case FileInfoBase.NANOSEC:
	                tf = 1.0E-9 * tf/60.0;
	                break;
	            case FileInfoBase.MICROSEC:
	         	   tf = 1.0E-6 * tf/60.0;
	         	   break;
	            case FileInfoBase.MILLISEC:
	         	   tf = 1.0E-3 * tf/60.0;
	         	   break;
	            case FileInfoBase.SECONDS:
	            case FileInfoBase.UNKNOWN_MEASURE:
	               tf = tf/60.0;
	         	   break;
	            case FileInfoBase.MINUTES:
	         	   break;
	            case FileInfoBase.HOURS:
	         	   tf = 60.0 * tf;
	         	   break;
	            default:
	         	   MipavUtil.displayError("Illegal time units = " + timeUnits);
	         	   setCompleted(false);
	         	   return;
            }
        }
        
        ts_array = new double[tDim];
        x_array = new double[tDim][3];
        for (t = 0; t < tDim; t++) {
        	x_array[t][0] = 1.0;
        	x_array[t][1] = t * tf;
        	x_array[t][2] = (t * tf) * (t * tf);
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
                }
            }
        } // for (t = 0; t < tDim; t++)
        mask = null;
        volume = null;
        
        min_constr[0] = 0.0;
        min_constr[1] = 0.0;
        min_constr[2] = 0.0;
        
        max_constr[0] = 0.99;
        max_constr[1] = 0.99;
        max_constr[2] = 0.99;
        
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
        ertr_c0 = 1 - Math.exp(-rib * tr) * cos0;
        c0_ertr_c0 = (1.0 - Math.exp(-rib * tr)) * cos0;
        
        Preferences.debug("tDim = " + tDim + "\n");
        Preferences.debug("m0 = " + m0 + "\n");
        Preferences.debug("rTR = " + rTR + "\n");
        Preferences.debug("R_r1 = " + R_r1 + "\n");
        Preferences.debug("ertr = " + ertr + "\n");
        Preferences.debug("ertr_c0 = " + ertr_c0 + "\n");
        Preferences.debug("c0_ertr_c0 = " + c0_ertr_c0 + "\n");
        
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
        
        Preferences.debug("Cp = " + "\n");
        for (c = 0; c < tDim; c++) {
            Preferences.debug("mp[c] = " + mp[c] + "\n");
        }
        Preferences.debug("\n");
        
        buf4D = new double[size4D];
        
        try {
            srcImage.exportData(0, size4D, buf4D);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException on srcImage.exportData(0, size4D, buf4D");
            setCompleted(false);
            return;
        }
        
        for (i = 0; i < volSize; i++) {
            for (t = 0; t < tDim; t++) {
            	ts_array[t] = buf4D[t*volSize + i];
            }
        } // for (i = 0; i < volSize; i++)
        
        setCompleted(true);
    }
    
    class FitDEMRI3ConstrainedModel extends NLConstrainedEngine {

        /**
         * Creates a new FitDEMRI3ConstrainedModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitDEMRI3ConstrainedModel(int nPoints, double[] xData, float[] yData,
                                                           double[] initial) {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3, xData, yData);

            int i;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            // Constrain parameter 0
            bl[0] = min_constr[0];
            bu[0] = max_constr[0];

            // Constrain parameter 1
            if (useVe) {
                bl[1] = Math.max(1.0E-5,min_constr[1]);
            }
            else {
            	bl[1] = min_constr[1];
            }
            bu[1] = max_constr[1];

            // Constrain parameter 2
            bl[2] = min_constr[2];
            bu[2] = max_constr[2];

            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            internalScaling = true;

            gues[0] = initial[0];
            gues[1] = initial[1];
            gues[2] = initial[2];
            gues[3] = initial[3];
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
            Preferences.debug(" ******* FitDoubleExponential ********* \n\n");
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n");
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n");
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n");
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n");
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n");
        }

        /**
         * Fit to function - a3 + (1-a3)*[1 - ao*exp(a1*x) - (1 - a0)*exp(a2*x)].
         *
         * @param  a          The best guess parameter values.
         * @param  residuals  ymodel - yData.
         * @param  covarMat   The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            int ctrl;
            int j;
            double ymodel[] = new double[tDim];
            double e1, e2;
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
            			Preferences.debug("Warning: Voxel = " + i + " ve = " + ve + " K = " + K + " kep = " + kep + "\n");
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
            	    	Preferences.debug("Warning: Voxel = " + i + " rit = " + rit + "\n");
            	    	return;
            	    }
            	} // if (r1i != null)
            	
            	// Compute Ct from Cp and params
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
            	
            	for (n = nFirst; n < tDim; n++) {
            		comp[n] += fvp * mp[n];
            	}
            	
            	for (n = nFirst; n < tDim; n++) {
            		comp[n] = rit + r1 * comp[n];
            	}
            	
            	P1 = Math.exp(-rit * tr);
            	P1c = 1.0 - P1 * cos0;
            	P1 = 1.0 - P1;
            	
            	for (n = 0; n < nFirst; n++) {
            		ymodel[n] = 1.0;
            	}
            	
            	for (n = nFirst; n < tDim; n++) {
            		e = Math.exp(-comp[n] * tr);
            		ymodel[n] = (1 - e)*P1c / ((1 - cos0*e) * P1);
            	}
                ctrl = ctrlMat[0];

                if ((ctrl == -1) || (ctrl == 1)) {

                    // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                    for (j = 0; j < nPts; j++) {
                        /*ymodel = a[3] +
                                 ((1.0 - a[3]) *
                                      (1 - (a[0] * Math.exp(a[1] * xSeries[i])) -
                                           ((1.0 - a[0]) * Math.exp(a[2] * xSeries[i]))));*/
                    	
                        residuals[j] = ymodel[j] - ySeries[j];
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {

                    // Calculate the Jacobian analytically
                    /*for (j = 0; j < nPts; j++) {
                        e1 = Math.exp(a[1] * xSeries[i]);
                        e2 = Math.exp(a[2] * xSeries[i]);
                        covarMat[i][0] = (1.0 - a[3]) * (-e1 + e2);
                        covarMat[i][1] = -(1.0 - a[3]) * a[0] * xSeries[i] * e1;
                        covarMat[i][2] = -(1.0 - a[3]) * (1.0 - a[0]) * xSeries[i] * e2;
                        covarMat[i][3] = (a[0] * e1) + ((1.0 - a[0]) * e2);
                    }*/
                } // else if (ctrl == 2)
            } catch (Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n");
            }

            return;
        }
    }
}
