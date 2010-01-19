package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;


/**
 * 3 model parameters are fit:
 * 1) K_trans in [0, 0.05]
 * 2) User choice of k_ep in [0, 0.05] or ve
 * 3) f_pv in [0, 0.99]
 * K_trans and k_ep default to rates per second, but the user changed select rates per minute.
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
    
    /** Array from 1D Mp(t) data file */
    private double mcp[] = null;
    
    /** x dimension of Mp(t) data file */
    private int mp_len;
    
    /** nfirst injection TR index of input dataset (0 - 1000) 
     *  Number of TRs before Gd injection */
    private int nFirst;
    
    /** If false, second parameter is back-transfer rate (k_ep)
     *  If true, Second parameter is external celluar volume fraction (ve)
     */
    private boolean useVe;
    
    private double epsilon = 1.0E-4;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmDEMRI3 object.
     
     */
    public AlgorithmDEMRI3(double r1, double rib, double rit, double r1i[], double theta,
                           double tr, double tf, boolean perMinute, double mcp[], int mp_len,
                           int nFirst, boolean useVe) {

        //super(null, R1IImage);
        this.r1 = r1;
        this.rib = rib;
        this.rit = rit;
        this.r1i = r1i;
        this.theta = theta;
        this.tr = tr;
        this.tf = tf;
        this.perMinute = perMinute;
        this.mcp = mcp;
        this.mp_len = mp_len;
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
        double cos0 = Math.cos(theta * Math.PI/180.0);
        double min_constr[] = new double[3];
        double max_constr[] = new double[3];
        double comp[] = new double[mp_len];
        double elist[] = new double[mp_len];
        double dval;
        min_constr[0] = 0.0;
        min_constr[1] = 0.0;
        min_constr[2] = 0.0;
        
        max_constr[0] = 0.99;
        max_constr[1] = 0.99;
        max_constr[2] = 0.99;
        
        /* Compute m0 equal to mean of first 'nFirst + 1' values */
        dval = 0.0;
        for (c = 0; c <= nFirst; c++) {
            dval += mcp[c];
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
        
        Preferences.debug("mp_len = " + mp_len + "\n");
        Preferences.debug("m0 = " + m0 + "\n");
        Preferences.debug("rTR = " + rTR + "\n");
        Preferences.debug("R_r1 = " + R_r1 + "\n");
        Preferences.debug("ertr = " + ertr + "\n");
        Preferences.debug("ertr_c0 = " + ertr_c0 + "\n");
        Preferences.debug("c0_ertr_c0 = " + c0_ertr_c0 + "\n");
        
        /* start with setting the first nFirst + 1 terms to 0 */
        for (c = 0; c <= nFirst; c++) {
            mcp[c] = 0.0;
        }
        
        /* and compute the remainder of the array */
        for (; c < mp_len; c++) {
            /* start with ratio */
            dval = (ertr_c0 - c0_ertr_c0 * mcp[c]/ m0)/ (ertr_c0 - ertr *mcp[c] / m0);
            
            if (dval < 1.0) {
                mcp[c] = 0.0; // If ln < 0, then c < 0, so skip
            }
            else {
                mcp[c] = rTR * Math.log(dval) - R_r1;
            }
            
            if (mcp[c] < 0.0) {
                mcp[c] = 0.0; /* Don't allow result < 0 */
            }
        }
        
        Preferences.debug("Cp = " + "\n");
        for (c = 0; c < mp_len; c++) {
            Preferences.debug("mcp[c] = " + mcp[c] + "\n");
        }
        Preferences.debug("\n");
        
        setCompleted(true);
    }
}
