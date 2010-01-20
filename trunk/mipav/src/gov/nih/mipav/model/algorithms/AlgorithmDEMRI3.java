package gov.nih.mipav.model.algorithms;


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
 * 3) f_pv in [0, 0.99]
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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmDEMRI3 object.
     
     */
    public AlgorithmDEMRI3(ModelImage srcImage, double r1, double rib, double rit, double r1i[], double theta,
                           double tr, double tf, boolean perMinute, int nFirst, boolean useVe) {

        super(null, srcImage);
        this.r1 = r1;
        this.rib = rib;
        this.rit = rit;
        this.r1i = r1i;
        this.theta = theta;
        this.tr = tr;
        this.tf = tf;
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
        double cos0 = Math.cos(theta * Math.PI/180.0);
        double min_constr[] = new double[3];
        double max_constr[] = new double[3];
        double comp[];
        double elist[];
        double dval;
        ViewVOIVector VOIs;
        int i;
        int xDim;
        int yDim;
        int zDim;
        int tDim;
        BitSet mask;
        double volume[];
        int volSize;
        int t;
        
        if (srcImage.getNDims() != 4) {
            MipavUtil.displayError("srcImage must be 4D");
            setCompleted(false);
            return;
        }
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        tDim = srcImage.getExtents()[3];
        volSize = xDim * yDim * zDim;
        
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
        
        setCompleted(true);
    }
}
