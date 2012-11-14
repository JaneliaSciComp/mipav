package gov.nih.mipav.model.algorithms;



import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.BitSet;
import java.util.concurrent.*;



public class AlgorithmMultiExponentialFitting extends AlgorithmBase {
    /**
     * This is a port of the Fortran program discrete.for version 2B (December, 1990) written by Stephen Provencher, PhD. 
     *  
     * The README.txt file has:
     * DISCLAIMER:  This software and its documentation are free and come "as is" with absolutely no guarantee and no support.
     * 
     * Applications:
     *    Analysis of multi-exponential decay data.
     *    Running in hundreds of laboratories on a wide variety of computers.
     *    
     * Methods:
     *    Fully automatic: no starting estimates needed for the number of exponentials or for their parameters;
     *    Modified Gauss-Newton least squares, with intensive searches from many starting points to find the global optimum;
     *    Methods in the references below are used to get starting estimates and to speed up the analysis:
     *    
     * References:
     *    S.W. Provencher: An eigenfunction expansion method for the analysis of exponential decay curves. J. Chem. Phys. 64, 
     *                                  pp. 2772-2777. (1976).
     *    S.W. Provencher & R.H. Vogel: Information loss with transform methods in system identification: 
     *                                  A new set of transforms with high information content. Math. Biosci. 50, pp. 251-262 (1980).
     *    S.W. Provencher & R.H. Vogel: Regularization techniques for inverse problems in molecular biology in: 
     *                                  Numerical Treatment of Inverse Problems in Differential and Integral Equations, 
     *                                  eds. P. Deuflhard & E. Hairer (Birkhauser, Boston, 1983), pp. 304-319.
     *    S.W. Provencher: A Fourier Method for the Analysis of Exponential Decay Curves, Biophysical Journal, Vol. 16,
     *                                  pp. 27-41 (1976). 
     *                                  
     * This program is used for the automatic analysis of multicomponent exponential decay for up to 9 components.
     * 
     * Download the Users Manual and other essential documentation from http://S-provencher.com.
     * 
     * runAlgorithm calls routines blockData, weight, fanlyz, yanlyz which in turn call lstSqr,
     * evar, varf, etheor, pivot, pivot1, anlerr, fisher, residu, plpres. 
     */
    
    private double dbloka[] = new double[5933];
    private double rbloka[] = new double[471];
    private int ibloka[] = new int[301];
    private int nmax = 200;
    private int nintmx = 10;
    private int iblokb[] = new int[12];
    private boolean lbloka[] = new boolean[45];
    private int mlammx = 9;
    
    // nlammx >=1 && nlammx <= mlammx
    private int nlammx;
    // iwt = +1, +-2, +-3, +4
    private int iwt;
    // mtry >= 1 && mtry <= 45
    private int mtry;
    private boolean last;
    // Read in t values if regint == false
    // Calculates t values from tstart, tend, and nt if regint == true
    private boolean regint;
    private boolean nobase;
    private boolean noneg;
    private boolean pry;
    private boolean prprel;
    private boolean prfinl;
    private boolean plotrs;
    private boolean repeat;
    // n > 2*nlammx+3 && n <= nmax
    private int n;
    // t has n values
    // Read in t values if regint == false
    private double t[];
    // nint >= 1 && nint <= nintmx
    private int nint;
    // tstart has nint values
    private double tstart[];
    private double tend;
    // nt has nint values
    // All values of the nt array must be >= 2
    private int nt[];
    // y has n values
    private double y[];
    // Special weights for iwt = +-4
    // sqrtw has n values
    private double sqrtw[];
    
    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    public AlgorithmMultiExponentialFitting(int nlammx, int iwt, int mtry, boolean last, boolean regint,
            boolean nobase, boolean noneg, boolean pry, boolean prprel, boolean prfinl, boolean plotrs,
            boolean repeat, int n, double t[], int nint, double tstart[], double tend, int nt[],
            double y[], double sqrtw[]) {
        this.nlammx = nlammx;
        this.iwt = iwt;
        this.mtry = mtry;
        this.last = last;
        this.regint = regint;
        this.nobase = nobase;
        this.noneg = noneg;
        this.pry = pry;
        this.prprel = prprel;
        this.prfinl = prfinl;
        this.plotrs = plotrs;
        this.repeat = repeat;
        this.n = n;
        this.t = t;
        this.nint = nint;
        this.tstart = tstart;
        this.tend = tend;
        this.nt = nt;
        this.y = y;
        this.sqrtw = sqrtw;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {
        // Original code has equivalence of e and gse
        double e[][] = new double[nmax][10];
        double gse[][] = new double[500][4];
        double ylyfit[] = new double[nmax];
        double deltat[] = new double[nintmx];
        double sqrtw[] = new double[nmax];
        int iwtsgn;
        int j;
        double dub;
        int l;
        int k;
        int ndime;
        int ndimg;
        
        iwtsgn = iwt;
        iwt = Math.abs(iwt);
        if (!regint) {
            // Reads in t values when regint == false
            nint = 1;
            tstart[0] = 0.0;
            deltat[0] = 0.0;
            nt[0] = n;
        } // if (!regint)
        else { // regint == true
            // Calculates t values from tstart, tend, and nt when regint == true
            for (j = 1; j <= nint; j++) {
                if (nt[j-1] < 2) {
                    MipavUtil.displayError("nt[" + (j-1) + "] = " + nt[j-1]  + " instead of being >= 2");
                    setCompleted(false);
                    return;
                }
                dub = (tend - tstart[j-1])/(nt[j-1] - 1.0);
                n++;
                t[n-1] = tstart[j-1];
                l = nt[j-1];
                deltat[j-1] = dub;
                for (k = 2; k <= l; k++) {
                    n++;
                    t[n-1] = t[n-2] + dub;
                } // for (k = 2; k <= l; k++)
            } // for (j = 1; j <= nint; j++)
        } // regint == true
        if (iwt != 4) {
            for (j = 0; j < n; j++) {
                Preferences.debug("t["+j+"] = " + t[j] + " y[" + j + "] = " + y[j] + "\n", Preferences.DEBUG_ALGORITHM);
            }
        }
        else { // iwt == 4
            // Uses special weights in sqrtw
            for (j = 0; j < n; j++) {
                // Why require this square root step here?
                sqrtw[j] = Math.sqrt(sqrtw[j]);
                Preferences.debug("t["+j+"] = " + t[j] + " y[" + j + "] = " + y[j] + 
                        " sqrtw[" + j + "] = " + sqrtw[j] + "\n", Preferences.DEBUG_ALGORITHM);
            }
        } // else iwt == 4
        
        if (!regint) {
            ndime = nmax;
            ndimg = 1;
        } // if (!regint)
        else {
            ndime = 1;
            ndimg = 500;
        }
        
        // weight generates crude starting values for constrained least square fits to raw data to generate
        // weights and does initial computations of quantities for later use.
        setCompleted(true);
        return;
    }
    
    private void weight(boolean finalVar, double timu[], double e[][], int ndime, double deltat[], double gse[][],
                        int dimg) {
        
    } // weight
    

}
