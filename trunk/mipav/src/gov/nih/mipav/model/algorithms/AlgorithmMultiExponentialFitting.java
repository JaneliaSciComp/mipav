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
    private double convrg = 5.0E-5;
    
    // nlammx >=1 && nlammx <= mlammx
    private int nlammx;
    // iwt = +1, +-2, +-3, +4
    private int iwt;
    // mtry >= 1 && mtry <= 45
    private int mtry;
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
    
    private double ylyfit[] = new double[nmax];
    // Original code has equivalence of e and gse
    private double e[][] = new double[nmax][10];
    private double gse[][] = new double[500][4];
    private int ndime;
    private double deltat[] = new double[nintmx];
    private int ndimg;
    private int iwtsgn;
    private int routineCalled;
    private int evarCalled = 1;
    private int varfCalled = 2;
    private boolean failed = false;
    private double var = 0.0;
    
    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    public AlgorithmMultiExponentialFitting(int nlammx, int iwt, int mtry, boolean regint,
            boolean nobase, boolean noneg, boolean pry, boolean prprel, boolean prfinl, boolean plotrs,
            boolean repeat, int n, double t[], int nint, double tstart[], double tend, int nt[],
            double y[], double sqrtw[]) {
        this.nlammx = nlammx;
        this.iwt = iwt;
        this.mtry = mtry;
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
        int j;
        double dub;
        int l;
        int k;
        
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
        weight(false);
        
        // fanlyz is for the constrained stepwise least squares analysis of the raw data and the transforms
        // to get starting estimates for the final least squares analysis of the raw data.
        fanlyz();
        
        // weight uses starting values from fanlyz for constrained least squares fits to raw data to generate
        // weights for the final least squares fits to the raw data.
        weight(true);
        
        // yanlyz is for the constrained stepwise least squares analysis of the raw data (using the starting 
        // values from fanlyz and the weights from weight).
        yanlyz();
        
        setCompleted(true);
        return;
    }
    
    private void weight(boolean finalVar) {
        // double timu[] in original code replaced by calling double ylyfit[]
        // Does initial least squares fits to raw data to generate weights, evaluates data transforms, transform
        // and exponential sums at grid points for interpolation, and other quantities for later use.
        // If finalVar == true, uses lamf (values from fanlyz).
        // If finalVar == false, generates its own crude starting lambdas.
        
        // Calls routines lstsqr, etheor
        // which in turn call evar, pivot, pivot1, anlerr
        double fhatmx;
        int mdima;
        int k;
        int j;
        double zero[] = new double[10];
        double tn;
        double ddum;
        double dum = 0.0;
        double dddum;
        double t1 = 0.0;
        double t2 = 0.0;
        double t5;
        double tnl1;
        double delta;
        double lamnmx[][] = new double[2][2];
        double t1lamx = 2.08;
        double tnlamn[] = new double[]{0.2, 0.02};
        int nparmx;
        int ibase = 0;
        double fhat[] = new double[19];
        double num;
        double xrange;
        double sumwt;
        double sumwty;
        double varmin;
        boolean wted;
        double varbes;
        double ratiol;
        int jlam;
        int nf;
        boolean ffail[][] = new boolean[9][2];
        double lamst[] = new double[9];
        double lamf[][][] = new double[9][9][2];
        int nlinwt = 0;
        double wtalp[] = new double[10];
        double palpha[] = new double[10];
        double wtlam[] = new double[10];
        double plam[] = new double[10];
        double s;
        int L = 0;
        int i;
        double term;
        double errfit;
        double dgride;
        double rgride;
        int jgeadd;
        double gest;
        double gestl;
        int nge1;
        int nperg2[] = new int[9];
        int ngrid1 = 240;
        int ngrid2[] = new int[9];
        double gamma[] = new double[21];
        int ng2max = 20;
        int nbinom[][] = new int[20][9];
        
        if (!finalVar) {
            fhatmx = 0.0; 
            mdima = 2*mlammx + 1;
            k = mlammx + 1;
            for (j = 1; j <= k; j++) {
                zero[j-1] = 0.0; 
            } // for (j = 1; j <= k; j++)
            tn = -1.0E20;
            ddum = -1.0E20;
            for (k = 1; k <= 5; k++) {
                dum = 1.0E20;
                for (j = 1; j <= n; j++) {
                    dddum = t[j-1];
                    if (k == 1) {
                        tn = Math.max(tn,dddum);
                    }
                    if ((dddum > ddum) && (dddum < dum)) {
                        dum = dddum;
                    }
                } // for (j = 1; j <= n; j++)
                ddum = dum + 1.0E-5*Math.abs(dum);
                if (k == 1) {
                    t1 = dum;
                }
                if (k == 2) {
                    t2 = dum;
                }
            } // for (k = 1; k <= 5; k++)
            t5 = dum;
            tnl1 = -1.0E20;
            dum = tn - 1.0E-5 * Math.abs(tn);
            for (j = 1; j <= n; j++) {
                if (t[j-1] < dum) {
                    tnl1 = Math.max(tnl1, t[j-1]);
                }
            } // for (j = 1; j <= n; j++)
            dum = 0.25*(t5-t1);
            delta = dum - t1;
            // lamnmx[j-1][k-1] = min. (j == 1) and max. (j == 2) allowed values of lambda
            // for least squares parameters (k == 1) and for starting values (k == 2).
            lamnmx[1][0] = t1lamx/(t1 + delta);
            lamnmx[1][1] = Math.min(lamnmx[1][0], 0.693/(t1 + delta));
            lamnmx[0][1] = tnlamn[0]/(tn + delta);
            if (!nobase) {
                nparmx = 2*nlammx + 1;
                ibase = 1;
                lamnmx[0][0] = lamnmx[0][1];
            } // if (!nobase)
            else { // nobase == true
                ibase = 0;
                nparmx = 2 * nlammx;
                fhat[nparmx] = 0.0;
                lamnmx[0][0] = tnlamn[1]/(tn + delta);
            } // else nobase == true
            // If necessary, increase delta so that distance between first two points on x = log(t + delta) axis
            // is less than a half-period of the highest frequency used in the transforms
            while (true) {
                num = tn + delta;
                num = num * num;
                xrange = Math.log(num/((t1 + delta)* (tnl1 + delta)));
                if (Math.log((t2 + delta)/(t1 + delta)) * (2.0 * nlammx) < xrange) {
                    break;
                }
                delta = delta + dum;
            } // while (true)
        } // if (!finalVar)
        else { // finalVar == true
            if ((iwt == 1) || (iwt == 4)) {
                return;
            }
            for (j = 0; j < n; j++) {
                y[j] = y[j]/sqrtw[j];  
            } 
        } // else finalVar == true
        if (iwt != 4) {
            sumwt = (double)n;
            sumwty = 0.0;
            dum = 0.0;
            for (j = 0; j < n; j++) {
                sumwty = sumwty + y[j];
                dum = dum + y[j] * y[j];
                sqrtw[j] = 1.0;
            } // for (j = 0; j < n; j++)
            varmin = dum * convrg * convrg;
            if (iwt != 1) {
                wted = false;
                varbes = 1.0E20;
                ratiol = lamnmx[1][1]/lamnmx[0][1];
                
                // Start of main loop for stepwise analysis
                for (jlam = 1; jlam <= nlammx; jlam++) {
                    nf = 2*jlam + ibase;  
                    Preferences.debug("Preliminary analysis to determine weights\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Analysis assuming " + jlam + " components\n", Preferences.DEBUG_ALGORITHM);
                    if (!finalVar) {
                        ffail[jlam-1][0] = true;
                        dum = Math.pow(ratiol, 1.0/(jlam+1.0));
                        lamst[0] = lamnmx[0][1] * dum;
                        if (jlam != 1) {
                            for (j = 2; j <= jlam; j++) {
                                lamst[j-1] = lamst[j-2] * dum;
                            } // for (j = 2; j <= jlam; j++)
                        } // if (jlam != 1)
                    } // if (!finalVar)
                    else { // finalVar == true
                        for (j = 1; j <= jlam; j++) {
                            lamst[j-1] = lamf[j-1][jlam-1][0];    
                        } // for (j = 1; j <= jlam; j++)
                    } // else finalVar == true
                    lstsqr(lamst, jlam, true, n, (jlam + ibase), evarCalled, 1, ffail[jlam-1][0], false, prprel);
                    if (failed || var >= varbes) {
                        break;
                    }
                    varbes = var;
                    nlinwt = jlam + ibase;
                    for (j = 0; j < nlinwt; j++) {
                        wtalp[j] = palpha[j];
                        wtlam[j] = plam[j];
                    } // for (j = 0; j < nlinwt; j++)
                    if (iwtsgn < 0) {
                        nlinwt = jlam;
                    }
                } // for (jlam = 1; jlam <= nlammx; jlam++)
                if (varbes >= 1.0E20) {
                    Preferences.debug("Component analysis to determine weights somehow failed\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Unit weights will be used\n", Preferences.DEBUG_ALGORITHM);
                    iwt = 1;
                }
                else {// varbes < 1.0E20
                    // Calculate weights including errfit (standard error in region of the theoretical curve where it
                    // is closest to y = 0.0, where a weight could otherwise be disastrously large if errfit were not
                    // accounted for).
                    dum = 1.0E20;
                    for (k = 1; k <= n; k++) {
                        s = 0.0;
                        ddum = t[k-1];
                        for (j = 0; j < nlinwt; j++) {
                            s = s + wtalp[j] * Math.exp(-ddum * wtlam[j]);
                        }
                        sqrtw[k-1] = s;
                        if (Math.abs(s) < dum) {
                            dum = Math.abs(s);
                            L = k;
                        }
                    } // for (k = 1; k <= n; k++)
                    k = Math.min(n, L+5);
                    i = Math.max(1, k-9);
                    dum = 0.0;
                    ddum = 0.0;
                    if ((iwtsgn < 0)  && (!nobase)) {
                        ddum = wtalp[nlinwt];
                    }
                    for (j = i; j <= k; j++) {
                        term = y[j-1] - sqrtw[j-1] - ddum;
                        dum = dum + term * term;
                    } // for (j = i; j <= k; j++)
                    errfit = Math.sqrt(dum/(k - i + 1.0));
                    for (k = 0; k < n; k++) {
                        sqrtw[k] = 1.0/(Math.abs(sqrtw[k]) + errfit);
                        if (iwt == 2) {
                            sqrtw[k] = Math.sqrt(sqrtw[k]);
                        }
                    } // for (k = 0; k < n; k++)
                    L = 1;
                    if (finalVar) {
                        L = 2;
                    }
                    if (L == 1) {
                        Preferences.debug("Parameters used to generate weights for transforms\n", Preferences.DEBUG_ALGORITHM);
                    }
                    else if (L == 2) {
                        Preferences.debug("Parameters used to generate weights for raw data\n", Preferences.DEBUG_ALGORITHM);    
                    }
                    Preferences.debug("errfit, the uncertainty term added to absolute values of theoretical curve = " + errfit + "\n",
                                      Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("lambda = wtlam  alpha = wtalp\n", Preferences.DEBUG_ALGORITHM);
                    for (k = 0; k < nlinwt; k++) {
                        Preferences.debug("wtlam["+k+"] = " + wtlam[k] + "  wtalp[" + k + "] = " + wtalp[k] + "\n", 
                                           Preferences.DEBUG_ALGORITHM);
                    }
                } // else varbes < 1.0E20
            } // if (iwt != 1)
        } // if (iwt != 4)
        wted = (iwt != 1);
        if (!finalVar) {
            // Initial computation of quantities to be used later
            dgride = Math.log(lamnmx[1][1]/lamnmx[0][1])/(ngrid1 - 1.0);
            rgride = Math.exp(dgride);
            //jgeadd = minter/2;
        } // if (!finalVar)
    } // weight
    
    private void lstsqr(double lamst[], int jlam, boolean rawdat, int n, int nlin, int routineCalled, int itype, boolean split,
                        boolean inter, boolean prlsq) {
        // routineCalled substituted for external vardum
        // routineCalled == evarCalled or routineCalled == varfCalled
        
    } // lstsqr
    
    private void fanlyz() {
        
    } // fanlyz
    
    private void yanlyz() {
        
    } // yanlyz
    

}
