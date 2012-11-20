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
    
    // nmax must be greater than or equal to the maximum number of data points you will ever use
    private int nmax = 200;
    // nintmx must be greater than or equal to the maximum value of nint you will ever use wiht regint == true
    private int nintmx = 10;
    private int mlammx = 9;
    private double convrg = 5.0E-5;
    private int ngrid2[] = new int[]{0, 20, 12, 10, 10, 10, 10, 10, 12};
    private int minter = 7;
    private double sigmap[] = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                           0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    private double precis = 1.0E-7;
    
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
    private int routineCalled;
    private int evarCalled = 1;
    private int varfCalled = 2;
    private double trbase[] = new double[19];
    private double gf[][] = new double[19][126];
    private double dgfl[][] = new double[19][126];
    private double dgridf;
    private double gfstl;
    private double fhat[] = new double[19];
    private double varmin;
    private double sumwty;
    private double sumwt;
    private double zero[] = new double[10];
    private double dgride = 1.0;
    private double gestl;
    private double var[] = new double []{0.0};
    private double palpha[] = new double[10];
    private double plam[] = new double[10];
    private double se[][] = new double[10][10];
    private double plmtry[] = new double[10];
    private double deltap[] = new double[19];
    private double dfcapl[][] = new double[10][9];
    private double sfde[] = new double[9];
    private double adub[][] = new double[19][19];
    private double ddse[][] = new double[9][9];
    private double dtoler[] = new double[19];
    private double f[][] = new double[19][10];
    private double df[][] = new double[19][10];
    private double delta = 0.0;
    private double fhatmx = 0.0;
    private double lamst[] = new double[9];
    private double lamf[][][] = new double[9][9][2];
    private double lamnmx[][] = new double[2][2];
    private double sigyy;
    private double enphi;
    private double pcerr[] = new double[19];
    private double vgrid2[] = new double[252];
    private int jgeadd;
    private int nbinom[][] = new int[20][9];
    private int nvgrid[] = new int[9];
    private int nperg2[] = new int[9];
    private int mdima;
    private int ibase = 0;
    private int iwtsgn;
    private int nf;
    private int iter;
    private int jlamp1;
    private int nftry[] = new int[9];
    private boolean failed = false;
    private boolean wted;
    private boolean ffail[][] = new boolean[9][2]; 
    private boolean pivalp[] = new boolean[19];
    
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
            if (pry) {
                for (j = 0; j < n; j++) {
                    Preferences.debug("t["+j+"] = " + t[j] + " y[" + j + "] = " + y[j] + "\n", Preferences.DEBUG_ALGORITHM);
                }
            }
        }
        else { // iwt == 4
            // Uses special weights in sqrtw
            for (j = 0; j < n; j++) {
                // Why require this square root step here?
                sqrtw[j] = Math.sqrt(sqrtw[j]);
                if (pry) {
                    Preferences.debug("t["+j+"] = " + t[j] + " y[" + j + "] = " + y[j] + 
                            " sqrtw[" + j + "] = " + sqrtw[j] + "\n", Preferences.DEBUG_ALGORITHM);
                }
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
        int k;
        int j;
        double tn;
        double ddum;
        double dum = 0.0;
        double dddum;
        double t1 = 0.0;
        double t2 = 0.0;
        double t5;
        double tnl1;
        double t1lamx = 2.08;
        double tnlamn[] = new double[]{0.2, 0.02};
        int nparmx = 1;
        double num;
        double xrange = 1.0;
        double varbes;
        double ratiol;
        int jlam;
        int nlinwt = 0;
        double wtalp[] = new double[10];
        double wtlam[] = new double[10];
        double s;
        int L = 0;
        int i;
        double term;
        double errfit;
        double rgride = 1.0;
        double gest = 0.0;
        int nge1 = 1;
        int ngrid1 = 240;
        double gamma[] = new double[21];
        int ng2max = 20;
        int ngf;
        double rgridf;
        double gfst;
        double dub;
        double ddub;
        double dddub[] = new double[1];
        double mu[] = new double[19];
        double ds[] = new double[1];
        double dss[] = new double[1];
        double dsss[] = new double[1];
        int ii;
        int jj;
        boolean ldum;
        
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
                    if (failed || var[0] >= varbes) {
                        break;
                    }
                    varbes = var[0];
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
            jgeadd = minter/2;
            if (nobase) {
                jgeadd = jgeadd + (int)(Math.log(lamnmx[0][1]/lamnmx[0][0])/dgride + 0.5);
            }
            gest = lamnmx[0][1]/Math.pow(rgride,(jgeadd+1));
            gestl = Math.log(gest);
            nge1 = ngrid1 + jgeadd + (int)(Math.log(2.0 * lamnmx[1][0]/lamnmx[1][1])/dgride + 0.5) + minter/2;
            if (nge1 >  500) {
                Preferences.debug("Fix lamnmx.  nge1 = " + nge1 + "\n", Preferences.DEBUG_ALGORITHM);
                setCompleted(false);
                return;
            }
            jgeadd++;
            if (nlammx != 1) {
                for (j = 1; j < nlammx; j++) {
                    nperg2[j] = ngrid1/ngrid2[j];
                }
            } // if (nlammx != 1)
            gamma[0] = 1.0;
            for (j = 1; j <= ng2max; j++) {
                gamma[j] = j * gamma[j-1];
            }
            for (j = 1; j <= nlammx; j++) {
                for (k = j; k <= ng2max; k++) {
                    L = k - j + 1;
                    nbinom[k-1][j-1] = (int)(gamma[k-1]/(gamma[L-1]*gamma[j-1]) + 0.5);
                } // for (k = j; k <= ng2max; k++)
                if (j != 1) {
                    L = ngrid2[j-1] + 1;
                    k = L - j;
                    nvgrid[j-1] = (int)(gamma[L-1]/(gamma[j]*gamma[k-1]) + 0.5);
                } // if (j != 1)
            } // for (j = 1; j <= nlammx; j++)
            ngf = ngrid1/2 + 2*(minter/2);
            dgridf = Math.log(lamnmx[1][0]/lamnmx[0][0])/(double)(ngrid1/2 - 1);
            rgridf = Math.exp(dgridf);
            gfst = lamnmx[0][0]/Math.pow(rgridf, (minter/2+1));
            gfstl = Math.log(gfst);
            
            // Compute fhat (transforms of data) and gf and dgfl (interpolation grid points for transforms and their 
            // derivates with respect to log(lambda)
            if (regint) {
                for (j = 0; j < nint; j++) {
                    tstart[j] = tstart[j] + delta;
                }
            } // if (regint)
            dub = delta;
            for (j = 0; j < n; j++) {
                t[j] = t[j] + dub;
            }
            mu[0] = 6.283185/xrange;
            for (k = 1; k <= nparmx; k++) {
                if (k == 1) {
                    for (j = 0; j < n; j++) {
                        fhatmx = fhatmx + sqrtw[j] * Math.abs(y[j]);
                        ylyfit[j] = sqrtw[j];
                    } // for (j = 0; j < n; j++)
                } // if (k == 1)
                else { // k > 1
                    L = k / 2;
                    if (L + L >= k) {
                        dub = mu[0];
                        if (L > 1) {
                            dub = mu[L-2] + dub;
                        }
                        mu[L-1] = dub;
                        for (j = 0; j < n; j++) {
                            ylyfit[j] = Math.cos(Math.log(t[j])*dub) * sqrtw[j];
                        }
                    }
                    else { // L+L < k
                        dub = mu[L-1];
                        for (j = 0; j < n; j++) {
                            ylyfit[j] = Math.sin(Math.log(t[j]) * dub) * sqrtw[j];
                        }
                    } // else L+L < k
                } // else k > 1
                dub = 0.0;
                ddub = 0.0;
                for (j = 0; j < n; j++) {
                    dub = dub + y[j] * ylyfit[j];
                    ddub = ddub + ylyfit[j];
                } // for (j = 0; j < n; j++)
                fhat[k-1] = dub;
                trbase[k-1] = ddub;
                dub = gfst;
                for (L = 1; L <= ngf; L++) {
                    dub = dub * rgridf;
                    ds[0] = 0.0;
                    dss[0] = 0.0;
                    if (!regint) {
                        for (j = 0; j < n; j++) {
                            ddub = Math.exp(-dub*t[j]) * ylyfit[j];
                            ds[0] = ds[0] + ddub;
                            dss[0] = dss[0] - ddub * t[j];
                        } // for (j = 0; j < n; j++)
                    } // if (!regint)
                    else { // if regint
                        // Use recursion relations if regint == true
                        j = 0;
                        for (jj = 0; jj < nint; jj++) {
                            ddub = Math.exp(-dub * deltat[jj]);
                            dddub[0] = Math.exp(-dub * tstart[jj]);
                            ii = nt[jj];
                            for (i = 1; i <= ii; i++) {
                                j++;
                                dsss[0] = dddub[0] * ylyfit[j-1];
                                ds[0] = ds[0] + dsss[0];
                                dss[0] = dss[0] - t[j-1] * dsss[0];
                                dddub[0] = dddub[0] * ddub;
                            } // for (i = 1; i <= ii; i++)
                        } // for (jj = 0; jj < nint; jj++)
                    } // else if regint
                    gf[k-1][L-1] = ds[0];
                    dgfl[k-1][L-1] = dss[0] * dub;
                } // for (L = 1; L <= ngf; L++)
            } // for (k = 1; k <= nparmx; k++)
            if (prprel) {
                Preferences.debug("delta = " + delta + " fhat[0] = " + fhat[0] + "\n", Preferences.DEBUG_ALGORITHM);
                for (k = 0; k < nlammx; k++) {
                    Preferences.debug("mu["+k+"] = " + mu[k] + "\n",Preferences.DEBUG_ALGORITHM); 
                    Preferences.debug("Real transform part fhat["+(2*k-1)+"] = " + fhat[2*k-1] + "\n", Preferences.DEBUG_ALGORITHM);   
                    Preferences.debug("Imaginary transform part fhat["+(2*k)+"] = " + fhat[2*k] + "\n", Preferences.DEBUG_ALGORITHM);   
                }
            } // if (prprel)
            dub = delta;
            for (j = 0; j < n; j++) {
                t[j] = t[j] - dub;
            }
            if (regint) {
                for (j = 0; j < nint; j++) {
                    tstart[j] = tstart[j] - delta;
                }
            }
        } // if (!finalVar)
        if (wted) {
            dum = 0.0;
            sumwt = 0.0;
            sumwty = 0.0;
            for (j = 0; j < n; j++) {
                y[j] = y[j] * sqrtw[j];
                dum = dum + y[j] * y[j];
                if (!nobase) {
                    dub = sqrtw[j];
                    sumwt = sumwt + dub * dub;
                    sumwty = sumwty + dub * y[j];
                } // if (!nobase)
            } // for (j = 0; j < n; j++)
            varmin = dum * convrg * convrg;
        } // if (wted)
        if (finalVar || (!regint)) {
            return;
        }
        
        // Compute gse (interpolation grid points for exponential sums)
        dub = gest;
        L = nge1 - (int)(.69315/dgride);
        for (k = 1; k <= nge1; k++) {
            dub = dub * rgride;
            ldum = (k <= L);
            etheor(dub, true, wted, ds, dss, dsss, dddub, true, ldum);
            gse[k-1][0] = ds[0];
            gse[k-1][1] = dss[0];
            gse[k-1][2] = dsss[0];
            gse[k-1][3] = dddub[0];
        } // for (k = 1; k <= nge1; k++)
        return;
    } // weight
    
    private void lstsqr(double lamst[], int jlam, boolean rawdat, int n, int nlin, int routineCalled, int itype, boolean split,
                        boolean inter, boolean prlsq) {
        // routineCalled substituted for external vardum
        // routineCalled == evarCalled or routineCalled == varfCalled
        // Constrained least squares fit using stepwise regression approach.
        // n = number of points to be fit, i.e., = n (rawdat == true) or = 2*jlam+ibase (rawdat == false).
        // y, t = raw data, regardless of rawdat.
        // itype = 1, 2, 3, 4 for preliminary determination of weights, fit to raw data with interpolation,
        //      fit to transforms, and final fit to raw data respectively.
        
        // Calls either evar or varf.  Also calls pivot and anlerr
        // which in turn call pivot1, etheor.
        
        double qmin = 0.001;
        int mconv = 3;
        int nabort = 4;
        int mxiter[] = new int[]{30, 30, 40, 40};
        double rlammn[] = new double[]{1.1, 2.0};
        double dflat = -1.0E-2;
        int isplit;
        int nconv;
        int nvarup;
        int nflat;
        boolean allpiv[] = new boolean[2];
        int j;
        boolean pivlam[] = new boolean[9];
        double dum;
        int k;
        int L = 1;
        double varold;
        double varg[] = new double[1];
        double q[] = new double[1];
        double qg[] = new double[1];
        int ntry;
        boolean ldum[] = new boolean[1];
        int ierror[] = new int[1];
        double r[] = new double[10];
        double dub[] = new double[1];
        int nu;
        double ddum = 0.0;
        double tmp;
        double dalpl[][] = new double[10][9];
        int i;
        double ddub = 0.0;
        double array[][];
        double qmax[] = new double[1];
        double qt[] = new double[3];
        double vt[] = new double[3];
        boolean go630 = false;
        double qtvar[];
        int jk;
        
        isplit = 1;
        if (split) {
            isplit = 2;
        }
        jlamp1 = jlam + 1;
        nconv = 0;
        nvarup = 0;
        nflat = 0;
        allpiv[1] = true;
        if (nobase) {
            palpha[jlamp1-1] = 0.0;
        }
        plam[jlamp1-1] = 0.0;
        plmtry[jlamp1-1] = 0.0;
        pivalp[jlamp1-1] = true;
        
        // Put plam in ascending order and separate them if necessary
        for (j = 0; j < jlam; j++) {
            pivlam[j] = false;
            deltap[j] = 0.0;
        } // for (j = 0; j < jlam; j++)
        for (j = 1; j <= jlam; j++) {
            dum = 1.0E20;
            for (k = 1; k <= jlam; k++) {
                if (pivlam[k-1] || (lamst[k-1] > dum)) {
                    continue;
                }
                L = k;
                dum = lamst[k-1];
            } // for (k = 1; k <= jlam; k++)
            pivlam[L-1] = true;
            if (j == 1) {
                plam[j-1] = Math.max(dum,lamnmx[0][isplit-1]);
            }
            if (j > 1) {
                plam[j-1] = Math.min(Math.max(dum, rlammn[isplit-1] * plam[j-2]), lamnmx[1][0]);
            }
        } // for (j = 1; j <= jlam; j++)
        varold = 1.0E20;
        varg[0] = 1.0E20;
        iter = -1;
        q[0] = 0.0;
        if (prlsq) {
            
        } // if (prlsq)
        
        // Start of main loop for least squares fit
        bigloop:
        while (true) {
            ntry = 1;
            while (true) {
                iter++;
                while (true) {
                    if (routineCalled == evarCalled) {
                        evar(q, jlam, var, true, ldum, ylyfit, nlin, varg[0], ntry, inter, ierror);
                    }
                    else if (routineCalled == varfCalled) {
                        varf(q, jlam, var, true, ldum, ylyfit, nlin, varg[0], ntry, inter, ierror);    
                    }
                    if (ierror[0] == 5) {
                        break bigloop;
                    }
                    allpiv[0] = ldum[0];
                    if ((var[0] <= varg[0]) || (ntry == 2)) {
                        break;
                    }
                    ntry = 2;
                    q[0] = qg[0];
                } // while (true)
                for (j = 0; j < jlam; j++) {
                    r[j] = palpha[j];
                    if (!rawdat) {
                      r[j] = palpha[j]*Math.exp(-plmtry[j]*delta);  
                    } 
                    plam[j] = plmtry[j];
                } // for (j = 0; j < jlam; j++)
                dub[0] = var[0] - varold;
                if (prlsq) {
                    nu = Math.min(5, jlam);
                    ddum = 1.0;
                    if (dub[0] > 0.0) {
                        ddum = 2.0;
                    }
                    for (j = 0; j < jlam; j++) {
                        pcerr[j] = 1.0;
                        if (!pivlam[j]) {
                            pcerr[j] = 2.0;
                        }
                    } // for (j = 0; j < jlam; j++)
                    Preferences.debug("iter = " + iter + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("var[0] = " + var[0] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ddum = " + ddum + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("q[0] = " + q[0] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ntry = " + ntry + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("palpha[jlamp1-1] = " + palpha[jlamp1-1] + "\n", Preferences.DEBUG_ALGORITHM);
                    for (j = 0; j < nu; j++) {
                        Preferences.debug("r["+j+"] = " + r[j] + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("plam["+j+"] = " + plam[j] + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("pcerr["+j+"] = " + pcerr[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    } // for (j = 0; j < nu; j++)
                    if (jlam > 5) {
                        for (j = 5; j < jlam; j++) {
                            Preferences.debug("r["+j+"] = " + r[j] + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("plam["+j+"] = " + plam[j] + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("pcerr["+j+"] = " + pcerr[j] + "\n", Preferences.DEBUG_ALGORITHM);    
                        } // for (j = 5; j < jlam; j++)
                    } // if (jlam > 5)
                } // if (prlsq)
                
                // Tests for convergence and divergence
                tmp = convrg * fhatmx;
                if (!((Math.abs(dub[0]) > Math.max(varmin, varold*convrg) && rawdat) || 
                        (var[0] > nf*tmp*tmp && !rawdat))) {
                    nconv++;
                    if (nconv >= mconv || !rawdat) {
                        break bigloop;
                    }
                    else {
                        nvarup = 0;
                    }
                }
                else {
                    nconv = 0;
                    if (dub[0] <= 0.0) {
                        nvarup = 0;
                    }
                    else {
                        nvarup++;
                        if ((nvarup >= nabort) && (iter >= jlam)) {
                            ierror[0] = 1;
                            break bigloop;
                        }
                    }
                    
                    if (iter >= mxiter[itype-1]) {
                        ierror[0] = 2;
                        break bigloop;
                    }
                    if (dub[0]/varold < dflat || rawdat) {
                        nflat = 0;
                    }
                    else {
                        nflat++;
                        if (nflat >= nabort) {
                            ierror[0] = 3;
                            break bigloop;
                        }
                    }
                }
                // Finish computation of dfcapl (partial of fcap with respect to lambda)
                for (k = 0; k < jlam; k++) {
                    for (j = 0; j < nlin; j++) {
                        dfcapl[j][k] = dfcapl[j][k] * palpha[k];
                    } // for (j = 0; j < nlin; j++)
                    dfcapl[k][k] = dfcapl[k][k] + sfde[k];
                } // for (k = 0; k < jlam; k++)
                
                // Calculate dalpl (partial of palpha with respect to lambda) from matrix
                // product adub * dfcapl
                
                for (j = 0; j < nlin; j++) {
                    for (k = 0; k < jlam; k++) {
                        dalpl[j][k] = 0.0;
                    }
                } 
                
                for (k = 0; k < jlam; k++) {
                    if (!pivalp[k]) {
                        continue;
                    }
                    for (j = 0; j < nlin; j++) {
                        if (!pivalp[j]) {
                            continue;
                        }
                        dub[0] = 0.0;
                        for (i = 0; i < nlin; i++) {
                            if (pivalp[i]) {
                                dub[0] = dub[0] + adub[j][i] * dfcapl[i][k];
                            }
                        } // for (i = 0; i < nlin; i++)
                        dalpl[j][k] = dub[0];
                    } // for (j = 0; j < nlin; j++)
                } // for (k = 0; k < jlam; k++)
                
                // Calculate adub (normal equation matrix for nonlinear least squares)
                // and invert it
                
                if (rawdat) {
                    for (k = 1; k <= jlam; k++) {
                        for (j = 1; j <= k; j++) {
                            dub[0] = 0.0;
                            for (L = 1; L <= nlin; L++) {
                                dub[0] = dub[0] - dalpl[L-1][j-1] * dfcapl[L-1][k-1];
                            } // for (L = 1; L <= nlin; L++)
                            if (Math.abs(palpha[k-1]) <= Math.abs(palpha[j-1])) {
                                dub[0] = dub[0] + dalpl[j-1][k-1] * sfde[j-1];
                                dub[0] = dub[0] + dalpl[k-1][j-1] * sfde[k-1];
                            }
                            else {
                                dub[0] = dub[0] + dalpl[k-1][j-1] * sfde[k-1];
                                dub[0] = dub[0] + dalpl[j-1][k-1] * sfde[j-1];
                            }
                            ddub = palpha[k-1] * palpha[j-1] * ddse[j-1][k-1];
                            adub[j-1][k-1] = dub[0] + ddub;
                        } // for (j = 1; j <= k; j++)
                        dtoler[k-1] = ddub;
                        adub[k-1][jlamp1-1] = palpha[k-1]*sfde[k-1];
                    } // for (k = 1; k <= jlam; k++)
                } // if (rawdat)
                else { //!rawdat
                    for (j = 1; j <= jlam; j++) {
                        dtoler[j-1] = 0.0;
                        r[j-1] = 0.0;
                        for (k = j; k <= jlamp1; k++) {
                            adub[j-1][k-1] = 0.0;
                        } // for (k = j; k <= jlamp1; k++)
                    } // for (j = 1; j <= jlam; j++)
                    
                    for (nu = 1; nu <= n; nu++) {
                        for (k = 1; k <= jlam; k++) {
                            if (!pivalp[k-1]) {
                                continue;
                            }
                            dub[0] = 0.0;
                            for (j = 1; j <= nlin; j++) {
                                dub[0] = dub[0] + f[nu-1][j-1] * dalpl[j-1][k-1];
                            } // for (j = 1; j <= nlin; j++)
                            ddub = df[nu-1][k-1]*palpha[k-1];
                            dtoler[k-1] = dtoler[k-1] + ddub*ddub;
                            r[k-1] = dub[0] + ddub;
                        } // for (k = 1; k <= jlam; k++)
                        ddub = ylyfit[nu-1];
                        for (k = 1; k <= jlam; k++) {
                            if (!pivalp[k-1]) {
                                continue;
                            }
                            dub[0] = r[k-1];
                            for (j = 1; j <= k; j++) {
                                adub[j-1][k-1] = adub[j-1][k-1] + r[j-1]*dub[0];
                            } // for (j = 1; j <= k; j++)
                            adub[k-1][jlamp1-1] = adub[k-1][jlamp1-1] + r[k-1] * ddub;
                        } // for (k = 1; k <= jlam; k++)
                    } // for (nu = 1; nu <= n; nu++)
                } // else !rawdat
                for (j = 0; j < jlam; j++) {
                    dtoler[j] = precis * Math.max(dtoler[j], adub[j][j]);
                    r[j] = adub[j][jlamp1-1];
                } // for (j = 0; j < jlam; j++)
                adub[jlamp1-1][jlamp1-1] = 0.0;
                array = new double[mdima][mdima];
                for (i = 0; i < mdima; i++) {
                    for (j = 0; j < mdima; j++) {
                        array[i][j] = adub[i][j];
                    }
                }
                pivot(array, mdima, jlam, false, false, true, lamnmx[0][0], lamnmx[1][0], plam, dtoler, deltap, ldum, qg, 
                      pivlam, jlam, qmax, ierror);
                for (i = 0; i < mdima; i++) {
                    for (j = 0; j < mdima; j++) {
                        adub[i][j] = array[i][j];
                    }
                }
                if (ierror[0] == 5) {
                    break bigloop;
                }
                allpiv[1] = ldum[0];
                
                // Use modification-A of G. E. P. box to estimate best Q (fractional step size)
                
                varold = var[0];
                ddum  = 0.0;
                for (j = 0; j < jlam; j++) {
                    ddum = ddum + deltap[j] * r[j];
                }
                if (ddum <= 0.0) {
                    if (prlsq) {
                        Preferences.debug("Gauss vector pointing in wrong direction\n", Preferences.DEBUG_ALGORITHM);
                    }
                    ntry = 2;
                    q[0] = Math.min(qmin, qmax[0]);
                } // if (ddum <= 0.0)
                else {
                    break;
                }
            } // while (true)
            
            if (routineCalled == evarCalled) {
                evar(qg, jlam, varg, false, ldum, ylyfit, nlin, var[0], ntry, inter, ierror);
            }
            else if (routineCalled == varfCalled) {
                varf(qg, jlam, varg, false, ldum, ylyfit, nlin, var[0], ntry, inter, ierror);    
            }
            if (ierror[0] == 5) {
                break bigloop;
            }
            allpiv[0] = ldum[0];
            dum = varg[0] - varold + 2.0 * ddum * qg[0];
            if (dum <= 0.0) {
                dum = 0.25 * ddum * qg[0] * qg[0];
            }
            q[0] = Math.min(ddum * qg[0] * qg[0]/dum, Math.min(4.0, qmax[0]));
            if (varg[0] <= varold || (q[0] >= 0.125*qg[0] && rawdat)) {
                continue;
            }
        
        
            // Put quadratic parabola thru actual variance surface when a 
            // strong nonlinearity is indicated by a small q[0]
            qt[0] = Math.min(0.125 * qg[0], Math.max(q[0], 0.02));
            if (q[0] > 0.125 * qg[0]) {
                qt[0] = q[0];
            }
            if (routineCalled == evarCalled) {
                evar(qt, jlam, dub, false, ldum, ylyfit, nlin, var[0], ntry, inter, ierror);
            }
            else if (routineCalled == varfCalled) {
                varf(qt, jlam, dub, false, ldum, ylyfit, nlin, var[0], ntry, inter, ierror);    
            }
            if (ierror[0] == 5) {
                break bigloop;
            }
            vt[0] = dub[0];
            if (vt[0] >= varold) {
                qt[1] = 0.5 * qt[0];
                qt[1] = Math.min(qt[1], 0.05);
            } // if (vt[0] >= varold)
            else {
                qt[1] = 4.0 * qt[0];
                if (qt[0] > 0.125 * qg[0]) {
                    qt[1] = 0.0;
                    vt[1] = varold;
                    qt[2] = qg[0];
                    vt[2] = varg[0];
                    go630 = true;
                }
            }
            
            if (!go630) {
                qtvar = new double[1];
                qtvar[0] = qt[1];
                if (routineCalled == evarCalled) {
                    evar(qtvar, jlam, dub, false, ldum, ylyfit, nlin, var[0], ntry, inter, ierror);
                }
                else if (routineCalled == varfCalled) {
                    varf(qtvar, jlam, dub, false, ldum, ylyfit, nlin, var[0], ntry, inter, ierror);    
                }
                qt[1] = qtvar[0];
                if (ierror[0] == 5) {
                    break bigloop;
                }
                vt[1] = dub[0];
                if ((vt[0] < varold)  && (vt[1] < vt[0])) {
                    qt[2] = qg[0];
                    vt[2] = varg[0];
                }
                else {
                    qt[2] = 0.0;
                    vt[2] = varold;
                }
            } // if (!go630)
            go630 = false;
            varg[0] = Math.min(vt[0], Math.min(vt[1], varg[0]));
            if (vt[0] <= varg[0]) {
                qg[0] = qt[0];
            }
            if (vt[1] <= varg[0]) {
                qg[0] = qt[1];
            }
            dum = (vt[0] - vt[2])/(qt[0] - qt[2]);
            ddum = (dum - (vt[1] - vt[2])/(qt[1] - qt[2]))/(qt[0] - qt[1]);
            if (ddum > 0.0) {
                dum = 0.5 * ((qt[0] + qt[2])- dum/ddum);
                if (dum <= 0.0) {
                    q[0] = Math.min(q[0], Math.min(0.125*qt[1], 0.01));
                    continue;
                }
                q[0] = Math.min(dum, Math.min(8.0, qmax[0]));
                continue;
            } // if (ddum > 0.0)
            q[0] = Math.min(q[0], Math.min(0.125*qt[1], 0.01));
        } // while (true)
        
        // Check for abnormal exit and call anlerr
        failed = ierror[0] != 0 || !allpiv[0];
        if (rawdat) {
            failed = failed || !allpiv[1];
            if (!failed || itype == 4) {
                anlerr(jlam, nlin, itype, inter, prlsq);    
            }
        } // if (rawdat)
        if (prlsq && failed) {
            if (ierror[0] == 3) {
                Preferences.debug("Variance did not significantly decrease the last " + nabort + " times\n",
                        Preferences.DEBUG_ALGORITHM);
            }
            else if (ierror[0] == 2) {
                Preferences.debug("Maximum iterations without convergence\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (ierror[0] == 1) {
                Preferences.debug("Variance increased the last " + nvarup + " times\n", Preferences.DEBUG_ALGORITHM);    
            }
            else if (ierror[0] == 5) {
                Preferences.debug("Nearly singular normal equations matrix\n", Preferences.DEBUG_ALGORITHM);
            }
        } // if (prlsq && failed)
        
        // Put plam in ascending order
        for (j = 1; j <= jlam; j++) {
            dub[0] = 1.0E20;
            for (k = j; k <= jlam; k++) {
                if (plam[k-1] > dub[0]) {
                    continue;
                }
                L= k;
                dub[0] = plam[k-1];
            } // for (k = j; k <= jlam; k++)
            plam[L-1] = plam[j-1];
            plam[j-1] = dub[0];
            dub[0] = palpha[L-1];
            palpha[L-1] = palpha[j-1];
            palpha[j-1] = dub[0];
            if (!rawdat) {
                continue;
            }
            dum = sigmap[L-1];
            sigmap[L-1] = sigmap[j-1];
            sigmap[j-1] = dum;
            k = L + jlam;
            jk = j + jlam;
            dum = sigmap[k-1];
            sigmap[k-1] = sigmap[jk-1];
            sigmap[jk-1] = dum;
            pcerr[j-1] = 100.0 * sigmap[j-1]/plam[j-1];
            if (Math.abs(palpha[j-1]) > 0.0) {
                pcerr[jk-1] = 100.0 * sigmap[jk-1]/Math.abs(palpha[j-1]);
            }
        } // for (j = 1; j <= jlam; j++)
        return;
    } // lstsqr
    
    private void pivot(double adub[][], int mdima, int np, boolean onlyin, boolean invert, boolean constr, double pmin,
                       double pmax, double p[], double dtoler[], double deltap[], boolean allpiv[], double qg[],
                       boolean piv[], int jlam, double qmax[], int ierror[]) {
        // adub has dimension [mdima][mdima]
    } // pivot
    
    private void evar(double q[], int jlam, double var[], boolean invert, boolean allpiv[], double ylydum[], int nlin, double varg, int ntry,
                      boolean inter, int ierror[]) {
        double ylyfit;
        
    } // evar
    
    private void varf(double q[], int jlam, double var[], boolean invert, boolean allpiv[], double ylyfit[], int nlin, double varg, int ntry,
                      boolean intdum, int ierror[]) {
        
    } // varf
    
    private void anlerr(int jlam, int nlin, int itype, boolean inter, boolean prlsq) {
        
    } // anlerr
    
    private void etheor(double plambd, boolean invert, boolean wted, double se[], double dse[], double ddse[], double sye[],
                        boolean doe, boolean doye) {
        
    } // etheor
    
    private void fanlyz() {
        // Does constrained stepwise least squares analysis fo raw data (using grid search if necessary) to
        // get starting values for analysis of transforms, which yield lamf (starting values for final analysis
        // of raw data in yanlyz).
        
        // Calls routines lstsqr, evar
        // which in turn call pivot, pivot1, etheor, varf
        int llpeak = 1;
        int jlam;
        double vare;
        double vartry;
        int lvgrid = 1;
        int j;
        int jlaml1;
        int jtry;
        // Must have lpeak >= 1 
        int lpeak = 1;
        int L;
        int lendeq[] = new int[9];
        int jcentr[] = new int[20];
        boolean search;
        double vgmax = 0.0;
        int maxdif = 0;
        double vgbest = 0.0;
        int loc;
        int leq[] = new int[9];
        int lst2;
        int lst3;
        int lst4;
        int lst5;
        int lst6;
        int lst7;
        int lst8;
        int lst9;
        int mindif;
        int i = 0;
        int k;
        int lstart[][] = new int[9][45];
        boolean go493 = false;
        boolean go494 = false;
        int dif;
        double qdum[];
        double dub[] = new double[1];
        boolean ldum[] = new boolean[1];
        int ierror[] = new int[1];
        
        wted = iwt != 1;
        lamst[0] = Math.sqrt(lamnmx[1][1] * lamnmx[0][1]);
        
        // Start of main loop for stepwise analysis
        for (jlam = 1; jlam <= nlammx; jlam++) {
            // Initialize for fits assuming jlam components
            vare = 1.0E20;
            vartry = 1.0E20;
            nf = 2 * jlam + ibase;
            if (jlam != 1) {
                lvgrid = nvgrid[jlam-1];
                for (j = 0; j < lvgrid; j++) {
                    vgrid2[j] = 0.0;
                }
            } // if (jlam != 1)
            jlaml1 = jlam - 1;
            jlamp1 = jlam + 1;
            if (prprel) {
                Preferences.debug("Initial analysis of the raw data and then the transforms - assuming " + jlam + " components\n",
                        Preferences.DEBUG_ALGORITHM);
            } // if (prprel)
            
            // Start of loop for up to mtry jlam-component fits
            
            for (jtry = 1; jtry <= mtry; jtry++) {
                if (prprel) {
                    Preferences.debug("Fit using starting set " + jtry + "\n", Preferences.DEBUG_ALGORITHM);
                }
                if (jlam != 1) {
                if (jtry == 1) {
                    // Select first set of starting values
                    for (j = 0; j < jlaml1; j++) {
                        lamst[j] = lamf[j][jlaml1-1][1];
                    }
                    if (lpeak == 1) {
                        lamst[jlam-1] = Math.sqrt(lamnmx[0][1] * lamf[0][jlaml1-1][1]);
                    }
                    else if (lpeak == jlam) {
                        lamst[jlam-1] = Math.sqrt(lamf[jlaml1-1][jlaml1-1][1] * lamnmx[1][1]);
                    }
                    else {
                        lamst[jlam-1] = Math.sqrt(lamf[lpeak-2][jlaml1-1][1] * lamf[lpeak-1][jlaml1-1][1]);
                    }
                } // if (jtry == 1)
                else { // jtry >= 2
                    if (jtry == 2) {
                        // Do coarse grid search
                        L = ngrid2[jlam-1] - jlam;
                        for (j = 0; j < jlam; j++) {
                            L++;
                            lendeq[j] = L;
                        } // for (j = 0; j < jlam; j++)
                        if (jlam != mlammx) {
                            for (j = jlamp1; j <= mlammx; j++) {
                                lendeq[j-1] = lendeq[jlam-1];
                            }
                        } // if (jlam != mlammx)
                        jcentr[0] = jgeadd - 1 + (nperg2[jlam-1] + 1)/2;
                        L = ngrid2[jlam-1];
                        for (j = 1; j <= L-1; j++) {
                            jcentr[j] = jcentr[j-1] + nperg2[jlam-1];
                        }
                        search = true;
                    } // if (jtry == 2)
                    else { // jtry >= 3
                        // Find lowest variance on coarse grid
                        vgmax = 1.0E20;
                        for (j = 0; j < lvgrid; j++) {
                            vgmax = Math.min(vgmax, vgrid2[j]);
                        }
                        if (jtry > 2) {
                            vgmax = 10.0 * vgmax;
                        }
                        maxdif = -1;
                        search = false;
                        vgbest = 1.0E20;
                    } // else jtry >= 3
                    whileloop:
                    while (true) {
                        // Entry point to find starting lambda values from grid
                        loc = 0;
                        for (leq[0] = 1; leq[0] <= lendeq[0]; leq[0]++) {
                            lst2 = leq[0]+1;
                            for (leq[1] = lst2; leq[1] <= lendeq[1]; leq[1]++) {
                                lst3 = leq[1] + 1;
                                if (jlam < 3) {
                                    lst3 = lendeq[2];
                                }
                                for (leq[2] = lst3; leq[2] <= lendeq[2]; leq[2]++) {
                                    lst4 = leq[2] + 1;
                                    if (jlam < 4) {
                                        lst4 = lendeq[3];
                                    }
                                    for (leq[3] = lst4; leq[3] <= lendeq[3]; leq[3]++) {
                                        lst5 = leq[3] + 1;
                                        if (jlam < 5) {
                                            lst5 = lendeq[4];
                                        }
                                        for (leq[4] = lst5; leq[4] <= lendeq[4]; leq[4]++) {
                                            lst6 = leq[4] + 1;
                                            if (jlam < 6) {
                                                lst6 = lendeq[5];
                                            }
                                            for (leq[5] = lst6; leq[5] <= lendeq[5]; leq[5]++) {
                                                lst7 = leq[5] + 1;
                                                if (jlam < 7) {
                                                    lst7 = lendeq[6];
                                                }
                                                for (leq[6] = lst7; leq[6] <= lendeq[6]; leq[6]++) {
                                                    lst8 = leq[6] + 1;
                                                    if (jlam < 8) {
                                                        lst8 = lendeq[7];
                                                    }
                                                    for (leq[7] = lst8; leq[7] <= lendeq[7]; leq[7]++) {
                                                        lst9 = leq[7] + 1;
                                                        if (jlam < 9) {
                                                            lst9 = lendeq[8];
                                                        }
                                                        loop9:
                                                        for (leq[8] = lst9; leq[8] <= lendeq[8]; leq[8]++) {
                                                            loc++;
                                                            if (!search) {
                                                                if (vgrid2[loc-1] <= vgmax) {
                                                                    mindif = 1000;
                                                                    if (jtry <= 2) {
                                                                        go493 = true;
                                                                    }
                                                                    else { // jtry > 2
                                                                        L = jtry - 1;
                                                                        for (j = 2; j <= L; j++) {
                                                                            i = 0;  
                                                                            for (k = 1; k <= jlam; k++) {
                                                                                i = i + Math.abs(leq[k-1] - lstart[k-1][j-1]);    
                                                                            } // for (k = 1; k <= jlam; k++)
                                                                        } // for (j = 2; j <= L; j++)
                                                                        mindif = Math.min(mindif,  i);
                                                                        dif = mindif - maxdif;
                                                                        if (dif == 0) {
                                                                            go493 = true;    
                                                                        }
                                                                        else if (dif > 0) {
                                                                            go494 = true;
                                                                        }
                                                                    } // else jtry > 2
                                                                    if (go493) {
                                                                        go493 = false;
                                                                        if (vgrid2[loc-1] < vgbest) {
                                                                            go494 = true;
                                                                        }
                                                                    } // if (go493)
                                                                    if (go494) {
                                                                        go494 = false;
                                                                        vgbest = vgrid2[loc-1];
                                                                        maxdif = mindif;
                                                                        for (j = 1; j <= jlam; j++) {
                                                                            lstart[j-1][jtry-1] = leq[j-1];
                                                                        }
                                                                    } // if (go494)
                                                                } // if (vgrid2[loc-1] <= vgmax)
                                                                if (loc < lvgrid) {
                                                                    continue loop9;
                                                                }
                                                                for (j = 0; j < jlam; j++) {
                                                                    L = lstart[j][jtry-1];
                                                                    lamst[j] = Math.exp(gestl + jcentr[L-1] * dgride);
                                                                } // for (j = 0; j < jlam; j++)
                                                                break whileloop;
                                                            } // if (!search)
                                                            for (j = 0; j < jlam; j++) {
                                                                L = leq[j];
                                                                plam[j] = Math.exp(gestl + jcentr[L-1] * dgride);
                                                            } // for (j = 0; j < jlam; j++)
                                                            qdum = new double[1];
                                                            ierror[0] = j;
                                                            evar(qdum, jlam, dub, false, ldum, ylyfit, jlam+ibase, 1.0E20, 0, true, ierror);
                                                        } // for (leq[8] = lst9; leq[8] <= lendeq[8]; leq[8]++)
                                                    } // for (leq[7] = lst8; leq[7] <= lendeq[7]; leq[7]++)
                                                } // for (leq[6] = lst7; leq[6] <= lendeq[6]; leq[6]++)
                                            } // for (leq[5] = lst6; leq[5] <= lendeq[5]; leq[5]++)
                                        } // for (leq[4] = lst5; leq[4] <= lendeq[4]; leq[4]++)
                                    } // for (leq[3] = lst4; leq[3] <= lendeq[3]; leq[3]++)
                                } // for (leq[2] = lst3; leq[2] <= lendeq[2]; leq[2]++)
                            } // for (leq[1] = lst2; leq[1] <= lendeq[1]; leq[1]++)
                        } // (for leq[0] = 1; leq[0] <= lendeq[0]; leq[0]++)
                        // Find the lowest variance on the coarse grid
                        vgmax = 1.0E20;
                        for (j = 0; j < lvgrid; j++) {
                            vgmax = Math.min(vgmax, vgrid2[j]);
                        }
                        if (jtry > 2) {
                            vgmax = 10.0 * vgmax;
                        }
                        maxdif = -1;
                        search = false;
                        vgbest = 1.0E20;
                    } // while (true)
                } // else jtry >= 2
                } // if (jlam != 1)
            } // for (jtry = 1; jtry <= mtry; jtry++)
        } // for (jlam = 1; jlam <= nlammx; jlam++)
    } // fanlyz
    
    private void yanlyz() {
        
    } // yanlyz
    

}
