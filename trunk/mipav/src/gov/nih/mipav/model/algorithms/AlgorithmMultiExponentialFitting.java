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
     *    I) S.W. Provencher: A Fourier Method for the Analysis of Exponential Decay Curves, Biophysical Journal, Vol. 16,
     *                                  pp. 27-41 (1976). 
     *    II)S.W. Provencher: An eigenfunction expansion method for the analysis of exponential decay curves. J. Chem. Phys. 64, 
     *                                  pp. 2772-2777. (1976).
     *    III)S.W. Provencher & R.H. Vogel: Information loss with transform methods in system identification: 
     *                                  A new set of transforms with high information content. Math. Biosci. 50, pp. 251-262 (1980).
     *    IV)S.W. Provencher & R.H. Vogel: Regularization techniques for inverse problems in molecular biology in: 
     *                                  Numerical Treatment of Inverse Problems in Differential and Integral Equations, 
     *                                  eds. P. Deuflhard & E. Hairer (Birkhauser, Boston, 1983), pp. 304-319.
     *                                  
     * This program is used for the automatic analysis of multicomponent exponential decay for up to 9 components.
     * 
     * Download the Users Manual and other essential documentation from http://S-provencher.com.
     * 
     * The data is represented by y[k] = sum from j = 0 to nlambda of alpha[j]*exp(-lambda[j] * t[k]), k = 1, 2, ..., n,
     * with nlambda varied from 1 to nlammx with nlammx <= mlammx = 9. 
     * Provision can be make for an unknown baseline component alpha[0] with lambda[0] = 0.
     * It is completely automatic in that only the raw data y[k] and t[k] are input; no potentially biased initial guesses
     * at alpha[j], lambda[j], or even the number of terms nlambda are needed or even allowed.
     * 
     * There is no need to normalize any of the input data like tstart, tend, t, y, or sqrtw.  However, the number 1.0E20
     * has been used throughout the program to represent an infinitely large number and hence nothing should get larger than
     * 1.0E20.  Since some of the values are squared and summed over n, it is best to choose units so that all input values
     * are between 1.0E-9 and 1.0E6 in absolute value.
     * 
     * runAlgorithm calls routines weight, fanlyz, yanlyz which in turn call lstSqr,
     * evar, varf, etheor, pivot, pivot1, anlerr, fisher, residu. 
     */
    
    // nmax must be greater than or equal to the maximum number of data points you will ever use
    // Since this version does not use last = false to get more data, nmax is not required.
    // Just used n, the number of data points, instead
    //private int nmax = 200;
    // nintmx must be greater than or equal to the maximum value of nint you will ever use with regint == true
    private int nintmx = 10;
    private int mlammx = 9;
    private double convrg = 5.0E-5;
    private int ngrid2[] = new int[]{0, 20, 12, 10, 10, 10, 10, 10, 12};
    private int minter = 7;
    private double sigmap[] = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                           0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    private double precis = 1.0E-7;
    
    // nlammx >=1 && nlammx <= mlammx
    // In each run the number of terms nlambda is varied from 1 to nlammx in looking for the best fit.
    private int nlammx;
    // iwt = +1, +-2, +-3, +4
    // iwt = 1 for the normal (unweighted) case of unit least squares weights w[k]; i.e., when sigma(y[k]),
    // the expected error in y[k] is independent of k.
    // iwt = 2 for w[k] = [abs(sum from j = 0 to nlambda of alpha[j]*exp(-lambda[j]*t[k])) + ERRFIT]**-1,
    // i.e., sigma(y[k]) is proportional to sqrt(y(t[k])), where ERRFIT is described below and y(t[k]) is
    // the exact value without random experimental error.  This error is appropriate (to a good approximation)
    // for many counting and correlation experiments.
    // iwt = -2 for w[k] = [abs(sum from j = 1 to nlambda of alpha[j]*exp(-lambda[j]*t[k])) + ERRFIT]**-1,
    // i.e., sigma(y[k]) = is proportional to sqrt(y(t[k]) - alpha[0])
    // iwt = 3 for w[k] =  [abs(sum from j = 0 to nlambda of alpha[j]*exp(-lambda[j]*t[k])) + ERRFIT]**-2,
    // i.e., sigma(y[k]) is proportional to y(t[k])
    // iwt = -3 for w[k] =  [abs(sum from j = 1 to nlambda of alpha[j]*exp(-lambda[j]*t[k])) + ERRFIT]**-2,
    // i.e., sigma(y[k]) is proportional to y(t[k]) - alpha[0]
    // iwt = 4 if you are going to input your own w[k]
    // Thus, when iwt = +-2 or +-3, the w[k] are calculated from a smooth least squares fit to the y[k], 
    // rather than directly from y[k], which would lead to erratic and biased weights.  However, even the
    // smooth curve could lead to a disastrously large w[k] if it happened to come very close to the x
    // axis near a t[k].  ERRFIT is the standard deviation of the fit to the 10 y[k] in the interval where
    // the least squares curve is closest to the x axis, and eliminates this danger.
    private int iwt = 1;
    // mtry >= 1 && mtry <= 45
    // mtry = the maximum number of tries that will be made to find a solution for a single value of nlambda
    // during the preliminary analyses of the transforms.  If the first try fails, a grid search is performed
    // in (lambda[j]-) parameter space and up to mtry-1 tries are started from points on this grid.  You must
    // input 1 <= mtry <= 45.
    // However, these limits are extreme values; e.g., for mtry = 1, no grid search is performed at all.  
    // DISCRETE was tested on the 24 data sets used in references I and II, as well as on 15 more difficult cases that
    // could only be successfully analyzed by DISCRETE.  Of these 39 unusually difficult cases, only 3 needed
    // mtry between 11 and 13; all others worked with mtry = 5 and nearly all with mtry = 3.  During stages
    // when mtry is searching for more components than there actually are, the computer time is almost
    // directly proportional to mtry, since mtry unsuccessful searches are often performed.  Thus, if the size
    // of the problem makes computer time a major consideration, mtry = 5 should be adequate.  If reliability
    // and thoroughness are of utmost importance an mtry equals about 15 can be used.  An mtry = 45 would be
    // practically always a great waste of computer time.
    private int mtry = 15;
    // Read in t values if regint == false
    // Calculates t values from tstart, tend, and nt if regint == true
    // regint == true if the data points are in "regular intervals" of t; i.e., if the y[k] and t[k] are read in
    // nint groups such that for each group L there are nt[L] t[k] running in equal increments between tstart[L]
    // and tend[L].  For example, if the t[k] are 1,2, 4,6,8,10, 20,30,40,50, 100,200,300,400,500
    // then you could input regint = true, nint = 4,
    //         L                 tstart              tend             nt
    //         1                   1.0                 2.0             2
    //         2                   4.0                10.0             4
    //         3                  20.0                50.0             4
    //         4                 100.0               500.0             5
    private boolean regint;
    // nobase = true if it is known apriori that there is no baseline alpha[0].
    private boolean nobase;
    // noneg - true if it is known apriori that alpha[j] >= 0 for j > 0; e.g., if the alpha[j] correspond to 
    // concentrations or populations.  If there is a baseline component, alpha[0] is not so constrained.
    private boolean nonneg;               
    // pry = true if the input values of y[k], t[k], and (if least squares weights w[k] are read in),
    // sqrt(w[k]) are to be printed out
    private boolean pry;
    // prprel = true if the results of each iteration of the preliminary analyses are to be printed out.
    private boolean prprel;
    // prfinl = true if the results of each iteration of the final analyses of the raw data are to be printed out.
    private boolean prfinl;
    // plotrs = true if the results of the fit of the final solution to the raw data are to be plotted by the printer
    // plotrs is not implemented yet.
    private boolean plotrs;
    // repeat = true if the final summary of the results is to be printed a second time
    private boolean repeat;
    private boolean fatalProblem = false;
    // n only needed if regint = false
    // n is the total number of data points
    // n > 2*nlammx+3 && n <= nmax
    private int n;
    // t has n values
    // Read in t values if regint == false
    // t only needed if regint == false, and in this cases they need not be in any special order and need not have any
    // regular spacing (duplicate values are even permitted).
    private double t[];
    // nint >= 1 && nint <= nintmx
    // nint only needed if regint == true
    private int nint;
    // tstart has nint values
    // tstart only needed if regint == true
    private double tstart[] = new double[1];
    // tend has nint values
    // tend only needed if regint == true
    private double tend[];
    // nt has nint values
    // All values of the nt array must be >= 2
    // nt[0] = n when regint == false
    private int nt[] = new int[1];
    // y has n values
    // y always needed
    // array of data points y[k] measured at t[k]
    private double y[];
    // Special weights for iwt = +4
    // sqrtw has n values
    private double sqrtw[];
    
    //private double ylyfit[] = new double[nmax];
    private double ylyfit[] = new double[n];
    // Original code has equivalence of e and gse
    //private double e[][] = new double[nmax][10];
    private double e[][];
    // ndimg is only used in establishing the first dimension of gse
    // if regint == false, ndimg = 1.  If regint ==  true, ndimg = 200.
    //private int ndimg;
    private double gse[][];
    // ndime is only used in establishing the first dimension of e[][]
    // If regint == false, ndime == n.  If regint = true, ndime = 1.
    //private int ndime;
    private double deltat[] = new double[nintmx];
   
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
            boolean nobase, boolean nonneg, boolean pry, boolean prprel, boolean prfinl, boolean plotrs,
            boolean repeat, int n, double t[], int nint, double tstart[], double tend[], int nt[],
            double y[], double sqrtw[]) {
        this.nlammx = nlammx;
        this.iwt = iwt;
        this.mtry = mtry;
        this.regint = regint;
        this.nobase = nobase;
        this.nonneg = nonneg;
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
    
    public AlgorithmMultiExponentialFitting() {
        
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------
    
    public void selfTest() {
        // The raw data for data sets 1 and 2 is the same.  It was stimulated using alpha[j] and lambda[j]
        // parameters of Example II of References I and II for j = 1,2,3.  However, noise with a constant
        // rms error = 0.01[y[0] - alpha[0]] = 0.03395 was added instead of the 5% rms error of example II.
        // The baseline error was alpha[0] = -0.207.  Hence data set 1 has the appropriate weighting (i.e., unweighted)
        // with iwt = 1, while set 2 has an incorrect iwt = -3.  In set 2, regint = false in order to test the
        // corresponding part of discrete and illustrate the input format for this case;  however, this is actually
        // a great waste of computer time.
        
        // Example 2 has:
        // alpha[0] = -0.214    lambda[0] = 0.0
        // alpha[1] = 1.029     lambda[1] = 0.0017
        // alpha[2] = 1.176     lambda[2] = 0.0105
        // alpha[3] = 1.190     lambda[3] = 0.0798
        // RMS, root mean square error 0.05t[t]
        // data weighting [y[t]]**-2
        // Number of equally spaced points in t range      Start of t range    End of t range
        //                10                                   0                   45
        //                30                                  75                  945
        // n, total number of data points = 40
        // The lambda[j] are always subscripted in increasing order.  Thus lambda[1]*t[n] measures how
        // completely the decay of the longest-lived component has been observed.  lambda[1]*t[n] = 1.6
        // Test data set 1 with proper unit weighting and normally comprehensive output
        // When iwt = -3 in the second set is changed to iwt = 1 there is a very close match between the 
        // regint = true and regint = false outputs.
        regint = true;
        nobase = false;
        nonneg = false;
        pry = true;
        prprel = true;
        prfinl = true;
        plotrs = true;
        repeat = true;
        nlammx = 4;
        iwt = 1;
        mtry = 5;
        nint = 2;
        tstart = new double[nint];
        tend = new double[nint];
        nt = new int[nint];
        tstart[0] = 0.0;
        tend[0] = 45.0;
        nt[0] = 10;
        tstart[1] = 75.0;
        tend[1] = 945.0;
        nt[1] = 30;
        n = 40;
        y = new double[]{3.17951, 2.72385, 2.38005, 2.17513, 1.96817,
                         1.83133, 1.71227, 1.66732, 1.60041, 1.52702,
                         1.28123, 1.03639, 0.911274, 0.793438, 0.704310,
                         0.649749, 0.595533, 0.499157, 0.461291, .0351978,
                         0.381508, 0.328100, 0.273046, 0.263287, 0.266722,
                         0.248385, 0.177709, 0.133162, 0.155815, 8.17959E-2,
                         0.103332, 0.161067, 7.68593E-2, 0.119229, 6.39275E-2,
                         5.87735E-3, 1.99340E-2, 2.95760E-2, 0.0, 2.92465E-2};
        runAlgorithm();
        
        // Test data set 2 with incorrect weighting and most comprehensive output
        regint = false;
        nobase = false;
        nonneg = false;
        pry = true;
        prprel = true;
        prfinl = true;
        plotrs = true;
        repeat = true;
        nlammx = 4;
        iwt = -3;
        //iwt = 1;
        mtry = 5;
        n = 40;
        t = new double[]{0.0, 5.0, 10.0, 15.0, 20.0,
                         25.0, 30.0, 35.0, 40.0, 45.0,
                         75.0, 105.0, 135.0, 165.0, 195.0,
                         225.0, 255.0, 285.0, 315.0, 345.0,
                         375.0, 405.0, 435.0, 465.0, 495.0,
                         525.0, 555.0, 585.0, 615.0, 645.0,
                         675.0, 705.0, 735.0, 765.0, 795.0,
                         825.0, 855.0, 885.0, 915.0, 945.0};
        
        y = new double[]{3.17951, 2.72385, 2.38005, 2.17513, 1.96817,
                1.83133, 1.71227, 1.66732, 1.60041, 1.52702,
                1.28123, 1.03639, 0.911274, 0.793438, 0.704310,
                0.649749, 0.595533, 0.499157, 0.461291, .0351978,
                0.381508, 0.328100, 0.273046, 0.263287, 0.266722,
                0.248385, 0.177709, 0.133162, 0.155815, 8.17959E-2,
                0.103332, 0.161067, 7.68593E-2, 0.119229, 6.39275E-2,
                5.87735E-3, 1.99340E-2, 2.95760E-2, 0.0, 2.92465E-2};
        runAlgorithm();
    }

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
        int L;
        int k;
        int index;
        
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
            n = nt[0];
            for (j = 2; j <= nint; j++) {
                n += nt[j-1];    
            }
            t = new double[n];
            index = 0;
            for (j = 1; j <= nint; j++) {
                if (nt[j-1] < 2) {
                    MipavUtil.displayError("nt[" + (j-1) + "] = " + nt[j-1]  + " instead of being >= 2");
                    setCompleted(false);
                    return;
                }
                dub = (tend[j-1] - tstart[j-1])/(nt[j-1] - 1.0);
                index++;
                t[index-1] = tstart[j-1];
                L = nt[j-1];
                deltat[j-1] = dub;
                for (k = 2; k <= L; k++) {
                    index++;
                    t[index-1] = t[index-2] + dub;
                } // for (k = 2; k <= l; k++)
            } // for (j = 1; j <= nint; j++)
        } // regint == true
        ylyfit = new double[n];
        if (iwt != 4) {
            sqrtw = new double[n];
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
            //ndime = nmax;
            e = new double[n][10];
            gse = new double[1][4];
        } // if (!regint)
        else {
            e = new double[1][10];
            gse = new double[500][4];
        }
        
        // weight generates crude starting values for constrained least square fits to raw data to generate
        // weights and does initial computations of quantities for later use.
        weight(false);
        if (fatalProblem) {
            setCompleted(false);
            return;
        }
        
        // fanlyz is for the constrained stepwise least squares analysis of the raw data and the transforms
        // to get starting estimates for the final least squares analysis of the raw data.
        fanlyz();
        
        // weight uses starting values from fanlyz for constrained least squares fits to raw data to generate
        // weights for the final least squares fits to the raw data.
        weight(true);
        if (fatalProblem) {
            setCompleted(false);
            return;
        }
        
        // yanlyz is for the constrained stepwise least squares analysis of the raw data (using the starting 
        // values obtained from fanlyz and the weights from weight).
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
                    if (prprel) {
                        Preferences.debug("Preliminary analysis to determine weights\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("Analysis assuming " + jlam + " components\n", Preferences.DEBUG_ALGORITHM);
                    } // if (prprel)
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
                    Preferences.debug("1-component analysis to determine weights somehow failed\n", Preferences.DEBUG_ALGORITHM);
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
            gest = lamnmx[0][1]/Math.pow(rgride,(jgeadd+1.0));
            gestl = Math.log(gest);
            nge1 = ngrid1 + jgeadd + (int)(Math.log(2.0 * lamnmx[1][0]/lamnmx[1][1])/dgride + 0.5) + minter/2;
            if (nge1 >  500) {
                Preferences.debug("Fix lamnmx.  nge1 = " + nge1 + "\n", Preferences.DEBUG_ALGORITHM);
                fatalProblem = true;
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
                for (k = 1; k < nlammx; k++) {
                    Preferences.debug("mu["+(k-1)+"] = " + mu[k-1] + "\n",Preferences.DEBUG_ALGORITHM); 
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
        //if (prlsq) {
            
        //} // if (prlsq)
        
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
                    Preferences.debug("damping q q[0] = " + q[0] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("ntry = " + ntry + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("baseline palpha[jlamp1-1] = " + palpha[jlamp1-1] + "\n", Preferences.DEBUG_ALGORITHM);
                    for (j = 0; j < nu; j++) {
                        Preferences.debug("alpha r["+j+"] = " + r[j] + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("lambda plam["+j+"] = " + plam[j] + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("pcerr["+j+"] = " + pcerr[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    } // for (j = 0; j < nu; j++)
                    if (jlam > 5) {
                        for (j = 5; j < jlam; j++) {
                            Preferences.debug("alpha r["+j+"] = " + r[j] + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("lambda plam["+j+"] = " + plam[j] + "\n", Preferences.DEBUG_ALGORITHM);
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
                pivot(adub, mdima, jlam, false, false, true, lamnmx[0][0], lamnmx[1][0], plam, dtoler, deltap, ldum, qg, 
                      pivlam, jlam, qmax, ierror);
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
        // Does Gauss-Jordan pivots on all np diagonal elements of augmented matrix adub except those that have
        // a tolerance less than dtoler or that cause a violation of the constraint pmin <= p <= pmax.
        // Only needs full upper triangle of matrix on input, and overwrites it with inverse on solution.
        // Calls pivot1
        int npp;
        int j;
        int L = 1;
        int k;
        int npiv;
        double dum = 0.0;
        double ddum;
        boolean go300 = false;
        double rdelta;
        
        ierror[0] = 0;
        qg[0] = 1.0;
        qmax[0] = 999.0;
        npp = np + 1;
        if (onlyin) {
            npp = np;
        }
        for (j = 0; j < np; j++) {
            piv[j] = false;
        } // for (j = 0; j < np; j++)
        if (npp != 1) {
            for (j = 2; j <= npp; j++) {
                L = j - 1;
                for (k = 1; k <= L; k++) {
                    adub[j-1][k-1] = adub[k-1][j-1];
                } // for (k = 1; k <= L; k++)
            } // for (j = 2; j <= npp; j++)
        } // if (npp != 1)
        
        // Main loop for complete pivoting
        for (npiv = 1; npiv <= np; npiv++) {
            // Find next pivot element
            dum = 0.0;
            for (j = 1; j <= np; j++) {
                if (piv[j-1] || adub[j-1][j-1] <= dtoler[j-1]) {
                    continue;
                }
                ddum = adub[j-1][npp-1] * adub[j-1][npp-1]/adub[j-1][j-1];
                if (ddum <= dum) {
                    continue;
                }
                dum = ddum;
                L = j;
            } // for (j = 1; j <= np; j++)
            if (dum <= 0.0) {
                go300 = true;
                break;
            }
            piv[L-1] = true;
            pivot1(adub, mdima, npp, L);
        } // for (npiv = 1; npiv <= np; npiv++)
        if (!go300) {
            npiv = np + 1;
        }
        npiv--;
        if (onlyin || !constr) {
            allpiv[0] = npiv == np;
            if (onlyin) {
                return;
            }
            for (j = 0; j < np; j++) {
                deltap[j] = 0.0;
                if (piv[j]) {
                    deltap[j] = adub[j][npp-1];
                }
            } // for (j = 0; j < np; j++)
            return;
        } // if (onlyin || !constr)
        
        // Enforce constraints by making qg[0] < 1 or by unpivoting
        for (j = 1; j <= jlam; j++) {
            dum = 1.0E20;
            for (k = 1; k <= jlam; k++) {
                if (!piv[k-1]) {
                    continue;
                }
                rdelta = 1.0/adub[k-1][npp-1];
                ddum = Math.max((pmin - p[k-1])*rdelta, (pmax - p[k-1]) * rdelta);
                if (ddum >= dum) {
                    continue;
                }
                dum = ddum;
                L = k;
            } // for (k = 1; k <= jlam; k++)
            if (dum > 1.0E-5) {
                break;
            }
            npiv--;
            piv[L-1] = false;
            pivot1(adub, mdima, npp, L);
        } // for (j = 1; j <= jlam; j++)
        
        // If all elements are somehow unpivoted, take error exit
        if (npiv <= 0) {
            ierror[0] = 5;
            return;
        }
        qg[0] = Math.min(dum, 1.0);
        qmax[0] = dum;
        allpiv[0] = npiv == np;
        if (onlyin) {
            return;
        }
        for (j = 0; j < np; j++) {
            deltap[j] = 0.0;
            if (piv[j]) {
                deltap[j] = adub[j][npp-1];
            }
        } // for (j = 0; j < np; j++)
        return;
    } // pivot
    
    private void pivot1(double adub[][], int mdima, int npp, int lpiv) {
        // Does a Gauss-Jordan pivot on diagonal element
        
        // adub[][] has dimension [mdima][mdima]
        double dub;
        double ddub;
        int j;
        int k;
        
        dub = 1.0/adub[lpiv-1][lpiv-1];
        for (j = 1; j <= npp; j++) {
            if (j == lpiv) {
                continue;
            }
            ddub = adub[j-1][lpiv-1] * dub;
            for (k = 1; k <= npp; k++) {
                if (k != lpiv) {
                    adub[j-1][k-1] = adub[j-1][k-1] - adub[lpiv-1][k-1] * ddub;
                }
            } // for (k = 1; k <= npp; k++)
        } // for (j = 1; j <= npp; j++)
        for (j = 1; j <= npp; j++) {
            adub[j-1][lpiv-1] = -adub[j-1][lpiv-1] * dub;
            adub[lpiv-1][j-1] = adub[lpiv-1][j-1] * dub;
        } // for (j = 1; j <= npp; j++)
        adub[lpiv-1][lpiv-1] = dub;
        return;
    } // pivot1
    
    private void evar(double q[], int jlam, double var[], boolean invert, boolean allpiv[], double ylydum[], int nlin, double varg, int ntry,
                      boolean inter, int ierror[]) {
        // For fractional step size q, evaluates palpha (linear least squares parameters), var (weighted variance), and
        // accumulates it in vgrid2 (coarse grid sum), and, if invert == true, evaluates arrays sfde, dfcapl, and full
        // upper triangles of ddse and se for later use in nonlinear least squares.
        
        // Calls routines etheor, pivot
        // which in turn call pivot1
        // double ylydum[] has length 1
        double ylyfit;
        int j;
        int nlinp;
        int k;
        boolean notinv;
        double dum[] = new double[1];
        double adum[] = new double[10];
        double ddum[] = new double[1];
        boolean lddum;
        int L;
        double dddum[] = new double[1];
        double xlam;
        int jz;
        double hz;
        int jm;
        int jp;
        double h;
        double hh;
        double dx;
        double ddx;
        double s[] = new double[1];
        double sp[] = new double[1];
        double factor[] = new double[9];
        double xexp[] = new double[9];
        int kk;
        int LL;
        int j2max;
        int jg2[] = new int[9];
        int lmax = 1;
        int loc;
        int j2next;
        int lnext = 1;
        int idiff;
        
        for (j = 0; j < jlam; j++) {
            plmtry[j] = Math.max(lamnmx[0][0], Math.min(lamnmx[1][0], plam[j] + q[0] * deltap[j]));    
        } // for (j = 0; j < jlam; j++)
        nlinp = nlin + 1;
        for (k = 0; k < nlinp; k++) {
            adub[k][nlinp-1] = 0.0;    
        } // for (I = 0; k < nlinp; k++)
        if (!nobase) {
            plmtry[nlin-1] = 0.0;
            adub[nlin-1][nlin-1] = sumwt;
            adub[nlin-1][nlinp-1] = sumwty;
        } // if (!nobase)
        notinv = !invert;
        if (!regint) {
            // Ordinary computation of e (exponentials), adub (normal equations matrix for linear least squares),
            // and, if invert == true, part of dfcapl (partial of fcap with respect to lambda) when regint == false.
            kloop:
            for (k = 1; k <= nlin; k++) {
                for (j = 1; j <= k; j++) {
                    if (j > jlam) {
                        continue kloop;
                    }
                    adub[j-1][k-1] = 0.0;
                    dfcapl[k-1][j-1] = 0.0;
                    if (k <= jlam) {
                        ddse[j-1][k-1] = 0.0;
                    }
                } // for (j = 1; j <= k; j++)
            } // for (k = 1; k <= nlin; k++)
            for (j = 1; j <= n; j++) {
                dum[0] = -t[j-1];
                for (k = 1; k <= jlam; k++) {
                    adum[k-1] = sqrtw[j-1] * Math.exp(plmtry[k-1] * dum[0]);
                    e[j-1][k-1] = adum[k-1];
                } // for (k = 1; k <= jlam; k++)
                if (!nobase) {
                    adum[nlin-1] = sqrtw[j-1];
                    e[j-1][nlin-1] = adum[nlin-1];
                } // if (!nobase)
                kloop2:
                for (k = 1; k <= nlin; k++) {
                    ddum[0] = adum[k-1];
                    lddum = k <= jlam;
                    for (L = 1; L <= k; L++) {
                        if (L > jlam) {
                            continue kloop2;
                        }
                        dddum[0] = adum[L-1] * ddum[0];
                        adub[L-1][k-1] = adub[L-1][k-1] + dddum[0];
                        if (notinv) {
                            continue;
                        }
                        dddum[0] = dddum[0] * dum[0];
                        dfcapl[k-1][L-1] = dfcapl[k-1][L-1] - dddum[0];
                        if (lddum) {
                            ddse[L-1][k-1] = ddse[L-1][k-1] + dddum[0] * dum[0];
                        }
                    } // for (L = 1; L <= k; L++)
                    adub[k-1][nlinp-1] = adub[k-1][nlinp-1] + ddum[0] * y[j-1];
                } // for (k = 1; k <= nlin; k++)
            } // for (j = 1; j <= n; j++)
        } // if (!regint)
        else { // regint
            // Evaluate adub and, if invert == true, part of dfcapl and ddse (second derivatives of exponential sums
            // with respect to lambda) by hermite and Lagrange interpolation, if regint == inter == true.
            if (inter) {
                kloop3:
                for (k = 1; k <= nlin; k++) {
                    lddum = k <= jlam;
                    for (j = 1; j <= k; j++) {
                        if (j > jlam) {
                            continue kloop3;
                        }
                        xlam = plmtry[k-1] + plmtry[j-1];
                        dum[0] = (Math.log(xlam) - gestl)/dgride;
                        jz = (int)(dum[0] + 0.5);
                        hz = dum[0] - jz;
                        if (Math.abs(hz) <= 1.0E-5) {
                            adub[j-1][k-1] = gse[jz-1][0];
                            if (notinv) {
                                continue;
                            }
                            dfcapl[k-1][j-1] = -gse[jz-1][1]/xlam;
                            if (lddum) {
                                ddse[j-1][k-1] = (gse[jz-1][2] - gse[jz-1][1])/(xlam * xlam);
                            }
                            continue;
                        } // if (Math.abs(hz) <= 1.0E-5)
                        jm = (int)dum[0];
                        jp = jm+1;
                        h = dum[0] - jm;
                        hh = 1.0 - h;
                        ddum[0] = 1.0 + 3.0 * h;
                        dddum[0] = 1.0 + 3.0 * hh;
                        dx = h * dgride;
                        ddx = -hh * dgride;
                        adub[j-1][k-1] = hh*hh*hh*((ddum[0]+6.0*h*h)*gse[jm-1][0]+dx*(ddum[0]*gse[jm-1][1]+ 
                                  .5*dx*gse[jm-1][2]))+h*h*h*((dddum[0]+6.0*hh*hh)*gse[jp-1][0]+
                                  ddx*(dddum[0]*gse[jp-1][1]+.5*ddx*gse[jp-1][2]));
                        if (notinv) {
                            continue;
                        }
                        hh = hz * hz;
                        dx = (hh - 1.0) * hz * 0.5;
                        dum[0] = 1.0/(hz + 1.0);
                        ddum[0] = 1.0/hz;
                        dddum[0] = 1.0/(hz - 1.0);
                        h = dx*dx*(((3.0*hz+4.0)*gse[jz-2][1]*dum[0]+dgride*gse[jz-2][2])*dum[0]+
                                  4.0*ddum[0]*(ddum[0]*gse[jz-1][1]+dgride*gse[jz-1][2])+dddum[0]*((4.0-3.0*
                                  hz)*gse[jz][1]*dddum[0]+dgride*gse[jz][2]));
                        dfcapl[k-1][j-1] = -h/xlam;
                        if (!lddum) {
                            continue;
                        }
                        dum[0] = dx*(hh-4.0)*(hh-9.0)*(gse[jz+2][2]/(hz-3.0)+gse[jz-4][2]/ 
                                  (hz+3.0)-6.0*(gse[jz+1][2]/(hz-2.0)+gse[jz-3][2]/(hz+2.0))+ 
                                  15.0*(gse[jz][2]*dddum[0]+gse[jz-2][2]*dum[0])-20.0*gse[jz-1][2]*ddum[0])/360.0;
                        ddse[j-1][k-1] = (dum[0]-h)/(xlam * xlam);
                    } // for (j = 1; j <= k; j++)
                    dum[0] = (Math.log(plmtry[k-1]) - gestl)/dgride;
                    jz = (int)(dum[0] + 0.5);
                    hz = dum[0] - jz;
                    if (Math.abs(hz) <= 1.0E-5) {
                        adub[k-1][nlinp-1] = gse[jz-1][3];
                        continue;
                    } // if (Math.abs(hz) <= 1.0E-5)
                    hh = hz * hz;
                    adub[k-1][nlinp-1] = hz*(hh-1.0)*(hh-4.0)*(hh-9.0)*(gse[jz+2][3]/(hz-
                              3.0)+gse[jz-4][3]/(hz+3.0)-6.0*(gse[jz+1][3]/(hz-2.0)+ 
                              gse[jz-3][3]/(hz+2.0))+15.0*(gse[jz][3]/(hz-1.0)+gse[jz-2][3]/
                              (hz+1.0))-20.0*gse[jz-1][3]/hz)/720.0;                            
                } // for (k = 1; k <= nlin; k++)
            } // if (inter)
            else { // !inter
                // Evaluate adub, part of dfcapl, and ddse by calling etheor if regint == true and inter == false
                kloop4:
                for (k = 1; k <= nlin; k++) {
                    lddum = k <= jlam;
                    for (j = 1; j <= k; j++) {
                        if (j > jlam) {
                            continue kloop4;
                        }
                        xlam = plmtry[k-1] + plmtry[j-1];
                        etheor(xlam, invert, wted, dum, ddum, dddum, s, true, false);
                        adub[j-1][k-1] = dum[0];
                        if (notinv) {
                            continue;
                        }
                        dfcapl[k-1][j-1] = -ddum[0]/xlam;
                        if (lddum) {
                            ddse[j-1][k-1] = (dddum[0] - ddum[0])/(xlam * xlam);
                        }
                    } // for (j = 1; j <= k; j++)
                    etheor(plmtry[k-1], false, wted, dum, ddum, dddum, s, false, true);
                    adub[k-1][nlinp-1] = s[0];
                } // for (k = 1; k <= nlin; k++)
            } // else !inter
        } // else regint
        for (k = 1; k <= nlin; k++) {
            dtoler[k-1] = 10.0 * precis * adub[k-1][k-1];
            for (j = 1; j <= k; j++) {
                se[j-1][k-1] = adub[j-1][k-1];
            }
        } // for (k = 1; k <= nlin; k++)
        
        // Solve normal equations for palpha using stepwise regression
        pivot(adub, mdima, nlin, false, invert, nonneg, 0.0, 1.0E20, zero, dtoler, palpha, allpiv, sp, pivalp, 
              jlam, sp, ierror);
        if (ierror[0] == 5) {
            var[0] = 1.0E20;
            return;
        }
        
        // Calculate var[0] and, if invert == true, sfde (sums of residuals times derivatives of nonlinear terms)
        var[0] = 0.0;
        if (!notinv) {
             for (k = 1; k <= jlam; k++) {
                 sfde[k-1] = 0.0;
                 for (j = 1; j <= k; j++) {
                     dfcapl[j-1][k-1] = dfcapl[k-1][j-1];
                 } // for (j = 1; j <= k; j++)
             } // for (k = 1; k <= jlam; k++)
        } // if (!notinv)
        if (regint) {
            k = 0;
            for (j = 1; j <= nint; j++) {
                for (L = 1; L <= jlam; L++) {
                    factor[L-1] = Math.exp(-plmtry[L-1] * deltat[j-1]);
                    xexp[L-1] = Math.exp(-plmtry[L-1] * tstart[j-1]);
                } // for (L = 1; L <= jlam; L++)
                kk = nt[j-1];
                for (LL = 1; LL <= kk; LL++) {
                    k++;
                    dum[0] = sqrtw[k-1];
                    ylyfit = y[k-1];
                    if (!nobase) {
                        ylyfit = ylyfit - palpha[nlin-1] * dum[0];
                    }
                    for (L = 1; L <= jlam; L++) {
                        adum[L-1] = xexp[L-1] * dum[0];
                        ylyfit = ylyfit - adum[L-1] * palpha[L-1];
                        xexp[L-1] = xexp[L-1] * factor[L-1];
                    } // for (L = 1; L <= jlam; L++)
                    var[0] = var[0] + ylyfit * ylyfit;
                    if (notinv) {
                        continue;
                    }
                    ddum[0] = t[k-1];
                    for (L = 1; L <= jlam; L++) {
                        sfde[L-1] = sfde[L-1] - ddum[0] * ylyfit * adum[L-1];
                    } // for (L = 1; L <= jlam; L++)
                } // for (LL = 1; LL <= kk; LL++)
            } // for (j = 1; j <= nint; j++)
        } // if (regint)
        else { // !regint
            for (j = 1; j <= n; j++) {
                ylyfit = y[j-1];
                for (k = 1; k <= nlin; k++) {
                    ylyfit = ylyfit - palpha[k-1] * e[j-1][k-1];
                } // for (k = 1; k <= nlin; k++)
                var[0] = var[0] + ylyfit * ylyfit;
                if (notinv) {
                    continue;
                }
                dum[0] = t[j-1];
                for (k = 1; k <= jlam; k++) {
                    sfde[k-1] = sfde[k-1] - dum[0] * ylyfit * e[j-1][k-1];
                } // for (k = 1; k <= jlam; k++)
            } // for (j = 1; j <= n; j++)
        } // else !regint
        if (jlam == 1 || !inter) {
            return;
        }
        // Add var[0] into vgrid2 (coarse grid sum)
        j2max = 0;
        for (L = 1; L <= jlam; L++) {
            if (plmtry[L-1] < lamnmx[0][1] || plmtry[L-1] > lamnmx[1][1]) {
                return;
            }
            jg2[L-1] = ((int)((Math.log(plmtry[L-1]) - gestl)/dgride + 0.5) - jgeadd)/nperg2[jlam-1] + 1;
            if (jg2[L-1] < j2max) {
                continue;
            }
            j2max = jg2[L-1];
            lmax = L;
        } // for (L = 1; L <= jlam; L++)
        jg2[lmax-1] = 0;
        loc = 1;
        for (j = 1; j <= jlam; j++) {
            j2next = 0;
            for (L = 1; L <= jlam; L++) {
                if (jg2[L-1] < j2next) {
                    continue;
                }
                j2next = jg2[L-1];
                lnext = L;
            } // for (L = 1; L <= jlam; L++)
            L = j2next + 1;
            idiff = j2max - L;
            if (idiff < 0) {
                return;
            }
            if (idiff > 0) {
                LL = j2max - 1;
                for (k = L; k <= LL; k++) {
                    kk = ngrid2[jlam-1] - k + 1;
                    loc = loc + nbinom[kk-1][j-1];
                } // for (k = L; k <= LL; k++)
            } // if (idiff > 0)
            j2max = j2next;
            jg2[lnext-1] = 0;
        } // for (j = 1; j <= jlam; j++)
        sp[0] = var[0];
        loc = Math.min(loc, 252);
        if (!invert || (var[0] > varg && ntry == 1)) {
            sp[0] = sp[0] * 1.0E-6;
        }
        vgrid2[loc-1] = vgrid2[loc-1] + sp[0];
        return;
    } // evar
    
    private void varf(double q[], int jlam, double var[], boolean invert, boolean allpiv[], double ylyfit[], int nlin, double varg, int ntry,
                      boolean intdum, int ierror[]) {
        // Same as evar, except for transforms instead of exponentials and does no accumulation in a coarse grid.
        
        // Calls routine pivot
        // which in turn calls pivot1
        int nlinp;
        boolean notinv;
        int j;
        double rlam;
        double dum;
        int jz;
        double hz;
        int k;
        double adum[] = new double[9];
        double hh;
        double ddum;
        double dddum;
        int L;
        double sp[] = new double[1];
        
        nlinp = nlin + 1;
        notinv = !invert;
        if (!nobase) {
            plmtry[nlin-1] = 0.0;
            for (j = 0; j < nf; j++) {
                f[j][nlin-1] = trbase[j];
            }
        } // if (!nobase)
        // Compute f (nonlinear terms) and, if invert == true, df (their derivatives with respect to lambda) by
        // hermite and lagrange interpolation
        for (j = 1; j <= jlam; j++) {
            plmtry[j-1] = Math.max(lamnmx[0][0], Math.min(lamnmx[1][0], plam[j-1] + q[0] * deltap[j-1])); 
            rlam = 1.0/plmtry[j-1];
            dum = (Math.log(plmtry[j-1]) - gfstl)/dgridf;
            jz = (int)(dum + 0.5);
            hz = dum - jz;
            if (Math.abs(hz) <= 1.0E-5) {
                for (k = 0; k < nf; k++) {
                    f[k][j-1] = gf[k][jz-1];
                    if (invert) {
                        df[k][j-1] = dgfl[k][jz-1] * rlam;
                    }
                } // for (k = 0; k < nf; k++)
                continue;
            } // if (Math.abs(hz) <= 1.0E-5)
            adum[6] = 1.0/hz;
            adum[0] = 4.0 * adum[6] * adum[6];
            adum[1] = 4.0 * dgridf * adum[6];
            adum[7] = 1.0/(hz + 1.0);
            adum[2] = (3.0 * hz + 4.0)* adum[7]*adum[7];
            adum[3] = dgridf * adum[7];
            adum[8] = 1.0/(hz - 1.0);
            adum[4] = (4.0 - 3.0 * hz)*adum[8]*adum[8];
            adum[5] = dgridf * adum[8];
            hh = hz * hz;
            ddum = (hh - 1.0) * hz * 0.5;
            dddum = ddum * ddum;
            for (k = 0; k < nf; k++) {
                f[k][j-1] = dddum * (adum[0] * gf[k][jz-1] + adum[1] * dgfl[k][jz-1] + adum[2] * gf[k][jz-2] + 
                            adum[3] * dgfl[k][jz-2] + adum[4] * gf[k][jz] + adum[5] * dgfl[k][jz]);
            } // for (k = 0; k < nf; k++)
            if (notinv) {
                continue;
            }
            adum[0] = -20.0 * adum[6];
            adum[1] = 15.0 * adum[7];
            adum[2] = 15.0 * adum[8];
            adum[3] = -6.0/(hz + 2.0);
            adum[4] = -6.0/(hz - 2.0);
            adum[5] = 1.0/(hz + 3.0);
            adum[6] = 1.0/(hz - 3.0);
            dddum = ddum * (hh - 4.0)* (hh - 9.0) * rlam/360.0;
            for (k = 0; k < nf; k++) {
                df[k][j-1] = dddum * (adum[0] * dgfl[k][jz-1] + adum[1] * dgfl[k][jz-2] + adum[2] * dgfl[k][jz] +
                             adum[3] * dgfl[k][jz-3] * adum[4] * dgfl[k][jz+1] + adum[5] * dgfl[k][jz-4] +
                             adum[6] * dgfl[k][jz+2]);
            } // for (k = 0; k < nf; k++)
        } // for (j = 1; j <= jlam; j++)
        
        // Compute and invert adub (normal equations matrix for linear least squares)
        adub[nlinp-1][nlinp-1] = 0.0;
        for (k = 1; k <= nlin; k++) {
            for (j = 1; j <= k; j++) {
                dum = 0.0;
                for (L = 0; L < nf; L++) {
                    dum = dum + f[L][j-1] * f[L][k-1];
                }
                adub[j-1][k-1] = dum;
            } // for (j = 1; j <= k; j++)
            dtoler[k-1] = 10.0 * precis * adub[k-1][k-1];
            dum = 0.0;
            for (L = 0; L < nf; L++) {
                dum = dum + f[L][k-1] * fhat[L];
            }
            adub[k-1][nlinp-1] = dum;
        } // for (k = 1; k <= nlin; k++)
        pivot(adub, mdima, nlin, false, invert, nonneg, 0.0, 1.0E20, zero, dtoler, palpha, allpiv, sp, pivalp, jlam, sp, ierror);
        if (ierror[0] == 5) {
            return;
        }
        
        // Calculate var (weighted variance of fit) and ylyfit (weighted residuals), and, if invert == true, part of dfcapl
        // for later use in nonlinear least squares.
        var[0] = 0.0;
        for (L = 0; L < nf; L++) {
            dum = fhat[L];
            for (j = 0; j < nlin; j++) {
                dum = dum - palpha[j] * f[L][j];
            }
            ylyfit[L] = dum;
            var[0] = var[0] + dum * dum;
        } // for (L = 0; L < nf; L++)
        if (notinv || (var[0] > varg && ntry == 1)) {
            return;
        }
        for (k = 0; k < jlam; k++) {
            for (j = 0; j < nlin; j++) {
                dum = 0.0;
                for (L = 0; L < nf; L++) {
                    dum = dum - f[L][j] * df[L][k];
                } // for (L = 0; L < nf; L++)
                dfcapl[j][k] = dum;
            } // for (j = 0; j < nlin; j++)
        } // for (k = 0; k < jlam; k++)
        return;
    } // varf
    
    private void anlerr(int jlam, int nlin, int itype, boolean inter, boolean prlsq) {
        // Does final error analysis of fit and calculates nonlinearity term enphi of 
        // Beale for later use.
        
        // Calls pivot
        // which in turn calls pivot1
        int k;
        int kk;
        int j[] = new int[1];
        int jj;
        boolean ldum[] = new boolean[1];
        double dum[] = new double[1];
        double ddum[] = new double[1];
        double cstar[] = new double[19];
        int jk = 0;
        int L = 0;
        double dub;
        int nuj;
        int nuk;
        double delt[][] = new double[18][18];
        double sum[] = new double[4];
        int nu;
        double cui[] = new double[19];
        int ii;
        int i;
        double tnu;
        double ss;
        double dddum;
        double s;
        double ccsum[][] = new double[19][18];
        double sss;
        double adum[] = new double[10];
        int nui;
        
        // Compute correlation coefficients and standard error estimates using full normal
        // equations matrix (adub) with all parameters assumed independent
        for (k = 1; k <= jlam; k++) {
            kk = k + jlam;
            for (j[0] = 1; j[0] <= k; j[0]++) {
                jj = j[0] + jlam;
                adub[j[0]-1][k-1] = palpha[j[0]-1] * palpha[k-1] * ddse[j[0]-1][k-1];
                adub[j[0]-1][kk-1] = -dfcapl[k-1][j[0]-1] * palpha[j[0]-1];
                adub[k-1][jj-1] = -dfcapl[j[0]-1][k-1] * palpha[k-1];
                adub[jj-1][kk-1] = se[j[0]-1][k-1];
            } // for (j[0] = 1; j[0] <= k; j[0]++)
            dtoler[k-1] = adub[k-1][k-1] * precis;
            dtoler[kk-1] = adub[kk-1][kk-1] * precis;
            if (nobase) {
                continue;
            }
            adub[k-1][nf-1] = -dfcapl[nlin-1][k-1] * palpha[k-1];
            adub[kk-1][nf-1] = se[k-1][nlin-1];
        } // for (k = 1; k <= jlam; k++)
        if (!nobase) {
            adub[nf-1][nf-1] = sumwt;
            dtoler[nf-1] = sumwt * precis;
        } // if (!nobase)
        pivot(adub, mdima, nf, true, true, false, 0.0, 0.0, dtoler, dtoler, dtoler, ldum, dum, pivalp, jlam, ddum, j);
        sigyy = Math.sqrt(var[0]/(n - nf));
        if (!ldum[0] || j[0] == 5) {
            failed = true;
            if (prlsq) {
                Preferences.debug("Singularity in inverting full least squares matrix, no correlations calculated\n",
                                  Preferences.DEBUG_ALGORITHM);
            }
            return;
        } // if (!ldum[0] || j[0] == 5)
        for (j[0] = 0; j[0] < nf; j[0]++) {
            if (adub[j[0]][j[0]] <= 0.0) {
                failed = true;
                if (prlsq) {
                    Preferences.debug("Singularity in inverting full least squares matrix, no correlations calculated\n",
                                      Preferences.DEBUG_ALGORITHM);
                }
                return;    
            } // if (adub[j[0]][j[0]] <= 0.0)
            cstar[j[0]] = 1.0/Math.sqrt(adub[j[0]][j[0]]);
            sigmap[j[0]] = sigyy/cstar[j[0]];
        } // for (j[0] = 0; j[0] < nf; j[0]++)
        if (inter) {
            return;
        } // if (inter)
        k = jlam;
        for (j[0] = 0; j[0] < jlam; j[0]++) {
            pcerr[j[0]] = 100.0 * sigmap[j[0]]/plam[j[0]];
            k++;
            pcerr[k-1] = 100.0 * sigmap[k-1]/Math.abs(palpha[j[0]]);
        } // for (j[0] = 0; j[0] < jlam; j[0]++)
        if (!nobase) {
            pcerr[nf-1] = 100.0 * sigmap[nf-1]/Math.abs(palpha[jlamp1-1]);
        } // if (!nobase)
        if (prlsq) {
            k = jlam;
            if (nobase) {
                k--;
            } // if (nobase)
            Preferences.debug("Correlation coefficients\n", Preferences.DEBUG_ALGORITHM);
            for (j[0] = 2; j[0] <= nf; j[0]++) {
                dum[0] = cstar[j[0]-1];
                jk = j[0] - 1;
                for (k = 1; k <= jk; k++) {
                    dtoler[k-1] = adub[j[0]-1][k-1] * dum[0] * cstar[k-1];
                } // for (k = 1; k <= jk; k++)
                L = (j[0] - 1)/jlam + 1;
                jj = j[0];
                if (L == 2) {
                    jj = jj - jlam;
                }
                if (L < 3) {
                    if (L == 1) {
                        Preferences.debug("LAMBDA\n", Preferences.DEBUG_ALGORITHM);
                    }
                    else if (L == 2) {
                        Preferences.debug("ALPHA\n", Preferences.DEBUG_ALGORITHM);
                    }
                    Preferences.debug("jj = " + jj + "\n", Preferences.DEBUG_ALGORITHM);
                    for (k = 0; k < jk; k++) {
                        Preferences.debug("dtoler["+k+"] = " + dtoler[k] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if (L < 3)
            } // for (j[0] = 2; j[0] <= nf; j[0]++)
            if (L == 3) {
                Preferences.debug("BASE\n", Preferences.DEBUG_ALGORITHM);
                for (k = 0; k < jk; k++) {
                    Preferences.debug("dtoler["+k+"] = " + dtoler[k] + "\n", Preferences.DEBUG_ALGORITHM);    
                }
            } // if (L == 3)
        } // if (prlsq)
        
        // To prevent a very small component (effectively a second baseline) that is highly correlated with the 
        // baseline from interfering with the calculation of the weights
        if (iwtsgn <= 0 && itype == 1) {
            dub = 1.0E20;
            for (j[0] = 1; j[0] <= jlam; j[0]++) {
                if (plam[j[0]-1] > dub) {
                    continue;
                }
                dub = plam[j[0]-1];
                L = j[0];
            } // for (j[0] = 1; j[0] <= jlam; j[0]++)
            if (pcerr[L-1] >= 1000.0) {
                failed = true;
                if (prlsq) {
                    Preferences.debug("Smallest lambda has too high a standard error\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("This could lead to inaccurate weights - solution rejected\n", Preferences.DEBUG_ALGORITHM);
                } // if (prlsq)
            } // if (pcerr[L-1] >= 1000.0)
        } // if (iwtsgn <= 0 && itype == 1)
        
        if (!failed && itype == 4) {
            // Calculation of enphi (Eq. 3.25 of E. M. L. Beale for F ratio correction factor due to nonlinearity)
            nuj = jlam;
            for (j[0] = 1; j[0] <= jlam; j[0]++) {
                dum[0] = adub[j[0]-1][j[0]-1];
                nuj++;
                nuk = jlam;
                for (k = 1; k <= jlam; k++) {
                    nuk++;
                    delt[j[0]-1][k-1] = dum[0] * adub[k-1][k-1] + 2.0 * adub[j[0]-1][k-1] * adub[j[0]-1][k-1];
                    delt[j[0]-1][nuk-1] = dum[0] *adub[k-1][nuk-1] + 2.0 * adub[j[0]-1][k-1] * adub[j[0]-1][nuk-1];
                    delt[nuj-1][k-1] = adub[j[0]-1][nuj-1] * adub[k-1][k-1] + 2.0 * adub[j[0]-1][k-1] * adub[nuj-1][k-1];
                    delt[nuj-1][nuk-1] = adub[j[0]-1][nuj-1] * adub[k-1][nuk-1] + adub[j[0]-1][k-1] * adub[nuj-1][nuk-1]
                                       + adub[j[0]-1][nuk-1] * adub[nuj-1][k-1];
                    for (L = 0; L < 4; L++) {
                        sum[L] = 0.0;
                    } // for (L = 0; L < 4; L++)
                    nu = 0;
                    for (L = 1; L <= nint; L++) {
                        cui[0] = Math.exp(-plam[j[0]-1] * deltat[L-1]);
                        cui[1] = Math.exp(-plam[k-1] * deltat[L-1]);
                        cui[2] = Math.exp(-plam[j[0]-1] * tstart[L-1]);
                        cui[3] = Math.exp(-plam[k-1] * tstart[L-1]);
                        ii = nt[L-1];
                        for (i = 1; i <= ii; i++) {
                            nu++;
                            tnu = t[nu-1];
                            if (!regint) {
                                ss = e[nu-1][j[0]-1];
                                dddum = tnu * e[nu-1][k-1];
                            } // if (!regint)
                            else { // regint
                                ss = sqrtw[nu-1] * cui[2];
                                dddum = tnu * sqrtw[nu-1] * cui[3];
                                cui[2] = cui[2] * cui[0];
                                cui[3] = cui[3] * cui[1];
                            } // else regint
                            s = palpha[j[0]-1] * tnu * ss;
                            ddum[0] = palpha[k-1] * tnu * dddum;
                            sum[0] = sum[0] - s * ddum[0];
                            sum[1] = sum[1] + s * dddum;
                            sum[2] = sum[2] + ss * ddum[0];
                            sum[3] = sum[3] - ss * dddum;
                        } // for (i = 1; i <= ii; i++)
                    } // for (L = 1; L <= nint; L++)
                    ccsum[j[0]-1][k-1] = sum[0];
                    ccsum[j[0]-1][nuk-1] = sum[1];
                    ccsum[nuj-1][k-1] = sum[2];
                    ccsum[nuj-1][nuk-1] = sum[3];
                } // for (k = 1; k <= jlam; k++)
                if (nobase) {
                    continue;
                } // if (nobase)
                s = 0.0;
                ss = 0.0;
                nu = 0;
                for (L = 1; L <= nint; L++) {
                    dum[0] = Math.exp(-plam[j[0]-1] * deltat[L-1]);
                    ddum[0] = Math.exp(-plam[j[0]-1] * tstart[L-1]);
                    ii = nt[L-1];
                    for (i = 1; i <= ii; i++) {
                        nu++;
                        tnu = t[nu-1];
                        if (!regint) {
                            sss = e[nu-1][j[0]-1];
                        } // if (!regint)
                        else { // regint
                            sss = ddum[0] * sqrtw[nu-1];
                            ddum[0] = ddum[0] * dum[0];
                        } // else regint
                        dddum = tnu * sss * sqrtw[nu-1];
                        ss = ss - dddum;
                        s = s + palpha[j[0]-1] * tnu * dddum;
                    } // for (i = 1; i <= ii; i++)
                } // for (L = 1; L <= nint; L++)
                ccsum[nf-1][j[0]-1] = s;
                ccsum[nf-1][nuj-1] = ss;
            } // for (j[0] = 1; j[0] <= jlam; j[0]++)
            ldum[0] = !nobase;
            enphi = 0.0;
            for (nu = 1; nu <= n; nu++) {
                tnu = t[nu-1];
                for (j[0] = 1; j[0] <= nlin; j[0]++) {
                    adum[j[0]-1] = sqrtw[nu-1] * Math.exp(-plam[j[0]-1] * tnu);
                } // for (j[0] = 1; j[0] <= nlin; j[0]++)
                nuj = jlam;
                for (j[0] = 1; j[0] <= jlam; j[0]++) {
                    nuj++;
                    sum[0] = 0.0;
                    sum[1] = 0.0;
                    nuk = jlam;
                    for (k = 1; k <= jlam; k++) {
                        s = 0.0;
                        if (ldum[0]) {
                            s = sqrtw[nu-1] * adub[k-1][nf-1];
                        } // if (ldum[0])
                        nui = jlam;
                        for (i = 1; i <= jlam; i++) {
                            nui++;
                            cui[nui-1] = adum[i-1];
                            cui[i-1] = -palpha[i-1] * tnu * cui[nui-1];
                            s = s + adub[k-1][i-1] * cui[i-1] + adub[k-1][nui-1] * cui[nui-1];
                        } // for (i = 1; i <= jlam; i++)
                        sum[0] = sum[0] + ccsum[k-1][j[0]-1] * s;
                        sum[1] = sum[1] + ccsum[k-1][nuj-1] * s;
                        nuk++;
                        s= 0.0;
                        if (ldum[0]) {
                            s = sqrtw[nu-1] * adub[nuk-1][nf-1];
                        } // if (ldum[0])
                        nui = jlam;
                        for (i = 1; i <= jlam; i++) {
                            nui++;
                            s = s + adub[nuk-1][i-1] * cui[i-1] + adub[nuk-1][nui-1] * cui[nui-1];
                        } // for (i = 1; i <= jlam; i++)
                        sum[0] = sum[0] + ccsum[nuk-1][j[0]-1] * s;
                        sum[1] = sum[1] + ccsum[nuk-1][nuj-1] * s;
                    } // for (k = 1; k <= jlam; k++)
                    if (!nobase) {
                        s = 0.0;
                        if (ldum[0]) {
                            s = sqrtw[nu-1] * adub[nf-1][nf-1];
                        } // if (ldum[0])
                        nui = jlam;
                        for (i = 1; i <= jlam; i++) {
                            nui++;
                            s = s + adub[nf-1][i-1] * cui[i-1] + adub[nf-1][nui-1] * cui[nui-1];
                        } // for (i = 1; i <= jlam; i++)
                        sum[0] = sum[0] + ccsum[nf-1][j[0]-1] * s;
                        sum[1] = sum[1] + ccsum[nf-1][nuj-1] * s;
                    } // if (!nobase)
                    dum[0] = tnu * adum[j[0]-1];
                    cstar[j[0]-1] = palpha[j[0]-1] * tnu * dum[0] - sum[0];
                    cstar[nuj-1] = -2.0 * (dum[0] + sum[1]);
                } // for (j[0] = 1; j[0] <= jlam; j[0]++)
                sum[0] = 0.0;
                nuj = jlam;
                for (j[0] = 1; j[0] <= jlam; j[0]++) {
                    nuj++;
                    s = 0.0;
                    ss = 0.0;
                    nuk = jlam;
                    for (k = 1; k <= jlam; k++) {
                        nuk++;
                        s = s + cstar[k-1] * delt[j[0]-1][k-1] + cstar[nuk-1] * delt[j[0]-1][nuk-1];
                        ss = ss + cstar[k-1] * delt[nuj-1][k-1] + cstar[nuk-1] * delt[nuj-1][nuk-1];
                    } // for (k = 1; k <= jlam; k++)
                    sum[0] = sum[0] + cstar[j[0]-1] * s + cstar[nuj-1] * ss;
                } // for (j[0] = 1; j[0] <= jlam; j[0]++)
                enphi = enphi + sum[0];
            } // for (nu = 1; nu <= n; nu++)
            enphi = enphi * sigyy * sigyy/(nf + 2.0);
        } // if (!failed && itype == 4)
        if (!prlsq) {
            return;
        } // if (!prlsq)
        if (!(failed || itype != 4)) {
            Preferences.debug("enphi = " + enphi + "\n", Preferences.DEBUG_ALGORITHM);    
        } // if (!(failed || itype != 4))
        Preferences.debug("Standard deviation of fit = " + sigyy + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("lambda +- standard error percent        alpha +- standard error percent\n", Preferences.DEBUG_ALGORITHM);
        k = jlam;
        for (j[0] = 0; j[0] < jlam; j[0]++) {
            k++;
            Preferences.debug("plam["+j[0]+"] = " + plam[j[0]] + " sigmap["+j[0]+"] = +-" + sigmap[j[0]] +
                              "pcerr[" + j[0] + "] = " + pcerr[j[0]] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("palpha["+j[0]+"] = " + palpha[j[0]] + " sigmap["+(k-1)+"] = +-" + sigmap[k-1] +
                              "pcerr[" + (k-1) + "] = " + pcerr[k-1] + "\n", Preferences.DEBUG_ALGORITHM);
        } // for (J[0] = 0; j[0] < jlam; j[0]++)
        if (!nobase) {
            Preferences.debug("BASELINE palpha["+(jlamp1-1)+ "] = " + palpha[jlamp1-1] + " sigmap["+(nf-1)+"] = +-" + sigmap[nf-1] +
                              " pcerr["+(nf-1)+"] = " + pcerr[nf-1] + "\n", Preferences.DEBUG_ALGORITHM);
        } // if (!nobase)
        return;
    } // anlerr
    
    private void etheor(double plambd, boolean invert, boolean wted, double se[], double dse[], double ddse[], double sye[],
                        boolean doe, boolean doye) {
        // If doe == true, puts exponential sum and, if invert == true, its first and second derivatives with respect to
        // log(lambda) in se, dse, and ddse.  If doye == true, puts sum of y times exponentials in sye.  Can be used only
        // when regint == true.  If wted == true, uses recursion relations, and if wted == false, use geometric sum 
        // formulas for se, dse, and ddse.
        double sei = 0.0;
        double dsei = 0.0;
        double ddsei = 0.0;
        boolean notinv;
        double syei = 0.0;
        boolean lddum;
        int j;
        int L;
        double ddum;
        double dddum;
        int kk;
        int k;
        double dum;
        double dl;
        double an;
        double dnl;
        double rdedl;
        double dednl;
        double elts;
        double s;
        double b;
        double edl;
        double ents;
        double c;
        double ss;
        
        if (doe) {
            sei = 0.0;
        }
        if (invert) {
            dsei = 0.0;
            ddsei = 0.0;
        } // if (invert)
        notinv = !invert;
        if (doye || wted) {
            if (doye) {
                syei = 0.0;
            }
            lddum = !(doye && wted);
            j = -1;
            for (L = 0; L < nint; L++) {
                ddum = Math.exp(-plambd * deltat[L]);
                dddum = Math.exp(-plambd * tstart[L]);
                kk = nt[L];
                for (k = 0; k < kk; k++) {
                    j++;
                    dum = dddum * sqrtw[j];
                    dddum = dddum * ddum;
                    if (doye) {
                        syei = syei + dum * y[j];
                    }
                    if (lddum) {
                        continue;
                    }
                    dum = dum * sqrtw[j];
                    sei = sei + dum;
                    if (notinv) {
                        continue;
                    }
                    dum = dum * t[j];
                    dsei = dsei - dum;
                    ddsei = ddsei + dum * t[j];
                } // for (k = 0; k < kk; k++)
            } // for (L = 0; L < nint; L++)
            if (doye) {
                sye[0] = syei;
            }
            if (!doe) {
                return;
            }
            if (wted) {
                se[0] = sei;
                if (notinv) {
                    return;
                }
                dse[0] = plambd * dsei;
                ddse[0] = dse[0] + ddsei * plambd * plambd;
                return;
            } // if (wted)
        } // if (doye || wted)
        
        // Use geometric formulas when wted == false
        for (L = 0; L < nint; L++) {
            dl = plambd * deltat[L];
            an = nt[L];
            dnl = an * dl;
            if (Math.abs(dl) > 1.0E-3) {
                rdedl = 1.0/(1.0 - Math.exp(-dl));
                dednl = 1.0 - Math.exp(-dnl);
            } // if (Math.abs(dl) > 1.0E-3)
            else { // Math.abs(dl) <= 1.0E-3
                rdedl = 1.0/expsma(dl);
                if (Math.abs(dnl) <= 1.0E-3) {
                    dednl = expsma(dnl);
                }
                else {
                    dednl = 1.0 - Math.exp(-dnl);
                }
            } // else Math.abs(dl) <= 1.0E-3
            elts = Math.exp(-plambd * tstart[L]);
            s = elts * dednl * rdedl;
            sei = sei + s;
            if (notinv) {
                continue;
            }
            b = deltat[L] * rdedl;
            edl = Math.exp(-dl);
            ents = elts * Math.exp(-dnl);
            c = an  * ents - s * edl;
            ss = b * c - tstart[L] * s;
            dsei = dsei + ss;
            ddsei = ddsei - tstart[L]*ss - b*(edl*(b*c+ss-s*deltat[L]) + an*(an*deltat[L]+tstart[L])*ents);
        } // for (L = 0; L < nint; L++)
        se[0] = sei;
        if (notinv) {
            return;
        }
        dse[0] = plambd * dsei;
        ddse[0] = dse[0] + ddsei * plambd * plambd;
        return;
    } // etheor
    
    private double expsma(double s) {
        double result = s * (720.0 - s*(360.0 - s* (120.0 - s * (30.0 - s * (6.0 - s)))))/720.0;
        return result;
    }
    
    private void fanlyz() {
        // Does constrained stepwise least squares analysis of raw data (using grid search if necessary) to
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
        // Original code should but does not initialize lpeak
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
        boolean savepk;
        boolean go690 = false;
        double ddub;
        
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
                                                                                mindif = Math.min(mindif,  i);
                                                                            } // for (j = 2; j <= L; j++)
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
                // Do least squares fits
                ldum[0] = jtry == 1;
                lstsqr(lamst, jlam, true, n, jlam+ibase, evarCalled, 2, ldum[0], true, prprel);
                failed = failed || var[0] > vare;
                if (var[0] > vare) {
                    continue;
                }
                vare = var[0];
                for (j = 0; j < jlam; j++) {
                    lamst[j] = plam[j];
                    lamf[j][jlam-1][0] = lamst[j];
                } // for (j = 0; j < jlam; j++)
                savepk = vartry >= 1.0E20;
                if (!failed) {
                    if (prprel) {
                        Preferences.debug("End of fit to raw data, start of fit to transforms\n", Preferences.DEBUG_ALGORITHM);
                    }
                    lstsqr(lamst, jlam, false, nf, jlam+ibase, varfCalled, 3, false, true, prprel);
                    if (failed && var[0] >= vartry) {
                        continue;
                    }
                    savepk = true;
                    vartry = var[0];
                    for (j = 0; j < jlam; j++) {
                        lamf[j][jlam-1][1] = plam[j];
                    } // for (j = 0; j < jlam; j++)
                } // if (!failed)
                if (!savepk && jlam != 1) {
                    continue;
                }
                if (jlam != nlammx) {
                    // Finds max sum of absolute adjacent amplitudes for later determination of starting value
                    // for lamst[jlamp1-1]
                    dub[0] = Math.abs(palpha[0]) + Math.abs(palpha[jlamp1-1]);
                    llpeak = 1;
                    if (dub[0] <= Math.abs(palpha[jlam-1])) {
                        llpeak = jlamp1;
                        dub[0] = Math.abs(palpha[jlam-1]);
                    } // if (dub[0] <= Math.abs(palpha[jlam-1]))
                    if (jlam == 1) {
                        go690 = true;
                        break;
                    } // if (jlam == 1)
                    for (j = 2; j <= jlam; j++) {
                        ddub = Math.abs(palpha[j-2]) + Math.abs(palpha[j-1]);
                        if (ddub > dub[0]) {
                            dub[0] = ddub;
                            llpeak = j;
                        } // if (ddub > dub[0])
                    } // for (j = 2; j <= jlam; j++)
                } // if (jlam != nlammx)
                if (!failed || jlam == 1) {
                    go690 = true;
                    break;
                }
            } // for (jtry = 1; jtry <= mtry; jtry++)
            if (!go690) {
                jtry = mtry;
            }
            go690 = false;
            lpeak = llpeak;
            nftry[jlam-1] = jtry;
            ffail[jlam-1][1] = failed;
            ffail[jlam-1][0] = vartry >= 1.0E20;
            if (!ffail[jlam-1][0]) {
                continue;
            }
            for (j = 0; j < jlam; j++) {
                lamf[j][jlam-1][1] = lamf[j][jlam-1][0];
            } // for (j = 0; j < jlam; j++)
        } // for (jlam = 1; jlam <= nlammx; jlam++)
        return;
    } // fanlyz
    
    private void yanlyz() {
        // For constrained stepwise least squares analysis of raw data using lamf (starting values obtained from fanlyz).
        
        // Calls routines lstsqr, fisher, residu
        // which in turn call evar, etheor, pivot, pivot1, plpres
        int jlambs;
        double varbes;
        double absymx;
        int j;
        int jlam;
        boolean yfail[] = new boolean[9];
        double sigysv[] = new double[9];
        double varsv[] = new double[9];
        int itersv[] = new int[9];
        double enphsv[] = new double[9];
        int i = 0;
        int k;
        double asave[][][] = new double[10][9][6];
        double dum;
        // L in bigloop is line number, so the L in bigloop can be omitted.
        int L;
        double pnxbes;
        int jlamnx;
        double probsv[] = new double[9];
        double ddum;
        double dddum;
        double probln[] = new double[9];
        // 2 instances of pnxlin in program are unused dead ends
        //double pnxlin;
        double puncor[] = new double[6];
        boolean done[] = new boolean[9];
        
        wted = iwt != 1;
        jlambs = 1;
        varbes = 1.0E20;
        varmin = 1.0E20;
        absymx = 0.0;
        for (j = 0; j < n; j++) {
            absymx = Math.max(absymx,  Math.abs(y[j]));
        }
        
        // Start of main loop for stepwise analysis
        for (jlam = 1; jlam <= nlammx; jlam++) {
            jlamp1 = jlam + 1;
            nf = 2 * jlam + ibase;
            if (prfinl) {
                Preferences.debug("Final analysis assuming " + jlam + " components\n", Preferences.DEBUG_ALGORITHM);
            } // if (prfinl)
            for (j = 0; j < jlam; j++) {
                lamst[j] = lamf[j][jlam-1][1];
            } // for (j = 0; j < jlam; j++)
            lstsqr(lamst, jlam, true, n, jlam+ibase, evarCalled, 4, ffail[jlam-1][1], false, prfinl);
            yfail[jlam-1] = failed;
            if (failed) {
                if (ffail[jlam-1][0]) {
                    varmin = Math.min(varmin, var[0]);
                    continue;
                }
                if (prfinl) {
                    Preferences.debug("Second attempt using starting lambdas from initial analysis of raw data\n",
                                      Preferences.DEBUG_ALGORITHM);    
                } // if (prfinl)
                for (j = 0; j < jlam; j++) {
                    lamst[j] = lamf[j][jlam-1][0];
                } // for (j = 0; j < jlam; j++)
                lstsqr(lamst, jlam, true, n, jlam+ibase, evarCalled, 4, false, false, prfinl);
                yfail[jlam-1] = failed;
                if (failed) {
                    varmin = Math.min(varmin, var[0]);
                    continue;
                }
            } // if (failed)
            
            // Save values for later use and for final output
            sigysv[jlam-1] = sigyy;
            varsv[jlam-1] = var[0];
            itersv[jlam-1] = iter;
            enphsv[jlam-1] = enphi;
            i = jlam;
            for (k = 0; k < jlam; k++) {
                asave[k][jlam-1][0] = palpha[k];
                asave[k][jlam-1][1] = plam[k];
                asave[k][jlam-1][2] = sigmap[k];
                i++;
                asave[k][jlam-1][3] = sigmap[i-1];
                asave[k][jlam-1][4] = pcerr[k];
                asave[k][jlam-1][5] = pcerr[i-1];
            } // for (k = 0; k < jlam; k++)
            if (!nobase) {
                asave[jlamp1-1][jlam-1][1] = 0.0;
                asave[jlamp1-1][jlam-1][2] = 0.0;
                asave[jlamp1-1][jlam-1][4] = 0.0;
                asave[jlamp1-1][jlam-1][0] = palpha[jlamp1-1];
                asave[jlamp1-1][jlam-1][3] = sigmap[nf-1];
                asave[jlamp1-1][jlam-1][5] = pcerr[nf-1];
            } // if (!nobase)
            
            // Test if this solution is the best so far
            if (varbes < 1.0E20) {
                if (var[0] >= varmin) {
                    continue;
                }
                k = n - nf;
                dum = varbes/var[0] - 1.0;
                L = 2 * (jlam - jlambs);
                if (fisher(k*dum/(L * (1.0 + enphi*((nf + 2.0) * n)/(nf * k))), L, k) <= 0.5) {
                    varmin = Math.min(varmin, var[0]);
                    continue;
                }
            } // if (varbes < 1.0E20)
            jlambs = jlam;
            varbes = var[0];
            varmin = Math.min(varmin, var[0]);
        } // for (jlam = 1; jlam <= nlammx; jlam++)
        
        // End of main loop for stepwise analysis 
        if (varbes > 1.0E20) {
            Preferences.debug("All analyses of the raw data failed - check input data for gross errors\n", Preferences.DEBUG_ALGORITHM);
            return;
        }
        
        // Calculate probsv and probln (probability and uncorrected probability (i.e., using enphi = 0.0)
        // that the best solution is actually better than the others.
        pnxbes = 1.0E20;
        jlamnx = jlambs;
        for (j = 1; j <= nlammx; j++) {
            probsv[j-1] = 2.0;
            if (j == jlambs || yfail[j-1]) {
                continue;
            }
            i = 2 * Math.max(jlambs, j) + ibase;
            k = n - i;
            L = 2 * (jlambs - j);
            dum = varsv[j-1]/varbes;
            if (L >= 0) {
                ddum = k * (dum - 1.0)/L;
                dddum = 1.0 + enphsv[jlambs-1] * ((i + 2.0) * n)/(i * k);
                probsv[j-1] = fisher(ddum/dddum, L, k);
                probln[j-1] = fisher(ddum, L, k);
            } // if (L >= 0)
            else { // L < 0
                if (dum >= 1.0) {
                    probsv[j-1] = 1.0;
                    probln[j-1] = 1.0;
                } // if (dum >= 1.0)
                else { // dum < 1.0
                    ddum = k * (1.0 - 1.0/dum)/L;
                    dddum = 1.0 + enphsv[j-1] * ((i + 2.0) * n)/(i * k);
                    probln[j-1] = 1.0 - fisher(ddum, -L, k);
                    probsv[j-1] = 1.0 - fisher(ddum/dddum, -L, k);
                } // else dum < 1.0
            } // else L < 0
            if (probsv[j-1] >= pnxbes) {
                continue;
            }
            pnxbes = probsv[j-1];
            //pnxlin = probln[j-1];
            jlamnx = j;
        } // for (j = 1; j <= nlammx; j++)
        probsv[jlambs-1] = 0.0;
        if (pnxbes >= 1.0E20) {
            pnxbes = 1.0;
            //pnxlin = 1.0;
        } // if (pnxbes >= 1.0E20)
        
        // Final summary of results - summarizes up to best 5 solutions.
        residu(jlambs, plotrs, puncor, asave);
        bigloop:
        while (true) {
            Preferences.debug("DISCRETE - Version 2B (December, 1990)\n", Preferences.DEBUG_ALGORITHM);
            for (j = 0; j < nlammx; j++) {
                done[j] = yfail[j];
            } // for (j = 0; j < nlammx; j++)
            if (jlambs == 1) {
                Preferences.debug("The best solution has " + jlambs + " component\n", Preferences.DEBUG_ALGORITHM);
            }
            else {
                Preferences.debug("The best solution has " + jlambs + " components\n", Preferences.DEBUG_ALGORITHM);    
            }
            if (pnxbes <= 0.95) {
                Preferences.debug("The approximate probability that this solution is actually best is only " + pnxbes + "\n",
                                   Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Look at the second best solution also\n", Preferences.DEBUG_ALGORITHM);
            } // if (pnxbes <= 0.95)
            for (j = 1; j <= nlammx; j++) {
                if (j == 1) {
                    done[jlambs-1] = true;
                    i = jlambs;
                } // if (j == 1)
                else { // j > 1
                    dum = 1.0E20;
                    for (k = 1; k <= nlammx; k++) {
                        if (probsv[k-1] > dum || done[k-1]) {
                            continue;
                        }
                        dum = probsv[k-1];
                        i = k;
                    } // for (k = 1; k <= nlammx; k++)
                    if (dum >= 1.0E20 || j > 5) {
                        if (!repeat) {
                            return;
                        }
                        repeat = false;
                        residu(jlambs, false, puncor, asave);
                        continue bigloop;
                    } // if (dum >= 1.0E20 || j > 5)
                    done[i-1] = true;
                    residu(i, false, puncor, asave);
                    if (j == 2) {
                        if (i == 1) {
                            Preferences.debug("The second best solution has " + i + " component\n", Preferences.DEBUG_ALGORITHM);
                        }
                        else {
                            Preferences.debug("The second best solution has " + i + " components\n", Preferences.DEBUG_ALGORITHM);    
                        }
                    }
                    else if (j == 3) {
                        if (i == 1) {
                            Preferences.debug("The third best solution has " + i + " component\n", Preferences.DEBUG_ALGORITHM);     
                        }
                        else {
                            Preferences.debug("The third best solution has " + i + " components\n", Preferences.DEBUG_ALGORITHM);
                        }
                    }
                    else if (j == 4) {
                        if (i == 1) {
                            Preferences.debug("The fourth best solution has " + i + " component\n", Preferences.DEBUG_ALGORITHM);
                        }
                        else {
                            Preferences.debug("The fourth best solution has " + i + " components\n", Preferences.DEBUG_ALGORITHM);    
                        }
                    }
                    else if (j == 5) {
                        if (i == 1) {
                            Preferences.debug("The fifth best solution has " + i + " component\n", Preferences.DEBUG_ALGORITHM);
                        }
                        else {
                            Preferences.debug("The fifth best solution has " + i + " components\n", Preferences.DEBUG_ALGORITHM);    
                        }
                    }
                    if (dum <= 0.95) {
                        Preferences.debug("A significant possibility\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // else j > 1
                Preferences.debug("alpha +- std err percent lambda +- std err percent\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Starting lambda from fit to transforms " + nftry[i-1] + " tries\n", Preferences.DEBUG_ALGORITHM);
                for (k = 1; k <= i; k++) {
                    Preferences.debug("alpha["+k+"]  = " + asave[k-1][i-1][0] + "\n", Preferences.DEBUG_ALGORITHM);;
                    Preferences.debug("standard error["+k+"] = " + asave[k-1][i-1][3] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("percent["+k+"] = " + asave[k-1][i-1][5] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("lambda["+k+"]  = " + asave[k-1][i-1][1] + "\n", Preferences.DEBUG_ALGORITHM);;
                    Preferences.debug("standard error["+k+"] = " + asave[k-1][i-1][2] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("percent["+k+"] = " + asave[k-1][i-1][4] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("starting lambda = " + lamf[k-1][i-1][1] + "\n", Preferences.DEBUG_ALGORITHM);
                } // for (k = 1; k <= i; k++)
                if (ffail[i-1][1]) {
                    Preferences.debug("No exact fit to the transforms found\n", Preferences.DEBUG_ALGORITHM);
                }
                if (!nobase) {
                    k = i + 1;
                    Preferences.debug("alpha["+k+"]  = " + asave[k-1][i-1][0] + "\n", Preferences.DEBUG_ALGORITHM);;
                    Preferences.debug("standard error["+k+"] = " + asave[k-1][i-1][3] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("percent["+k+"] = " + asave[k-1][i-1][5] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("lambda["+k+"]  = " + asave[k-1][i-1][1] + "\n", Preferences.DEBUG_ALGORITHM);;
                    Preferences.debug("standard error["+k+"] = " + asave[k-1][i-1][2] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("percent["+k+"] = " + asave[k-1][i-1][4] + "\n", Preferences.DEBUG_ALGORITHM);
                } // if (!nobase)
                if (j == 1 && pnxbes > 0.95) {
                    Preferences.debug("Approximate probability that this solution is really better than the\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("second best solution = PNG(" + jlamnx + "/" + jlambs + ") = " +
                                      pnxbes + "\n", Preferences.DEBUG_ALGORITHM);	 
                } // if (j == 1 && pnxbes > 0.95)
                if (j > 1) {
                    Preferences.debug("PNG(" + i + "/" + jlambs + ") = " + probsv[i-1] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("NPHI = " + enphsv[i-1] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Uncorrected PNG would be = " + probln[i-1] + "\n", Preferences.DEBUG_ALGORITHM);
                }
                ddum = absymx/sigysv[i-1];
                Preferences.debug("Iterations in fit = " + itersv[i-1] + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Standard deviation of fit = " + sigysv[i-1] + "\n", Preferences.DEBUG_ALGORITHM);
                if (!wted) {
                    Preferences.debug("Signal/noise ratio of fit = " + ddum + "\n", Preferences.DEBUG_ALGORITHM);
                }
                if (j == 1) {
                    Preferences.debug("lambda held between " + lamnmx[0][0] + " and " + lamnmx[1][0] + "\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("NPHI = " + enphsv[jlambs-1] + "\n", Preferences.DEBUG_ALGORITHM);
                } // if (j == 1)
                Preferences.debug("Probability residuals uncorrelated:\n", Preferences.DEBUG_ALGORITHM);
                for (k = 1; k <= 5; k++) {
                    Preferences.debug("LAG " + k + " puncor["+ (k-1) + "] = " + puncor[k-1] + "\n", Preferences.DEBUG_ALGORITHM);
                }
                Preferences.debug("Weighted average puncor[5] = " + puncor[5] + "\n", Preferences.DEBUG_ALGORITHM);
            } // for (j = 1; j <= nlammx; j++)
            if (!repeat) {
                Preferences.debug("END OF RUN\n", Preferences.DEBUG_ALGORITHM);
                return;
            }
            repeat = false;
            residu(jlambs, false, puncor, asave);
        } // while (true)
        
    } // yanlyz
    
    private double fisher(double fs, int nu1, int nu2) {
        // Calculates probability that fs(nu1, nu2) is less than f for fisher f-test.
        // nu2 must be >= 3
        // Sum until a term is found less than tol * (current value of sum).
        // Equation numbers refer to Abramowitz and Stegun, page 946.
        double tol = 1.0E-9;
        double rnu1;
        double rnu2;
        double x;
        double result = 0.0;
        double q;
        double term;
        double tx;
        double tn;
        int k;
        int j = 0;
        double th;
        double cth;
        double cc;
        double sth;
        double ss;
        double pr;
        double dum;
        int L;
        boolean go220 = false;
        
        rnu1 = nu1;
        rnu2 = nu2;
        x = rnu2/(rnu2 + rnu1 * fs);
        if ((x >= 1.0) && (fs >= 0.0)) {
            result = 0.0;
            return result;
        }
        if ((nu1 % 2) == 0) {
            // Evaluates equation 26.6.4
            q = 1.0;
            if (nu1 != 2) {
                term = 1.0;
                tx = 1.0 - x;
                tn = rnu2 - 2.0;
                k = nu1 - 2;
                for (j = 2; j <= k; j += 2) {
                    tn = tn + 2.0;
                    term = term * tn * tx/j;
                    q = q + term;
                    if (term <= tol * q) {
                        break;
                    }
                } // for (j = 2; j <= k; j += 2)
            } // if (nu1 != 2) 
            result = 1.0 - Math.pow(x, 0.5*rnu2) * q;
            return result;
        } // if ((nu1 % 2) == 0)
        if ((nu2 % 2) != 0) {
            // Evaluates equation 26.6.8
            th = Math.atan(Math.sqrt(rnu1 * fs/rnu2));
            cth = Math.cos(th);
            cc = cth * cth;
            sth = Math.sin(th);
            ss = sth * sth;
            term = 1.0;
            q = 1.0;
            k = nu2 - 2;
            pr = 1.0;
            tn = 0.0;
            if (k >= 3) {
                for (j = 3; j <= k; j += 2) {
                    tn = tn + 2.0;
                    dum = tn/j;
                    pr = pr * dum;
                    term = term * cc * dum;
                    q = q + term;
                    if (term <= tol * q) {
                        go220 = true;
                        break;
                    }
                } // for (j = 3; j <= k; j += 2)
                if (!go220) {
                    j = k;
                }
            } // if (k >= 3)
            result = th + sth * cth * q;
            q = 0.0;
            if (nu1 != 1) {
                tn = tn + 2.0;
                pr = pr * tn;
                if (j != k) {
                    L = j + 2;
                    for (j = L; j <= k; j += 2) {
                        tn = tn + 2.0;
                        pr = pr * tn/j;
                    } // for (j = L; j <= k; j += 2)
                } // if (j != k)
                tn = rnu2 - 1.0;
                q = 1.0;
                term = 1.0;
                k = nu1 - 2;
                if (k >= 3) {
                    for (j = 3; j <= k; j += 2) {
                        tn = tn + 2.0;
                        term = term * tn * ss/j;
                        q = q + term;
                        if (term <= tol * q) {
                            break;
                        }
                    } // for (j = 3; j <= k; j += 2)
                } // if (k >= 3)
                result = result - q * pr * sth * Math.pow(cth, nu2);
            } // if (nu1 != 1)
            result = result * 0.6366198;
            return result;
        } // if ((nu2 % 2) != 0)
        // nu2 % 2 == 0
        // Evaluates equation 26.6.5
        term = 1.0;
        q = 1.0;
        k = nu2 - 2;
        tn = rnu1 - 2.0;
        for (j = 2; j <= k; j += 2) {
            tn = tn + 2.0;
            term = term * tn * x/j;
            q = q + term;
            if (term <= tol * q) {
                break;
            }
        } // for (j = 2; j <= k; j += 2)
        result = Math.sqrt(1.0 - x) * q;
        return result;
    } // fisher
    
    private void residu(int jlam, boolean plot, double puncor[], double asave[][][]) {
        // Calculates weighted residuals from asave[j-1][jlam-1][0] and asave[j-1][jlam-1][1], which correspond
        // to alpha and lambda, returns puncor (approximate probability that the residuals are uncorrelated
        // for lags 1 thru 5) and weighted average in puncor[5], and if plot == true, calls plpres to plot
        // weighted residuals on printer.
        
        // Calls routines fisher and plpres
        boolean ldum;
        double dum = 0.0;
        int k;
        int j;
        double ddum;
        double rsumst;
        double rsumen;
        int L;
        int nend;
        double stbar;
        double enbar;
        double ss;
        double se;
        double ee;
        double s;
        double e;
        double rsq;
        
        jlamp1 = jlam + 1;
        ldum = !nobase;
        if (ldum) {
            dum = asave[jlamp1-1][jlam-1][0];
        } // if (ldum)
        for (k = 0; k < n; k++) {
            ylyfit[k] = y[k];
            if (ldum) {
                ylyfit[k] = ylyfit[k] - dum * sqrtw[k];
            } // if (ldum)
        } // for (k = 0; k < n; k++)
        for (j = 0; j < jlam; j++) {
            dum = asave[j][jlam-1][0];
            ddum = asave[j][jlam-1][1];
            for (k = 0; k < n; k++) {
                ylyfit[k] = ylyfit[k] - dum * Math.exp(-ddum * t[k]) * sqrtw[k];
            } // for (k = 0; k < n; k++)
        } // for (j = 0; j < jlam; j++)
        rsumst = 0.0;
        if (!ldum) {
            for (k = 0; k < n; k++) {
                rsumst = rsumst + ylyfit[k];
            } // for (k = 0; k < n; k++)
        } // if (!ldum)
        rsumen = rsumst;
        
        // Calculate autocovariances and probabilities that residuals are uncorrelated (Hamilton, page 183)
        puncor[5] = 0.0;
        for (L = 1; L <= 5; L++) {
            nend = n - L;
            rsumst = rsumst - ylyfit[nend];
            rsumen = rsumen - ylyfit[L-1];
            stbar = rsumst/nend;
            enbar = rsumen/nend;
            ss = 0.0;
            se = 0.0;
            ee = 0.0;
            j = L;
            for (k = 1; k <= nend; k++) {
                s = ylyfit[k-1] - stbar;
                j++;
                e = ylyfit[j-1] - enbar;
                ss = ss + s * s;
                se = se + s * e;
                ee = ee + e * e;
            } // for (k = 1; k <= nend; k++)
            rsq = se*se/(ss * ee);
            puncor[L-1] = 1.0 - fisher((nend - 2.0)*rsq/(1.0 - rsq), 1, nend - 2);
            puncor[5] = puncor[5] + puncor[L-1]/L;
        } // for (L = 1; L <= 5; L++)
        puncor[5] = puncor[5]/2.283333;
        //if (plot) {
            //plpres();
        //} // if (plot)
        return;
    } // residu
    

}
