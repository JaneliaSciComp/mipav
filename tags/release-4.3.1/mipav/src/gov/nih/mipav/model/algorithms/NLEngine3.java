package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.jama.JamaMatrix;

import gov.nih.mipav.view.*;


/**
 * Port of MATLAB nlinfit Nonlinearleast-squares fitting by the Gauss-Newton method.
 *  The original nlinfit was written by B.A. Jones 12-06-94.
 *  nlinfit MATLAB code is Copyright 1993-2000 The MathWorks, Inc. 
 *  Can be downloaded from http://www.math.montana.edu/~greenwood/nlinfit_mod1.m or from
 *  http://www.ks.uiuc.edu/Training/Tutorials/namd/namd-tutorial-files/2-7-echoes/01_equil_NVE/nlinfit.m
 */

public abstract class NLEngine3 {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** variables. */
    protected static double[] anew;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected double[] a; // fitted coefficients

    /** DOCUMENT ME! */
    protected double[] gues;

    /** DOCUMENT ME! */
    protected int kk; // number of iterations

    /** DOCUMENT ME! */
    protected int ma; // number of coefficients

    /** DOCUMENT ME! */
    protected int ndata; // number of data points

    /** DOCUMENT ME! */
    protected double[] xseries, yseries;

    /** DOCUMENT ME! */
    private double anorm;

    /** DOCUMENT ME! */
    private double[] aplus;

    /** DOCUMENT ME! */
    private double atol;

    /** DOCUMENT ME! */
    private double[] delta;

    /** DOCUMENT ME! */
    private double[][] eyep;

    /** DOCUMENT ME! */
    private int iter1;

    /** DOCUMENT ME! */
    private double[][] J; // Jacobian

    /** DOCUMENT ME! */
    private JamaMatrix Jplus;

    /** DOCUMENT ME! */
    private int maxiter;

    /** DOCUMENT ME! */
    private double nb;

    /** DOCUMENT ME! */
    private double[] r; // residuals

    /** DOCUMENT ME! */
    private double[] rnew;

    /** DOCUMENT ME! */
    private JamaMatrix rplus;

    /** DOCUMENT ME! */
    private double rtol;

    /** DOCUMENT ME! */
    private double s10;

    /** DOCUMENT ME! */
    private double seps;

    /** DOCUMENT ME! */
    private double sse;

    /** DOCUMENT ME! */
    private double sseold;

    /** DOCUMENT ME! */
    private JamaMatrix St;

    /** DOCUMENT ME! */
    private double[] step;

    /** DOCUMENT ME! */
    private double temp;

    /** DOCUMENT ME! */
    private double[] yfit;

    /** DOCUMENT ME! */
    private double[] yfitnew;

    /** DOCUMENT ME! */
    private double[] yplus;

    /** DOCUMENT ME! */
    private double[] za;

    /** DOCUMENT ME! */
    private double[] zerosp;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * NLEngine - non-linear fit to a function.
     *
     * @param  nPts  number of points to fit to the function to
     * @param  _ma   number of parameters of function
     */
    public NLEngine3(int nPts, int _ma) {

        try {
            ndata = nPts;
            ma = _ma;

            xseries = new double[nPts];
            yseries = new double[nPts];
            yfit = new double[nPts];
            yplus = new double[nPts];
            r = new double[nPts];

            a = new double[ma];
            gues = new double[ma];
            anew = new double[ma];
            J = new double[nPts][ma];

            for (int i = 0; i < ndata; i++) {

                for (int j = 0; j < ma; j++) {
                    J[i][j] = 0.0;
                }
            }

            za = new double[ma];

            for (int i = 0; i < ma; i++) {
                za[i] = 0.0;
            }

            eyep = new double[ma][ma];

            for (int i = 0; i < ma; i++) {

                for (int j = 0; j < ma; j++) {

                    if (i == j) {
                        eyep[i][j] = 1.0;
                    } else {
                        eyep[i][j] = 0.0;
                    }
                }
            }

            zerosp = new double[ma];

            for (int i = 0; i < ma; i++) {
                zerosp[i] = 0.0;
            }

            delta = new double[ma];
            aplus = new double[ma];
            Jplus = new JamaMatrix(nPts + ma, ma, 0.0);
            rplus = new JamaMatrix(nPts + ma, 1, 0.0);
            step = new double[ma];
            yfitnew = new double[nPts];
            rnew = new double[nPts];
        } catch (OutOfMemoryError error) { }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     *
     * @param   x1    DOCUMENT ME!
     * @param   atry  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public abstract double[] fitToFunction(double[] x1, double[] atry);

    /**
     * driver.
     */
    public void driver() {
        int i, j, k;

        try {

            // provide an initial guess for the parameters a.
            for (i = 0; i < ma; i++) {
                a[i] = gues[i];
                anew[i] = a[i] + 1.0;
            }

            maxiter = 2000;
            kk = 0;
            atol = 1.0e-4;
            rtol = 1.0e-4;
            sse = 1.0;
            seps = Math.sqrt(Math.pow(2.0, -52.0));
            s10 = Math.sqrt(10.0);

            anorm = 0.0;

            for (i = 0; i < ma; i++) {
                temp = (anew[i] - a[i]) / (a[i] + seps);
                anorm += temp * temp;
            }

            anorm = Math.sqrt(anorm);

            while (((anorm > atol) || (Math.abs((sseold - sse) / (sse + seps)) > rtol)) && (kk < maxiter)) {

                if (kk > 0) {

                    for (i = 0; i < ma; i++) {
                        a[i] = anew[i];
                    }
                }

                kk++;
                sseold = 0.0;
                yfit = fitToFunction(xseries, a);

                for (i = 0; i < ndata; i++) {
                    r[i] = yseries[i] - yfit[i];
                    sseold += r[i] * r[i];
                }

                for (k = 0; k < ma; k++) {

                    for (i = 0; i < ma; i++) {
                        delta[i] = za[i];
                    }

                    if (a[k] == 0.0) {
                        anorm = 0.0;

                        for (i = 0; i < ma; i++) {
                            anorm += a[i] * a[i];
                        }

                        nb = Math.sqrt(Math.sqrt(anorm));

                        if (nb == 0.0) {
                            delta[k] = seps;
                        } else {
                            delta[k] = seps * nb;
                        }
                    } // if (a[k] == 0.0)
                    else {
                        delta[k] = seps * a[k];
                    }

                    for (i = 0; i < ma; i++) {
                        aplus[i] = a[i] + delta[i];
                    }

                    yplus = fitToFunction(xseries, aplus);

                    for (i = 0; i < ndata; i++) {
                        J[i][k] = (yplus[i] - yfit[i]) / delta[k];
                    }
                } // for (k = 0; k < ma; k++)

                for (i = 0; i < ndata; i++) {

                    for (j = 0; j < ma; j++) {
                        Jplus.set(i, j, J[i][j]);
                    }
                }

                for (i = 0; i < ma; i++) {

                    for (j = 0; j < ma; j++) {
                        Jplus.set(i + ndata, j, 1.0e-2 * eyep[i][j]);
                    }
                }

                for (i = 0; i < ndata; i++) {
                    rplus.set(i, 0, r[i]);
                }

                for (i = 0; i < ma; i++) {
                    rplus.set(i + ndata, 0, zerosp[i]);
                }

                St = Jplus.solve(rplus);

                for (i = 0; i < ma; i++) {
                    step[i] = St.get(i, 0);
                    anew[i] = a[i] + step[i];
                }

                sse = 0.0;
                yfitnew = fitToFunction(xseries, anew);

                for (i = 0; i < ndata; i++) {
                    rnew[i] = yseries[i] - yfitnew[i];
                    sse += rnew[i] * rnew[i];
                }

                iter1 = 0;

                while ((sse > sseold) && (iter1 < 12)) {

                    for (i = 0; i < ma; i++) {
                        step[i] = step[i] / s10;
                        anew[i] = a[i] + step[i];
                    } // for (i = 0; i < ma; i++)

                    sse = 0.0;
                    yfitnew = fitToFunction(xseries, anew);

                    for (i = 0; i < ndata; i++) {
                        rnew[i] = yseries[i] - yfitnew[i];
                        sse += rnew[i] * rnew[i];
                    } // for (i = 0; i < ndata; i++)

                    iter1++;
                } // while ((sse > sseold) && (iter1 < 12))

                anorm = 0.0;

                for (i = 0; i < ma; i++) {
                    temp = (anew[i] - a[i]) / (a[i] + seps);
                    anorm += temp * temp;
                }

                anorm = Math.sqrt(anorm);
            } // while (((anorm > atol) || (Math.abs((sseold - sse)/(sse+seps)) > rtol)) &&
              // kk < maxiter)

            if (kk == maxiter) {
                Preferences.debug("NLINFIT did not converge");
                Preferences.debug("Returing results from last iteration");
            }
        } catch (Exception e) {
            Preferences.debug("driver error: " + e.getMessage());
        }
    }


    /**
     * getParameters accessor to function parameters.
     *
     * @return  the function parameters determined by the algorithm
     */
    public double[] getParameters() {
        return a;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  double[] yfit
     */
    public double[] getYfit() {
        return yfit;
    }


    /*
     *   fitLine -  @param x1    the x value of the data point  @param atry  the best guess parameter values  @return
     *   the calculated y value
     */
    /*  public double[] fitLine(double x1[], double atry[]) {
            // called by mrqcof
            // mrqcof supplies x1 and atry[]
            // function returns the calculated ymod
            double ymod[] = new double[x1.length];
            int i;
            double fac;
            try {
                          for (i = 0; i < x1.length; i++) {
                fac     = atry[1] * x1[i];
                ymod[i]    = atry[0] + fac;
                         }
            }
            catch (Exception e) {
                if (Preferences.isDebug()) System.err.println("function error: " + e.getMessage());
            }
            return ymod;
        }
      */
    /*
     *   fitExponential - a0 + a1*exp(a2*x)  @param x1    the x value of the data point  @param atry  the best guess
     * parameter values  @return      the calculated y value
     */
    /*public double[] fitExponential(double x1[], double atry[]) {
     *  // mrqcof calls function // mrqcof supplies x1 and best guess parameters atry[] // function returns the
     * calculated ymod double ymod[] = new double[x1.length]; int i; try {          for (i = 0; i < x1.length; i++) {
     *  ymod[i] = atry[0] + atry[1]*Math.exp(atry[2] * x1[i]);          } } catch (Exception e) {     if
     * (Preferences.isDebug()) System.err.println("function error: " + e.getMessage()); } return ymod;}*/


}
