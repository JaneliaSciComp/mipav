/*
 * put your module comment here
 * formatted with JxBeauty (c) johann.langhofer@nextra.at
 */

package gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util;

/**
 *  Insert the type's description here. Creation date: (10/11/2000 3:26:25 AM)
 *
 *@author     kutsyy
 *@created    January 14, 2001
 *@author:
 */
public final class MyMath {

    /**
     *  mod of a%p
     *
     *@param  a
     *@param  p
     *@return    a%p
     */
    public static double mod(double a, double p) {
        return a - ((int) a / p) * p;
    }


    /**
     *  mod of a%p
     *
     *@param  a
     *@param  p
     *@return    a%p
     */
    public static int mod(int a, int p) {
        return (int) (a - ((int) a / p) * p);
    }


    /**
     *  factorial
     *
     *@param  n  n
     *@return    n!
     */
    public final static int factorial(int n) {
        return n > 1 ? n * factorial(n - 1) : 1;
    }


    /**
     *  maximum of the vector
     *
     *@param  x
     *@return    max(x)
     */
    public final static int max(int[] x) {
        int a = x[0];
        for (int i = 0; i < x.length; i++) {
            if (a < x[i]) {
                a = x[i];
            }
        }
        return a;
    }


    /**
     *  minimum of the vector
     *
     *@param  x
     *@return    min(x)
     */
    public final static int min(int[] x) {
        int a = x[0];
        for (int i = 0; i < x.length; i++) {
            if (a > x[i]) {
                a = x[i];
            }
        }
        return a;
    }


    /**
     *  maximum of the vector
     *
     *@param  x
     *@return    max(x)
     */
    public final static double max(double[] x) {
        double a = x[0];
        for (int i = 0; i < x.length; i++) {
            if (a < x[i]) {
                a = x[i];
            }
        }
        return a;
    }


    /**
     *  minimum of the vector
     *
     *@param  x
     *@return    min(x)
     */
    public final static double min(double[] x) {
        double a = x[0];
        for (int i = 0; i < x.length; i++) {
            if (a > x[i]) {
                a = x[i];
            }
        }
        return a;
    }
}


