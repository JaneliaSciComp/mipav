/*
 * Interface for multivariate function
 */

package gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util;

/**
 *  put your documentation comment here
 *
 *@author     kutsyy
 *@created    January 14, 2001
 */
public interface MvFunction {

    /**
     *  multivariate function
     *
     *@param  x                                            point at which
     *      fanction should be calculated
     *@return                                              value of the function
     *      at x
     *@exception  java.lang.UnsupportedOperationException  Description of
     *      Exception
     */
    double f(double[] x) throws java.lang.UnsupportedOperationException;



    /**
     *  vector of first derivitives of f(x)
     *
     *@param  x                                            point at which
     *      fanction should be calculated
     *@param  g                                            vector of first
     *      derivitives evaluated at x
     *@exception  java.lang.UnsupportedOperationException  Description of
     *      Exception
     */
    void g(double[] x, double[] g) throws java.lang.UnsupportedOperationException;
}


