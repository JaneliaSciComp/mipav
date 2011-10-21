package gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util;

/**
 *  Interface used for some univariate routine in Numeric class <BR>
 *
 *
 *@author     <A href="http://www.kutsyy.com">Vadim Kutsyy</A>
 *@created    December 1, 2000
 *@see        Numeric Created by <A href="http://www.kutsyy.com">Vadim Kutsyy
 *      </A><BR>
 *
 */
public interface Function {

    /**
     *  Univariate function definition
     *
     *@param  x  imput value
     *@return    return value
     *@see       Numeric
     */
    double f(double x);

}
