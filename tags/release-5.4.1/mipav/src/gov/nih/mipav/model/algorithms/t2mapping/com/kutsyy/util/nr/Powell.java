/*
 * put your module comment here
 * formatted with JxBeauty (c) johann.langhofer@nextra.at
 */

package gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util.nr;

import gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;

/**
 *@author     Craig Jones, Erick Wong
 *@created    May 22, 2003
 *@version    0.1
 */
public class Powell extends NumericBase {
	/**
	 *  Description of the Field
	 */

	/**
	 *  Constructor for the powell object
	 *
	 */
	public Powell() {
	}


	/**
	 *  Constructor for the powell object
	 *
	 *@param  p     Description of Parameter
	 *@param  xi    Description of Parameter
	 *@param  ftol  Description of Parameter
	 *@param  func  Description of Parameter
	 */
	public Powell(double[] p, double[][] xi, double ftol, MvFunction func) {
		powell(p, xi, ftol, func);
	}


	/**
	 *  Description of the Method
	 *
	 *@param  p              Initial solution, overwritten by final solution
	 *@param  xi             Initial direction set (e.g. identity matrix)
	 *@param  ftol           Currently unused by MSMRI Powell
	 *@param  func           Objective function
	 *@exception  IllegalArgumentException      Description of Exception
	 */
	public void powell(double[] p, double[][] xi, double ftol, MvFunction func)
			 throws IllegalArgumentException {
		if (p.length != xi.length || xi.length != xi[0].length) {
			throw new IllegalArgumentException("dimensions must agree");
		}
	
		MPowell mp = new MPowell();
		mp.setVerbose(getVerbose());

		OMFunction ff = new OMFunction(func);
		mp.setObjFunction(ff);	

		VectorValue fs = new VectorValue(p.length);
		mp.minimize(p, fs);
		Vect.copy(p, fs.v);
	}
}

