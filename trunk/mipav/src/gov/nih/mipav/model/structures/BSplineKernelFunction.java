	package gov.nih.mipav.model.structures;

import gov.nih.mipav.view.MipavUtil;
	
	
	
	
	/**
	 
	 *
	 * @version  0.1 April 4, 2014
	 * @author   William Gandler
	 * 
	  * This a a port of itkBSplineKernelFunction.h from the itk package.  Here is the original itk header
	  * from the itkBSplineKernelFunction.h file:
	  *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
    This class encapsulates BSpline kernel for density estimation or nonparametric regression.
  
	 */
	public class BSplineKernelFunction  {
		
		private int splineOrder;
	
		
	    
	    public BSplineKernelFunction(int splineOrder) {
	        this.splineOrder = splineOrder;
	    }
	    
	    
	    
	  //~ Methods --------------------------------------------------------------------------------------------------------

	    
	    
	    public double evaluate(double u) {
	    	double absValue = Math.abs(u);
	    	double sqrValue;
	    	switch (splineOrder) {
	    	    case 0:
			    	if (absValue < 0.5) {
			    		return 1.0;
			    	}
			    	else if (absValue == 0.5) {
			    		return 0.5;
			    	}
			    	else {
			    		return 0.0;
			    	}
	    	    case 1:
	    	    	if (absValue < 1.0) {
	    	    		return (1.0 - absValue);
	    	    	}
	    	    	else {
	    	    		return 0.0;
	    	    	}
	    	    case 2:
	    	    	sqrValue = absValue * absValue;
	    	    	if (absValue < 0.5) {
	    	    	    return( 0.75 - sqrValue);	
	    	    	}
	    	    	else if (absValue < 1.5) {
	    	    	    return ((9.0 - 12.0 * absValue + 4.0 * sqrValue) * 0.125);	
	    	    	}
	    	    	else {
	    	    		return 0.0;
	    	    	}
	    	    case 3:
	    	    	sqrValue = absValue * absValue;
	    	    	if (absValue < 1.0) {
	    	    		return ((4.0 - 6.0 * sqrValue + 3.0 * sqrValue * absValue)/6.0);
	    	    	}
	    	    	else if (absValue < 2.0) {
	    	    		return ((8.0 - 12.0 * absValue + 6.0 * sqrValue - sqrValue * absValue)/6.0);
	    	    	}
	    	    	else {
	    	    		return 0.0;
	    	    	}
		    	default:
		    		MipavUtil.displayError("Evaluate not implemented for splineOrder = " + splineOrder);
		    		return 0.0;
	    	}
	    }
	    
}

