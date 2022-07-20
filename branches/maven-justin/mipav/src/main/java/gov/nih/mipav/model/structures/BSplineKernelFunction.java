	package gov.nih.mipav.model.structures;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
	
	
	
	
	/**
	 
	 *
	 * @version  0.1 April 4, 2014
	 * @author   William Gandler
	 * 
	  * This a a port of itkBSplineKernelFunction.h  and itkBSplineKernelFunctionTest.cxxfrom the itk package.  
	  * Here is the original itk header from the itkBSplineKernelFunction.h file:
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
		
		public BSplineKernelFunction() {
			
		}
		
	    
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
	    
	    public void selfTest() {
	    	// All 49 splineOrder = 0 tests passed
	        // All 49 splineOrder = 1 tests passed
	        // All 49 splineOrder = 2 tests passed
	    	// All 49 splineOrder = 3 tests passed
	    	int npoints = 49;
	    	double x[] = new double[] { -3, -2.875, -2.75, -2.625, -2.5, 
	    	                            -2.375, -2.25, -2.125, -2, -1.875, 
	    	                            -1.75, -1.625, -1.5, -1.375, -1.25, 
	    	                            -1.125, -1, -0.875, -0.75, -0.625, 
	    	                            -0.5, -0.375, -0.25, -0.125, 0, 
	    	                             0.125, 0.25, 0.375, 0.5, 0.625, 
	    	                             0.75, 0.875, 1, 1.125, 1.25, 
	    	                             1.375, 1.5, 1.625, 1.75, 1.875, 
	    	                             2, 2.125, 2.25, 2.375, 2.5, 
	    	                             2.625, 2.75, 2.875, 3 }; 
	    
	    	double b0[] = new double[] { 0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0.5, 1, 1, 1, 1, 
	    	                             1, 1, 1, 0.5, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0 }; 
	    
	    	double b1[] = new double[] { 0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0.125, 0.25, 0.375, 
	    	                             0.5, 0.625, 0.75, 0.875, 1, 
	    	                             0.875, 0.75, 0.625, 0.5, 0.375, 
	    	                             0.25, 0.125, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0 }; 
	 
	    	double b2[] = new double[] { 0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0.0078125, 0.03125, 
	    	                             0.0703125, 0.125, 0.195313, 0.28125, 0.382813, 
	    	                             0.5, 0.609375, 0.6875, 0.734375, 0.75, 
	    	                             0.734375, 0.6875, 0.609375, 0.5, 0.382813, 
	    	                             0.28125, 0.195313, 0.125, 0.0703125, 0.03125, 
	    	                             0.0078125, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0 }; 
	   
	    	double b3[] = new double[] { 0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0, 0.000325521, 
	    	                             0.00260417, 0.00878906, 0.0208333, 0.0406901, 0.0703125, 
	    	                             0.111654, 0.166667, 0.236003, 0.315104, 0.398112, 
	    	                             0.479167, 0.552409, 0.611979, 0.652018, 0.666667, 
	    	                             0.652018, 0.611979, 0.552409, 0.479167, 0.398112, 
	    	                             0.315104, 0.236003, 0.166667, 0.111654, 0.0703125, 
	    	                             0.0406901, 0.0208333, 0.00878906, 0.00260417, 0.000325521, 
	    	                             0, 0, 0, 0, 0, 
	    	                             0, 0, 0, 0 }; 
	    	int i;
	    	Preferences.debug("Testing BSplineKernelFunction\n", Preferences.DEBUG_ALGORITHM);
	    	int tests0Passed = 0;
	    	int tests1Passed = 0;
	    	int tests2Passed = 0;
	    	int tests3Passed = 0;
	    	
	        splineOrder = 0;
	        for (i = 0; i < npoints; i++) {
	        	double result = evaluate(x[i]);
	            if (Math.abs(result - b0[i]) > 1.0E-6) {
	            	Preferences.debug("For splineOrder = 0 failure for x = " + x[i] + "\n", Preferences.DEBUG_ALGORITHM);
	            	Preferences.debug("True answer = " + b0[i] + "  Calculated answer = " + result + "\n", Preferences.DEBUG_ALGORITHM);
	            }
	            else {
	            	tests0Passed++;
	            }
	        }
	        if (tests0Passed == npoints) {
	            Preferences.debug("All " + npoints + " splineOrder = 0 tests passed\n", Preferences.DEBUG_ALGORITHM);	
	        }
	        else {
	            Preferences.debug(tests0Passed + " out of " + npoints + " splineOrder = 0 tests passed\n", Preferences.DEBUG_ALGORITHM);	
	        }
	        
	        splineOrder = 1;
	        for (i = 0; i < npoints; i++) {
	        	double result = evaluate(x[i]);
	            if (Math.abs(result - b1[i]) > 1.0E-6) {
	            	Preferences.debug("For splineOrder = 1 failure for x = " + x[i] + "\n", Preferences.DEBUG_ALGORITHM);
	            	Preferences.debug("True answer = " + b1[i] + "  Calculated answer = " + result + "\n", Preferences.DEBUG_ALGORITHM);
	            }
	            else {
	            	tests1Passed++;
	            }
	        }
	        if (tests0Passed == npoints) {
	            Preferences.debug("All " + npoints + " splineOrder = 1 tests passed\n", Preferences.DEBUG_ALGORITHM);	
	        }
	        else {
	            Preferences.debug(tests1Passed + " out of " + npoints + " splineOrder = 1 tests passed\n", Preferences.DEBUG_ALGORITHM);	
	        }
	        
	        splineOrder = 2;
	        for (i = 0; i < npoints; i++) {
	        	double result = evaluate(x[i]);
	            if (Math.abs(result - b2[i]) > 1.0E-6) {
	            	Preferences.debug("For splineOrder = 2 failure for x = " + x[i] + "\n", Preferences.DEBUG_ALGORITHM);
	            	Preferences.debug("True answer = " + b2[i] + "  Calculated answer = " + result + "\n", Preferences.DEBUG_ALGORITHM);
	            }
	            else {
	            	tests2Passed++;
	            }
	        }
	        if (tests2Passed == npoints) {
	            Preferences.debug("All " + npoints + " splineOrder = 2 tests passed\n", Preferences.DEBUG_ALGORITHM);	
	        }
	        else {
	            Preferences.debug(tests2Passed + " out of " + npoints + " splineOrder = 2 tests passed\n", Preferences.DEBUG_ALGORITHM);	
	        }
	        
	        splineOrder = 3;
	        for (i = 0; i < npoints; i++) {
	        	double result = evaluate(x[i]);
	            if (Math.abs(result - b3[i]) > 1.0E-6) {
	            	Preferences.debug("For splineOrder = 3 failure for x = " + x[i] + "\n", Preferences.DEBUG_ALGORITHM);
	            	Preferences.debug("True answer = " + b3[i] + "  Calculated answer = " + result + "\n", Preferences.DEBUG_ALGORITHM);
	            }
	            else {
	            	tests3Passed++;
	            }
	        }
	        if (tests3Passed == npoints) {
	            Preferences.debug("All " + npoints + " splineOrder = 3 tests passed\n", Preferences.DEBUG_ALGORITHM);	
	        }
	        else {
	            Preferences.debug(tests3Passed + " out of " + npoints + " splineOrder = 3 tests passed\n", Preferences.DEBUG_ALGORITHM);	
	        }
	        
	    }
	    
}

