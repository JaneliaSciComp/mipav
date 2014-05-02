	package gov.nih.mipav.model.structures;

import gov.nih.mipav.view.Preferences;
	
	
	
	
	/**
	 
	 *
	 * @version  0.1 April 1, 2014
	 * @author   William Gandler
	 * 
	  * This a a port of itkCoxDeBoorBSplineKernelFunction.hxx, itkCoxDeBoorBSplineKernelFunction.h, and 
	  * itkCoxDeBoorBSplineKernelFunctionTest.cxx from the itk package.  
	  * Here is the original itk header
	  * from the itkCoxDeBoorBSplineKernelFunction.hxx file:
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
 * This class enscapsulates BSpline kernel for
 * density estimation or nonparameteric regression.
 * See documentation for KernelFunctionBase for more details.
 *
 * This class is templated over the spline order to cohere with
 * the previous incarnation of this class. One can change the
 * order during an instantiation's existence.  Note that
 * other authors have defined the B-spline order as being the
 * degree of spline + 1.  In the ITK context (e.g. in this
 * class), the spline order is equivalent to the degree of
 * the spline.
 *
 * \author Nicholas J. Tustison
 *
 * This code was contributed in the Insight Journal paper:
 * "N-D C^k B-Spline Scattered Data Approximation"
 * by Nicholas J. Tustison, James C. Gee
 * http://hdl.handle.net/1926/140
 * http://www.insight-journal.org/browse/publication/57
 *

  
	 */
	public class CoxDeBoorBSplineKernelFunction  {
		
		private int splineOrder;
		private double[][] BSplineShapeFunctions;
		// Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        //epsilon = 1.0;
        //neweps = 1.0;

        //while (true) {

        //     if (1.0 == (1.0 + neweps)) {
        //         break;
        //     } else {
        //         epsilon = neweps;
        //        neweps = neweps / 2.0;
        //     }
        // } // while(true)
		private double epsilon = 2.2204460e-16;
		
	    public CoxDeBoorBSplineKernelFunction() {
	    	
	    }
	    
	    public CoxDeBoorBSplineKernelFunction(int splineOrder) {
	        this.splineOrder = splineOrder;
	        generateBSplineShapeFunctions(splineOrder + 1);
	    }
	    
	  //~ Methods --------------------------------------------------------------------------------------------------------

	    /**
	     * Prepares this class for destruction.
	     */
	    public void finalize() {
	        if (BSplineShapeFunctions != null) {
	        	int i;
	        	for (i = 0; i < BSplineShapeFunctions.length; i++) {
	        		BSplineShapeFunctions[i] = null;
	        	}
	        	BSplineShapeFunctions = null;
	        }
	    }
	    
	    private void generateBSplineShapeFunctions(int order) {
	    	double knots[];
	    	int i;
	    	int j;
	    	int numberOfPieces = (int)(0.5 * (order + 1));
	    	BSplineShapeFunctions = new double[numberOfPieces][order];
	    	knots = new double[order + 1];
	    	for (i = 0; i < knots.length; i++) {
	    		knots[i] = -0.5 * order + i;
	    	}
	    	for (i = 0; i < numberOfPieces; i++) {
	    	    RealPolynomial poly = generatePoly(order, knots, 0, (int)(0.5 * order + i));
	    	    double coeff[] = poly.getCoefficients();
	    	    for (j = 0; j < coeff.length; j++) {
	    	    	BSplineShapeFunctions[i][j] = coeff[j];
	    	    }
	    	}
	    }
	    
	    private RealPolynomial generatePoly(int order, double knots[], int whichBasisFunction, int whichPiece) {
	    	double tmp[] = new double[2];
	    	RealPolynomial poly1 = new RealPolynomial(0.0);
	    	RealPolynomial poly2 = new RealPolynomial(0.0);
	    	int p = order - 1;
	    	int i = whichBasisFunction;
	    	
	    	if (p == 0 && whichBasisFunction == whichPiece) {
	    		RealPolynomial poly = new RealPolynomial(1.0);
	    		return poly;
	    	}
	    	
	    	// Term 1
	    	double den = knots[i+p] - knots[i];
	    	if (den == 0.0) {
	    		RealPolynomial poly = new RealPolynomial(0.0);
	    		poly1 = poly;
	    	}
	    	else {
	    		tmp[0] = 1.0;
	    		tmp[1] = -knots[i];
	    		tmp[0] /= den;
	    		tmp[1] /= den;
	    		poly1 = new RealPolynomial(tmp);
	    		poly1 = poly1.multiply(generatePoly(order - 1, knots, i, whichPiece));
	    	}
	    	
	    	// Term 2
	    	den = knots[i + p + 1] - knots[i+1];
	    	if (den == 0.0) {
	    		RealPolynomial poly = new RealPolynomial(0.0);
	    		poly2 = poly;	
	    	}
	    	else {
	    		tmp[0] = -1.0;
	    		tmp[1] = knots[i + p + 1];
	    		tmp[0] /= den;
	    		tmp[1] /= den;
	    		poly2 = new RealPolynomial(tmp);
	    		poly2 = poly2.multiply(generatePoly(order - 1, knots, i + 1, whichPiece));
	    	}
	    	return poly1.add(poly2);
	    }
	    
	    public double[][] getShapeFunctionsInZeroToOneInterval() {
	    	int i, j;
	        int order = splineOrder + 1;
	        int numberOfPieces = order;
	        double shapeFunctions[][] = new double[numberOfPieces][order];
	        double knots[] = new double[2 * order];
	        
	        for (i = 0; i < knots.length; i++) {
	            knots[i] = (double)(-splineOrder + i);	
	        }
	    	
	        for (i = 0; i < numberOfPieces; i++) {
	        	RealPolynomial poly = generatePoly(order, knots, i, order - 1);
	    	    double coeff[] = poly.getCoefficients();
	    	    for (j = 0; j < coeff.length; j++) {
	    	    	shapeFunctions[i][j] = coeff[j];
	    	    }	
	        }
	    	  
	    	return shapeFunctions;

	    }
	    
	    public double evaluate(double u) {
	    	double absValue = Math.abs(u);
	    	int which;
	    	if ((splineOrder % 2) == 0) {
	    		which = (int)(absValue + 0.5);
	    	}
	    	else {
	    		which = (int)(absValue);
	    	}
	    	if (which < BSplineShapeFunctions.length) {
	    		RealPolynomial poly = new RealPolynomial(BSplineShapeFunctions[which]);
	    		return poly.evaluate(absValue);
	    	}
	    	else {
	    		return 0.0;
	    	}
	    }

	    public void selfTest() {
	    	// All 4 self tests passed.
	    	int i;
	    	Preferences.debug("Testing CoxDeBoorBSplineKernelFunction\n", Preferences.DEBUG_ALGORITHM);
	    	int testsPassed = 0;
	    	
	    	double trueCoefficientsOrder0[][] = new double[1][1];
	    	trueCoefficientsOrder0[0][0] = 1.0;
	    	splineOrder = 0;
	    	generateBSplineShapeFunctions(splineOrder + 1);
	    	if (Math.abs(trueCoefficientsOrder0[0][0] - BSplineShapeFunctions[0][0]) > epsilon) {
	    		Preferences.debug("Failure for splineOrder = 0\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder0[0][0] = 1.0 BSplineShapeCoefficients[0][0] = " + 
	    		BSplineShapeFunctions[0][0] + "\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    	else {
	    		Preferences.debug("Self test passed for spline order = 0\n");
	    		testsPassed++;
	    	}
	    	for (i = 0; i < BSplineShapeFunctions.length; i++) {
        		BSplineShapeFunctions[i] = null;
        	}
        	BSplineShapeFunctions = null;
        	for (i = 0; i < trueCoefficientsOrder0.length; i++) {
        	    trueCoefficientsOrder0[i] = null;	
        	}
        	trueCoefficientsOrder0 = null;
	    	
	    	double trueCoefficientsOrder1[][] = new double[1][2];
	    	trueCoefficientsOrder1[0][0] = -1.0;
	    	trueCoefficientsOrder1[0][1] = 1.0;
	    	splineOrder = 1;
	    	generateBSplineShapeFunctions(splineOrder + 1);
	    	if ((Math.abs(trueCoefficientsOrder1[0][0] - BSplineShapeFunctions[0][0]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder1[0][1] - BSplineShapeFunctions[0][1]) > epsilon)){
	    		Preferences.debug("Failure for splineOrder = 1\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder1[0][0] = -1.0 BSplineShapeCoefficients[0][0] = " + 
	    		BSplineShapeFunctions[0][0] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder1[0][1] = 1.0 BSplineShapeCoefficients[0][1] = " + 
	    	    		BSplineShapeFunctions[0][1] + "\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    	else {
	    		Preferences.debug("Self test passed for spline order = 1\n");
	    		testsPassed++;
	    	}
	    	for (i = 0; i < BSplineShapeFunctions.length; i++) {
        		BSplineShapeFunctions[i] = null;
        	}
        	BSplineShapeFunctions = null;
        	for (i = 0; i < trueCoefficientsOrder1.length; i++) {
        	    trueCoefficientsOrder1[i] = null;	
        	}
        	trueCoefficientsOrder1 = null;
        	
        	double trueCoefficientsOrder2[][] = new double[2][3];
        	trueCoefficientsOrder2[0][0] = -1.0;
        	trueCoefficientsOrder2[0][1] = 0.0;
        	trueCoefficientsOrder2[0][2] = 0.75;
        	trueCoefficientsOrder2[1][0] = 0.5;
        	trueCoefficientsOrder2[1][1] = -1.5;
        	trueCoefficientsOrder2[1][2] = 1.125;
        	splineOrder = 2;
	    	generateBSplineShapeFunctions(splineOrder + 1);
	    	if ((Math.abs(trueCoefficientsOrder2[0][0] - BSplineShapeFunctions[0][0]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder2[0][1] - BSplineShapeFunctions[0][1]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder2[0][2] - BSplineShapeFunctions[0][2]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder2[1][0] - BSplineShapeFunctions[1][0]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder2[1][1] - BSplineShapeFunctions[1][1]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder2[1][2] - BSplineShapeFunctions[1][2]) > epsilon)){
	    		Preferences.debug("Failure for splineOrder = 2\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder2[0][0] = -1.0 BSplineShapeCoefficients[0][0] = " + 
	    		BSplineShapeFunctions[0][0] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder2[0][1] = 0.0 BSplineShapeCoefficients[0][1] = " + 
	    	    		BSplineShapeFunctions[0][1] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder2[0][2] = 0.75 BSplineShapeCoefficients[0][2] = " + 
	    	    		BSplineShapeFunctions[0][2] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder2[1][0] = 0.5 BSplineShapeCoefficients[1][0] = " + 
	    		        BSplineShapeFunctions[1][0] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder2[1][1] = -1.5 BSplineShapeCoefficients[1][1] = " + 
	    	    		BSplineShapeFunctions[1][1] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder2[1][2] = 1.125 BSplineShapeCoefficients[1][2] = " + 
	    	    		BSplineShapeFunctions[1][2] + "\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    	else {
	    		Preferences.debug("Self test passed for spline order = 2\n");
	    		testsPassed++;
	    	}
	    	for (i = 0; i < BSplineShapeFunctions.length; i++) {
        		BSplineShapeFunctions[i] = null;
        	}
        	BSplineShapeFunctions = null;
        	for (i = 0; i < trueCoefficientsOrder2.length; i++) {
        	    trueCoefficientsOrder2[i] = null;	
        	}
        	trueCoefficientsOrder2 = null;
        	
        	double trueCoefficientsOrder3[][] = new double[2][4];
        	trueCoefficientsOrder3[0][0] = 0.5;
        	trueCoefficientsOrder3[0][1] = -1.0;
        	trueCoefficientsOrder3[0][2] = 0.0;
        	trueCoefficientsOrder3[0][3] = 2.0/3.0;
        	trueCoefficientsOrder3[1][0] = -1.0/6.0;
        	trueCoefficientsOrder3[1][1] = 1.0;
        	trueCoefficientsOrder3[1][2] = -2.0;
        	trueCoefficientsOrder3[1][3] = 4.0/3.0;
        	splineOrder = 3;
	    	generateBSplineShapeFunctions(splineOrder + 1);
	    	if ((Math.abs(trueCoefficientsOrder3[0][0] - BSplineShapeFunctions[0][0]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder3[0][1] - BSplineShapeFunctions[0][1]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder3[0][2] - BSplineShapeFunctions[0][2]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder3[0][3] - BSplineShapeFunctions[0][3]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder3[1][0] - BSplineShapeFunctions[1][0]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder3[1][1] - BSplineShapeFunctions[1][1]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder3[1][2] - BSplineShapeFunctions[1][2]) > epsilon) ||
	    	    (Math.abs(trueCoefficientsOrder3[1][3] - BSplineShapeFunctions[1][3]) > epsilon)){
	    		Preferences.debug("Failure for splineOrder = 3\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder3[0][0] = 0.5 BSplineShapeCoefficients[0][0] = " + 
	    		BSplineShapeFunctions[0][0] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder3[0][1] = -1.0 BSplineShapeCoefficients[0][1] = " + 
	    	    		BSplineShapeFunctions[0][1] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder3[0][2] = 0.0 BSplineShapeCoefficients[0][2] = " + 
	    	    		BSplineShapeFunctions[0][2] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder3[0][3] = 2.0/3.0 BSplineShapeCoefficients[0][3] = " + 
	    	    		BSplineShapeFunctions[0][3] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder3[1][0] = -1.0/6.0 BSplineShapeCoefficients[1][0] = " + 
	    		        BSplineShapeFunctions[1][0] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder3[1][1] = 1.0 BSplineShapeCoefficients[1][1] = " + 
	    	    		BSplineShapeFunctions[1][1] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder3[1][2] = -2.0 BSplineShapeCoefficients[1][2] = " + 
	    	    		BSplineShapeFunctions[1][2] + "\n", Preferences.DEBUG_ALGORITHM);
	    		Preferences.debug("trueCoefficientsOrder3[1][3] = 4.0/3.0 BSplineShapeCoefficients[1][3] = " + 
	    	    		BSplineShapeFunctions[1][3] + "\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    	else {
	    		Preferences.debug("Self test passed for spline order = 3\n");
	    		testsPassed++;
	    	}
	    	for (i = 0; i < BSplineShapeFunctions.length; i++) {
        		BSplineShapeFunctions[i] = null;
        	}
        	BSplineShapeFunctions = null;
        	for (i = 0; i < trueCoefficientsOrder3.length; i++) {
        	    trueCoefficientsOrder3[i] = null;	
        	}
        	trueCoefficientsOrder3 = null;

	    	if (testsPassed == 4) {
        	    Preferences.debug("All 4 self tests passed\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    	else {
	    		Preferences.debug(testsPassed + " out of 4 self tests passed\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    }
}

