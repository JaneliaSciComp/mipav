package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.ViewUserInterface;

/**
Copyright (c) 2000-2009, Jay St. Pierre
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the distribution

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/


public class Quaternions extends AlgorithmBase {
	
    private ViewUserInterface UI;
    
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.2204460e-16
    // epsilon is called the largest relative spacing
    private double epsilon = 2.2204460E-16;
    private double neweps;
	
	public Quaternions() {
		UI = ViewUserInterface.getReference();
		epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)
	}
	
	public void runAlgorithm() {
		
	}
	
	public void test_isq() {
		// Test passes
		// TEST_ISQ runs unit tests for the ISQ function.
	
		// Release: $Name: quaternions-1_3 $
		// $Revision: 1.6 $
		// $Date: 2009-07-24 19:14:44 $
	
		// Copyright (c) 2000-2009, Jay A. St. Pierre.  All rights reserved.
	    UI.setDataText("test_title = test_isq\n");
	
		int r,c;
	    int failures=0;
		double q[][] = null;
		int truth_value;
		int test_value;
		RandomNumberGen randomGen = new RandomNumberGen();
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 2D, but neither dim is size 4');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		truth_value = 0;
		q = new double[3][5];
		for (r = 0; r < 3; r++) {
			for (c = 0; c < 5; c++) {
				q[r][c] = 1.0;
			}
		}
		test_value  = isq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isq() fails on input is 2D, but neither dim is size 4\n");
			failures++;
		}
	
		
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4xN, N~=4');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		truth_value = 1;
		q = new double[4][5];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 5; c++) {
				q[r][c] = randomGen.genUniformRandomNum(-0.5, 0.5);
			}
		}
		test_value  = isq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isq() Input is 4xN, N!=4\n");
			failures++;
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4x4, only columns are normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		double M[] = new double[4];
		double qMag = 0.0;
		for (r = 0; r < 4; r++) {
			M[r] = randomGen.genUniformRandomNum(1.0, 2.0);
			qMag += (M[r]*M[r]);
		}
		qMag = Math.sqrt(qMag);
		for (r = 0; r < 4; r++) {
			M[r] = M[r]/qMag;
		}
		q = new double[4][4];
		for (c = 0; c < 4; c++) {
			for (r = 0; r < 4; r++) {
				q[r][c] = M[r];
			}
		}
		truth_value = 1;
		test_value  = isq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isq() Input is 4x4, only columns are normalized\n");
			failures++;
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is Nx4, N~=4');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		truth_value = 2;
		q = new double[3][4];
		for (r = 0; r < 3; r++) {
			for (c = 0; c < 4; c++) {
				q[r][c] = randomGen.genUniformRandomNum(0.0, 1.0);
			}
		}
		test_value  = isq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isq() Input is Nx4, N!=4\n");
			failures++;
		}
		
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4x4, only rows are normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		M = new double[4];
		qMag = 0.0;
		for (c = 0; c < 4; c++) {
			M[c] = randomGen.genUniformRandomNum(1.0, 2.0);
			qMag += (M[c]*M[c]);
		}
		qMag = Math.sqrt(qMag);
		for (c = 0; c < 4; c++) {
			M[c] = M[c]/qMag;
		}
		q = new double[4][4];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				q[r][c] = M[c];
			}
		}
		truth_value = 2;
		test_value  = isq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isq() Input is 4x4, only rows are normalized\n");
			failures++;
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4x4, both rows and columns are normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		q = new double[4][4];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				q[r][c] = 0.5;
			}
		}
		truth_value = 3;
		test_value  = isq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isq() Input is 4x4, both rows and columns are normalized\n");
			failures++;
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4x4, neither rows nor columns are normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		q = new double[4][4];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				q[r][c] = 1.0;
			}
		}
		truth_value = 3;
		test_value  = isq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isq() Input is 4x4, neither rows nor columns are normalized\n");
			failures++;
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		UI.setDataText("In test_isq " + failures + " failures\n");
		if (failures > 0) {
			UI.setDataText("test_isq FAILED\n");
		}
	
	}
	
	public void test_isnormq() {
		// Test passes.
		// TEST_ISNORMQ runs unit tests for the ISNORMQ function.
	
		// Release: $Name: quaternions-1_3 $
		// $Revision: 1.6 $
		// $Date: 2009-07-24 19:14:44 $
	
		// Copyright (c) 2000-2009, Jay A. St. Pierre.  All rights reserved.
	    UI.setDataText("test_title = test_isnormq\n");
	
		int r,c;
	    int failures=0;
		double q[][] = null;
		int truth_value;
		int test_value;
		RandomNumberGen randomGen = new RandomNumberGen();
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 2D, but neither dim is size 4');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		truth_value = 0;
		q = new double[3][5];
		for (r = 0; r < 3; r++) {
			for (c = 0; c < 5; c++) {
				q[r][c] = 1.0;
			}
		}
		test_value  = isnormq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isnormq() fails on input is 2D, but neither dim is size 4\n");
			failures++;
		}
	
		
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4xN, N~=4, columns are not normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		truth_value = 0;
		q = new double[4][5];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 5; c++) {
				q[r][c] = randomGen.genUniformRandomNum(1.0, 2.0);
			}
		}
		test_value  = isnormq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isnormq() Input is 4xN, N!=4, columns are not normalized\n");
			failures++;
		}
		
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4xN, N != 4, columns are normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		double M[] = new double[4];
		double qMag = 0.0;
		for (r = 0; r < 4; r++) {
			M[r] = randomGen.genUniformRandomNum(1.0, 2.0);
			qMag += (M[r]*M[r]);
		}
		qMag = Math.sqrt(qMag);
		for (r = 0; r < 4; r++) {
			M[r] = M[r]/qMag;
		}
		q = new double[4][5];
		for (c = 0; c < 5; c++) {
			for (r = 0; r < 4; r++) {
				q[r][c] = M[r];
			}
		}
		truth_value = 1;
		test_value  = isnormq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isnormq() Input is 4xN, N != 4, columns are normalized\n");
			failures++;
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4x4, only columns are normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		M = new double[4];
		qMag = 0.0;
		for (r = 0; r < 4; r++) {
			M[r] = randomGen.genUniformRandomNum(1.0, 2.0);
			qMag += (M[r]*M[r]);
		}
		qMag = Math.sqrt(qMag);
		for (r = 0; r < 4; r++) {
			M[r] = M[r]/qMag;
		}
		q = new double[4][4];
		for (c = 0; c < 4; c++) {
			for (r = 0; r < 4; r++) {
				q[r][c] = M[r];
			}
		}
		truth_value = 1;
		test_value  = isnormq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isnormq() Input is 4x4, only columns are normalized\n");
			failures++;
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is Nx4, N~=4, rows are not normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		truth_value = 0;
		q = new double[3][4];
		for (r = 0; r < 3; r++) {
			for (c = 0; c < 4; c++) {
				q[r][c] = randomGen.genUniformRandomNum(1.0, 2.0);
			}
		}
		test_value  = isnormq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isnormq() Input is Nx4, N!=4, rows are not normalized\n");
			failures++;
		}
		
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is Nx4, N != 4, rows are normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		M = new double[4];
		qMag = 0.0;
		for (c = 0; c < 4; c++) {
			M[c] = randomGen.genUniformRandomNum(1.0, 2.0);
			qMag += (M[c]*M[c]);
		}
		qMag = Math.sqrt(qMag);
		for (c = 0; c < 4; c++) {
			M[c] = M[c]/qMag;
		}
		q = new double[5][4];
		for (r = 0; r < 5; r++) {
			for (c = 0; c < 4; c++) {
				q[r][c] = M[c];
			}
		}
		truth_value = 2;
		test_value  = isnormq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isnormq() Input is Nx4, N!= 4, rows are normalized\n");
			failures++;
		}
		
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4x4, only rows are normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		M = new double[4];
		qMag = 0.0;
		for (c = 0; c < 4; c++) {
			M[c] = randomGen.genUniformRandomNum(1.0, 2.0);
			qMag += (M[c]*M[c]);
		}
		qMag = Math.sqrt(qMag);
		for (c = 0; c < 4; c++) {
			M[c] = M[c]/qMag;
		}
		q = new double[4][4];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				q[r][c] = M[c];
			}
		}
		truth_value = 2;
		test_value  = isnormq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isnormq() Input is 4x4, only rows are normalized\n");
			failures++;
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4x4, both rows and columns are normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		q = new double[4][4];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				q[r][c] = 0.5;
			}
		}
		truth_value = 3;
		test_value  = isnormq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isnormq() Input is 4x4, both rows and columns are normalized\n");
			failures++;
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Input is 4x4, neither rows nor columns are normalized');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		q = new double[4][4];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				q[r][c] = 1.0;
			}
		}
		truth_value = 0;
		test_value  = isnormq(q);
		if (truth_value != test_value) {
			UI.setDataText("test_isnormq() Input is 4x4, neither rows nor columns are normalized\n");
			failures++;
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		UI.setDataText("In test_isnormq " + failures + " failures\n");
		if (failures > 0) {
			UI.setDataText("test_isnormq FAILED\n");
		}
	
	}
	
	public void test_qnorm() {
		// Test passes.
		// TEST_QNORM runs unit tests for the QNORM function.
	
		// Release: $Name: quaternions-1_3 $
		// $Revision: 1.7 $
		// $Date: 2009-07-26 20:05:13 $
	
		// Copyright (c) 2000-2009, Jay A. St. Pierre.  All rights reserved.
	
		UI.setDataText("test_title = test_qnorm\n");
		
		int r,c;
	    int failures=0;
		double q[][] = null;
		double truth_value[][];
		double test_value[][];
	
		
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Column of two quaternions');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		UI.setDataText("Column of two quaternions\n");
		q = new double[2][4];
		double row0SumSquared = 0.0;
		double row1SumSquared = 0.0;
		for (c = 0; c < 4; c++) {
			q[0][c] = c+1;
			q[1][c] = 4-c;
			row0SumSquared += (q[0][c]*q[0][c]);
			row1SumSquared += (q[1][c]*q[1][c]);
		}
		double row0Mag = Math.sqrt(row0SumSquared);
		double row1Mag = Math.sqrt(row1SumSquared);
		truth_value = new double[2][4];
		for (c = 0; c < 4; c++) {
		    truth_value[0][c] = q[0][c]/row0Mag;
		    truth_value[1][c] = q[1][c]/row1Mag;
		}
		test_value = qnorm(q);
		double absDiff;
		for (r = 0; r < 2; r++) {
			for (c = 0; c < 4; c++) {
			    absDiff = Math.abs(test_value[r][c] - truth_value[r][c]);
			    if (absDiff > epsilon) {
			    	failures++;
			    }
			}
		}
		UI.setDataText("In test_qnorm column of two quaternions failures = " + failures + "\n");
		
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Row of 6 quaternions');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		failures = 0;
		UI.setDataText("Row of 6 quaternions\n");
		truth_value = new double[4][6];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 6; c++) {
				truth_value[r][c] = 0.5;
			}
		}
		q = new double[4][6];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 6; c++) {
				q[r][c] = 1.0;
			}
		}
		test_value = qnorm(q);
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 6; c++) {
			    absDiff = Math.abs(test_value[r][c] - truth_value[r][c]);
			    if (absDiff > epsilon) {
			    	failures++;
			    }
			}
		}
		UI.setDataText("In test_qnorm row of 6 quaternions failures = " + failures + "\n");
	
	}
	
	public void test_qmult() {
		// Test passes.
		// TEST_QMULT runs unit tests for the QMULT function.
	
		// Release: $Name: quaternions-1_3 $
		// $Revision: 1.8 $
		// $Date: 2009-07-26 20:05:13 $
	
		// Copyright (c) 2000-2009, Jay A. St. Pierre.  All rights reserved.
	
		UI.setDataText("test_title = test_qmult\n");
	
		int failures=0;
	
		double q[] = new double[] {0, 0, 0, 1};
	    double q1[][];
	    double q2[][];
	    double q1q2[][];
	    int r,c;
	    double truth_value[][];
	    double test_value[][];
	    int wrong_values;
	
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('q1 and q2 are vectors of different lengths');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//expected_err = ...
		//  ['Inputs do not have the same number of elements:', 10, ...
		//   '   number of quaternions in q1 = 5', 10,...
		//   '   number of quaternions in q2 = 3', 10,...
		//   'Inputs must have the same number of elements, or', 10, ...
		//   'one of the inputs must be a single quaternion (not a', 10, ...
		//   'vector of quaternions).'];
	    q1 = new double[4][5];
	    for (r = 0; r < 4; r++) {
	    	for (c = 0; c < 5; c++) {
	    		q1[r][c] = 1.0;
	    	}
	    }
	    q2 = new double[3][4];
	    for (r = 0; r < 3; r++) {
	    	for (c = 0; c < 4; c++) {
	    		q2[r][c] = 1.0;
	    	}
	    }
		q1q2 = qmult(q1,q2);
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//%% Products
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('quaternions are row vectors');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		double preq1[][] = new double[][] {{1, 2.0, 3, 4}};
		q1 = qnorm(preq1);
		double preq2[][] = new double[][] {{-1, 0.5, 2, -2}};
		q2 = qnorm(preq2);
		q1q2 = new double[][] {{q1[0][0]*q2[0][3] + q1[0][1]*q2[0][2] -q1[0][2]*q2[0][1] + q1[0][3]*q2[0][0],
			-q1[0][0]*q2[0][2] + q1[0][1]*q2[0][3] + q1[0][2]*q2[0][0] + q1[0][3]*q2[0][1],
			q1[0][0]*q2[0][1] - q1[0][1]*q2[0][0] + q1[0][2]*q2[0][3] + q1[0][3]*q2[0][2],
			-q1[0][0]*q2[0][0] - q1[0][1]*q2[0][1] - q1[0][2]*q2[0][2] + q1[0][3]*q2[0][3]}};
		truth_value = new double[1][4];
		for (c = 0; c < 4; c++) {
			truth_value[0][c] = q1q2[0][c];
		}
		
		test_value  = qmult(q1, q2);
		wrong_values = 0;
		for (c = 0; c < 4; c++) {
			if (Math.abs(truth_value[0][c] - test_value[0][c]) > 1.0E-15) {
				wrong_values++;
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qmult quaterninons are row vectors failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('q1 is a column vector, q2 is a row vector');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		truth_value = new double[4][1];
		for (r = 0; r < 4; r++) {
			truth_value[r][0] = q1q2[0][r];
		}
		double q1t[][] = new double[4][1];
		for (r = 0; r < 4; r++) {
			q1t[r][0] = q1[0][r];
		}
		test_value  = qmult(q1t, q2);
		wrong_values = 0;
		for (r = 0; r < 4; r++) {
			if (Math.abs(truth_value[r][0] - test_value[r][0]) > 1.0E-15) {
				wrong_values++;
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qmult q1 is a column vector, q2 is a row vector failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('q1 is a row vector, q2 is a column vector');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		truth_value = new double[1][4];
		for (c = 0; c < 4; c++) {
			truth_value[0][c] = q1q2[0][c];
		}
		double q2t[][] = new double[4][1];
		for (r = 0; r < 4; r++) {
			q2t[r][0] = q2[0][r];
		}
		test_value  = qmult(q1, q2t);
		wrong_values = 0;
		for (c = 0; c < 4; c++) {
			if (Math.abs(truth_value[0][c] - test_value[0][c]) > 1.0E-15) {
				wrong_values++;
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qmult q1 is a row vector, q2 is a column vector failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('quaternions are column vectors');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		double preq3[][] = new double[][]{{0.2}, {-1.0}, {0.5}, {0.3}};
		double q3[][] = qnorm(preq3);
		double preq4[][] = new double[][] {{2.0}, {1.5}, {-1.0}, {0.5}};
		double q4[][] = qnorm(preq4);
		double q3q4[][] = new double[][] {{q3[0][0]*q4[3][0] + q3[1][0]*q4[2][0] -q3[2][0]*q4[1][0] + q3[3][0]*q4[0][0]},
			{-q3[0][0]*q4[2][0] + q3[1][0]*q4[3][0] + q3[2][0]*q4[0][0] + q3[3][0]*q4[1][0]},
			{q3[0][0]*q4[1][0] - q3[1][0]*q4[0][0] + q3[2][0]*q4[3][0] + q3[3][0]*q4[2][0]},
			{-q3[0][0]*q4[0][0] - q3[1][0]*q4[1][0] - q3[2][0]*q4[2][0] + q3[3][0]*q4[3][0]}};
		truth_value = new double[4][1];
		for (r = 0; r < 4; r++) {
			truth_value[r][0] = q3q4[r][0];
		}
		test_value  = qmult(q3, q4);
		wrong_values = 0;
		for (r = 0; r < 4; r++) {
			if (Math.abs(truth_value[r][0] - test_value[r][0]) > 1.0E-15) {
				wrong_values++;
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qmult quaternions are column vectors failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Multiply two vectors of quaternions');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		double Q1[][] = new double[4][2];
		for (r = 0; r < 4; r++) {
			Q1[r][0] = q1[0][r];
			Q1[r][1] = q3[r][0];
		}
		double Q2[][] = new double[2][4];
		for (r = 0; r < 4; r++) {
			Q2[0][r] = q2[0][r];
			Q2[1][r] = q4[r][0];
		}
		double q1q2t[][] = new double[4][1];
		for (r = 0; r < 4; r++) {
			q1q2t[r][0] = q1q2[0][r];
		}
		truth_value = new double[4][2];
		for (r = 0; r < 4; r++) {
			truth_value[r][0] = q1q2t[r][0];
			truth_value[r][1] = q3q4[r][0];
		}
		test_value  = qmult(Q1, Q2);
		wrong_values = 0;
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 2; c++) {
				if (Math.abs(truth_value[r][c] - test_value[r][c]) > 1.0E-15) {
					wrong_values++;
				}
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qmult Multiply 2 vectors of quaternions failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Multiply vector of quaternions by a single quaternion');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		Q1 = new double[1][4];
		for (c = 0; c < 4; c++) {
			Q1[0][c] = q1[0][c];
		}
		Q2 = new double[4][2];
		for (r = 0; r < 4; r++) {
			Q2[r][0] = q2[0][r];
			Q2[r][1] = q4[r][0];
		}
		double q1q4[][] = new double[][] {{q1[0][0]*q4[3][0] + q1[0][1]*q4[2][0] -q1[0][2]*q4[1][0] + q1[0][3]*q4[0][0],
			-q1[0][0]*q4[2][0] + q1[0][1]*q4[3][0] + q1[0][2]*q4[0][0] + q1[0][3]*q4[1][0],
			q1[0][0]*q4[1][0] - q1[0][1]*q4[0][0] + q1[0][2]*q4[3][0] + q1[0][3]*q4[2][0],
			-q1[0][0]*q4[0][0] - q1[0][1]*q4[1][0] - q1[0][2]*q4[2][0] + q1[0][3]*q4[3][0]}};
	    truth_value = new double[2][4];
	    for (c = 0; c < 4; c++) {
	    	truth_value[0][c] = q1q2[0][c];
	    	truth_value[1][c] = q1q4[0][c];
	    }
		
		test_value  = qmult(Q1, Q2);
		wrong_values = 0;
		for (r = 0; r < 2; r++) {
			for (c = 0; c < 4; c++) {
				if (Math.abs(truth_value[r][c] - test_value[r][c]) > 1.0E-15) {
					wrong_values++;
				}
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qmult Multiply vector of quaternions by a single quaternion failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Q1 is of indeterminate shape, Q2 is a row');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		Q1 = new double[4][4];
		for (c = 0; c < 4; c++) {
			Q1[0][c] = q1[0][c];
		}
		for (r = 1; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				Q1[r][c] = 1.0;
			}
		}
		Q2 = new double[1][4];
		for (c = 0; c < 4; c++) {
			Q2[0][c] = q2[0][c];
		}
		truth_value = new double[1][4];
		for (c = 0; c < 4; c++) {
			truth_value[0][c] = q1q2[0][c];
		}
		double Q1Q2[][] = qmult(Q1, Q2);
		test_value = new double[1][4];
		for (c = 0; c < 4; c++) {
		    test_value[0][c] = Q1Q2[0][c];	
		}
		wrong_values = 0;
		for (c = 0; c < 4; c++) {
			if (Math.abs(truth_value[0][c] - test_value[0][c]) > 1.0E-15) {
				wrong_values++;
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qmult Q1 is of indeterminate shape, Q2 is a row failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Q1 is of indeterminate shape, Q2 is a column');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		Q1 = new double[4][4];
		for (r = 0; r < 4; r++) {
			Q1[r][0] = q3[r][0];
		}
		for (r = 0; r < 4; r++) {
			for (c = 1; c < 4; c++) {
				Q1[r][c] = 1.0;
			}
		}
		Q2 = new double[4][1];
		for (r = 0; r < 4; r++) {
			Q2[r][0] = q4[r][0];
		}
		truth_value = new double[4][1];
		for (r = 0; r < 4; r++) {
			truth_value[r][0] = q3q4[r][0];
		}
		Q1Q2 = qmult(Q1, Q2);
		test_value = new double[4][1];
		for (r = 0; r < 4; r++) {
			test_value[r][0] = Q1Q2[r][0];
		}
		wrong_values = 0;
		for (r = 0; r < 4; r++) {
			if (Math.abs(truth_value[r][0] - test_value[r][0]) > 1.0E-15) {
				wrong_values++;
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qmult Q1 is of indeterminate shape, Q2 is a column failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Both inputs 4x4, normalized differently');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		Q1 = new double[4][4];
		for (c = 0; c < 4; c++) {
			Q1[0][c] = q1[0][c];
			Q1[1][c] = q3[c][0];
			Q1[2][c] = q1[0][c];
			Q1[3][c] = q3[c][0];
		}
	    Q2 = new double[4][4];
	    for (c = 0; c < 4; c++) {
	    	Q2[0][c] = q2[0][c];
	    	Q2[1][c] = q4[c][0];
	    	Q2[2][c] = q2[0][c];
	    	Q2[3][c] = q4[c][0];
	    }
		truth_value = new double[4][4];
		for (c = 0; c < 4; c++) {
			truth_value[0][c] = q1q2[0][c];
			truth_value[1][c] = q3q4[c][0];
			truth_value[2][c] = q1q2[0][c];
			truth_value[3][c] = q3q4[c][0];
		}
		test_value  = qmult(Q1, Q2);
		wrong_values = 0;
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				if (Math.abs(truth_value[r][c] - test_value[r][c]) > 1.0E-15) {
					wrong_values++;
				}
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qmult Both inputs 4x4, normalized differently failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Both inputs 4x4, and of indeterminate shape');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		Q1 = new double[4][4];
		Q2 = new double[4][4];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				Q1[r][c] = 1.0;
				Q2[r][c] = 1.0;
			}
		}
		
		truth_value = new double[4][4];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 3; c++) {
				truth_value[r][c] = 2.0;
			}
			truth_value[r][3] = -2.0;
		}
		test_value  = qmult(Q1, Q2);
		wrong_values = 0;
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				if (Math.abs(truth_value[r][c] - test_value[r][c]) > 1.0E-15) {
					wrong_values++;
				}
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qmult Both inputs 4x4, and of indeterminate shape failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
	
		UI.setDataText("In test_qmult " + failures + " failures\n");
		if (failures > 0) {
			UI.setDataText("test_qmult FAILED\n");
		}
	}

	public void test_qconj() {
		// Test passes.
		// TEST_QCONJ runs unit tests for the QCONJ function.
	
		// Release: $Name: quaternions-1_3 $
		// $Revision: 1.8 $
		// $Date: 2009-07-26 20:05:12 $
	
		// Copyright (c) 2000-2009, Jay A. St. Pierre.  All rights reserved.
	
		UI.setDataText("test_title = test_qconj\n");
		
		int failures=0;
	    int r,c;
	    double truth_value[][];
	    double test_value[][];
	    int wrong_values;
	    double qin[][];
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Column of two quaternions');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	    truth_value = new double[2][4];
	    for (r = 0; r < 2; r++) {
	    	for (c = 0; c < 3; c++) {
	    		truth_value[r][c] = -1.0;
	    	}
	    	truth_value[r][3] = 1.0;
	    }
        qin = new double[2][4];
        for (r = 0; r < 2; r++) {
	    	for (c = 0; c < 4; c++) {
	    		qin[r][c] = 1.0;
	    	}
	    }
		test_value  = qconj(qin);
		wrong_values = 0;
		for (r = 0; r < 2; r++) {
			for (c = 0; c < 4; c++) {
				if (truth_value[r][c] != test_value[r][c]) {
					wrong_values++;
				}
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qconj Column of two quaternions failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Row of 6 quaternions');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		truth_value = new double[4][6];
		for (c = 0; c < 6; c++) {
			for (r = 0; r < 3; r++) {
				truth_value[r][c] = -1.0;
			}
			truth_value[3][c] = 1.0;
		}
		qin = new double[4][6];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 6; c++) {
				qin[r][c] = 1.0;
			}
		}
		test_value  = qconj(qin);
		wrong_values = 0;
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 6; c++) {
				if (truth_value[r][c] != test_value[r][c]) {
					wrong_values++;
				}
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qconj Row of 6 quaternions failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//disp_test_name('Ambiguous Input: 4x4 normalized in both directions');
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//expected_warn = ['Component quaternion shape indeterminate, assuming' ...
		                 //' row vectors'];
		truth_value = new double[4][4];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 3; c++) {
				truth_value[r][c] = -0.5;
			}
			truth_value[r][3] = 0.5;
		}
		qin = new double[4][4];
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				qin[r][c] = 0.5;
			}
		}
		test_value  = qconj(qin);
		wrong_values = 0;
		for (r = 0; r < 4; r++) {
			for (c = 0; c < 4; c++) {
				if (truth_value[r][c] != test_value[r][c]) {
					wrong_values++;
				}
			}
		}
		
		if (wrong_values > 0) {
			failures++;
			UI.setDataText("In test_qconj Ambiguous Input: 4x4 normalized in both directions failed with " + wrong_values + " wrong values\n");
		}
	
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
		UI.setDataText("In test_qconj " + failures + " failures\n");
		if (failures > 0) {
			UI.setDataText("test_qconj FAILED\n");
		}
	}




	
	public int isq(double q[][]) {
			// ISQ(Q) checks to see if Q is a quaternion or set of quaternions.
			//     ISQ returns a value accordingly:
			
			//        0 if Q is not a quaternion or vector of quaternions:
			//          has more than 2 dimensions or neither dimension is of length 4
			       
			//        1 if the component quaternions of Q are column vectors:
			//          Q is 4xN, where N~=4, or
			//          Q is 4x4 and only the columns are normalized 
			
			//        2 if the component quaternions of Q are row vectors:
			//          Q is Nx4, where N~=4, or
			//          Q is 4x4 and only the rows are normalized 
			
			//        3 if the shape of the component quaternions is indeterminant:
			//          Q is 4x4, and either both the columns and rows are normalized
			//          or neither the columns nor rows are normalized.
			
			//     In other words, if Q is 4x4, ISQ attempts to discern the shape of
			//     component quaternions by determining whether the rows or the columns
			//     are normalized (i.e., it assumes that normalized quaternions are
			//     the more typical use of quaternions).
			
			//     The test for normalization uses 5*EPS as a tolerance.
			//
			// See also ISNORMQ, EPS.

			// Release: $Name: quaternions-1_3 $
			// $Revision: 1.7 $
			// $Date: 2009-07-26 20:05:12 $
			 
			// Copyright (c) 2001-2009, Jay A. St. Pierre.  All rights reserved.

			
              int r,c;
			  double tol=5*epsilon;
			  
			  int row_size_q= q.length;
			  int col_size_q = q[0].length;
			  
			  if ((row_size_q != 4) && (col_size_q != 4)) {
				  // Not a quaternion or quaternion vector
				  return 0;
			  }
			  double col_sum_squared;
			  double col_diff;
			  double col_diff_total = 0.0;
			  boolean col_diff_within_tol = true;
			  double row_sum_squared;
			  double row_diff;
			  double row_diff_total = 0.0;
			  boolean row_diff_within_tol = true;
	
	          if ((row_size_q == 4) && (col_size_q == 4)) {
	        	  for (r = 0; r < 4; r++) {
	        	      row_sum_squared = 0.0;
	        	      for (c = 0; c < 4; c++) {
	        	    	  row_sum_squared += q[r][c]*q[r][c];
	        	      }
	        	      row_diff = Math.abs(row_sum_squared - 1.0);
	        	      row_diff_total += row_diff;
	        	  }
	        	  if (row_diff_total > tol) {
	        	      row_diff_within_tol = false;  
	        	  }
	        	  
	        	  for (c = 0; c < 4; c++) {
	        		  col_sum_squared = 0.0;
	        		  for (r = 0; r < 4; r++) {
	        			  col_sum_squared += q[r][c]*q[r][c];
	        		  }
	        		  col_diff = Math.abs(col_sum_squared - 1.0);
	        		  col_diff_total += col_diff;
	        	  }
	        	  if (col_diff_total > tol) {
	        		  col_diff_within_tol = false;
	        	  }
	          }
	          
	          if ((row_size_q == 4) && ((col_size_q != 4) || (col_diff_within_tol && (!row_diff_within_tol)))) {
	        	  // Component q's are column vectors
	        	  return 1;  
	          }
	          
	          if ((col_size_q == 4) && ((row_size_q != 4) || (row_diff_within_tol && (!col_diff_within_tol)))) {
	        	  // Component q's are row vectors
	        	  return 2;
	          }
	          
	          // Components q's are either columns or rows (indeteminate)
	          return 3;

	}
	
	public int isnormq(double q[][]) {
			// ISQ(Q) checks to see if Q is a normalized quaternion or set of quaternions.
			//     ISNORMQ returns a value accordingly:
			
			//        0 if Q is not a normalized quaternion or a vector of normalized
			//        quaternions.
			
			//        1 if Q is 4xN and only the columns are normalized.
			
			//        2 if Q is Nx4 and only the rows are normalized.
			
			//        3 if Q is 4x4 and both the columns and rows are normalized.
			
			//     The test for normalization uses 5*EPS as a tolerance.
			
			//     Some texts refer to a normalized quaternion as a "versor".
			
			// See also ISQ, EPS.

			// Release: $Name: quaternions-1_3 $
			// $Revision: 1.7 $
			// $Date: 2009-07-26 20:05:12 $
			 
			// Copyright (c) 2001-2009, Jay A. St. Pierre.  All rights reserved.

		  int r,c;
		  double tol=5*epsilon;
		  
		  int row_size_q= q.length;
		  int col_size_q = q[0].length;
		  
		  if ((row_size_q != 4) && (col_size_q != 4)) {
			  // Not a quaternion or quaternion vector
			  return 0;
		  }
		  double col_sum_squared;
		  double col_diff;
		  double col_diff_total = 0.0;
		  boolean cols_are_norm = true;
		  double row_sum_squared;
		  double row_diff;
		  double row_diff_total = 0.0;
		  boolean rows_are_norm = true;
		  
		  if (row_size_q != 4) {
			  cols_are_norm = false;
		  }
		  else {
			  for (c = 0; c < col_size_q; c++) {
        		  col_sum_squared = 0.0;
        		  for (r = 0; r < 4; r++) {
        			  col_sum_squared += q[r][c]*q[r][c];
        		  }
        		  col_diff = Math.abs(col_sum_squared - 1.0);
        		  col_diff_total += col_diff;
        	  }
        	  if (col_diff_total > tol) {
        		  cols_are_norm = false;
        	  }  
		  }
		  
		  if (col_size_q != 4) {
			  rows_are_norm = false;
		  }
		  else {
			  for (r = 0; r < row_size_q; r++) {
        		  row_sum_squared = 0.0;
        		  for (c = 0; c < 4; c++) {
        			  row_sum_squared += q[r][c]*q[r][c];
        		  }
        		  row_diff = Math.abs(row_sum_squared - 1.0);
        		  row_diff_total += row_diff;
        	  }
        	  if (row_diff_total > tol) {
        		  rows_are_norm = false;
        	  }   
		  }
		  
		  if ((!cols_are_norm) && (!rows_are_norm)) {
			  // Not a normalized quaternion or quaternion vector
			  return 0;
		  }
		  
		  if (cols_are_norm && (!rows_are_norm)) {
			  // Component normalized q's are column vectors
			  return 1;
		  }
		  
		  if (rows_are_norm && (!cols_are_norm)) {
			  // Component normalized q's are row vectors
			  return 2;
		  }
		  
		  // Component normalized q's are either columns or rows
		  return 3;

	}


	public double[][] qnorm(double qinorg[][]) {
			// QNORM(Q) normalizes quaternions.
			//     Works on vectors of quaternions too.  If input is a vector of four
			//    quaternions, QNORM will determine whether the quaternions are row or
			//     column vectors according to ISQ.
			
			// See also ISQ.

			// Release: $Name: quaternions-1_3 $
			// $Revision: 1.11 $
			// $Date: 2009-07-26 20:05:12 $
			 
			// Copyright (c) 2001-2009, Jay A. St. Pierre.  All rights reserved.


			int r,c;
			int qtype = isq(qinorg);
			  if ( qtype == 0 ) {
			    System.err.println("Invalid qnorm input: must be a quaternion or a vector of quarternions");
			    return null;
			  }
			  else if ( qtype==3 ) {
			    System.out.println("Warning qnorm:indeterminateShape");
			    System.out.println("Component quaternion shape indeterminate, assuming row vectors");
			  }


			// Make sure qin is a row of quaternions
			 double qin[][];
			if( qtype == 1 ) {
			    qin = new double[qinorg[0].length][qinorg.length];
			    for (r = 0; r < qinorg.length; r++) {
			    	for (c = 0; c < qinorg[0].length; c++) {
			    		qin[c][r] = qinorg[r][c];
			    	}
			    }
			}
			else {
				qin = qinorg;
			}

			double rowSquareSum;
			double qmag;
			double qout[][] = new double[qin.length][qin[0].length];
			// Find the magnitude of each quaternion
			for (r = 0; r < qin.length; r++) {
			    rowSquareSum = 0.0;
			    for (c = 0; c < 4; c++) {
	                rowSquareSum += (qin[r][c]*qin[r][c]);		    	
			    }
			    qmag = Math.sqrt(rowSquareSum);
			    for (c = 0; c < 4; c++) {
			    	qout[r][c] = qin[r][c]/qmag;
			    }
			}
			
            if (qtype != 1) {
            	return qout;
            }
			// Make sure output is same shape as input
			double qtranspose[][] = new double[qout[0].length][qout.length];
			for (r = 0; r < qout.length; r++) {
				for (c = 0; c < qout[0].length; c++) {
					qtranspose[c][r] = qout[r][c];
				}
			}
			return qtranspose;
	}

	public double[][] qmult(double q1org[][], double q2org[][]) {
			// QMULT(Q1,Q2) calculates the product of two quaternions Q1 and Q2.
			//    Inputs can be vectors of quaternions, but they must either have the
			//    same number of component quaternions, or one input must be a single
			//    quaternion.  QMULT will determine whether the component quaternions of
			//    the inputs are row or column vectors according to ISQ.
			  
			//    The output will have the same shape as Q1.  If the component
			//    quaternions of either Q1 or Q2 (but not both) are of indeterminate
			//    shape (see ISQ), then the shapes will be assumed to be the same for
			//    both inputs.  If both Q1 and Q2 are of indeterminate shape, then both
			//    are assumed to be composed of row vector quaternions.
			
			// See also ISQ.

			// Release: $Name: quaternions-1_3 $
			// $Revision: 1.14 $
			// $Date: 2009-07-26 20:05:12 $
			 
			// Copyright (c) 2001-2009, Jay A. St. Pierre.  All rights reserved.
			 
            int r,c;
			int q1type = isq(q1org);
			if ( q1type == 0 ) {
			    System.err.println("Invalid qmult input: q1org must be a quaternion or a vector of quaternions");
			    return null;
	        }
			int q2type = isq(q2org);
			if ( q2type == 0 ) {
				System.err.println("Invalid qmult input: q2org must be a quaternion or a vector of quaternions");
			    return null;
			}

			// Make sure q1 is a column of quaternions (components are rows)
			double q1mid[][];
			if ( q1type==1 || (q1type==3 && q2type==1) ) {
			  q1mid = new double[q1org[0].length][q1org.length];
			  for (r = 0; r < q1org.length; r++) {
				  for (c = 0; c < q1org[0].length; c++) {
					  q1mid[c][r] = q1org[r][c];
				  }
			  }
			}
			else {
				q1mid = q1org;
			}

			// Make sure q2 is a column of quaternions (components are rows)
			double q2mid[][];
			if ( q2type==1 || (q2type==3 && q1type==1) ) {
			    q2mid = new double[q2org[0].length][q2org.length];
			    for (r = 0; r < q2org.length; r++) {
			    	for (c = 0; c < q2org[0].length; c++) {
			    		q2mid[c][r] = q2org[r][c];
			    	}
			    }
			}
			else {
				q2mid = q2org;
			}

			int num_q1 = q1mid.length;
			int num_q2 = q2mid.length;

			if (  num_q1 !=num_q2 && num_q1 !=1 && num_q2 !=1 ) {
			  System.err.println("Inputs do not have the same number of elements:");
			  System.err.println("Number of quaternions in q1 = " + num_q1);
			  System.err.println("Number of quaternions in q2 = " + num_q2);
			  System.err.println("Inputs must have the same number of elements, or");
			  System.err.println("one of the inputs must be a single quaternion (not a");
			  System.err.println("vector of quaternions).");
			  return null;
			}

			// Build up full quaternion vector if one input is a single quaternion
			double q1[][];
			double q2[][];
			if (( num_q1 != num_q2 ) && (num_q1 == 1)) {
				q1 = new double[num_q2][4];
				for (r = 0; r < num_q2; r++) {
					for (c = 0; c < 4; c++) {
						q1[r][c] = q1mid[0][c];
					}
				}
				q2 = q2mid;
			}
			else if ((num_q1 != num_q2) && (num_q2 == 1)) {
				q2 = new double[num_q1][4];
				for (r = 0; r < num_q1; r++) {
					for (c = 0; c < 4; c++) {
						q2[r][c] = q2mid[0][c];
					}
				}
				q1 = q1mid;
			}
			else {
				q1 = q1mid;
				q2 = q2mid;
			}
			  
			// Products

			// If q1 and q2 are not vectors of quaternions, then:
			//
			//   q1*q2 = q1*[ q2(4) -q2(3)  q2(2) -q2(1)
			//                q2(3)  q2(4) -q2(1) -q2(2)
			//               -q2(2)  q2(1)  q2(4) -q2(3)
			//                q2(1)  q2(2)  q2(3)  q2(4) ]
			
			// But to deal with vectorized quaternions, we have to use the ugly
			// commands below.
			int rows = Math.max(num_q1, num_q2);
			double prod1[][] = new double[rows][4];
			double prod2[][] = new double[rows][4];
			double prod3[][] = new double[rows][4];
			double prod4[][] = new double[rows][4];
			for (r = 0; r < rows; r++) {
				prod1[r][0] = q1[r][0]*q2[r][3];
				prod1[r][1] = -q1[r][0]*q2[r][2];
				prod1[r][2] = q1[r][0]*q2[r][1];
				prod1[r][3] = -q1[r][0]*q2[r][0];
				prod2[r][0] = q1[r][1]*q2[r][2];
				prod2[r][1] = q1[r][1]*q2[r][3];
				prod2[r][2] = -q1[r][1]*q2[r][0];
				prod2[r][3] = -q1[r][1]*q2[r][1];
				prod3[r][0] = -q1[r][2]*q2[r][1];
				prod3[r][1] = q1[r][2]*q2[r][0];
				prod3[r][2] = q1[r][2]*q2[r][3];
				prod3[r][3] = -q1[r][2]*q2[r][2];
				prod4[r][0] = q1[r][3]*q2[r][0];
				prod4[r][1] = q1[r][3]*q2[r][1];
				prod4[r][2] = q1[r][3]*q2[r][2];
				prod4[r][3] = q1[r][3]*q2[r][3];
			}
			
			double qout[][] = new double[rows][4];
			for (r = 0; r < rows; r++) {
				for (c = 0; c < 4; c++) {
					qout[r][c] = prod1[r][c] + prod2[r][c] + prod3[r][c] + prod4[r][c];
				}
			}

			

			// Make sure output is same format as q1
			if ( q1type==1 || (q1type==3 && q2type==1) ) {
			  double qtranspose[][] = new double[4][rows];
			  for (r = 0; r < rows; r++) {
				  for (c = 0; c < 4; c++) {
					  qtranspose[c][r] = qout[r][c];
				  }
			  }
			  return qtranspose;
			}
			
			return qout;

			// NOTE that the following algorithm proved to be slower than the one used
			// above:
			
			// q_out = zeros(size(q1));
			 
			// q_out(:,1:3) = ...
			//     [q1(:,4) q1(:,4) q1(:,4)].*q2(:,1:3) + ...
			//     [q2(:,4) q2(:,4) q2(:,4)].*q1(:,1:3) + ...
			//     cross(q1(:,1:3), q2(:,1:3));
			// 
			// q_out(:,4) = q1(:,4).*q2(:,4) - dot(q1(:,1:3), q2(:,1:3), 2);
	}

	public double[][] qconj(double qinorg[][]) {
			// QCONJ(Q) calculates the conjugate of the quaternion Q.
			//     Works on "vectors" of quaterions as well.  Will return the same shape
			//     vector as input.  If input is a vector of four quaternions, QCONJ will
			//     determine whether the quaternions are row or column vectors according
			//     to ISQ.
			
			// See also ISQ.

			// Release: $Name: quaternions-1_3 $
			// $Revision: 1.16 $
			// $Date: 2009-07-26 20:05:12 $
			 
			// Copyright (c) 2001-2009, Jay A. St. Pierre.  All rights reserved.


			int r,c;
			int qtype = isq(qinorg);
			if ( qtype==0 ) {
			    System.err.println("conj invalid input: must be a quaternion or a vector of quaternions");
			    return null;
			}
			else if ( qtype==3 ) {
			    System.out.println("Warning: qconj:indeterminateShape");
			    System.out.println("Component quaternion shape indeterminate, assuming row vectors");
			}

			// Make sure component quaternions are row vectors
			double qin[][];
			if( qtype == 1 ) {
			  qin = new double[qinorg[0].length][4];
			  for (r = 0; r < 4; r++) {
				  for (c = 0; c < qinorg[0].length; c++) {
					  qin[c][r] = qinorg[r][c];
				  }
			  }
			}
			else {
				qin = qinorg;
			}
            
			double qout[][] = new double[qin.length][4];
			for (r = 0; r < qout.length; r++) {
			    qout[r][0] = -qin[r][0];
			    qout[r][1] = -qin[r][1];
			    qout[r][2] = -qin[r][2];
			    qout[r][3] = qin[r][3];
			}
			
			if (qtype != 1) {
				return qout;
			}

			// Make sure output is same shape as input
			double qout_trans[][] = new double[4][qout.length];
			for (r = 0; r < qout.length; r++) {
				for (c = 0; c < 4; c++) {
					qout_trans[c][r] = qout[r][c];
				}
			}
			return qout_trans;
	}

	
}