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
	
	private int isnormq(double q[][]) {
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



	
}