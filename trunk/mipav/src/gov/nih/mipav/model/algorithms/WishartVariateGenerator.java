package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;
import java.util.Random;

/**
    Ported from original FORTRAN code Algorithm AS 53: Wishart Variate Generator
    Authors: W. B. Smith and R. R. Hocking
    Source: Journal of the Royal Statistical Society Series C (applied Statistics),
    1972, Vol. 21, No. 3, pp. 341-345
    Published by: Wiley for the Royal Statistical Society
    Stable URL: https://www.jstor.org/stable/2346290
    */

public class WishartVariateGenerator {
	
	private double sigma[][];
	// N = n if mean known
	// N = n-1 if mean unknown
	private int N;
	// SA is np * np symmetric matrix
	private double SA[][];
	// SB is np * np
	private double SB[][];
	RandomNumberGen random = new RandomNumberGen();
	
	public WishartVariateGenerator(double sigma[][], int N, double SA[][], double SB[][]) {
	    this.sigma = sigma;	
	    this.N = N;
	    this.SA = SA;
	    this.SB = SB;
	}
	
	public void run() {
		// dpotrf computes the Cholesky factorization of a real symmetric
	    // positive definite matrix A
		// D is an upper triangular array, such that sigma = DPRIME * D
		// On output from dpotrf the leading n-by-n upper triangular part of D
		// contains the upper triangular part of the matrix D.
		int i,j,index,k,row,col,df,diag,rn,nr,ip,nq,ii;
		int rowIndexip[] = new int[1];
		int colIndexip[] = new int[1];
		int rowIndexnq[] = new int[1];
		int colIndexnq[] = new int[1];
		double c;
		double u1[] = new double[1];
		double u2[] = new double[1];
		int info[] = new int[1];
		int np = sigma[0].length;
		int nnp = np * (np + 1)/2;
		double D[] = new double[nnp];
		GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
		ge.dpotrf('U', sigma[0].length, sigma, sigma.length, info);
		if (info[0] < 0) {
			System.err.println("In dpotrf argument " + (-info[0]) + " had an illegal value");
			return;
		}
		else if (info[0] > 0) {
			System.err.println("In dpotrf the leading minor of order " + info[0] + " is not positive definite,"); 
			System.err.println("and the factorization could not be completed.");
			return;
		}
		
		// FORTRAN is column ordered
		for (j = 0, index = 0; j < np; j++) {
			for (i = 0; i <= j; i++) {
			    D[index++] = sigma[i][j];	
			}
		}
		k = 1;
		row = 0;
		col = 0;
		while (k <= nnp) {
			rand(u1,u2);
			
			// Load SB with independent normal(0,1) variates
			
			SB[row][col] = u1[0];
			k = k + 1;
			if (row < col) {
				row++;
			}
			else {
				row = 0;
				col++;
			}
			if (k > nnp) {
				break;
			}
			SB[row][col] = u2[0];
			k = k + 1;
			if (row < col) {
				row++;
			}
			else {
				row = 0;
				col++;
			}
		} // while (k <= nnp)
		
		// Load diagonal elements with square root of chi-square variates
		
		diag = -1;
		for (i = 1; i <= np; i++) {
		  df = N - i + 1;
		  diag++;
		  u1[0] = 2.0 / (9.0 * df);
		  u2[0] = 1.0 - u1[0];
		  u1[0] = Math.sqrt(u1[0]);
		  
		  // Wilson-Hilferty formula for approxmating chi-square variates
		  // from normal variates
		  SB[diag][diag] = Math.sqrt(df * Math.pow((u2[0] + SB[diag][diag] * u1[0]),3.0));
		} // for (i = 1; i <= np; i++)
		
		rn = N;
		nr = 1;
		for (i = 1; i <= np; i++) {
		    nr = nr + i - 1;
		    for (j = i; j <= np; j++) {
		        ip = nr;
		        nq = (j * j - j)/2 + i - 1;
		        c = 0.0;
		        for (k = i; k <= j; k++) {
		            ip = ip + k - 1;
		            nq = nq + 1;
		            rcIndex(ip, np, rowIndexip, colIndexip);
		            c = c + SB[rowIndexip[0]][colIndexip[0]] * D[nq-1];
		        } // for (k = i; k <= j; k++)
		        rcIndex(ip, np, rowIndexip, colIndexip);
		        SA[rowIndexip[0]][colIndexip[0]] = c;
		    } // for (j = i; j <= np; j++)
		} // for (i = 1; i <= np; i++)
		
		for (i = 1; i <= np; i++) {
		    ii = np - i + 1;
		    nq = nnp - np;
		    for (j = 1; j <= i; j++) {
		        ip = (ii * ii - ii)/2;
		        c = 0.0;
		        for (k = i; k <= np; k++) {
		            ip = ip + 1;
		            nq = nq + 1;
		            rcIndex(ip, np, rowIndexip, colIndexip);
		            rcIndex(nq, np, rowIndexnq, colIndexnq);
		            c = c + SA[rowIndexip[0]][colIndexip[0]] * SA[rowIndexnq[0]][colIndexnq[0]];
		        } // for (k = i; k <= np; k++)
		        rcIndex(nq, np, rowIndexnq, colIndexnq);
		        SA[rowIndexnq[0]][colIndexnq[0]] = c/rn;
		        nq = nq - 2 * np + i + j - 1;
		    } // for (j = 1; j <= i; j++)
		} // for (i = 1; i <= np; i++)
		
		// Fill in lower triangle of symmetric SA
		for (j = 0; j < np; j++) {
			for (i = j+1; j < np; j++) {
				SA[i][j] = SA[j][i];
			}
		}
		return;
	}
	
	private void rcIndex(int index, int np, int rowIndex[], int colIndex[]) {
		int row, col;
		int rcIndex = 1;
		for (col = 0; col < np; col++) {
			for (row = 0; row <= col; row++, rcIndex++) {
			    if (rcIndex == index) {
			    	rowIndex[0] = row;
			    	colIndex[0] = col;
			    	return;
			    }
			}
		}
		
	}
	
	private void rand(double u1[], double u2[]) {
		// Algorithm AS 53.1 Appl. Statist. (1972), Vol. No 21, No.3
		
		// Sets u1[0] and u2[0] to two independent standardized random normal 
		// deivates.  This is a version of the method given in Knuth(1969).
		
		// x and y must come from a random rectangularly distrubution between
		// the limits 0.0 and 1.0 exclusive.
		double x = 0.0;
	    double y = 0.0;
		double s = Double.MAX_VALUE;
		while ( s > 1.0) {
		    x = random.genUniformRandomNum(0.0, 1.0);
		    y = random.genUniformRandomNum(0.0, 1.0);
		    x = 2.0 * x - 1.0;
		    y = 2.0 * y - 1.0;
		    s = x*x + y*y;
		} // while ( s > 1.0)
		s = Math.sqrt(-2.0 * Math.log(s) / s);
		u1[0] = x*s;
		u2[0] = y*s;
		return;
	}
}