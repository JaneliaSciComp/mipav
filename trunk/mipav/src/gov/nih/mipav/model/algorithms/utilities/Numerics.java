package gov.nih.mipav.model.algorithms.utilities;

import java.io.*;
import java.util.*;

/**
 *
 *  This class computes various basic numerical functions.
 *	<p> 
 *	Includes min, max, gaussian random numbers, special functions, 
 *	mathematical constants. This duplicates some of Java's Math functions
 *	where these have limited type support (e.g. double, but not float) or
 *	perform multiple casting operations (slow).
 *
 *	@version    May 17, 2005
 *	@author     Pierre-Louis Bazin
 */

public class Numerics {
	
	/** mathematical constants */
	public static final float ZERO = 1e-30f;
	/** mathematical constants */
	public static final float INF = 1e+30f;
	/** mathematical constants */
	public static final float PI = 3.1416f;
	
	/**
	 *	Gaussian random numbers (from the Box-Muller method, as in NRC)
	 *  mean 0, variance 1
	 */
    public static final double randomGaussian() {
		double v1,v2,rsq;
        do {
			v1 = 2.0*Math.random()-1.0;
			v2 = 2.0*Math.random()-1.0;
			rsq = v1*v1+v2*v2;
		} while ( (rsq<=0) || (rsq>=1) );
		return v1*Math.sqrt( -2.0*Math.log(rsq)/rsq);
    }
	
	/**
	 *	Student T 3rd order random numbers
	 *  mean 0, variance given
	 *  note: the formula should be the inverse of the probability integral (wrong here!)
	 */
    public static final double randomT3(double sigma) {
		double v1,v2,rsq;
        do {
			v1 = 2.0*Math.random()-1.0;
			v2 = 2.0*Math.random()-1.0;
			rsq = v1*v1+v2*v2;
		} while ( (rsq<=0) || (rsq>=1) );
		if (v1>0) return Math.sqrt(sigma*sigma + Math.sqrt(2.0*sigma*sigma*sigma/Math.PI/rsq));
		else return -Math.sqrt(sigma*sigma + Math.sqrt(2.0*sigma*sigma*sigma/Math.PI/rsq));
    }
	
	/**
	 *  Close-form 3D matrix determinant
	 */
	public static final float determinant3D(float m[][]) {
		float det;
		det = m[0][0]*m[1][1]*m[2][2] + m[1][0]*m[2][1]*m[0][2] + m[2][0]*m[0][1]*m[1][2]
			- m[2][0]*m[1][1]*m[0][2] - m[2][1]*m[1][2]*m[0][0] - m[2][2]*m[1][0]*m[0][1];

		return det;
	}
	
	/**
	 *  Close-form 3D matrix inversion
	 */
	public static final void invert3Dmatrix(float m[][]) {
		float[] d = new float[9];
		float det;
		det = m[0][0]*m[1][1]*m[2][2] + m[1][0]*m[2][1]*m[0][2] + m[2][0]*m[0][1]*m[1][2]
			- m[2][0]*m[1][1]*m[0][2] - m[2][1]*m[1][2]*m[0][0] - m[2][2]*m[1][0]*m[0][1];
		det = 1.0f/det;

		d[0] = (m[1][1]*m[2][2]-m[1][2]*m[2][1])*det;
		d[1] = (m[2][1]*m[0][2]-m[2][2]*m[0][1])*det;
		d[2] = (m[0][1]*m[1][2]-m[0][2]*m[1][1])*det;
		d[3] = (m[1][2]*m[2][0]-m[1][0]*m[2][2])*det;
		d[4] = (m[0][0]*m[2][2]-m[0][2]*m[2][0])*det;
		d[5] = (m[1][0]*m[0][2]-m[1][2]*m[0][0])*det;
		d[6] = (m[1][0]*m[2][1]-m[1][1]*m[2][0])*det;
		d[7] = (m[0][1]*m[2][0]-m[0][0]*m[2][1])*det;
		d[8] = (m[0][0]*m[1][1]-m[0][1]*m[1][0])*det;

		m[0][0] = d[0];
		m[1][0] = d[1];
		m[2][0] = d[2];
		m[0][1] = d[3];
		m[1][1] = d[4];
		m[2][1] = d[5];
		m[0][2] = d[6];
		m[1][2] = d[7];
		m[2][2] = d[8];

		return;
	}
	
	/**
	 * This method compute the inverse of a matrix 
	 * @param m : the given matrix which should be a square array of floats 
	 * @param inverse : is the result of inversion
	 * @param nColumn : number of coulmn of squared aaray
	 * @return : determinent of the matrix
	 */
	
	public static final float invertMatrix(float m[][], float[][] inverse, int nColumn){
		
		float det = 0.0f;
		//float[][] inverse = new float[nColumn][nColumn];
		
		if (nColumn == 1) {
			det = m[0][0];
			if (det != 0.0f)
				inverse[0][0] = 1.0f/m[0][0];
			else
				System.out.println("Matrix is singular");
			
			return det;
			//return inverse;
		}else if (nColumn == 2){
			det = m[0][0]*m[1][1] - m[0][1]*m[1][0];
			if (det != 0.0f){
				inverse [0][0] =m[1][1]/det;
				inverse [0][1] = -m[1][0]/det;
				inverse [1][0] = -m[0][1]/det;
				inverse [1][1] = m[1][1]/det;
				
			} else
				System.out.println("Matrix is singular");
			return det;
			//return inverse;
		}else if (nColumn == 3){
			det = m[0][0]*m[1][1]*m[2][2] + m[1][0]*m[2][1]*m[0][2] + m[2][0]*m[0][1]*m[1][2]
			    - m[2][0]*m[1][1]*m[0][2] - m[2][1]*m[1][2]*m[0][0] - m[2][2]*m[1][0]*m[0][1];
			if (det != 0.0f) {
				inverse[0][0] = (m[1][1]*m[2][2]-m[1][2]*m[2][1])/det;
				inverse[0][1]= (m[2][1]*m[0][2]-m[2][2]*m[0][1])/det;
				inverse[0][2] = (m[0][1]*m[1][2]-m[0][2]*m[1][1])/det;
				inverse[1][0] = (m[1][2]*m[2][0]-m[1][0]*m[2][2])/det;
				inverse[1][1] = (m[0][0]*m[2][2]-m[0][2]*m[2][0])/det;
				inverse[1][2] = (m[1][0]*m[0][2]-m[1][2]*m[0][0])/det;
				inverse[2][0] = (m[1][0]*m[2][1]-m[1][1]*m[2][0])/det;
				inverse[2][1]= (m[0][1]*m[2][0]-m[0][0]*m[2][1])/det;
				inverse[2][2] = (m[0][0]*m[1][1]-m[0][1]*m[1][0])/det;
			}else
				System.out.println("Matrix is singular");
			return det;
			//return inverse;
		}else{
			int[] pivlst = new int[2*nColumn+1];
			boolean[] pivchk = new boolean[nColumn];
			int leRow,leCol;
			leRow = leCol =0;
			float piv,t,leval,temp;
			int i,j,k; //loop counters
			int zeroMat = -1;
			det = 1.0f;
			for (i =0; i<nColumn ;i++ ){
				pivchk[i] = false;
				for (j =0; j<nColumn; j++)
					inverse[i][j] = m[i][j];
			}
			
			for (i=0; i<nColumn; i++){
				leval = 0.0f;
				for ( j = 0; j < nColumn; j++ ) 
		            if ( ! (pivchk[j]) ) 
		               for ( k = 0; k < nColumn; k++ ) 
		                  if ( ! (pivchk[k]) ) {
							 temp = Math.abs(inverse[j][k]);
		                     if ( temp > leval ) {
		                        leRow = j;
		                        leCol = k;
		                        leval = temp;
								zeroMat=0;
		                     }
		                  }
				System.out.println("leCol= "+ leCol + "leRow= "+leRow);
				if(zeroMat != 0){
					System.out.println("Cannot invert the zero matrix\n");
					det=0.0f;
					inverse = null;;
				 }
		         pivchk[leCol] = true;
		         pivlst[i*2] = leRow;
		         pivlst[i*2+1] = leCol;
		         if ( leRow != leCol ) {
		            det = -det;
		            for ( int l = 0; l < nColumn; l++ ) {
		               float swap = inverse[leRow][l];
		               inverse[leRow][l] = inverse[leCol][l];
		               inverse[leCol][l] = swap;
		            }
		         }
		         piv = inverse[leCol][leCol];
		         det = det * piv;
		         System.out.println(det);
		         if ( det > 1.0e+30 ) {
		            det = 1.0f;
		         }
		         inverse[leCol][leCol] = 1.0f;
		         for ( int l = 0; l < nColumn; l++ ) {
		            inverse[leCol][l] = inverse[leCol][l] / piv;
		            System.out.println("inverse["+leCol+"]["+l+"]= "+ inverse[leCol][l]);
		         }
		         for ( int l1 = 0; l1 < nColumn; l1++ ) {
		            if ( l1 != leCol ) {
		               t = inverse[l1][leCol];
		               inverse[l1][leCol] = 0.0f;
		               for ( int l = 0; l < nColumn; l++ ) {
		                  inverse[l1][l] = inverse[l1][l] - inverse[leCol][l] * t;
		               }
		            }
		         }
		      }
		      
			  for ( i = 0; i < nColumn; i++ ) {
		         int l = nColumn - i - 1;
		         if ( pivlst[l*2] != pivlst[l*2+1] ) {
		            leRow = pivlst[l*2];
		            leCol = pivlst[l*2+1];
		            for ( k = 0; k < nColumn; k++ ) {
		            	float swap = inverse[k][leRow];
			            inverse[k][leRow] = inverse[k][leCol];
			            inverse[k][leCol] = swap;
		            }
		         }
		      }
		      
		   }
		return det;
		
	}
	/** 
	 *	Close-form 3D matrix eigenvalues.
	 *	<p>
	 *	This may not work well all the time with arbitrary matrices.
	 */
	public static final float[] eigenvalues3D(float m[][]) {
		float[] values = new float[3];
		
		// polynomial coefficients X^3 + a2 X^2 + a1 X + a0 = 0
		
		double a2 = - (m[0][0]+m[1][1]+m[2][2]);
		
		double a1 =   (m[0][0]*m[1][1]-m[0][1]*m[1][0])
					+(m[1][1]*m[2][2]-m[1][2]*m[2][1])
					+(m[2][2]*m[0][0]-m[2][0]*m[0][2]);
		 
		double a0 = -( m[0][0]*m[1][1]*m[2][2]
					  +m[0][1]*m[1][2]*m[2][0]
					  +m[1][0]*m[0][2]*m[2][1]
					  -m[0][1]*m[1][0]*m[2][2]
					  -m[1][2]*m[2][1]*m[0][0]
					  -m[2][0]*m[0][2]*m[1][1] );
	
		double Q = ( 3.0*a1-a2*a2 )/9.0;
		double R = ( 9.0*a2*a1 - 27.0*a0 - 2.0*a2*a2*a2 )/54.0;
		
		// discriminant
		double D = Q*Q*Q + R*R;
		
		if ( D > 0 ) { 
			// positive discriminant: imaginary solutions
			//System.out.print("positive discriminant");
			values[0] = 0.0f;
			values[1] = 0.0f;
			values[2] = 0.0f;
		} else if ( D < 0 ) {
			// usual case
			double theta = Math.acos( R / Math.sqrt( -Q*Q*Q ) );
			double sQ = 2.0*Math.sqrt(-Q);
			
			values[0] = (float)(sQ*Math.cos(theta/3.0) - a2/3.0 );
			values[1] = (float)(sQ*Math.cos((theta+2.0*Math.PI)/3.0) - a2/3.0 );
			values[2] = (float)(sQ*Math.cos((theta+4.0*Math.PI)/3.0) - a2/3.0 );
			
			// re-ordering by decreasing size : |values[0]| >= |values[1]| >= |values[2]|
			if (Numerics.abs(values[0])<Numerics.abs(values[1])) { float tmp = values[0]; values[0] = values[1]; values[1] = tmp; }
			if (Numerics.abs(values[0])<Numerics.abs(values[2])) { float tmp = values[0]; values[0] = values[2]; values[2] = tmp; }
			if (Numerics.abs(values[1])<Numerics.abs(values[2])) { float tmp = values[1]; values[1] = values[2]; values[2] = tmp; }					
		} else {
			// multiple roots
			double S = Math.cbrt(R);
			
			values[0] = (float)(-a2/3.0 + 2.0*S);
			values[1] = (float)(-a2/3.0 - S);
			values[2] = values[1];
			
			// re-ordering by decreasing size : |values[0]| >= |values[1]| >= |values[2]|
			if (Numerics.abs(values[0])<Numerics.abs(values[1])) { float tmp = values[0]; values[0] = values[2]; values[2] = tmp; }
		}
		return values;
	}
	
	/** 
	 *	Simple geometric technique to get eigenvectors of 3D matrices.
	 *	<p>
	 *	This may not work well all the time: sometimes you have a 2 or 3D sub-space with one eigenvalue.
	 *	We assume here that the highest eigenvalues give the most reliable directions, and lesser eigenvectors
	 *	are projected on orthogonal subspaces.
	 *	We also assume ordered, positive eigenvalues. 
	 */
	public static final float[][] eigenvectors3D(float m[][], float values[]) {
		float[][] vector = new float[3][3];
		
		double[] ArIx = new double[3];
		double[] ArIy = new double[3];
		double[] ArIz = new double[3];
		
		if (values[0]==0) {
			vector[0][0] = 1.0f;
			vector[1][0] = 0.0f;			
			vector[2][0] = 0.0f;
			vector[0][1] = 0.0f;
			vector[1][1] = 1.0f;			
			vector[2][1] = 0.0f;
			vector[0][2] = 0.0f;
			vector[1][2] = 0.0f;			
			vector[2][2] = 1.0f;
			return vector;
		}			
		
		for (int i=0;i<3;i++) {
		
			// first eigenvalue
			ArIx[0] = m[0][0]-values[i];
			ArIx[1] = m[1][0];
			ArIx[2] = m[2][0];
			double normx2 = ArIx[0]*ArIx[0]+ArIx[1]*ArIx[1]+ArIx[2]*ArIx[2];
			
			ArIy[0] = m[0][1];
			ArIy[1] = m[1][1]-values[i];
			ArIy[2] = m[2][1];
			double normy2 = ArIy[0]*ArIy[0]+ArIy[1]*ArIy[1]+ArIy[2]*ArIy[2];
			
			ArIz[0] = m[0][2];
			ArIz[1] = m[1][2];
			ArIz[2] = m[2][2]-values[i];
			double normz2 = ArIz[0]*ArIz[0]+ArIz[1]*ArIz[1]+ArIz[2]*ArIz[2];
			
			if ( (normx2<normy2) && (normx2<normz2) ) {
				vector[0][i] = (float)(ArIy[1]*ArIz[2]-ArIy[2]*ArIz[1]);
				vector[1][i] = (float)(ArIy[2]*ArIz[0]-ArIy[0]*ArIz[2]);			
				vector[2][i] = (float)(ArIy[0]*ArIz[1]-ArIy[1]*ArIz[0]);
			} else if ( (normy2<normz2) && (normy2<normx2) ) {
				vector[0][i] = (float)(ArIz[1]*ArIx[2]-ArIz[2]*ArIx[1]);
				vector[1][i] = (float)(ArIz[2]*ArIx[0]-ArIz[0]*ArIx[2]);			
				vector[2][i] = (float)(ArIz[0]*ArIx[1]-ArIz[1]*ArIx[0]);
			} else {
				vector[0][i] = (float)(ArIx[1]*ArIy[2]-ArIx[2]*ArIy[1]);
				vector[1][i] = (float)(ArIx[2]*ArIy[0]-ArIx[0]*ArIy[2]);			
				vector[2][i] = (float)(ArIx[0]*ArIy[1]-ArIx[1]*ArIy[0]);
			}
			// orthogonalize
			if (i==1) {
				// v0 is already normalized
				float v0v1 = vector[0][0]*vector[0][1]+vector[1][0]*vector[1][1]+vector[2][0]*vector[2][1];
				vector[0][1] = vector[0][1] - v0v1*vector[0][0];
				vector[1][1] = vector[1][1] - v0v1*vector[1][0];	
				vector[2][1] = vector[2][1] - v0v1*vector[2][0];
			} else if (i==2) {
				// v0, v1 normalized
				// (note: it would be tempting to take the cross product, but it may not be well defined)
				float v0v2 = vector[0][0]*vector[0][2]+vector[1][0]*vector[1][2]+vector[2][0]*vector[2][2];
				float v1v2 = vector[0][1]*vector[0][2]+vector[1][1]*vector[1][2]+vector[2][1]*vector[2][2];
				vector[0][2] = vector[0][2] - v0v2*vector[0][0] - v1v2*vector[0][1];
				vector[1][2] = vector[1][2] - v0v2*vector[1][0] - v1v2*vector[1][1];	
				vector[2][2] = vector[2][2] - v0v2*vector[2][0] - v1v2*vector[2][1];
			}
			// normalize
			double norm = Math.sqrt(vector[0][i]*vector[0][i]+vector[1][i]*vector[1][i]+vector[2][i]*vector[2][i]);
			vector[0][i] = (float)(vector[0][i]/norm);
			vector[1][i] = (float)(vector[1][i]/norm);	
			vector[2][i] = (float)(vector[2][i]/norm);			
		}

		/* debug: comment out for faster code 
		// check for orthogonality, same value ?
		float prodxy = vector[0][0]*vector[0][1] + vector[1][0]*vector[1][1] + vector[2][0]*vector[2][1];
		float prodyz = vector[0][1]*vector[0][2] + vector[1][1]*vector[1][2] + vector[2][1]*vector[2][2];
		float prodzx = vector[0][2]*vector[0][0] + vector[1][2]*vector[1][0] + vector[2][2]*vector[2][0];
			
		if ( (prodxy > 0.1) || (prodxy < -0.1) 
			|| (prodyz > 0.1) || (prodyz < -0.1) 
			|| (prodzx > 0.1) || (prodzx < -0.1) ) {
			System.out.print("pb:"+prodxy+"|"+prodyz+"|"+prodzx+"|");
			System.out.print("l:"+values[0]+"|"+values[1]+"|"+values[2]+"|");
		}
		*/
		return vector;
	}
	
	/**
	 *	rounding function (faster than regular Java)
	 */
    public static final int round(float num) {
		if (num>0) return (int)(num+0.5f);
		else return (int)(num-0.5f);
    }
    public static final int floor(float num) {		
		if (num>=0) return (int)(num);
		else return (int)(num)-1;
	}
    public static final int ceil(float num) {		
		if (num>0) return (int)(num)+1;
		else return (int)(num);
    }
	
    public static final int round(double num) {		
		if (num>0) return (int)(num+0.5f);
		else return (int)(num-0.5f);
    }
    public static final int floor(double num) {		
		if (num>=0) return (int)(num);
		else return (int)(num)-1;
    }
    public static final int ceil(double num) {		
		if (num>0) return (int)(num)+1;
		else return (int)(num);
    }
	/* min, max */
	public static final float min( float a, float b) {
		if (a < b) return a;
		else return b;
	}
	public static final int min( int a, int b) {
		if (a < b) return a;
		else return b;
	}
	public static final long min( long a, long b) {
		if (a < b) return a;
		else return b;
	}
	public static final float min( float a, float b, float c) {
		return min(a,min(b,c));
	}
	
	public static final float max( float a, float b) {
		if (a > b) return a;
		else return b;
	}
	public static final double max( double a, double b) {
		if (a > b) return a;
		else return b;
	}
	public static final int max( int a, int b) {
		if (a > b) return a;
		else return b;
	}
	public static final int max( int a, int b, int c) {
		return max(a,max(b,c));
	}
	public static final float max( float a, float b, float c) {
		return max(a,max(b,c));
	}
	
	public static final int bounded( int x, int a, int b) {
		return max(a,min(x,b));
	}
	
	public static final float bounded( float x, float a, float b) {
		return max(a,min(x,b));
	}
	
	/* absolute value, sign */
	public static final float abs( float a) {
		if (a > 0) return a;
		else return -a;
	}
	public static final double abs( double a) {
		if (a > 0) return a;
		else return -a;
	}
	public static final int abs( int a) {
		if (a > 0) return a;
		else return -a;
	}
	public static final int sign( float a) {
		if (a > 0) return 1;
		else if (a < 0) return -1;
		else return 0;
	}
	
	/** find the index of the largest values in val (>=0) */
	public static final byte bestIndex(float[] val) {
		byte nmax=0;
		for (byte m=1;m<val.length;m++) if (val[m]>val[nmax]) {
			nmax = m;
		}
		return nmax;
	}	
			
	/** find the indices of the num largest values in val (>=0) */
	public static final byte[] bestIndex(float[] val, int num) {
		byte[] id = new byte[num];
		for (int n=0;n<num;n++) {
			byte nmax=0;
			for (byte m=1;m<val.length;m++) if (val[m]>val[nmax]) {
				nmax = m;
			}
			id[n] = nmax;
			val[nmax] *= -1;
		}
		// rewrite the values
		for (int n=0;n<num;n++) {
			val[id[n]] *= -1;
		}
		return id;
	}	
			
	/** find the indices of the num largest values in val (>=0) */
	public static final void bestIndex(byte[] id, float[] bestval, float[] val, int num) {
		for (int n=0;n<num;n++) {
			byte nmax=0;
			for (byte m=1;m<val.length;m++) if (val[m]>val[nmax]) {
				nmax = m;
			}
			id[n] = nmax;
			bestval[n] = val[nmax];
			val[nmax] = -INF;
		}
		return;
	}	
			
	/** find the indices of the num largest values in val (>=0) */
	public static final void bestIndex(byte[] id, float[] bestval, float[] val, int num, byte first) {
		for (int n=0;n<num;n++) {
			byte nmax=first;
			for (byte m=0;m<val.length;m++) if (val[m]>val[nmax]) {
				nmax = m;
			}
			id[n] = nmax;
			bestval[n] = val[nmax];
			val[nmax] = -INF;
		}
		return;
	}	
			
	/** find the indices of the num largest values in val (>=0) */
	public static final byte bin(boolean val) {
		if (val) return 1;
		else return 0;
	}

			
}
