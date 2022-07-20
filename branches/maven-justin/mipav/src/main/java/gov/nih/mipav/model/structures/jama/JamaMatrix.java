/**
 * 
 */
package gov.nih.mipav.model.structures.jama;

import Jama.Matrix;

/**
 * Wrapper around Jama package, @see src/Jama/
 * Provides access to features unique to Jama lib, 
 * otherwise use WildMagic Matrix3f, Matrix4f and GMatrix classes.
 * 
 * @author helser
 *
 */
public class JamaMatrix  {
	
	private Jama.Matrix matrix;
	
	/** Initial value set to 0.0 for all elements
	 * @param m number of columns
	 * @param n number of rows
	 */
	public JamaMatrix(int m, int n) {
		matrix = new Matrix(m, n, 0.0);
	}

	/**
	 * @param m number of columns
	 * @param n number of rows
	 * @param s initial value for all elements.
	 */
	public JamaMatrix(int m, int n, double s) {
		matrix = new Matrix(m, n, s);
	}

	/**
	 * @param A rectangular 2D matrix with data, stores a reference.
	 */
	public JamaMatrix(double[][] A) {
		matrix = new Matrix(A);
		// TODO Auto-generated constructor stub
	}
	
	/** Convert results from Jama lib
	 * @param B internal data 
	 */
	private JamaMatrix(Matrix B) {
		matrix = B;
	}
	
	public JamaMatrix solve(JamaMatrix B) {
		Matrix ret = matrix.solve(B.matrix);
		return new JamaMatrix(ret);
	}
	
	/** Get a single element.
	   @param i    Row index.
	   @param j    Column index.
	   @return     A(i,j)
	   @exception  ArrayIndexOutOfBoundsException
	*/
	public double get (int i, int j) {
		return matrix.get(i,j);
	}

	/** Set a single element.
	   @param i    Row index.
	   @param j    Column index.
	   @param s    A(i,j).
	   @exception  ArrayIndexOutOfBoundsException
	 */

	public void set (int i, int j, double s) {
		matrix.set(i, j, s);
	}

	/** Copy the internal two-dimensional array.
	   @return     Two-dimensional array copy of matrix elements.
	 */

	public double[][] getArrayCopy () {
		return matrix.getArrayCopy();
	}

}
