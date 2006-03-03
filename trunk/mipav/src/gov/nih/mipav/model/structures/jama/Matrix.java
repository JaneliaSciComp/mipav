package gov.nih.mipav.model.structures.jama;


import java.text.NumberFormat;
import java.text.DecimalFormat;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.StreamTokenizer;
import gov.nih.mipav.model.structures.jama.*;
import gov.nih.mipav.model.structures.jama.util.*;


/**
 * Jama = Java Matrix class.
 * <P>
 * The Java Matrix Class provides the fundamental operations of numerical
 * linear algebra.  Various constructors create Matrices from two dimensional
 * arrays of double precision floating point numbers.  Various "gets" and
 * "sets" provide access to submatrices and matrix elements.  Several methods
 * implement basic matrix arithmetic, including matrix addition and
 * multiplication, matrix norms, and element-by-element array operations.
 * Methods for reading and printing matrices are also included.  All the
 * operations in this version of the Matrix Class involve real matrices.
 * Complex matrices may be handled in a future version.
 * <P>
 * Five fundamental matrix decompositions, which consist of pairs or triples
 * of matrices, permutation vectors, and the like, produce results in five
 * decomposition classes.  These decompositions are accessed by the Matrix
 * class to compute solutions of simultaneous linear equations, determinants,
 * inverses and other matrix functions.  The five decompositions are:
 * <P><UL>
 * <LI>Cholesky Decomposition of symmetric, positive definite matrices.
 * <LI>LU Decomposition of rectangular matrices.
 * <LI>QR Decomposition of rectangular matrices.
 * <LI>Singular Value Decomposition of rectangular matrices.
 * <LI>Eigenvalue Decomposition of both symmetric and nonsymmetric square matrices.
 * </UL>
 * <DL>
 * <DT><B>Example of use:</B></DT>
 * <P>
 * <DD>Solve a linear system A x = b and compute the residual norm, ||b - A x||.
 * <P><PRE>
 * double[][] vals = {{1.,2.,3},{4.,5.,6.},{7.,8.,10.}};
 * Matrix A = new Matrix(vals);
 * Matrix b = Matrix.random(3,1);
 * Matrix x = A.solve(b);
 * Matrix r = A.times(x).minus(b);
 * double rnorm = r.normInf();
 * </PRE></DD>
 * </DL>
 *
 * @author The MathWorks, Inc. and the National Institute of Standards and Technology.
 * @version 5 August 1998
 */
public class Matrix implements Cloneable, java.io.Serializable {

    /* ------------------------
     * Class variables
     * ------------------------ */

    /**
     * Array for internal storage of elements.
     * @serial internal array storage.
     */
    protected double[][] matrix;

    /**
     * Row dimension.
     * @serial column dimension.
     */
    protected int mRow;

    /**
     * Column dimension.
     * @serial column dimension.
     */
    protected int nCol;

    /* ------------------------
     * Constructors
     * ------------------------ */

    /**
     * Construct an m-by-n matrix of zeros.
     * @param m    Number of rows.
     * @param n    Number of colums.
     */
    public Matrix( int m, int n ) {
        this.mRow = m;
        this.nCol = n;
        matrix = new double[mRow][nCol];
    }

    /**
     * Reallocates memory for matrix without constructing a new object a new object.
     * @param r    Number of rows.
     * @param c    Number of colums.
     */
    protected void reConstruct( int r, int c ) {
        this.mRow = r;
        this.nCol = c;
        matrix = new double[mRow][nCol];
    }

    /**
     * Construct an m-by-n constant matrix.
     * @param m    Number of rows.
     * @param n    Number of colums.
     * @param s    Fill the matrix with this scalar value.
     */
    public Matrix( int m, int n, double s ) {
        this.mRow = m;
        this.nCol = n;
        matrix = new double[mRow][nCol];
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                matrix[i][j] = s;
            }
        }
    }

    /**
     * Construct a matrix from a 2-D array.
     * @param matrix  Two-dimensional array of doubles.
     * @exception     IllegalArgumentException All rows must have the same length
     * @see           #constructWithCopy
     */
    public Matrix( double[][] matrix ) {
        mRow = matrix.length;
        nCol = matrix[0].length;
        for ( int i = 0; i < mRow; i++ ) {
            if ( matrix[i].length != nCol ) {
                throw new IllegalArgumentException( "All rows must have the same length." );
            }
        }
        this.matrix = matrix;
    }

    /**
     * Construct a matrix quickly without checking arguments.
     * @param matrix  Two-dimensional array of doubles.
     * @param m       Number of rows.
     * @param n       Number of colums.
     */
    public Matrix( double[][] matrix, int m, int n ) {
        this.matrix = matrix;
        this.mRow = m;
        this.nCol = n;
    }

    /**
     * Construct a matrix from a one-dimensional packed array.
     * @param vals  One-dimensional array of doubles, packed by columns (ala Fortran).
     * @param m     Number of rows.
     * @exception   IllegalArgumentException Array length must be a multiple of m.
     */
    public Matrix( double[] vals, int m ) {
        this.mRow = m;
        nCol = ( mRow != 0 ? vals.length / mRow : 0 );
        if ( mRow * nCol != vals.length ) {
            throw new IllegalArgumentException( "Array length must be a multiple of mRow." );
        }
        matrix = new double[mRow][nCol];
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                matrix[i][j] = vals[i + j * mRow];
            }
        }
    }

    /* ------------------------
     * Public Methods
     * ------------------------ */

    /**
     * Construct a matrix from a copy of a 2-D array.
     * @param matrix  Two-dimensional array of doubles.
     * @return        The newly constructed matrix.
     * @exception     IllegalArgumentException All rows must have the same length.
     */
    public static Matrix constructWithCopy( double[][] matrix ) {
        int m = matrix.length;
        int n = matrix[0].length;
        Matrix X = new Matrix( m, n );
        double[][] C = X.getArray();
        for ( int i = 0; i < m; i++ ) {
            if ( matrix[i].length != n ) {
                throw new IllegalArgumentException
                        ( "All rows must have the same length." );
            }
            for ( int j = 0; j < n; j++ ) {
                C[i][j] = matrix[i][j];
            }
        }
        return X;
    }

    /**
     * Cleans memory.
     * @throws Throwable  if there is a problem cleaning up memory.
     */
    public void finalize() throws Throwable {
        matrix = null;
        super.finalize();
    }

    /**
     * Make a deep copy of a matrix.
     * @return  a new copy of the matrix.
     */
    public Matrix copy() {
        Matrix X = new Matrix( mRow, nCol );
        double[][] C = X.getArray();
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                C[i][j] = matrix[i][j];
            }
        }
        return X;
    }

    /**
     * Clone the Matrix object.
     * @return  a new copy of the matrix.
     */
    public Object clone() {
        return this.copy();
    }

    /**
     * Access the internal two-dimensional array.
     * @return     Pointer to the two-dimensional array of matrix elements.
     */
    public double[][] getArray() {
        return matrix;
    }

    /**
     * Copy the internal two-dimensional array.
     * @return     Two-dimensional array copy of matrix elements.
     */
    public double[][] getArrayCopy() {
        double[][] C = new double[mRow][nCol];
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                C[i][j] = matrix[i][j];
            }
        }
        return C;
    }

    /**
     * Set the double array of the matrix.
     * @param inArray the new array of doubles.
     */
    public void setArray( double[][] inArray ) {
        mRow = inArray.length;
        nCol = inArray[0].length;
        for ( int i = 0; i < mRow; i++ ) {
            if ( matrix[i].length != nCol ) {
                throw new IllegalArgumentException( "All rows must have the same length." );
            }
        }
        matrix = inArray;
    }

    /**
     * Make a one-dimensional column packed copy of the internal array.
     * @return     Matrix elements packed in a one-dimensional array by columns.
     */
    public double[] getColumnPackedCopy() {
        double[] vals = new double[mRow * nCol];
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                vals[i + j * mRow] = matrix[i][j];
            }
        }
        return vals;
    }

    /**
     * Make a one-dimensional row packed copy of the internal array.
     * @return     Matrix elements packed in a one-dimensional array by rows.
     */
    public double[] getRowPackedCopy() {
        double[] vals = new double[mRow * nCol];
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                vals[i * nCol + j] = matrix[i][j];
            }
        }
        return vals;
    }

    /**
     * Get row dimension.
     * @return     m, the number of rows.
     */
    public int getRowDimension() {
        return mRow;
    }

    /**
     * Get column dimension.
     * @return     n, the number of columns.
     */
    public int getColumnDimension() {
        return nCol;
    }

    /**
     * Get a single element.
     * @param i    Row index.
     * @param j    Column index.
     * @return     matrix(i,j)
     * @exception  ArrayIndexOutOfBoundsException
     */
    public double get( int i, int j ) {
        return matrix[i][j];
    }

    /**
     * Get a submatrix.
     * @param i0   Initial row index.
     * @param i1   Final row index.
     * @param j0   Initial column index.
     * @param j1   Final column index.
     * @return     matrix(i0:i1,j0:j1).
     * @exception  ArrayIndexOutOfBoundsException Submatrix indices.
     */
    public Matrix getMatrix( int i0, int i1, int j0, int j1 ) {
        Matrix X = new Matrix( i1 - i0 + 1, j1 - j0 + 1 );
        double[][] B = X.getArray();
        try {
            for ( int i = i0; i <= i1; i++ ) {
                for ( int j = j0; j <= j1; j++ ) {
                    B[i - i0][j - j0] = matrix[i][j];
                }
            }
        } catch ( ArrayIndexOutOfBoundsException e ) {
            throw new ArrayIndexOutOfBoundsException( "Submatrix indices" );
        }
        return X;
    }

    /**
     * Get a submatrix.
     * @param r    Array of row indices.
     * @param c    Array of column indices.
     * @return     matrix(r(:),c(:)).
     * @exception  ArrayIndexOutOfBoundsException Submatrix indices.
     */
    public Matrix getMatrix( int[] r, int[] c ) {
        Matrix X = new Matrix( r.length, c.length );
        double[][] B = X.getArray();
        try {
            for ( int i = 0; i < r.length; i++ ) {
                for ( int j = 0; j < c.length; j++ ) {
                    B[i][j] = matrix[r[i]][c[j]];
                }
            }
        } catch ( ArrayIndexOutOfBoundsException e ) {
            throw new ArrayIndexOutOfBoundsException( "Submatrix indices" );
        }
        return X;
    }

    /**
     * Get a submatrix.
     * @param i0   Initial row index.
     * @param i1   Final row index.
     * @param c    Array of column indices.
     * @return     matrix(i0:i1,c(:)).
     * @exception  ArrayIndexOutOfBoundsException Submatrix indices.
     */
    public Matrix getMatrix( int i0, int i1, int[] c ) {
        Matrix X = new Matrix( i1 - i0 + 1, c.length );
        double[][] B = X.getArray();
        try {
            for ( int i = i0; i <= i1; i++ ) {
                for ( int j = 0; j < c.length; j++ ) {
                    B[i - i0][j] = matrix[i][c[j]];
                }
            }
        } catch ( ArrayIndexOutOfBoundsException e ) {
            throw new ArrayIndexOutOfBoundsException( "Submatrix indices" );
        }
        return X;
    }

    /**
     * Get a submatrix.
     * @param r    Array of row indices.
     * @param j0   Initial column index.
     * @param j1   Final column index.
     * @return     matrix(r(:),j0:j1).
     * @exception  ArrayIndexOutOfBoundsException Submatrix indices.
     */
    public Matrix getMatrix( int[] r, int j0, int j1 ) {
        Matrix X = new Matrix( r.length, j1 - j0 + 1 );
        double[][] B = X.getArray();
        try {
            for ( int i = 0; i < r.length; i++ ) {
                for ( int j = j0; j <= j1; j++ ) {
                    B[i][j - j0] = matrix[r[i]][j];
                }
            }
        } catch ( ArrayIndexOutOfBoundsException e ) {
            throw new ArrayIndexOutOfBoundsException( "Submatrix indices" );
        }
        return X;
    }

    /**
     * Set a single element.
     * @param i    Row index.
     * @param j    Column index.
     * @param s    matrix(i,j).
     * @exception  ArrayIndexOutOfBoundsException if i,j outside of matrix dimensions
     */
    public void set( int i, int j, double s ) {
        matrix[i][j] = s;
    }

    /**
     * Set the matrix.
     * @param matrix  the new matrix.
     */
    public void setMatrix( double[][] matrix ) {

        mRow = matrix.length;
        nCol = matrix[0].length;
        for ( int i = 0; i < mRow; i++ ) {
            if ( matrix[i].length != nCol ) {
                throw new IllegalArgumentException( "All rows must have the same length." );
            }
        }
        this.matrix = matrix;
    }

    /**
     * Set a submatrix.
     * @param i0   Initial row index.
     * @param i1   Final row index.
     * @param j0   Initial column index.
     * @param j1   Final column index.
     * @param X    matrix(i0:i1,j0:j1).
     * @exception  ArrayIndexOutOfBoundsException Submatrix indices.
     */
    public void setMatrix( int i0, int i1, int j0, int j1, Matrix X ) {
        try {
            for ( int i = i0; i <= i1; i++ ) {
                for ( int j = j0; j <= j1; j++ ) {
                    matrix[i][j] = X.get( i - i0, j - j0 );
                }
            }
        } catch ( ArrayIndexOutOfBoundsException e ) {
            throw new ArrayIndexOutOfBoundsException( "Submatrix indices" );
        }
    }

    /**
     * Set a submatrix.
     * @param r    Array of row indices.
     * @param c    Array of column indices.
     * @param X    matrix(r(:),c(:)).
     * @exception  ArrayIndexOutOfBoundsException Submatrix indices.
     */
    public void setMatrix( int[] r, int[] c, Matrix X ) {
        try {
            for ( int i = 0; i < r.length; i++ ) {
                for ( int j = 0; j < c.length; j++ ) {
                    matrix[r[i]][c[j]] = X.get( i, j );
                }
            }
        } catch ( ArrayIndexOutOfBoundsException e ) {
            throw new ArrayIndexOutOfBoundsException( "Submatrix indices" );
        }
    }

    /**
     * Set a submatrix.
     * @param r    Array of row indices.
     * @param j0   Initial column index.
     * @param j1   Final column index.
     * @param X    matrix(r(:),j0:j1).
     * @exception  ArrayIndexOutOfBoundsException Submatrix indices.
     */
    public void setMatrix( int[] r, int j0, int j1, Matrix X ) {
        try {
            for ( int i = 0; i < r.length; i++ ) {
                for ( int j = j0; j <= j1; j++ ) {
                    matrix[r[i]][j] = X.get( i, j - j0 );
                }
            }
        } catch ( ArrayIndexOutOfBoundsException e ) {
            throw new ArrayIndexOutOfBoundsException( "Submatrix indices" );
        }
    }

    /**
     * Set a submatrix.
     * @param i0   Initial row index.
     * @param i1   Final row index.
     * @param c    Array of column indices.
     * @param X    matrix(i0:i1,c(:)).
     * @exception  ArrayIndexOutOfBoundsException Submatrix indices.
     */
    public void setMatrix( int i0, int i1, int[] c, Matrix X ) {
        try {
            for ( int i = i0; i <= i1; i++ ) {
                for ( int j = 0; j < c.length; j++ ) {
                    matrix[i][c[j]] = X.get( i - i0, j );
                }
            }
        } catch ( ArrayIndexOutOfBoundsException e ) {
            throw new ArrayIndexOutOfBoundsException( "Submatrix indices" );
        }
    }

    /**
     * Matrix transpose.
     * @return    matrix'.
     */
    public Matrix transpose() {
        Matrix X = new Matrix( nCol, mRow );
        double[][] C = X.getArray();
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                C[j][i] = matrix[i][j];
            }
        }
        return X;
    }

    /**
     * One norm.
     * @return    maximum column sum.
     */
    public double norm1() {
        double f = 0;
        for ( int j = 0; j < nCol; j++ ) {
            double s = 0;
            for ( int i = 0; i < mRow; i++ ) {
                s += Math.abs( matrix[i][j] );
            }
            f = Math.max( f, s );
        }
        return f;
    }

    /**
     * Two norm.
     * @return    maximum singular value.
     */
    public double norm2() {
        return ( new SingularValueDecomposition( this ).norm2() );
    }

    /**
     * Infinity norm.
     * @return    maximum row sum.
     */
    public double normInf() {
        double f = 0;
        for ( int i = 0; i < mRow; i++ ) {
            double s = 0;
            for ( int j = 0; j < nCol; j++ ) {
                s += Math.abs( matrix[i][j] );
            }
            f = Math.max( f, s );
        }
        return f;
    }

    /**
     * Frobenius norm.
     * @return    sqrt of sum of squares of all elements.
     */
    public double normF() {
        double f = 0;
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                f = Maths.hypot( f, matrix[i][j] );
            }
        }
        return f;
    }

    /**
     * Unary minus.
     * @return    -matrix.
     */
    public Matrix uminus() {
        Matrix X = new Matrix( mRow, nCol );
        double[][] C = X.getArray();
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                C[i][j] = -matrix[i][j];
            }
        }
        return X;
    }

    /**
     * C = matrix + B.
     * @param B    another matrix.
     * @return     matrix + B.
     */
    public Matrix plus( Matrix B ) {
        checkMatrixDimensions( B );
        Matrix X = new Matrix( mRow, nCol );
        double[][] C = X.getArray();
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                C[i][j] = matrix[i][j] + B.matrix[i][j];
            }
        }
        return X;
    }

    /**
     * matrix = matrix + B.
     * @param B    another matrix.
     * @return     matrix + B.
     */
    public Matrix plusEquals( Matrix B ) {
        checkMatrixDimensions( B );
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                matrix[i][j] = matrix[i][j] + B.matrix[i][j];
            }
        }
        return this;
    }

    /**
     * C = matrix - B.
     * @param B    another matrix.
     * @return     matrix - B.
     */
    public Matrix minus( Matrix B ) {
        checkMatrixDimensions( B );
        Matrix X = new Matrix( mRow, nCol );
        double[][] C = X.getArray();
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                C[i][j] = matrix[i][j] - B.matrix[i][j];
            }
        }
        return X;
    }

    /**
     * matrix = matrix - B.
     * @param B    another matrix.
     * @return     matrix - B.
     */
    public Matrix minusEquals( Matrix B ) {
        checkMatrixDimensions( B );
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                matrix[i][j] = matrix[i][j] - B.matrix[i][j];
            }
        }
        return this;
    }

    /**
     * Element-by-element multiplication, C = matrix.*B.
     * @param B    another matrix.
     * @return     matrix.*B.
     */
    public Matrix arrayTimes( Matrix B ) {
        checkMatrixDimensions( B );
        Matrix X = new Matrix( mRow, nCol );
        double[][] C = X.getArray();
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                C[i][j] = matrix[i][j] * B.matrix[i][j];
            }
        }
        return X;
    }

    /**
     * Element-by-element multiplication in place, matrix = matrix.*B.
     * @param B    another matrix.
     * @return     matrix.*B.
     */
    public Matrix arrayTimesEquals( Matrix B ) {
        checkMatrixDimensions( B );
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                matrix[i][j] = matrix[i][j] * B.matrix[i][j];
            }
        }
        return this;
    }

    /**
     * Element-by-element right division, C = matrix./B.
     * @param B    another matrix.
     * @return     matrix./B.
     */
    public Matrix arrayRightDivide( Matrix B ) {
        checkMatrixDimensions( B );
        Matrix X = new Matrix( mRow, nCol );
        double[][] C = X.getArray();
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                C[i][j] = matrix[i][j] / B.matrix[i][j];
            }
        }
        return X;
    }

    /**
     * Element-by-element right division in place, matrix = matrix./B.
     * @param B    another matrix.
     * @return     matrix./B.
     */
    public Matrix arrayRightDivideEquals( Matrix B ) {
        checkMatrixDimensions( B );
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                matrix[i][j] = matrix[i][j] / B.matrix[i][j];
            }
        }
        return this;
    }

    /**
     * Element-by-element left division, C = matrix.\B.
     * @param B    another matrix.
     * @return     matrix.\B.
     */
    public Matrix arrayLeftDivide( Matrix B ) {
        checkMatrixDimensions( B );
        Matrix X = new Matrix( mRow, nCol );
        double[][] C = X.getArray();
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                C[i][j] = B.matrix[i][j] / matrix[i][j];
            }
        }
        return X;
    }

    /**
     * Element-by-element left division in place, matrix = matrix.\B.
     * @param B    another matrix.
     * @return     matrix.\B.
     */
    public Matrix arrayLeftDivideEquals( Matrix B ) {
        checkMatrixDimensions( B );
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                matrix[i][j] = B.matrix[i][j] / matrix[i][j];
            }
        }
        return this;
    }

    /**
     * Multiply a matrix by a scalar, C = s*matrix.
     * @param s    scalar.
     * @return     s*matrix.
     */
    public Matrix times( double s ) {
        Matrix X = new Matrix( mRow, nCol );
        double[][] C = X.getArray();
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                C[i][j] = s * matrix[i][j];
            }
        }
        return X;
    }

    /**
     * Multiply a matrix by a scalar in place, matrix = s*matrix.
     * @param s    scalar.
     * @return     replace matrix by s*matrix.
     */
    public Matrix timesEquals( double s ) {
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                matrix[i][j] = s * matrix[i][j];
            }
        }
        return this;
    }

    /**
     * Linear algebraic matrix multiplication, matrix * B.
     * @param B    another matrix.
     * @return     Matrix product, matrix * B.
     * @exception  IllegalArgumentException Matrix inner dimensions must agree.
     */
    public Matrix times( Matrix B ) {
        if ( B.mRow != nCol ) {
            throw new IllegalArgumentException( "Matrix inner dimensions must agree." );
        }
        Matrix X = new Matrix( mRow, B.nCol );
        double[][] C = X.getArray();
        double[] Bcolj = new double[nCol];
        for ( int j = 0; j < B.nCol; j++ ) {
            for ( int k = 0; k < nCol; k++ ) {
                Bcolj[k] = B.matrix[k][j];
            }
            for ( int i = 0; i < mRow; i++ ) {
                double[] Arowi = matrix[i];
                double s = 0;
                for ( int k = 0; k < nCol; k++ ) {
                    s += Arowi[k] * Bcolj[k];
                }
                C[i][j] = s;
            }
        }
        return X;
    }

    /**
     * Linear algebraic matrix multiplication, matrix = matrix * B.
     * Added by Delia McGarry 1/11/2000.
     * @param B    another matrix
     * @exception  IllegalArgumentException Matrix inner dimensions must agree.
     * @exception  IllegalArgumentException num columns must agree with matrix's.
     */
    public void timesEquals( Matrix B ) {
        if ( B.mRow != nCol ) {
            throw new IllegalArgumentException( "Matrix inner dimensions must agree." );
        }
        if ( B.nCol != nCol ) {

            /* If B were allowed to have more columns than A, the new matrix would have that many columns,
             but the new matrix will be stored eventually in A.*/
            throw new IllegalArgumentException( "# of rows must equalA's." );
        }
        Matrix X = new Matrix( mRow, B.nCol );
        double[][] C = X.getArray();
        double[] Bcolj = new double[nCol];
        for ( int j = 0; j < B.nCol; j++ ) {
            for ( int k = 0; k < nCol; k++ ) {
                Bcolj[k] = B.matrix[k][j];
            }
            for ( int i = 0; i < mRow; i++ ) {
                double[] Arowi = matrix[i];
                double s = 0;
                for ( int k = 0; k < nCol; k++ ) {
                    s += Arowi[k] * Bcolj[k];
                }
                C[i][j] = s;
            }
        }
        matrix = X.getArray();

    }

    /**
     * LU Decomposition.
     * @return     LUDecomposition.
     * @see LUDecomposition.
     */
    public LUDecomposition lu() {
        return new LUDecomposition( this );
    }

    /**
     * QR Decomposition.
     * @return     QRDecomposition.
     * @see QRDecomposition.
     */
    public QRDecomposition qr() {
        return new QRDecomposition( this );
    }

    /**
     * Cholesky Decomposition.
     * @return     CholeskyDecomposition.
     * @see CholeskyDecomposition.
     */
    public CholeskyDecomposition chol() {
        return new CholeskyDecomposition( this );
    }

    /**
     * Singular Value Decomposition.
     * @return     SingularValueDecomposition.
     * @see SingularValueDecomposition.
     */
    public SingularValueDecomposition svd() {
        return new SingularValueDecomposition( this );
    }

    /**
     * Eigenvalue Decomposition.
     * @return     EigenvalueDecomposition.
     * @see EigenvalueDecomposition.
     */
    public EigenvalueDecomposition eig() {
        return new EigenvalueDecomposition( this );
    }

    /**
     * Solve matrix*X = B.
     * @param B    right hand side.
     * @return     solution if matrix is square, least squares solution otherwise.
     */
    public Matrix solve( Matrix B ) {
        return ( mRow == nCol ? ( new LUDecomposition( this ) ).solve( B ) : ( new QRDecomposition( this ) ).solve( B ) );
    }

    /**
     * Solve X*matrix = B, which is also matrix'*X' = B'.
     * @param B    right hand side.
     * @return     solution if matrix is square, least squares solution otherwise.
     */
    public Matrix solveTranspose( Matrix B ) {
        return transpose().solve( B.transpose() );
    }

    /**
     * Matrix inverse or pseudoinverse that returns a new Matrix.
     * @return     inverse(matrix) if matrix is square, pseudoinverse otherwise.
     */
    public Matrix inverse() {
        return solve( identity( mRow, mRow ) );
    }

    /**
     * Matrix inversion that replaces this objects matrix with an inverted matrix.
     * This method calls this classes inverse() method.
     */
    public void invert() {
        matrix = inverse().getArray();
    }

    /**
     * Matrix determinant.
     * @return     determinant.
     */
    public double det() {
        return new LUDecomposition( this ).det();
    }

    /**
     * Matrix rank.
     * @return     effective numerical rank, obtained from SVD.
     */
    public int rank() {
        return new SingularValueDecomposition( this ).rank();
    }

    /** Matrix condition (2 norm)
     * @return     ratio of largest to smallest singular value.
     */
    public double cond() {
        return new SingularValueDecomposition( this ).cond();
    }

    /**
     * Matrix trace.
     * @return     sum of the diagonal elements.
     */
    public double trace() {
        double t = 0;
        for ( int i = 0; i < Math.min( mRow, nCol ); i++ ) {
            t += matrix[i][i];
        }
        return t;
    }

    /**
     * Generate matrix with random elements.
     * @param m    Number of rows.
     * @param n    Number of colums.
     * @return     An m-by-n matrix with uniformly distributed random elements.
     */
    public static Matrix random( int m, int n ) {
        Matrix matrix = new Matrix( m, n );
        double[][] X = matrix.getArray();
        for ( int i = 0; i < m; i++ ) {
            for ( int j = 0; j < n; j++ ) {
                X[i][j] = Math.random();
            }
        }
        return matrix;
    }

    /**
     * Generate identity matrix.
     * @param m    Number of rows.
     * @param n    Number of colums.
     * @return     An m-by-n matrix with ones on the diagonal and zeros elsewhere.
     */
    public static Matrix identity( int m, int n ) {
        Matrix matrix = new Matrix( m, n );
        double[][] X = matrix.getArray();
        for ( int i = 0; i < m; i++ ) {
            for ( int j = 0; j < n; j++ ) {
                X[i][j] = ( i == j ? 1.0 : 0.0 );
            }
        }
        return matrix;
    }

    /**
     * Print the matrix to stdout.  Line the elements up in columns
     * with a Fortran-like 'Fw.d' style format.
     * @param w    Column width.
     * @param d    Number of digits after the decimal.
     */
    public void print( int w, int d ) {
        print( new PrintWriter( System.out, true ), w, d );
    }

    /**
     * Print the matrix to the output stream.  Line the elements up in
     * columns with a Fortran-like 'Fw.d' style format.
     * @param output Output stream.
     * @param w      Column width.
     * @param d      Number of digits after the decimal.
     */
    public void print( PrintWriter output, int w, int d ) {
        DecimalFormat format = new DecimalFormat();
        format.setMinimumIntegerDigits( 1 );
        format.setMaximumFractionDigits( d );
        format.setMinimumFractionDigits( d );
        format.setGroupingUsed( false );
        print( output, format, w + 2 );
    }

    /**
     * Print the matrix to stdout.  Line the elements up in columns.
     * Use the format object, and right justify within columns of width characters.
     * @param format matrix  Formatting object for individual elements.
     * @param width     Field width for each column.
     */
    public void print( NumberFormat format, int width ) {
        print( new PrintWriter( System.out, true ), format, width );
    }

    // DecimalFormat is a little disappointing coming from Fortran or C's printf.
    // Since it doesn't pad on the left, the elements will come out different
    // widths.  Consequently, we'll pass the desired column width in as an
    // argument and do the extra padding ourselves.

    /**
     * Print the matrix to the output stream.  Line the elements up in columns.
     * Use the format object, and right justify within columns of width characters.
     * @param output the output stream.
     * @param format matrix formatting object to format the matrix elements
     * @param width  Column width.
     */
    public void print( PrintWriter output, NumberFormat format, int width ) {
        output.println(); // start on new line.
        for ( int i = 0; i < mRow; i++ ) {
            for ( int j = 0; j < nCol; j++ ) {
                String s = format.format( matrix[i][j] ); // format the number
                int padding = Math.max( 1, width - s.length() ); // At _least_ 1 space
                for ( int k = 0; k < padding; k++ ) {
                    output.print( ' ' );
                }
                output.print( s );
            }
            output.println();
        }
        output.println(); // end with blank line.
    }

    /**
     * Produce a string showing the contents of the matrix.
     * @return  a string with the matrix values.
     */
    public String toString() {
        return matrixToString( 10, 4 );
    }

    /**
     * Produces a string of the matrix values.
     * @param w    Column width.
     * @param d    Number of digits after the decimal.
     * @return     String containing the values from the matrix.
     */
    public String matrixToString( int w, int d ) {
        String s = new String();
        int i, j;
        DecimalFormat format = new DecimalFormat();
        format.setMinimumIntegerDigits( 1 );
        format.setMaximumFractionDigits( d );
        format.setMinimumFractionDigits( d );
        format.setGroupingUsed( false );

        for ( i = 0; i < mRow; i++ ) {
            s += "  ";
            for ( j = 0; j < nCol; j++ ) {
                s += format.format( matrix[i][j] ); // format the number
                s = s + "  ";
            }
            s = s + "\n";
        }
        return s;
    }

    /**
     * Read a matrix from a stream.  The format is the same the print method,
     * so printed matrices can be read back in.  Elements are separated by
     * whitespace, all the elements for each row appear on a single line,
     * the last row is followed by a blank line.
     * @param input  the input stream.
     * @return       the read-in matrix.
     * @exception java.io.IOException  if there is a problem reading from the stream
     */
    public static Matrix read( BufferedReader input ) throws java.io.IOException {
        StreamTokenizer tokenizer = new StreamTokenizer( input );

        // Although StreamTokenizer will parse numbers, it doesn't recognize
        // scientific notation (E or D); however, Double.valueOf does.
        // The strategy here is to disable StreamTokenizer's number parsing.
        // We'll only get whitespace delimited words, EOL's and EOF's.
        // These words should all be numbers, for Double.valueOf to parse.

        tokenizer.resetSyntax();
        tokenizer.wordChars( 0, 255 );
        tokenizer.whitespaceChars( 0, ' ' );
        tokenizer.eolIsSignificant( true );
        java.util.Vector v = new java.util.Vector();

        // Ignore initial empty lines
        while ( tokenizer.nextToken() == StreamTokenizer.TT_EOL ) {
            ;
        }
        if ( tokenizer.ttype == StreamTokenizer.TT_EOF ) {
            throw new java.io.IOException( "Unexpected EOF on matrix read." );
        }
        do {
            v.addElement( Double.valueOf( tokenizer.sval ) ); // Read & store 1st row.
        } while ( tokenizer.nextToken() == StreamTokenizer.TT_WORD );

        int n = v.size(); // Now we've got the number of columns!
        double[] row = new double[n];
        for ( int j = 0; j < n; j++ ) { // extract the elements of the 1st row.
            row[j] = ( (Double) v.elementAt( j ) ).doubleValue();
        }
        v.removeAllElements();
        v.addElement( row ); // Start storing rows instead of columns.
        while ( tokenizer.nextToken() == StreamTokenizer.TT_WORD ) {
            // While non-empty lines
            v.addElement( row = new double[n] );
            int j = 0;
            do {
                if ( j >= n ) {
                    throw new java.io.IOException
                            ( "Row " + v.size() + " is too long." );
                }
                row[j++] = Double.valueOf( tokenizer.sval ).doubleValue();
            } while ( tokenizer.nextToken() == StreamTokenizer.TT_WORD );
            if ( j < n ) {
                throw new java.io.IOException
                        ( "Row " + v.size() + " is too short." );
            }
        }
        int m = v.size(); // Now we've got the number of rows.
        double[][] matrix = new double[m][];
        v.copyInto( matrix ); // copy the rows out of the vector
        return new Matrix( matrix );
    }

    /* ------------------------
     * Private Methods
     * ------------------------ */

    /**
     * Check if size(matrix) == size(B).
     * @param B  the matrix to check against this matrix's dimensions.
     **/
    private void checkMatrixDimensions( Matrix B ) {
        if ( B.mRow != mRow || B.nCol != nCol ) {
            throw new IllegalArgumentException( "Matrix dimensions must agree." );
        }
    }

}
