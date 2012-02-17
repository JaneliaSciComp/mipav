package gov.nih.mipav.model.algorithms.t2mapping.cj.math.nnls;

import java.lang.*;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.nnls.Nnls;


/**  NNLS class is strictly a wrapper for the fortran->C->Java 
 *   nnls code.  Nothing else should be done here except 
 *   things that are needed for the wrapper.
 *
 */
public class NNLSWrapper
{

	private double[] A;
	private double[] A_passed;
	private double[] b_temp;

	private Nnls nnls;
	private int nrows;
	private int ncols;
	private int ndata;
    private double[] w;
    private double[] zz;
    doubleW rnorm;
    int[] index;
    intW mode;

    double[] b;
    double[] x;

	public NNLSWrapper() 
	{
        nnls = new Nnls();
    }

	public NNLSWrapper( final double[] A_in, final int nrows_in, 
	                                         final int ncols_in ) 
	{

		if( A_in != null && A_in.length != 0 ) {
			A = new double[ A_in.length ];
			A_passed = new double[ A_in.length ];
		
			for( int ii=0; ii<A_in.length; ii++) {
				A[ii] = A_in[ii];
			}

			nrows = nrows_in;
			ncols = ncols_in;

			initialize();
		}
	}

	public void update( double[] A) {

		if( A.length != this.A.length ) 
		{
			System.out.println("Must be same number of elements or specify the rows and columns");
			System.exit(-1);
		}

		System.arraycopy(A, 0, this.A, 0, A.length);
	}

	public void update( final double[] A_in, final int nrows_in, 
	                                         final int ncols_in ) 
	{

		if( A_in != null && A_in.length != 0 ) {
			A = new double[ A_in.length ];
			A_passed = new double[ A_in.length ];
		
			System.arraycopy(A_in, 0, this.A, 0, A_in.length);

			nrows = nrows_in;
			ncols = ncols_in;

			initialize();
		}
	}
		
	private void initialize() {

		ndata = nrows;

        try {
            w = new double[ncols];
            zz = new double[nrows];
            index = new int[ncols];
            b = new double[nrows];
            b_temp = new double[nrows];
            x = new double[ncols];
        }
        catch (Exception e) {
            System.err.println("Caught IOException: " + e.getMessage());
            System.exit(0);
        }                                                     
                                                              
        mode = new intW(0);                                   
        rnorm = new doubleW(0.0);                             

	}

	public void solve(final double[] b, double[] x) 
	{

		if( b.length != nrows ) {
			System.out.println("NNLS: compute: b is the wrong length");
		}

		if( x.length != ncols ) {
			System.out.println("NNLS: compute: x is the wrong length");
		}

        //
        //  Copy over a.
        //
		System.arraycopy( A, 0, A_passed, 0, A.length);
		System.arraycopy( b, 0, b_temp, 0, b.length);

		nnls.nnls(A_passed, 0, nrows, nrows, ncols, b_temp, 0, x, 0, rnorm,
                  w, 0, zz, 0, index, 0, mode);
	}
};
