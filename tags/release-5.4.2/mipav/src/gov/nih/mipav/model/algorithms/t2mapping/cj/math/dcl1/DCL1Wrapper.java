package gov.nih.mipav.model.algorithms.t2mapping.cj.math.dcl1;

import java.lang.*;
import java.util.Arrays;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;

/**  DCL1Wrapper class is strictly a wrapper for the fortran->Java 
 *   dcl1 code.  Nothing else should be done here except 
 *   things that are needed for the wrapper.
 *   method of linear programming to calculate an l1 solution 
 *   to A K by N system of linear equations 
 *             AX=B 
 *   subject to L linear equality constraints 
 *             CX=D 
 *   and M linear inequality constraints 
 *             EX <= F. 
 *
 *             A B 
 *         Q = C D 
 *             E F 
 *        these values are destroyed by the subroutine. 
 *
 */
public class DCL1Wrapper
{
	/* 
	 *  Ax = B.
	 */
	private double[] A = null;
	private int nrowsA;
	private int ncolsA;

	/* 
	 *  Cx = D equality constraints.
	 */
	private double[] C = null;
	private double[] D = null;
	private int nrowsC;
	private int ncolsC;

	/* 
	 *  Ex <= F inequality constraints.
	 */
	private double[] E = null;
	private double[] F = null;
	private int nrowsE;
	private int ncolsE;

	private Dcl1 dcl1 = null;

	/*
	 *  Things that need to be passed into dcl1.
	 */
	int K = 0;
	int L = 0;
	int M = 0;
	int N = 0;
	int KLMD = 0;
	int KLM2D = 0;
	int NKLMD = 0;
	int N2D = 0;
	intW KODE;
	double toler = 0;
	intW iter;
	doubleW error;
	double[] RES = null;
	double[] CU = null;
	int[] IU = null;
	int[] S = null;
    double[] X = null;

    double[] Q = null;
    double[] Qtopass = null;

	public DCL1Wrapper() 
	{
		dcl1 = new Dcl1();
	}

	/** Set the A matrix as defined above.
	 */
	public void setA(final double[] A, final int nrowsA, final int ncolsA)
	{
		if( A.length != (nrowsA*ncolsA) )
		{
			System.out.print("DCL1Wrapper::updateA: A is not " + nrowsA);
			System.out.println("x" + ncolsA + " as it should be.");
			System.exit(-1);
		}

		if( this.A == null || this.A.length != (nrowsA * ncolsA) )
		{
			this.A = new double[ nrowsA * ncolsA ];
		}

		System.arraycopy(A, 0, this.A, 0, A.length);
		this.nrowsA = nrowsA;
		this.ncolsA = ncolsA;

		initialize();
	}

	/** Set the C matrix and D vector as defined above.
	 */
	public void setCD(final double[] C, final int nrowsC, final int ncolsC, 
	                  final double[] D)
	{
		if( C.length != (nrowsC*ncolsC) )
		{
			System.out.print("DCL1Wrapper::updateC: C is not " + nrowsC);
			System.out.println("x" + ncolsC + " as it should be.");
			System.exit(-1);
		}

		/*
		 *  Copy over the C matrix.
		 */
		if( this.C == null || this.C.length != (nrowsC * ncolsC) )
		{
			this.C = new double[ nrowsC * ncolsC ];
		}

		System.arraycopy(C, 0, this.C, 0, C.length);
		this.nrowsC = nrowsC;
		this.ncolsC = ncolsC;

		/*
		 *  Copy over the D vector.
		 */
		if( this.D == null || this.D.length != nrowsC )
		{
			this.D = new double[ nrowsC ];
		}

		System.arraycopy(D, 0, this.D, 0, D.length);

		initialize();
	}


	/** Set the E matrix and F vector as defined above.
	 */
	public void setEF(final double[] E, final int nrowsE, final int ncolsE, 
	                  final double[] F)
	{
		if( E.length != (nrowsE*ncolsE) )
		{
			System.out.print("DCL1Wrapper::updateE: E is not " + nrowsE);
			System.out.println("x" + ncolsE + " as it should be.");
			System.exit(-1);
		}

		/*
		 *  Eopy over the E matrix.
		 */
		if( this.E == null || this.E.length != (nrowsE * ncolsE) )
		{
			this.E = new double[ nrowsE * ncolsE ];
		}

		System.arraycopy(E, 0, this.E, 0, E.length);
		this.nrowsE = nrowsE;
		this.ncolsE = ncolsE;

		/*
		 *  Eopy over the F vector.
		 */
		if( this.F == null || this.F.length != nrowsE )
		{
			this.F = new double[ nrowsE ];
		}

		System.arraycopy(F, 0, this.F, 0, F.length);

		initialize();
	}

	/** Setup the Q matrix and the other variables that are required
	 *  by the Dcl1 code.
	 */
	private void initialize() 
	{
		K = nrowsA;
		L = nrowsC;
		M = nrowsE;
		N = ncolsA;

		KLMD = K + L + M;
		KLM2D = K + L + M + 2;
		NKLMD = N + K + L + M;
		N2D = N + 2;

		if( KODE == null )
		{
			KODE = new intW(1);
		}

		toler = Math.pow(10, -20*2/3);

		if( iter == null )
		{
			iter = new intW(10*(K+L+M));
		}

		error = new doubleW(0.0);

		X = new double[N2D];
		Arrays.fill(X, 1.0);

		RES = new double[KLMD];
		Arrays.fill(RES, 0.0);

		CU = new double[2*NKLMD];
		IU = new int[2*NKLMD];
		S = new int[KLMD];

		Q = new double[ KLM2D * N2D ];
		Qtopass = new double[ KLM2D * N2D ];

		/*
		 *  Copy A into Q.
		 */
		Arrays.fill(Q, 0.0);
		Arrays.fill(Qtopass, 0.0);
		for(int ii=0; ii<K; ii++)
		{
			for(int jj=0; jj<N; jj++)
			{
				Q[index(ii,jj,KLM2D,N2D)] = A[index(ii,jj,K,N)];
			}
		}

		if( C != null ) 
		{
			/*
			 *  Copy C into Q.
			 */
			for(int ii=0; ii<K; ii++)
			{
				for(int jj=0; jj<N; jj++)
				{
					Q[index(ii+K,jj,KLM2D,N2D)] = C[index(ii,jj,K,N)];
				}
			}

			/*
			 *  Copy D into Q.
			 */
			for(int ii=0; ii<K; ii++)
			{
				Q[index(ii+K,N,KLM2D,N2D)] = D[ii];
			}
		}

		if( E != null ) 
		{
			/*
			 *  Copy E into Q.
			 */
			for(int ii=0; ii<K; ii++)
			{
				for(int jj=0; jj<N; jj++)
				{
					Q[index(ii+K+L,jj,KLM2D,N2D)] = E[index(ii,jj,K,N)];
				}
			}

			/*
			 *  Copy F into Q.
			 */
			for(int ii=0; ii<K; ii++)
			{
				Q[index(ii+K+L,N,KLM2D,N2D)] = F[ii];
			}
		}
	}

	private int index(final int row, final int col, 
	                  final int nrows, final int ncols)
	{
		return (col*nrows + row);
	}

	public void solve(final double[] b, double[] x) 
	{
		/*
		 *  Setup the Q matirx and the other variables.
		 */
//		initialize();

		/*
		 *  Setup the variables.
		 */
//		KODE.val = 1;
//		toler = Math.pow(10, -20*2/3);
//		iter.val = 10*(K+L+M);
		Arrays.fill(X, 1.0);  // Positivity
		Arrays.fill(RES, 0.0);

		System.arraycopy(Q, 0, Qtopass, 0, Q.length);

		/*
		 *  Copy B into Q.
		 */
		if( K != b.length ) 
		{
			System.out.println("nrows A (" + K + " != nrows B ("+ 
			                   b.length +"), why?");
			System.exit(-1);
		}

		for(int ii=0; ii<K; ii++)
		{
			Qtopass[index(ii,N,KLM2D,N2D)] = b[ii];
		}

		/*
		 *  Now call the dcl1 routine.
		 */
//        long start = System.currentTimeMillis();
		dcl1.init( K, L, M, N, KLMD,  KLM2D, NKLMD, N2D,
			KODE, toler, iter, X, RES,  error, 
			CU, IU, S);
		dcl1.setQ( Qtopass );
		dcl1.dcl1( X );
//        long end = System.currentTimeMillis();
//		System.out.println("DCL1Wrapper dcl1_: start-end " + (float)(end-start)/1000.0 + " seconds");



		System.arraycopy(X, 0, x, 0, x.length);
	}
}
