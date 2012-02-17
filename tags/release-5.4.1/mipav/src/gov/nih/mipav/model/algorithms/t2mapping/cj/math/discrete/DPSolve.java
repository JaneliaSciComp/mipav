package gov.nih.mipav.model.algorithms.t2mapping.cj.math.discrete;

import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;

public class DPSolve
{
	/**
	 * Dynamic programming algorithm for maximum-weight monotone path.
	 *
	 * Given a matrix of weights, and a list of adjacency constraints,
     * we wish to find a path down the rows of the matrix which maximises
	 * the sum of all the weights on the path.  The constraints determine
	 * which columns of a given row are adjacent to which columns in the
     * subsequent row, based on relative (non-cyclic) offsets.
	 *
	 * @param val - Weighting matrix, in row-major order.
	 * @param adj - Which relative positions are adjacent, e.g. {0,1}
     */
	public static int[] solveMaxMonotonePath( double[][] val, int[] adj )
	{
		int h = Matrix.getNRows(val), w = Matrix.getNCols(val);

		// In standard DP fashion we inductively solve the more specific
		// problem "what is the maximum weight path that starts anywhere
        // at the top and ends at a fixed element?"

		int[][] bp = new int[h][w];  // back-pointer indices
		double[][] accum = new double[h][w];  // optimal subsolutions

		// First row is trivial
		for(int i=0;i<w;i++) accum[0][i] = val[0][i];

		// Now solve subproblem on each following row
		for(int i=1;i<h;i++)
		{
			for(int j=0;j<w;j++)
			{
				bp[i][j] = -1;
				accum[i][j] = Double.NEGATIVE_INFINITY;

				// Find neighbour with best subsolution
				for(int k=0;k<adj.length;k++)
				{
					int pos = j-adj[k];
					if (pos < 0 || pos >= w) continue;

					double m = accum[i-1][pos] + val[i][j];

					if (m > accum[i][j])
						{ bp[i][j] = pos; accum[i][j] = m; }
				}
			}
		}

		// Now read off solution
		double m = Double.NEGATIVE_INFINITY;
		int ind = -1;

		// First find the highest-valued end-point
		for(int j=0;j<w;j++) if (accum[h-1][j] > m)
			{ m = accum[h-1][j]; ind = j; }

		if (ind == -1) return null;

		// Now follow back-pointers to rest of solution
		int[] out = new int[h]; out[h-1] = ind;

		for(int i=h-1;i>0;i--) { ind = out[i-1] = bp[i][ind]; }

		return out;
	}

	public static void main(String argv[])
	{
		int m=24,n=21; double[][] A = new double[m][n];

		for(int i=0;i<m;i++) for(int j=0;j<n;j++)
			A[i][j] = (int)(Math.random()*100) - 50;
		System.out.println(Matrix.toString(A));

		int[][] off = {{-1,0,1},{0,-1},{0,1},{-1,1}};

		for(int k=0;k<50000;k++)
		{
			int[] foo = solveMaxMonotonePath(A,off[k%off.length]);
			for(int i=0;i<foo.length;i++) System.out.print((1+foo[i]) + " ");
			System.out.print("-- ");
			double tot=0; for(int i=0;i<foo.length;i++) tot += A[i][foo[i]];
			System.out.println(tot);
		}
	}
}
