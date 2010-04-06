package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

public abstract class LevmarBoxConstraint {
	// This is a port of LEVMAR_BC_DER.  It requires a function for computing the Jacobian.
	// If no such function is available, use LEVMAR_BC_DIF rather than LEVMAR_BC_DER.
	private final double INIT_MU = 1.0E-3;
	
	private final double STOP_THRESH = 1.0E-17;
	
	private final double DIFF_DELTA = 1.0E-6;
	
	private final int LM_ERROR = -1;
	
	private final int BLOCKSZ = 32; /* block size for cache-friendly matrix-matrix multiply. It should be
     * such that __BLOCKSZ__^2*sizeof(LM_REAL) is smaller than the CPU (L1)
     * data cache size. Notice that a value of 32 when LM_REAL=double assumes
     * an 8Kb L1 data cache (32*32*8=8K). This is a concervative choice since
     * newer Pentium 4s have a L1 data cache of size 16K, capable of holding
     * up to 45x45 double blocks.
     */
    private final int BLOCKSZ_SQ  = BLOCKSZ * BLOCKSZ;
	
	// On input the parameter estimates.  On output the estimated solution
	private double[] param; 
	
	private double[] xSeries;
	
	// The measurement vector.  null implies a zero vector
	private double[] ySeries;
	
	private int paramNum;
	
	// Measurement vector dimension
	private int nPts;
	
	// Vector of lower bounds.  If null, no lower bounds apply.
	private double lb[];
	
	// Vector of upper bounds.  If null, no upper bounds apply.
	private double ub[];
	
	private int maxIterations;
	
	// 4 minimization options mu, epsilon1, epsilon2, epsilon3
	// Respectively the scale factor for initial \mu,
    // stopping thresholds for ||J^T e||_inf, ||Dp||_2 and ||e||_2. Set to NULL for defaults to be used.
    // Note that ||J^T e||_inf is computed on free (not equal to lb[i] or ub[i]) variables only.
	private double opts[];
	
	// Information regarding minimization size 10  Set to null if don't care.
	// info[0]= ||e||_2 at initial p.
    // info[1-4]=[ ||e||_2, ||J^T e||_inf,  ||Dp||_2, mu/max[J^T J]_ii ], all computed at estimated p.
    // info[5]= # iterations,
    // info[6]=reason for terminating: 1 - stopped by small gradient J^T e
    //                                 2 - stopped by small Dp
    //                                3 - stopped by itmax
    //                                 4 - singular matrix. Restart from current p with increased mu 
    //                                5 - no further error reduction is possible. Restart with increased mu
    //                                6 - stopped by small ||e||_2
    //                                7 - stopped by invalid (i.e. NaN or Inf) "func" values. This is a user error
    // info[7]= # function evaluations
    // info[8]= # Jacobian evaluations
    // info[9]= # linear systems solved, i.e. # attempts for reducing error
	private int info[];
	
	double DOUBLE_EPSILON;
	
	public LevmarBoxConstraint(double param[], double xSeries[], double ySeries[], int paramNum, int nPts,
			                   double lb[], double ub[], int maxIterations, 
			                   double opts[], int info[]) {
		try {
			this.param = param;
			this.xSeries = xSeries;
			this.ySeries = ySeries;
			this.paramNum = paramNum;
			this.nPts = nPts;
			this.lb = lb;
			this.ub = ub;
			this.maxIterations = maxIterations;
			this.opts = opts;
			this.info = info;
		}
		catch (OutOfMemoryError error) { }
	} // public LevmarBoxConstraint
	
	public abstract void fitToFunction(double[] param, double[] hx, int paramNum, int nPts);
	
	public abstract void fitToJacobian(double[] param, double[] jac, int paramNum, int nPts);

	
	public int driver() {
		int i, j, k, l;
		int worksz;
		int freework = 0;
		int issolved;
		/* temp work arrays */
		double e[] = new double[nPts];
		double hx[] = new double[nPts];
		double jacTe[] = new double[paramNum];
		double jac[] = new double[nPts * paramNum];
		double jacTjac[] = new double[paramNum * paramNum];
		double Dp[] = new double[paramNum];
		// diagonal of J^T J
		double diag_jacTjac[] = new double[paramNum];
		double pDp[] = new double[paramNum];
		// damping constant
		double mu;
		// mainly used in matrix and vector multiplications
		double tmp;
		// ||e(p)||_2
		double p_eL2;
		// ||J^T e||_inf
		double jacTe_inf;
		// ||e(p+Dp) ||_2
		double pDp_eL2;
		double p_L2;
		double Dp_L2 = Double.MAX_VALUE;
		double dF;
		double dL;
		double tau;
		double eps1;
		double eps2;
		double eps2_sq;
		double eps3;
		double init_p_eL2;
		int nu = 2;
		int nu2;
		int stop = 0;
		int nfev[] = new int[1];
		int njev = 0;
		int nlss = 0;
		final int nm = nPts * paramNum;
		
		/* variables for constrained LM */
		int fstate_nPts;
		int fstate_nfev[];
		double fstate_hx[];
		double fstate_ySeries[];
		double alpha = 1.0E-4;
		double beta = 0.9;
		double gamma = 0.99995;
		double gamma_sq = gamma * gamma;
	    double rho = 1.0E-8;
	    double t;
	    double t0;
	    releps();
	    double stept1 = 1.0E3 * Math.sqrt(DOUBLE_EPSILON);
	    double jacTeDp;
	    /* Minimum step length for LS and PG steps */
	    double tmin = 1.0E-12;
	    double tming = 1.0E-18;
	    /* Initial step length for the LS and PG steps */
	    final double tini = 1.0;
	    int nLMsteps = 0;
	    int nLSsteps = 0;
	    int nPGsteps = 0;
	    int gprevtaken = 0;
	    int numactive;
	    
	    mu = 0.0;
	    jacTe_inf = 0.0;
	    t = 0.0;
	    
	    if (nPts < paramNum) {
	    	MipavUtil.displayError("Cannot solve a problem with fewer measurements = " + nPts +
	    			              " than unknowns = " + paramNum);
	    	return LM_ERROR;
	    }
	    
	    if ((lb != null) && (ub != null)) {
	    	for (i = 0; i < paramNum; i++) {
	    		if (lb[i] > ub[i]) {
	    		    MipavUtil.displayError("At least one lower bound exceeds the upper one");
	    		    return LM_ERROR;
	    		}
	    	}
	    } // if ((lb != null) && (ub != null))
	    
	    if (opts != null) {
	    	tau = opts[0];
	    	eps1 = opts[1];
	    	eps2 = opts[2];
	    	eps2_sq = opts[2] * opts[2];
	    	eps3 = opts[3];
	    }
	    else {
	    	// use default values
	    	tau = INIT_MU;
	    	eps1 = STOP_THRESH;
	    	eps2 = STOP_THRESH;
	    	eps2_sq = STOP_THRESH * STOP_THRESH;
	    	eps3 = STOP_THRESH;
	    }
	    
	    fstate_nPts = nPts;
	    fstate_hx = hx;
	    fstate_ySeries = ySeries;
	    fstate_nfev = nfev;
	    
	    /* See if starting point is within the feasible set */
	    for (i = 0; i < paramNum; ++i) {
	    	pDp[i] = param[i];
	    }
	    /* project to feasible set */
	    boxProject(param, lb, ub, paramNum);
	    for (i = 0; i < paramNum; ++i) {
	    	if (pDp[i] != param[i]) {
	    		Preferences.debug("Warning: component " + i + " of starting point not feasible in LevmarBoxConstraint\n");
	    		Preferences.debug(pDp[i] + " projected to " + param[i] + "\n");
	    	} 
	    }
	    /* Compute e = ySeries - f(param) and its L2 norm */
	    fitToFunction(param, hx, paramNum, nPts);
	    nfev[0] = 1;
	    /* e = ySeries - hx, p_eL2 = ||e|| */
	    p_eL2 = LEVMAR_L2NRMXMY(e, ySeries, hx, nPts);
	    init_p_eL2 = p_eL2;
	    if ((Double.isInfinite(p_eL2)) || (Double.isNaN(p_eL2))) {
	    	stop = 7;
	    }
	    
	    for (k = 0; k < maxIterations && (stop == 0); ++k) {
	        /* Note that param and e have been updated at a previous iteration */
	    	
	    	if (p_eL2 <= eps3) { // error is small
	    		stop = 6;
	    		break;
	    	}
	    	
	    	/* Compute the Jacobian J at p,  J^T J,  J^T e,  ||J^T e||_inf and ||p||^2.
	         * Since J^T J is symmetric, its computation can be sped up by computing
	         * only its upper triangular part and copying it to the lower part
	         */
	    	
	    	fitToJacobian(param, jac, paramNum, nPts);
	    	
	    	/* J^T J, J^T e */
	        if(nm < BLOCKSZ_SQ){ // this is a small problem
	          /* J^T*J_ij = \sum_l J^T_il * J_lj = \sum_l J_li * J_lj.
	           * Thus, the product J^T J can be computed using an outer loop for
	           * l that adds J_li*J_lj to each element ij of the result. Note that
	           * with this scheme, the accesses to J and JtJ are always along rows,
	           * therefore induces less cache misses compared to the straightforward
	           * algorithm for computing the product (i.e., l loop is innermost one).
	           * A similar scheme applies to the computation of J^T e.
	           * However, for large minimization problems (i.e., involving a large number
	           * of unknowns and measurements) for which J/J^T J rows are too large to
	           * fit in the L1 cache, even this scheme incures many cache misses. In
	           * such cases, a cache-efficient blocking scheme is preferable.
	           *
	           * Thanks to John Nitao of Lawrence Livermore Lab for pointing out this
	           * performance problem.
	           *
	           * Note that the non-blocking algorithm is faster on small
	           * problems since in this case it avoids the overheads of blocking. 
	           */
	          int im;
	          int jaclmIndex;
	          
	          /* looping downwards saves a few computations */
	          for(i=paramNum*paramNum; i-->0; )
	            jacTjac[i]=0.0;
	          for(i=paramNum; i-->0; )
	            jacTe[i]=0.0;

	          for(l=nPts; l-->0; ){
	            jaclmIndex = l*paramNum;
	            for(i=paramNum; i-->0; ){
	              im=i*paramNum;
	              alpha=jac[jaclmIndex + i]; //jac[l*m+i];
	              for(j=i+1; j-->0; ) /* j<=i computes lower triangular part only */
	                jacTjac[im+j]+=jac[jaclmIndex + j]*alpha; //jac[l*m+j]

	              /* J^T e */
	              jacTe[i]+=alpha*e[l];
	            }
	          }

	          for(i=paramNum; i-->0; ) /* copy to upper part */
	            for(j=i+1; j<paramNum; ++j)
	              jacTjac[i*paramNum+j]=jacTjac[j*paramNum+i];
	        } // if(nm < BLOCKSZ_SQ){ // this is a small problem
	        else{ // this is a large problem
	            /* Cache efficient computation of J^T J based on blocking
	             */
	            LEVMAR_TRANS_MAT_MAT_MULT(jac, jacTjac, nPts, paramNum);

	            /* cache efficient computation of J^T e */
	            for(i=0; i<paramNum; ++i)
	              jacTe[i]=0.0;

	            for(i=0; i<nPts; ++i){
	              int jacrowIndex;

	              for(l=0, jacrowIndex=i*paramNum, tmp=e[i]; l<paramNum; ++l)
	                jacTe[l]+=jac[jacrowIndex+l]*tmp;
	            }
	          }
	    } // for (k = 0; k < maxIterations && (stop == 0); ++k)
	    return 0;
	} // public void driver()
	
	void LEVMAR_TRANS_MAT_MAT_MULT(double a[], double b[], int n, int m)
	{
	//#ifdef HAVE_LAPACK /* use BLAS matrix multiply */

	//double alpha=1.0;
	//double beta=0.0;
	  /* Fool BLAS to compute a^T*a avoiding transposing a: a is equivalent to a^T in column major,
	   * therefore BLAS computes a*a^T with a and a*a^T in column major, which is equivalent to
	   * computing a^T*a in row major!
	   */
	  //GEMM("N", "T", &m, &m, &n, &alpha, a, &m, a, &m, &beta, b, &m);

	//#else /* no LAPACK, use blocking-based multiply */

	int i, j, k, jj, kk;
	double sum;
	int bimIndex, akmIndex;
	final int bsize= BLOCKSZ;

	

	  /* compute upper triangular part using blocking */
	  for(jj=0; jj<m; jj+=bsize){
	    for(i=0; i<m; ++i){
	      bimIndex=i*m;
	      for(j=Math.max(jj, i); j< Math.min(jj+bsize, m); ++j)
	        b[bimIndex+j]=0.0; //b[i*m+j]=0.0;
	    }

	    for(kk=0; kk<n; kk+=bsize){
	      for(i=0; i<m; ++i){
	        bimIndex=i*m;
	        for(j=Math.max(jj, i); j< Math.min(jj+bsize, m); ++j){
	          sum=0.0;
	          for(k=kk; k< Math.min(kk+bsize, n); ++k){
	            akmIndex=k*m;
	            sum+=a[akmIndex+i]*a[akmIndex+j]; //a[k*m+i]*a[k*m+j];
	          }
	          b[bimIndex+j]+=sum; //b[i*m+j]+=sum;
	        }
	      }
	    }
	  }

	  /* copy upper triangular part to the lower one */
	  for(i=0; i<m; ++i)
	    for(j=0; j<i; ++j)
	      b[i*m+j]=b[j*m+i];

	//#endif /* HAVE_LAPACK */
	}
	
	/* Compute e=x-y for two n-vectors x and y and return the squared L2 norm of e.
	 * e can coincide with either x or y; x can be NULL, in which case it is assumed
	 * to be equal to the zero vector.
	 * Uses loop unrolling and blocking to reduce bookkeeping overhead & pipeline
	 * stalls and increase instruction-level parallelism; see http://www.abarnett.demon.co.uk/tutorial.html
	 */

	private double LEVMAR_L2NRMXMY(double e[], double x[], double y[], int n)
	{
	final int blocksize=8, bpwr=3; /* 8=2^3 */
	int i;
	int j1, j2, j3, j4, j5, j6, j7;
	int blockn;
	double sum0=0.0, sum1=0.0, sum2=0.0, sum3=0.0;

	  /* n may not be divisible by blocksize, 
	   * go as near as we can first, then tidy up.
	   */ 
	  blockn = (n>>bpwr)<<bpwr; /* (n / blocksize) * blocksize; */

	  /* unroll the loop in blocks of `blocksize'; looping downwards gains some more speed */
	  if(x != null){
	    for(i=blockn-1; i>0; i-=blocksize){
	              e[i ]=x[i ]-y[i ]; sum0+=e[i ]*e[i ];
	      j1=i-1; e[j1]=x[j1]-y[j1]; sum1+=e[j1]*e[j1];
	      j2=i-2; e[j2]=x[j2]-y[j2]; sum2+=e[j2]*e[j2];
	      j3=i-3; e[j3]=x[j3]-y[j3]; sum3+=e[j3]*e[j3];
	      j4=i-4; e[j4]=x[j4]-y[j4]; sum0+=e[j4]*e[j4];
	      j5=i-5; e[j5]=x[j5]-y[j5]; sum1+=e[j5]*e[j5];
	      j6=i-6; e[j6]=x[j6]-y[j6]; sum2+=e[j6]*e[j6];
	      j7=i-7; e[j7]=x[j7]-y[j7]; sum3+=e[j7]*e[j7];
	    }

	   /*
	    * There may be some left to do.
	    * This could be done as a simple for() loop, 
	    * but a switch is faster (and more interesting) 
	    */ 

	    i=blockn;
	    if(i<n){ 
	      /* Jump into the case at the place that will allow
	       * us to finish off the appropriate number of items. 
	       */ 

	      switch(n - i){ 
	        case 7 : e[i]=x[i]-y[i]; sum0+=e[i]*e[i]; ++i;
	        case 6 : e[i]=x[i]-y[i]; sum1+=e[i]*e[i]; ++i;
	        case 5 : e[i]=x[i]-y[i]; sum2+=e[i]*e[i]; ++i;
	        case 4 : e[i]=x[i]-y[i]; sum3+=e[i]*e[i]; ++i;
	        case 3 : e[i]=x[i]-y[i]; sum0+=e[i]*e[i]; ++i;
	        case 2 : e[i]=x[i]-y[i]; sum1+=e[i]*e[i]; ++i;
	        case 1 : e[i]=x[i]-y[i]; sum2+=e[i]*e[i]; //++i;
	      }
	    }
	  }
	  else{ /* x==0 */
	    for(i=blockn-1; i>0; i-=blocksize){
	              e[i ]=-y[i ]; sum0+=e[i ]*e[i ];
	      j1=i-1; e[j1]=-y[j1]; sum1+=e[j1]*e[j1];
	      j2=i-2; e[j2]=-y[j2]; sum2+=e[j2]*e[j2];
	      j3=i-3; e[j3]=-y[j3]; sum3+=e[j3]*e[j3];
	      j4=i-4; e[j4]=-y[j4]; sum0+=e[j4]*e[j4];
	      j5=i-5; e[j5]=-y[j5]; sum1+=e[j5]*e[j5];
	      j6=i-6; e[j6]=-y[j6]; sum2+=e[j6]*e[j6];
	      j7=i-7; e[j7]=-y[j7]; sum3+=e[j7]*e[j7];
	    }

	   /*
	    * There may be some left to do.
	    * This could be done as a simple for() loop, 
	    * but a switch is faster (and more interesting) 
	    */ 

	    i=blockn;
	    if(i<n){ 
	      /* Jump into the case at the place that will allow
	       * us to finish off the appropriate number of items. 
	       */ 

	      switch(n - i){ 
	        case 7 : e[i]=-y[i]; sum0+=e[i]*e[i]; ++i;
	        case 6 : e[i]=-y[i]; sum1+=e[i]*e[i]; ++i;
	        case 5 : e[i]=-y[i]; sum2+=e[i]*e[i]; ++i;
	        case 4 : e[i]=-y[i]; sum3+=e[i]*e[i]; ++i;
	        case 3 : e[i]=-y[i]; sum0+=e[i]*e[i]; ++i;
	        case 2 : e[i]=-y[i]; sum1+=e[i]*e[i]; ++i;
	        case 1 : e[i]=-y[i]; sum2+=e[i]*e[i]; //++i;
	      }
	    }
	  }

	  return sum0+sum1+sum2+sum3;
	}

	
	private void boxProject(double p[], double lb[], double ub[], int m) {
		int i;
		if (lb == null) { // no lower bounds
			if (ub == null) { // no upper bounds
				return;
			}
			else { // upper bounds only
				for (i = 0; i < m; ++i) {
					if (p[i] > ub[i]) {
						p[i] = ub[i];
					}
				}
			}
		} // if (lb == null) 
		else {
			if (ub == null) { // lower bounds only
				for (i = 0; i < m; ++i) {
					if (p[i] < lb[i]) {
						p[i] = lb[i];
					}
				}
			} // if (ub == null)
			else { // box bounds
				for (i = 0; i < m; ++i) {
					p[i] = median3(lb[i], p[i], ub[i]);
				}
			}
		} // else
	} // boxProject
	
	private double median3(double a, double b, double c) {
		if (a >= b) {
			if (c >= a) {
				return a;
			} // if (c >= a)
			else if (c <= b) {
				return b;
			}
			else {
				return c;
			}
		} // if (a >= b)
		else if (c >= b) {
			return b;
		}
		else if (c <= a) {
			return a;
		}
		else {
			return c;
		}
	}
	
	private void releps() {
        // COMPUTE SRELPR = DOUBLE RELATIVE PRECISION FOR A BINARY
        // MACHINE   I.E.
        // DETERMINE THE SMALLEST POSITIVE NUMBER 0.5**K FOR WHICH
        // (1.0+0.5**K) > 1.0  AND  (1.0+0.5**(K+1)) = 1.0
        // WHERE K IS A POSITIVE INTEGER

        double temp, frac;
        boolean loop = true;
        frac = 1.0;

        while (loop) {
            frac = 0.5 * frac;
            temp = frac + 1.0;

            if (temp == 1.0) {
                break;
            }
        } // while (loop)

        DOUBLE_EPSILON = 2.0 * frac;

        return;
    }

}