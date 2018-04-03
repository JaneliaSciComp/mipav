package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;

public abstract class CVODES_ASA extends CVODES {
	final int CVArhs_select = 100;
	public CVODES_ASA() {
		super();
		runcvsRoberts_ASAi_dns();
	}
	
	// In CVODES.java have int problem = cvsRoberts_ASAi_dns;
	// Use the following code to call CVODES_ASA from another module:
		/*boolean testme = true;
	    class CVODEStest extends CVODES_ASA { // if running runcvsRoberts_ASAi_dns()
	    	  public CVODEStest() {
	    		  super();
	    	  }
	    	  public int f(double t, NVector yv, NVector ydotv, UserData user_data) {
	    		  return 0;
	    	  }
	    	  
	    	  public int g(double t, NVector yv, double gout[], UserData user_data) {
	    		  return 0;
	    	  }
	    	  
	    	  public int fQ(double t, NVector x, NVector y, UserData user_data) {
	    		  return 0;
	    	  }
	    	  
	    	  public int fS1(int Ns, double t, NVector yv, NVector ydot, int is,
				         NVector yS, NVector ySdot, UserData user_data, NVector tmp1, NVector tmp2) {
	        		  return 0;  
	          }
	    	  
	    	  public int Jac(double t, NVector yv, NVector fy, double J[][], UserData data, NVector tmp1, 
	    			  NVector tmp2, NVector tmp3) {
	    		  return 0;
	    	  }
	    	  
	    	  public int ewt(NVector y, NVector w, UserData user_data) {
	              return 0;
	          }
	          
	          public int fB(double t, NVector y, NVector yB, NVector yBdot, UserData user_dataB) {
	              return 0;
	          }
	          
	          public int JacB(double t, NVector y, NVector yB, NVector fyB, double JB[][], UserData user_dataB,
				NVector tmp1B, NVector tmp2B, NVector tmp3B) {
				  return 0;
			  }
	    	  
	    	  public int fQB(double t, NVector y, NVector yB, NVector qBdot, UserData user_dataB) {
	              return 0;
	          }
	      }
	    if (testme) {
	    	new CVODEStest();
	    	return;
	    } */
	
	private void runcvsRoberts_ASAi_dns() {
		/* -----------------------------------------------------------------
		 * Programmer(s): Radu Serban @ LLNL
		 * -----------------------------------------------------------------
		 * LLNS Copyright Start
		 * Copyright (c) 2014, Lawrence Livermore National Security
		 * This work was performed under the auspices of the U.S. Department
		 * of Energy by Lawrence Livermore National Laboratory in part under
		 * Contract W-7405-Eng-48 and in part under Contract DE-AC52-07NA27344.
		 * Produced at the Lawrence Livermore National Laboratory.
		 * All rights reserved.
		 * For details, see the LICENSE file.
		 * LLNS Copyright End
		 * -----------------------------------------------------------------
		 * Adjoint sensitivity example problem.
		 * The following is a simple example problem, with the coding
		 * needed for its solution by CVODES. The problem is from chemical
		 * kinetics, and consists of the following three rate equations.
		 *    dy1/dt = -p1*y1 + p2*y2*y3
		 *    dy2/dt =  p1*y1 - p2*y2*y3 - p3*(y2)^2
		 *    dy3/dt =  p3*(y2)^2
		 * on the interval from t = 0.0 to t = 4.e10, with initial
		 * conditions: y1 = 1.0, y2 = y3 = 0. The reaction rates are:
		 * p1=0.04, p2=1e4, and p3=3e7. The problem is stiff.
		 * This program solves the problem with the BDF method, Newton
		 * iteration with the CVODE dense linear solver, and a user-supplied
		 * Jacobian routine.
		 * It uses a scalar relative tolerance and a vector absolute
		 * tolerance.
		 * Output is printed in decades from t = .4 to t = 4.e10.
		 * Run statistics (optional outputs) are printed at the end.
		 * 
		 * Optionally, CVODES can compute sensitivities with respect to
		 * the problem parameters p1, p2, and p3 of the following quantity:
		 *   G = int_t0^t1 g(t,p,y) dt
		 * where
		 *   g(t,p,y) = y3
		 *        
		 * The gradient dG/dp is obtained as:
		 *   dG/dp = int_t0^t1 (g_p - lambda^T f_p ) dt - lambda^T(t0)*y0_p
		 *         = - xi^T(t0) - lambda^T(t0)*y0_p
		 * where lambda and xi are solutions of:
		 *   d(lambda)/dt = - (f_y)^T * lambda - (g_y)^T
		 *   lambda(t1) = 0
		 * and
		 *   d(xi)/dt = - (f_p)^T * lambda + (g_p)^T
		 *   xi(t1) = 0
		 * 
		 * During the backward integration, CVODES also evaluates G as
		 *   G = - phi(t0)
		 * where
		 *   d(phi)/dt = g(t,y,p)
		 *   phi(t1) = 0
		 * -----------------------------------------------------------------*/
	
		/** Problem Constants */
		final int NEQ = 3; // Number of equations
		//final double RTOL = 1.0E-6; // scalar relative tolerance
		final double RTOL = 1.0E-12;
		//final double ATOL1 = 1.0E-8; // vector absolute tolerance components
		final double ATOL1 = 1.0E-12;
		//final double ATOL2 = 1.0E-14;
		final double ATOL2 = 1.0E-15;
		//final double ATOL3 = 1.0E-6;
		final double ATOL3 = 1.0E-12;
		final double ATOLl = 1.0E-8; // absolute tolerance for adjoint variables
		final double ATOLq = 1.0E-6; // absolute tolerannce for quuadratures
		final double T0 = 0.0; // initial time
		final double TOUT = 4.0E7; // final time
		final double TB1 = 4.0E7; // starting point for adjoint problem
		final double TB2 = 50.0; // starting point for adjoint problem
		final double TBout1 = 40.0; // intermediate t for adjoint problem
		final int STEPS = 150; // number of steps between check points
		final int NP = 3; // number of problem parameters
		double reltolQ;
		double abstolQ;
		CVodeMemRec cvode_mem;
		int flag;
        int f = cvsRoberts_ASAi_dns;
        int Jac = cvsRoberts_ASAi_dns;
		int ewt_select = cvEwtUser_select1;
		int fQ= cvsRoberts_ASAi_dns;
		double A[][];
		SUNLinearSolver LS;
		int steps;
		double time[] = new double[1];
		int ncheck[] = new int[1];
		long nst;
		int i;
		CVadjCheckPointRec ckpnt[];
		double reltolB;
		double abstolB;
		double abstolQB;
		int indexB[] = new int[1];
		int fB = cvsRoberts_ASAi_dns;
		
		// Print problem description 
		System.out.printf("\nAdjoint Sensitivity Example for Chemical Kinetics\n");
		System.out.printf("-------------------------------------------------\n\n");
		System.out.printf("ODE: dy1/dt = -p1*y1 + p2*y2*y3\n");
		System.out.printf("     dy2/dt =  p1*y1 - p2*y2*y3 - p3*(y2)^2\n");
		System.out.printf("     dy3/dt =  p3*(y2)^2\n\n");
		System.out.printf("Find dG/dp for\n");
		System.out.printf("     G = int_t0^tB0 g(t,p,y) dt\n");
		System.out.printf("     g(t,p,y) = y3\n\n\n");

		// User data structure
		UserData data = new UserData();
		data.array = new double[3];
		data.array[0] = 0.04;
		data.array[1] = 1.0E4;
		data.array[2] = 3.0E7;
		
		// Initialize y
		NVector y = new NVector();
		double yr[] = new double[]{1.0,0.0,0.0};
		N_VNew_Serial(y, NEQ);
		y.setData(yr);
		
		// Initialize q
		NVector q = new NVector();
		double qr[] = new double[]{0.0};
		N_VNew_Serial(q,1);
		q.setData(qr);
		
		// Set the scalar relative and absolute tolerances reltolQ and abstolQ;
		reltolQ = RTOL;
		abstolQ = ATOLq;
		
		/* Create and allocate CVODES memory for forward run */
		System.out.printf("Create and allocate CVODES memory for forward runs\n");

		/* Call CVodeCreate to create the solver memory and specify the 
		     Backward Differentiation Formula and the use of a Newton iteration */
		cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
		if (cvode_mem == null) {
		    return;	
		}
		// Allow unlimited steps in reaching tout
		flag = CVodeSetMaxNumSteps(cvode_mem, -1);
		if (flag != CV_SUCCESS) {
			return;
		}
		// Allow many error test failures
		flag = CVodeSetMaxErrTestFails(cvode_mem, Integer.MAX_VALUE);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Call CVodeInit to initialize the integrator memory and specify the
		// user's right hand side function in y' = f(t,y), the initial time T0, and
	    // the initial dependent variable vector y
		flag = CVodeInit(cvode_mem, f, T0, y);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Use private function to compute error weights
		// CVodeWFtolerances specifies a user-provides function (of type CVEwtFn)
		// which will be called to set the error weight vector.
		flag = CVodeWFtolerances(cvode_mem, ewt_select);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Attach user data
		cvode_mem.cv_user_data = data;
		
		
		// Create dense SUNMATRIX for use in linear solver
		// indexed by columns stacked on top of each other
		try {
		    A = new double[NEQ][NEQ];
		}
		catch (OutOfMemoryError e) {
		    MipavUtil.displayError("Out of memory error trying to create A");
		    return;
		}
		
		// Create dense SUNLinearSolver object for use by CVode
		LS = SUNDenseLinearSolver(y, A);
		if (LS == null) {
			return;
		}
		
		// Call CVDlsSetLinearSolver to attach the matrix and linear solver to CVode
		flag = CVDlsSetLinearSolver(cvode_mem, LS, A);
		if (flag != CVDLS_SUCCESS) {
			return;
		}
		
		// Set the user-supplied Jacobian routine Jac
		flag = CVDlsSetJacFn(cvode_mem, Jac);
		if (flag != CVDLS_SUCCESS) {
			return;
		}
		
		// Call CVodeQuadInit to allocate internal memory and initialize
		// quadrature integration
		flag = CVodeQuadInit(cvode_mem, fQ, q);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		/* Call CVodeSetQuadErrCon to specify whether or not the quadrature variables
	     are to be used in the step size control mechanism within CVODES. Call
	     CVodeQuadSStolerances or CVodeQuadSVtolerances to specify the integration
	     tolerances for the quadrature variables. */
		cvode_mem.cv_errconQ = true;
		
		/* Call CVodeQuadSStolerances to specify scalar relative and absolute
	     tolerances. */
	    flag = CVodeQuadSStolerances(cvode_mem, reltolQ, abstolQ);
        if (flag != CV_SUCCESS) {
        	return;
        }

        /* Call CVodeAdjInit to update CVODES memory block by allocating the internal 
        memory needed for backward integration.*/
        steps = STEPS; /* no. of integration steps between two consecutive checkpoints*/
        flag = CVodeAdjInit(cvode_mem, steps, CV_HERMITE);
        /*
        flag = CVodeAdjInit(cvode_mem, steps, CV_POLYNOMIAL);
        */
        if (flag != CV_SUCCESS) {
        	return;
        }
        
        // Perform forward run
        System.out.printf("Forward integration ... ");
       
        // Call CVodeF to integrate the forward problem over an interval in time and
        // save checkpointing data.
        flag = CVodeF(cvode_mem, TOUT, y, time, CV_NORMAL, ncheck);
        if (flag < 0) {
        	return;
        }
        // Get the current number of integration steps
        nst = cvode_mem.cv_nst;

        System.out.printf("done ( nst = " + nst+")");
        System.out.printf("\nncheck = " + ncheck[0] + "\n\n");
        
        flag = CVodeGetQuad(cvode_mem, time, q);
        
        System.out.printf("------------------------------------------------\n");
        System.out.printf("G: " + q.data[0] + "\n");
        System.out.printf("------------------------------------------------\n\n");
        
        // Test check point linked list
        // (Uncomment next block to print check point information)
        /*System.out.printf("\nList of Check Points (ncheck = " + ncheck[0] + ")\n\n");
        ckpnt = new CVadjCheckPointRec[ncheck[0]+1];
        for (i = 0; i < ncheck[0]+1; i++) {
        	ckpnt[i] = new CVadjCheckPointRec();
        }
        CVodeGetAdjCheckPointsInfo(cvode_mem, ckpnt);
        for (i = 0; i <= ncheck[0]; i++) {
        	System.out.printf("Time interval: " + ckpnt[i].t0 + ", " +  ckpnt[i].t1 + "\n");
        	System.out.printf("Order: " + ckpnt[i].nstep + "\n");
        	System.out.printf("Step size: " + ckpnt[i].step + "\n");
        	System.out.printf("\n");
        }*/
        
        // Initialize yB
        NVector yB = new NVector();
		double yBr[] = new double[]{0.0,0.0,0.0};
		N_VNew_Serial(yB, NEQ);
		yB.setData(yBr);
		
		// Initialize qB
		NVector qB = new NVector();
		double qBr[] = new double[]{0.0,0.0,0.0};
		N_VNew_Serial(qB,NP);
		qB.setData(qBr);
		
		// Set the scalar relative tolerance reltolB
		reltolB = RTOL;
		
		// Set the scalar absolute tolerance abstolB
		abstolB = ATOLl;
		
		// Set the scalar absolute tolerance abstolQB
		abstolQB = ATOLq;
		
		// Create and allocate CVODES memory for backward run
		System.out.printf("Create and allocate CVODES memory for backward run\n");
		
		// Call CVodeCreateB to specify the solution method for the backward problem.
		flag = CVodeCreateB(cvode_mem, CV_BDF, CV_NEWTON, indexB);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Call CVodeInitB to allocate internal memory and initialize the
		// backward problem.
		flag = CVodeInitB(cvode_mem, indexB[0], fB, TB1, yB);
	}
	
	/*
	 * CVodeQuadInit
	 *
	 * CVodeQuadInit allocates and initializes quadrature related 
	 * memory for a problem. All problem specification inputs are 
	 * checked for errors. If any error occurs during initialization, 
	 * it is reported to the file whose file pointer is errfp. 
	 * The return value is CV_SUCCESS = 0 if no errors occurred, or
	 * a negative value otherwise.
	 */

	private int CVodeQuadInit(CVodeMemRec cv_mem, int fQ, NVector yQ0)
	{
	  boolean allocOK;
	  long lrw1Q[] = new long[1];
	  long liw1Q[] = new long[1];

	  /* Check cvode_mem */
	  if (cv_mem==null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeQuadInit",
	                   MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Set space requirements for one N_Vector */
	  N_VSpace_Serial(yQ0, lrw1Q, liw1Q);
	  cv_mem.cv_lrw1Q = lrw1Q[0];
	  cv_mem.cv_liw1Q = liw1Q[0];

	  /* Allocate the vectors (using yQ0 as a template) */
	  allocOK = cvQuadAllocVectors(cv_mem, yQ0);
	  if (!allocOK) {
	    cvProcessError(cv_mem, CV_MEM_FAIL, "CVODES",
	                   "CVodeQuadInit", MSGCV_MEM_FAIL);
	    return(CV_MEM_FAIL);
	  }

	  /* Initialize znQ[0] in the history array */
	  N_VScale_Serial(ONE, yQ0, cv_mem.cv_znQ[0]);

	  /* Copy the input parameters into CVODES state */
	  cv_mem.cv_fQ = fQ;

	  /* Initialize counters */
	  cv_mem.cv_nfQe  = 0;
	  cv_mem.cv_netfQ[0] = 0;

	  /* Quadrature integration turned ON */
	  cv_mem.cv_quadr = true;
	  cv_mem.cv_QuadMallocDone = true;

	  /* Quadrature initialization was successfull */
	  return(CV_SUCCESS);
	}

	/*
	 * CVodeQuadAllocVectors
	 *
	 * NOTE: Space for ewtQ is allocated even when errconQ=SUNFALSE, 
	 * although in this case, ewtQ is never used. The reason for this
	 * decision is to allow the user to re-initialize the quadrature
	 * computation with errconQ=SUNTRUE, after an initialization with
	 * errconQ=SUNFALSE, without new memory allocation within 
	 * CVodeQuadReInit.
	 */

	private boolean cvQuadAllocVectors(CVodeMemRec cv_mem, NVector tmpl) 
	{
	  int i, j;

	  /* Allocate ewtQ */
	  cv_mem.cv_ewtQ = N_VClone(tmpl);
	  if (cv_mem.cv_ewtQ == null) {
	    return(false);
	  }
	  
	  /* Allocate acorQ */
	  cv_mem.cv_acorQ = N_VClone(tmpl);
	  if (cv_mem.cv_acorQ == null) {
	    N_VDestroy(cv_mem.cv_ewtQ);
	    return(false);
	  }

	  /* Allocate yQ */
	  cv_mem.cv_yQ = N_VClone(tmpl);
	  if (cv_mem.cv_yQ == null) {
	    N_VDestroy(cv_mem.cv_ewtQ);
	    N_VDestroy(cv_mem.cv_acorQ);
	    return(false);
	  }

	  /* Allocate tempvQ */
	  cv_mem.cv_tempvQ = N_VClone(tmpl);
	  if (cv_mem.cv_tempvQ == null) {
	    N_VDestroy(cv_mem.cv_ewtQ);
	    N_VDestroy(cv_mem.cv_acorQ);
	    N_VDestroy(cv_mem.cv_yQ);
	    return(false);
	  }

	  /* Allocate zQn[0] ... zQn[maxord] */

	  for (j=0; j <= cv_mem.cv_qmax; j++) {
	    cv_mem.cv_znQ[j] = N_VClone(tmpl);
	    if (cv_mem.cv_znQ[j] == null) {
	      N_VDestroy(cv_mem.cv_ewtQ);
	      N_VDestroy(cv_mem.cv_acorQ);
	      N_VDestroy(cv_mem.cv_yQ);
	      N_VDestroy(cv_mem.cv_tempvQ);
	      for (i=0; i < j; i++) N_VDestroy(cv_mem.cv_znQ[i]);
	      return(false);
	    }
	  }

	  /* Store the value of qmax used here */
	  cv_mem.cv_qmax_allocQ = cv_mem.cv_qmax;

	  /* Update solver workspace lengths */
	  cv_mem.cv_lrw += (cv_mem.cv_qmax + 5)*cv_mem.cv_lrw1Q;
	  cv_mem.cv_liw += (cv_mem.cv_qmax + 5)*cv_mem.cv_liw1Q;

	  return(true);
	}

	private int CVodeQuadSStolerances(CVodeMemRec cv_mem, double reltolQ, double abstolQ)
	{

	  if (cv_mem==null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODES",
	                   "CVodeQuadSStolerances", MSGCV_NO_MEM);    
	    return(CV_MEM_NULL);
	  }

	  /* Ckeck if quadrature was initialized? */

	  if (cv_mem.cv_QuadMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_QUAD, "CVODES",
	                   "CVodeQuadSStolerances", MSGCV_NO_QUAD); 
	    return(CV_NO_QUAD);
	  }

	  /* Test user-supplied tolerances */

	  if (reltolQ < ZERO) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES",
	                   "CVodeQuadSStolerances", MSGCV_BAD_RELTOLQ);
	    return(CV_ILL_INPUT);
	  }

	  if (abstolQ < 0) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES",
	                   "CVodeQuadSStolerances", MSGCV_BAD_ABSTOLQ);
	    return(CV_ILL_INPUT);
	  }

	  /* Copy tolerances into memory */

	  cv_mem.cv_itolQ = CV_SS;

	  cv_mem.cv_reltolQ  = reltolQ;
	  cv_mem.cv_SabstolQ = abstolQ;

	  return(CV_SUCCESS);
	}
	
	/*
	 * CVodeAdjInit
	 *
	 * This routine initializes ASA and allocates space for the adjoint 
	 * memory structure.
	 */

	private int CVodeAdjInit(CVodeMemRec cv_mem, int steps, int interp)
	{
	  CVadjMemRec ca_mem;
	  int i, ii;

	  /* ---------------
	   * Check arguments
	   * --------------- */

	  if (cv_mem == null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeAdjInit", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  if (steps <= 0) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeAdjInit", MSGCV_BAD_STEPS);
	    return(CV_ILL_INPUT);
	  }

	  if ( (interp != CV_HERMITE) && (interp != CV_POLYNOMIAL) ) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeAdjInit", MSGCV_BAD_INTERP);
	    return(CV_ILL_INPUT);
	  } 

	  /* ----------------------------
	   * Allocate CVODEA memory block
	   * ---------------------------- */

	  ca_mem = null;
	  ca_mem = new CVadjMemRec();

	  /* Attach ca_mem to CVodeMem structure */

	  cv_mem.cv_adj_mem = ca_mem;

	  /* ------------------------------
	   * Initialization of check points
	   * ------------------------------ */

	  /* Set Check Points linked list to NULL */
	  ca_mem.ck_mem = null;

	  /* Initialize nckpnts to ZERO */
	  ca_mem.ca_nckpnts = 0;

	  /* No interpolation data is available */
	  ca_mem.ca_ckpntData = null;

	  /* ------------------------------------
	   * Initialization of interpolation data
	   * ------------------------------------ */

	  /* Interpolation type */

	  ca_mem.ca_IMtype = interp;

	  /* Number of steps between check points */

	  ca_mem.ca_nsteps = steps;

	  /* Allocate space for the array of Data Point structures */

	  ca_mem.dt_mem = null;
	  ca_mem.dt_mem = new DtpntMemRec[steps+1];
	  if (ca_mem.dt_mem == null) {
	    ca_mem = null;
	    cvProcessError(cv_mem, CV_MEM_FAIL, "CVODEA", "CVodeAdjInit", MSGCV_MEM_FAIL);
	    return(CV_MEM_FAIL);
	  }

	  for (i=0; i<=steps; i++) { 
	    ca_mem.dt_mem[i] = null;
	    ca_mem.dt_mem[i] = new DtpntMemRec();
	    if (ca_mem.dt_mem[i] == null) {
	      for(ii=0; ii<i; ii++) {ca_mem.dt_mem[ii] = null;}
	      ca_mem.dt_mem = null;
	      ca_mem = null;
	      cvProcessError(cv_mem, CV_MEM_FAIL, "CVODEA", "CVodeAdjInit", MSGCV_MEM_FAIL);
	      return(CV_MEM_FAIL);
	    }
	  }

	  /* Attach functions for the appropriate interpolation module */
	  
	  switch(interp) {

	  case CV_HERMITE:
	    
	    ca_mem.ca_IMmalloc = CVAhermiteMalloc_select;
	    //ca_mem.ca_IMfree   = CVAhermiteFree;
	    ca_mem.ca_IMget    = CVAhermiteGetY_select;
	    ca_mem.ca_IMstore  = CVAhermiteStorePnt_select;

	    break;
	    
	  case CV_POLYNOMIAL:
	  
	    ca_mem.ca_IMmalloc = CVApolynomialMalloc_select;
	    //ca_mem.ca_IMfree   = CVApolynomialFree;
	    ca_mem.ca_IMget    = CVApolynomialGetY_select;
	    ca_mem.ca_IMstore  = CVApolynomialStorePnt_select;

	    break;

	  }

	  /* The interpolation module has not been initialized yet */

	  ca_mem.ca_IMmallocDone = false;

	  /* By default we will store but not interpolate sensitivities
	   *  - IMstoreSensi will be set in CVodeF to SUNFALSE if FSA is not enabled
	   *    or if the user can force this through CVodeSetAdjNoSensi 
	   *  - IMinterpSensi will be set in CVodeB to SUNTRUE if IMstoreSensi is
	   *    SUNTRUE and if at least one backward problem requires sensitivities */

	  ca_mem.ca_IMstoreSensi = true;
	  ca_mem.ca_IMinterpSensi = false;

	  /* ------------------------------------
	   * Initialize list of backward problems
	   * ------------------------------------ */

	  ca_mem.cvB_mem = null;
	  ca_mem.ca_bckpbCrt = null;
	  ca_mem.ca_nbckpbs = 0;

	  /* --------------------------------
	   * CVodeF and CVodeB not called yet
	   * -------------------------------- */

	  ca_mem.ca_firstCVodeFcall = true;
	  ca_mem.ca_tstopCVodeFcall = false;

	  ca_mem.ca_firstCVodeBcall = true;

	  /* ---------------------------------------------
	   * ASA initialized and allocated
	   * --------------------------------------------- */

	  cv_mem.cv_adj = true;
	  cv_mem.cv_adjMallocDone = true;

	  return(CV_SUCCESS);
	} 

	
		/*
		 * CVodeF
		 *
		 * This routine integrates to tout and returns solution into yout.
		 * In the same time, it stores check point data every 'steps' steps. 
		 * 
		 * CVodeF can be called repeatedly by the user.
		 *
		 * ncheckPtr points to the number of check points stored so far.
		 */

		private int CVodeF(CVodeMemRec cv_mem, double tout, NVector yout, 
		           double tret[], int itask, int ncheckPtr[])
		{
		  CVadjMemRec ca_mem;
		  CkpntMemRec tmp;
		  DtpntMemRec dt_mem[];
		  int flag, i;
		  boolean iret, allocOK;

		  /* Check if cvode_mem exists */
		  if (cv_mem == null) {
		    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeF", MSGCV_NO_MEM);
		    return(CV_MEM_NULL);
		  }

		  /* Was ASA initialized? */
		  if (cv_mem.cv_adjMallocDone == false) {
		    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeF", MSGCV_NO_ADJ);
		    return(CV_NO_ADJ);
		  } 

		  ca_mem = cv_mem.cv_adj_mem;

		  /* Check for yout != NULL */
		  if (yout == null) {
		    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeF", MSGCV_YOUT_NULL);
		    return(CV_ILL_INPUT);
		  }
		  
		  /* Check for tret != NULL */
		  if (tret == null) {
		    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeF", MSGCV_TRET_NULL);
		    return(CV_ILL_INPUT);
		  }

		  /* Check for valid itask */
		  if ( (itask != CV_NORMAL) && (itask != CV_ONE_STEP) ) {
		    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeF", MSGCV_BAD_ITASK);
		    return(CV_ILL_INPUT);
		  }

		  /* All error checking done */

		  dt_mem = ca_mem.dt_mem;

		  /* If tstop is enabled, store some info */
		  if (cv_mem.cv_tstopset) {
		    ca_mem.ca_tstopCVodeFcall = true;
		    ca_mem.ca_tstopCVodeF = cv_mem.cv_tstop;
		  }

		  /* We will call CVode in CV_ONE_STEP mode, regardless
		   * of what itask is, so flag if we need to return */
		  if (itask == CV_ONE_STEP) iret = true;
		  else                      iret = false;

		  /* On the first step:
		   *   - set tinitial
		   *   - initialize list of check points
		   *   - if needed, initialize the interpolation module
		   *   - load dt_mem[0]
		   * On subsequent steps, test if taking a new step is necessary. 
		   */
		  if ( ca_mem.ca_firstCVodeFcall ) {

		    ca_mem.ca_tinitial = cv_mem.cv_tn;

		    ca_mem.ck_mem = CVAckpntInit(cv_mem);
		    if (ca_mem.ck_mem == null) {
		      cvProcessError(cv_mem, CV_MEM_FAIL, "CVODEA", "CVodeF", MSGCV_MEM_FAIL);
		      return(CV_MEM_FAIL);
		    }

		    if ( !ca_mem.ca_IMmallocDone ) {

		      /* Do we need to store sensitivities? */
		      if (!cv_mem.cv_sensi) ca_mem.ca_IMstoreSensi = false;

		      /* Allocate space for interpolation data */
		      if (ca_mem.ca_IMmalloc == CVAhermiteMalloc_select) {
		          allocOK = CVAhermiteMalloc(cv_mem);
		      }
		      else {
		    	  allocOK = CVApolynomialMalloc(cv_mem);
		      }
		      if (!allocOK) {
		        cvProcessError(cv_mem, CV_MEM_FAIL, "CVODEA", "CVodeF", MSGCV_MEM_FAIL);
		        return(CV_MEM_FAIL);
		      }

		      /* Rename zn and, if needed, znS for use in interpolation */
		      for (i=0;i<L_MAX;i++) ca_mem.ca_Y[i] = cv_mem.cv_zn[i];
		      if (ca_mem.ca_IMstoreSensi) {
		        for (i=0;i<L_MAX;i++) ca_mem.ca_YS[i] = cv_mem.cv_znS[i];
		      }

		      ca_mem.ca_IMmallocDone = true;

		    }

		    dt_mem[0].t = ca_mem.ck_mem.ck_t0;
		    if (ca_mem.ca_IMstore == CVAhermiteStorePnt_select) {
		        CVAhermiteStorePnt(cv_mem, dt_mem[0]);
		    }
		    else {
		    	CVApolynomialStorePnt(cv_mem, dt_mem[0]);	
		    }

		    ca_mem.ca_firstCVodeFcall = false;

		  } else if ( (cv_mem.cv_tn - tout)*cv_mem.cv_h >= ZERO ) {

		    /* If tout was passed, return interpolated solution. 
		       No changes to ck_mem or dt_mem are needed. */
		    tret[0] = tout;
		    flag = CVodeGetDky(cv_mem, tout, 0, yout);
		    ncheckPtr[0] = ca_mem.ca_nckpnts;
		    ca_mem.ca_IMnewData = true;
		    ca_mem.ca_ckpntData = ca_mem.ck_mem;
		    ca_mem.ca_np = cv_mem.cv_nst % ca_mem.ca_nsteps + 1;

		    return(flag);

		  }

		  /* Integrate to tout (in CV_ONE_STEP mode) while loading check points */
		  for(;;) {

		    /* Perform one step of the integration */

		    flag = CVode(cv_mem, tout, yout, tret, CV_ONE_STEP);
		    if (flag < 0) break;

		    /* Test if a new check point is needed */

		    if ( cv_mem.cv_nst % ca_mem.ca_nsteps == 0 ) {

		      ca_mem.ck_mem.ck_t1 = tret[0];

		      /* Create a new check point, load it, and append it to the list */
		      tmp = CVAckpntNew(cv_mem);
		      if (tmp == null) {
		        cvProcessError(cv_mem, CV_MEM_FAIL, "CVODEA", "CVodeF", MSGCV_MEM_FAIL);
		        flag = CV_MEM_FAIL;
		        break;
		      }
		      tmp.ck_next = ca_mem.ck_mem;
		      ca_mem.ck_mem = tmp;
		      ca_mem.ca_nckpnts++;
		      cv_mem.cv_forceSetup = true;
		      
		      /* Reset i=0 and load dt_mem[0] */
		      dt_mem[0].t = ca_mem.ck_mem.ck_t0;
		      if (ca_mem.ca_IMstore == CVAhermiteStorePnt_select) {
		          CVAhermiteStorePnt(cv_mem, dt_mem[0]);
		      }
		      else {
		    	  CVApolynomialStorePnt(cv_mem, dt_mem[0]);	
		      }

		    } else {

		      /* Load next point in dt_mem */
		      dt_mem[(int)(cv_mem.cv_nst % ca_mem.ca_nsteps)].t = tret[0];
		      if (ca_mem.ca_IMstore == CVAhermiteStorePnt_select) {
		          CVAhermiteStorePnt(cv_mem, dt_mem[(int)(cv_mem.cv_nst % ca_mem.ca_nsteps)]);
		      }
		      else {
		    	  CVApolynomialStorePnt(cv_mem, dt_mem[(int)(cv_mem.cv_nst % ca_mem.ca_nsteps)]);  
		      }

		    }

		    /* Set t1 field of the current ckeck point structure
		       for the case in which there will be no future
		       check points */
		    ca_mem.ck_mem.ck_t1 = tret[0];

		    /* tfinal is now set to *tret */
		    ca_mem.ca_tfinal = tret[0];

		    /* Return if in CV_ONE_STEP mode */
		    if (iret) break;

		    /* Return if tout reached */
		    if ( (tret[0] - tout)*cv_mem.cv_h >= ZERO ) {
		      tret[0] = tout;
		      CVodeGetDky(cv_mem, tout, 0, yout);
		      /* Reset tretlast in cv_mem so that CVodeGetQuad and CVodeGetSens 
		       * evaluate quadratures and/or sensitivities at the proper time */
		      cv_mem.cv_tretlast = tout;
		      break;
		    }

		  } /* end of for(;;)() */

		  /* Get ncheck from ca_mem */ 
		  ncheckPtr[0] = ca_mem.ca_nckpnts;

		  /* Data is available for the last interval */
		  ca_mem.ca_IMnewData = true;
		  ca_mem.ca_ckpntData = ca_mem.ck_mem;
		  ca_mem.ca_np = cv_mem.cv_nst % ca_mem.ca_nsteps + 1;

		  return(flag);
		}

		/*
		 * CVAckpntInit
		 *
		 * This routine initializes the check point linked list with 
		 * information from the initial time.
		 */

		private CkpntMemRec CVAckpntInit(CVodeMemRec cv_mem)
		{
		  CkpntMemRec ck_mem;
		  int is;

		  /* Allocate space for ckdata */
		  ck_mem = null;
		  ck_mem = new CkpntMemRec();

		  ck_mem.ck_zn[0] = N_VClone_Serial(cv_mem.cv_tempv);
		  if (ck_mem.ck_zn[0] == null) {
		    ck_mem = null;
		    return(null);
		  }
		  
		  ck_mem.ck_zn[1] = N_VClone_Serial(cv_mem.cv_tempv);
		  if (ck_mem.ck_zn[1] == null) {
		    N_VDestroy(ck_mem.ck_zn[0]);
		    ck_mem = null;
		    return(null);
		  }

		  /* ck_mem->ck_zn[qmax] was not allocated */
		  ck_mem.ck_zqm = 0;

		  /* Load ckdata from cv_mem */
		  N_VScale_Serial(ONE, cv_mem.cv_zn[0], ck_mem.ck_zn[0]);
		  ck_mem.ck_t0    = cv_mem.cv_tn;
		  ck_mem.ck_nst   = 0;
		  ck_mem.ck_q     = 1;
		  ck_mem.ck_h     = 0.0;
		  
		  /* Do we need to carry quadratures */
		  ck_mem.ck_quadr = cv_mem.cv_quadr && cv_mem.cv_errconQ;

		  if (ck_mem.ck_quadr) {

		    ck_mem.ck_znQ[0] = N_VClone_Serial(cv_mem.cv_tempvQ);
		    if (ck_mem.ck_znQ[0] == null) {
		      N_VDestroy(ck_mem.ck_zn[0]);
		      N_VDestroy(ck_mem.ck_zn[1]);
		      ck_mem = null;
		      return(null);
		    }

		    N_VScale_Serial(ONE, cv_mem.cv_znQ[0], ck_mem.ck_znQ[0]);

		  }

		  /* Do we need to carry sensitivities? */
		  ck_mem.ck_sensi = cv_mem.cv_sensi;

		  if (ck_mem.ck_sensi) {

		    ck_mem.ck_Ns = cv_mem.cv_Ns;

		    ck_mem.ck_znS[0] = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempv);
		    if (ck_mem.ck_znS[0] == null) {
		      N_VDestroy(ck_mem.ck_zn[0]);
		      N_VDestroy(ck_mem.ck_zn[1]);
		      if (ck_mem.ck_quadr) N_VDestroy(ck_mem.ck_znQ[0]);
		      ck_mem = null;
		      return(null);
		    }

		    for (is=0; is<cv_mem.cv_Ns; is++)
		      N_VScale_Serial(ONE, cv_mem.cv_znS[0][is], ck_mem.ck_znS[0][is]);

		  }

		  /* Do we need to carry quadrature sensitivities? */
		  ck_mem.ck_quadr_sensi = cv_mem.cv_quadr_sensi && cv_mem.cv_errconQS;

		  if (ck_mem.ck_quadr_sensi) {
		    ck_mem.ck_znQS[0] = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempvQ);
		    if (ck_mem.ck_znQS[0] == null) {
		      N_VDestroy(ck_mem.ck_zn[0]);
		      N_VDestroy(ck_mem.ck_zn[1]);
		      if (ck_mem.ck_quadr) N_VDestroy(ck_mem.ck_znQ[0]);
		      N_VDestroyVectorArray_Serial(ck_mem.ck_znS[0], cv_mem.cv_Ns);
		      ck_mem = null;
		      return(null);
		    }
		    
		    for (is=0; is<cv_mem.cv_Ns; is++)
		      N_VScale_Serial(ONE, cv_mem.cv_znQS[0][is], ck_mem.ck_znQS[0][is]);

		  }

		  /* Next in list */
		  ck_mem.ck_next  = null;

		  return(ck_mem);
		}
		
		/*
		 * CVAhermiteMalloc
		 *
		 * This routine allocates memory for storing information at all
		 * intermediate points between two consecutive check points. 
		 * This data is then used to interpolate the forward solution 
		 * at any other time.
		 */

		private boolean CVAhermiteMalloc(CVodeMemRec cv_mem)
		{
		  CVadjMemRec ca_mem;
		  DtpntMemRec dt_mem[];
		  HermiteDataMemRec content;
		  int i, ii=0;
		  boolean allocOK;

		  allocOK = true;

		  ca_mem = cv_mem.cv_adj_mem;

		  /* Allocate space for the vectors ytmp and yStmp */

		  ca_mem.ca_ytmp = N_VClone(cv_mem.cv_tempv);
		  if (ca_mem.ca_ytmp == null) {
		    return(false);
		  }

		  if (ca_mem.ca_IMstoreSensi) {
		    ca_mem.ca_yStmp = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempv);
		    if (ca_mem.ca_yStmp == null) {
		      N_VDestroy(ca_mem.ca_ytmp);
		      return(false);
		    }
		  }

		  /* Allocate space for the content field of the dt structures */

		  dt_mem = ca_mem.dt_mem;

		  for (i=0; i<=ca_mem.ca_nsteps; i++) {

		    content = null;
		    content = new HermiteDataMemRec();

		    content.y = N_VClone(cv_mem.cv_tempv);
		    if (content.y == null) {
		      content = null;
		      ii = i;
		      allocOK = false;
		      break;
		    }

		    content.yd = N_VClone(cv_mem.cv_tempv);
		    if (content.yd == null) {
		      N_VDestroy(content.y);
		      content = null;
		      ii = i;
		      allocOK = false;
		      break;
		    }

		    if (ca_mem.ca_IMstoreSensi) {

		      content.yS = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempv);
		      if (content.yS == null) {
		        N_VDestroy(content.y);
		        N_VDestroy(content.yd);
		        content = null;
		        ii = i;
		        allocOK = false;
		        break;
		      }

		      content.ySd = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempv);
		      if (content.ySd == null) {
		        N_VDestroy(content.y);
		        N_VDestroy(content.yd);
		        N_VDestroyVectorArray_Serial(content.yS, cv_mem.cv_Ns);
		        content = null;
		        ii = i;
		        allocOK = false;
		        break;
		      }
		      
		    }
		    
		    dt_mem[i].content = CV_HERMITE;
		    dt_mem[i].hermiteContent = content;

		  } 

		  /* If an error occurred, deallocate and return */

		  if (!allocOK) {

		    N_VDestroy(ca_mem.ca_ytmp);

		    if (ca_mem.ca_IMstoreSensi) {
		      N_VDestroyVectorArray_Serial(ca_mem.ca_yStmp, cv_mem.cv_Ns);
		    }

		    for (i=0; i<ii; i++) {
		      content = (dt_mem[i].hermiteContent);
		      N_VDestroy(content.y);
		      N_VDestroy(content.yd);
		      if (ca_mem.ca_IMstoreSensi) {
		        N_VDestroyVectorArray_Serial(content.yS, cv_mem.cv_Ns);
		        N_VDestroyVectorArray_Serial(content.ySd, cv_mem.cv_Ns);
		      }
		      dt_mem[i].hermiteContent = null;
		    }

		  }

		  return(allocOK);
		}

		
		/*
		 * CVApolynomialMalloc
		 *
		 * This routine allocates memory for storing information at all
		 * intermediate points between two consecutive check points. 
		 * This data is then used to interpolate the forward solution 
		 * at any other time.
		 */

		private boolean CVApolynomialMalloc(CVodeMemRec cv_mem)
		{
		  CVadjMemRec ca_mem;
		  DtpntMemRec dt_mem[];
		  PolynomialDataMemRec content;
		  int i, ii=0;
		  boolean allocOK;

		  allocOK = true;

		  ca_mem = cv_mem.cv_adj_mem;

		  /* Allocate space for the vectors ytmp and yStmp */

		  ca_mem.ca_ytmp = N_VClone(cv_mem.cv_tempv);
		  if (ca_mem.ca_ytmp == null) {
		    return(false);
		  }

		  if (ca_mem.ca_IMstoreSensi) {
		    ca_mem.ca_yStmp = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempv);
		    if (ca_mem.ca_yStmp == null) {
		      N_VDestroy(ca_mem.ca_ytmp);
		      return(false);
		    }
		  }

		  /* Allocate space for the content field of the dt structures */

		  dt_mem = ca_mem.dt_mem;

		  for (i=0; i<=ca_mem.ca_nsteps; i++) {

		    content = null;
		    content = new PolynomialDataMemRec();

		    content.y = N_VClone(cv_mem.cv_tempv);
		    if (content.y == null) {
		      content = null;
		      ii = i;
		      allocOK = false;
		      break;
		    }

		    if (ca_mem.ca_IMstoreSensi) {

		      content.yS = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempv);
		      if (content.yS == null) {
		        N_VDestroy(content.y);
		        content = null;
		        ii = i;
		        allocOK = false;
		        break;
		      }

		    }

		    dt_mem[i].content = CV_POLYNOMIAL;
		    dt_mem[i].polynomialContent = content;

		  } 

		  /* If an error occurred, deallocate and return */

		  if (!allocOK) {

		    N_VDestroy(ca_mem.ca_ytmp);

		    if (ca_mem.ca_IMstoreSensi) {
		      N_VDestroyVectorArray_Serial(ca_mem.ca_yStmp, cv_mem.cv_Ns);
		    }

		    for (i=0; i<ii; i++) {
		      content = (dt_mem[i].polynomialContent);
		      N_VDestroy(content.y);
		      if (ca_mem.ca_IMstoreSensi) {
		        N_VDestroyVectorArray_Serial(content.yS, cv_mem.cv_Ns);
		      }
		      dt_mem[i].polynomialContent = null;
		    }

		  }

		  return(allocOK);

		}
		
		/*
		 * CVAhermiteStorePnt ( -> IMstore )
		 *
		 * This routine stores a new point (y,yd) in the structure d for use
		 * in the cubic Hermite interpolation.
		 * Note that the time is already stored.
		 */

		private int CVAhermiteStorePnt(CVodeMemRec cv_mem, DtpntMemRec d)
		{
		  CVadjMemRec ca_mem;
		  HermiteDataMemRec content;
		  int is;
		  /* int retval; */

		  ca_mem = cv_mem.cv_adj_mem;

		  content = d.hermiteContent;

		  /* Load solution */

		  N_VScale_Serial(ONE, cv_mem.cv_zn[0], content.y);
		  
		  if (ca_mem.ca_IMstoreSensi) {
		    for (is=0; is<cv_mem.cv_Ns; is++) 
		      N_VScale_Serial(ONE, cv_mem.cv_znS[0][is], content.yS[is]);
		  }

		  /* Load derivative */

		  if (cv_mem.cv_nst == 0) {

		    /**  retval = */ 
			  if (testMode) {
				  fTestMode(cv_mem.cv_tn, content.y, content.yd, cv_mem.cv_user_data);
			  }
			  else {
			      f(cv_mem.cv_tn, content.y, content.yd, cv_mem.cv_user_data);
			  }

		    if (ca_mem.ca_IMstoreSensi) {
		      /* retval = */ cvSensRhsWrapper(cv_mem, cv_mem.cv_tn, content.y, content.yd,
		                                content.yS, content.ySd,
		                                cv_mem.cv_tempv, cv_mem.cv_ftemp);
		    }

		  } else {

		    N_VScale_Serial(ONE/cv_mem.cv_h, cv_mem.cv_zn[1], content.yd);

		    if (ca_mem.ca_IMstoreSensi) {
		      for (is=0; is<cv_mem.cv_Ns; is++) 
		        N_VScale_Serial(ONE/cv_mem.cv_h, cv_mem.cv_znS[1][is], content.ySd[is]);
		    }

		  }

		  return(0);
		}

		/*
		 * CVApolynomialStorePnt ( -> IMstore )
		 *
		 * This routine stores a new point y in the structure d for use
		 * in the Polynomial interpolation.
		 * Note that the time is already stored.
		 */

		private int CVApolynomialStorePnt(CVodeMemRec cv_mem, DtpntMemRec d)
		{
		  CVadjMemRec ca_mem;
		  PolynomialDataMemRec content;
		  int is;

		  ca_mem = cv_mem.cv_adj_mem;

		  content = d.polynomialContent;

		  N_VScale_Serial(ONE, cv_mem.cv_zn[0], content.y);

		  if (ca_mem.ca_IMstoreSensi) {
		    for (is=0; is<cv_mem.cv_Ns; is++) 
		      N_VScale_Serial(ONE, cv_mem.cv_znS[0][is], content.yS[is]);
		  }

		  content.order = cv_mem.cv_qu;

		  return(0);
		}
		
		/*
		 * CVAckpntNew
		 *
		 * This routine allocates space for a new check point and sets 
		 * its data from current values in cv_mem.
		 */

		private CkpntMemRec CVAckpntNew(CVodeMemRec cv_mem)
		{
		  CkpntMemRec ck_mem;
		  int j, jj, is, qmax; 

		  /* Allocate space for ckdata */
		  ck_mem = null;
		  ck_mem = new CkpntMemRec();

		  /* Set cv_next to NULL */
		  ck_mem.ck_next = null;

		  /* Test if we need to allocate space for the last zn.
		   * NOTE: zn(qmax) may be needed for a hot restart, if an order
		   * increase is deemed necessary at the first step after a check point */
		  qmax = cv_mem.cv_qmax;
		  ck_mem.ck_zqm = (cv_mem.cv_q < qmax) ? qmax : 0;

		  for (j=0; j<=cv_mem.cv_q; j++) {
		    ck_mem.ck_zn[j] = N_VClone(cv_mem.cv_tempv);
		    if (ck_mem.ck_zn[j] == null) {
		      for (jj=0; jj<j; jj++) N_VDestroy(ck_mem.ck_zn[jj]);
		      ck_mem = null;
		      return(null);
		    }
		  }

		  if (cv_mem.cv_q < qmax) {
		    ck_mem.ck_zn[qmax] = N_VClone(cv_mem.cv_tempv);
		    if (ck_mem.ck_zn[qmax] == null) {
		      for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_zn[jj]);
		      ck_mem = null;
		      return(null);
		    }
		  }

		  /* Test if we need to carry quadratures */
		  ck_mem.ck_quadr = cv_mem.cv_quadr && cv_mem.cv_errconQ;

		  if (ck_mem.ck_quadr) {

		    for (j=0; j<=cv_mem.cv_q; j++) {
		      ck_mem.ck_znQ[j] = N_VClone(cv_mem.cv_tempvQ);
		      if(ck_mem.ck_znQ[j] == null) {
		        for (jj=0; jj<j; jj++) N_VDestroy(ck_mem.ck_znQ[jj]);
		        if (cv_mem.cv_q < qmax) N_VDestroy(ck_mem.ck_zn[qmax]);
		        for (jj=0; jj<=cv_mem.cv_q; j++) N_VDestroy(ck_mem.ck_zn[jj]);
		        ck_mem = null;
		        return(null);
		      }
		    }

		    if (cv_mem.cv_q < qmax) {
		      ck_mem.ck_znQ[qmax] = N_VClone(cv_mem.cv_tempvQ);
		      if (ck_mem.ck_znQ[qmax] == null) {
		        for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_znQ[jj]);
		        N_VDestroy(ck_mem.ck_zn[qmax]);
		        for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_zn[jj]);
		        ck_mem = null;
		        return(null);
		      }
		    }

		  }

		  /* Test if we need to carry sensitivities */
		  ck_mem.ck_sensi = cv_mem.cv_sensi;

		  if (ck_mem.ck_sensi) {

		    ck_mem.ck_Ns = cv_mem.cv_Ns;

		    for (j=0; j<=cv_mem.cv_q; j++) {
		      ck_mem.ck_znS[j] = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempv);
		      if (ck_mem.ck_znS[j] == null) {
		        for (jj=0; jj<j; jj++) N_VDestroyVectorArray_Serial(ck_mem.ck_znS[jj], cv_mem.cv_Ns);
		        if (ck_mem.ck_quadr) {
		          if (cv_mem.cv_q < qmax) N_VDestroy(ck_mem.ck_znQ[qmax]);
		          for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_znQ[jj]);
		        }
		        if (cv_mem.cv_q < qmax) N_VDestroy(ck_mem.ck_zn[qmax]);
		        for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_zn[jj]);
		        ck_mem = null;
		        return(null);
		      }
		    }

		    if ( cv_mem.cv_q < qmax) {
		      ck_mem.ck_znS[qmax] = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempv);
		      if (ck_mem.ck_znS[qmax] == null) {
		        for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroyVectorArray_Serial(ck_mem.ck_znS[jj], cv_mem.cv_Ns);
		        if (ck_mem.ck_quadr) {
		          N_VDestroy(ck_mem.ck_znQ[qmax]);
		          for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_znQ[jj]);
		        }
		        N_VDestroy(ck_mem.ck_zn[qmax]);
		        for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_zn[jj]);
		        ck_mem = null;
		        return(null);
		      }
		    }

		  }

		  /* Test if we need to carry quadrature sensitivities */
		  ck_mem.ck_quadr_sensi = cv_mem.cv_quadr_sensi && cv_mem.cv_errconQS;

		  if (ck_mem.ck_quadr_sensi) {

		    for (j=0; j<=cv_mem.cv_q; j++) {
		      ck_mem.ck_znQS[j] = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempvQ);
		      if (ck_mem.ck_znQS[j] == null) {
		        for (jj=0; jj<j; jj++) N_VDestroyVectorArray_Serial(ck_mem.ck_znQS[jj], cv_mem.cv_Ns);
		        if (cv_mem.cv_q < qmax) N_VDestroyVectorArray_Serial(ck_mem.ck_znS[qmax], cv_mem.cv_Ns);
		        for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroyVectorArray_Serial(ck_mem.ck_znS[jj], cv_mem.cv_Ns);
		        if (ck_mem.ck_quadr) {
		          if (cv_mem.cv_q < qmax) N_VDestroy(ck_mem.ck_znQ[qmax]);
		          for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_znQ[jj]);
		        }
		        if (cv_mem.cv_q < qmax) N_VDestroy(ck_mem.ck_zn[qmax]);
		        for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_zn[jj]);
		        ck_mem = null;
		        return(null);
		      }
		    }

		    if ( cv_mem.cv_q < qmax) {
		      ck_mem.ck_znQS[qmax] = N_VCloneVectorArray_Serial(cv_mem.cv_Ns, cv_mem.cv_tempvQ);
		      if (ck_mem.ck_znQS[qmax] == null) {
		        for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroyVectorArray_Serial(ck_mem.ck_znQS[jj], cv_mem.cv_Ns);
		        N_VDestroyVectorArray_Serial(ck_mem.ck_znS[qmax], cv_mem.cv_Ns);
		        for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroyVectorArray_Serial(ck_mem.ck_znS[jj], cv_mem.cv_Ns);
		        if (ck_mem.ck_quadr) {
		          N_VDestroy(ck_mem.ck_znQ[qmax]);
		          for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_zn[jj]);
		        }
		        N_VDestroy(ck_mem.ck_zn[qmax]);
		        for (jj=0; jj<=cv_mem.cv_q; jj++) N_VDestroy(ck_mem.ck_zn[jj]);
		        ck_mem = null;
		        return(null);
		      }
		    }

		  }

		  /* Load check point data from cv_mem */

		  for (j=0; j<=cv_mem.cv_q; j++) N_VScale_Serial(ONE, cv_mem.cv_zn[j], ck_mem.ck_zn[j]);
		  if ( cv_mem.cv_q < qmax ) N_VScale_Serial(ONE, cv_mem.cv_zn[qmax], ck_mem.ck_zn[qmax]);

		  if (ck_mem.ck_quadr) {
		    for (j=0; j<=cv_mem.cv_q; j++) N_VScale_Serial(ONE, cv_mem.cv_znQ[j], ck_mem.ck_znQ[j]);
		    if ( cv_mem.cv_q < qmax ) N_VScale_Serial(ONE, cv_mem.cv_znQ[qmax], ck_mem.ck_znQ[qmax]);
		  }

		  if (ck_mem.ck_sensi) {
		    for (is=0; is<cv_mem.cv_Ns; is++) {
		      for (j=0; j<=cv_mem.cv_q; j++) N_VScale_Serial(ONE, cv_mem.cv_znS[j][is], ck_mem.ck_znS[j][is]);
		      if ( cv_mem.cv_q < qmax ) N_VScale_Serial(ONE, cv_mem.cv_znS[qmax][is], ck_mem.ck_znS[qmax][is]);
		    }
		  }

		  if (ck_mem.ck_quadr_sensi) {
		    for (is=0; is<cv_mem.cv_Ns; is++) {
		      for (j=0; j<=cv_mem.cv_q; j++) N_VScale_Serial(ONE, cv_mem.cv_znQS[j][is], ck_mem.ck_znQS[j][is]);
		      if ( cv_mem.cv_q < qmax ) N_VScale_Serial(ONE, cv_mem.cv_znQS[qmax][is], ck_mem.ck_znQS[qmax][is]);
		    }
		  }

		  for (j=0; j<=L_MAX; j++)        ck_mem.ck_tau[j] = cv_mem.cv_tau[j];
		  for (j=0; j<=NUM_TESTS; j++)    ck_mem.ck_tq[j] = cv_mem.cv_tq[j];
		  for (j=0; j<=cv_mem.cv_q; j++) ck_mem.ck_l[j] = cv_mem.cv_l[j];
		  ck_mem.ck_nst       = cv_mem.cv_nst;
		  ck_mem.ck_tretlast  = cv_mem.cv_tretlast;
		  ck_mem.ck_q         = cv_mem.cv_q;
		  ck_mem.ck_qprime    = cv_mem.cv_qprime;
		  ck_mem.ck_qwait     = cv_mem.cv_qwait;
		  ck_mem.ck_L         = cv_mem.cv_L;
		  ck_mem.ck_gammap    = cv_mem.cv_gammap;
		  ck_mem.ck_h         = cv_mem.cv_h;
		  ck_mem.ck_hprime    = cv_mem.cv_hprime;
		  ck_mem.ck_hscale    = cv_mem.cv_hscale;
		  ck_mem.ck_eta       = cv_mem.cv_eta;
		  ck_mem.ck_etamax    = cv_mem.cv_etamax;
		  ck_mem.ck_t0        = cv_mem.cv_tn;
		  ck_mem.ck_saved_tq5 = cv_mem.cv_saved_tq5;

		  return(ck_mem);
		}
		
		/* 
		 * CVodeGetQuad
		 *
		 * This routine extracts quadrature solution into yQout at the
		 * time which CVode returned the solution.
		 * This is just a wrapper that calls CVodeGetQuadDky with k=0.
		 */
		 
		private int CVodeGetQuad(CVodeMemRec cv_mem, double tret[], NVector yQout)
		{
		  int flag;

		  if (cv_mem == null) {
		    cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeGetQuad", MSGCV_NO_MEM);
		    return(CV_MEM_NULL);
		  } 

		  tret[0] = cv_mem.cv_tretlast;
		  
		  flag = CVodeGetQuadDky(cv_mem,cv_mem.cv_tretlast,0,yQout);

		  return(flag);
		}
		
		/*
		 * CVodeGetQuadDky
		 *
		 * CVodeQuadDky computes the kth derivative of the yQ function at
		 * time t, where tn-hu <= t <= tn, tn denotes the current         
		 * internal time reached, and hu is the last internal step size   
		 * successfully used by the solver. The user may request 
		 * k=0, 1, ..., qu, where qu is the current order. 
		 * The derivative vector is returned in dky. This vector 
		 * must be allocated by the caller. It is only legal to call this         
		 * function after a successful return from CVode with quadrature
		 * computation enabled.
		 */

		private int CVodeGetQuadDky(CVodeMemRec cv_mem, double t, int k, NVector dkyQ)
		{ 
		  double s, c, r;
		  double tfuzz, tp, tn1;
		  int i, j;
		  
		  /* Check all inputs for legality */
		  
		  if (cv_mem == null) {
		    cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeGetQuadDky", MSGCV_NO_MEM);
		    return(CV_MEM_NULL);
		  } 

		  if(cv_mem.cv_quadr != true) {
		    cvProcessError(cv_mem, CV_NO_QUAD, "CVODES", "CVodeGetQuadDky", MSGCV_NO_QUAD);
		    return(CV_NO_QUAD);
		  }

		  if (dkyQ == null) {
		    cvProcessError(cv_mem, CV_BAD_DKY, "CVODES", "CVodeGetQuadDky", MSGCV_NULL_DKY);
		    return(CV_BAD_DKY);
		  }
		  
		  if ((k < 0) || (k > cv_mem.cv_q)) {
		    cvProcessError(cv_mem, CV_BAD_K, "CVODES", "CVodeGetQuadDky", MSGCV_BAD_K);
		    return(CV_BAD_K);
		  }
		  
		  /* Allow for some slack */
		  tfuzz = FUZZ_FACTOR * cv_mem.cv_uround *
		    (Math.abs(cv_mem.cv_tn) + Math.abs(cv_mem.cv_hu));
		  if (cv_mem.cv_hu < ZERO) tfuzz = -tfuzz;
		  tp = cv_mem.cv_tn - cv_mem.cv_hu - tfuzz;
		  tn1 = cv_mem.cv_tn + tfuzz;
		  if ((t-tp)*(t-tn1) > ZERO) {
		    cvProcessError(cv_mem, CV_BAD_T, "CVODES", "CVodeGetQuadDky", MSGCV_BAD_T);
		    return(CV_BAD_T);
		  }
		  
		  /* Sum the differentiated interpolating polynomial */
		  
		  s = (t - cv_mem.cv_tn) / cv_mem.cv_h;
		  for (j=cv_mem.cv_q; j >= k; j--) {
		    c = ONE;
		    for (i=j; i >= j-k+1; i--) c *= i;
		    if (j == cv_mem.cv_q) {
		      N_VScale_Serial(c, cv_mem.cv_znQ[cv_mem.cv_q], dkyQ);
		    } else {
		      N_VLinearSum_Serial(c, cv_mem.cv_znQ[j], s, dkyQ, dkyQ);
		    }
		  }
		  if (k == 0) return(CV_SUCCESS);
		  r = Math.pow(cv_mem.cv_h, -k);
		  N_VScale_Serial(r, dkyQ, dkyQ);
		  return(CV_SUCCESS);
		  
		}

		class CVadjCheckPointRec {
			  CkpntMemRec my_addr;
			  CkpntMemRec next_addr;
			  double t0;
			  double t1;
			  long nstep;
			  int order;
			  double step;
			};

			/*
			 * CVodeGetAdjCheckPointsInfo
			 *
			 * This routine loads an array of nckpnts structures of type CVadjCheckPointRec.
			 * The user must allocate space for ckpnt.
			 */

			private int CVodeGetAdjCheckPointsInfo(CVodeMemRec cv_mem, CVadjCheckPointRec ckpnt[])
			{
			  CVadjMemRec ca_mem;
			  CkpntMemRec ck_mem;
			  int i;

			  /* Check if cvode_mem exists */
			  if (cv_mem == null) {
			    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeGetAdjCheckPointsInfo", MSGCV_NO_MEM);
			    return(CV_MEM_NULL);
			  }

			  /* Was ASA initialized? */
			  if (cv_mem.cv_adjMallocDone == false) {
			    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeGetAdjCheckPointsInfo", MSGCV_NO_ADJ);
			    return(CV_NO_ADJ);
			  } 
			  ca_mem = cv_mem.cv_adj_mem;

			  ck_mem = ca_mem.ck_mem;

			  i = 0;

			  while (ck_mem != null) {

			    ckpnt[i].my_addr = ck_mem;
			    ckpnt[i].next_addr = ck_mem.ck_next;
			    ckpnt[i].t0 = ck_mem.ck_t0;
			    ckpnt[i].t1 = ck_mem.ck_t1;
			    ckpnt[i].nstep = ck_mem.ck_nst;
			    ckpnt[i].order = ck_mem.ck_q;
			    ckpnt[i].step = ck_mem.ck_h;

			    ck_mem = ck_mem.ck_next;
			    i++;

			  }

			  return(CV_SUCCESS);

			}
			
			private int CVodeCreateB(CVodeMemRec cv_mem, int lmmB, int iterB, int which[])
			{
			  CVadjMemRec ca_mem;
			  CVodeBMemRec new_cvB_mem;
			  CVodeMemRec cvodeB_mem;

			  /* Check if cvode_mem exists */
			  if (cv_mem == null) {
			    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeCreateB", MSGCV_NO_MEM);
			    return(CV_MEM_NULL);
			  }

			  /* Was ASA initialized? */
			  if (cv_mem.cv_adjMallocDone == false) {
			    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeCreateB", MSGCV_NO_ADJ);
			    return(CV_NO_ADJ);
			  }
			  ca_mem = cv_mem.cv_adj_mem;

			  /* Allocate space for new CVodeBMem object */

			  new_cvB_mem = null;
			  new_cvB_mem = new CVodeBMemRec();

			  /* Create and set a new CVODES object for the backward problem */

			  cvodeB_mem = CVodeCreate(lmmB, iterB);
			  if (cvodeB_mem == null) {
			    cvProcessError(cv_mem, CV_MEM_FAIL, "CVODEA", "CVodeCreateB", MSGCV_MEM_FAIL);
			    return(CV_MEM_FAIL);
			  }

			  UserData userData = new UserData();
			  userData.memRec = cv_mem;
			  cvodeB_mem.cv_user_data = userData;

			  cvodeB_mem.cv_mxhnil = -1;

			  //CVodeSetErrHandlerFn(cvodeB_mem, cv_mem->cv_ehfun, cv_mem->cv_eh_data);
			  //cvodeB_mem.cv_ehfun = cv_mem.cv_ehfun;
			  cvodeB_mem.cv_eh_data = cv_mem.cv_eh_data;

			  //CVodeSetErrFile(cvodeB_mem, cv_mem->cv_errfp);
			  cvodeB_mem.cv_errfp = cv_mem.cv_errfp;

			  /* Set/initialize fields in the new CVodeBMem object, new_cvB_mem */

			  new_cvB_mem.cv_index   = ca_mem.ca_nbckpbs;

			  new_cvB_mem.cv_mem     = cvodeB_mem;

			  //new_cvB_mem.cv_f       = null;
			  //new_cvB_mem.cv_fs      = null;

			  //new_cvB_mem.cv_fQ      = null;
			  //new_cvB_mem.cv_fQs     = null;

			  new_cvB_mem.cv_user_data  = null;

			  //new_cvB_mem.cv_lmem    = null;
			  //new_cvB_mem.cv_lfree   = null;
			  //new_cvB_mem.cv_pmem    = null;
			  //new_cvB_mem.cv_pfree   = null;

			  new_cvB_mem.cv_y       = null;

			  new_cvB_mem.cv_f_withSensi = false;
			  new_cvB_mem.cv_fQ_withSensi = false;

			  /* Attach the new object to the linked list cvB_mem */

			  new_cvB_mem.cv_next = ca_mem.cvB_mem;
			  ca_mem.cvB_mem = new_cvB_mem;
			  
			  /* Return the index of the newly created CVodeBMem object.
			   * This must be passed to CVodeInitB and to other ***B 
			   * functions to set optional inputs for this backward problem */

			  which[0] = ca_mem.ca_nbckpbs;

			  ca_mem.ca_nbckpbs++;

			  return(CV_SUCCESS);
			}

			private int CVodeInitB(CVodeMemRec cv_mem, int which, 
		               int fB,
		               double tB0, NVector yB0)
		{
		  CVadjMemRec ca_mem;
		  CVodeBMemRec cvB_mem;
		  CVodeMemRec cvodeB_mem;
		  int flag;

		  /* Check if cvode_mem exists */

		  if (cv_mem == null) {
		    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeInitB", MSGCV_NO_MEM);
		    return(CV_MEM_NULL);
		  }

		  /* Was ASA initialized? */

		  if (cv_mem.cv_adjMallocDone == false) {
		    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeInitB", MSGCV_NO_ADJ);
		    return(CV_NO_ADJ);
		  } 
		  ca_mem = cv_mem.cv_adj_mem;

		  /* Check the value of which */

		  if ( which >= ca_mem.ca_nbckpbs ) {
		    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeInitB", MSGCV_BAD_WHICH);
		    return(CV_ILL_INPUT);
		  }

		  /* Find the CVodeBMem entry in the linked list corresponding to which */

		  cvB_mem = ca_mem.cvB_mem;
		  while (cvB_mem != null) {
		    if ( which == cvB_mem.cv_index ) break;
		    cvB_mem = cvB_mem.cv_next;
		  }

		  cvodeB_mem = cvB_mem.cv_mem;
		  
		  /* Allocate and set the CVODES object */

		  flag = CVodeInit(cvodeB_mem, CVArhs_select, tB0, yB0);

		  if (flag != CV_SUCCESS) return(flag);

		  /* Copy fB function in cvB_mem */

		  cvB_mem.cv_f_withSensi = false;
		  //cvB_mem.cv_f = fB;

		  /* Allocate space and initialize the y Nvector in cvB_mem */

		  cvB_mem.cv_t0 = tB0;
		  cvB_mem.cv_y = N_VClone(yB0);
		  N_VScale_Serial(ONE, yB0, cvB_mem.cv_y);

		  return(CV_SUCCESS);
		}

			/*
			 * CVArhs
			 *
			 * This routine interfaces to the CVRhsFnB (or CVRhsFnBS) routine 
			 * provided by the user.
			 */


			private int CVArhs(double t, NVector yB, 
			                  NVector yBdot, CVodeMemRec cv_mem)
			{
			  CVadjMemRec ca_mem;
			  CVodeBMemRec cvB_mem;
			  int flag = 0;
			  int retval = 0;

			  ca_mem = cv_mem.cv_adj_mem;

			  cvB_mem = ca_mem.ca_bckpbCrt;

			  /* Get forward solution from interpolation */

			  if (ca_mem.ca_IMinterpSensi)
				if (ca_mem.ca_IMget == CVAhermiteGetY_select) {
			        flag = CVAhermiteGetY(cv_mem, t, ca_mem.ca_ytmp, ca_mem.ca_yStmp);
				}
			  else 
				  if (ca_mem.ca_IMget == CVAhermiteGetY_select) {
			          flag = CVAhermiteGetY(cv_mem, t, ca_mem.ca_ytmp, null);
				  }

			  if (flag != CV_SUCCESS) {
			    cvProcessError(cv_mem, -1, "CVODEA", "CVArhs", MSGCV_BAD_TINTERP, t);
			    return(-1);
			  }

			  /* Call the user's RHS function */

			  if (cvB_mem.cv_f_withSensi)
				if (testMode) {
					//retval = fBS1TestMode(t, ca_mem.ca_ytmp, ca_mem.ca_yStmp, yB, yBdot, cvB_mem.cv_user_data);	
				}
				else {
			        //retval = fBS1(t, ca_mem.ca_ytmp, ca_mem.ca_yStmp, yB, yBdot, cvB_mem.cv_user_data);
				}
			  else
				  if (testMode) {
					  retval = fBTestMode(t, ca_mem.ca_ytmp, yB, yBdot, cvB_mem.cv_user_data);
				  }
				  else {
			          retval = fB(t, ca_mem.ca_ytmp, yB, yBdot, cvB_mem.cv_user_data);
				  }

			  return(retval);
			}
			
			/*
			 * CVAhermiteGetY ( -> IMget )
			 *
			 * This routine uses cubic piece-wise Hermite interpolation for 
			 * the forward solution vector. 
			 * It is typically called by the wrapper routines before calling
			 * user provided routines (fB, djacB, bjacB, jtimesB, psolB) but
			 * can be directly called by the user through CVodeGetAdjY
			 */

			private int CVAhermiteGetY(CVodeMemRec cv_mem, double t,
			                          NVector y, NVector yS[])
			{
			  CVadjMemRec ca_mem;
			  DtpntMemRec dt_mem[];
			  HermiteDataMemRec content0, content1;

			  double t0, t1, delta;
			  double factor1, factor2, factor3;

			  NVector y0, yd0, y1, yd1;
			  NVector yS0[]=null;
			  NVector ySd0[]= null;
			  NVector yS1[], ySd1[];

			  int flag, is, NS;
			  long indx[] = new long[1];
			  boolean newpoint[] = new boolean[1];

			 
			  ca_mem = cv_mem.cv_adj_mem;
			  dt_mem = ca_mem.dt_mem;
			 
			  /* Local value of Ns */
			 
			  NS = (ca_mem.ca_IMinterpSensi && (yS != null)) ? cv_mem.cv_Ns : 0;

			  /* Get the index in dt_mem */

			  flag = CVAfindIndex(cv_mem, t, indx, newpoint);
			  if (flag != CV_SUCCESS) return(flag);

			  /* If we are beyond the left limit but close enough,
			     then return y at the left limit. */

			  if (indx[0] == 0) {
			    content0 = dt_mem[0].hermiteContent;
			    N_VScale_Serial(ONE, content0.y, y);
			    for (is=0; is<NS; is++) N_VScale_Serial(ONE, content0.yS[is], yS[is]);
			    return(CV_SUCCESS);
			  }

			  /* Extract stuff from the appropriate data points */

			  t0 = dt_mem[(int)(indx[0]-1)].t;
			  t1 = dt_mem[(int)indx[0]].t;
			  delta = t1 - t0;

			  content0 = dt_mem[(int)(indx[0]-1)].hermiteContent;
			  y0  = content0.y;
			  yd0 = content0.yd;
			  if (ca_mem.ca_IMinterpSensi) {
			    yS0  = content0.yS;
			    ySd0 = content0.ySd;
			  }

			  if (newpoint[0]) {
			    
			    /* Recompute Y0 and Y1 */

			    content1 = dt_mem[(int)indx[0]].hermiteContent;

			    y1  = content1.y;
			    yd1 = content1.yd;

			    N_VLinearSum_Serial(ONE, y1, -ONE, y0, ca_mem.ca_Y[0]);
			    N_VLinearSum_Serial(ONE, yd1,  ONE, yd0, ca_mem.ca_Y[1]);
			    N_VLinearSum_Serial(delta, ca_mem.ca_Y[1], -TWO, ca_mem.ca_Y[0], ca_mem.ca_Y[1]);
			    N_VLinearSum_Serial(ONE, ca_mem.ca_Y[0], -delta, yd0, ca_mem.ca_Y[0]);


			    yS1  = content1.yS;
			    ySd1 = content1.ySd;
			      
			    for (is=0; is<NS; is++) {
			      N_VLinearSum_Serial(ONE, yS1[is], -ONE, yS0[is], ca_mem.ca_YS[0][is]);
			      N_VLinearSum_Serial(ONE, ySd1[is],  ONE, ySd0[is], ca_mem.ca_YS[1][is]);
			      N_VLinearSum_Serial(delta, ca_mem.ca_YS[1][is], -TWO, ca_mem.ca_YS[0][is], ca_mem.ca_YS[1][is]);
			      N_VLinearSum_Serial(ONE, ca_mem.ca_YS[0][is], -delta, ySd0[is], ca_mem.ca_YS[0][is]);
			    }

			  }

			  /* Perform the actual interpolation. */

			  factor1 = t - t0;

			  factor2 = factor1/delta;
			  factor2 = factor2*factor2;

			  factor3 = factor2*(t-t1)/delta;

			  N_VLinearSum_Serial(ONE, y0, factor1, yd0, y);
			  N_VLinearSum_Serial(ONE, y, factor2, ca_mem.ca_Y[0], y);
			  N_VLinearSum_Serial(ONE, y, factor3, ca_mem.ca_Y[1], y);

			  for (is=0; is<NS; is++) {
			    N_VLinearSum_Serial(ONE, yS0[is], factor1, ySd0[is], yS[is]);
			    N_VLinearSum_Serial(ONE, yS[is], factor2, ca_mem.ca_YS[0][is], yS[is]);
			    N_VLinearSum_Serial(ONE, yS[is], factor3, ca_mem.ca_YS[1][is], yS[is]);
			  }


			  return(CV_SUCCESS);
			}
			
			/*
			 * CVAfindIndex
			 *
			 * Finds the index in the array of data point strctures such that
			 *     dt_mem[indx-1].t <= t < dt_mem[indx].t
			 * If indx is changed from the previous invocation, then newpoint = SUNTRUE
			 *
			 * If t is beyond the leftmost limit, but close enough, indx=0.
			 *
			 * Returns CV_SUCCESS if successful and CV_GETY_BADT if unable to
			 * find indx (t is too far beyond limits).
			 */

			static int CVAfindIndex(CVodeMemRec cv_mem, double t, 
			                        long indx[], boolean newpoint[])
			{
			  CVadjMemRec ca_mem;
			  long ilast = 0;
			  DtpntMemRec dt_mem[];
			  int sign;
			  boolean to_left, to_right;

			  ca_mem = cv_mem.cv_adj_mem;
			  dt_mem = ca_mem.dt_mem;

			  newpoint[0] = false;

			  /* Find the direction of integration */
			  sign = (ca_mem.ca_tfinal - ca_mem.ca_tinitial > ZERO) ? 1 : -1;

			  /* If this is the first time we use new data */
			  if (ca_mem.ca_IMnewData) {
			    ilast     = ca_mem.ca_np-1;
			    newpoint[0] = true;
			    ca_mem.ca_IMnewData   = false;
			  }

			  /* Search for indx starting from ilast */
			  to_left  = ( sign*(t - dt_mem[(int)(ilast-1)].t) < ZERO);
			  to_right = ( sign*(t - dt_mem[(int)(ilast)].t)   > ZERO);

			  if ( to_left ) {
			    /* look for a new indx to the left */

			    newpoint[0] = true;
			    
			    indx[0] = ilast;
			    for(;;) {
			      if ( indx[0] == 0 ) break;
			      if ( sign*(t - dt_mem[(int)(indx[0]-1)].t) <= ZERO ) (indx[0])--;
			      else                                         break;
			    }

			    if ( indx[0] == 0 )
			      ilast = 1;
			    else
			      ilast = indx[0];

			    if ( indx[0] == 0 ) {
			      /* t is beyond leftmost limit. Is it too far? */  
			      if ( Math.abs(t - dt_mem[0].t) > FUZZ_FACTOR * cv_mem.cv_uround ) {
			        return(CV_GETY_BADT);
			      }
			    }

			  } else if ( to_right ) {
			    /* look for a new indx to the right */

			    newpoint[0] = true;

			    indx [0]= ilast;
			    for(;;) {
			      if ( sign*(t - dt_mem[(int)indx[0]].t) > ZERO) (indx[0])++;
			      else                                     break;
			    }

			    ilast = indx[0];


			  } else {
			    /* ilast is still OK */

			    indx[0] = ilast;

			  }

			  return(CV_SUCCESS);


			}




}