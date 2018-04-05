package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;

public abstract class CVODES_ASA extends CVODES {
	
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
        int JacB = cvsRoberts_ASAi_dns;
		int ewt_select = cvEwtUser_select1;
		int fQ= cvsRoberts_ASAi_dns;
		int fQB = cvsRoberts_ASAi_dns;
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
		double AB[][];
		SUNLinearSolver LSB;
		long nstB[] = new long[1];
		
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
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Set the scalar relative and absolute tolerances.
		flag = CVodeSStolerancesB(cvode_mem, indexB[0], reltolB, abstolB);
		if (flag < 0) {
			return;
		}
		
		// Attach the user data for backward problem
		flag = CVodeSetUserDataB(cvode_mem, indexB[0], data);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Create dense SUNMatrix for user in linear solver
		AB = new double[NEQ][NEQ];
		
		// Create dense SUNLinearSolver object
		LSB = SUNDenseLinearSolver(yB, AB);
		if (LSB == null) {
			return;
		}
		
		// Attach the matrix and linear solver
		flag = CVDlsSetLinearSolverB(cvode_mem, indexB[0], LSB, AB);
		if (flag < 0) {
			return;
		}
		
		// Set the user-supplied Jacobian routine JacB
		flag = CVDlsSetJacFnB(cvode_mem, indexB[0], JacB);
		if (flag < 0) {
			return;
		}
		
		// Call CVodeQuadInitB to allocate internal memory and initialize backward
		// quadrature integration.
		flag = CVodeQuadInitB(cvode_mem, indexB[0], fQB, qB);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Call CVodeSetQuadErrConB to specify whether or not the quadrature variables
		// are to be used in the step size control mechanism within CVODES.  Call
		// CVodeQuadSStoerancesB or CVodeQuadSVtolerancesB to specify the integration
		// tolerances for the quadrature variables.
		flag = CVodeSetQuadErrConB(cvode_mem, indexB[0], true);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Call CVodeQUadSStolerancesB to specify the scalar relative and absolute tolerances 
		// for the backward problem.
		flag = CVodeQuadSStolerancesB(cvode_mem, indexB[0], reltolB, abstolQB);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Backward integration
		System.out.printf("Backward integration from tB0 = " + TB1 + "\n\n");
		
		// First get results at t = TBout1
		
		// Call CVodeBto integrate the backward ODE problem.
		flag = CVodeB(cvode_mem, TBout1, CV_NORMAL);
		if (flag < 0) {
			return;
		}
		
		// Call CVodeGetB to get yB of the backward ODE problem
		flag = CVodeGetB(cvode_mem, indexB[0], time, yB);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Call CvodeGetAdjY to get the interpolated value of the forward solution
		// y during a backward integration.
		flag = CVodeGetAdjY(cvode_mem, TBout1, y);
		if (flag < 0) {
			return;
		}
		
		System.out.printf("returned t: " + time[0] + "\n");
		System.out.printf("tout: " + TBout1 + "\n");
		System.out.printf("lambda(t): " + yB.data[0] + "  " + yB.data[1] + "  " + yB.data[2] + "\n");
		System.out.printf("y(t): " + y.data[0] + "  " + y.data[1] + "  " + y.data[2] + "\n");
		
		// Then at t = T0
		
		flag = CVodeB(cvode_mem, T0, CV_NORMAL);
		if (flag < 0) {
			return;
		}
		
		CVodeGetNumSteps(CVodeGetAdjCVodeBmem(cvode_mem, indexB[0]), nstB);
		System.out.printf("Done (nst = " + nstB[0] + ")\n");
		
		flag = CVodeGetB(cvode_mem, indexB[0], time, yB);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Call CVodeGetQuadB to get the quadrature solution vector after a
		// successful return from CVodeB.
		flag = CVodeGetQuadB(cvode_mem, indexB[0], time, qB);
		if (flag < 0) {
			return;
		}
		
		flag = CVodeGetAdjY(cvode_mem, T0, y);
		if (flag < 0) {
			return;
		}
		
		System.out.printf("returned t: " + time[0] + "\n");
		System.out.printf("lambda(t0): " + yB.data[0] + "  " + yB.data[1] + "  " + yB.data[2] + "\n");
		System.out.printf("y(t0): " + y.data[0] + "  " + y.data[1] + "  " + y.data[2] + "\n");
		System.out.printf("dG/dp: " + (-qB.data[0]) + "  " + (-qB.data[1]) + "  " + (-qB.data[2]) + "\n");
		
		// Reinitialize backward phase (new tB0)
		
		yB.data[0] = ZERO;
		yB.data[1] = ZERO;
		yB.data[2] = ZERO;
		
		qB.data[0] = ZERO;
		qB.data[1] = ZERO;
		qB.data[2] = ZERO;
		
		System.out.printf("Re-initialize CVODES memory for backward run\n");
		
		flag = CVodeReInitB(cvode_mem, indexB[0], TB2, yB);
		if (flag < 0) {
			return;
		}
		
		flag = CVodeQuadReInitB(cvode_mem, indexB[0], qB);
		if (flag != CV_SUCCESS) {
			return;
		}
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

	  new_cvB_mem.cv_f       = -1;
	  //new_cvB_mem.cv_fs      = null;

	  new_cvB_mem.cv_fQ      = -1;
	  //new_cvB_mem.cv_fQs     = null;

	  new_cvB_mem.cv_user_data  = null;

	  new_cvB_mem.cv_lmem    = null;
	  //new_cvB_mem.cv_lfree   = null;
	  new_cvB_mem.cv_lfree_select = -1;
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
  cvB_mem.cv_f = fB;

  /* Allocate space and initialize the y Nvector in cvB_mem */

  cvB_mem.cv_t0 = tB0;
  cvB_mem.cv_y = N_VClone(yB0);
  N_VScale_Serial(ONE, yB0, cvB_mem.cv_y);

  return(CV_SUCCESS);
}

	
	
	

	private int CVodeSStolerancesB(CVodeMemRec cv_mem, int which, double reltolB, double abstolB)
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem;
	  CVodeMemRec cvodeB_mem;
	  int flag;

	  /* Check if cvode_mem exists */

	  if (cv_mem == null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeSStolerancesB", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Was ASA initialized? */

	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeSStolerancesB", MSGCV_NO_ADJ);
	    return(CV_NO_ADJ);
	  } 
	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check the value of which */

	  if ( which >= ca_mem.ca_nbckpbs ) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeSStolerancesB", MSGCV_BAD_WHICH);
	    return(CV_ILL_INPUT);
	  }

	  /* Find the CVodeBMem entry in the linked list corresponding to which */

	  cvB_mem = ca_mem.cvB_mem;
	  while (cvB_mem != null) {
	    if ( which == cvB_mem.cv_index ) break;
	    cvB_mem = cvB_mem.cv_next;
	  }

	  cvodeB_mem = cvB_mem.cv_mem;

	  /* Set tolerances */

	  flag = CVodeSStolerances(cvodeB_mem, reltolB, abstolB);

	  return(flag);
	}

	private int CVodeSetUserDataB(CVodeMemRec cv_mem, int which, UserData user_dataB)
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem;

	  /* Check if cvode_mem exists */
	  if (cv_mem == null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeSetUserDataB", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Was ASA initialized? */
	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeSetUserDataB", MSGCV_NO_ADJ);
	    return(CV_NO_ADJ);
	  } 
	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check which */
	  if ( which >= ca_mem.ca_nbckpbs ) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeSetUserDataB", MSGCV_BAD_WHICH);
	    return(CV_ILL_INPUT);
	  }

	  /* Find the CVodeBMem entry in the linked list corresponding to which */
	  cvB_mem = ca_mem.cvB_mem;
	  while (cvB_mem != null) {
	    if ( which == cvB_mem.cv_index ) break;
	    cvB_mem = cvB_mem.cv_next;
	  }

	  cvB_mem.cv_user_data = user_dataB;

	  return(CV_SUCCESS);
	}
	
	/*---------------------------------------------------------------
	  CVDlsSetLinearSolverB specifies the direct linear solver for 
	  backward integration.
	  ---------------------------------------------------------------*/
	private int CVDlsSetLinearSolverB(CVodeMemRec cv_mem, int which,
	                          SUNLinearSolver LS, double A[][])
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem;
	  CVodeMemRec cvodeB_mem;
	  CVDlsMemRecB cvdlsB_mem;
	  int flag;

	  /* Check if cvode_mem exists */
	  if (cv_mem == null) {
	    cvProcessError(null, CVDLS_MEM_NULL, "CVSDLS",
	                   "CVDlsSetLinearSolverB", MSGD_CVMEM_NULL);
	    return(CVDLS_MEM_NULL);
	  }

	  /* Was ASA initialized? */
	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, CVDLS_NO_ADJ, "CVSDLS",
	                   "CVDlsSetLinearSolverB", MSGD_NO_ADJ);
	    return(CVDLS_NO_ADJ);
	  } 
	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check which */
	  if ( which >= ca_mem.ca_nbckpbs ) {
	    cvProcessError(cv_mem, CVDLS_ILL_INPUT, "CVSDLS",
	                   "CVDlsSetLinearSolverB", MSGD_BAD_WHICH);
	    return(CVDLS_ILL_INPUT);
	  }

	  /* Find the CVodeBMem entry in the linked list corresponding to which */
	  cvB_mem = ca_mem.cvB_mem;
	  while (cvB_mem != null) {
	    if ( which == cvB_mem.cv_index ) break;
	    cvB_mem = cvB_mem.cv_next;
	  }

	  /* Get memory for CVDlsMemRecB */
	  cvdlsB_mem = new CVDlsMemRecB();

	  /* free any existing system solver attached to cvB */
	  //if (cvB_mem.cv_lfree_select > 0)  cvB_mem.cv_lfree(cvB_mem);

	  /* Attach lmemB data and lfreeB function. */
	  cvB_mem.cv_lmem  = cvdlsB_mem;
	  cvB_mem.cv_lfree_select = cvDlsFreeB_select;

	  /* initialize jacB and jacBS pointers */
	  cvdlsB_mem.jacB  = -1;
	  cvdlsB_mem.jacBS = -1;

	  /* set the linear solver for this backward problem */
	  cvodeB_mem = cvB_mem.cv_mem;
	  flag = CVDlsSetLinearSolver(cvodeB_mem, LS, A);
	  if (flag != CVDLS_SUCCESS) {
	    cvdlsB_mem = null;
	  }

	  return(flag);
	}

	/*-----------------------------------------------------------------
	  Type: CVDlsJacFnB
	  -----------------------------------------------------------------
	  A Jacobian approximation function jacB for the adjoint
	  (backward) problem must have the prototype given below. 
	  -----------------------------------------------------------------*/
	//typedef int (*CVDlsJacFnB)(realtype t, N_Vector y, N_Vector yB,
	                           //N_Vector fyB, SUNMatrix JB,
	                          // void *user_dataB, N_Vector tmp1B,
	                          // N_Vector tmp2B, N_Vector tmp3B);

	/*-----------------------------------------------------------------
	  Type: CVDlsJacFnBS
	  -----------------------------------------------------------------
	  A Jacobian approximation function jacBS for the adjoint
	  (backward) problem, sensitivity-dependent case,  must have the
	  prototype given below. 
	  -----------------------------------------------------------------*/
	//typedef int (*CVDlsJacFnBS)(realtype t, N_Vector y, N_Vector *yS,
	                           // N_Vector yB, N_Vector fyB, SUNMatrix JB,
	                            //void *user_dataB, N_Vector tmp1B,
	                           // N_Vector tmp2B, N_Vector tmp3B);


	
				
	private int CVDlsSetJacFnB(CVodeMemRec cv_mem, int which, int jacB)
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem;
	  CVDlsMemRecB cvdlsB_mem;
	  CVodeMemRec cvodeB_mem;
	  int flag;

	  /* Check if cvode_mem exists */
	  if (cv_mem == null) {
	    cvProcessError(null, CVDLS_MEM_NULL, "CVSDLS",
	                   "CVDlsSetJacFnB", MSGD_CVMEM_NULL);
	    return(CVDLS_MEM_NULL);
	  }

	  /* Was ASA initialized? */
	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, CVDLS_NO_ADJ, "CVSDLS",
	                   "CVDlsSetJacFnB", MSGD_NO_ADJ);
	    return(CVDLS_NO_ADJ);
	  } 
	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check which */
	  if ( which >= ca_mem.ca_nbckpbs ) {
	    cvProcessError(cv_mem, CVDLS_ILL_INPUT, "CVSDLS",
	                   "CVDlsSetJacFnB", MSGD_BAD_WHICH);
	    return(CVDLS_ILL_INPUT);
	  }

	  /* Find the CVodeBMem entry in the linked list corresponding to which */
	  cvB_mem = ca_mem.cvB_mem;
	  while (cvB_mem != null) {
	    if ( which == cvB_mem.cv_index ) break;
	    cvB_mem = cvB_mem.cv_next;
	  }

	  cvodeB_mem = cvB_mem.cv_mem;

	  if (cvB_mem.cv_lmem == null) {
	    cvProcessError(cv_mem, CVDLS_LMEMB_NULL, "CVSDLS",
	                   "CVDlsSetJacFnB", MSGD_LMEMB_NULL);
	    return(CVDLS_LMEMB_NULL);
	  }
	  cvdlsB_mem = cvB_mem.cv_lmem;

	  cvdlsB_mem.jacB = jacB;

	  if (jacB > 0) {
	    flag = CVDlsSetJacFn(cvodeB_mem, cvDlsJacBWrapper_select);
	  } else {
	    flag = CVDlsSetJacFn(cvodeB_mem, -1);
	  }

	  return(flag);
	}
	
		private int CVodeQuadInitB(CVodeMemRec cv_mem, int which,
	           int fQB, NVector yQB0)
	{
	CVadjMemRec ca_mem;
	CVodeBMemRec cvB_mem;
	CVodeMemRec cvodeB_mem;
	int flag;
	
	/* Check if cvode_mem exists */
	if (cv_mem == null) {
	cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeQuadInitB", MSGCV_NO_MEM);
	return(CV_MEM_NULL);
	}
	
	/* Was ASA initialized? */
	if (cv_mem.cv_adjMallocDone == false) {
	cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeQuadInitB", MSGCV_NO_ADJ);
	return(CV_NO_ADJ);
	} 
	ca_mem = cv_mem.cv_adj_mem;
	
	/* Check which */
	if ( which >= ca_mem.ca_nbckpbs ) {
	cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeQuadInitB", MSGCV_BAD_WHICH);
	return(CV_ILL_INPUT);
	}
	
	/* Find the CVodeBMem entry in the linked list corresponding to which */
	cvB_mem = ca_mem.cvB_mem;
	while (cvB_mem != null) {
	if ( which == cvB_mem.cv_index ) break;
	cvB_mem = cvB_mem.cv_next;
	}
	
	cvodeB_mem = cvB_mem.cv_mem;
	
	flag = CVodeQuadInit(cvodeB_mem, CVArhsQ_select, yQB0);
	if (flag != CV_SUCCESS) return(flag);
	
	cvB_mem.cv_fQ_withSensi = false;
	cvB_mem.cv_fQ = fQB;
	
	return(CV_SUCCESS);
	}
		
	/*
	 * CVodeSetQuad*B
	 *
	 * Wrappers for the backward phase around the corresponding 
	 * CVODES quadrature optional input functions
	 */

	private int CVodeSetQuadErrConB(CVodeMemRec cv_mem, int which, boolean errconQB)
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem;
	  CVodeMemRec cvodeB_mem;

	  /* Check if cvode_mem exists */
	  if (cv_mem == null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeSetQuadErrConB", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Was ASA initialized? */
	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeSetQuadErrConB", MSGCV_NO_ADJ);
	    return(CV_NO_ADJ);
	  } 
	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check which */
	  if ( which >= ca_mem.ca_nbckpbs ) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeSetQuadErrConB", MSGCV_BAD_WHICH);
	    return(CV_ILL_INPUT);
	  }

	  /* Find the CVodeBMem entry in the linked list corresponding to which */
	  cvB_mem = ca_mem.cvB_mem;
	  while (cvB_mem != null) {
	    if ( which == cvB_mem.cv_index ) break;
	    cvB_mem = cvB_mem.cv_next;
	  }

	  cvodeB_mem = cvB_mem.cv_mem;

	  cvodeB_mem.cv_errconQ = errconQB;

	  return(CV_SUCCESS);
	}
	
	private int CVodeQuadSStolerancesB(CVodeMemRec cv_mem, int which, double reltolQB, double abstolQB)
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem;
	  CVodeMemRec cvodeB_mem;
	  int flag;

	  /* Check if cvode_mem exists */
	  if (cv_mem == null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeQuadSStolerancesB", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Was ASA initialized? */
	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeQuadSStolerancesB", MSGCV_NO_ADJ);
	    return(CV_NO_ADJ);
	  } 
	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check which */
	  if ( which >= ca_mem.ca_nbckpbs ) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeQuadSStolerancesB", MSGCV_BAD_WHICH);
	    return(CV_ILL_INPUT);
	  }

	  /* Find the CVodeBMem entry in the linked list corresponding to which */
	  cvB_mem = ca_mem.cvB_mem;
	  while (cvB_mem != null) {
	    if ( which == cvB_mem.cv_index ) break;
	    cvB_mem = cvB_mem.cv_next;
	  }

	  cvodeB_mem = cvB_mem.cv_mem;

	  flag = CVodeQuadSStolerances(cvodeB_mem, reltolQB, abstolQB);

	  return(flag);
	}
	
	/*
	 * CVodeB
	 *
	 * This routine performs the backward integration towards tBout
	 * of all backward problems that were defined.
	 * When necessary, it performs a forward integration between two 
	 * consecutive check points to update interpolation data.
	 *
	 * On a successful return, CVodeB returns CV_SUCCESS.
	 *
	 * NOTE that CVodeB DOES NOT return the solution for the backward
	 * problem(s). Use CVodeGetB to extract the solution at tBret
	 * for any given backward problem.
	 *
	 * If there are multiple backward problems and multiple check points,
	 * CVodeB may not succeed in getting all problems to take one step
	 * when called in ONE_STEP mode.
	 */

	private int CVodeB(CVodeMemRec cv_mem, double tBout, int itaskB)
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem, tmp_cvB_mem;
	  CkpntMemRec ck_mem;
	  int sign, flag=0;
	  double tfuzz, tBn;
	  double tBret[] = new double[1];
	  boolean gotCheckpoint, isActive, reachedTBout;
	  
	  /* Check if cvode_mem exists */

	  if (cv_mem == null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeB", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Was ASA initialized? */

	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeB", MSGCV_NO_ADJ);
	    return(CV_NO_ADJ);
	  }
	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check if any backward problem has been defined */

	  if ( ca_mem.ca_nbckpbs == 0 ) {
	    cvProcessError(cv_mem, CV_NO_BCK, "CVODEA", "CVodeB", MSGCV_NO_BCK);
	    return(CV_NO_BCK);
	  }
	  cvB_mem = ca_mem.cvB_mem;

	  /* Check whether CVodeF has been called */

	  if ( ca_mem.ca_firstCVodeFcall ) {
	    cvProcessError(cv_mem, CV_NO_FWD, "CVODEA", "CVodeB", MSGCV_NO_FWD);
	    return(CV_NO_FWD);
	  }
	  sign = (ca_mem.ca_tfinal - ca_mem.ca_tinitial > ZERO) ? 1 : -1;

	  /* If this is the first call, loop over all backward problems and
	   *   - check that tB0 is valid
	   *   - check that tBout is ahead of tB0 in the backward direction
	   *   - check whether we need to interpolate forward sensitivities
	   */

	  if ( ca_mem.ca_firstCVodeBcall ) {

	    tmp_cvB_mem = cvB_mem;

	    while(tmp_cvB_mem != null) {

	      tBn = tmp_cvB_mem.cv_mem.cv_tn;

	      if ( (sign*(tBn-ca_mem.ca_tinitial) < ZERO) || (sign*(ca_mem.ca_tfinal-tBn) < ZERO) ) {
	        cvProcessError(cv_mem, CV_BAD_TB0, "CVODEA", "CVodeB", MSGCV_BAD_TB0,
	                       tmp_cvB_mem.cv_index);
	        return(CV_BAD_TB0);
	      }

	      if (sign*(tBn-tBout) <= ZERO) {
	        cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeB", MSGCV_BAD_TBOUT,
	                       tmp_cvB_mem.cv_index);
	        return(CV_ILL_INPUT);
	      }

	      if ( tmp_cvB_mem.cv_f_withSensi || tmp_cvB_mem.cv_fQ_withSensi )
	          ca_mem.ca_IMinterpSensi = true;

	      tmp_cvB_mem = tmp_cvB_mem.cv_next;

	    }

	    if ( ca_mem.ca_IMinterpSensi && !ca_mem.ca_IMstoreSensi) {
	      cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeB", MSGCV_BAD_SENSI);
	      return(CV_ILL_INPUT);
	    }

	    ca_mem.ca_firstCVodeBcall = false;
	  }

	  /* Check if itaskB is legal */

	  if ( (itaskB != CV_NORMAL) && (itaskB != CV_ONE_STEP) ) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeB", MSGCV_BAD_ITASKB);
	    return(CV_ILL_INPUT);
	  }

	  /* Check if tBout is legal */

	  if ( (sign*(tBout-ca_mem.ca_tinitial) < ZERO) || (sign*(ca_mem.ca_tfinal-tBout) < ZERO) ) {
	    tfuzz = HUNDRED*cv_mem.cv_uround*(Math.abs(ca_mem.ca_tinitial) + Math.abs(ca_mem.ca_tfinal));
	    if ( (sign*(tBout-ca_mem.ca_tinitial) < ZERO) && (Math.abs(tBout-ca_mem.ca_tinitial) < tfuzz) ) {
	      tBout = ca_mem.ca_tinitial;
	    } else {
	      cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeB", MSGCV_BAD_TBOUT);
	      return(CV_ILL_INPUT);
	    }
	  }

	  /* Loop through the check points and stop as soon as a backward
	   * problem has its tn value behind the current check point's t0_
	   * value (in the backward direction) */

	  ck_mem = ca_mem.ck_mem;

	  gotCheckpoint = false;

	  for(;;) {

	    tmp_cvB_mem = cvB_mem;
	    while(tmp_cvB_mem != null) {
	      tBn = tmp_cvB_mem.cv_mem.cv_tn;

	      if ( sign*(tBn-ck_mem.ck_t0) > ZERO ) {
	        gotCheckpoint = true;
	        break;
	      }

	      if ( (itaskB==CV_NORMAL) && (tBn == ck_mem.ck_t0) && (sign*(tBout-ck_mem.ck_t0) >= ZERO) ) {
	        gotCheckpoint = true;
	        break;
	      }

	      tmp_cvB_mem = tmp_cvB_mem.cv_next;
	    }

	    if (gotCheckpoint) break;

	    if (ck_mem.ck_next == null) break;

	    ck_mem = ck_mem.ck_next;
	  }

	  /* Starting with the current check point from above, loop over check points
	     while propagating backward problems */

	  for(;;) {

	    /* Store interpolation data if not available.
	       This is the 2nd forward integration pass */

	    if (ck_mem != ca_mem.ca_ckpntData) {
	      flag = CVAdataStore(cv_mem, ck_mem);
	      if (flag != CV_SUCCESS) break;
	    }

	    /* Loop through all backward problems and, if needed,
	     * propagate their solution towards tBout */

	    tmp_cvB_mem = cvB_mem;
	    while (tmp_cvB_mem != null) {

	      /* Decide if current backward problem is "active" in this check point */

	      isActive = true;

	      tBn = tmp_cvB_mem.cv_mem.cv_tn;

	      if ( (tBn == ck_mem.ck_t0) && (sign*(tBout-ck_mem.ck_t0) < ZERO ) ) isActive = false;
	      if ( (tBn == ck_mem.ck_t0) && (itaskB==CV_ONE_STEP) ) isActive = false;

	      if ( sign * (tBn - ck_mem.ck_t0) < ZERO ) isActive = false;

	      if ( isActive ) {

	        /* Store the address of current backward problem memory 
	         * in ca_mem to be used in the wrapper functions */
	        ca_mem.ca_bckpbCrt = tmp_cvB_mem;

	        /* Integrate current backward problem */
	        CVodeSetStopTime(tmp_cvB_mem.cv_mem, ck_mem.ck_t0);
	        flag = CVode(tmp_cvB_mem.cv_mem, tBout, tmp_cvB_mem.cv_y, tBret, itaskB);

	        /* Set the time at which we will report solution and/or quadratures */
	        tmp_cvB_mem.cv_tout = tBret[0];

	        /* If an error occurred, exit while loop */
	        if (flag < 0) break;

	      } else {
	        flag = CV_SUCCESS;
	        tmp_cvB_mem.cv_tout = tBn;
	      }

	      /* Move to next backward problem */

	      tmp_cvB_mem = tmp_cvB_mem.cv_next;
	    }

	    /* If an error occurred, return now */

	    if (flag <0) {
	      cvProcessError(cv_mem, flag, "CVODEA", "CVodeB", MSGCV_BACK_ERROR,
	                     tmp_cvB_mem.cv_index);
	      return(flag);
	    }

	    /* If in CV_ONE_STEP mode, return now (flag = CV_SUCCESS) */

	    if (itaskB == CV_ONE_STEP) break;

	    /* If all backward problems have succesfully reached tBout, return now */

	    reachedTBout = true;

	    tmp_cvB_mem = cvB_mem;
	    while(tmp_cvB_mem != null) {
	      if ( sign*(tmp_cvB_mem.cv_tout - tBout) > ZERO ) {
	        reachedTBout = false;
	        break;
	      }
	      tmp_cvB_mem = tmp_cvB_mem.cv_next;
	    }

	    if ( reachedTBout ) break;

	    /* Move check point in linked list to next one */

	    ck_mem = ck_mem.ck_next;

	  } 

	  return(flag);
	}

	/*
	 * CVAdataStore
	 *
	 * This routine integrates the forward model starting at the check
	 * point ck_mem and stores y and yprime at all intermediate steps.
	 *
	 * Return values:
	 * CV_SUCCESS
	 * CV_REIFWD_FAIL
	 * CV_FWD_FAIL
	 */

	private int CVAdataStore(CVodeMemRec cv_mem, CkpntMemRec ck_mem)
	{
      CVadjMemRec ca_mem;
	  DtpntMemRec dt_mem[];
	  double t[] = new double[1];
	  long i;
	  int flag, sign;

	  ca_mem = cv_mem.cv_adj_mem;
	  dt_mem = ca_mem.dt_mem;

	  /* Initialize cv_mem with data from ck_mem */
	  flag = CVAckpntGet(cv_mem, ck_mem);
	  if (flag != CV_SUCCESS)
	    return(CV_REIFWD_FAIL);

	  /* Set first structure in dt_mem[0] */
	  dt_mem[0].t = ck_mem.ck_t0;
	  if (ca_mem.ca_IMstore == CVAhermiteStorePnt_select) {
	        CVAhermiteStorePnt(cv_mem, dt_mem[0]);
	  }
	  else {
		    CVApolynomialStorePnt(cv_mem, dt_mem[0]);
	  }

	  /* Decide whether TSTOP must be activated */
	  if (ca_mem.ca_tstopCVodeFcall) {
	    CVodeSetStopTime(cv_mem, ca_mem.ca_tstopCVodeF);
	  }

	  sign = (ca_mem.ca_tfinal - ca_mem.ca_tinitial > ZERO) ? 1 : -1;


	  /* Run CVode to set following structures in dt_mem[i] */
	  i = 1;
	  do {

	    flag = CVode(cv_mem, ck_mem.ck_t1, ca_mem.ca_ytmp, t, CV_ONE_STEP);
	    if (flag < 0) return(CV_FWD_FAIL);

	    dt_mem[(int)i].t = t[0];
	    if (ca_mem.ca_IMstore == CVAhermiteStorePnt_select) {
	        CVAhermiteStorePnt(cv_mem, dt_mem[(int)i]);
	  }
	  else {
		    CVApolynomialStorePnt(cv_mem, dt_mem[(int)i]);
	  }
	    i++;

	  } while ( sign*(ck_mem.ck_t1 - t[0]) > ZERO );


	  ca_mem.ca_IMnewData = true;     /* New data is now available    */
	  ca_mem.ca_ckpntData = ck_mem;   /* starting at this check point */
	  ca_mem.ca_np = i;               /* and we have this many points */

	  return(CV_SUCCESS);
	}
	
	/* 
	 * CVodeSetStopTime
	 *
	 * Specifies the time beyond which the integration is not to proceed.
	 */

	private int CVodeSetStopTime(CVodeMemRec cv_mem, double tstop)
	{

	  if (cv_mem==null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeSetStopTime", MSGCV_NO_MEM);
	    return (CV_MEM_NULL);
	  }

	  /* If CVode was called at least once, test if tstop is legal
	   * (i.e. if it was not already passed).
	   * If CVodeSetStopTime is called before the first call to CVode,
	   * tstop will be checked in CVode. */
	  if (cv_mem.cv_nst > 0) {

	    if ( (tstop - cv_mem.cv_tn) * cv_mem.cv_h < ZERO ) {
	      cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVodeSetStopTime", MSGCV_BAD_TSTOP, tstop, cv_mem.cv_tn);
	      return(CV_ILL_INPUT);
	    }

	  }

	  cv_mem.cv_tstop = tstop;
	  cv_mem.cv_tstopset = true;

	  return(CV_SUCCESS);
	}

	/*
	 * CVAckpntGet
	 *
	 * This routine prepares CVODES for a hot restart from
	 * the check point ck_mem
	 */

	private int CVAckpntGet(CVodeMemRec cv_mem, CkpntMemRec ck_mem) 
	{
	  int flag, j, is, qmax;

	  if (ck_mem.ck_next == null) {

	    /* In this case, we just call the reinitialization routine,
	     * but make sure we use the same initial stepsize as on 
	     * the first run. */

	    // Specify the initial step size
		  cv_mem.cv_hin = cv_mem.cv_h0u;

	    flag = CVodeReInit(cv_mem, ck_mem.ck_t0, ck_mem.ck_zn[0]);
	    if (flag != CV_SUCCESS) return(flag);

	    if (ck_mem.ck_quadr) {
	      flag = CVodeQuadReInit(cv_mem, ck_mem.ck_znQ[0]);
	      if (flag != CV_SUCCESS) return(flag);
	    }

	    if (ck_mem.ck_sensi) {
	      flag = CVodeSensReInit(cv_mem, cv_mem.cv_ism, ck_mem.ck_znS[0]);
	      if (flag != CV_SUCCESS) return(flag);
	    }

	    if (ck_mem.ck_quadr_sensi) {
	      flag = CVodeQuadSensReInit(cv_mem, ck_mem.ck_znQS[0]);
	      if (flag != CV_SUCCESS) return(flag);
	    }

	  } else {
	    
	    qmax = cv_mem.cv_qmax;

	    /* Copy parameters from check point data structure */

	    cv_mem.cv_nst       = ck_mem.ck_nst;
	    cv_mem.cv_tretlast  = ck_mem.ck_tretlast;
	    cv_mem.cv_q         = ck_mem.ck_q;
	    cv_mem.cv_qprime    = ck_mem.ck_qprime;
	    cv_mem.cv_qwait     = ck_mem.ck_qwait;
	    cv_mem.cv_L         = ck_mem.ck_L;
	    cv_mem.cv_gammap    = ck_mem.ck_gammap;
	    cv_mem.cv_h         = ck_mem.ck_h;
	    cv_mem.cv_hprime    = ck_mem.ck_hprime;
	    cv_mem.cv_hscale    = ck_mem.ck_hscale;
	    cv_mem.cv_eta       = ck_mem.ck_eta;
	    cv_mem.cv_etamax    = ck_mem.ck_etamax;
	    cv_mem.cv_tn        = ck_mem.ck_t0;
	    cv_mem.cv_saved_tq5 = ck_mem.ck_saved_tq5;
	    
	    /* Copy the arrays from check point data structure */

	    for (j=0; j<=cv_mem.cv_q; j++) N_VScale_Serial(ONE, ck_mem.ck_zn[j], cv_mem.cv_zn[j]);
	    if ( cv_mem.cv_q < qmax ) N_VScale_Serial(ONE, ck_mem.ck_zn[qmax], cv_mem.cv_zn[qmax]);

	    if (ck_mem.ck_quadr) {
	      for (j=0; j<=cv_mem.cv_q; j++) N_VScale_Serial(ONE, ck_mem.ck_znQ[j], cv_mem.cv_znQ[j]);
	      if ( cv_mem.cv_q < qmax ) N_VScale_Serial(ONE, ck_mem.ck_znQ[qmax], cv_mem.cv_znQ[qmax]);
	    }

	    if (ck_mem.ck_sensi) {
	      for (is=0; is<cv_mem.cv_Ns; is++) {
	        for (j=0; j<=cv_mem.cv_q; j++) N_VScale_Serial(ONE, ck_mem.ck_znS[j][is], cv_mem.cv_znS[j][is]);
	        if ( cv_mem.cv_q < qmax ) N_VScale_Serial(ONE, ck_mem.ck_znS[qmax][is], cv_mem.cv_znS[qmax][is]);
	      }
	    }

	    if (ck_mem.ck_quadr_sensi) {
	      for (is=0; is<cv_mem.cv_Ns; is++) {
	        for (j=0; j<=cv_mem.cv_q; j++) N_VScale_Serial(ONE, ck_mem.ck_znQS[j][is], cv_mem.cv_znQS[j][is]);
	        if ( cv_mem.cv_q < qmax ) N_VScale_Serial(ONE, ck_mem.ck_znQS[qmax][is], cv_mem.cv_znQS[qmax][is]);
	      }
	    }

	    for (j=0; j<=L_MAX; j++)        cv_mem.cv_tau[j] = ck_mem.ck_tau[j];
	    for (j=0; j<=NUM_TESTS; j++)    cv_mem.cv_tq[j] = ck_mem.ck_tq[j];
	    for (j=0; j<=cv_mem.cv_q; j++) cv_mem.cv_l[j] = ck_mem.ck_l[j];
	    
	    /* Force a call to setup */

	    cv_mem.cv_forceSetup = true;

	  }

	  return(CV_SUCCESS);
	}
	
	/*
	 * CVodeReInit
	 *
	 * CVodeReInit re-initializes CVODES's memory for a problem, assuming
	 * it has already been allocated in a prior CVodeInit call.
	 * All problem specification inputs are checked for errors.
	 * If any error occurs during initialization, it is reported to the
	 * file whose file pointer is errfp.
	 * The return value is CV_SUCCESS = 0 if no errors occurred, or
	 * a negative value otherwise.
	 */

	private int CVodeReInit(CVodeMemRec cv_mem, double t0, NVector y0)
	{
	  int i,k;
	 
	  /* Check cvode_mem */

	  if (cv_mem==null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeReInit",
	                   MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Check if cvode_mem was allocated */

	  if (cv_mem.cv_MallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_MALLOC, "CVODES", "CVodeReInit",
	                   MSGCV_NO_MALLOC);
	    return(CV_NO_MALLOC);
	  }

	  /* Check for legal input parameters */

	  if (y0 == null) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVodeReInit",
	                   MSGCV_NULL_Y0);
	    return(CV_ILL_INPUT);
	  }
	  
	  /* Copy the input parameters into CVODES state */

	  cv_mem.cv_tn = t0;

	  /* Set step parameters */

	  cv_mem.cv_q      = 1;
	  cv_mem.cv_L      = 2;
	  cv_mem.cv_qwait  = cv_mem.cv_L;
	  cv_mem.cv_etamax = ETAMX1;

	  cv_mem.cv_qu     = 0;
	  cv_mem.cv_hu     = ZERO;
	  cv_mem.cv_tolsf  = ONE;

	  /* Set forceSetup to SUNFALSE */

	  cv_mem.cv_forceSetup = false;

	  /* Initialize zn[0] in the history array */

	  N_VScale_Serial(ONE, y0, cv_mem.cv_zn[0]);
	 
	  /* Initialize all the counters */

	  cv_mem.cv_nst     = 0;
	  cv_mem.cv_nfe     = 0;
	  cv_mem.cv_ncfn[0]    = 0;
	  cv_mem.cv_netf[0]    = 0;
	  cv_mem.cv_nni     = 0;
	  cv_mem.cv_nsetups = 0;
	  cv_mem.cv_nhnil   = 0;
	  cv_mem.cv_nstlp   = 0;
	  cv_mem.cv_nscon   = 0;
	  cv_mem.cv_nge     = 0;

	  cv_mem.cv_irfnd   = 0;

	  /* Initialize other integrator optional outputs */

	  cv_mem.cv_h0u      = ZERO;
	  cv_mem.cv_next_h   = ZERO;
	  cv_mem.cv_next_q   = 0;

	  /* Initialize Stablilty Limit Detection data */

	  cv_mem.cv_nor = 0;
	  for (i = 1; i <= 5; i++)
	    for (k = 1; k <= 3; k++) 
	      cv_mem.cv_ssdat[i-1][k-1] = ZERO;
	  
	  /* Problem has been successfully re-initialized */

	  return(CV_SUCCESS);
	}

	/*
	 * CVodeQuadReInit
	 *
	 * CVodeQuadReInit re-initializes CVODES's quadrature related memory 
	 * for a problem, assuming it has already been allocated in prior 
	 * calls to CVodeInit and CVodeQuadInit. 
	 * All problem specification inputs are checked for errors.
	 * If any error occurs during initialization, it is reported to the
	 * file whose file pointer is errfp.
	 * The return value is CV_SUCCESS = 0 if no errors occurred, or
	 * a negative value otherwise.
	 */

	private int CVodeQuadReInit(CVodeMemRec cv_mem, NVector yQ0)
	{

	  /* Check cvode_mem */
	  if (cv_mem==null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODES",
	                   "CVodeQuadReInit", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Ckeck if quadrature was initialized? */
	  if (cv_mem.cv_QuadMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_QUAD, "CVODES",
	                   "CVodeQuadReInit", MSGCV_NO_QUAD);
	    return(CV_NO_QUAD);
	  }

	  /* Initialize znQ[0] in the history array */
	  N_VScale_Serial(ONE, yQ0, cv_mem.cv_znQ[0]);

	  /* Initialize counters */
	  cv_mem.cv_nfQe  = 0;
	  cv_mem.cv_netfQ[0] = 0;

	  /* Quadrature integration turned ON */
	  cv_mem.cv_quadr = true;

	  /* Quadrature re-initialization was successfull */
	  return(CV_SUCCESS);
	}

	/*
	 * CVodeSensReInit
	 *
	 * CVodeSensReInit re-initializes CVODES's sensitivity related memory 
	 * for a problem, assuming it has already been allocated in prior 
	 * calls to CVodeInit and CVodeSensInit/CVodeSensInit1. 
	 * All problem specification inputs are checked for errors.
	 * The number of sensitivities Ns is assumed to be unchanged since
	 * the previous call to CVodeSensInit.
	 * If any error occurs during initialization, it is reported to the
	 * file whose file pointer is errfp.
	 * The return value is CV_SUCCESS = 0 if no errors occurred, or
	 * a negative value otherwise.
	 */ 

	private int CVodeSensReInit(CVodeMemRec cv_mem, int ism, NVector yS0[])
	{
	  int is;  

	  /* Check cvode_mem */

	  if (cv_mem==null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeSensReInit",
	                   MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Was sensitivity initialized? */

	  if (cv_mem.cv_SensMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_SENS, "CVODES", "CVodeSensReInit",
	                   MSGCV_NO_SENSI);
	    return(CV_NO_SENS);
	  } 

	  /* Check if ism is compatible */

	  if ((cv_mem.cv_ifS==CV_ALLSENS) && (ism==CV_STAGGERED1)) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES",
	                   "CVodeSensReInit", MSGCV_BAD_ISM_IFS);
	    return(CV_ILL_INPUT);
	  }
	  
	  /* Check if ism is legal */

	  if ((ism!=CV_SIMULTANEOUS) && (ism!=CV_STAGGERED) && (ism!=CV_STAGGERED1)) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES",
	                   "CVodeSensReInit", MSGCV_BAD_ISM);
	    return(CV_ILL_INPUT);
	  }
	  cv_mem.cv_ism = ism;

	  /* Check if yS0 is non-null */

	  if (yS0 == null) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES",
	                   "CVodeSensReInit", MSGCV_NULL_YS0);
	    return(CV_ILL_INPUT);
	  }  

	  /* Allocate ncfS1, ncfnS1, and nniS1 if needed */

	  if ( (ism==CV_STAGGERED1) && (cv_mem.cv_stgr1alloc==false) ) {
	    cv_mem.cv_stgr1alloc = true;
	    cv_mem.cv_ncfS1 = null;
	    cv_mem.cv_ncfS1 = new int[cv_mem.cv_Ns][1];
	    cv_mem.cv_ncfnS1 = null;
	    cv_mem.cv_ncfnS1 = new long[cv_mem.cv_Ns][1];
	    cv_mem.cv_nniS1 = null;
	    cv_mem.cv_nniS1 = new long[cv_mem.cv_Ns];
	    if ( (cv_mem.cv_ncfS1==null) ||
	         (cv_mem.cv_ncfnS1==null) ||
	         (cv_mem.cv_nniS1==null) ) {
	      cvProcessError(cv_mem, CV_MEM_FAIL, "CVODES",
	                     "CVodeSensReInit", MSGCV_MEM_FAIL);
	      return(CV_MEM_FAIL);
	    }
	  }

	  /*---------------------------------------------- 
	    All error checking is complete at this point 
	    -----------------------------------------------*/

	  /* Initialize znS[0] in the history array */

	  for (is=0; is<cv_mem.cv_Ns; is++) 
	    N_VScale_Serial(ONE, yS0[is], cv_mem.cv_znS[0][is]);

	  /* Initialize all sensitivity related counters */

	  cv_mem.cv_nfSe     = 0;
	  cv_mem.cv_nfeS     = 0;
	  cv_mem.cv_ncfnS[0]    = 0;
	  cv_mem.cv_netfS[0]    = 0;
	  cv_mem.cv_nniS     = 0;
	  cv_mem.cv_nsetupsS = 0;
	  if (ism==CV_STAGGERED1)
	    for (is=0; is<cv_mem.cv_Ns; is++) {
	      cv_mem.cv_ncfnS1[is][0] = 0;
	      cv_mem.cv_nniS1[is] = 0;
	    }

	  /* Problem has been successfully re-initialized */

	  cv_mem.cv_sensi = true;

	  return(CV_SUCCESS);
	}
	
	/*
	 * CVodeQuadSensReInit
	 *
	 */

	private int CVodeQuadSensReInit(CVodeMemRec cv_mem, NVector yQS0[])
	{
	  int is;

	  /* Check cvode_mem */
	  if (cv_mem==null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeQuadSensReInit",
	                   MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Check if sensitivity analysis is active */
	  if (!cv_mem.cv_sensi) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES",
	                   "CVodeQuadSensReInit", MSGCV_NO_SENSI);
	    return(CV_NO_SENS);
	  }

	  /* Was quadrature sensitivity initialized? */
	  if (cv_mem.cv_QuadSensMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_QUADSENS, "CVODES",
	                   "CVodeQuadSensReInit", MSGCV_NO_QUADSENSI);
	    return(CV_NO_QUADSENS);
	  } 

	  /* Check if yQS0 is non-null */
	  if (yQS0 == null) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES",
	                   "CVodeQuadSensReInit", MSGCV_NULL_YQS0);
	    return(CV_ILL_INPUT);
	  }

	  /*---------------------------------------------- 
	    All error checking is complete at this point 
	    -----------------------------------------------*/

	  /* Initialize znQS[0] in the history array */
	  for (is=0; is<cv_mem.cv_Ns; is++) 
	    N_VScale_Serial(ONE, yQS0[is], cv_mem.cv_znQS[0][is]);

	  /* Initialize all sensitivity related counters */
	  cv_mem.cv_nfQSe  = 0;
	  cv_mem.cv_nfQeS  = 0;
	  cv_mem.cv_netfQS[0] = 0;

	  /* Quadrature sensitivities will be computed */
	  cv_mem.cv_quadr_sensi = true;

	  /* Problem has been successfully re-initialized */
	  return(CV_SUCCESS);
	}

	private int CVodeGetB(CVodeMemRec cv_mem, int which, double tret[], NVector yB)
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem;

	  /* Check if cvode_mem exists */
	  if (cv_mem == null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeGetB", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Was ASA initialized? */
	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeGetB", MSGCV_NO_ADJ);
	    return(CV_NO_ADJ);
	  } 

	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check the value of which */
	  if ( which >= ca_mem.ca_nbckpbs ) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeGetB", MSGCV_BAD_WHICH);
	    return(CV_ILL_INPUT);
	  }

	  /* Find the CVodeBMem entry in the linked list corresponding to which */
	  cvB_mem = ca_mem.cvB_mem;
	  while (cvB_mem != null) {
	    if ( which == cvB_mem.cv_index ) break;
	    cvB_mem = cvB_mem.cv_next;
	  } 

	  N_VScale_Serial(ONE, cvB_mem.cv_y, yB);
	  tret[0] = cvB_mem.cv_tout;

	  return(CV_SUCCESS);
	}
	
	/*
	 * CVodeGetAdjY
	 *
	 * This routine returns the interpolated forward solution at time t.
	 * The user must allocate space for y.
	 */

	private int CVodeGetAdjY(CVodeMemRec cv_mem, double t, NVector y)
	{
	  CVadjMemRec ca_mem;
	  int flag;

	  if (cv_mem == null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeGetAdjY", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  ca_mem = cv_mem.cv_adj_mem;

	  if (ca_mem.ca_IMget == CVAhermiteGetY_select) {
	        flag = CVAhermiteGetY(cv_mem, t, y, null);
	  }
	  else {
		    flag = CVApolynomialGetY(cv_mem, t, y, null);
	  }
	  
	  return(flag);
	}
	
	/*
	 * CVodeGetAdjCVodeBmem
	 *
	 * This function returns a (void *) pointer to the CVODES     
	 * memory allocated for the backward problem. This pointer can    
	 * then be used to call any of the CVodeGet* CVODES routines to  
	 * extract optional output for the backward integration phase.
	 */

	private CVodeMemRec CVodeGetAdjCVodeBmem(CVodeMemRec cv_mem, int which)
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem;
	  CVodeMemRec cvodeB_mem;

	  /* Check if cvode_mem exists */
	  if (cv_mem == null) {
	    cvProcessError(null, 0, "CVODEA", "CVodeGetAdjCVodeBmem", MSGCV_NO_MEM);
	    return(null);
	  }

	  /* Was ASA initialized? */
	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, 0, "CVODEA", "CVodeGetAdjCVodeBmem", MSGCV_NO_ADJ);
	    return(null);
	  } 
	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check which */
	  if ( which >= ca_mem.ca_nbckpbs ) {
	    cvProcessError(cv_mem, 0, "CVODEA", "CVodeGetAdjCVodeBmem", MSGCV_BAD_WHICH);
	    return(null);
	  }

	  /* Find the CVodeBMem entry in the linked list corresponding to which */
	  cvB_mem = ca_mem.cvB_mem;
	  while (cvB_mem != null) {
	    if ( which == cvB_mem.cv_index ) break;
	    cvB_mem = cvB_mem.cv_next;
	  }

	  cvodeB_mem = cvB_mem.cv_mem;

	  return(cvodeB_mem);
	}

	/*
	 * CVodeGetNumSteps
	 *
	 * Returns the current number of integration steps
	 */

	private int CVodeGetNumSteps(CVodeMemRec cv_mem, long nsteps[])
	{

	  if (cv_mem== null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeGetNumSteps", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  nsteps[0] = cv_mem.cv_nst;

	  return(CV_SUCCESS);
	}

	/*
	 * CVodeGetQuadB
	 */

	private int CVodeGetQuadB(CVodeMemRec cv_mem, int which, double tret[], NVector qB)
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem;
	  CVodeMemRec cvodeB_mem;
	  long nstB[] = new long[1];
	  int flag;

	  /* Check if cvode_mem exists */
	  if (cv_mem == null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeGetQuadB", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Was ASA initialized? */
	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeGetQuadB", MSGCV_NO_ADJ);
	    return(CV_NO_ADJ);
	  } 

	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check the value of which */
	  if ( which >= ca_mem.ca_nbckpbs ) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeGetQuadB", MSGCV_BAD_WHICH);
	    return(CV_ILL_INPUT);
	  }

	  /* Find the CVodeBMem entry in the linked list corresponding to which */
	  cvB_mem = ca_mem.cvB_mem;
	  while (cvB_mem != null) {
	    if ( which == cvB_mem.cv_index ) break;
	    cvB_mem = cvB_mem.cv_next;
	  } 

	  cvodeB_mem = cvB_mem.cv_mem;

	  /* If the integration for this backward problem has not started yet,
	   * simply return the current value of qB (i.e. the final conditions) */

	  flag = CVodeGetNumSteps(cvodeB_mem, nstB);
	  
	  if (nstB[0] == 0) {
	    N_VScale_Serial(ONE, cvB_mem.cv_mem.cv_znQ[0], qB);
	    tret[0] = cvB_mem.cv_tout;
	  } else {
	    flag = CVodeGetQuad(cvodeB_mem, tret, qB);
	  }

	  return(flag);
	}
	
		private int CVodeReInitB(CVodeMemRec cv_mem, int which,
	            double tB0, NVector yB0)
	{
	CVadjMemRec ca_mem;
	CVodeBMemRec cvB_mem;
	CVodeMemRec cvodeB_mem;
	int flag;
	
	/* Check if cvode_mem exists */
	if (cv_mem == null) {
	cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeReInitB", MSGCV_NO_MEM);
	return(CV_MEM_NULL);
	}
	
	/* Was ASA initialized? */
	if (cv_mem.cv_adjMallocDone == false) {
	cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeReInitB", MSGCV_NO_ADJ);
	return(CV_NO_ADJ);
	}
	ca_mem = cv_mem.cv_adj_mem;
	
	/* Check the value of which */
	if ( which >= ca_mem.ca_nbckpbs ) {
	cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeReInitB", MSGCV_BAD_WHICH);
	return(CV_ILL_INPUT);
	}
	
	/* Find the CVodeBMem entry in the linked list corresponding to which */
	cvB_mem = ca_mem.cvB_mem;
	while (cvB_mem != null) {
	if ( which == cvB_mem.cv_index ) break;
	cvB_mem = cvB_mem.cv_next;
	}
	
	cvodeB_mem = cvB_mem.cv_mem;
	
	/* Reinitialize CVODES object */
	
	flag = CVodeReInit(cvodeB_mem, tB0, yB0);
	
	return(flag);
	}

	private int CVodeQuadReInitB(CVodeMemRec cv_mem, int which, NVector yQB0)
	{
	  CVadjMemRec ca_mem;
	  CVodeBMemRec cvB_mem;
	  CVodeMemRec cvodeB_mem;
	  int flag;

	  /* Check if cvode_mem exists */
	  if (cv_mem == null) {
	    cvProcessError(null, CV_MEM_NULL, "CVODEA", "CVodeQuadReInitB", MSGCV_NO_MEM);
	    return(CV_MEM_NULL);
	  }

	  /* Was ASA initialized? */
	  if (cv_mem.cv_adjMallocDone == false) {
	    cvProcessError(cv_mem, CV_NO_ADJ, "CVODEA", "CVodeQuadReInitB", MSGCV_NO_ADJ);
	    return(CV_NO_ADJ);
	  } 
	  ca_mem = cv_mem.cv_adj_mem;

	  /* Check the value of which */
	  if ( which >= ca_mem.ca_nbckpbs ) {
	    cvProcessError(cv_mem, CV_ILL_INPUT, "CVODEA", "CVodeQuadReInitB", MSGCV_BAD_WHICH);
	    return(CV_ILL_INPUT);
	  }

	  /* Find the CVodeBMem entry in the linked list corresponding to which */
	  cvB_mem = ca_mem.cvB_mem;
	  while (cvB_mem != null) {
	    if ( which == cvB_mem.cv_index ) break;
	    cvB_mem = cvB_mem.cv_next;
	  }

	  cvodeB_mem = cvB_mem.cv_mem;

	  flag = CVodeQuadReInit(cvodeB_mem, yQB0);
	  if (flag != CV_SUCCESS) return(flag);

	  return(CV_SUCCESS);
	}

}