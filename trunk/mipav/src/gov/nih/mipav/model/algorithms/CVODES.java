package gov.nih.mipav.model.algorithms;

import java.io.RandomAccessFile;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

public abstract class CVODES {
	// This is a port of code from CVODES, a stiff and non-stiff ODE solver, from C to Java
	/*Copyright (c) 2002-2016, Lawrence Livermore National Security. 
	Produced at the Lawrence Livermore National Laboratory.
	Written by A.C. Hindmarsh, D.R. Reynolds, R. Serban, C.S. Woodward,
	S.D. Cohen, A.G. Taylor, S. Peles, L.E. Banks, and D. Shumaker.
	LLNL-CODE-667205    (ARKODE)
	UCRL-CODE-155951    (CVODE)
	UCRL-CODE-155950    (CVODES)
	UCRL-CODE-155952    (IDA)
	UCRL-CODE-237203    (IDAS)
	LLNL-CODE-665877    (KINSOL)
	All rights reserved. 

	This file is part of SUNDIALS.  For details, 
	see http://computation.llnl.gov/projects/sundials

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions
	are met:

	1. Redistributions of source code must retain the above copyright
	notice, this list of conditions and the disclaimer below.

	2. Redistributions in binary form must reproduce the above copyright
	notice, this list of conditions and the disclaimer (as noted below)
	in the documentation and/or other materials provided with the
	distribution.

	3. Neither the name of the LLNS/LLNL nor the names of its contributors
	may be used to endorse or promote products derived from this software
	without specific prior written permission.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
	"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
	LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
	FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL 
	LAWRENCE LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF 
	ENERGY OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
	SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
	TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
	DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
	THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

	Additional BSD Notice
	---------------------
	1. This notice is required to be provided under our contract with
	the U.S. Department of Energy (DOE). This work was produced at
	Lawrence Livermore National Laboratory under Contract 
	No. DE-AC52-07NA27344 with the DOE.

	2. Neither the United States Government nor Lawrence Livermore 
	National Security, LLC nor any of their employees, makes any warranty, 
	express or implied, or assumes any liability or responsibility for the
	accuracy, completeness, or usefulness of any */
	
	/* Basic CVODES constants */

	final int ADAMS_Q_MAX = 12;      /* max value of q for lmm == ADAMS    */
	final int BDF_Q_MAX = 5;      /* max value of q for lmm == BDF      */
	final int  Q_MAX = ADAMS_Q_MAX;  /* max value of q for either lmm      */
	final int L_MAX  = (Q_MAX+1);    /* max value of L for either lmm      */
	final int NUM_TESTS = 5;      /* number of error test quantities    */
	
    double DBL_EPSILON;
    double UNIT_ROUNDOFF;



	 // lmm:   The user of the CVODES package specifies whether to use
	 // the CV_ADAMS or CV_BDF (backward differentiation formula)
	 // linear multistep method. The BDF method is recommended
	 // for stiff problems, and the CV_ADAMS method is recommended
	 // for nonstiff problems.
	final int CV_ADAMS = 1;
	final int CV_BDF = 2;
	
	//iter:  At each internal time step, a nonlinear equation must
	//        be solved. The user can specify either CV_FUNCTIONAL
	//        iteration, which does not require linear algebra, or a
	//        CV_NEWTON iteration, which requires the solution of linear
	//        systems. In the CV_NEWTON case, the user also specifies a
	//        CVODE linear solver. CV_NEWTON is recommended in case of
	//        stiff problems.
	final int CV_FUNCTIONAL = 1;
	final int CV_NEWTON = 2;
	
	/*
	 * Control constants for type of sensitivity RHS
	 * ---------------------------------------------
	 */

	final int CV_ONESENS = 1;
	final int CV_ALLSENS = 2;

	
	/*
	 * Control constants for tolerances
	 * --------------------------------
	 */

	final int CV_NN = 0;
	final int CV_SS = 1;
	final int CV_SV = 2;
	final int CV_WF = 3;
	final int CV_EE = 4;
	
	/* DQtype */
	final int CV_CENTERED = 1;
	final int CV_FORWARD = 2;
	
	/* 
	 * ----------------------------------------
	 * CVODES return flags
	 * ----------------------------------------
	 */

	final int  CV_SUCCESS = 0;
	/*#define CV_TSTOP_RETURN          1
	#define CV_ROOT_RETURN           2

	#define CV_WARNING              99

	#define CV_TOO_MUCH_WORK        -1
	#define CV_TOO_MUCH_ACC         -2
	#define CV_ERR_FAILURE          -3
	#define CV_CONV_FAILURE         -4

	#define CV_LINIT_FAIL           -5
	#define CV_LSETUP_FAIL          -6
	#define CV_LSOLVE_FAIL          -7
	#define CV_RHSFUNC_FAIL         -8
	#define CV_FIRST_RHSFUNC_ERR    -9
	#define CV_REPTD_RHSFUNC_ERR    -10
	#define CV_UNREC_RHSFUNC_ERR    -11
	#define CV_RTFUNC_FAIL          -12

	#define CV_MEM_FAIL             -20
	#define CV_MEM_NULL             -21
	#define CV_ILL_INPUT            -22
	#define CV_NO_MALLOC            -23
	#define CV_BAD_K                -24
	#define CV_BAD_T                -25
	#define CV_BAD_DKY              -26
	#define CV_TOO_CLOSE            -27

	#define CV_NO_QUAD              -30
	#define CV_QRHSFUNC_FAIL        -31
	#define CV_FIRST_QRHSFUNC_ERR   -32
	#define CV_REPTD_QRHSFUNC_ERR   -33
	#define CV_UNREC_QRHSFUNC_ERR   -34

	#define CV_NO_SENS              -40
	#define CV_SRHSFUNC_FAIL        -41
	#define CV_FIRST_SRHSFUNC_ERR   -42
	#define CV_REPTD_SRHSFUNC_ERR   -43
	#define CV_UNREC_SRHSFUNC_ERR   -44

	#define CV_BAD_IS               -45

	#define CV_NO_QUADSENS          -50
	#define CV_QSRHSFUNC_FAIL       -51
	#define CV_FIRST_QSRHSFUNC_ERR  -52
	#define CV_REPTD_QSRHSFUNC_ERR  -53
	#define CV_UNREC_QSRHSFUNC_ERR  -54*/

	final int CV_MEM_NULL = -21;
	final int CV_ILL_INPUT = -22;



	final double HMIN_DEFAULT = 0.0;    /* hmin default value     */
	final double HMAX_INV_DEFAULT = 0.0;    /* hmax_inv default value */
	final int MXHNIL_DEFAULT =  10;             /* mxhnil default value   */
	final long MXSTEP_DEFAULT = 500L;            /* mxstep default value   */
	final int NLS_MAXCOR = 3;
	final int MXNCF = 10;
	final int MXNEF = 7;
	final double CORTES = 0.1;
	final double ZERO = 0.0;
	
	final String MSG_TIME = "t = %lg";
	final String MSG_TIME_H = "t = %lg and h = %lg";
	final String MSG_TIME_INT = "t = %lg is not between tcur - hu = %lg and tcur = %lg.";
	final String MSG_TIME_TOUT = "tout = %lg";
	final String MSG_TIME_TSTOP = "tstop = %lg";

	
	/* Initialization and I/O error messages */

	final String MSGCV_NO_MEM = "cvode_mem = NULL illegal.";
	final String MSGCV_CVMEM_FAIL = "Allocation of cvode_mem failed.";
	final String MSGCV_MEM_FAIL = "A memory request failed.";
	final String MSGCV_BAD_LMM = "Illegal value for lmm. The legal values are CV_ADAMS and CV_BDF.";
	final String MSGCV_BAD_ITER = "Illegal value for iter. The legal values are CV_FUNCTIONAL and CV_NEWTON.";
	final String MSGCV_NO_MALLOC = "Attempt to call before CVodeInit.";
	final String MSGCV_NEG_MAXORD = "maxord <= 0 illegal.";
	final String MSGCV_BAD_MAXORD = "Illegal attempt to increase maximum method order.";
	final String MSGCV_SET_SLDET = "Attempt to use stability limit detection with the CV_ADAMS method illegal.";
	final String MSGCV_NEG_HMIN = "hmin < 0 illegal.";
	final String MSGCV_NEG_HMAX = "hmax < 0 illegal.";
	final String MSGCV_BAD_HMIN_HMAX = "Inconsistent step size limits: hmin > hmax.";
	final String MSGCV_BAD_RELTOL = "reltol < 0 illegal.";
	final String MSGCV_BAD_ABSTOL = "abstol has negative component(s) (illegal).";
	final String MSGCV_NULL_ABSTOL = "abstol = NULL illegal.";
	final String MSGCV_NULL_Y0 = "y0 = NULL illegal.";
	final String MSGCV_NULL_F = "f = NULL illegal.";
	final String MSGCV_NULL_G = "g = NULL illegal.";
	final String MSGCV_BAD_NVECTOR = "A required vector operation is not implemented.";
	final String MSGCV_BAD_K = "Illegal value for k.";
	final String MSGCV_NULL_DKY = "dky = NULL illegal.";
	final String MSGCV_BAD_T = "Illegal value for t.";
	final String MSGCV_NO_ROOT = "Rootfinding was not initialized.";

	final String MSGCV_NO_QUAD  = "Quadrature integration not activated.";
	final String MSGCV_BAD_ITOLQ = "Illegal value for itolQ. The legal values are CV_SS and CV_SV.";
	final String MSGCV_NULL_ABSTOLQ = "abstolQ = NULL illegal.";
	final String MSGCV_BAD_RELTOLQ = "reltolQ < 0 illegal.";
	final String MSGCV_BAD_ABSTOLQ = "abstolQ has negative component(s) (illegal).";  

	final String MSGCV_SENSINIT_2 = "Sensitivity analysis already initialized.";
	final String MSGCV_NO_SENSI  = "Forward sensitivity analysis not activated.";
	final String MSGCV_BAD_ITOLS = "Illegal value for itolS. The legal values are CV_SS, CV_SV, and CV_EE.";
	final String MSGCV_NULL_ABSTOLS = "abstolS = NULL illegal.";
	final String MSGCV_BAD_RELTOLS = "reltolS < 0 illegal.";
	final String MSGCV_BAD_ABSTOLS = "abstolS has negative component(s) (illegal).";  
	final String MSGCV_BAD_PBAR = "pbar has zero component(s) (illegal).";
	final String MSGCV_BAD_PLIST = "plist has negative component(s) (illegal).";
	final String MSGCV_BAD_NS = "NS <= 0 illegal.";
	final String MSGCV_NULL_YS0 = "yS0 = NULL illegal.";
	final String MSGCV_BAD_ISM = "Illegal value for ism. Legal values are: CV_SIMULTANEOUS, CV_STAGGERED and CV_STAGGERED1.";
	final String MSGCV_BAD_IFS = "Illegal value for ifS. Legal values are: CV_ALLSENS and CV_ONESENS.";
	final String MSGCV_BAD_ISM_IFS = "Illegal ism = CV_STAGGERED1 for CVodeSensInit.";
	final String MSGCV_BAD_IS = "Illegal value for is.";
	final String MSGCV_NULL_DKYA = "dkyA = NULL illegal.";
	final String MSGCV_BAD_DQTYPE = "Illegal value for DQtype. Legal values are: CV_CENTERED and CV_FORWARD.";
	final String MSGCV_BAD_DQRHO = "DQrhomax < 0 illegal.";

	final String MSGCV_BAD_ITOLQS = "Illegal value for itolQS. The legal values are CV_SS, CV_SV, and CV_EE.";
	final String MSGCV_NULL_ABSTOLQS = "abstolQS = NULL illegal.";
	final String MSGCV_BAD_RELTOLQS = "reltolQS < 0 illegal.";
	final String MSGCV_BAD_ABSTOLQS = "abstolQS has negative component(s) (illegal).";  
	final String MSGCV_NO_QUADSENSI = "Forward sensitivity analysis for quadrature variables not activated.";
	final String MSGCV_NULL_YQS0 = "yQS0 = NULL illegal.";

	/* CVode Error Messages */

	final String MSGCV_NO_TOL = "No integration tolerances have been specified.";
	final String MSGCV_LSOLVE_NULL = "The linear solver's solve routine is NULL.";
	final String MSGCV_YOUT_NULL = "yout = NULL illegal.";
	final String MSGCV_TRET_NULL = "tret = NULL illegal.";
	final String MSGCV_BAD_EWT = "Initial ewt has component(s) equal to zero (illegal).";
	final String MSGCV_EWT_NOW_BAD = "At " + MSG_TIME + ", a component of ewt has become <= 0.";
	final String MSGCV_BAD_ITASK = "Illegal value for itask.";
	final String MSGCV_BAD_H0 = "h0 and tout - t0 inconsistent.";
	final String MSGCV_BAD_TOUT = "Trouble interpolating at " + MSG_TIME_TOUT + ". tout too far back in direction of integration";
	final String MSGCV_EWT_FAIL = "The user-provide EwtSet function failed.";
	final String MSGCV_EWT_NOW_FAIL = "At " + MSG_TIME + ", the user-provide EwtSet function failed.";
	final String MSGCV_LINIT_FAIL = "The linear solver's init routine failed.";
	final String MSGCV_HNIL_DONE = "The above warning has been issued mxhnil times and will not be issued again for this problem.";
	final String MSGCV_TOO_CLOSE = "tout too close to t0 to start integration.";
	final String MSGCV_MAX_STEPS = "At " + MSG_TIME + ", mxstep steps taken before reaching tout.";
	final String MSGCV_TOO_MUCH_ACC = "At " + MSG_TIME + ", too much accuracy requested.";
	final String MSGCV_HNIL = "Internal " + MSG_TIME_H + " are such that t + h = t on the next step. The solver will continue anyway.";
	final String MSGCV_ERR_FAILS = "At " + MSG_TIME_H + ", the error test failed repeatedly or with |h| = hmin.";
	final String MSGCV_CONV_FAILS = "At " + MSG_TIME_H + ", the corrector convergence test failed repeatedly or with |h| = hmin.";
	final String MSGCV_SETUP_FAILED = "At " + MSG_TIME + ", the setup routine failed in an unrecoverable manner.";
	final String MSGCV_SOLVE_FAILED = "At " + MSG_TIME + ", the solve routine failed in an unrecoverable manner.";
	final String MSGCV_RHSFUNC_FAILED = "At " + MSG_TIME + ", the right-hand side routine failed in an unrecoverable manner.";
	final String MSGCV_RHSFUNC_UNREC = "At " + MSG_TIME + ", the right-hand side failed in a recoverable manner, but no recovery is possible.";
	final String MSGCV_RHSFUNC_REPTD = "At " + MSG_TIME + " repeated recoverable right-hand side function errors.";
	final String MSGCV_RHSFUNC_FIRST = "The right-hand side routine failed at the first call.";
	final String MSGCV_RTFUNC_FAILED = "At " + MSG_TIME + ", the rootfinding routine failed in an unrecoverable manner.";
	final String MSGCV_CLOSE_ROOTS = "Root found at and very near " + MSG_TIME + ".";
	final String MSGCV_BAD_TSTOP = "The value " + MSG_TIME_TSTOP + " is behind current " + MSG_TIME + " in the direction of integration.";
	final String MSGCV_INACTIVE_ROOTS = "At the end of the first step, there are still some root functions identically 0. This warning will not be issued again.";

	final String MSGCV_NO_TOLQ = "No integration tolerances for quadrature variables have been specified.";
	final String MSGCV_BAD_EWTQ = "Initial ewtQ has component(s) equal to zero (illegal).";
	final String MSGCV_EWTQ_NOW_BAD = "At " + MSG_TIME + ", a component of ewtQ has become <= 0.";
	final String MSGCV_QRHSFUNC_FAILED = "At " + MSG_TIME + ", the quadrature right-hand side routine failed in an unrecoverable manner.";
	final String MSGCV_QRHSFUNC_UNREC = "At " + MSG_TIME + ", the quadrature right-hand side failed in a recoverable manner, but no recovery is possible.";
	final String MSGCV_QRHSFUNC_REPTD = "At " + MSG_TIME + " repeated recoverable quadrature right-hand side function errors.";
	final String MSGCV_QRHSFUNC_FIRST = "The quadrature right-hand side routine failed at the first call.";

	final String MSGCV_NO_TOLS = "No integration tolerances for sensitivity variables have been specified.";
	final String MSGCV_NULL_P = "p = NULL when using internal DQ for sensitivity RHS illegal.";
	final String MSGCV_BAD_EWTS = "Initial ewtS has component(s) equal to zero (illegal).";
	final String MSGCV_EWTS_NOW_BAD = "At " + MSG_TIME + ", a component of ewtS has become <= 0.";
	final String MSGCV_SRHSFUNC_FAILED = "At " + MSG_TIME + ", the sensitivity right-hand side routine failed in an unrecoverable manner.";
	final String MSGCV_SRHSFUNC_UNREC = "At " + MSG_TIME + ", the sensitivity right-hand side failed in a recoverable manner, but no recovery is possible.";
	final String MSGCV_SRHSFUNC_REPTD = "At " + MSG_TIME + " repeated recoverable sensitivity right-hand side function errors.";
	final String MSGCV_SRHSFUNC_FIRST = "The sensitivity right-hand side routine failed at the first call.";

	final String MSGCV_NULL_FQ = "CVODES is expected to use DQ to evaluate the RHS of quad. sensi., but quadratures were not initialized.";
	final String MSGCV_NO_TOLQS = "No integration tolerances for quadrature sensitivity variables have been specified.";
	final String MSGCV_BAD_EWTQS = "Initial ewtQS has component(s) equal to zero (illegal).";
	final String MSGCV_EWTQS_NOW_BAD = "At " + MSG_TIME + ", a component of ewtQS has become <= 0.";
	final String MSGCV_QSRHSFUNC_FAILED = "At " + MSG_TIME + ", the quadrature sensitivity right-hand side routine failed in an unrecoverable manner.";
	final String MSGCV_QSRHSFUNC_UNREC = "At " + MSG_TIME + ", the quadrature sensitivity right-hand side failed in a recoverable manner, but no recovery is possible.";
	final String MSGCV_QSRHSFUNC_REPTD = "At " + MSG_TIME + " repeated recoverable quadrature sensitivity right-hand side function errors.";
	final String MSGCV_QSRHSFUNC_FIRST = "The quadrature sensitivity right-hand side routine failed at the first call.";

	/* 
	 * =================================================================
	 *   C V O D E A    E R R O R    M E S S A G E S
	 * =================================================================
	 */

	final String MSGCV_NO_ADJ = "Illegal attempt to call before calling CVodeAdjMalloc.";
	final String MSGCV_BAD_STEPS = "Steps nonpositive illegal.";
	final String MSGCV_BAD_INTERP = "Illegal value for interp.";
	final String MSGCV_BAD_WHICH  = "Illegal value for which.";
	final String MSGCV_NO_BCK = "No backward problems have been defined yet.";
	final String MSGCV_NO_FWD = "Illegal attempt to call before calling CVodeF.";
	final String MSGCV_BAD_TB0 = "The initial time tB0 for problem %d is outside the interval over which the forward problem was solved.";
	final String MSGCV_BAD_SENSI = "At least one backward problem requires sensitivities, but they were not stored for interpolation.";
	final String MSGCV_BAD_ITASKB = "Illegal value for itaskB. Legal values are CV_NORMAL and CV_ONE_STEP.";
	final String MSGCV_BAD_TBOUT  = "The final time tBout is outside the interval over which the forward problem was solved.";
	final String MSGCV_BACK_ERROR  = "Error occured while integrating backward problem # %d"; 
	final String MSGCV_BAD_TINTERP = "Bad t = %g for interpolation.";
	final String MSGCV_WRONG_INTERP = "This function cannot be called for the specified interp type.";


    final int cvsRoberts_dns = 1;
    int problem = cvsRoberts_dns;
	
	
	public CVODES() {
		// eps returns the distance from 1.0 to the next larger double-precision
		// number, that is, eps = 2^-52.
		// epsilon = D1MACH(4)
		// Machine epsilon is the smallest positive epsilon such that
		// (1.0 + epsilon) != 1.0.
		// epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
		// epsilon = 2.2204460e-16
		// epsilon is called the largest relative spacing
		DBL_EPSILON = 1.0;
		double neweps = 1.0;

		while (true) {

			if (1.0 == (1.0 + neweps)) {
				break;
			} else {
				DBL_EPSILON = neweps;
				neweps = neweps / 2.0;
			}
		} // while(true)
		
	    UNIT_ROUNDOFF = DBL_EPSILON;
	    if (problem == cvsRoberts_dns) {
	    	runcvsRoberts_dns();
	    }
	    return;
	}
	
	private void runcvsRoberts_dns() {
		/* -----------------------------------------------------------------
		 * Programmer(s): Scott D. Cohen, Alan C. Hindmarsh and
		 *                Radu Serban @ LLNL
		 * -----------------------------------------------------------------
		 * Example problem:
		 * 
		 * The following is a simple example problem, with the coding
		 * needed for its solution by CVODE. The problem is from
		 * chemical kinetics, and consists of the following three rate
		 * equations:         
		 *    dy1/dt = -.04*y1 + 1.e4*y2*y3
		 *    dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*(y2)^2
		 *    dy3/dt = 3.e7*(y2)^2
		 * on the interval from t = 0.0 to t = 4.e10, with initial
		 * conditions: y1 = 1.0, y2 = y3 = 0. The problem is stiff.
		 * While integrating the system, we also use the rootfinding
		 * feature to find the points at which y1 = 1e-4 or at which
		 * y3 = 0.01. This program solves the problem with the BDF method,
		 * Newton iteration with the SUNDENSE dense linear solver, and a
		 * user-supplied Jacobian routine.
		 * It uses a scalar relative tolerance and a vector absolute
		 * tolerance. Output is printed in decades from t = .4 to t = 4.e10.
		 * Run statistics (optional outputs) are printed at the end.
		 * -----------------------------------------------------------------*/
		
		/** Problem Constants */
		final int NEQ = 3; // Number of equations
		final double Y1 = 1.0; // Initial y components
		final double Y2 = 0.0;
		final double Y3 = 0.0;
		final double RTOL = 1.0E-4; // scalar relative tolerance
		final double ATOL1 = 1.0E-8; // vector absolute tolerance components
		final double ATOL2 = 1.0E-14;
		final double ATOL3 = 1.0E-6;
		final double T0 = 0.0; // initial time
		final double T1 = 0.4; // first output time
		final double TMULT = 10.0; // output time factor
		final int NOUT = 12; // number of output times
		
		// Set initial conditions
		double y[] = new double[]{Y1,Y2,Y3};
		double reltol = RTOL; // Set the scalar relative tolerance
		// Set the vector absolute tolerance
		double abstol[] = new double[]{ATOL1,ATOL2,ATOL3};
		CVodeMemRec cvode_mem;
		int flag;
		
		// Call CVodeCreate to create the solver memory and specify the
		// Backward Differentiation Formula and the use of a Newton
		// iteration
		cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
		if (cvode_mem == null) {
		    return;	
		}
		flag = CVodeInit(cvode_mem, T0, y);
		
		// Call CVodeInit to initialize the integrator memory and specify the
		// user's right hand side function in y' = f(t,y), the initial time T0, and
		// the initial dependent variable vector y
		
	}
	
	private int f(double t, double y[], double ydot[]) {
		if (problem == 1) {
		    ydot[0] = -0.04*y[0] + 1.0E4*y[1]*y[2];
		    ydot[2] = 3.0E7*y[1]*y[1];
		    ydot[1] = -ydot[0] - ydot[2];
		}
		return 0;
	}
	
	// Types: struct CVodeMemRec, CVodeMem
	// -----------------------------------------------------------------
	// The type CVodeMem is type pointer to struct CVodeMemRec.
	// This structure contains fields to keep track of problem state.
	
	  
	private class CVodeMemRec {
	    
	  double cv_uround;         /* machine unit roundoff                         */   

	  /*-------------------------- 
	    Problem Specification Data 
	    --------------------------*/

	  //CVRhsFn cv_f;               /* y' = f(t,y(t))                                */
	  //void *cv_user_data;         /* user pointer passed to f                      */

	  int cv_lmm;                 /* lmm = ADAMS or BDF                            */
	  int cv_iter;                /* iter = FUNCTIONAL or NEWTON                   */

	  int cv_itol;                /* itol = CV_SS, CV_SV, or CV_WF, or CV_NN       */
	  double cv_reltol;         /* relative tolerance                            */
	  double cv_Sabstol;        /* scalar absolute tolerance                     */
	  double cv_Vabstol[];        /* vector absolute tolerance                     */
	  boolean cv_user_efun;   /* SUNTRUE if user sets efun                     */
	  //CVEwtFn cv_efun;            /* function to set ewt                           */
	  //void *cv_e_data;            /* user pointer passed to efun                   */

	  /*-----------------------
	    Quadrature Related Data 
	    -----------------------*/

	  boolean cv_quadr;       /* SUNTRUE if integrating quadratures            */

	  //CVQuadRhsFn cv_fQ;          /* q' = fQ(t, y(t))                              */

	  boolean cv_errconQ;     /* SUNTRUE if quadrs. are included in error test */

	  int cv_itolQ;               /* itolQ = CV_SS or CV_SV                        */
	  double cv_reltolQ;        /* relative tolerance for quadratures            */
	  double cv_SabstolQ;       /* scalar absolute tolerance for quadratures     */
	  double cv_VabstolQ[];       /* vector absolute tolerance for quadratures     */

	  /*------------------------
	    Sensitivity Related Data 
	    ------------------------*/

	  boolean cv_sensi;       /* SUNTRUE if computing sensitivities           */

	  int cv_Ns;                  /* Number of sensitivities                      */

	  int cv_ism;                 /* ism = SIMULTANEOUS or STAGGERED              */

	  //CVSensRhsFn cv_fS;          /* fS = (df/dy)*yS + (df/dp)                    */
	  //CVSensRhs1Fn cv_fS1;        /* fS1 = (df/dy)*yS_i + (df/dp)                 */
	  //void *cv_fS_data;           /* data pointer passed to fS                    */
	  boolean cv_fSDQ;        /* SUNTRUE if using internal DQ functions       */
	  int cv_ifS;                 /* ifS = ALLSENS or ONESENS                     */

	  double cv_p[];             /* parameters in f(t,y,p)                       */
	  double cv_pbar[];          /* scale factors for parameters                 */
	  int cv_plist[];              /* list of sensitivities                        */
	  int cv_DQtype;              /* central/forward finite differences           */
	  double cv_DQrhomax;       /* cut-off value for separate/simultaneous FD   */

	  boolean cv_errconS;     /* SUNTRUE if yS are considered in err. control */

	  int cv_itolS;
	  double cv_reltolS;        /* relative tolerance for sensitivities         */
	  double cv_SabstolS[];      /* scalar absolute tolerances for sensi.        */
	  double cv_VabstolS[];      /* vector absolute tolerances for sensi.        */

	  /*-----------------------------------
	    Quadrature Sensitivity Related Data 
	    -----------------------------------*/

	  boolean cv_quadr_sensi; /* SUNTRUE if computing sensitivties of quadrs. */

	  //CVQuadSensRhsFn cv_fQS;     /* fQS = (dfQ/dy)*yS + (dfQ/dp)                 */
	  //void *cv_fQS_data;          /* data pointer passed to fQS                   */
	  boolean cv_fQSDQ;       /* SUNTRUE if using internal DQ functions       */

	  boolean cv_errconQS;    /* SUNTRUE if yQS are considered in err. con.   */

	  int cv_itolQS;
	  double cv_reltolQS;       /* relative tolerance for yQS                   */
	  double cv_SabstolQS[];     /* scalar absolute tolerances for yQS           */
	  double cv_VabstolQS[];     /* vector absolute tolerances for yQS           */

	  /*-----------------------
	    Nordsieck History Array 
	    -----------------------*/

	  double cv_zn[] = new double[L_MAX];      /* Nordsieck array, of size N x (q+1).
	                                 zn[j] is a vector of length N (j=0,...,q)
	                                 zn[j] = [1/factorial(j)] * h^j * 
	                                 (jth derivative of the interpolating poly.)  */

	  /*-------------------
	    Vectors of length N 
	    -------------------*/

	  double cv_ewt[];            /* error weight vector                          */
	  double cv_y[];              /* y is used as temporary storage by the solver.
	                                 The memory is provided by the user to CVode 
	                                 where the vector is named yout.              */
	  double cv_acor[];           /* In the context of the solution of the
	                                 nonlinear equation, acor = y_n(m) - y_n(0).
	                                 On return, this vector is scaled to give
	                                 the estimated local error in y.              */
	  double cv_tempv[];          /* temporary storage vector                     */
	  double cv_ftemp[];          /* temporary storage vector                     */
	  
	  /*--------------------------
	    Quadrature Related Vectors 
	    --------------------------*/

	  double cv_znQ[] = new double[L_MAX];     /* Nordsieck arrays for quadratures             */
	  double cv_ewtQ[];           /* error weight vector for quadratures          */
	  double cv_yQ[];             /* Unlike y, yQ is not allocated by the user    */
	  double cv_acorQ[];          /* acorQ = yQ_n(m) - yQ_n(0)                    */
	  double cv_tempvQ[];         /* temporary storage vector (~ tempv)           */

	  /*---------------------------
	    Sensitivity Related Vectors 
	    ---------------------------*/

	  double cv_znS[][] = new double[L_MAX][];    /* Nordsieck arrays for sensitivities           */
	  double cv_ewtS[][];          /* error weight vectors for sensitivities       */
	  double cv_yS[][];            /* yS=yS0 (allocated by the user)               */
	  double cv_acorS[][];         /* acorS = yS_n(m) - yS_n(0)                    */
	  double cv_tempvS[][];        /* temporary storage vector (~ tempv)           */
	  double cv_ftempS[][];        /* temporary storage vector (~ ftemp)           */

	  boolean cv_stgr1alloc;  /* Did we allocate ncfS1, ncfnS1, and nniS1?    */

	  /*--------------------------------------
	    Quadrature Sensitivity Related Vectors 
	    --------------------------------------*/

	  double cv_znQS[][] = new double[L_MAX][];   /* Nordsieck arrays for quadr. sensitivities    */
	  double cv_ewtQS[][];         /* error weight vectors for sensitivities       */
	  double cv_yQS[][];           /* Unlike yS, yQS is not allocated by the user  */
	  double cv_acorQS[][];        /* acorQS = yQS_n(m) - yQS_n(0)                 */
	  double cv_tempvQS[][];       /* temporary storage vector (~ tempv)           */
	  double cv_ftempQ[];         /* temporary storage vector (~ ftemp)           */
	  
	  /*-----------------
	    Tstop information
	    -----------------*/

	  boolean cv_tstopset;
	  double cv_tstop;

	  /*---------
	    Step Data 
	    ---------*/

	  int cv_q;                    /* current order                               */
	  int cv_qprime;               /* order to be used on the next step
	                                * qprime = q-1, q, or q+1                     */
	  int cv_next_q;               /* order to be used on the next step           */
	  int cv_qwait;                /* number of internal steps to wait before
	                                * considering a change in q                   */
	  int cv_L;                    /* L = q + 1                                   */

	  double cv_hin;
	  double cv_h;               /* current step size                           */
	  double cv_hprime;          /* step size to be used on the next step       */ 
	  double cv_next_h;          /* step size to be used on the next step       */ 
	  double cv_eta;             /* eta = hprime / h                            */
	  double cv_hscale;          /* value of h used in zn                       */
	  double cv_tn;              /* current internal value of t                 */
	  double cv_tretlast;        /* last value of t returned                    */

	  double cv_tau[] = new double[L_MAX+1];    /* array of previous q+1 successful step
	                                * sizes indexed from 1 to q+1                 */
	  double cv_tq[] =new double[NUM_TESTS+1]; /* array of test quantities indexed from
	                                * 1 to NUM_TESTS(=5)                          */
	  double cv_l[] = new double[L_MAX];        /* coefficients of l(x) (degree q poly)        */

	  double cv_rl1;             /* the scalar 1/l[1]                           */
	  double cv_gamma;           /* gamma = h * rl1                             */
	  double cv_gammap;          /* gamma at the last setup call                */
	  double cv_gamrat;          /* gamma / gammap                              */

	  double cv_crate;           /* est. corrector conv. rate in Nls            */
	  double cv_crateS;          /* est. corrector conv. rate in NlsStgr        */
	  double cv_acnrm;           /* | acor |                                    */
	  double cv_acnrmQ;          /* | acorQ |                                   */
	  double cv_acnrmS;          /* | acorS |                                   */
	  double cv_acnrmQS;         /* | acorQS |                                  */
	  double cv_nlscoef;         /* coeficient in nonlinear convergence test    */
	  int  cv_mnewt;               /* Newton iteration counter                    */
	  int  cv_ncfS1[];              /* Array of Ns local counters for conv.  
	                                * failures (used in CVStep for STAGGERED1)    */

	  /*------
	    Limits 
	    ------*/

	  int cv_qmax;             /* q <= qmax                                       */
	  long cv_mxstep;      /* maximum number of internal steps for one 
				      user call                                       */
	  int cv_maxcor;           /* maximum number of corrector iterations for 
				      the solution of the nonlinear equation          */
	  int cv_maxcorS;
	  int cv_mxhnil;           /* max. number of warning messages issued to the
				      user that t + h == t for the next internal step */
	  int cv_maxnef;           /* maximum number of error test failures           */
	  int cv_maxncf;           /* maximum number of nonlinear conv. failures      */
	  
	  double cv_hmin;        /* |h| >= hmin                                     */
	  double cv_hmax_inv;    /* |h| <= 1/hmax_inv                               */
	  double cv_etamax;      /* eta <= etamax                                   */

	  /*----------
	    Counters 
	    ----------*/

	  long cv_nst;         /* number of internal steps taken                  */

	  long cv_nfe;         /* number of f calls                               */
	  long cv_nfQe;        /* number of fQ calls                              */
	  long cv_nfSe;        /* number of fS calls                              */
	  long cv_nfeS;        /* number of f calls from sensi DQ                 */
	  long cv_nfQSe;       /* number of fQS calls                             */
	  long cv_nfQeS;       /* number of fQ calls from sensi DQ                */


	  long cv_ncfn;        /* number of corrector convergence failures        */
	  long cv_ncfnS;       /* number of total sensi. corr. conv. failures     */
	  long cv_ncfnS1[];     /* number of sensi. corrector conv. failures       */

	  long cv_nni;         /* number of nonlinear iterations performed        */
	  long cv_nniS;        /* number of total sensi. nonlinear iterations     */
	  long cv_nniS1[];      /* number of sensi. nonlinear iterations           */

	  long cv_netf;        /* number of error test failures                   */
	  long cv_netfQ;       /* number of quadr. error test failures            */
	  long cv_netfS;       /* number of sensi. error test failures            */
	  long cv_netfQS;      /* number of quadr. sensi. error test failures     */

	  long cv_nsetups;     /* number of setup calls                           */
	  long cv_nsetupsS;    /* number of setup calls due to sensitivities      */

	  int cv_nhnil;            /* number of messages issued to the user that
				      t + h == t for the next iternal step            */

	  /*-----------------------------
	    Space requirements for CVODES 
	    -----------------------------*/

	  long cv_lrw1;        /* no. of realtype words in 1 N_Vector y           */ 
	  long cv_liw1;        /* no. of integer words in 1 N_Vector y            */ 
	  long cv_lrw1Q;       /* no. of realtype words in 1 N_Vector yQ          */ 
	  long cv_liw1Q;       /* no. of integer words in 1 N_Vector yQ           */ 
	  long cv_lrw;             /* no. of realtype words in CVODES work vectors    */
	  long cv_liw;             /* no. of integer words in CVODES work vectors     */

	  /*----------------
	    Step size ratios
	    ----------------*/

	  double cv_etaqm1;      /* ratio of new to old h for order q-1             */
	  double cv_etaq;        /* ratio of new to old h for order q               */
	  double cv_etaqp1;      /* ratio of new to old h for order q+1             */

	  /*------------------
	    Linear Solver Data 
	    ------------------*/

	  /* Linear Solver functions to be called */

	  //int (*cv_linit)(struct CVodeMemRec *cv_mem);

	  //int (*cv_lsetup)(struct CVodeMemRec *cv_mem, int convfail, 
			   //N_Vector ypred, N_Vector fpred, booleantype *jcurPtr, 
			   //N_Vector vtemp1, N_Vector vtemp2, N_Vector vtemp3); 

	  //int (*cv_lsolve)(struct CVodeMemRec *cv_mem, N_Vector b, N_Vector weight,
			   //N_Vector ycur, N_Vector fcur);

	  //int (*cv_lfree)(struct CVodeMemRec *cv_mem);

	  /* Linear Solver specific memory */

	  //void *cv_lmem;           

	  /* Flag to request a call to the setup routine */

	  boolean cv_forceSetup;

	  /*------------
	    Saved Values
	    ------------*/

	  int cv_qu;                   /* last successful q value used                */
	  long cv_nstlp;           /* step number of last setup call              */
	  double cv_h0u;             /* actual initial stepsize                     */
	  double cv_hu;              /* last successful h value used                */
	  double cv_saved_tq5;       /* saved value of tq[5]                        */
	  boolean cv_jcur;         /* is Jacobian info for linear solver current? */
	  int cv_convfail;             /* flag storing previous solver failure mode   */
	  double cv_tolsf;           /* tolerance scale factor                      */
	  int cv_qmax_alloc;           /* qmax used when allocating mem               */
	  int cv_qmax_allocQ;          /* qmax used when allocating quad. mem         */
	  int cv_qmax_allocS;          /* qmax used when allocating sensi. mem        */
	  int cv_qmax_allocQS;         /* qmax used when allocating quad. sensi. mem  */
	  int cv_indx_acor;            /* index of zn vector in which acor is saved   */

	  /*--------------------------------------------------------------------
	    Flags turned ON by CVodeInit, CVodeSensMalloc, and CVodeQuadMalloc 
	    and read by CVodeReInit, CVodeSensReInit, and CVodeQuadReInit
	    --------------------------------------------------------------------*/

	  boolean cv_VabstolMallocDone;
	  boolean cv_MallocDone;

	  boolean cv_VabstolQMallocDone;
	  boolean cv_QuadMallocDone;

	  boolean cv_VabstolSMallocDone;
	  boolean cv_SabstolSMallocDone;
	  boolean cv_SensMallocDone;

	  boolean cv_VabstolQSMallocDone;
	  boolean cv_SabstolQSMallocDone;
	  boolean cv_QuadSensMallocDone;

	  /*-------------------------------------------
	    Error handler function and error ouput file 
	    -------------------------------------------*/

	  //CVErrHandlerFn cv_ehfun;    /* Error messages are handled by ehfun          */
	  CVodeMemRec cv_eh_data;           /* dats pointer passed to ehfun                 */
	  RandomAccessFile cv_errfp;             /* CVODES error messages are sent to errfp      */    

	  /*-------------------------
	    Stability Limit Detection
	    -------------------------*/

	  boolean cv_sldeton;     /* Is Stability Limit Detection on?             */
	  double cv_ssdat[][] = new double[6][4];    /* scaled data array for STALD                  */
	  int cv_nscon;               /* counter for STALD method                     */
	  long cv_nor;            /* counter for number of order reductions       */

	  /*----------------
	    Rootfinding Data
	    ----------------*/

	  //CVRootFn cv_gfun;        /* Function g for roots sought                     */
	  int cv_nrtfn;            /* number of components of g                       */
	  int cv_iroots[];          /* array for root information                      */
	  int cv_rootdir[];         /* array specifying direction of zero-crossing     */
	  double cv_tlo;         /* nearest endpoint of interval in root search     */
	  double cv_thi;         /* farthest endpoint of interval in root search    */
	  double cv_trout;       /* t value returned by rootfinding routine         */
	  double cv_glo[];        /* saved array of g values at t = tlo              */
	  double cv_ghi[];        /* saved array of g values at t = thi              */
	  double cv_grout[];      /* array of g values at t = trout                  */
	  double cv_toutc;       /* copy of tout (if NORMAL mode)                   */
	  double cv_ttol;        /* tolerance on root location trout                */
	  int cv_taskc;            /* copy of parameter itask                         */
	  int cv_irfnd;            /* flag showing whether last step had a root       */
	  long cv_nge;         /* counter for g evaluations                       */
	  boolean cv_gactive[]; /* array with active/inactive event functions      */
	  int cv_mxgnull;          /* number of warning messages about possible g==0  */

	  /*------------------------
	    Adjoint sensitivity data
	    ------------------------*/

	  boolean cv_adj;             /* SUNTRUE if performing ASA                */

	  CVodeMemRec cv_adj_mem; /* Pointer to adjoint memory structure      */

	  boolean cv_adjMallocDone;

	} ;


	
	// Call CVodeCreate to create the solver memory and specify the
	// Backward Differentiation Formula and the use of a Newton 
	// iteration
	
	 // CVodeCreate creates an internal memory block for a problem to
	 // be solved by CVODES.
	 //
	 // lmm  is the type of linear multistep method to be used.
	 //      The legal values are CV_ADAMS and CV_BDF (see previous
	 //      description).
	 //
	 // iter  is the type of iteration used to solve the nonlinear
	 //       system that arises during each internal time step.
	 //       The legal values are CV_FUNCTIONAL and CV_NEWTON.
	 //
	 //If successful, CVodeCreate returns a pointer to initialized
	 // problem memory. This pointer should be passed to CVodeInit.
	 // If an initialization error occurs, CVodeCreate prints an error
	 // message to standard err and returns NULL.

     private CVodeMemRec CVodeCreate(int lmm, int iter) {
    	 int maxord;
         CVodeMemRec cv_mem;

    	  /* Test inputs */

    	  if ((lmm != CV_ADAMS) && (lmm != CV_BDF)) {
    	    cvProcessError(null, 0, "CVODES", "CVodeCreate", MSGCV_BAD_LMM);
    	    return null;
    	  }
    	  
    	  if ((iter != CV_FUNCTIONAL) && (iter != CV_NEWTON)) {
    	    cvProcessError(null, 0, "CVODES", "CVodeCreate", MSGCV_BAD_ITER);
    	    return null;
    	  }

    	  cv_mem = null;
    	  cv_mem = new CVodeMemRec();
    	  if (cv_mem == null) {
    	    cvProcessError(null, 0, "CVODES", "CVodeCreate", MSGCV_CVMEM_FAIL);
    	    return null;
    	  }

    	  /* Zero out cv_mem */
    	  //memset(cv_mem, 0, sizeof(struct CVodeMemRec));

    	  maxord = (lmm == CV_ADAMS) ? ADAMS_Q_MAX : BDF_Q_MAX;

    	  /* copy input parameters into cv_mem */

    	  cv_mem.cv_lmm  = lmm;
    	  cv_mem.cv_iter = iter;

    	  /* Set uround */

    	  cv_mem.cv_uround = UNIT_ROUNDOFF;

    	  /* Set default values for integrator optional inputs */

    	  //cv_mem.cv_f          = null;
    	  //cv_mem.cv_user_data  = null;
    	  cv_mem.cv_itol       = CV_NN;
    	  cv_mem.cv_user_efun  = false;
    	  //cv_mem.cv_efun       = null;
    	  //cv_mem.cv_e_data     = null;
    	  //cv_mem.cv_ehfun      = cvErrHandler;
    	  cv_mem.cv_eh_data    = cv_mem;
    	  //cv_mem.cv_errfp      = stderr;
    	  cv_mem.cv_qmax       = maxord;
    	  cv_mem.cv_mxstep     = MXSTEP_DEFAULT;
    	  cv_mem.cv_mxhnil     = MXHNIL_DEFAULT;
    	  cv_mem.cv_sldeton    = false;
    	  cv_mem.cv_hin        = ZERO;
    	  cv_mem.cv_hmin       = HMIN_DEFAULT;
    	  cv_mem.cv_hmax_inv   = HMAX_INV_DEFAULT;
    	  cv_mem.cv_tstopset   = false;
    	  cv_mem.cv_maxcor     = NLS_MAXCOR;
    	  cv_mem.cv_maxnef     = MXNEF;
    	  cv_mem.cv_maxncf     = MXNCF;
    	  cv_mem.cv_nlscoef    = CORTES;

    	  /* Initialize root finding variables */

    	  cv_mem.cv_glo        = null;
    	  cv_mem.cv_ghi        = null;
    	  cv_mem.cv_grout      = null;
    	  cv_mem.cv_iroots     = null;
    	  cv_mem.cv_rootdir    = null;
    	  //cv_mem.cv_gfun       = null;
    	  cv_mem.cv_nrtfn      = 0;  
    	  cv_mem.cv_gactive    = null;
    	  cv_mem.cv_mxgnull    = 1;

    	  /* Set default values for quad. optional inputs */

    	  cv_mem.cv_quadr      = false;
    	  //cv_mem.cv_fQ         = null;
    	  cv_mem.cv_errconQ    = false;
    	  cv_mem.cv_itolQ      = CV_NN;

    	  /* Set default values for sensi. optional inputs */

    	  cv_mem.cv_sensi      = false;
    	  //cv_mem.cv_fS_data    = mull;
    	  //cv_mem.cv_fS         = cvSensRhsInternalDQ;
    	  //cv_mem.cv_fS1        = cvSensRhs1InternalDQ;
    	  cv_mem.cv_fSDQ       = true;
    	  cv_mem.cv_ifS        = CV_ONESENS;
    	  cv_mem.cv_DQtype     = CV_CENTERED;
    	  cv_mem.cv_DQrhomax   = ZERO;
    	  cv_mem.cv_p          = null;
    	  cv_mem.cv_pbar       = null;
    	  cv_mem.cv_plist      = null;
    	  cv_mem.cv_errconS    = false;
    	  cv_mem.cv_maxcorS    = NLS_MAXCOR;
    	  cv_mem.cv_ncfS1      = null;
    	  cv_mem.cv_ncfnS1     = null;
    	  cv_mem.cv_nniS1      = null;
    	  cv_mem.cv_itolS      = CV_NN;

    	  /* Set default values for quad. sensi. optional inputs */

    	  cv_mem.cv_quadr_sensi = false;
    	  //cv_mem.cv_fQS         = null;
    	  //cv_mem.cv_fQS_data    = null;
    	  cv_mem.cv_fQSDQ       = true;
    	  cv_mem.cv_errconQS    = false;
    	  cv_mem.cv_itolQS      = CV_NN;

    	  /* Set default for ASA */

    	  cv_mem.cv_adj         = false;
    	  cv_mem.cv_adj_mem     = null;

    	  /* Set the saved values for qmax_alloc */

    	  cv_mem.cv_qmax_alloc  = maxord;
    	  cv_mem.cv_qmax_allocQ = maxord;
    	  cv_mem.cv_qmax_allocS = maxord;

    	  /* Initialize lrw and liw */

    	  cv_mem.cv_lrw = 65 + 2*L_MAX + NUM_TESTS;
    	  cv_mem.cv_liw = 52;

    	  /* No mallocs have been done yet */

    	  cv_mem.cv_VabstolMallocDone   = false;
    	  cv_mem.cv_MallocDone          = false;

    	  cv_mem.cv_VabstolQMallocDone  = false;
    	  cv_mem.cv_QuadMallocDone      = false;

    	  cv_mem.cv_VabstolSMallocDone  = false;
    	  cv_mem.cv_SabstolSMallocDone  = false;
    	  cv_mem.cv_SensMallocDone      = false;

    	  cv_mem.cv_VabstolQSMallocDone = false;
    	  cv_mem.cv_SabstolQSMallocDone = false;
    	  cv_mem.cv_QuadSensMallocDone  = false;

    	  cv_mem.cv_adjMallocDone       = false;

    	  /* Return pointer to CVODES memory block */

    	  return cv_mem;

     }
     
     /*
      * cvProcessError is a high level error handling function.
      * - If cv_mem==NULL it prints the error message to stderr.
      * - Otherwise, it sets up and calls the error handling function 
      *   pointed to by cv_ehfun.
      */

     void cvProcessError(CVodeMemRec cv_mem, 
                         int error_code,  String module, String fname, 
                         String msgfmt)
     {
    	 MipavUtil.displayError("In module " + module + " in function " + fname + " msgfmt");
       //va_list ap;
       //char msg[256];

       /* Initialize the argument pointer variable 
          (msgfmt is the last required argument to cvProcessError) */

       //va_start(ap, msgfmt);

       /* Compose the message */

       //vsprintf(msg, msgfmt, ap);

       //if (cv_mem == NULL) {    /* We write to stderr */
     //#ifndef NO_FPRINTF_OUTPUT
         //fprintf(stderr, "\n[%s ERROR]  %s\n  ", module, fname);
         //fprintf(stderr, "%s\n\n", msg);
     //#endif

       //} else {                 /* We can call ehfun */
         //cv_mem->cv_ehfun(error_code, module, fname, msg, cv_mem->cv_eh_data);
       //}

       /* Finalize argument processing */
       //va_end(ap);

       return;
     }
     
     /*-----------------------------------------------------------------*/

     /*
      * CVodeInit
      * 
      * CVodeInit allocates and initializes memory for a problem. All 
      * problem inputs are checked for errors. If any error occurs during 
      * initialization, it is reported to the file whose file pointer is 
      * errfp and an error flag is returned. Otherwise, it returns CV_SUCCESS
      */

     int CVodeInit(CVodeMemRec cv_mem, double t0, double y0[])
     {
       boolean nvectorOK, allocOK;
       long lrw1, liw1;
       int i,k;

       /* Check cvode_mem */

       if (cv_mem==null) {
         cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeInit",
                        MSGCV_NO_MEM);
         return(CV_MEM_NULL);
       }

       /* Check for legal input parameters */

       if (y0==null) {
         cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVodeInit",
                        MSGCV_NULL_Y0);
         return(CV_ILL_INPUT);
       }

       //if (f == NULL) {
         //cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVodeInit",
                        //MSGCV_NULL_F);
         //return(CV_ILL_INPUT);
       //}

       /* Test if all required vector operations are implemented */

       /*nvectorOK = cvCheckNvector(y0);
       if(!nvectorOK) {
         cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVodeInit",
                        MSGCV_BAD_NVECTOR);
         return(CV_ILL_INPUT);
       }*/

       /* Set space requirements for one N_Vector */

       /*if (y0->ops->nvspace != NULL) {
         N_VSpace(y0, &lrw1, &liw1);
       } else {
         lrw1 = 0;
         liw1 = 0;
       }
       cv_mem->cv_lrw1 = lrw1;
       cv_mem->cv_liw1 = liw1;*/

       /* Allocate the vectors (using y0 as a template) */

      /* allocOK = cvAllocVectors(cv_mem, y0);
       if (!allocOK) {
         cvProcessError(cv_mem, CV_MEM_FAIL, "CVODES", "CVodeInit",
                        MSGCV_MEM_FAIL);
         return(CV_MEM_FAIL);
       }*/

       /* All error checking is complete at this point */

       /* Copy the input parameters into CVODES state */

       /*cv_mem->cv_f  = f;
       cv_mem->cv_tn = t0;*/

       /* Set step parameters */

       /*cv_mem->cv_q      = 1;
       cv_mem->cv_L      = 2;
       cv_mem->cv_qwait  = cv_mem->cv_L;
       cv_mem->cv_etamax = ETAMX1;

       cv_mem->cv_qu     = 0;
       cv_mem->cv_hu     = ZERO;
       cv_mem->cv_tolsf  = ONE;*/

       /* Set the linear solver addresses to NULL.
          (We check != NULL later, in CVode, if using CV_NEWTON.) */

       /*cv_mem->cv_linit  = NULL;
       cv_mem->cv_lsetup = NULL;
       cv_mem->cv_lsolve = NULL;
       cv_mem->cv_lfree  = NULL;
       cv_mem->cv_lmem   = NULL;*/

       /* Set forceSetup to SUNFALSE */

       //cv_mem->cv_forceSetup = SUNFALSE;

       /* Initialize zn[0] in the history array */

       //N_VScale(ONE, y0, cv_mem->cv_zn[0]);

       /* Initialize all the counters */

       /*cv_mem->cv_nst     = 0;
       cv_mem->cv_nfe     = 0;
       cv_mem->cv_ncfn    = 0;
       cv_mem->cv_netf    = 0;
       cv_mem->cv_nni     = 0;
       cv_mem->cv_nsetups = 0;
       cv_mem->cv_nhnil   = 0;
       cv_mem->cv_nstlp   = 0;
       cv_mem->cv_nscon   = 0;
       cv_mem->cv_nge     = 0;

       cv_mem->cv_irfnd   = 0;*/

       /* Initialize other integrator optional outputs */

       /*cv_mem->cv_h0u      = ZERO;
       cv_mem->cv_next_h   = ZERO;
       cv_mem->cv_next_q   = 0;*/

       /* Initialize Stablilty Limit Detection data */
       /* NOTE: We do this even if stab lim det was not
          turned on yet. This way, the user can turn it
          on at any time */

       /*cv_mem->cv_nor = 0;
       for (i = 1; i <= 5; i++)
         for (k = 1; k <= 3; k++) 
           cv_mem->cv_ssdat[i-1][k-1] = ZERO;*/

       /* Problem has been successfully initialized */

       /*cv_mem->cv_MallocDone = SUNTRUE;*/

       return(CV_SUCCESS);
     }
     
     /*
      * cvCheckNvector
      * This routine checks if all required vector operations are present.
      * If any of them is missing it returns SUNFALSE.
      */

     /*static booleantype cvCheckNvector(N_Vector tmpl)
     {
       if((tmpl->ops->nvclone     == NULL) || // clone
          (tmpl->ops->nvdestroy   == NULL) || // set null
          (tmpl->ops->nvlinearsum == NULL) || // (double a, double x[], double b, double y[], double z[])
                                              // z[i] = a*x[i] + b*y[i];
          (tmpl->ops->nvconst     == NULL) ||
          (tmpl->ops->nvprod      == NULL) ||
          (tmpl->ops->nvdiv       == NULL) ||
          (tmpl->ops->nvscale     == NULL) ||
          (tmpl->ops->nvabs       == NULL) ||
          (tmpl->ops->nvinv       == NULL) ||
          (tmpl->ops->nvaddconst  == NULL) ||
          (tmpl->ops->nvmaxnorm   == NULL) ||
          (tmpl->ops->nvwrmsnorm  == NULL) ||
          (tmpl->ops->nvmin       == NULL))
         return(SUNFALSE);
       else
         return(SUNTRUE);
     }*/



}