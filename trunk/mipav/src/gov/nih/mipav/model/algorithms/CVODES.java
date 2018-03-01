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
	
	//itask: The itask input parameter to CVode indicates the job
	//        of the solver for the next user step. The CV_NORMAL
	//        itask is to have the solver take internal steps until
	//        it has reached or just passed the user specified tout
	//        parameter. The solver then interpolates in order to
	//        return an approximate value of y(tout). The CV_ONE_STEP
	//        option tells the solver to just take one internal step
	//        and return the solution at the point reached by that step.
	final int CV_NORMAL = 1;
	final int CV_ONE_STEP = 2;
	
	//ism:   This parameter specifies the sensitivity corrector type
	//        to be used. In the CV_SIMULTANEOUS case, the nonlinear
	//        systems for states and all sensitivities are solved
	//        simultaneously. In the CV_STAGGERED case, the nonlinear
	//        system for states is solved first and then, the
	//        nonlinear systems for all sensitivities are solved
	//        at the same time. Finally, in the CV_STAGGERED1 approach
	//        all nonlinear systems are solved in a sequence.
	final int CV_SIMULTANEOUS = 1;
	final int CV_STAGGERED = 2;
	final int CV_STAGGERED1 = 3;


	
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
	final int CV_TSTOP_RETURN = 1;
	final int CV_ROOT_RETURN = 2;

	final int CV_WARNING = 99;

	final int CV_TOO_MUCH_WORK = -1;
	final int CV_TOO_MUCH_ACC = -2;
	final int CV_ERR_FAILURE = -3;
	final int CV_CONV_FAILURE = -4;

	final int CV_LINIT_FAIL = -5;
	final int CV_LSETUP_FAIL = -6;
	final int CV_LSOLVE_FAIL = -7;
	final int CV_RHSFUNC_FAIL = -8;
	final int CV_FIRST_RHSFUNC_ERR = -9;
	final int CV_REPTD_RHSFUNC_ERR = -10;
	final int CV_UNREC_RHSFUNC_ERR = -11;
	final int  CV_RTFUNC_FAIL = -12;

	final int CV_MEM_FAIL = -20;
	final int CV_MEM_NULL = -21;
	final int CV_ILL_INPUT = -22;
	final int CV_NO_MALLOC = -23;
	final int CV_BAD_K = -24;
	final int CV_BAD_T = -25;
	final int CV_BAD_DKY = -26;
	final int CV_TOO_CLOSE = -27;

	final int CV_NO_QUAD = -30;
	final int CV_QRHSFUNC_FAIL = -31;
	final int CV_FIRST_QRHSFUNC_ERR = -32;
	final int CV_REPTD_QRHSFUNC_ERR = -33;
	final int CV_UNREC_QRHSFUNC_ERR = -34;

	final int CV_NO_SENS = -40;
	final int CV_SRHSFUNC_FAIL = -41;
	final int CV_FIRST_SRHSFUNC_ERR = -42;
	final int CV_REPTD_SRHSFUNC_ERR = -43;
	final int CV_UNREC_SRHSFUNC_ERR = -44;

	final int CV_BAD_IS = -45;

	final int CV_NO_QUADSENS = -50;
	final int CV_QSRHSFUNC_FAIL = -51;
	final int CV_FIRST_QSRHSFUNC_ERR = -52;
	final int CV_REPTD_QSRHSFUNC_ERR = -53;
	final int CV_UNREC_QSRHSFUNC_ERR = -54;

	final double HMIN_DEFAULT = 0.0;    /* hmin default value     */
	final double HMAX_INV_DEFAULT = 0.0;    /* hmax_inv default value */
	final int MXHNIL_DEFAULT =  10;             /* mxhnil default value   */
	final long MXSTEP_DEFAULT = 500L;            /* mxstep default value   */
	final int NLS_MAXCOR = 3;
	final int MXNCF = 10;
	final int MXNEF = 7;
	final double CORTES = 0.1;
	final double ZERO = 0.0;
	final double PT1 = 0.1;
	final double POINT2 = 0.2;
	final double HALF = 0.5;
	final double H_BIAS = HALF;
	final double ONE = 1.0;
	final double TWO = 2.0;
	final double FOUR = 4.0;
	final double FIVE = 5.0;
	final double TWELVE = 12.0;
	final double HUNDRED = 100.0;
	final double ETAMX1 = 10000.0; 
	final double HUB_FACTOR = 0.1;
	final double HLB_FACTOR = 100.0;
	final double FUZZ_FACTOR = 100.0;
	final int MAX_ITERS = 4;
	// CRDOWN constant used in the estimation of the convergence rate (crate)
	//        of the iterates for the nonlinear equation
	final double CRDOWN = 0.3;
	// DGMAX  iter == CV_NEWTON, |gamma/gammap-1| > DGMAX => call lsetup
    final double DGMAX = 0.3;
    // RDIV declare divergence if ratio del/delp > RDIV
    final double RDIV = TWO;
    // MSBP  max no. of steps between lsetup calls
    final int MSBP = 20;

	
	final int RTFOUND = +1;
	final int CLOSERT = +3;


	
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
	
	final int CVDLS_SUCCESS = 0;
	final int CVDLS_MEM_NULL = -1;
	final int CVDLS_LMEM_NULL = -2;
	final int CVDLS_ILL_INPUT = -3;
	final int  CVDLS_MEM_FAIL = -4;

	/* Additional last_flag values */

	final int CVDLS_JACFUNC_UNRECVR = -5;
	final int CVDLS_JACFUNC_RECVR = -6;
	final int CVDLS_SUNMAT_FAIL = -7;

	/* Return values for the adjoint module */

	final int CVDLS_NO_ADJ = -101;
	final int CVDLS_LMEMB_NULL = -102;

	final String MSGD_CVMEM_NULL = "Integrator memory is NULL.";
	final String MSGD_BAD_NVECTOR = "A required vector operation is not implemented.";
	final String MSGD_BAD_SIZES = "Illegal bandwidth parameter(s). Must have 0 <=  ml, mu <= N-1.";
	final String MSGD_MEM_FAIL = "A memory request failed.";
	final String MSGD_LMEM_NULL ="Linear solver memory is NULL.";
	final String MSGD_JACFUNC_FAILED = "The Jacobian routine failed in an unrecoverable manner.";
	final String MSGD_MATCOPY_FAILED = "The SUNMatCopy routine failed in an unrecoverable manner.";
	final String MSGD_MATZERO_FAILED = "The SUNMatZero routine failed in an unrecoverable manner.";
	final String MSGD_MATSCALEADDI_FAILED = "The SUNMatScaleAddI routine failed in an unrecoverable manner.";

	final String MSGD_NO_ADJ = "Illegal attempt to call before calling CVodeAdjMalloc.";
	final String MSGD_BAD_WHICH = "Illegal value for which.";
	final String MSGD_LMEMB_NULL = "Linear solver memory is NULL for the backward integration.";
	final String MSGD_BAD_TINTERP = "Bad t for interpolation.";
	
	final int DO_ERROR_TEST = +2;
	final int PREDICT_AGAIN = +3;

	final int CONV_FAIL = +4; 
	final int TRY_AGAIN = +5;

	final int FIRST_CALL = +6;
	final int PREV_CONV_FAIL = +7;
	final int PREV_ERR_FAIL = +8;

	final int RHSFUNC_RECVR = +9;

	final int QRHSFUNC_RECVR = +11;
	final int SRHSFUNC_RECVR = +12;
	final int QSRHSFUNC_RECVR = +13;

	/*
	 * -----------------------------------------------------------------
	 * Communication between CVODE and a CVODE Linear Solver
	 * -----------------------------------------------------------------
	 * convfail (input to cv_lsetup)
	 *
	 * CV_NO_FAILURES : Either this is the first cv_setup call for this
	 *                  step, or the local error test failed on the
	 *                  previous attempt at this step (but the Newton
	 *                  iteration converged).
	 *
	 * CV_FAIL_BAD_J  : This value is passed to cv_lsetup if
	 *
	 *                  (a) The previous Newton corrector iteration
	 *                      did not converge and the linear solver's
	 *                      setup routine indicated that its Jacobian-
	 *                      related data is not current
	 *                                   or
	 *                  (b) During the previous Newton corrector
	 *                      iteration, the linear solver's solve routine
	 *                      failed in a recoverable manner and the
	 *                      linear solver's setup routine indicated that
	 *                      its Jacobian-related data is not current.
	 *
	 * CV_FAIL_OTHER  : During the current internal step try, the
	 *                  previous Newton iteration failed to converge
	 *                  even though the linear solver was using current
	 *                  Jacobian-related data.
	 * -----------------------------------------------------------------
	 */
	/* Constants for convfail (input to cv_lsetup) */
	final int CV_NO_FAILURES = 0;
    final int CV_FAIL_BAD_J = 1;
	final int CV_FAIL_OTHER = 2;
	
	/*-----------------------------------------------------------------
	  CVDLS solver constants
	  -----------------------------------------------------------------
	  CVD_MSBJ   maximum number of steps between Jacobian evaluations
	  CVD_DGMAX  maximum change in gamma between Jacobian evaluations
	  -----------------------------------------------------------------*/

	final int CVD_MSBJ = 50;
	final double CVD_DGMAX = 0.2;


	/*-----------------------------------------------------------------
	 * IV. SUNLinearSolver error codes
	 * ---------------------------------------------------------------
	 */

	final int SUNLS_SUCCESS = 0;  /* successful/converged          */

	final int SUNLS_MEM_NULL = -1;  /* mem argument is NULL          */
	final int SUNLS_ILL_INPUT = -2;  /* illegal function input        */
	final int SUNLS_MEM_FAIL = -3;  /* failed memory access          */
	final int SUNLS_ATIMES_FAIL_UNREC = -4;  /* atimes unrecoverable failure  */
	final int SUNLS_PSET_FAIL_UNREC = -5;  /* pset unrecoverable failure    */
	final int SUNLS_PSOLVE_FAIL_UNREC = -6;  /* psolve unrecoverable failure  */
	final int SUNLS_PACKAGE_FAIL_UNREC = -7;  /* external package unrec. fail  */
	final int SUNLS_GS_FAIL = -8;  /* Gram-Schmidt failure          */        
	final int SUNLS_QRSOL_FAIL = -9;  /* QRsol found singular R        */

	final int SUNLS_RES_REDUCED = 1;  /* nonconv. solve, resid reduced */
	final int SUNLS_CONV_FAIL = 2;  /* nonconvergent solve           */
	final int SUNLS_ATIMES_FAIL_REC = 3;  /* atimes failed recoverably     */
	final int SUNLS_PSET_FAIL_REC = 4;  /* pset failed recoverably       */
	final int SUNLS_PSOLVE_FAIL_REC = 5;  /* psolve failed recoverably     */
	final int SUNLS_PACKAGE_FAIL_REC = 6;  /* external package recov. fail  */
	final int SUNLS_QRFACT_FAIL = 7;  /* QRfact found singular matrix  */
	final int SUNLS_LUFACT_FAIL = 8;  /* LUfact found singular matrix  */

	
	public enum SUNLinearSolver_Type{
		  SUNLINEARSOLVER_DIRECT,
		  SUNLINEARSOLVER_ITERATIVE,
		  SUNLINEARSOLVER_CUSTOM
		};

    final int cvDlsDQJac = -1;
    
    // For cv_mem.cv_efun_select
    final int cvEwtSet_select = 1;
    // For cv_mem.cv_linit_select
    final int cvDlsInitialize_select = 1;
    // For cv_mem.cv_lsolve_select
    final int cvDlsSolve_select = 1;
    // For cv_mem.cv_lfree_select
    final int cvDlsFree_select = 1;
    // For cv_mem.cv_setup_select
    final int cvDlsSetup_select = 1;

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
		int f = cvsRoberts_dns;
		int g = cvsRoberts_dns;
		int Jac = cvsRoberts_dns;
		
		// Set initial conditions
		NVector y = new NVector();
		NVector abstol = new NVector();
		double yr[] = new double[]{Y1,Y2,Y3};
		N_VNew_Serial(y, NEQ);
		y.setData(yr);
		double reltol = RTOL; // Set the scalar relative tolerance
		// Set the vector absolute tolerance
		double abstolr[] = new double[]{ATOL1,ATOL2,ATOL3};
		N_VNew_Serial(abstol,NEQ);
		abstol.setData(abstolr);
		CVodeMemRec cvode_mem;
		int flag;
		double A[][];
		SUNLinearSolver LS;
		double tout;
		int iout;
		double t[] = new double[1];
		
		// Call CVodeCreate to create the solver memory and specify the
		// Backward Differentiation Formula and the use of a Newton
		// iteration
		cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
		if (cvode_mem == null) {
		    return;	
		}
		
		// Call CVodeInit to initialize the integrator memory and specify the
		// user's right hand side function in y' = f(t,y), the initial time T0, and
	    // the initial dependent variable vector y
		flag = CVodeInit(cvode_mem, f, T0, y);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Call CVodeSVtolerances to specify the scalar relative tolerance
		// and vector absolute tolerances
		flag = CVodeSVtolerances(cvode_mem, reltol, abstol);
		if (flag != CV_SUCCESS) {
			return;
		}
		
		// Call CVRootInit to specify the root function g with 2 components
		flag = CVodeRootInit(cvode_mem, 2, g);
		if (flag != CV_SUCCESS) {
			return;
		}
		
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
		
		// In loop, call CVode, print results, and test for error.
		// Break out of loop when NOUT preset output times have been reached.
		System.out.println(" \n3-species kinetics problem\n");
		
		iout = 0;
		tout = T1;
		
		while (true) {
			flag = CVode(cvode_mem, tout, y, t, CV_NORMAL);
		} // while (true)
	}
	
	private void N_VNew_Serial(NVector y, int length) {
	    if ((y != null) && (length > 0)) {
	    	y.data = new double[length];
	    	y.own_data = true;
	    }
	}
	
	/**
	 * f routine.  Compte function f(t,y).
	 * @param t
	 * @param yv
	 * @param ydotv
	 * @param user_data
	 * @return
	 */
	private int f(double t, NVector yv, NVector ydotv, CVodeMemRec user_data) {
		double y[] = yv.getData();
		double ydot[] = ydotv.getData();
		if (problem == 1) {
		    ydot[0] = -0.04*y[0] + 1.0E4*y[1]*y[2];
		    ydot[2] = 3.0E7*y[1]*y[1];
		    ydot[1] = -ydot[0] - ydot[2];
		}
		return 0;
	}
	
	private int fQ(double t, NVector x, NVector y, CVodeMemRec user_data) {
		return 0;
	}
	
	/**
	 * g routine.  Compute functions g_i(t,y) for i = 0,1.
	 * @param t
	 * @param yv
	 * @param gout
	 * @param user_data
	 * @return
	 */
	private int g(double t, NVector yv, double gout[], CVodeMemRec user_data) {
		double y[] = yv.getData();
		if (problem == 1) {
		    gout[0] = y[0] - 0.0001;
		    gout[1] = y[2] - 0.01;
		}
		return 0;
	}
	
	/**
	 * Jacobian routine.  Compute J(t,y) = df/dy.
	 * @param t
	 * @param y
	 * @param fy
	 * @param J
	 * @param tmp1
	 * @param tmp2
	 * @return
	 */
	private int Jac(double t, NVector yv, NVector fy, double J[][], CVodeMemRec data, NVector tmp1, NVector tmp2, NVector tmp3) {
		double y[] = yv.getData();
	    J[0][0] = -0.04;
	    J[0][1] = 1.0E4 * y[2];
	    J[0][2] = 1.0E4 * y[1];
	    
	    J[1][0] = 0.04;
	    J[1][1] = -1.0E4 * y[2] - 6.0E7*y[1];
	    J[1][2] = -1.0E4 * y[1];
	    
	    J[2][0] = ZERO;
	    J[2][1] = 6.0E7 * y[1];
	    J[2][2] = ZERO;
	    
	    return 0;
	}
	
	private int fS(int cv_Ns, double time, NVector ycur, NVector fcur, NVector yScur[], 
            NVector fScur[], CVodeMemRec cv_fS_data, NVector temp1, NVector temp2) {
		return 0;
	}
	
	private int fS1(int cv_Ns, double time, NVector ycur, NVector fcur, int is, NVector yScur, 
            NVector fScur, CVodeMemRec cv_fS_data, NVector temp1, NVector temp2) {
		return 0;
	}
	
	private int fQS(int cv_Ns, double cv_tn, NVector cv_zn,
                                NVector cv_znS[], NVector cv_znQ,
                                NVector cv_znQS[], CVodeMemRec cv_fQS_data,
                                NVector cv_tempv, NVector cv_tempvQ) {
		return 0;
	}
	
	// Types: struct CVodeMemRec, CVodeMem
	// -----------------------------------------------------------------
	// The type CVodeMem is type pointer to struct CVodeMemRec.
	// This structure contains fields to keep track of problem state.
	
	private class NVector {
		double data[];
		boolean own_data;
		
		void setData(double data[]) {
			this.data = data;
		}
		double[] getData() {
		    return data;	
		}
	}
	
	  
	private class CVodeMemRec {
	    
	  double cv_uround;         /* machine unit roundoff                         */   

	  /*-------------------------- 
	    Problem Specification Data 
	    --------------------------*/

	  //CVRhsFn cv_f;               /* y' = f(t,y(t))                                */
	  int cv_f;
	  //void *cv_user_data;         /* user pointer passed to f                      */
	  CVodeMemRec cv_user_data; 

	  int cv_lmm;                 /* lmm = ADAMS or BDF                            */
	  int cv_iter;                /* iter = FUNCTIONAL or NEWTON                   */

	  int cv_itol;                /* itol = CV_SS, CV_SV, or CV_WF, or CV_NN       */
	  double cv_reltol;         /* relative tolerance                            */
	  double cv_Sabstol;        /* scalar absolute tolerance                     */
	  NVector cv_Vabstol = new NVector();        /* vector absolute tolerance                     */
	  boolean cv_user_efun;   /* SUNTRUE if user sets efun                     */
	  //CVEwtFn cv_efun;            /* function to set ewt                           */
	  int cv_efun_select;
	  //void *cv_e_data;            /* user pointer passed to efun                   */
	  CVodeMemRec cv_e_data;

	  /*-----------------------
	    Quadrature Related Data 
	    -----------------------*/

	  boolean cv_quadr;       /* SUNTRUE if integrating quadratures            */

	  //CVQuadRhsFn cv_fQ;          /* q' = fQ(t, y(t))                              */

	  boolean cv_errconQ;     /* SUNTRUE if quadrs. are included in error test */

	  int cv_itolQ;               /* itolQ = CV_SS or CV_SV                        */
	  double cv_reltolQ;        /* relative tolerance for quadratures            */
	  double cv_SabstolQ;       /* scalar absolute tolerance for quadratures     */
	  NVector cv_VabstolQ = new NVector();       /* vector absolute tolerance for quadratures     */

	  /*------------------------
	    Sensitivity Related Data 
	    ------------------------*/

	  boolean cv_sensi;       /* SUNTRUE if computing sensitivities           */

	  int cv_Ns;                  /* Number of sensitivities                      */

	  int cv_ism;                 /* ism = SIMULTANEOUS or STAGGERED              */

	  //CVSensRhsFn cv_fS;          /* fS = (df/dy)*yS + (df/dp)                    */
	  //CVSensRhs1Fn cv_fS1;        /* fS1 = (df/dy)*yS_i + (df/dp)                 */
	  //void *cv_fS_data;           /* data pointer passed to fS                    */
	  CVodeMemRec cv_fS_data;
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
	  NVector cv_VabstolS[];      /* vector absolute tolerances for sensi.        */

	  /*-----------------------------------
	    Quadrature Sensitivity Related Data 
	    -----------------------------------*/

	  boolean cv_quadr_sensi; /* SUNTRUE if computing sensitivties of quadrs. */

	  //CVQuadSensRhsFn cv_fQS;     /* fQS = (dfQ/dy)*yS + (dfQ/dp)                 */
	  //void *cv_fQS_data;          /* data pointer passed to fQS                   */
	  CVodeMemRec cv_fQS_data;
	  boolean cv_fQSDQ;       /* SUNTRUE if using internal DQ functions       */

	  boolean cv_errconQS;    /* SUNTRUE if yQS are considered in err. con.   */

	  int cv_itolQS;
	  double cv_reltolQS;       /* relative tolerance for yQS                   */
	  double cv_SabstolQS[];     /* scalar absolute tolerances for yQS           */
	  NVector cv_VabstolQS[];     /* vector absolute tolerances for yQS           */

	  /*-----------------------
	    Nordsieck History Array 
	    -----------------------*/

	  NVector cv_zn[] = new NVector[L_MAX];   /* Nordsieck array, of size N x (q+1).
	                                 zn[j] is a vector of length N (j=0,...,q)
	                                 zn[j] = [1/factorial(j)] * h^j * 
	                                 (jth derivative of the interpolating poly.)  */

	  /*-------------------
	    Vectors of length N 
	    -------------------*/

	  NVector cv_ewt = new NVector();            /* error weight vector                          */
	  NVector cv_y = new NVector();              // y is used as temporary storage by the solver.
	                              /*   The memory is provided by the user to CVode 
	                                 where the vector is named yout.              */
	  NVector cv_acor = new NVector();           // In the context of the solution of the
	                               /*  nonlinear equation, acor = y_n(m) - y_n(0).
	                                 On return, this vector is scaled to give
	                                 the estimated local error in y.              */
	  NVector cv_tempv = new NVector();          /* temporary storage vector                     */
	  NVector cv_ftemp = new NVector();          /* temporary storage vector                     */
	  
	  /*--------------------------
	    Quadrature Related Vectors 
	    --------------------------*/

	  NVector cv_znQ[] = new NVector[L_MAX];     /* Nordsieck arrays for quadratures             */
	  NVector cv_ewtQ = new NVector();           /* error weight vector for quadratures          */
	  NVector cv_yQ = new NVector();             /* Unlike y, yQ is not allocated by the user    */
	  NVector cv_acorQ = new NVector();          /* acorQ = yQ_n(m) - yQ_n(0)                    */
	  NVector cv_tempvQ = new NVector();         /* temporary storage vector (~ tempv)           */

	  /*---------------------------
	    Sensitivity Related Vectors 
	    ---------------------------*/

	  NVector cv_znS[][] = new NVector[L_MAX][];    /* Nordsieck arrays for sensitivities           */
	  NVector cv_ewtS[];          /* error weight vectors for sensitivities       */
	  NVector cv_yS[];            /* yS=yS0 (allocated by the user)               */
	  NVector cv_acorS[];         /* acorS = yS_n(m) - yS_n(0)                    */
	  NVector cv_tempvS[];        /* temporary storage vector (~ tempv)           */
	  NVector cv_ftempS[];        /* temporary storage vector (~ ftemp)           */

	  boolean cv_stgr1alloc;  /* Did we allocate ncfS1, ncfnS1, and nniS1?    */

	  /*--------------------------------------
	    Quadrature Sensitivity Related Vectors 
	    --------------------------------------*/

	  NVector cv_znQS[][] = new NVector[L_MAX][];   /* Nordsieck arrays for quadr. sensitivities    */
	  NVector cv_ewtQS[];         /* error weight vectors for sensitivities       */
	  NVector cv_yQS[];           /* Unlike yS, yQS is not allocated by the user  */
	  NVector cv_acorQS[];        /* acorQS = yQS_n(m) - yQS_n(0)                 */
	  NVector cv_tempvQS[];       /* temporary storage vector (~ tempv)           */
	  NVector cv_ftempQ = new NVector();         /* temporary storage vector (~ ftemp)           */
	  
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
	  int cv_linit_select;

	  //int (*cv_lsetup)(struct CVodeMemRec *cv_mem, int convfail, 
			   //N_Vector ypred, N_Vector fpred, booleantype *jcurPtr, 
			   //N_Vector vtemp1, N_Vector vtemp2, N_Vector vtemp3); 
	  int cv_lsetup_select;

	  //int (*cv_lsolve)(struct CVodeMemRec *cv_mem, N_Vector b, N_Vector weight,
			   //N_Vector ycur, N_Vector fcur);
	  int cv_lsolve_select;

	  //int (*cv_lfree)(struct CVodeMemRec *cv_mem);
	  int cv_lfree_select;

	  /* Linear Solver specific memory */

	  //void *cv_lmem; 
	  CVDlsMemRec cv_lmem;

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
	  boolean cv_jcur[] = new boolean[1];         /* is Jacobian info for linear solver current? */
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
	  int cv_gfun;
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
    	  cv_mem.cv_f = -1;
    	  //cv_mem.cv_user_data  = null;
    	  cv_mem.cv_itol       = CV_NN;
    	  cv_mem.cv_user_efun  = false;
    	  //cv_mem.cv_efun       = null;
    	  cv_mem.cv_efun_select = -1;
    	  cv_mem.cv_e_data     = null;
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
    	  cv_mem.cv_gfun = -1;
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
                         String msgfmt, double... numbers)
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

     int CVodeInit(CVodeMemRec cv_mem, int f, double t0, NVector y0)
     {
       boolean nvectorOK, allocOK;
       long lrw1[] = new long[1];
       long liw1[] = new long[1];
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

       if (f < 0) {
         cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVodeInit",
                        MSGCV_NULL_F);
         return(CV_ILL_INPUT);
       }

       /* Test if all required vector operations are implemented */

       /*nvectorOK = cvCheckNvector(y0);
       if(!nvectorOK) {
         cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVodeInit",
                        MSGCV_BAD_NVECTOR);
         return(CV_ILL_INPUT);
       }*/

       /* Set space requirements for one N_Vector */

       if (y0.getData() != null) {
         N_VSpace_Serial(y0, lrw1, liw1);
       } else {
         lrw1[0] = 0;
         liw1[0] = 0;
       }
       cv_mem.cv_lrw1 = lrw1[0];
       cv_mem.cv_liw1 = liw1[0];

       /* Allocate the vectors (using y0 as a template) */

      allocOK = cvAllocVectors(cv_mem, y0);
       if (!allocOK) {
         cvProcessError(cv_mem, CV_MEM_FAIL, "CVODES", "CVodeInit",
                        MSGCV_MEM_FAIL);
         return(CV_MEM_FAIL);
       }

       /* All error checking is complete at this point */

       /* Copy the input parameters into CVODES state */

       //cv_mem.cv_f  = f;
       cv_mem.cv_tn = t0;

       /* Set step parameters */

       cv_mem.cv_q      = 1;
       cv_mem.cv_L      = 2;
       cv_mem.cv_qwait  = cv_mem.cv_L;
       cv_mem.cv_etamax = ETAMX1;

       cv_mem.cv_qu     = 0;
       cv_mem.cv_hu     = ZERO;
       cv_mem.cv_tolsf  = ONE;

       /* Set the linear solver addresses to NULL.
          (We check != NULL later, in CVode, if using CV_NEWTON.) */

       //cv_mem.cv_linit  = null;
       cv_mem.cv_linit_select = -1;
       //cv_mem.cv_lsetup = null;
       cv_mem.cv_lsetup_select = -1;
       //cv_mem.cv_lsolve = null;
       cv_mem.cv_lsolve_select = -1;
       //cv_mem.cv_lfree  = null;
       cv_mem.cv_lfree_select = -1;
       //cv_mem.cv_lmem   = null;

       /* Set forceSetup to SUNFALSE */

       cv_mem.cv_forceSetup = false;

       /* Initialize zn[0] in the history array */

       N_VScale_Serial(ONE, y0, cv_mem.cv_zn[0]);

       /* Initialize all the counters */

       cv_mem.cv_nst     = 0;
       cv_mem.cv_nfe     = 0;
       cv_mem.cv_ncfn    = 0;
       cv_mem.cv_netf    = 0;
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
       /* NOTE: We do this even if stab lim det was not
          turned on yet. This way, the user can turn it
          on at any time */

       cv_mem.cv_nor = 0;
       for (i = 1; i <= 5; i++)
         for (k = 1; k <= 3; k++) 
           cv_mem.cv_ssdat[i-1][k-1] = ZERO;

       /* Problem has been successfully initialized */

       cv_mem.cv_MallocDone = true;

       return(CV_SUCCESS);
     }
     
     void N_VSpace_Serial(NVector v, long lrw[], long liw[])
     {
       lrw[0] = v.getData().length;
       liw[0] = 1;

       return;
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
          (tmpl->ops->nvconst     == NULL) || // set all to constant
          (tmpl->ops->nvprod      == NULL) || // (double x[], double y[], double z[])
                                              // z[i] = x[i] * y[i]
          (tmpl->ops->nvdiv       == NULL) || // double x[], double y[], double z[])
                                              // z[i] = x[i]/y[i]
          (tmpl->ops->nvscale     == NULL) || // (double c, double x[], double z[])
                                              // z[i] = c * x[i]
          (tmpl->ops->nvabs       == NULL) || // (double x[], double z[])
                                              // z[i] = abs(x[i])
          (tmpl->ops->nvinv       == NULL) || // (double z[], double z[])
                                              // z[i] = 1.0/x[i]
          (tmpl->ops->nvaddconst  == NULL) || // (double x[], double b, double z[])
                                              // z[i] = x[i] + b
          (tmpl->ops->nvmaxnorm   == NULL) || // (double x[])
                                              // max(abs(x[i]))
          (tmpl->ops->nvwrmsnorm  == NULL) || // (double x[], double w[])
                                              // prod = x[i] * w[i]
                                              // sum = sum over all i of prod[i]*prod[i]
                                              // answer = sqrt(sum/N)
          (tmpl->ops->nvmin       == NULL))   // (double x[])
                                              // min(x[i])
         return(SUNFALSE);
       else
         return(SUNTRUE);
     }*/
     
     /*
      * cvAllocVectors
      *
      * This routine allocates the CVODES vectors ewt, acor, tempv, ftemp, and
      * zn[0], ..., zn[maxord].
      * If all memory allocations are successful, cvAllocVectors returns SUNTRUE. 
      * Otherwise all allocated memory is freed and cvAllocVectors returns SUNFALSE.
      * This routine also sets the optional outputs lrw and liw, which are
      * (respectively) the lengths of the real and integer work spaces
      * allocated here.
      */

     private boolean cvAllocVectors(CVodeMemRec cv_mem, NVector tmpl)
     {
       int i, j;

       /* Allocate ewt, acor, tempv, ftemp */
       
       cv_mem.cv_ewt = N_VClone(tmpl);
       if (cv_mem.cv_ewt == null) return(false);

       cv_mem.cv_acor = N_VClone(tmpl);
       if (cv_mem.cv_acor == null) {
         N_VDestroy(cv_mem.cv_ewt);
         return false;
       }

       cv_mem.cv_tempv = N_VClone(tmpl);
       if (cv_mem.cv_tempv == null) {
         N_VDestroy(cv_mem.cv_ewt);
         N_VDestroy(cv_mem.cv_acor);
         return false;
       }

       cv_mem.cv_ftemp = N_VClone(tmpl);
       if (cv_mem.cv_ftemp == null) {
         N_VDestroy(cv_mem.cv_tempv);
         N_VDestroy(cv_mem.cv_ewt);
         N_VDestroy(cv_mem.cv_acor);
         return false;
       }

       /* Allocate zn[0] ... zn[qmax] */

       for (j=0; j <= cv_mem.cv_qmax; j++) {
         cv_mem.cv_zn[j] = N_VClone(tmpl);
         if (cv_mem.cv_zn[j] == null) {
           N_VDestroy(cv_mem.cv_ewt);
           N_VDestroy(cv_mem.cv_acor);
           N_VDestroy(cv_mem.cv_tempv);
           N_VDestroy(cv_mem.cv_ftemp);
           for (i=0; i < j; i++) N_VDestroy(cv_mem.cv_zn[i]);
           return false;
         }
       }

       /* Update solver workspace lengths  */
       cv_mem.cv_lrw += (cv_mem.cv_qmax + 5)*cv_mem.cv_lrw1;
       cv_mem.cv_liw += (cv_mem.cv_qmax + 5)*cv_mem.cv_liw1;

       /* Store the value of qmax used here */
       cv_mem.cv_qmax_alloc = cv_mem.cv_qmax;

       return true;
     }
     
     private NVector N_VClone(NVector y) {
         NVector x = new NVector();
         x.data = y.data.clone();
         x.own_data = y.own_data;
         return x;
     }

     private void N_VDestroy(NVector x) {
    	 x.data = null;
    	 x = null;
     }
     
     int CVodeSVtolerances(CVodeMemRec cv_mem, double reltol, NVector abstol)
     {

       if (cv_mem==null) {
         cvProcessError(null, CV_MEM_NULL, "CVODES",
                        "CVodeSVtolerances", MSGCV_NO_MEM);
         return(CV_MEM_NULL);
       }

       if (cv_mem.cv_MallocDone == false) {
         cvProcessError(cv_mem, CV_NO_MALLOC, "CVODES",
                        "CVodeSVtolerances", MSGCV_NO_MALLOC);
         return(CV_NO_MALLOC);
       }

       /* Check inputs */

       if (reltol < ZERO) {
         cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES",
                        "CVodeSVtolerances", MSGCV_BAD_RELTOL);
         return(CV_ILL_INPUT);
       }

       if (N_VMin(abstol) < ZERO) {
         cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES",
                        "CVodeSVtolerances", MSGCV_BAD_ABSTOL);
         return(CV_ILL_INPUT);
       }

       /* Copy tolerances into memory */
       
       if ( !(cv_mem.cv_VabstolMallocDone) ) {
         cv_mem.cv_Vabstol = N_VClone(cv_mem.cv_ewt);
         cv_mem.cv_lrw += cv_mem.cv_lrw1;
         cv_mem.cv_liw += cv_mem.cv_liw1;
         cv_mem.cv_VabstolMallocDone = true;
       }

       cv_mem.cv_reltol = reltol;
       N_VScale_Serial(ONE, abstol, cv_mem.cv_Vabstol);

       cv_mem.cv_itol = CV_SV;

       cv_mem.cv_user_efun = false;
       //cv_mem.cv_efun = cvEwtSet;
       cv_mem.cv_efun_select = cvEwtSet_select;
       cv_mem.cv_e_data = null; /* will be set to cvode_mem in InitialSetup */

       return(CV_SUCCESS);
     }

     private double N_VMin(NVector x) {
    	 int i;
    	 if ((x == null) || (x.data == null) || (x.data.length == 0)) {
    		 return Double.NaN;
    	 }
    	 double data[] = x.data;
    	 int n = data.length;
    	 double min = data[0];
    	 for (i = 1; i < n; i++) {
    		 if (data[i] < min) {
    			 min = data[i];
    		 }
    	 }
    	 return min;
     }
     
     /*
      * CVodeRootInit
      *
      * CVodeRootInit initializes a rootfinding problem to be solved
      * during the integration of the ODE system.  It loads the root
      * function pointer and the number of root functions, and allocates
      * workspace memory.  The return value is CV_SUCCESS = 0 if no errors
      * occurred, or a negative value otherwise.
      */

     int CVodeRootInit(CVodeMemRec cv_mem, int nrtfn, int g)
     {
       int i, nrt;

       /* Check cvode_mem */
       if (cv_mem==null) {
         cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeRootInit",
                        MSGCV_NO_MEM);
         return(CV_MEM_NULL);
       }

       nrt = (nrtfn < 0) ? 0 : nrtfn;

       /* If rerunning CVodeRootInit() with a different number of root
          functions (changing number of gfun components), then free
          currently held memory resources */
       if ((nrt != cv_mem.cv_nrtfn) && (cv_mem.cv_nrtfn > 0)) {
         cv_mem.cv_glo = null;
         cv_mem.cv_ghi = null;
         cv_mem.cv_grout = null;
         cv_mem.cv_iroots = null;
         cv_mem.cv_rootdir = null;
         cv_mem.cv_gactive = null;

         cv_mem.cv_lrw -= 3 * (cv_mem.cv_nrtfn);
         cv_mem.cv_liw -= 3 * (cv_mem.cv_nrtfn);
       }

       /* If CVodeRootInit() was called with nrtfn == 0, then set cv_nrtfn to
          zero and cv_gfun to NULL before returning */
       if (nrt == 0) {
         cv_mem.cv_nrtfn = nrt;
         cv_mem.cv_gfun = -1;
         return(CV_SUCCESS);
       }

       /* If rerunning CVodeRootInit() with the same number of root functions
          (not changing number of gfun components), then check if the root
          function argument has changed */
       /* If g != NULL then return as currently reserved memory resources
          will suffice */
       if (nrt == cv_mem.cv_nrtfn) {
         if (g != cv_mem.cv_gfun) {
           if (g < 0) {
     	     cv_mem.cv_glo = null;
     	     cv_mem.cv_ghi = null;
     	     cv_mem.cv_grout = null;
     	     cv_mem.cv_iroots = null;
             cv_mem.cv_rootdir = null;
             cv_mem.cv_gactive = null;

             cv_mem.cv_lrw -= 3*nrt;
             cv_mem.cv_liw -= 3*nrt;

             cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVodeRootInit",
                            MSGCV_NULL_G);
             return(CV_ILL_INPUT);
           }
           else {
     	     cv_mem.cv_gfun = g;
             return(CV_SUCCESS);
           }
         }
         else return(CV_SUCCESS);
       }

       /* Set variable values in CVode memory block */
       cv_mem.cv_nrtfn = nrt;
       if (g < 0) {
         cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVodeRootInit",
                        MSGCV_NULL_G);
         return(CV_ILL_INPUT);
       }
       else cv_mem.cv_gfun = g;

       /* Allocate necessary memory and return */
       cv_mem.cv_glo = null;
       cv_mem.cv_glo = new double[nrt];
       if (cv_mem.cv_glo == null) {
         cvProcessError(cv_mem, CV_MEM_FAIL, "CVODES", "CVodeRootInit",
                        MSGCV_MEM_FAIL);
         return(CV_MEM_FAIL);
       }
         
       cv_mem.cv_ghi = null;
       cv_mem.cv_ghi = new double[nrt];
       if (cv_mem.cv_ghi == null) {
         cv_mem.cv_glo = null;
         cvProcessError(cv_mem, CV_MEM_FAIL, "CVODES", "CVodeRootInit",
                        MSGCV_MEM_FAIL);
         return(CV_MEM_FAIL);
       }
         
       cv_mem.cv_grout = null;
       cv_mem.cv_grout = new double[nrt];
       if (cv_mem.cv_grout == null) {
         cv_mem.cv_glo = null;
         cv_mem.cv_ghi = null;
         cvProcessError(cv_mem, CV_MEM_FAIL, "CVODES", "CVodeRootInit",
                        MSGCV_MEM_FAIL);
         return(CV_MEM_FAIL);
       }

       cv_mem.cv_iroots = null;
       cv_mem.cv_iroots = new int[nrt];
       if (cv_mem.cv_iroots == null) {
         cv_mem.cv_glo = null;
         cv_mem.cv_ghi = null;
         cv_mem.cv_grout = null;
         cvProcessError(cv_mem, CV_MEM_FAIL, "CVODES", "CVodeRootInit",
                        MSGCV_MEM_FAIL);
         return(CV_MEM_FAIL);
       }

       cv_mem.cv_rootdir = null;
       cv_mem.cv_rootdir = new int[nrt];
       if (cv_mem.cv_rootdir == null) {
         cv_mem.cv_glo = null; 
         cv_mem.cv_ghi = null;
         cv_mem.cv_grout = null;
         cv_mem.cv_iroots = null;
         cvProcessError(cv_mem, CV_MEM_FAIL, "CVODES", "CVodeRootInit",
                        MSGCV_MEM_FAIL);
         return(CV_MEM_FAIL);
       }


       cv_mem.cv_gactive = null;
       cv_mem.cv_gactive = new boolean[nrt];
       if (cv_mem.cv_gactive == null) {
         cv_mem.cv_glo = null; 
         cv_mem.cv_ghi = null;
         cv_mem.cv_grout = null;
         cv_mem.cv_iroots = null;
         cv_mem.cv_rootdir = null;
         cvProcessError(cv_mem, CV_MEM_FAIL, "CVODES", "CVodeRootInit",
                        MSGCV_MEM_FAIL);
         return(CV_MEM_FAIL);
       }


       /* Set default values for rootdir (both directions) */
       for(i=0; i<nrt; i++) cv_mem.cv_rootdir[i] = 0;

       /* Set default values for gactive (all active) */
       for(i=0; i<nrt; i++) cv_mem.cv_gactive[i] = true;

       cv_mem.cv_lrw += 3*nrt;
       cv_mem.cv_liw += 3*nrt;

       return(CV_SUCCESS);
     }
     
     class SUNLinearSolver {
         long N; // Size of the linear system, the number of matrix rows
         int pivots[]; // Array of size N, index array for partial pivoting in LU factorization
         int last_flag; // last error return flag from internal setup/solve
         SUNLinearSolver_Type type;
     }
     
     private SUNLinearSolver SUNDenseLinearSolver(NVector y, double A[][]) {
    	 int matrixRows = A.length;
    	 int matrixColumns = A[0].length;
    	 if (matrixRows != matrixColumns) {
    		 MipavUtil.displayError("Did not have the matrix rows = matrix columns required for a LinearSolver");
    		 return null;
    	 }
    	 int vecLength = y.getData().length;
    	 if (matrixRows != vecLength) {
    		 MipavUtil.displayError("Did not have the matrix rows equal to vector length required for a LinearSolver");
    		 return null;
    	 }
    	 SUNLinearSolver S = new SUNLinearSolver();
    	 S.N = matrixRows;
    	 S.last_flag = 0;
    	 S.pivots = new int[matrixRows];
    	 S.type = SUNLinearSolver_Type.SUNLINEARSOLVER_DIRECT;
    	 return S;
     }
     
     /*---------------------------------------------------------------
     CVDlsSetLinearSolver specifies the direct linear solver.
    ---------------------------------------------------------------*/
    int CVDlsSetLinearSolver(CVodeMemRec cv_mem, SUNLinearSolver LS,
                           double A[][])
    {
      CVDlsMemRec cvdls_mem;

      /* Return immediately if any input is NULL */
      if (cv_mem == null) {
        cvProcessError(null, CVDLS_MEM_NULL, "CVSDLS", 
                       "CVDlsSetLinearSolver", MSGD_CVMEM_NULL);
        return(CVDLS_MEM_NULL);
      }
      if ( (LS == null)  || (A == null) ) {
        cvProcessError(null, CVDLS_ILL_INPUT, "CVSDLS", 
                       "CVDlsSetLinearSolver",
                        "Both LS and A must be non-NULL");
        return(CVDLS_ILL_INPUT);
      }

      /* Test if solver and vector are compatible with DLS */
      if (LS.type != SUNLinearSolver_Type.SUNLINEARSOLVER_DIRECT) {
        cvProcessError(cv_mem, CVDLS_ILL_INPUT, "CVSDLS", 
                       "CVDlsSetLinearSolver", 
                       "Non-direct LS supplied to CVDls interface");
        return(CVDLS_ILL_INPUT);
      }
      //if (cv_mem.cv_tempv.ops.nvgetarraypointer == null ||
          //cv_mem.cv_tempv.ops.nvsetarraypointer ==  null) {
        //cvProcessError(cv_mem, CVDLS_ILL_INPUT, "CVSDLS", 
                       //"CVDlsSetLinearSolver", MSGD_BAD_NVECTOR);
        //return(CVDLS_ILL_INPUT);
      //}

      /* free any existing system solver attached to CVode */
      if (cv_mem.cv_lfree_select > 0)  cv_lfree(cv_mem, cv_mem.cv_lfree_select);

      /* Set four main system linear solver function fields in cv_mem */
      //cv_mem.cv_linit  = cvDlsInitialize;
      cv_mem.cv_linit_select = cvDlsInitialize_select;
      //cv_mem.cv_lsetup = cvDlsSetup;
      cv_mem.cv_lsetup_select = cvDlsSetup_select;
      //cv_mem.cv_lsolve = cvDlsSolve;
      cv_mem.cv_lsolve_select = cvDlsSolve_select;
      //cv_mem.cv_lfree  = cvDlsFree;
      cv_mem.cv_lfree_select = cvDlsFree_select;
      
      /* Get memory for CVDlsMemRec */
      cvdls_mem = null;
      cvdls_mem = new CVDlsMemRec();

      /* set SUNLinearSolver pointer */
      cvdls_mem.LS = LS;
      
      /* Initialize Jacobian-related data */
      cvdls_mem.jacDQ = true;
      cvdls_mem.jac = cvDlsDQJac;
      cvdls_mem.J_data = cv_mem;
      cvdls_mem.last_flag = CVDLS_SUCCESS;

      /* Initialize counters */
      cvDlsInitializeCounters(cvdls_mem);

      /* Store pointer to A and create saved_J */
      cvdls_mem.A = A;
      cvdls_mem.savedJ = A.clone();
      if (cvdls_mem.savedJ == null) {
        cvProcessError(cv_mem, CVDLS_MEM_FAIL, "CVSDLS", 
                        "CVDlsSetLinearSolver", MSGD_MEM_FAIL);
        cvdls_mem = null;
        return(CVDLS_MEM_FAIL);
      }

      /* Allocate memory for x */
      cvdls_mem.x = N_VClone(cv_mem.cv_tempv);
      if (cvdls_mem.x == null) {
        cvProcessError(cv_mem, CVDLS_MEM_FAIL, "CVSDLS", 
                        "CVDlsSetLinearSolver", MSGD_MEM_FAIL);
        cvdls_mem.savedJ = null;
        cvdls_mem = null;
        return(CVDLS_MEM_FAIL);
      }
      /* Attach linear solver memory to integrator memory */
      cv_mem.cv_lmem = cvdls_mem;

      return(CVDLS_SUCCESS);
    }
    
    //-----------------------------------------------------------------
    //cvDlsInitializeCounters
    //-----------------------------------------------------------------
    //This routine resets the counters inside the CVDlsMem object.
    //-----------------------------------------------------------------*/
  int cvDlsInitializeCounters(CVDlsMemRec cvdls_mem)
  {
    cvdls_mem.nje   = 0;
    cvdls_mem.nfeDQ = 0;
    cvdls_mem.nstlj = 0;
    return(0);
  }

    
    
    
    //-----------------------------------------------------------------
    //CVDlsMem is pointer to a CVDlsMemRec structure.
    //-----------------------------------------------------------------*/

  class CVDlsMemRec {

    boolean jacDQ;    /* true if using internal DQ Jacobian approx. */
    //CVDlsJacFn jac;       /* Jacobian routine to be called                 */
    int jac;
    //void *J_data;         /* data pointer passed to jac                    */
    CVodeMemRec J_data;

    double A[][];          /* A = I - gamma * df/dy                         */
    double savedJ[][];     /* savedJ = old Jacobian                         */

    SUNLinearSolver LS;   /* generic direct linear solver object           */

    NVector x;           /* solution vector used by SUNLinearSolver       */
    
    long nstlj;       /* nstlj = nst at last Jacobian eval.            */

    long nje;         /* nje = no. of calls to jac                     */

    long nfeDQ;       /* no. of calls to f due to DQ Jacobian approx.  */

    int last_flag;   /* last error return flag                        */

  };


  /* CVDlsSetJacFn specifies the Jacobian function. */
  int CVDlsSetJacFn(CVodeMemRec cv_mem, int jac)
  {
    CVDlsMemRec cvdls_mem;

    /* Return immediately if cvode_mem or cv_mem->cv_lmem are NULL */
    if (cv_mem == null) {
      cvProcessError(null, CVDLS_MEM_NULL, "CVSDLS",
                     "CVDlsSetJacFn", MSGD_CVMEM_NULL);
      return(CVDLS_MEM_NULL);
    }
  
    if (cv_mem.cv_lmem == null) {
      cvProcessError(cv_mem, CVDLS_LMEM_NULL, "CVSDLS",
                     "CVDlsSetJacFn", MSGD_LMEM_NULL);
      return(CVDLS_LMEM_NULL);
    }
    cvdls_mem = cv_mem.cv_lmem;

    if (jac >= 0) {
      cvdls_mem.jacDQ  = false;
      cvdls_mem.jac    = jac;
      cvdls_mem.J_data = cv_mem.cv_user_data;
    } else {
      cvdls_mem.jacDQ  = true;
      cvdls_mem.jac    = cvDlsDQJac;
      cvdls_mem.J_data = cv_mem;
    }

    return(CVDLS_SUCCESS);
  }
  
  /*
   * CVode
   *
   * This routine is the main driver of the CVODES package. 
   *
   * It integrates over a time interval defined by the user, by calling
   * cvStep to do internal time steps.
   *
   * The first time that CVode is called for a successfully initialized
   * problem, it computes a tentative initial step size h.
   *
   * CVode supports two modes, specified by itask: CV_NORMAL, CV_ONE_STEP.
   * In the CV_NORMAL mode, the solver steps until it reaches or passes tout
   * and then interpolates to obtain y(tout).
   * In the CV_ONE_STEP mode, it takes one internal step and returns.
   */

  int CVode(CVodeMemRec cv_mem, double tout, NVector yout, 
            double tret[], int itask)
  {
    long nstloc; 
    int retval, hflag, kflag, istate, is, ir, ier, irfndp;
    double troundoff, tout_hin, rh, nrm;
    boolean inactive_roots;

    /*
     * -------------------------------------
     * 1. Check and process inputs
     * -------------------------------------
     */

    /* Check if cvode_mem exists */
    if (cv_mem == null) {
      cvProcessError(null, CV_MEM_NULL, "CVODES", "CVode",
                     MSGCV_NO_MEM);
      return(CV_MEM_NULL);
    }

    /* Check if cvode_mem was allocated */
    if (cv_mem.cv_MallocDone == false) {
      cvProcessError(cv_mem, CV_NO_MALLOC, "CVODES", "CVode",
                     MSGCV_NO_MALLOC);
      return(CV_NO_MALLOC);
    }
    
    /* Check for yout != NULL */
    if ((cv_mem.cv_y = yout) == null) {
      cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                     MSGCV_YOUT_NULL);
      return(CV_ILL_INPUT);
    }
    
    /* Check for tret != NULL */
    if (tret == null) {
      cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                     MSGCV_TRET_NULL);
      return(CV_ILL_INPUT);
    }

    /* Check for valid itask */
    if ( (itask != CV_NORMAL) && (itask != CV_ONE_STEP) ) {
      cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                     MSGCV_BAD_ITASK);
      return(CV_ILL_INPUT);
    }

    if (itask == CV_NORMAL) cv_mem.cv_toutc = tout;
    cv_mem.cv_taskc = itask;

    /*
     * ----------------------------------------
     * 2. Initializations performed only at
     *    the first step (nst=0):
     *    - initial setup
     *    - initialize Nordsieck history array
     *    - compute initial step size
     *    - check for approach to tstop
     *    - check for approach to a root
     * ----------------------------------------
     */

    if (cv_mem.cv_nst == 0) {

      cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;

      /* Check inputs for corectness */

      ier = cvInitialSetup(cv_mem);
      if (ier!= CV_SUCCESS) return(ier);

      /* 
       * Call f at (t0,y0), set zn[1] = y'(t0). 
       * If computing any quadratures, call fQ at (t0,y0), set znQ[1] = yQ'(t0)
       * If computing sensitivities, call fS at (t0,y0,yS0), set znS[1][is] = yS'(t0), is=1,...,Ns.
       * If computing quadr. sensi., call fQS at (t0,y0,yS0), set znQS[1][is] = yQS'(t0), is=1,...,Ns.
       */

      retval = f(cv_mem.cv_tn, cv_mem.cv_zn[0],
                            cv_mem.cv_zn[1], cv_mem.cv_user_data); 
      cv_mem.cv_nfe++;
      if (retval < 0) {
        cvProcessError(cv_mem, CV_RHSFUNC_FAIL, "CVODES", "CVode",
                       MSGCV_RHSFUNC_FAILED, cv_mem.cv_tn);
        return(CV_RHSFUNC_FAIL);
      }
      if (retval > 0) {
        cvProcessError(cv_mem, CV_FIRST_RHSFUNC_ERR, "CVODES", "CVode",
                       MSGCV_RHSFUNC_FIRST);
        return(CV_FIRST_RHSFUNC_ERR);
      }

      if (cv_mem.cv_quadr) {
        retval = fQ(cv_mem.cv_tn, cv_mem.cv_zn[0],
                               cv_mem.cv_znQ[1], cv_mem.cv_user_data);
        cv_mem.cv_nfQe++;
        if (retval < 0) {
          cvProcessError(cv_mem, CV_QRHSFUNC_FAIL, "CVODES", "CVode",
                         MSGCV_QRHSFUNC_FAILED, cv_mem.cv_tn);
          return(CV_QRHSFUNC_FAIL);
        }
        if (retval > 0) {
          cvProcessError(cv_mem, CV_FIRST_QRHSFUNC_ERR, "CVODES",
                         "CVode", MSGCV_QRHSFUNC_FIRST);
          return(CV_FIRST_QRHSFUNC_ERR);
        }
      }

      if (cv_mem.cv_sensi) {
        retval = cvSensRhsWrapper(cv_mem, cv_mem.cv_tn, cv_mem.cv_zn[0],
                                  cv_mem.cv_zn[1], cv_mem.cv_znS[0],
                                  cv_mem.cv_znS[1], cv_mem.cv_tempv,
                                  cv_mem.cv_ftemp);
        if (retval < 0) {
          cvProcessError(cv_mem, CV_SRHSFUNC_FAIL, "CVODES", "CVode",
                         MSGCV_SRHSFUNC_FAILED, cv_mem.cv_tn);
          return(CV_SRHSFUNC_FAIL);
        } 
        if (retval > 0) {
          cvProcessError(cv_mem, CV_FIRST_SRHSFUNC_ERR, "CVODES",
                         "CVode", MSGCV_SRHSFUNC_FIRST);
          return(CV_FIRST_SRHSFUNC_ERR);
        }
      }

      if (cv_mem.cv_quadr_sensi) {
        retval = fQS(cv_mem.cv_Ns, cv_mem.cv_tn, cv_mem.cv_zn[0],
                                cv_mem.cv_znS[0], cv_mem.cv_znQ[1],
                                cv_mem.cv_znQS[1], cv_mem.cv_fQS_data,
                                cv_mem.cv_tempv, cv_mem.cv_tempvQ); 
        cv_mem.cv_nfQSe++;
        if (retval < 0) {
          cvProcessError(cv_mem, CV_QSRHSFUNC_FAIL, "CVODES", "CVode",
                         MSGCV_QSRHSFUNC_FAILED, cv_mem.cv_tn);
          return(CV_QSRHSFUNC_FAIL);
        } 
        if (retval > 0) {
          cvProcessError(cv_mem, CV_FIRST_QSRHSFUNC_ERR, "CVODES",
                         "CVode", MSGCV_QSRHSFUNC_FIRST);
          return(CV_FIRST_QSRHSFUNC_ERR);
        }
      } 

      /* Test input tstop for legality. */

      if (cv_mem.cv_tstopset) {
        if ( (cv_mem.cv_tstop - cv_mem.cv_tn)*(tout - cv_mem.cv_tn) <= ZERO ) {
          cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                         MSGCV_BAD_TSTOP, cv_mem.cv_tstop, cv_mem.cv_tn);
          return(CV_ILL_INPUT);
        }
      }

      /* Set initial h (from H0 or cvHin). */
      
      cv_mem.cv_h = cv_mem.cv_hin;
      if ( (cv_mem.cv_h != ZERO) && ((tout-cv_mem.cv_tn)*cv_mem.cv_h < ZERO) ) {
        cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode", MSGCV_BAD_H0);
        return(CV_ILL_INPUT);
      }
      if (cv_mem.cv_h == ZERO) {
        tout_hin = tout;
        if ( cv_mem.cv_tstopset &&
             (tout-cv_mem.cv_tn)*(tout-cv_mem.cv_tstop) > ZERO )
          tout_hin = cv_mem.cv_tstop; 
        hflag = cvHin(cv_mem, tout_hin);
        if (hflag != CV_SUCCESS) {
          istate = cvHandleFailure(cv_mem, hflag);
          return(istate);
        }
      }
      rh = Math.abs(cv_mem.cv_h)*cv_mem.cv_hmax_inv;
      if (rh > ONE) cv_mem.cv_h /= rh;
      if (Math.abs(cv_mem.cv_h) < cv_mem.cv_hmin)
        cv_mem.cv_h *= cv_mem.cv_hmin/Math.abs(cv_mem.cv_h);

      /* Check for approach to tstop */

      if (cv_mem.cv_tstopset) {
        if ( (cv_mem.cv_tn + cv_mem.cv_h - cv_mem.cv_tstop)*cv_mem.cv_h > ZERO ) 
          cv_mem.cv_h = (cv_mem.cv_tstop - cv_mem.cv_tn)*(ONE-FOUR*cv_mem.cv_uround);
      }

      /* 
       * Scale zn[1] by h.
       * If computing any quadratures, scale znQ[1] by h.
       * If computing sensitivities,  scale znS[1][is] by h. 
       * If computing quadrature sensitivities,  scale znQS[1][is] by h. 
       */

      cv_mem.cv_hscale = cv_mem.cv_h;
      cv_mem.cv_h0u    = cv_mem.cv_h;
      cv_mem.cv_hprime = cv_mem.cv_h;

      N_VScale_Serial(cv_mem.cv_h, cv_mem.cv_zn[1], cv_mem.cv_zn[1]);
      
      if (cv_mem.cv_quadr)
        N_VScale_Serial(cv_mem.cv_h, cv_mem.cv_znQ[1], cv_mem.cv_znQ[1]);

      if (cv_mem.cv_sensi)
        for (is=0; is<cv_mem.cv_Ns; is++) 
          N_VScale_Serial(cv_mem.cv_h, cv_mem.cv_znS[1][is], cv_mem.cv_znS[1][is]);

      if (cv_mem.cv_quadr_sensi)
        for (is=0; is<cv_mem.cv_Ns; is++) 
          N_VScale_Serial(cv_mem.cv_h, cv_mem.cv_znQS[1][is], cv_mem.cv_znQS[1][is]);
      
      /* Check for zeros of root function g at and near t0. */

      if (cv_mem.cv_nrtfn > 0) {

        retval = cvRcheck1(cv_mem);

        if (retval == CV_RTFUNC_FAIL) {
          cvProcessError(cv_mem, CV_RTFUNC_FAIL, "CVODES", "cvRcheck1",
                         MSGCV_RTFUNC_FAILED, cv_mem.cv_tn);
          return(CV_RTFUNC_FAIL);
        }

      }

    } /* end first call block */ // if (cv_mem.cv_nst == 0)

    /*
     * ------------------------------------------------------
     * 3. At following steps, perform stop tests:
     *    - check for root in last step
     *    - check if we passed tstop
     *    - check if we passed tout (NORMAL mode)
     *    - check if current tn was returned (ONE_STEP mode)
     *    - check if we are close to tstop
     *      (adjust step size if needed)
     * -------------------------------------------------------
     */

    if (cv_mem.cv_nst > 0) {

      /* Estimate an infinitesimal time interval to be used as
         a roundoff for time quantities (based on current time 
         and step size) */
      troundoff = FUZZ_FACTOR * cv_mem.cv_uround *
        (Math.abs(cv_mem.cv_tn) + Math.abs(cv_mem.cv_h));

      /* First check for a root in the last step taken, other than the
         last root found, if any.  If itask = CV_ONE_STEP and y(tn) was not
         returned because of an intervening root, return y(tn) now.     */
      if (cv_mem.cv_nrtfn > 0) {
        
        irfndp = cv_mem.cv_irfnd;
        
        retval = cvRcheck2(cv_mem);

        if (retval == CLOSERT) {
          cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvRcheck2",
                         MSGCV_CLOSE_ROOTS, cv_mem.cv_tlo);
          return(CV_ILL_INPUT);
        } else if (retval == CV_RTFUNC_FAIL) {
          cvProcessError(cv_mem, CV_RTFUNC_FAIL, "CVODES", "cvRcheck2",
                         MSGCV_RTFUNC_FAILED, cv_mem.cv_tlo);
          return(CV_RTFUNC_FAIL);
        } else if (retval == RTFOUND) {
          cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tlo;
          return(CV_ROOT_RETURN);
        }
        
        /* If tn is distinct from tretlast (within roundoff),
           check remaining interval for roots */
        if ( Math.abs(cv_mem.cv_tn - cv_mem.cv_tretlast) > troundoff ) {

          retval = cvRcheck3(cv_mem);

          if (retval == CV_SUCCESS) {     /* no root found */
            cv_mem.cv_irfnd = 0;
            if ((irfndp == 1) && (itask == CV_ONE_STEP)) {
              cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;
              N_VScale_Serial(ONE, cv_mem.cv_zn[0], yout);
              return(CV_SUCCESS);
            }
          } else if (retval == RTFOUND) {  /* a new root was found */
            cv_mem.cv_irfnd = 1;
            cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tlo;
            return(CV_ROOT_RETURN);
          } else if (retval == CV_RTFUNC_FAIL) {  /* g failed */
            cvProcessError(cv_mem, CV_RTFUNC_FAIL, "CVODES", "cvRcheck3", 
                           MSGCV_RTFUNC_FAILED, cv_mem.cv_tlo);
            return(CV_RTFUNC_FAIL);
          }

        }
        
      } /* end of root stop check */
      
      /* In CV_NORMAL mode, test if tout was reached */
      if ( (itask == CV_NORMAL) && ((cv_mem.cv_tn-tout)*cv_mem.cv_h >= ZERO) ) {
        cv_mem.cv_tretlast = tret[0] = tout;
        ier =  CVodeGetDky(cv_mem, tout, 0, yout);
        if (ier != CV_SUCCESS) {
          cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                         MSGCV_BAD_TOUT, tout);
          return(CV_ILL_INPUT);
        }
        return(CV_SUCCESS);
      }
      
      /* In CV_ONE_STEP mode, test if tn was returned */
      if ( itask == CV_ONE_STEP &&
           Math.abs(cv_mem.cv_tn - cv_mem.cv_tretlast) > troundoff ) {
        cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;
        N_VScale_Serial(ONE, cv_mem.cv_zn[0], yout);
        return(CV_SUCCESS);
      }
      
      /* Test for tn at tstop or near tstop */
      if ( cv_mem.cv_tstopset ) {
        
        if ( Math.abs(cv_mem.cv_tn - cv_mem.cv_tstop) <= troundoff ) {
          ier =  CVodeGetDky(cv_mem, cv_mem.cv_tstop, 0, yout);
          if (ier != CV_SUCCESS) {
            cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                           MSGCV_BAD_TSTOP, cv_mem.cv_tstop, cv_mem.cv_tn);
            return(CV_ILL_INPUT);
          }
          cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tstop;
          cv_mem.cv_tstopset = false;
          return(CV_TSTOP_RETURN);
        }
        
        /* If next step would overtake tstop, adjust stepsize */
        if ( (cv_mem.cv_tn + cv_mem.cv_hprime - cv_mem.cv_tstop)*cv_mem.cv_h > ZERO ) {
          cv_mem.cv_hprime = (cv_mem.cv_tstop - cv_mem.cv_tn)*(ONE-FOUR*cv_mem.cv_uround);
          cv_mem.cv_eta = cv_mem.cv_hprime / cv_mem.cv_h;
        }
        
      }
      
    } /* end stopping tests block at nst>0 */  
    
    /*
     * --------------------------------------------------
     * 4. Looping point for internal steps
     *
     *    4.1. check for errors (too many steps, too much
     *         accuracy requested, step size too small)
     *    4.2. take a new step (call cvStep)
     *    4.3. stop on error 
     *    4.4. perform stop tests:
     *         - check for root in last step
     *         - check if tout was passed
     *         - check if close to tstop
     *         - check if in ONE_STEP mode (must return)
     * --------------------------------------------------
     */  
    
    nstloc = 0;
    for(;;) {
     
      cv_mem.cv_next_h = cv_mem.cv_h;
      cv_mem.cv_next_q = cv_mem.cv_q;
      
      /* Reset and check ewt, ewtQ, ewtS */   
      if (cv_mem.cv_nst > 0) {

        ier = cv_efun(cv_mem.cv_zn[0], cv_mem.cv_ewt, cv_mem.cv_e_data, cv_mem.cv_efun_select);
        if(ier != 0) {
          if (cv_mem.cv_itol == CV_WF)
            cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                           MSGCV_EWT_NOW_FAIL, cv_mem.cv_tn);
          else
            cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                           MSGCV_EWT_NOW_BAD, cv_mem.cv_tn);
          istate = CV_ILL_INPUT;
          cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;
          N_VScale_Serial(ONE, cv_mem.cv_zn[0], yout);
          break;
        }

        if (cv_mem.cv_quadr && cv_mem.cv_errconQ) {
          ier = cvQuadEwtSet(cv_mem, cv_mem.cv_znQ[0], cv_mem.cv_ewtQ);
          if(ier != 0) {
            cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                           MSGCV_EWTQ_NOW_BAD, cv_mem.cv_tn);
            istate = CV_ILL_INPUT;
            cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;
            N_VScale_Serial(ONE, cv_mem.cv_zn[0], yout);
            break;
          }
        }

        if (cv_mem.cv_sensi) {
          ier = cvSensEwtSet(cv_mem, cv_mem.cv_znS[0], cv_mem.cv_ewtS);
          if (ier != 0) {
            cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                           MSGCV_EWTS_NOW_BAD, cv_mem.cv_tn);
            istate = CV_ILL_INPUT;
            cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;
            N_VScale_Serial(ONE, cv_mem.cv_zn[0], yout);
            break;
          }
        }

        if (cv_mem.cv_quadr_sensi && cv_mem.cv_errconQS) {
          ier = cvQuadSensEwtSet(cv_mem, cv_mem.cv_znQS[0], cv_mem.cv_ewtQS);
          if (ier != 0) {
            cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "CVode",
                           MSGCV_EWTQS_NOW_BAD, cv_mem.cv_tn);
            istate = CV_ILL_INPUT;
            cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;
            N_VScale_Serial(ONE, cv_mem.cv_zn[0], yout);
            break;
          }
        }

      }

      /* Check for too many steps */
      if ( (cv_mem.cv_mxstep>0) && (nstloc >= cv_mem.cv_mxstep) ) {
        cvProcessError(cv_mem, CV_TOO_MUCH_WORK, "CVODES", "CVode",
                       MSGCV_MAX_STEPS, cv_mem.cv_tn);
        istate = CV_TOO_MUCH_WORK;
        cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;
        N_VScale_Serial(ONE, cv_mem.cv_zn[0], yout);
        break;
      }

      /* Check for too much accuracy requested */
      nrm = N_VWrmsNorm_Serial(cv_mem.cv_zn[0], cv_mem.cv_ewt);
      if (cv_mem.cv_quadr && cv_mem.cv_errconQ) {
        nrm = cvQuadUpdateNorm(cv_mem, nrm, cv_mem.cv_znQ[0], cv_mem.cv_ewtQ); 
      }
      if (cv_mem.cv_sensi && cv_mem.cv_errconS) {
        nrm = cvSensUpdateNorm(cv_mem, nrm, cv_mem.cv_znS[0], cv_mem.cv_ewtS);
      }
      if (cv_mem.cv_quadr_sensi && cv_mem.cv_errconQS) {
        nrm = cvQuadSensUpdateNorm(cv_mem, nrm, cv_mem.cv_znQS[0], cv_mem.cv_ewtQS);
      }
      cv_mem.cv_tolsf = cv_mem.cv_uround * nrm;
      if (cv_mem.cv_tolsf > ONE) {
        cvProcessError(cv_mem, CV_TOO_MUCH_ACC, "CVODES", "CVode",
                       MSGCV_TOO_MUCH_ACC, cv_mem.cv_tn);
        istate = CV_TOO_MUCH_ACC;
        cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;
        N_VScale_Serial(ONE, cv_mem.cv_zn[0], yout);
        cv_mem.cv_tolsf *= TWO;
        break;
      } else {
        cv_mem.cv_tolsf = ONE;
      }
      
      /* Check for h below roundoff level in tn */
      if (cv_mem.cv_tn + cv_mem.cv_h == cv_mem.cv_tn) {
        cv_mem.cv_nhnil++;
        if (cv_mem.cv_nhnil <= cv_mem.cv_mxhnil) 
          cvProcessError(cv_mem, CV_WARNING, "CVODES", "CVode", MSGCV_HNIL,
                         cv_mem.cv_tn, cv_mem.cv_h);
        if (cv_mem.cv_nhnil == cv_mem.cv_mxhnil) 
          cvProcessError(cv_mem, CV_WARNING, "CVODES", "CVode", MSGCV_HNIL_DONE);
      }

      /* Call cvStep to take a step */
      kflag = cvStep(cv_mem);

      /* Process failed step cases, and exit loop */
      if (kflag != CV_SUCCESS) {
        istate = cvHandleFailure(cv_mem, kflag);
        cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;
        N_VScale_Serial(ONE, cv_mem.cv_zn[0], yout);
        break;
      }
      
      nstloc++;

      /* If tstop is set and was reached, reset tn = tstop */
      if ( cv_mem.cv_tstopset ) {
        troundoff = FUZZ_FACTOR * cv_mem.cv_uround *
          (Math.abs(cv_mem.cv_tn) + Math.abs(cv_mem.cv_h));
        if ( Math.abs(cv_mem.cv_tn - cv_mem.cv_tstop) <= troundoff)
          cv_mem.cv_tn = cv_mem.cv_tstop;
      }

      /* Check for root in last step taken. */    
      if (cv_mem.cv_nrtfn > 0) {
        
        retval = cvRcheck3(cv_mem);
        
        if (retval == RTFOUND) {  /* A new root was found */
          cv_mem.cv_irfnd = 1;
          istate = CV_ROOT_RETURN;
          cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tlo;
          break;
        } else if (retval == CV_RTFUNC_FAIL) { /* g failed */
          cvProcessError(cv_mem, CV_RTFUNC_FAIL, "CVODES", "cvRcheck3",
                         MSGCV_RTFUNC_FAILED, cv_mem.cv_tlo);
          istate = CV_RTFUNC_FAIL;
          break;
        }

        /* If we are at the end of the first step and we still have
         * some event functions that are inactive, issue a warning
         * as this may indicate a user error in the implementation
         * of the root function. */

        if (cv_mem.cv_nst==1) {
          inactive_roots = false;
          for (ir=0; ir<cv_mem.cv_nrtfn; ir++) { 
            if (!cv_mem.cv_gactive[ir]) {
              inactive_roots = true;
              break;
            }
          }
          if ((cv_mem.cv_mxgnull > 0) && inactive_roots) {
            cvProcessError(cv_mem, CV_WARNING, "CVODES", "CVode",
                           MSGCV_INACTIVE_ROOTS);
          }
        }

      }

      /* In NORMAL mode, check if tout reached */
      if ( (itask == CV_NORMAL) &&  (cv_mem.cv_tn-tout)*cv_mem.cv_h >= ZERO ) {
        istate = CV_SUCCESS;
        cv_mem.cv_tretlast = tret[0] = tout;
        CVodeGetDky(cv_mem, tout, 0, yout);
        cv_mem.cv_next_q = cv_mem.cv_qprime;
        cv_mem.cv_next_h = cv_mem.cv_hprime;
        break;
      }

      /* Check if tn is at tstop, or about to pass tstop */
      if ( cv_mem.cv_tstopset ) {

        troundoff = FUZZ_FACTOR * cv_mem.cv_uround *
          (Math.abs(cv_mem.cv_tn) + Math.abs(cv_mem.cv_h));
        if ( Math.abs(cv_mem.cv_tn - cv_mem.cv_tstop) <= troundoff) {
          CVodeGetDky(cv_mem, cv_mem.cv_tstop, 0, yout);
          cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tstop;
          cv_mem.cv_tstopset = false;
          istate = CV_TSTOP_RETURN;
          break;
        }

        if ( (cv_mem.cv_tn + cv_mem.cv_hprime - cv_mem.cv_tstop)*cv_mem.cv_h > ZERO ) {
          cv_mem.cv_hprime = (cv_mem.cv_tstop - cv_mem.cv_tn)*(ONE-FOUR*cv_mem.cv_uround);
          cv_mem.cv_eta = cv_mem.cv_hprime / cv_mem.cv_h;
        }

      }

      /* In ONE_STEP mode, copy y and exit loop */
      if (itask == CV_ONE_STEP) {
        istate = CV_SUCCESS;
        cv_mem.cv_tretlast = tret[0] = cv_mem.cv_tn;
        N_VScale_Serial(ONE, cv_mem.cv_zn[0], yout);
        cv_mem.cv_next_q = cv_mem.cv_qprime;
        cv_mem.cv_next_h = cv_mem.cv_hprime;
        break;
      }

    } /* end looping for internal steps */
    
    /* Load optional output */
    if (cv_mem.cv_sensi && (cv_mem.cv_ism==CV_STAGGERED1)) { 
      cv_mem.cv_nniS  = 0;
      cv_mem.cv_ncfnS = 0;
      for (is=0; is<cv_mem.cv_Ns; is++) {
        cv_mem.cv_nniS  += cv_mem.cv_nniS1[is];
        cv_mem.cv_ncfnS += cv_mem.cv_ncfnS1[is];
      }
    }
    
    return(istate);

  }
  
  /*  
   * cvInitialSetup
   *
   * This routine performs input consistency checks at the first step.
   * If needed, it also checks the linear solver module and calls the
   * linear solver initialization routine.
   */

  private int cvInitialSetup(CVodeMemRec cv_mem)
  {
    int ier;

    /* Did the user specify tolerances? */
    if (cv_mem.cv_itol == CV_NN) {
      cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                     MSGCV_NO_TOL);
      return(CV_ILL_INPUT);
    }

    /* Set data for efun */
    if (cv_mem.cv_user_efun) cv_mem.cv_e_data = cv_mem.cv_user_data;
    else                      cv_mem.cv_e_data = cv_mem;

    /* Load initial error weights */
    ier = cv_efun(cv_mem.cv_zn[0], cv_mem.cv_ewt,
                          cv_mem.cv_e_data, cv_mem.cv_efun_select);
    if (ier != 0) {
      if (cv_mem.cv_itol == CV_WF) 
        cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                       MSGCV_EWT_FAIL);
      else
        cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                       MSGCV_BAD_EWT);
      return(CV_ILL_INPUT);
    }
    
    /* Quadrature initial setup */

    if (cv_mem.cv_quadr && cv_mem.cv_errconQ) {

      /* Did the user specify tolerances? */
      if (cv_mem.cv_itolQ == CV_NN) {
        cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                       MSGCV_NO_TOLQ);
        return(CV_ILL_INPUT);
      }

      /* Load ewtQ */
      ier = cvQuadEwtSet(cv_mem, cv_mem.cv_znQ[0], cv_mem.cv_ewtQ);
      if (ier != 0) {
        cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                       MSGCV_BAD_EWTQ);
        return(CV_ILL_INPUT);
      }

    }

    if (!cv_mem.cv_quadr) cv_mem.cv_errconQ = false;

    /* Forward sensitivity initial setup */

    if (cv_mem.cv_sensi) {

      /* Did the user specify tolerances? */
      if (cv_mem.cv_itolS == CV_NN) {
        cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                       MSGCV_NO_TOLS);
        return(CV_ILL_INPUT);
      }

      /* If using the internal DQ functions, we must have access to the problem parameters */
      if(cv_mem.cv_fSDQ && (cv_mem.cv_p == null)) {
        cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                       MSGCV_NULL_P);
        return(CV_ILL_INPUT);
      }

      /* Load ewtS */
      ier = cvSensEwtSet(cv_mem, cv_mem.cv_znS[0], cv_mem.cv_ewtS);
      if (ier != 0) {
        cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                       MSGCV_BAD_EWTS);
        return(CV_ILL_INPUT);
      }

    }

    /* FSA of quadrature variables */

    if (cv_mem.cv_quadr_sensi) {

      /* If using the internal DQ functions, we must have access to fQ
       * (i.e. quadrature integration must be enabled) and to the problem parameters */

      if (cv_mem.cv_fQSDQ) {

        /* Test if quadratures are defined, so we can use fQ */
        if (!cv_mem.cv_quadr) {
          cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                         MSGCV_NULL_FQ);
          return(CV_ILL_INPUT);
        }

        /* Test if we have the problem parameters */
        if(cv_mem.cv_p == null) {
          cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                         MSGCV_NULL_P);
          return(CV_ILL_INPUT);
        }

      }

      if (cv_mem.cv_errconQS) {
        
        /* Did the user specify tolerances? */
        if (cv_mem.cv_itolQS == CV_NN) {
          cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                         MSGCV_NO_TOLQS);
          return(CV_ILL_INPUT);
        }

        /* If needed, did the user provide quadrature tolerances? */
        if ( (cv_mem.cv_itolQS == CV_EE) && (cv_mem.cv_itolQ == CV_NN) ) {
          cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                         MSGCV_NO_TOLQ);
          return(CV_ILL_INPUT);
        }

        /* Load ewtQS */
        ier = cvQuadSensEwtSet(cv_mem, cv_mem.cv_znQS[0], cv_mem.cv_ewtQS);
        if (ier != 0) {
          cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                         MSGCV_BAD_EWTQS);
          return(CV_ILL_INPUT);
        }

      }

    } else {

      cv_mem.cv_errconQS = false;

    }

    /* Check if lsolve function exists (if needed) and call linit function (if it exists) */
    if (cv_mem.cv_iter == CV_NEWTON) {
      if (cv_mem.cv_lsolve_select < 0) {
        cvProcessError(cv_mem, CV_ILL_INPUT, "CVODES", "cvInitialSetup",
                       MSGCV_LSOLVE_NULL);
        return(CV_ILL_INPUT);
      }
      if (cv_mem.cv_linit_select > 0) {
        ier = cv_linit(cv_mem, cv_mem.cv_linit_select);
        if (ier != 0) {
          cvProcessError(cv_mem, CV_LINIT_FAIL, "CVODES", "cvInitialSetup",
                         MSGCV_LINIT_FAIL);
          return(CV_LINIT_FAIL);
        }
      }
    }
      
    return(CV_SUCCESS);
  }

  /*
   * cvEwtSet
   *
   * This routine is responsible for setting the error weight vector ewt,
   * according to tol_type, as follows:
   *
   * (1) ewt[i] = 1 / (reltol * SUNRabs(ycur[i]) + *abstol), i=0,...,neq-1
   *     if tol_type = CV_SS
   * (2) ewt[i] = 1 / (reltol * SUNRabs(ycur[i]) + abstol[i]), i=0,...,neq-1
   *     if tol_type = CV_SV
   *
   * cvEwtSet returns 0 if ewt is successfully set as above to a
   * positive vector and -1 otherwise. In the latter case, ewt is
   * considered undefined.
   *
   * All the real work is done in the routines cvEwtSetSS, cvEwtSetSV.
   */

  int cvEwtSet(NVector ycur, NVector weight, CVodeMemRec cv_mem)
  {
    int flag = 0;

    switch(cv_mem.cv_itol) {
    case CV_SS:
      flag = cvEwtSetSS(cv_mem, ycur, weight);
      break;
    case CV_SV:
      flag = cvEwtSetSV(cv_mem, ycur, weight);
      break;
    }

    return(flag);
  }
  
  /*
   * cvEwtSetSS
   *
   * This routine sets ewt as decribed above in the case tol_type = CV_SS.
   * It tests for non-positive components before inverting. cvEwtSetSS
   * returns 0 if ewt is successfully set to a positive vector
   * and -1 otherwise. In the latter case, ewt is considered undefined.
   */

  private int cvEwtSetSS(CVodeMemRec cv_mem, NVector ycur, NVector weight)
  {
    N_VAbs_Serial(ycur, cv_mem.cv_tempv);
    N_VScale_Serial(cv_mem.cv_reltol, cv_mem.cv_tempv, cv_mem.cv_tempv);
    N_VAddConst_Serial(cv_mem.cv_tempv, cv_mem.cv_Sabstol, cv_mem.cv_tempv);
    if (N_VMin(cv_mem.cv_tempv) <= ZERO) return(-1);
    N_VInv_Serial(cv_mem.cv_tempv, weight);

    return(0);
  }

  /*
   * cvEwtSetSV
   *
   * This routine sets ewt as decribed above in the case tol_type = CV_SV.
   * It tests for non-positive components before inverting. cvEwtSetSV
   * returns 0 if ewt is successfully set to a positive vector
   * and -1 otherwise. In the latter case, ewt is considered undefined.
   */

  private int cvEwtSetSV(CVodeMemRec cv_mem, NVector ycur, NVector weight)
  {
    N_VAbs_Serial(ycur, cv_mem.cv_tempv);
    N_VLinearSum_Serial(cv_mem.cv_reltol, cv_mem.cv_tempv, ONE,
                 cv_mem.cv_Vabstol, cv_mem.cv_tempv);
    if (N_VMin(cv_mem.cv_tempv) <= ZERO) return(-1);
    N_VInv_Serial(cv_mem.cv_tempv, weight);
    return(0);
  }

  private void N_VAbs_Serial(NVector x, NVector z)
  {
    int i, N;
    double xd[], zd[];

    xd = zd = null;
    xd = x.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = Math.abs(xd[i]);

    return;
  }
  
  private void N_VAddConst_Serial(NVector x, double b, NVector z)
  {
    int i, N;
    double xd[], zd[];

    xd = zd = null;
    xd = x.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++) 
      zd[i] = xd[i]+b;

    return;
  }

  private void N_VInv_Serial(NVector x, NVector z)
  {
    int i, N;
    double xd[], zd[];

    xd = zd = null;
    
    xd = x.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = ONE/xd[i];

    return;
  }
  
  private void N_VLinearSum_Serial(double a, NVector x, double b, NVector y, NVector z)
  {
    int i, N;
    double c, xd[], yd[], zd[];
    NVector v1, v2;
    boolean test;

    xd = yd = zd = null;

    if ((b == ONE) && (z == y)) {    /* BLAS usage: axpy y <- ax+y */
      Vaxpy_Serial(a,x,y);
      return;
    }

    if ((a == ONE) && (z == x)) {    /* BLAS usage: axpy x <- by+x */
      Vaxpy_Serial(b,y,x);
      return;
    }

    /* Case: a == b == 1.0 */

    if ((a == ONE) && (b == ONE)) {
      VSum_Serial(x, y, z);
      return;
    }

    /* Cases: (1) a == 1.0, b = -1.0, (2) a == -1.0, b == 1.0 */

    if ((test = ((a == ONE) && (b == -ONE))) || ((a == -ONE) && (b == ONE))) {
      v1 = test ? y : x;
      v2 = test ? x : y;
      VDiff_Serial(v2, v1, z);
      return;
    }

    /* Cases: (1) a == 1.0, b == other or 0.0, (2) a == other or 0.0, b == 1.0 */
    /* if a or b is 0.0, then user should have called N_VScale */

    if ((test = (a == ONE)) || (b == ONE)) {
      c  = test ? b : a;
      v1 = test ? y : x;
      v2 = test ? x : y;
      VLin1_Serial(c, v1, v2, z);
      return;
    }

    /* Cases: (1) a == -1.0, b != 1.0, (2) a != 1.0, b == -1.0 */

    if ((test = (a == -ONE)) || (b == -ONE)) {
      c = test ? b : a;
      v1 = test ? y : x;
      v2 = test ? x : y;
      VLin2_Serial(c, v1, v2, z);
      return;
    }

    /* Case: a == b */
    /* catches case both a and b are 0.0 - user should have called N_VConst */

    if (a == b) {
      VScaleSum_Serial(a, x, y, z);
      return;
    }

    /* Case: a == -b */

    if (a == -b) {
      VScaleDiff_Serial(a, x, y, z);
      return;
    }

    /* Do all cases not handled above:
       (1) a == other, b == 0.0 - user should have called N_VScale
       (2) a == 0.0, b == other - user should have called N_VScale
       (3) a,b == other, a !=b, a != -b */
    
    xd = x.data;
    yd = y.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = (a*xd[i])+(b*yd[i]);

    return;
  }
  
  private void Vaxpy_Serial(double a, NVector x, NVector y)
  {
    int i, N;
    double xd[], yd[];

    xd = yd = null;

    xd = x.data;
    yd = y.data;
    N = xd.length;

    if (a == ONE) {
      for (i = 0; i < N; i++)
        yd[i] += xd[i];
      return;
    }

    if (a == -ONE) {
      for (i = 0; i < N; i++)
        yd[i] -= xd[i];
      return;
    }    

    for (i = 0; i < N; i++)
      yd[i] += a*xd[i];

    return;
  }


  private void N_VScale_Serial(double c, NVector x, NVector z)
  {
    int i, N;
    double xd[], zd[];

    xd = zd = null;

    if (z == x) {  /* BLAS usage: scale x <- cx */
      VScaleBy_Serial(c, x);
      return;
    }

    if (c == ONE) {
      VCopy_Serial(x, z);
    } else if (c == -ONE) {
      VNeg_Serial(x, z);
    } else {
    	xd = x.data;
        zd = z.data;
        N = xd.length;
      for (i = 0; i < N; i++) 
        zd[i] = c*xd[i];
    }

    return;
  }
  
  private void VScaleBy_Serial(double a, NVector x)
  {
    int i, N;
    double xd[];

    xd = null;

    xd = x.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      xd[i] *= a;

    return;
  }
  
  private void VCopy_Serial(NVector x, NVector z)
  {
    int i, N;
    double xd[], zd[];

    xd = zd = null;

    xd = x.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = xd[i]; 

    return;
  }

  private void VNeg_Serial(NVector x, NVector z)
  {
    int i, N;
    double xd[], zd[];

    xd = zd = null;

    xd = x.data;
    zd = z.data;
    N = xd.length;
    
    for (i = 0; i < N; i++)
      zd[i] = -xd[i];

    return;
  }
  
  private void VSum_Serial(NVector x, NVector y, NVector z)
  {
    int i, N;
    double xd[], yd[], zd[];

    xd = yd = zd = null;

    xd = x.data;
    yd = y.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = xd[i]+yd[i];

    return;
  }

  private void VDiff_Serial(NVector x, NVector y, NVector z)
  {
    int i, N;
    double xd[], yd[], zd[];

    xd = yd = zd = null;

    xd = x.data;
    yd = y.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = xd[i]-yd[i];

    return;
  }

  private void VLin1_Serial(double a, NVector x, NVector y, NVector z)
  {
    int i, N;
    double xd[], yd[], zd[];

    xd = yd = zd = null;

    xd = x.data;
    yd = y.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = (a*xd[i])+yd[i];

    return;
  }

  private void VLin2_Serial(double a, NVector x, NVector y, NVector z)
  {
    int i, N;
    double xd[], yd[], zd[];

    xd = yd = zd = null;

    xd = x.data;
    yd = y.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = (a*xd[i])-yd[i];

    return;
  }
  
  private void VScaleSum_Serial(double c, NVector x, NVector y, NVector z)
  {
    int i, N;
    double xd[], yd[], zd[];

    xd = yd = zd = null;

    xd = x.data;
    yd = y.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = c*(xd[i]+yd[i]);

    return;
  }

  private void VScaleDiff_Serial(double c, NVector x, NVector y, NVector z)
  {
    int i, N;
    double xd[], yd[], zd[];

    xd = yd = zd = null;

    xd = x.data;
    yd = y.data;
    zd = z.data;
    N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = c*(xd[i]-yd[i]);

    return;
  }
  
  private void N_VDiv_Serial(NVector x, NVector y, NVector z)
  {
	  int i, N;
	  double xd[], yd[], zd[];

	  xd = yd = zd = null;

	  xd = x.data;
	  yd = y.data;
	  zd = z.data;
	  N = xd.length;

    for (i = 0; i < N; i++)
      zd[i] = xd[i]/yd[i];

    return;
  }
  
  private double N_VMaxNorm_Serial(NVector x)
  {
    int i, N;
    double max, xd[];

    max = ZERO;
    xd = null;

    xd = x.data;
    N = xd.length;

    for (i = 0; i < N; i++) {
      if (Math.abs(xd[i]) > max) max = Math.abs(xd[i]);
    }

    return(max);
  }

  
  private double N_VWrmsNorm_Serial(NVector x, NVector w)
  {
    int i, N;
    double sum, prodi, xd[],wd[];

    sum = ZERO;
    xd = wd = null;

    xd = x.data;
    wd = w.data;
    N = xd.length;

    for (i = 0; i < N; i++) {
      prodi = xd[i]*wd[i];
      sum += (prodi*prodi);
    }

    return(Math.sqrt(sum/N));
  }
  
  private void N_VConst_Serial(double c, NVector z)
  {
    int i, N;
    double zd[];

    zd = null;

    zd = z.data;
    N = zd.length;

    for (i = 0; i < N; i++) zd[i] = c;

    return;
  }




  private int cv_efun(NVector ycur, NVector weight, CVodeMemRec cv_mem, int cv_efun_select) {
	  int flag = 0;
	  switch (cv_efun_select) {
	  case cvEwtSet_select:
		  flag = cvEwtSet(ycur, weight, cv_mem);  
		  break;
	  }
	  return flag;
  }
  
  /*
   * cvQuadEwtSet
   *
   */

  private int cvQuadEwtSet(CVodeMemRec cv_mem, NVector qcur, NVector weightQ)
  {
    int flag=0;

    switch (cv_mem.cv_itolQ) {
    case CV_SS:
      flag = cvQuadEwtSetSS(cv_mem, qcur, weightQ);
      break;
    case CV_SV:
      flag = cvQuadEwtSetSV(cv_mem, qcur, weightQ);
      break;
    }

    return(flag);

  }

  /*
   * cvQuadEwtSetSS
   *
   */

  private int cvQuadEwtSetSS(CVodeMemRec cv_mem, NVector qcur, NVector weightQ)
  {
    N_VAbs_Serial(qcur, cv_mem.cv_tempvQ);
    N_VScale_Serial(cv_mem.cv_reltolQ, cv_mem.cv_tempvQ, cv_mem.cv_tempvQ);
    N_VAddConst_Serial(cv_mem.cv_tempvQ, cv_mem.cv_SabstolQ, cv_mem.cv_tempvQ);
    if (N_VMin(cv_mem.cv_tempvQ) <= ZERO) return(-1);
    N_VInv_Serial(cv_mem.cv_tempvQ, weightQ);

    return(0);
  }

  /*
   * cvQuadEwtSetSV
   *
   */

  private int cvQuadEwtSetSV(CVodeMemRec cv_mem, NVector qcur, NVector weightQ)
  {
    N_VAbs_Serial(qcur, cv_mem.cv_tempvQ);
    N_VLinearSum_Serial(cv_mem.cv_reltolQ, cv_mem.cv_tempvQ, ONE,
                 cv_mem.cv_VabstolQ, cv_mem.cv_tempvQ);
    if (N_VMin(cv_mem.cv_tempvQ) <= ZERO) return(-1);
    N_VInv_Serial(cv_mem.cv_tempvQ, weightQ);

    return(0);
  }
  
  /*
   * cvSensEwtSet
   *
   */

  private int cvSensEwtSet(CVodeMemRec cv_mem, NVector yScur[], NVector weightS[])
  {
    int flag=0;

    switch (cv_mem.cv_itolS) {
    case CV_EE:
      flag = cvSensEwtSetEE(cv_mem, yScur, weightS);
      break;
    case CV_SS:
      flag = cvSensEwtSetSS(cv_mem, yScur, weightS);
      break;
    case CV_SV:
      flag = cvSensEwtSetSV(cv_mem, yScur, weightS);
      break;
    }

    return(flag);
  }

  /*
   * cvSensEwtSetEE
   *
   * In this case, the error weight vector for the i-th sensitivity is set to
   *
   * ewtS_i = pbar_i * efun(pbar_i*yS_i)
   *
   * In other words, the scaled sensitivity pbar_i * yS_i has the same error
   * weight vector calculation as the solution vector.
   *
   */

  private int cvSensEwtSetEE(CVodeMemRec cv_mem, NVector yScur[], NVector weightS[])
  {
    int is;
    NVector pyS;
    int flag;

    /* Use tempvS[0] as temporary storage for the scaled sensitivity */
    pyS = cv_mem.cv_tempvS[0];

    for (is=0; is<cv_mem.cv_Ns; is++) {
      N_VScale_Serial(cv_mem.cv_pbar[is], yScur[is], pyS);
      flag = cv_efun(pyS, weightS[is], cv_mem.cv_e_data, cv_mem.cv_efun_select);
      if (flag != 0) return(-1);
      N_VScale_Serial(cv_mem.cv_pbar[is], weightS[is], weightS[is]);
    }

    return(0);
  }

  /*
   * cvSensEwtSetSS
   *
   */

  private int cvSensEwtSetSS(CVodeMemRec cv_mem, NVector yScur[], NVector weightS[])
  {
    int is;
    
    for (is=0; is<cv_mem.cv_Ns; is++) {
      N_VAbs_Serial(yScur[is], cv_mem.cv_tempv);
      N_VScale_Serial(cv_mem.cv_reltolS, cv_mem.cv_tempv, cv_mem.cv_tempv);
      N_VAddConst_Serial(cv_mem.cv_tempv, cv_mem.cv_SabstolS[is], cv_mem.cv_tempv);
      if (N_VMin(cv_mem.cv_tempv) <= ZERO) return(-1);
      N_VInv_Serial(cv_mem.cv_tempv, weightS[is]);
    }
    return(0);
  }
  
  /*
   * cvSensEwtSetSV
   *
   */

  private int cvSensEwtSetSV(CVodeMemRec cv_mem, NVector yScur[], NVector weightS[])
  {
    int is;
    
    for (is=0; is<cv_mem.cv_Ns; is++) {
      N_VAbs_Serial(yScur[is], cv_mem.cv_tempv);
      N_VLinearSum_Serial(cv_mem.cv_reltolS, cv_mem.cv_tempv, ONE,
                   cv_mem.cv_VabstolS[is], cv_mem.cv_tempv);
      if (N_VMin(cv_mem.cv_tempv) <= ZERO) return(-1);
      N_VInv_Serial(cv_mem.cv_tempv, weightS[is]);
    }

    return(0);
  }

  /*
   * cvQuadSensEwtSet
   *
   */

  private int cvQuadSensEwtSet(CVodeMemRec cv_mem, NVector yQScur[], NVector weightQS[])
  {
    int flag=0;

    switch (cv_mem.cv_itolQS) {
    case CV_EE:
      flag = cvQuadSensEwtSetEE(cv_mem, yQScur, weightQS);
      break;
    case CV_SS:
      flag = cvQuadSensEwtSetSS(cv_mem, yQScur, weightQS);
      break;
    case CV_SV:
      flag = cvQuadSensEwtSetSV(cv_mem, yQScur, weightQS);
      break;
    }

    return(flag);
  }

  /*
   * cvQuadSensEwtSetEE
   *
   * In this case, the error weight vector for the i-th quadrature sensitivity
   * is set to
   *
   * ewtQS_i = pbar_i * cvQuadEwtSet(pbar_i*yQS_i)
   *
   * In other words, the scaled sensitivity pbar_i * yQS_i has the same error
   * weight vector calculation as the quadrature vector.
   *
   */
  private int cvQuadSensEwtSetEE(CVodeMemRec cv_mem, NVector yQScur[], NVector weightQS[])
  {
    int is;
    NVector pyS;
    int flag;

    /* Use tempvQS[0] as temporary storage for the scaled sensitivity */
    pyS = cv_mem.cv_tempvQS[0];

    for (is=0; is<cv_mem.cv_Ns; is++) {
      N_VScale_Serial(cv_mem.cv_pbar[is], yQScur[is], pyS);
      flag = cvQuadEwtSet(cv_mem, pyS, weightQS[is]);
      if (flag != 0) return(-1);
      N_VScale_Serial(cv_mem.cv_pbar[is], weightQS[is], weightQS[is]);
    }

    return(0);
  }

  private int cvQuadSensEwtSetSS(CVodeMemRec cv_mem, NVector yQScur[], NVector weightQS[])
  {
    int is;

    for (is=0; is<cv_mem.cv_Ns; is++) {
      N_VAbs_Serial(yQScur[is], cv_mem.cv_tempvQ);
      N_VScale_Serial(cv_mem.cv_reltolQS, cv_mem.cv_tempvQ, cv_mem.cv_tempvQ);
      N_VAddConst_Serial(cv_mem.cv_tempvQ, cv_mem.cv_SabstolQS[is], cv_mem.cv_tempvQ);
      if (N_VMin(cv_mem.cv_tempvQ) <= ZERO) return(-1);
      N_VInv_Serial(cv_mem.cv_tempvQ, weightQS[is]);
    }

    return(0);
  }

  private int cvQuadSensEwtSetSV(CVodeMemRec cv_mem, NVector yQScur[], NVector weightQS[])
  {
    int is;
    
    for (is=0; is<cv_mem.cv_Ns; is++) {
      N_VAbs_Serial(yQScur[is], cv_mem.cv_tempvQ);
      N_VLinearSum_Serial(cv_mem.cv_reltolQS, cv_mem.cv_tempvQ, ONE,
                   cv_mem.cv_VabstolQS[is], cv_mem.cv_tempvQ);
      if (N_VMin(cv_mem.cv_tempvQ) <= ZERO) return(-1);
      N_VInv_Serial(cv_mem.cv_tempvQ, weightQS[is]);
    }

    return(0);
  }
  
  private int cv_linit(CVodeMemRec cv_mem, int cv_linit_select) {
	  int flag = 0;
	  switch (cv_linit_select) {
	  case cvDlsInitialize_select:
		  flag = cvDlsInitialize(cv_mem);
		  break;
	  }
	  return flag;
  }
  
  /*-----------------------------------------------------------------
  cvDlsInitialize
  -----------------------------------------------------------------
  This routine performs remaining initializations specific
  to the direct linear solver interface (and solver itself)
  -----------------------------------------------------------------*/
    private int cvDlsInitialize(CVodeMemRec cv_mem)
{
  CVDlsMemRec cvdls_mem;

  /* Return immediately if cv_mem or cv_mem->cv_lmem are NULL */
  if (cv_mem == null) {
    cvProcessError(null, CVDLS_MEM_NULL, "CVSDLS", 
                    "cvDlsInitialize", MSGD_CVMEM_NULL);
    return(CVDLS_MEM_NULL);
  }
  if (cv_mem.cv_lmem == null) {
    cvProcessError(cv_mem, CVDLS_LMEM_NULL, "CVSDLS", 
                    "cvDlsInitialize", MSGD_LMEM_NULL);
    return(CVDLS_LMEM_NULL);
  }
  cvdls_mem = cv_mem.cv_lmem;
 
  cvDlsInitializeCounters(cvdls_mem);

  /* Set Jacobian function and data, depending on jacDQ (in case 
     it has changed based on user input) */
  if (cvdls_mem.jacDQ) {
    cvdls_mem.jac    = cvDlsDQJac;
    cvdls_mem.J_data = cv_mem;
  } else {
    cvdls_mem.J_data = cv_mem.cv_user_data;
  }

  /* Call LS initialize routine */
  cvdls_mem.last_flag = SUNLinSolInitialize_Dense(cvdls_mem.LS);
  return(cvdls_mem.last_flag);
}
    
    private int SUNLinSolInitialize_Dense(SUNLinearSolver S)
    {
      /* all solver-specific memory has already been allocated */
      S.last_flag = SUNLS_SUCCESS;
      return S.last_flag;
    }


   private void cv_lfree(CVodeMemRec cv_mem, int cv_lfree_select) {
	   switch (cv_lfree_select) {
	   case cvDlsFree_select:
		   cvDlsFree(cv_mem);
	   break;
	   }
   }
   
   /*-----------------------------------------------------------------
   cvDlsFree
   -----------------------------------------------------------------
   This routine frees memory associates with the CVDls solver 
   interface.
   -----------------------------------------------------------------*/
 private int cvDlsFree(CVodeMemRec cv_mem)
 {
   CVDlsMemRec cvdls_mem;

   /* Return immediately if cv_mem or cv_mem->cv_lmem are NULL */
   if (cv_mem == null)  return (CVDLS_SUCCESS);
   if (cv_mem.cv_lmem == null)  return(CVDLS_SUCCESS);
   cvdls_mem = cv_mem.cv_lmem;

   /* Free x vector */
   if (cvdls_mem.x != null) {
     N_VDestroy(cvdls_mem.x);
   }

   /* Free savedJ memory */
   if (cvdls_mem.savedJ != null) {
     cvdls_mem.savedJ = null;
   }

   /* Nullify other SUNMatrix pointer */
   cvdls_mem.A = null;

   /* free CVDls interface structure */
   cv_mem.cv_lmem = null;
   
   return(CVDLS_SUCCESS);
 }

 /*
  * cvSensRhsWrapper
  *
  * CVSensRhs is a high level routine that returns right hand side 
  * of sensitivity equations. Depending on the 'ifS' flag, it either 
  * calls directly the fS routine (ifS=CV_ALLSENS) or (if ifS=CV_ONESENS) 
  * calls the fS1 routine in a loop over all sensitivities.
  *
  * CVSensRhs is called:
  *  (*) by CVode at the first step
  *  (*) by cvYddNorm if errcon=SUNTRUE
  *  (*) by cvNlsFunctional, cvNlsNewton, and cvNewtonIteration
  *      if ism=CV_SIMULTANEOUS
  *  (*) by cvDoErrorTest when restarting from scratch
  *  (*) in the corrector loop if ism=CV_STAGGERED
  *  (*) by cvStgrDoErrorTest when restarting from scratch 
  *
  * The return value is that of the sensitivity RHS function fS,
  *
  */

 int cvSensRhsWrapper(CVodeMemRec cv_mem, double time, 
                      NVector ycur, NVector fcur, 
                      NVector yScur[], NVector fScur[],
                      NVector temp1, NVector temp2)
 {
   int retval=0, is;

   if (cv_mem.cv_ifS==CV_ALLSENS) {
     retval = fS(cv_mem.cv_Ns, time, ycur, fcur, yScur, 
                            fScur, cv_mem.cv_fS_data, temp1, temp2);
     cv_mem.cv_nfSe++;
   } else {
     for (is=0; is<cv_mem.cv_Ns; is++) {
       retval = fS1(cv_mem.cv_Ns, time, ycur, fcur, is, yScur[is], 
                               fScur[is], cv_mem.cv_fS_data, temp1, temp2);
       cv_mem.cv_nfSe++;
       if (retval != 0) break;
     }
   }

   return(retval);
 }

 
 /*
  * cvHin
  *
  * This routine computes a tentative initial step size h0. 
  * If tout is too close to tn (= t0), then cvHin returns CV_TOO_CLOSE
  * and h remains uninitialized. Note that here tout is either the value
  * passed to CVode at the first call or the value of tstop (if tstop is 
  * enabled and it is closer to t0=tn than tout).
  * If any RHS function fails unrecoverably, cvHin returns CV_*RHSFUNC_FAIL.
  * If any RHS function fails recoverably too many times and recovery is
  * not possible, cvHin returns CV_REPTD_*RHSFUNC_ERR.
  * Otherwise, cvHin sets h to the chosen value h0 and returns CV_SUCCESS.
  *
  * The algorithm used seeks to find h0 as a solution of
  *       (WRMS norm of (h0^2 ydd / 2)) = 1, 
  * where ydd = estimated second derivative of y. Here, y includes
  * all variables considered in the error test.
  *
  * We start with an initial estimate equal to the geometric mean of the
  * lower and upper bounds on the step size.
  *
  * Loop up to MAX_ITERS times to find h0.
  * Stop if new and previous values differ by a factor < 2.
  * Stop if hnew/hg > 2 after one iteration, as this probably means
  * that the ydd value is bad because of cancellation error.        
  *  
  * For each new proposed hg, we allow MAX_ITERS attempts to
  * resolve a possible recoverable failure from f() by reducing
  * the proposed stepsize by a factor of 0.2. If a legal stepsize
  * still cannot be found, fall back on a previous value if possible,
  * or else return CV_REPTD_RHSFUNC_ERR.
  *
  * Finally, we apply a bias (0.5) and verify that h0 is within bounds.
  */

 private int cvHin(CVodeMemRec cv_mem, double tout)
 {
   int retval = 0;
   int sign, count1, count2;
   double tdiff, tdist, tround, hlb, hub;
   double hnew = 0.0;
   double hg, hgs, hs, hrat, h0;
   double yddnrm[] = new double[1];
   boolean hgOK, hnewOK;

   /* If tout is too close to tn, give up */
   
   if ((tdiff = tout-cv_mem.cv_tn) == ZERO) return(CV_TOO_CLOSE);
   
   sign = (tdiff > ZERO) ? 1 : -1;
   tdist = Math.abs(tdiff);
   tround = cv_mem.cv_uround * Math.max(Math.abs(cv_mem.cv_tn), Math.abs(tout));

   if (tdist < TWO*tround) return(CV_TOO_CLOSE);
   
   /* 
      Set lower and upper bounds on h0, and take geometric mean 
      as first trial value.
      Exit with this value if the bounds cross each other.
   */

   hlb = HLB_FACTOR * tround;
   hub = cvUpperBoundH0(cv_mem, tdist);

   hg  = Math.sqrt(hlb*hub);

   if (hub < hlb) {
     if (sign == -1) cv_mem.cv_h = -hg;
     else            cv_mem.cv_h =  hg;
     return(CV_SUCCESS);
   }
   
   /* Outer loop */

   hnewOK = false;
   hs = hg;         /* safeguard against 'uninitialized variable' warning */

   for(count1 = 1; count1 <= MAX_ITERS; count1++) {

     /* Attempts to estimate ydd */

     hgOK = false;

     for (count2 = 1; count2 <= MAX_ITERS; count2++) {
       hgs = hg*sign;
       retval = cvYddNorm(cv_mem, hgs, yddnrm);
       /* If a RHS function failed unrecoverably, give up */
       if (retval < 0) return(retval);
       /* If successful, we can use ydd */
       if (retval == CV_SUCCESS) {hgOK = true; break;}
       /* A RHS function failed recoverably; cut step size and test it again */
       hg *= POINT2;
     }

     /* If a RHS function failed recoverably MAX_ITERS times */

     if (!hgOK) {
       /* Exit if this is the first or second pass. No recovery possible */
       if (count1 <= 2) 
         if (retval == RHSFUNC_RECVR)  return(CV_REPTD_RHSFUNC_ERR);
         if (retval == QRHSFUNC_RECVR) return(CV_REPTD_QRHSFUNC_ERR);
         if (retval == SRHSFUNC_RECVR) return(CV_REPTD_SRHSFUNC_ERR);
       /* We have a fall-back option. The value hs is a previous hnew which
          passed through f(). Use it and break */
       hnew = hs;
       break;
     }

     /* The proposed step size is feasible. Save it. */
     hs = hg;

     /* If the stopping criteria was met, or if this is the last pass, stop */
     if ( (hnewOK) || (count1 == MAX_ITERS))  {hnew = hg; break;}

     /* Propose new step size */
     hnew = (yddnrm[0]*hub*hub > TWO) ? Math.sqrt(TWO/yddnrm[0]) : Math.sqrt(hg*hub);
     hrat = hnew/hg;
     
     /* Accept hnew if it does not differ from hg by more than a factor of 2 */
     if ((hrat > HALF) && (hrat < TWO)) {
       hnewOK = true;
     }

     /* After one pass, if ydd seems to be bad, use fall-back value. */
     if ((count1 > 1) && (hrat > TWO)) {
       hnew = hg;
       hnewOK = true;
     }

     /* Send this value back through f() */
     hg = hnew;

   }

   /* Apply bounds, bias factor, and attach sign */

   h0 = H_BIAS*hnew;
   if (h0 < hlb) h0 = hlb;
   if (h0 > hub) h0 = hub;
   if (sign == -1) h0 = -h0;
   cv_mem.cv_h = h0;

   return(CV_SUCCESS);
 }
 
 /*
  * cvUpperBoundH0
  *
  * This routine sets an upper bound on abs(h0) based on
  * tdist = tn - t0 and the values of y[i]/y'[i].
  */

 private double cvUpperBoundH0(CVodeMemRec cv_mem, double tdist)
 {
   double hub_inv, hubQ_inv, hubS_inv, hubQS_inv, hub;
   NVector temp1, temp2;
   NVector tempQ1, tempQ2;
   NVector tempS1[];
   NVector tempQS1[];
   int is;

   /* 
    * Bound based on |y|/|y'| -- allow at most an increase of
    * HUB_FACTOR in y0 (based on a forward Euler step). The weight 
    * factor is used as a safeguard against zero components in y0. 
    */

   temp1 = cv_mem.cv_tempv;
   temp2 = cv_mem.cv_acor;

   N_VAbs_Serial(cv_mem.cv_zn[0], temp2);
   cv_efun(cv_mem.cv_zn[0], temp1, cv_mem.cv_e_data, cv_mem.cv_efun_select);
   N_VInv_Serial(temp1, temp1);
   N_VLinearSum_Serial(HUB_FACTOR, temp2, ONE, temp1, temp1);

   N_VAbs_Serial(cv_mem.cv_zn[1], temp2);

   N_VDiv_Serial(temp2, temp1, temp1);
   hub_inv = N_VMaxNorm_Serial(temp1);

   /* Bound based on |yQ|/|yQ'| */
   
   if (cv_mem.cv_quadr && cv_mem.cv_errconQ) {

     tempQ1 = cv_mem.cv_tempvQ;
     tempQ2 = cv_mem.cv_acorQ;

     N_VAbs_Serial(cv_mem.cv_znQ[0], tempQ2);
     cvQuadEwtSet(cv_mem, cv_mem.cv_znQ[0], tempQ1);
     N_VInv_Serial(tempQ1, tempQ1);
     N_VLinearSum_Serial(HUB_FACTOR, tempQ2, ONE, tempQ1, tempQ1);
     
     N_VAbs_Serial(cv_mem.cv_znQ[1], tempQ2);
     
     N_VDiv_Serial(tempQ2, tempQ1, tempQ1);
     hubQ_inv = N_VMaxNorm_Serial(tempQ1);

     if (hubQ_inv > hub_inv) hub_inv = hubQ_inv;

   }

   /* Bound based on |yS|/|yS'| */

   if (cv_mem.cv_sensi && cv_mem.cv_errconS) {

     tempS1 = cv_mem.cv_acorS;
     cvSensEwtSet(cv_mem, cv_mem.cv_znS[0], tempS1);

     for (is=0; is<cv_mem.cv_Ns; is++) {

       N_VAbs_Serial(cv_mem.cv_znS[0][is], temp2);
       N_VInv_Serial(tempS1[is], temp1);
       N_VLinearSum_Serial(HUB_FACTOR, temp2, ONE, temp1, temp1);
       
       N_VAbs_Serial(cv_mem.cv_znS[1][is], temp2);
       
       N_VDiv_Serial(temp2, temp1, temp1);
       hubS_inv = N_VMaxNorm_Serial(temp1);

       if (hubS_inv > hub_inv) hub_inv = hubS_inv;

     }

   }

   /* Bound based on |yQS|/|yQS'| */

   if (cv_mem.cv_quadr_sensi && cv_mem.cv_errconQS) {

     tempQ1 = cv_mem.cv_tempvQ;
     tempQ2 = cv_mem.cv_acorQ;

     tempQS1 = cv_mem.cv_acorQS;
     cvQuadSensEwtSet(cv_mem, cv_mem.cv_znQS[0], tempQS1);

     for (is=0; is<cv_mem.cv_Ns; is++) {

       N_VAbs_Serial(cv_mem.cv_znQS[0][is], tempQ2);
       N_VInv_Serial(tempQS1[is], tempQ1);
       N_VLinearSum_Serial(HUB_FACTOR, tempQ2, ONE, tempQ1, tempQ1);
       
       N_VAbs_Serial(cv_mem.cv_znQS[1][is], tempQ2);
       
       N_VDiv_Serial(tempQ2, tempQ1, tempQ1);
       hubQS_inv = N_VMaxNorm_Serial(tempQ1);

       if (hubQS_inv > hub_inv) hub_inv = hubQS_inv;

     }

   }


   /*
    * bound based on tdist -- allow at most a step of magnitude
    * HUB_FACTOR * tdist
    */
   
   hub = HUB_FACTOR*tdist;

   /* Use the smaler of the two */

   if (hub*hub_inv > ONE) hub = ONE/hub_inv;

   return(hub);
 }
 
 /*
  * cvYddNorm
  *
  * This routine computes an estimate of the second derivative of Y
  * using a difference quotient, and returns its WRMS norm.
  *
  * Y contains all variables included in the error test. 
  */

 private int cvYddNorm(CVodeMemRec cv_mem, double hg, double yddnrm[])
 {
   int retval, is;
   NVector wrk1, wrk2;
   
   /* y <- h*y'(t) + y(t) */
   
   N_VLinearSum_Serial(hg, cv_mem.cv_zn[1], ONE, cv_mem.cv_zn[0], cv_mem.cv_y);
   
   if (cv_mem.cv_sensi && cv_mem.cv_errconS) 
     for (is=0; is<cv_mem.cv_Ns; is++)
       N_VLinearSum_Serial(hg, cv_mem.cv_znS[1][is], ONE,
                    cv_mem.cv_znS[0][is], cv_mem.cv_yS[is]);
   
   /* tempv <- f(t+h, h*y'(t)+y(t)) */

   retval = f(cv_mem.cv_tn+hg, cv_mem.cv_y,
                         cv_mem.cv_tempv, cv_mem.cv_user_data);
   cv_mem.cv_nfe++;
   if (retval < 0) return(CV_RHSFUNC_FAIL);
   if (retval > 0) return(RHSFUNC_RECVR);

   if (cv_mem.cv_quadr && cv_mem.cv_errconQ) {
     retval = fQ(cv_mem.cv_tn+hg, cv_mem.cv_y,
                            cv_mem.cv_tempvQ, cv_mem.cv_user_data);
     cv_mem.cv_nfQe++;
     if (retval < 0) return(CV_QRHSFUNC_FAIL);
     if (retval > 0) return(QRHSFUNC_RECVR);
   }

   if (cv_mem.cv_sensi && cv_mem.cv_errconS) {
     wrk1 = cv_mem.cv_ftemp;
     wrk2 = cv_mem.cv_acor;
     retval = cvSensRhsWrapper(cv_mem, cv_mem.cv_tn+hg, cv_mem.cv_y,
                               cv_mem.cv_tempv, cv_mem.cv_yS,
                               cv_mem.cv_tempvS, wrk1, wrk2);
     if (retval < 0) return(CV_SRHSFUNC_FAIL);
     if (retval > 0) return(SRHSFUNC_RECVR);
   }  

   if (cv_mem.cv_quadr_sensi && cv_mem.cv_errconQS) {
     wrk1 = cv_mem.cv_ftemp;
     wrk2 = cv_mem.cv_acorQ;
     retval = fQS(cv_mem.cv_Ns, cv_mem.cv_tn+hg,
                             cv_mem.cv_y, cv_mem.cv_yS,
                             cv_mem.cv_tempvQ, cv_mem.cv_tempvQS,
                             cv_mem.cv_fQS_data, wrk1, wrk2);

     cv_mem.cv_nfQSe++;
     if (retval < 0) return(CV_QSRHSFUNC_FAIL);
     if (retval > 0) return(QSRHSFUNC_RECVR);
   } 

   /* Load estimate of ||y''|| into tempv:
    * tempv <-  (1/h) * f(t+h, h*y'(t)+y(t)) - y'(t) */
   
   N_VLinearSum_Serial(ONE, cv_mem.cv_tempv, -ONE, cv_mem.cv_zn[1], cv_mem.cv_tempv);
   N_VScale_Serial(ONE/hg, cv_mem.cv_tempv, cv_mem.cv_tempv);
   yddnrm[0] = N_VWrmsNorm_Serial(cv_mem.cv_tempv, cv_mem.cv_ewt);

   if (cv_mem.cv_quadr && cv_mem.cv_errconQ) {
     N_VLinearSum_Serial(ONE, cv_mem.cv_tempvQ, -ONE, 
                  cv_mem.cv_znQ[1], cv_mem.cv_tempvQ);
     N_VScale_Serial(ONE/hg, cv_mem.cv_tempvQ, cv_mem.cv_tempvQ);
     yddnrm[0] = cvQuadUpdateNorm(cv_mem, yddnrm[0], cv_mem.cv_tempvQ,
                                cv_mem.cv_ewtQ);
   }

   if (cv_mem.cv_sensi && cv_mem.cv_errconS) {
     for (is=0; is<cv_mem.cv_Ns; is++) {
       N_VLinearSum_Serial(ONE, cv_mem.cv_tempvS[is], -ONE,
                    cv_mem.cv_znS[1][is], cv_mem.cv_tempvS[is]);
       N_VScale_Serial(ONE/hg, cv_mem.cv_tempvS[is], cv_mem.cv_tempvS[is]);
     }
     yddnrm[0] = cvSensUpdateNorm(cv_mem, yddnrm[0], cv_mem.cv_tempvS,
                                cv_mem.cv_ewtS);
   }

   if (cv_mem.cv_quadr_sensi && cv_mem.cv_errconQS) {
     for (is=0; is<cv_mem.cv_Ns; is++) {
       N_VLinearSum_Serial(ONE, cv_mem.cv_tempvQS[is], -ONE,
                    cv_mem.cv_znQS[1][is], cv_mem.cv_tempvQS[is]);
       N_VScale_Serial(ONE/hg, cv_mem.cv_tempvQS[is], cv_mem.cv_tempvQS[is]);
     }
     yddnrm[0] = cvQuadSensUpdateNorm(cv_mem, yddnrm[0], cv_mem.cv_tempvQS,
                                    cv_mem.cv_ewtQS);
   }

   return(CV_SUCCESS);
 }

 /*
  * cvQuadUpdateNorm
  *
  * Updates the norm old_nrm to account for all quadratures.
  */

 private double cvQuadUpdateNorm(CVodeMemRec cv_mem, double old_nrm,
                                  NVector xQ, NVector wQ)
 {
   double qnrm;

   qnrm = N_VWrmsNorm_Serial(xQ, wQ);
   if (old_nrm > qnrm) return(old_nrm);
   else                return(qnrm);
 }

 /*
  * cvSensUpdateNorm
  *
  * Updates the norm old_nrm to account for all sensitivities.
  */

 private double cvSensUpdateNorm(CVodeMemRec cv_mem, double old_nrm,
                                  NVector xS[], NVector wS[])
 {
   double snrm;
   
   snrm = cvSensNorm(cv_mem, xS, wS);
   if (old_nrm > snrm) return(old_nrm);
   else                return(snrm);
 }
 
 private double cvSensNorm(CVodeMemRec cv_mem, NVector xS[], NVector wS[])
 {
   int is;
   double nrm, snrm;

   nrm = N_VWrmsNorm_Serial(xS[0],wS[0]);
   for (is=1; is<cv_mem.cv_Ns; is++) {
     snrm = N_VWrmsNorm_Serial(xS[is],wS[is]);
     if ( snrm > nrm ) nrm = snrm;
   }

   return(nrm);
 }

 private double cvQuadSensUpdateNorm(CVodeMemRec cv_mem, double old_nrm,
         NVector xQS[], NVector wQS[])
{
double snrm;

snrm = cvQuadSensNorm(cv_mem, xQS, wQS);
if (old_nrm > snrm) return(old_nrm);
else                return(snrm);
}
 
 /*
  * cvQuadSensNorm
  *
  * This routine returns the maximum over the weighted root mean 
  * square norm of xQS with weight vectors wQS:
  *
  *  max { wrms(xQS[0],wS[0]) ... wrms(xQS[Ns-1],wS[Ns-1]) }    
  *
  * Called by cvQuadSensUpdateNorm.
  */

 private double cvQuadSensNorm(CVodeMemRec cv_mem, NVector xQS[], NVector wQS[])
 {
   int is;
   double nrm, snrm;

   nrm = N_VWrmsNorm_Serial(xQS[0],wQS[0]);
   for (is=1; is<cv_mem.cv_Ns; is++) {
     snrm = N_VWrmsNorm_Serial(xQS[is],wQS[is]);
     if ( snrm > nrm ) nrm = snrm;
   }

   return(nrm);
 }

 /* 
  * -----------------------------------------------------------------
  * Function to handle failures
  * -----------------------------------------------------------------
  */

 /*
  * cvHandleFailure
  *
  * This routine prints error messages for all cases of failure by
  * cvHin or cvStep. 
  * It returns to CVode the value that CVode is to return to the user.
  */

 private int cvHandleFailure(CVodeMemRec cv_mem, int flag)
 {

   /* Set vector of  absolute weighted local errors */
   /*
   N_VProd(acor, ewt, tempv);
   N_VAbs(tempv, tempv);
   */

   /* Depending on flag, print error message and return error flag */
   switch (flag) {
   case CV_ERR_FAILURE:
     cvProcessError(cv_mem, CV_ERR_FAILURE, "CVODES", "CVode",
                    MSGCV_ERR_FAILS, cv_mem.cv_tn, cv_mem.cv_h);
     break;
   case CV_CONV_FAILURE:
     cvProcessError(cv_mem, CV_CONV_FAILURE, "CVODES", "CVode",
                    MSGCV_CONV_FAILS, cv_mem.cv_tn, cv_mem.cv_h);
     break;
   case CV_LSETUP_FAIL:
     cvProcessError(cv_mem, CV_LSETUP_FAIL, "CVODES", "CVode",
                    MSGCV_SETUP_FAILED, cv_mem.cv_tn);
     break;
   case CV_LSOLVE_FAIL:
     cvProcessError(cv_mem, CV_LSOLVE_FAIL, "CVODES", "CVode",
                    MSGCV_SOLVE_FAILED, cv_mem.cv_tn);
     break;
   case CV_RHSFUNC_FAIL:
     cvProcessError(cv_mem, CV_RHSFUNC_FAIL, "CVODES", "CVode",
                    MSGCV_RHSFUNC_FAILED, cv_mem.cv_tn);
     break;
   case CV_UNREC_RHSFUNC_ERR:
     cvProcessError(cv_mem, CV_UNREC_RHSFUNC_ERR, "CVODES", "CVode",
                    MSGCV_RHSFUNC_UNREC, cv_mem.cv_tn);
     break;
   case CV_REPTD_RHSFUNC_ERR:
     cvProcessError(cv_mem, CV_REPTD_RHSFUNC_ERR, "CVODES", "CVode",
                    MSGCV_RHSFUNC_REPTD, cv_mem.cv_tn);
     break;
   case CV_RTFUNC_FAIL:
     cvProcessError(cv_mem, CV_RTFUNC_FAIL, "CVODES", "CVode",
                    MSGCV_RTFUNC_FAILED, cv_mem.cv_tn);
     break;
   case CV_QRHSFUNC_FAIL:
     cvProcessError(cv_mem, CV_QRHSFUNC_FAIL, "CVODES", "CVode",
                    MSGCV_QRHSFUNC_FAILED, cv_mem.cv_tn);
     break;
   case CV_UNREC_QRHSFUNC_ERR:
     cvProcessError(cv_mem, CV_UNREC_QRHSFUNC_ERR, "CVODES", "CVode",
                    MSGCV_QRHSFUNC_UNREC, cv_mem.cv_tn);
     break;
   case CV_REPTD_QRHSFUNC_ERR:
     cvProcessError(cv_mem, CV_REPTD_QRHSFUNC_ERR, "CVODES", "CVode",
                    MSGCV_QRHSFUNC_REPTD, cv_mem.cv_tn);
     break;
   case CV_SRHSFUNC_FAIL:
     cvProcessError(cv_mem, CV_SRHSFUNC_FAIL, "CVODES", "CVode",
                    MSGCV_SRHSFUNC_FAILED, cv_mem.cv_tn);
     break;
   case CV_UNREC_SRHSFUNC_ERR:
     cvProcessError(cv_mem, CV_UNREC_SRHSFUNC_ERR, "CVODES", "CVode",
                    MSGCV_SRHSFUNC_UNREC, cv_mem.cv_tn);
     break;
   case CV_REPTD_SRHSFUNC_ERR:
     cvProcessError(cv_mem, CV_REPTD_SRHSFUNC_ERR, "CVODES", "CVode",
                    MSGCV_SRHSFUNC_REPTD, cv_mem.cv_tn);
     break;
   case CV_QSRHSFUNC_FAIL:
     cvProcessError(cv_mem, CV_QSRHSFUNC_FAIL, "CVODES", "CVode",
                    MSGCV_QSRHSFUNC_FAILED, cv_mem.cv_tn);
     break;
   case CV_UNREC_QSRHSFUNC_ERR:
     cvProcessError(cv_mem, CV_UNREC_QSRHSFUNC_ERR, "CVODES", "CVode",
                    MSGCV_QSRHSFUNC_UNREC, cv_mem.cv_tn);
     break;
   case CV_REPTD_QSRHSFUNC_ERR:
     cvProcessError(cv_mem, CV_REPTD_QSRHSFUNC_ERR, "CVODES", "CVode",
                    MSGCV_QSRHSFUNC_REPTD, cv_mem.cv_tn);
     break;
   case CV_TOO_CLOSE:
     cvProcessError(cv_mem, CV_TOO_CLOSE, "CVODES", "CVode",
                    MSGCV_TOO_CLOSE);
     break;
   default:
     return(CV_SUCCESS);
   }

   return(flag);
 }
 
 /* 
  * cvRcheck1
  *
  * This routine completes the initialization of rootfinding memory
  * information, and checks whether g has a zero both at and very near
  * the initial point of the IVP.
  *
  * This routine returns an int equal to:
  *  CV_RTFUNC_FAIL < 0 if the g function failed, or
  *  CV_SUCCESS     = 0 otherwise.
  */

 private int cvRcheck1(CVodeMemRec cv_mem)
 {
   int i, retval;
   double smallh, hratio, tplus;
   boolean zroot;

   for (i = 0; i < cv_mem.cv_nrtfn; i++)
     cv_mem.cv_iroots[i] = 0;
   cv_mem.cv_tlo = cv_mem.cv_tn;
   cv_mem.cv_ttol = (Math.abs(cv_mem.cv_tn) + Math.abs(cv_mem.cv_h)) *
     cv_mem.cv_uround*HUNDRED;

   /* Evaluate g at initial t and check for zero values. */
   retval = g(cv_mem.cv_tlo, cv_mem.cv_zn[0],
                            cv_mem.cv_glo, cv_mem.cv_user_data);
   cv_mem.cv_nge = 1;
   if (retval != 0) return(CV_RTFUNC_FAIL);

   zroot = false;
   for (i = 0; i < cv_mem.cv_nrtfn; i++) {
     if (Math.abs(cv_mem.cv_glo[i]) == ZERO) {
       zroot = true;
       cv_mem.cv_gactive[i] = false;
     }
   }
   if (!zroot) return(CV_SUCCESS);

   /* Some g_i is zero at t0; look at g at t0+(small increment). */
   hratio = Math.max(cv_mem.cv_ttol/Math.abs(cv_mem.cv_h), PT1);
   smallh = hratio*cv_mem.cv_h;
   tplus = cv_mem.cv_tlo + smallh;
   N_VLinearSum_Serial(ONE, cv_mem.cv_zn[0], hratio, cv_mem.cv_zn[1], cv_mem.cv_y);
   retval = g(tplus, cv_mem.cv_y,
                            cv_mem.cv_ghi, cv_mem.cv_user_data);
   cv_mem.cv_nge++;
   if (retval != 0) return(CV_RTFUNC_FAIL);

   /* We check now only the components of g which were exactly 0.0 at t0
    * to see if we can 'activate' them. */
   for (i = 0; i < cv_mem.cv_nrtfn; i++) {
     if (!cv_mem.cv_gactive[i] && Math.abs(cv_mem.cv_ghi[i]) != ZERO) {
       cv_mem.cv_gactive[i] = true;
       cv_mem.cv_glo[i] = cv_mem.cv_ghi[i];
     }
   }
   return(CV_SUCCESS);
 }

 /*
  * cvRcheck2
  *
  * This routine checks for exact zeros of g at the last root found,
  * if the last return was a root.  It then checks for a close pair of
  * zeros (an error condition), and for a new root at a nearby point.
  * The array glo = g(tlo) at the left endpoint of the search interval
  * is adjusted if necessary to assure that all g_i are nonzero
  * there, before returning to do a root search in the interval.
  *
  * On entry, tlo = tretlast is the last value of tret returned by
  * CVode.  This may be the previous tn, the previous tout value,
  * or the last root location.
  *
  * This routine returns an int equal to:
  *     CV_RTFUNC_FAIL  < 0 if the g function failed, or
  *     CLOSERT         = 3 if a close pair of zeros was found, or
  *     RTFOUND         = 1 if a new zero of g was found near tlo, or
  *     CV_SUCCESS      = 0 otherwise.
  */

 private int cvRcheck2(CVodeMemRec cv_mem)
 {
   int i, retval;
   double smallh, hratio, tplus;
   boolean zroot;

   if (cv_mem.cv_irfnd == 0) return(CV_SUCCESS);

   CVodeGetDky(cv_mem, cv_mem.cv_tlo, 0, cv_mem.cv_y);
   retval = g(cv_mem.cv_tlo, cv_mem.cv_y,
                            cv_mem.cv_glo, cv_mem.cv_user_data);
   cv_mem.cv_nge++;
   if (retval != 0) return(CV_RTFUNC_FAIL);

   zroot = false;
   for (i = 0; i < cv_mem.cv_nrtfn; i++)
     cv_mem.cv_iroots[i] = 0;
   for (i = 0; i < cv_mem.cv_nrtfn; i++) {
     if (!cv_mem.cv_gactive[i]) continue;
     if (Math.abs(cv_mem.cv_glo[i]) == ZERO) {
       zroot = true;
       cv_mem.cv_iroots[i] = 1;
     }
   }
   if (!zroot) return(CV_SUCCESS);

   /* One or more g_i has a zero at tlo.  Check g at tlo+smallh. */
   cv_mem.cv_ttol = (Math.abs(cv_mem.cv_tn) + Math.abs(cv_mem.cv_h)) *
     cv_mem.cv_uround*HUNDRED;
   smallh = (cv_mem.cv_h > ZERO) ? cv_mem.cv_ttol : -cv_mem.cv_ttol;
   tplus = cv_mem.cv_tlo + smallh;
   if ( (tplus - cv_mem.cv_tn)*cv_mem.cv_h >= ZERO) {
     hratio = smallh/cv_mem.cv_h;
     N_VLinearSum_Serial(ONE, cv_mem.cv_y, hratio, cv_mem.cv_zn[1], cv_mem.cv_y);
   } else {
     CVodeGetDky(cv_mem, tplus, 0, cv_mem.cv_y);
   }
   retval = g(tplus, cv_mem.cv_y,
                            cv_mem.cv_ghi, cv_mem.cv_user_data);
   cv_mem.cv_nge++;
   if (retval != 0) return(CV_RTFUNC_FAIL);

   /* Check for close roots (error return), for a new zero at tlo+smallh,
   and for a g_i that changed from zero to nonzero. */
   zroot = false;
   for (i = 0; i < cv_mem.cv_nrtfn; i++) {
     if (!cv_mem.cv_gactive[i]) continue;
     if (Math.abs(cv_mem.cv_ghi[i]) == ZERO) {
       if (cv_mem.cv_iroots[i] == 1) return(CLOSERT);
       zroot = true;
       cv_mem.cv_iroots[i] = 1;
     } else {
       if (cv_mem.cv_iroots[i] == 1)
         cv_mem.cv_glo[i] = cv_mem.cv_ghi[i];
     }
   }
   if (zroot) return(RTFOUND);
   return(CV_SUCCESS);
 }
 
 /*
  * CVodeGetDky
  *
  * This routine computes the k-th derivative of the interpolating
  * polynomial at the time t and stores the result in the vector dky.
  * The formula is:
  *         q 
  *  dky = SUM c(j,k) * (t - tn)^(j-k) * h^(-j) * zn[j] , 
  *        j=k 
  * where c(j,k) = j*(j-1)*...*(j-k+1), q is the current order, and
  * zn[j] is the j-th column of the Nordsieck history array.
  *
  * This function is called by CVode with k = 0 and t = tout, but
  * may also be called directly by the user.
  */

 private int CVodeGetDky(CVodeMemRec cv_mem, double t, int k, NVector dky)
 {
   double s, c, r;
   double tfuzz, tp, tn1;
   int i, j;
   
   /* Check all inputs for legality */
  
   if (cv_mem == null) {
     cvProcessError(null, CV_MEM_NULL, "CVODES", "CVodeGetDky", MSGCV_NO_MEM);
     return(CV_MEM_NULL);
   }

   if (dky == null) {
     cvProcessError(cv_mem, CV_BAD_DKY, "CVODES", "CVodeGetDky", MSGCV_NULL_DKY);
     return(CV_BAD_DKY);
   }

   if ((k < 0) || (k > cv_mem.cv_q)) {
     cvProcessError(cv_mem, CV_BAD_K, "CVODES", "CVodeGetDky", MSGCV_BAD_K);
     return(CV_BAD_K);
   }
   
   /* Allow for some slack */
   tfuzz = FUZZ_FACTOR * cv_mem.cv_uround *
     (Math.abs(cv_mem.cv_tn) + Math.abs(cv_mem.cv_hu));
   if (cv_mem.cv_hu < ZERO) tfuzz = -tfuzz;
   tp = cv_mem.cv_tn - cv_mem.cv_hu - tfuzz;
   tn1 = cv_mem.cv_tn + tfuzz;
   if ((t-tp)*(t-tn1) > ZERO) {
     cvProcessError(cv_mem, CV_BAD_T, "CVODES", "CVodeGetDky", MSGCV_BAD_T,
                    t, cv_mem.cv_tn-cv_mem.cv_hu, cv_mem.cv_tn);
     return(CV_BAD_T);
   }

   /* Sum the differentiated interpolating polynomial */

   s = (t - cv_mem.cv_tn) / cv_mem.cv_h;
   for (j=cv_mem.cv_q; j >= k; j--) {
     c = ONE;
     for (i=j; i >= j-k+1; i--) c *= i;
     if (j == cv_mem.cv_q) {
       N_VScale_Serial(c, cv_mem.cv_zn[cv_mem.cv_q], dky);
     } else {
       N_VLinearSum_Serial(c, cv_mem.cv_zn[j], s, dky, dky);
     }
   }
   if (k == 0) return(CV_SUCCESS);
   r = Math.pow(cv_mem.cv_h, -k);
   N_VScale_Serial(r, dky, dky);
   return(CV_SUCCESS);
 }

 /*
  * cvRcheck3
  *
  * This routine interfaces to cvRootfind to look for a root of g
  * between tlo and either tn or tout, whichever comes first.
  * Only roots beyond tlo in the direction of integration are sought.
  *
  * This routine returns an int equal to:
  *     CV_RTFUNC_FAIL  < 0 if the g function failed, or
  *     RTFOUND         = 1 if a root of g was found, or
  *     CV_SUCCESS      = 0 otherwise.
  */

 private int cvRcheck3(CVodeMemRec cv_mem)
 {
   int i, ier, retval;

   /* Set thi = tn or tout, whichever comes first; set y = y(thi). */
   if (cv_mem.cv_taskc == CV_ONE_STEP) {
     cv_mem.cv_thi = cv_mem.cv_tn;
     N_VScale_Serial(ONE, cv_mem.cv_zn[0], cv_mem.cv_y);
   }
   if (cv_mem.cv_taskc == CV_NORMAL) {
     if ( (cv_mem.cv_toutc - cv_mem.cv_tn)*cv_mem.cv_h >= ZERO) {
       cv_mem.cv_thi = cv_mem.cv_tn; 
       N_VScale_Serial(ONE, cv_mem.cv_zn[0], cv_mem.cv_y);
     } else {
       cv_mem.cv_thi = cv_mem.cv_toutc;
       CVodeGetDky(cv_mem, cv_mem.cv_thi, 0, cv_mem.cv_y);
     }
   }

   /* Set ghi = g(thi) and call cvRootfind to search (tlo,thi) for roots. */
   retval = g(cv_mem.cv_thi, cv_mem.cv_y,
                            cv_mem.cv_ghi, cv_mem.cv_user_data);
   cv_mem.cv_nge++;
   if (retval != 0) return(CV_RTFUNC_FAIL);

   cv_mem.cv_ttol = (Math.abs(cv_mem.cv_tn) + Math.abs(cv_mem.cv_h)) *
     cv_mem.cv_uround*HUNDRED;
   ier = cvRootfind(cv_mem);
   if (ier == CV_RTFUNC_FAIL) return(CV_RTFUNC_FAIL);
   for(i=0; i<cv_mem.cv_nrtfn; i++) {
     if(!cv_mem.cv_gactive[i] && cv_mem.cv_grout[i] != ZERO)
       cv_mem.cv_gactive[i] = true;
   }
   cv_mem.cv_tlo = cv_mem.cv_trout;
   for (i = 0; i < cv_mem.cv_nrtfn; i++)
     cv_mem.cv_glo[i] = cv_mem.cv_grout[i];

   /* If no root found, return CV_SUCCESS. */  
   if (ier == CV_SUCCESS) return(CV_SUCCESS);

   /* If a root was found, interpolate to get y(trout) and return.  */
   CVodeGetDky(cv_mem, cv_mem.cv_trout, 0, cv_mem.cv_y);
   return(RTFOUND);
 }

 /*
  * cvRootfind
  *
  * This routine solves for a root of g(t) between tlo and thi, if
  * one exists.  Only roots of odd multiplicity (i.e. with a change
  * of sign in one of the g_i), or exact zeros, are found.
  * Here the sign of tlo - thi is arbitrary, but if multiple roots
  * are found, the one closest to tlo is returned.
  *
  * The method used is the Illinois algorithm, a modified secant method.
  * Reference: Kathie L. Hiebert and Lawrence F. Shampine, Implicitly
  * Defined Output Points for Solutions of ODEs, Sandia National
  * Laboratory Report SAND80-0180, February 1980.
  *
  * This routine uses the following parameters for communication:
  *
  * nrtfn    = number of functions g_i, or number of components of
  *            the vector-valued function g(t).  Input only.
  *
  * gfun     = user-defined function for g(t).  Its form is
  *            (void) gfun(t, y, gt, user_data)
  *
  * rootdir  = in array specifying the direction of zero-crossings.
  *            If rootdir[i] > 0, search for roots of g_i only if
  *            g_i is increasing; if rootdir[i] < 0, search for
  *            roots of g_i only if g_i is decreasing; otherwise
  *            always search for roots of g_i.
  *
  * gactive  = array specifying whether a component of g should
  *            or should not be monitored. gactive[i] is initially
  *            set to SUNTRUE for all i=0,...,nrtfn-1, but it may be
  *            reset to SUNFALSE if at the first step g[i] is 0.0
  *            both at the I.C. and at a small perturbation of them.
  *            gactive[i] is then set back on SUNTRUE only after the 
  *            corresponding g function moves away from 0.0.
  *
  * nge      = cumulative counter for gfun calls.
  *
  * ttol     = a convergence tolerance for trout.  Input only.
  *            When a root at trout is found, it is located only to
  *            within a tolerance of ttol.  Typically, ttol should
  *            be set to a value on the order of
  *               100 * UROUND * max (SUNRabs(tlo), SUNRabs(thi))
  *            where UROUND is the unit roundoff of the machine.
  *
  * tlo, thi = endpoints of the interval in which roots are sought.
  *            On input, these must be distinct, but tlo - thi may
  *            be of either sign.  The direction of integration is
  *            assumed to be from tlo to thi.  On return, tlo and thi
  *            are the endpoints of the final relevant interval.
  *
  * glo, ghi = arrays of length nrtfn containing the vectors g(tlo)
  *            and g(thi) respectively.  Input and output.  On input,
  *            none of the glo[i] should be zero.
  *
  * trout    = root location, if a root was found, or thi if not.
  *            Output only.  If a root was found other than an exact
  *            zero of g, trout is the endpoint thi of the final
  *            interval bracketing the root, with size at most ttol.
  *
  * grout    = array of length nrtfn containing g(trout) on return.
  *
  * iroots   = int array of length nrtfn with root information.
  *            Output only.  If a root was found, iroots indicates
  *            which components g_i have a root at trout.  For
  *            i = 0, ..., nrtfn-1, iroots[i] = 1 if g_i has a root
  *            and g_i is increasing, iroots[i] = -1 if g_i has a
  *            root and g_i is decreasing, and iroots[i] = 0 if g_i
  *            has no roots or g_i varies in the direction opposite
  *            to that indicated by rootdir[i].
  *
  * This routine returns an int equal to:
  *      CV_RTFUNC_FAIL  < 0 if the g function failed, or
  *      RTFOUND         = 1 if a root of g was found, or
  *      CV_SUCCESS      = 0 otherwise.
  */

 private int cvRootfind(CVodeMemRec cv_mem)
 {
   double alph, tmid, gfrac, maxfrac, fracint, fracsub;
   int i, retval, imax, side, sideprev;
   boolean zroot, sgnchg;

   imax = 0;

   /* First check for change in sign in ghi or for a zero in ghi. */
   maxfrac = ZERO;
   zroot = false;
   sgnchg = false;
   for (i = 0;  i < cv_mem.cv_nrtfn; i++) {
     if(!cv_mem.cv_gactive[i]) continue;
     if (Math.abs(cv_mem.cv_ghi[i]) == ZERO) {
       if(cv_mem.cv_rootdir[i]*cv_mem.cv_glo[i] <= ZERO) {
         zroot = true;
       }
     } else {
       if ( (cv_mem.cv_glo[i]*cv_mem.cv_ghi[i] < ZERO) &&
            (cv_mem.cv_rootdir[i]*cv_mem.cv_glo[i] <= ZERO) ) {
         gfrac = Math.abs(cv_mem.cv_ghi[i]/(cv_mem.cv_ghi[i] - cv_mem.cv_glo[i]));
         if (gfrac > maxfrac) {
           sgnchg = true;
           maxfrac = gfrac;
           imax = i;
         }
       }
     }
   }

   /* If no sign change was found, reset trout and grout.  Then return
      CV_SUCCESS if no zero was found, or set iroots and return RTFOUND.  */ 
   if (!sgnchg) {
     cv_mem.cv_trout = cv_mem.cv_thi;
     for (i = 0; i < cv_mem.cv_nrtfn; i++)
       cv_mem.cv_grout[i] = cv_mem.cv_ghi[i];
     if (!zroot) return(CV_SUCCESS);
     for (i = 0; i < cv_mem.cv_nrtfn; i++) {
       cv_mem.cv_iroots[i] = 0;
       if(!cv_mem.cv_gactive[i]) continue;
       if ( (Math.abs(cv_mem.cv_ghi[i]) == ZERO) &&
            (cv_mem.cv_rootdir[i]*cv_mem.cv_glo[i] <= ZERO) )
         cv_mem.cv_iroots[i] = cv_mem.cv_glo[i] > 0 ? -1:1;
     }
     return(RTFOUND);
   }

   /* Initialize alph to avoid compiler warning */
   alph = ONE;

   /* A sign change was found.  Loop to locate nearest root. */

   side = 0;  sideprev = -1;
   for(;;) {                                    /* Looping point */

     /* If interval size is already less than tolerance ttol, break. */
       if (Math.abs(cv_mem.cv_thi - cv_mem.cv_tlo) <= cv_mem.cv_ttol) break;

     /* Set weight alph.
        On the first two passes, set alph = 1.  Thereafter, reset alph
        according to the side (low vs high) of the subinterval in which
        the sign change was found in the previous two passes.
        If the sides were opposite, set alph = 1.
        If the sides were the same, then double alph (if high side),
        or halve alph (if low side).
        The next guess tmid is the secant method value if alph = 1, but
        is closer to cv_mem->cv_tlo if alph < 1, and closer to thi if alph > 1.    */

     if (sideprev == side) {
       alph = (side == 2) ? alph*TWO : alph*HALF;
     } else {
       alph = ONE;
     }

     /* Set next root approximation tmid and get g(tmid).
        If tmid is too close to tlo or thi, adjust it inward,
        by a fractional distance that is between 0.1 and 0.5.  */
     tmid = cv_mem.cv_thi - (cv_mem.cv_thi - cv_mem.cv_tlo) *
       cv_mem.cv_ghi[imax] / (cv_mem.cv_ghi[imax] - alph*cv_mem.cv_glo[imax]);
     if (Math.abs(tmid - cv_mem.cv_tlo) < HALF*cv_mem.cv_ttol) {
       fracint = Math.abs(cv_mem.cv_thi - cv_mem.cv_tlo)/cv_mem.cv_ttol;
       fracsub = (fracint > FIVE) ? PT1 : HALF/fracint;
       tmid = cv_mem.cv_tlo + fracsub*(cv_mem.cv_thi - cv_mem.cv_tlo);
     }
     if (Math.abs(cv_mem.cv_thi - tmid) < HALF*cv_mem.cv_ttol) {
       fracint = Math.abs(cv_mem.cv_thi - cv_mem.cv_tlo)/cv_mem.cv_ttol;
       fracsub = (fracint > FIVE) ? PT1 : HALF/fracint;
       tmid = cv_mem.cv_thi - fracsub*(cv_mem.cv_thi - cv_mem.cv_tlo);
     }

     CVodeGetDky(cv_mem, tmid, 0, cv_mem.cv_y);
     retval = g(tmid, cv_mem.cv_y, cv_mem.cv_grout,
                              cv_mem.cv_user_data);
     cv_mem.cv_nge++;
     if (retval != 0) return(CV_RTFUNC_FAIL);

     /* Check to see in which subinterval g changes sign, and reset imax.
        Set side = 1 if sign change is on low side, or 2 if on high side.  */  
     maxfrac = ZERO;
     zroot = false;
     sgnchg = false;
     sideprev = side;
     for (i = 0;  i < cv_mem.cv_nrtfn; i++) {
       if(!cv_mem.cv_gactive[i]) continue;
       if (Math.abs(cv_mem.cv_grout[i]) == ZERO) {
         if(cv_mem.cv_rootdir[i]*cv_mem.cv_glo[i] <= ZERO) zroot = true;
       } else {
         if ( (cv_mem.cv_glo[i]*cv_mem.cv_grout[i] < ZERO) &&
              (cv_mem.cv_rootdir[i]*cv_mem.cv_glo[i] <= ZERO) ) {
           gfrac = Math.abs(cv_mem.cv_grout[i] /
                           (cv_mem.cv_grout[i] - cv_mem.cv_glo[i]));
           if (gfrac > maxfrac) {
             sgnchg = true;
             maxfrac = gfrac;
             imax = i;
           }
         }
       }
     }
     if (sgnchg) {
       /* Sign change found in (tlo,tmid); replace thi with tmid. */
       cv_mem.cv_thi = tmid;
       for (i = 0; i < cv_mem.cv_nrtfn; i++)
         cv_mem.cv_ghi[i] = cv_mem.cv_grout[i];
       side = 1;
       /* Stop at root thi if converged; otherwise loop. */
       if (Math.abs(cv_mem.cv_thi - cv_mem.cv_tlo) <= cv_mem.cv_ttol) break;
       continue;  /* Return to looping point. */
     }

     if (zroot) {
       /* No sign change in (tlo,tmid), but g = 0 at tmid; return root tmid. */
       cv_mem.cv_thi = tmid;
       for (i = 0; i < cv_mem.cv_nrtfn; i++)
         cv_mem.cv_ghi[i] = cv_mem.cv_grout[i];
       break;
     }

     /* No sign change in (tlo,tmid), and no zero at tmid.
        Sign change must be in (tmid,thi).  Replace tlo with tmid. */
     cv_mem.cv_tlo = tmid;
     for (i = 0; i < cv_mem.cv_nrtfn; i++)
       cv_mem.cv_glo[i] = cv_mem.cv_grout[i];
     side = 2;
     /* Stop at root thi if converged; otherwise loop back. */
     if (Math.abs(cv_mem.cv_thi - cv_mem.cv_tlo) <= cv_mem.cv_ttol) break;

   } /* End of root-search loop */

   /* Reset trout and grout, set iroots, and return RTFOUND. */
   cv_mem.cv_trout = cv_mem.cv_thi;
   for (i = 0; i < cv_mem.cv_nrtfn; i++) {
     cv_mem.cv_grout[i] = cv_mem.cv_ghi[i];
     cv_mem.cv_iroots[i] = 0;
     if(!cv_mem.cv_gactive[i]) continue;
     if ( (Math.abs(cv_mem.cv_ghi[i]) == ZERO) &&
          (cv_mem.cv_rootdir[i]*cv_mem.cv_glo[i] <= ZERO) )
       cv_mem.cv_iroots[i] = cv_mem.cv_glo[i] > 0 ? -1:1;
     if ( (cv_mem.cv_glo[i]*cv_mem.cv_ghi[i] < ZERO) &&
          (cv_mem.cv_rootdir[i]*cv_mem.cv_glo[i] <= ZERO) ) 
       cv_mem.cv_iroots[i] = cv_mem.cv_glo[i] > 0 ? -1:1;
   }
   return(RTFOUND);
 }

 /* 
  * -----------------------------------------------------------------
  * Main cvStep function
  * -----------------------------------------------------------------
  */

 /* 
  * cvStep
  *
  * This routine performs one internal cvode step, from tn to tn + h.
  * It calls other routines to do all the work.
  *
  * The main operations done here are as follows:
  * - preliminary adjustments if a new step size was chosen;
  * - prediction of the Nordsieck history array zn at tn + h;
  * - setting of multistep method coefficients and test quantities;
  * - solution of the nonlinear system;
  * - testing the local error;
  * - updating zn and other state data if successful;
  * - resetting stepsize and order for the next step.
  * - if SLDET is on, check for stability, reduce order if necessary.
  * On a failure in the nonlinear system solution or error test, the
  * step may be reattempted, depending on the nature of the failure.
  */

 private int cvStep(CVodeMemRec cv_mem)
 {
   double saved_t, dsm, dsmQ, dsmS, dsmQS;
   boolean do_sensi_stg, do_sensi_stg1;
   int ncf, ncfS;
   int nef, nefQ, nefS, nefQS;
   int nflag, kflag, eflag;
   int retval, is;

   /* Are we computing sensitivities with a staggered approach? */

   do_sensi_stg  = (cv_mem.cv_sensi && (cv_mem.cv_ism==CV_STAGGERED));
   do_sensi_stg1 = (cv_mem.cv_sensi && (cv_mem.cv_ism==CV_STAGGERED1));

   /* Initialize local counters for convergence and error test failures */

   ncf  = nef  = 0;
   nefQ = nefQS = 0;
   ncfS = nefS = 0;
   if (do_sensi_stg1) {
     for (is=0; is<cv_mem.cv_Ns; is++)
       cv_mem.cv_ncfS1[is] = 0;
   }

   /* If needed, adjust method parameters */

   if ((cv_mem.cv_nst > 0) && (cv_mem.cv_hprime != cv_mem.cv_h))
     cvAdjustParams(cv_mem);

   /* Looping point for attempts to take a step */

   saved_t = cv_mem.cv_tn;
   nflag = FIRST_CALL;

   for(;;) {  

     cvPredict(cv_mem);  
     cvSet(cv_mem);

     /* ------ Correct state variables ------ */
     
     nflag = cvNls(cv_mem, nflag);
   /*  kflag = cvHandleNFlag(cv_mem, &nflag, saved_t, &ncf, &(cv_mem->cv_ncfn));*/

     /* Go back in loop if we need to predict again (nflag=PREV_CONV_FAIL) */
    /* if (kflag == PREDICT_AGAIN) continue;*/

     /* Return if nonlinear solve failed and recovery not possible. */
    /* if (kflag != DO_ERROR_TEST) return(kflag);*/

     /* Perform error test (nflag=CV_SUCCESS) */
   /*  eflag = cvDoErrorTest(cv_mem, &nflag, saved_t, cv_mem->cv_acnrm,
                           &nef, &(cv_mem->cv_netf), &dsm);*/

     /* Go back in loop if we need to predict again (nflag=PREV_ERR_FAIL) */
    /* if (eflag == TRY_AGAIN) continue;*/

     /* Return if error test failed and recovery not possible. */
    /* if (eflag != CV_SUCCESS) return(eflag); */

     /* Error test passed (eflag=CV_SUCCESS, nflag=CV_SUCCESS), go on */

     /* ------ Correct the quadrature variables ------ */

    /* if (cv_mem->cv_quadr) {

       ncf = nef = 0;*/ /* reset counters for states */

     /*  nflag = cvQuadNls(cv_mem);
       kflag = cvHandleNFlag(cv_mem, &nflag, saved_t, &ncf, &(cv_mem->cv_ncfn));

       if (kflag == PREDICT_AGAIN) continue;
       if (kflag != DO_ERROR_TEST) return(kflag);*/

       /* Error test on quadratures */
      /* if (cv_mem->cv_errconQ) {
         cv_mem->cv_acnrmQ = N_VWrmsNorm(cv_mem->cv_acorQ, cv_mem->cv_ewtQ);
         eflag = cvDoErrorTest(cv_mem, &nflag, saved_t, cv_mem->cv_acnrmQ,
                               &nefQ, &(cv_mem->cv_netfQ), &dsmQ);

         if (eflag == TRY_AGAIN) continue;
         if (eflag != CV_SUCCESS) return(eflag);*/

         /* Set dsm = max(dsm, dsmQ) to be used in cvPrepareNextStep */
        /* if (dsmQ > dsm) dsm = dsmQ;
       }

     }*/

     /* ------ Correct the sensitivity variables (STAGGERED or STAGGERED1) ------- */

    /* if (do_sensi_stg || do_sensi_stg1) {

       ncf = nef = 0;    */    /* reset counters for states     */
      /* if (cv_mem->cv_quadr) nefQ = 0;  *//* reset counter for quadratures */

       /* Evaluate f at converged y, needed for future evaluations of sens. RHS 
        * If f() fails recoverably, treat it as a convergence failure and 
        * attempt the step again */

      /* retval = cv_mem->cv_f(cv_mem->cv_tn, cv_mem->cv_y,
                             cv_mem->cv_ftemp, cv_mem->cv_user_data);
       cv_mem->cv_nfe++;
       if (retval < 0) return(CV_RHSFUNC_FAIL);
       if (retval > 0) {
         nflag = PREV_CONV_FAIL;
         continue;
       }

       if (do_sensi_stg) {*/
         /* Nonlinear solve for sensitivities (all-at-once) */
        /* nflag = cvStgrNls(cv_mem);
         kflag = cvHandleNFlag(cv_mem, &nflag, saved_t, &ncfS,
                               &(cv_mem->cv_ncfnS));
       } else {*/
         /* Nonlinear solve for sensitivities (one-by-one) */
        /* for (is=0; is<cv_mem->cv_Ns; is++) { 
           nflag = cvStgr1Nls(cv_mem, is); 
           kflag = cvHandleNFlag(cv_mem, &nflag, saved_t,
                                 &(cv_mem->cv_ncfS1[is]),
                                 &(cv_mem->cv_ncfnS1[is]));
           if (kflag != DO_ERROR_TEST) break; 
         }
       }

       if (kflag == PREDICT_AGAIN) continue;
       if (kflag != DO_ERROR_TEST) return(kflag);*/

       /* Error test on sensitivities */
     /*  if (cv_mem->cv_errconS) {

         if (do_sensi_stg1)
           cv_mem->cv_acnrmS = cvSensNorm(cv_mem, cv_mem->cv_acorS, cv_mem->cv_ewtS);

         eflag = cvDoErrorTest(cv_mem, &nflag, saved_t, cv_mem->cv_acnrmS,
                               &nefS, &(cv_mem->cv_netfS), &dsmS);

         if (eflag == TRY_AGAIN)  continue;
         if (eflag != CV_SUCCESS) return(eflag);*/

         /* Set dsm = max(dsm, dsmS) to be used in cvPrepareNextStep */
      /*   if (dsmS > dsm) dsm = dsmS;

       }

     } */

     /* ------ Correct the quadrature sensitivity variables ------ */

   /*  if (cv_mem->cv_quadr_sensi) { */

       /* Reset local convergence and error test failure counters */
    /*   ncf = nef = 0;
       if (cv_mem->cv_quadr) nefQ = 0;
       if (do_sensi_stg) ncfS = nefS = 0;
       if (do_sensi_stg1) {
         for (is=0; is<cv_mem->cv_Ns; is++)
           cv_mem->cv_ncfS1[is] = 0;
         nefS = 0;
       }*/

       /* Note that ftempQ contains yQdot evaluated at the converged y
        * (stored in cvQuadNls) and can be used in evaluating fQS */

     /*  nflag = cvQuadSensNls(cv_mem);
       kflag = cvHandleNFlag(cv_mem, &nflag, saved_t, &ncf, &(cv_mem->cv_ncfn));

       if (kflag == PREDICT_AGAIN) continue;
       if (kflag != DO_ERROR_TEST) return(kflag);*/

       /* Error test on quadrature sensitivities */
     /*  if (cv_mem->cv_errconQS) {
         cv_mem->cv_acnrmQS = cvQuadSensNorm(cv_mem, cv_mem->cv_acorQS,
                                             cv_mem->cv_ewtQS);
         eflag = cvDoErrorTest(cv_mem, &nflag, saved_t, cv_mem->cv_acnrmQS,
                               &nefQS, &(cv_mem->cv_netfQS), &dsmQS);

         if (eflag == TRY_AGAIN) continue;
         if (eflag != CV_SUCCESS) return(eflag);*/

         /* Set dsm = max(dsm, dsmQS) to be used in cvPrepareNextStep */
       /*  if (dsmQS > dsm) dsm = dsmQS;
       }


     }*/


     /* Everything went fine; exit loop */ 
     break;

   }

   /* Nonlinear system solve and error test were both successful.
      Update data, and consider change of step and/or order.       */

   /*cvCompleteStep(cv_mem); 

   cvPrepareNextStep(cv_mem, dsm);*/ 

   /* If Stablilty Limit Detection is turned on, call stability limit
      detection routine for possible order reduction. */

  /* if (cv_mem->cv_sldeton) cvBDFStab(cv_mem);

   cv_mem->cv_etamax = (cv_mem->cv_nst <= SMALL_NST) ? ETAMX2 : ETAMX3;*/

   /*  Finally, we rescale the acor array to be the 
       estimated local error vector. */

 /*  N_VScale(cv_mem->cv_tq[2], cv_mem->cv_acor, cv_mem->cv_acor);

   if (cv_mem->cv_quadr)
     N_VScale(cv_mem->cv_tq[2], cv_mem->cv_acorQ, cv_mem->cv_acorQ);

   if (cv_mem->cv_sensi)
     for (is=0; is<cv_mem->cv_Ns; is++)
       N_VScale(cv_mem->cv_tq[2], cv_mem->cv_acorS[is], cv_mem->cv_acorS[is]);

   if (cv_mem->cv_quadr_sensi)
     for (is=0; is<cv_mem->cv_Ns; is++)
       N_VScale(cv_mem->cv_tq[2], cv_mem->cv_acorQS[is], cv_mem->cv_acorQS[is]);*/

   return(CV_SUCCESS);
       
 }
 
 /*
  * cvAdjustParams
  *
  * This routine is called when a change in step size was decided upon,
  * and it handles the required adjustments to the history array zn.
  * If there is to be a change in order, we call cvAdjustOrder and reset
  * q, L = q+1, and qwait.  Then in any case, we call cvRescale, which
  * resets h and rescales the Nordsieck array.
  */

 private void cvAdjustParams(CVodeMemRec cv_mem)
 {
   if (cv_mem.cv_qprime != cv_mem.cv_q) {
     cvAdjustOrder(cv_mem, cv_mem.cv_qprime-cv_mem.cv_q);
     cv_mem.cv_q = cv_mem.cv_qprime;
     cv_mem.cv_L = cv_mem.cv_q+1;
     cv_mem.cv_qwait = cv_mem.cv_L;
   }
   cvRescale(cv_mem);
 }
 
 /*
  * cvAdjustOrder
  *
  * This routine is a high level routine which handles an order
  * change by an amount deltaq (= +1 or -1). If a decrease in order
  * is requested and q==2, then the routine returns immediately.
  * Otherwise cvAdjustAdams or cvAdjustBDF is called to handle the
  * order change (depending on the value of lmm).
  */

 private void cvAdjustOrder(CVodeMemRec cv_mem, int deltaq)
 {
   if ((cv_mem.cv_q==2) && (deltaq != 1)) return;
   
   switch(cv_mem.cv_lmm){
   case CV_ADAMS:
     cvAdjustAdams(cv_mem, deltaq);
     break;
   case CV_BDF:
     cvAdjustBDF(cv_mem, deltaq);
     break;
   }
 }

 /*
  * cvAdjustAdams
  *
  * This routine adjusts the history array on a change of order q by
  * deltaq, in the case that lmm == CV_ADAMS.
  */

 private void cvAdjustAdams(CVodeMemRec cv_mem, int deltaq)
 {
   int i, j;
   int is;
   double xi, hsum;

   /* On an order increase, set new column of zn to zero and return */
   
   if (deltaq==1) {
     N_VConst_Serial(ZERO, cv_mem.cv_zn[cv_mem.cv_L]);
     if (cv_mem.cv_quadr)
       N_VConst_Serial(ZERO, cv_mem.cv_znQ[cv_mem.cv_L]);
     if (cv_mem.cv_sensi)
       for (is=0; is<cv_mem.cv_Ns; is++)
         N_VConst_Serial(ZERO, cv_mem.cv_znS[cv_mem.cv_L][is]);
     return;
   }

   /*
    * On an order decrease, each zn[j] is adjusted by a multiple of zn[q].
    * The coeffs. in the adjustment are the coeffs. of the polynomial:
    *        x
    * q * INT { u * ( u + xi_1 ) * ... * ( u + xi_{q-2} ) } du 
    *        0
    * where xi_j = [t_n - t_(n-j)]/h => xi_0 = 0
    */

   for (i=0; i <= cv_mem.cv_qmax; i++) cv_mem.cv_l[i] = ZERO;
   cv_mem.cv_l[1] = ONE;
   hsum = ZERO;
   for (j=1; j <= cv_mem.cv_q-2; j++) {
     hsum += cv_mem.cv_tau[j];
     xi = hsum / cv_mem.cv_hscale;
     for (i=j+1; i >= 1; i--)
       cv_mem.cv_l[i] = cv_mem.cv_l[i]*xi + cv_mem.cv_l[i-1];
   }
   
   for (j=1; j <= cv_mem.cv_q-2; j++)
     cv_mem.cv_l[j+1] = cv_mem.cv_q * (cv_mem.cv_l[j] / (j+1));
   
   for (j=2; j < cv_mem.cv_q; j++)
     N_VLinearSum_Serial(-cv_mem.cv_l[j], cv_mem.cv_zn[cv_mem.cv_q], ONE,
                  cv_mem.cv_zn[j], cv_mem.cv_zn[j]);

   if (cv_mem.cv_quadr)
     for (j=2; j < cv_mem.cv_q; j++)
       N_VLinearSum_Serial(-cv_mem.cv_l[j], cv_mem.cv_znQ[cv_mem.cv_q], ONE,
                    cv_mem.cv_znQ[j], cv_mem.cv_znQ[j]);

   if (cv_mem.cv_sensi)
     for (is=0; is<cv_mem.cv_Ns; is++)
       for (j=2; j < cv_mem.cv_q; j++)
         N_VLinearSum_Serial(-cv_mem.cv_l[j], cv_mem.cv_znS[cv_mem.cv_q][is],
                      ONE, cv_mem.cv_znS[j][is], cv_mem.cv_znS[j][is]);

 }

 /*
  * cvAdjustBDF
  *
  * This is a high level routine which handles adjustments to the
  * history array on a change of order by deltaq in the case that 
  * lmm == CV_BDF.  cvAdjustBDF calls cvIncreaseBDF if deltaq = +1 and 
  * cvDecreaseBDF if deltaq = -1 to do the actual work.
  */

 private void cvAdjustBDF(CVodeMemRec cv_mem, int deltaq)
 {
   switch(deltaq) {
   case 1: 
     cvIncreaseBDF(cv_mem);
     return;
   case -1: 
     cvDecreaseBDF(cv_mem);
     return;
   }
 }

 /*
  * cvIncreaseBDF
  *
  * This routine adjusts the history array on an increase in the 
  * order q in the case that lmm == CV_BDF.  
  * A new column zn[q+1] is set equal to a multiple of the saved 
  * vector (= acor) in zn[indx_acor].  Then each zn[j] is adjusted by
  * a multiple of zn[q+1].  The coefficients in the adjustment are the 
  * coefficients of the polynomial x*x*(x+xi_1)*...*(x+xi_j),
  * where xi_j = [t_n - t_(n-j)]/h.
  */

 private void cvIncreaseBDF(CVodeMemRec cv_mem)
 {
   double alpha0, alpha1, prod, xi, xiold, hsum, A1;
   int i, j;
   int is;

   for (i=0; i <= cv_mem.cv_qmax; i++)
     cv_mem.cv_l[i] = ZERO;
   cv_mem.cv_l[2] = alpha1 = prod = xiold = ONE;
   alpha0 = -ONE;
   hsum = cv_mem.cv_hscale;
   if (cv_mem.cv_q > 1) {
     for (j=1; j < cv_mem.cv_q; j++) {
       hsum += cv_mem.cv_tau[j+1];
       xi = hsum / cv_mem.cv_hscale;
       prod *= xi;
       alpha0 -= ONE / (j+1);
       alpha1 += ONE / xi;
       for (i=j+2; i >= 2; i--)
         cv_mem.cv_l[i] = cv_mem.cv_l[i]*xiold + cv_mem.cv_l[i-1];
       xiold = xi;
     }
   }
   A1 = (-alpha0 - alpha1) / prod;

   /* 
      zn[indx_acor] contains the value Delta_n = y_n - y_n(0) 
      This value was stored there at the previous successful
      step (in cvCompleteStep) 
      
      A1 contains dbar = (1/xi* - 1/xi_q)/prod(xi_j)
   */
   
   N_VScale_Serial(A1, cv_mem.cv_zn[cv_mem.cv_indx_acor], cv_mem.cv_zn[cv_mem.cv_L]);
   for (j=2; j <= cv_mem.cv_q; j++)
     N_VLinearSum_Serial(cv_mem.cv_l[j], cv_mem.cv_zn[cv_mem.cv_L],
                  ONE, cv_mem.cv_zn[j], cv_mem.cv_zn[j]);

   if (cv_mem.cv_quadr) {
     N_VScale_Serial(A1, cv_mem.cv_znQ[cv_mem.cv_indx_acor], cv_mem.cv_znQ[cv_mem.cv_L]);
     for (j=2; j <= cv_mem.cv_q; j++)
       N_VLinearSum_Serial(cv_mem.cv_l[j], cv_mem.cv_znQ[cv_mem.cv_L],
                    ONE, cv_mem.cv_znQ[j], cv_mem.cv_znQ[j]);
   }

   if (cv_mem.cv_sensi) {
     for (is=0; is<cv_mem.cv_Ns; is++) {
       N_VScale_Serial(A1, cv_mem.cv_znS[cv_mem.cv_indx_acor][is],
                cv_mem.cv_znS[cv_mem.cv_L][is]);
       for (j=2; j <= cv_mem.cv_q; j++)
         N_VLinearSum_Serial(cv_mem.cv_l[j], cv_mem.cv_znS[cv_mem.cv_L][is],
                      ONE, cv_mem.cv_znS[j][is], cv_mem.cv_znS[j][is]);
     }
   }

   if (cv_mem.cv_quadr_sensi) {
     for (is=0; is<cv_mem.cv_Ns; is++) {
       N_VScale_Serial(A1, cv_mem.cv_znQS[cv_mem.cv_indx_acor][is],
                cv_mem.cv_znQS[cv_mem.cv_L][is]);
       for (j=2; j <= cv_mem.cv_q; j++)
         N_VLinearSum_Serial(cv_mem.cv_l[j], cv_mem.cv_znQS[cv_mem.cv_L][is],
                      ONE, cv_mem.cv_znQS[j][is], cv_mem.cv_znQS[j][is]);
     }
   }

 }

 /*
  * cvDecreaseBDF
  *
  * This routine adjusts the history array on a decrease in the 
  * order q in the case that lmm == CV_BDF.  
  * Each zn[j] is adjusted by a multiple of zn[q].  The coefficients
  * in the adjustment are the coefficients of the polynomial
  *   x*x*(x+xi_1)*...*(x+xi_j), where xi_j = [t_n - t_(n-j)]/h.
  */

 private void cvDecreaseBDF(CVodeMemRec cv_mem)
 {
   double hsum, xi;
   int i, j;
   int is;
   
   for (i=0; i <= cv_mem.cv_qmax; i++)
     cv_mem.cv_l[i] = ZERO;
   cv_mem.cv_l[2] = ONE;
   hsum = ZERO;
   for (j=1; j <= cv_mem.cv_q-2; j++) {
     hsum += cv_mem.cv_tau[j];
     xi = hsum / cv_mem.cv_hscale;
     for (i=j+2; i >= 2; i--)
       cv_mem.cv_l[i] = cv_mem.cv_l[i]*xi + cv_mem.cv_l[i-1];
   }
   
   for (j=2; j < cv_mem.cv_q; j++)
     N_VLinearSum_Serial(-cv_mem.cv_l[j], cv_mem.cv_zn[cv_mem.cv_q],
                  ONE, cv_mem.cv_zn[j], cv_mem.cv_zn[j]);

   if (cv_mem.cv_quadr) {
     for (j=2; j < cv_mem.cv_q; j++)
       N_VLinearSum_Serial(-cv_mem.cv_l[j], cv_mem.cv_znQ[cv_mem.cv_q],
                    ONE, cv_mem.cv_znQ[j], cv_mem.cv_znQ[j]);
   }

   if (cv_mem.cv_sensi) {
     for (is=0; is<cv_mem.cv_Ns; is++) 
       for (j=2; j < cv_mem.cv_q; j++)
         N_VLinearSum_Serial(-cv_mem.cv_l[j], cv_mem.cv_znS[cv_mem.cv_q][is],
                      ONE, cv_mem.cv_znS[j][is], cv_mem.cv_znS[j][is]);
   }

   if (cv_mem.cv_quadr_sensi) {
     for (is=0; is<cv_mem.cv_Ns; is++) 
       for (j=2; j < cv_mem.cv_q; j++)
         N_VLinearSum_Serial(-cv_mem.cv_l[j], cv_mem.cv_znQS[cv_mem.cv_q][is],
                      ONE, cv_mem.cv_znQS[j][is], cv_mem.cv_znQS[j][is]);
   }
 }

 /*
  * cvRescale
  *
  * This routine rescales the Nordsieck array by multiplying the
  * jth column zn[j] by eta^j, j = 1, ..., q.  Then the value of
  * h is rescaled by eta, and hscale is reset to h.
  */

 private void cvRescale(CVodeMemRec cv_mem)
 {
   int j;
   int is;
   double factor;

   factor = cv_mem.cv_eta;
   for (j=1; j <= cv_mem.cv_q; j++) {

     N_VScale_Serial(factor, cv_mem.cv_zn[j], cv_mem.cv_zn[j]);

     if (cv_mem.cv_quadr)
       N_VScale_Serial(factor, cv_mem.cv_znQ[j], cv_mem.cv_znQ[j]);

     if (cv_mem.cv_sensi)
       for (is=0; is<cv_mem.cv_Ns; is++)
         N_VScale_Serial(factor, cv_mem.cv_znS[j][is], cv_mem.cv_znS[j][is]);

     if (cv_mem.cv_quadr_sensi)
       for (is=0; is<cv_mem.cv_Ns; is++)
         N_VScale_Serial(factor, cv_mem.cv_znQS[j][is], cv_mem.cv_znQS[j][is]);

     factor *= cv_mem.cv_eta;

   }

   cv_mem.cv_h = cv_mem.cv_hscale * cv_mem.cv_eta;
   cv_mem.cv_next_h = cv_mem.cv_h;
   cv_mem.cv_hscale = cv_mem.cv_h;
   cv_mem.cv_nscon = 0;

 }
 
 /*
  * cvPredict
  *
  * This routine advances tn by the tentative step size h, and computes
  * the predicted array z_n(0), which is overwritten on zn.  The
  * prediction of zn is done by repeated additions.
  * If tstop is enabled, it is possible for tn + h to be past tstop by roundoff,
  * and in that case, we reset tn (after incrementing by h) to tstop.
  */

 private void cvPredict(CVodeMemRec cv_mem)
 {
   int j, k;
   int is;

   cv_mem.cv_tn += cv_mem.cv_h;
   if (cv_mem.cv_tstopset) {
     if ((cv_mem.cv_tn - cv_mem.cv_tstop)*cv_mem.cv_h > ZERO)
       cv_mem.cv_tn = cv_mem.cv_tstop;
   }

   for (k = 1; k <= cv_mem.cv_q; k++)
     for (j = cv_mem.cv_q; j >= k; j--) 
       N_VLinearSum_Serial(ONE, cv_mem.cv_zn[j-1], ONE,
                    cv_mem.cv_zn[j], cv_mem.cv_zn[j-1]); 

   if (cv_mem.cv_quadr) {
     for (k = 1; k <= cv_mem.cv_q; k++)
       for (j = cv_mem.cv_q; j >= k; j--) 
         N_VLinearSum_Serial(ONE, cv_mem.cv_znQ[j-1], ONE,
                      cv_mem.cv_znQ[j], cv_mem.cv_znQ[j-1]);
   }

   if (cv_mem.cv_sensi) {
     for (is=0; is<cv_mem.cv_Ns; is++) {
       for (k = 1; k <= cv_mem.cv_q; k++)
         for (j = cv_mem.cv_q; j >= k; j--) 
           N_VLinearSum_Serial(ONE, cv_mem.cv_znS[j-1][is], ONE,
                        cv_mem.cv_znS[j][is], cv_mem.cv_znS[j-1][is]);
     }
   }

   if (cv_mem.cv_quadr_sensi) {
     for (is=0; is<cv_mem.cv_Ns; is++) {
       for (k = 1; k <= cv_mem.cv_q; k++)
         for (j = cv_mem.cv_q; j >= k; j--) 
           N_VLinearSum_Serial(ONE, cv_mem.cv_znQS[j-1][is], ONE,
                        cv_mem.cv_znQS[j][is], cv_mem.cv_znQS[j-1][is]);
     }
   }

 }

 /*
  * cvSet
  *
  * This routine is a high level routine which calls cvSetAdams or
  * cvSetBDF to set the polynomial l, the test quantity array tq, 
  * and the related variables  rl1, gamma, and gamrat.
  *
  * The array tq is loaded with constants used in the control of estimated
  * local errors and in the nonlinear convergence test.  Specifically, while
  * running at order q, the components of tq are as follows:
  *   tq[1] = a coefficient used to get the est. local error at order q-1
  *   tq[2] = a coefficient used to get the est. local error at order q
  *   tq[3] = a coefficient used to get the est. local error at order q+1
  *   tq[4] = constant used in nonlinear iteration convergence test
  *   tq[5] = coefficient used to get the order q+2 derivative vector used in
  *           the est. local error at order q+1
  */

 private void cvSet(CVodeMemRec cv_mem)
 {
   switch(cv_mem.cv_lmm) {
   case CV_ADAMS:
     cvSetAdams(cv_mem);
     break;
   case CV_BDF:
     cvSetBDF(cv_mem);
     break;
   }
   cv_mem.cv_rl1 = ONE / cv_mem.cv_l[1];
   cv_mem.cv_gamma = cv_mem.cv_h * cv_mem.cv_rl1;
   if (cv_mem.cv_nst == 0) cv_mem.cv_gammap = cv_mem.cv_gamma;
   cv_mem.cv_gamrat = (cv_mem.cv_nst > 0) ?
     cv_mem.cv_gamma / cv_mem.cv_gammap : ONE;  /* protect x / x != 1.0 */
 }
 
 /*
  * cvSetAdams
  *
  * This routine handles the computation of l and tq for the
  * case lmm == CV_ADAMS.
  *
  * The components of the array l are the coefficients of a
  * polynomial Lambda(x) = l_0 + l_1 x + ... + l_q x^q, given by
  *                          q-1
  * (d/dx) Lambda(x) = c * PRODUCT (1 + x / xi_i) , where
  *                          i=1
  *  Lambda(-1) = 0, Lambda(0) = 1, and c is a normalization factor.
  * Here xi_i = [t_n - t_(n-i)] / h.
  *
  * The array tq is set to test quantities used in the convergence
  * test, the error test, and the selection of h at a new order.
  */

 private void cvSetAdams(CVodeMemRec cv_mem)
 {
   double m[] = new double[L_MAX];
   double M[] = new double[3];
   double hsum;
   
   if (cv_mem.cv_q == 1) {
     cv_mem.cv_l[0] = cv_mem.cv_l[1] = cv_mem.cv_tq[1] = cv_mem.cv_tq[5] = ONE;
     cv_mem.cv_tq[2] = HALF;
     cv_mem.cv_tq[3] = ONE/TWELVE;
     cv_mem.cv_tq[4] = cv_mem.cv_nlscoef / cv_mem.cv_tq[2];       /* = 0.1 / tq[2] */
     return;
   }
   
   hsum = cvAdamsStart(cv_mem, m);
   
   M[0] = cvAltSum(cv_mem.cv_q-1, m, 1);
   M[1] = cvAltSum(cv_mem.cv_q-1, m, 2);
   
   cvAdamsFinish(cv_mem, m, M, hsum);
 }

 /*
  * cvAdamsStart
  *
  * This routine generates in m[] the coefficients of the product
  * polynomial needed for the Adams l and tq coefficients for q > 1.
  */

 private double cvAdamsStart(CVodeMemRec cv_mem, double m[])
 {
   double hsum, xi_inv, sum;
   int i, j;
   
   hsum = cv_mem.cv_h;
   m[0] = ONE;
   for (i=1; i <= cv_mem.cv_q; i++) m[i] = ZERO;
   for (j=1; j < cv_mem.cv_q; j++) {
     if ((j==cv_mem.cv_q-1) && (cv_mem.cv_qwait == 1)) {
       sum = cvAltSum(cv_mem.cv_q-2, m, 2);
       cv_mem.cv_tq[1] = cv_mem.cv_q * sum / m[cv_mem.cv_q-2];
     }
     xi_inv = cv_mem.cv_h / hsum;
     for (i=j; i >= 1; i--)
       m[i] += m[i-1] * xi_inv;
     hsum += cv_mem.cv_tau[j];
     /* The m[i] are coefficients of product(1 to j) (1 + x/xi_i) */
   }
   return(hsum);
 }

 /*
  * cvAdamsFinish
  *
  * This routine completes the calculation of the Adams l and tq.
  */

 private void cvAdamsFinish(CVodeMemRec cv_mem, double m[], double M[], double hsum)
 {
   int i;
   double M0_inv, xi, xi_inv;
   
   M0_inv = ONE / M[0];
   
   cv_mem.cv_l[0] = ONE;
   for (i=1; i <= cv_mem.cv_q; i++)
     cv_mem.cv_l[i] = M0_inv * (m[i-1] / i);
   xi = hsum / cv_mem.cv_h;
   xi_inv = ONE / xi;
   
   cv_mem.cv_tq[2] = M[1] * M0_inv / xi;
   cv_mem.cv_tq[5] = xi / cv_mem.cv_l[cv_mem.cv_q];

   if (cv_mem.cv_qwait == 1) {
     for (i=cv_mem.cv_q; i >= 1; i--)
       m[i] += m[i-1] * xi_inv;
     M[2] = cvAltSum(cv_mem.cv_q, m, 2);
     cv_mem.cv_tq[3] = M[2] * M0_inv / cv_mem.cv_L;
   }

   cv_mem.cv_tq[4] = cv_mem.cv_nlscoef / cv_mem.cv_tq[2];
 }

 /*  
  * cvAltSum
  *
  * cvAltSum returns the value of the alternating sum
  *   sum (i= 0 ... iend) [ (-1)^i * (a[i] / (i + k)) ].
  * If iend < 0 then cvAltSum returns 0.
  * This operation is needed to compute the integral, from -1 to 0,
  * of a polynomial x^(k-1) M(x) given the coefficients of M(x).
  */

 private double cvAltSum(int iend, double a[], int k)
 {
   int i, sign;
   double sum;
   
   if (iend < 0) return(ZERO);
   
   sum = ZERO;
   sign = 1;
   for (i=0; i <= iend; i++) {
     sum += sign * (a[i] / (i+k));
     sign = -sign;
   }
   return(sum);
 }

 /*
  * cvSetBDF
  *
  * This routine computes the coefficients l and tq in the case
  * lmm == CV_BDF.  cvSetBDF calls cvSetTqBDF to set the test
  * quantity array tq. 
  * 
  * The components of the array l are the coefficients of a
  * polynomial Lambda(x) = l_0 + l_1 x + ... + l_q x^q, given by
  *                                 q-1
  * Lambda(x) = (1 + x / xi*_q) * PRODUCT (1 + x / xi_i) , where
  *                                 i=1
  *  xi_i = [t_n - t_(n-i)] / h.
  *
  * The array tq is set to test quantities used in the convergence
  * test, the error test, and the selection of h at a new order.
  */

 private void cvSetBDF(CVodeMemRec cv_mem)
 {
   double alpha0, alpha0_hat, xi_inv, xistar_inv, hsum;
   int i,j;
   
   cv_mem.cv_l[0] = cv_mem.cv_l[1] = xi_inv = xistar_inv = ONE;
   for (i=2; i <= cv_mem.cv_q; i++) cv_mem.cv_l[i] = ZERO;
   alpha0 = alpha0_hat = -ONE;
   hsum = cv_mem.cv_h;
   if (cv_mem.cv_q > 1) {
     for (j=2; j < cv_mem.cv_q; j++) {
       hsum += cv_mem.cv_tau[j-1];
       xi_inv = cv_mem.cv_h / hsum;
       alpha0 -= ONE / j;
       for (i=j; i >= 1; i--)
         cv_mem.cv_l[i] += cv_mem.cv_l[i-1]*xi_inv;
       /* The l[i] are coefficients of product(1 to j) (1 + x/xi_i) */
     }
     
     /* j = q */
     alpha0 -= ONE / cv_mem.cv_q;
     xistar_inv = -cv_mem.cv_l[1] - alpha0;
     hsum += cv_mem.cv_tau[cv_mem.cv_q-1];
     xi_inv = cv_mem.cv_h / hsum;
     alpha0_hat = -cv_mem.cv_l[1] - xi_inv;
     for (i=cv_mem.cv_q; i >= 1; i--)
       cv_mem.cv_l[i] += cv_mem.cv_l[i-1]*xistar_inv;
   }

   cvSetTqBDF(cv_mem, hsum, alpha0, alpha0_hat, xi_inv, xistar_inv);
 }

 /*
  * cvSetTqBDF
  *
  * This routine sets the test quantity array tq in the case
  * lmm == CV_BDF.
  */

 private void cvSetTqBDF(CVodeMemRec cv_mem, double hsum, double alpha0,
                        double alpha0_hat, double xi_inv, double xistar_inv)
 {
   double A1, A2, A3, A4, A5, A6;
   double C, Cpinv, Cppinv;
   
   A1 = ONE - alpha0_hat + alpha0;
   A2 = ONE + cv_mem.cv_q * A1;
   cv_mem.cv_tq[2] = Math.abs(A1 / (alpha0 * A2));
   cv_mem.cv_tq[5] = Math.abs(A2 * xistar_inv / (cv_mem.cv_l[cv_mem.cv_q] * xi_inv));
   if (cv_mem.cv_qwait == 1) {
     if (cv_mem.cv_q > 1) {
       C = xistar_inv / cv_mem.cv_l[cv_mem.cv_q];
       A3 = alpha0 + ONE / cv_mem.cv_q;
       A4 = alpha0_hat + xi_inv;
       Cpinv = (ONE - A4 + A3) / A3;
       cv_mem.cv_tq[1] = Math.abs(C * Cpinv);
     }
     else cv_mem.cv_tq[1] = ONE;
     hsum += cv_mem.cv_tau[cv_mem.cv_q];
     xi_inv = cv_mem.cv_h / hsum;
     A5 = alpha0 - (ONE / (cv_mem.cv_q+1));
     A6 = alpha0_hat - xi_inv;
     Cppinv = (ONE - A6 + A5) / A2;
     cv_mem.cv_tq[3] = Math.abs(Cppinv / (xi_inv * (cv_mem.cv_q+2) * A5));
   }
   cv_mem.cv_tq[4] = cv_mem.cv_nlscoef / cv_mem.cv_tq[2];
 }

 /*
  * cvNls
  *
  * This routine attempts to solve the nonlinear system associated
  * with a single implicit step of the linear multistep method.
  * Depending on iter, it calls cvNlsFunctional or cvNlsNewton
  * to do the work.
  */

 private int cvNls(CVodeMemRec cv_mem, int nflag)
 {
   int flag = CV_SUCCESS;

   switch(cv_mem.cv_iter) {
   case CV_FUNCTIONAL:
     flag = cvNlsFunctional(cv_mem);
     break;
   case CV_NEWTON:
     flag = cvNlsNewton(cv_mem, nflag);
     break;
   }
   
   return(flag);

 }

 /*
  * cvNlsFunctional
  *
  * This routine attempts to solve the nonlinear system using 
  * functional iteration (no matrices involved).
  * 
  * This routine also handles the functional iteration of the
  * combined system (states + sensitivities) when sensitivities are 
  * computed using the CV_SIMULTANEOUS approach.
  *
  * Possible return values are:
  *
  *   CV_SUCCESS       ---> continue with error test
  *
  *   CV_RHSFUNC_FAIL  -+   
  *   CV_SRHSFUNC_FAIL -+-> halt the integration 
  *
  *   CONV_FAIL        -+
  *   RHSFUNC_RECVR     |-> predict again or stop if too many
  *   SRHSFUNC_RECVR   -+
  *
  */

 private int cvNlsFunctional(CVodeMemRec cv_mem)
 {
   int m;
   double del, delS, Del, Delp, dcon;
   int retval, is;
   boolean do_sensi_sim;
   NVector wrk1, wrk2;

   /* Are we computing sensitivities with the CV_SIMULTANEOUS approach? */
   do_sensi_sim = (cv_mem.cv_sensi && (cv_mem.cv_ism==CV_SIMULTANEOUS));

   /* Initialize counter and evaluate f at predicted y */
   cv_mem.cv_crate = ONE;
   m = 0;

   /* Initialize delS and Delp to avoid compiler warning message */
   delS = Delp = ZERO;

   retval = f(cv_mem.cv_tn, cv_mem.cv_zn[0],
                         cv_mem.cv_tempv, cv_mem.cv_user_data);
   cv_mem.cv_nfe++;
   if (retval < 0) return(CV_RHSFUNC_FAIL);
   if (retval > 0) return(RHSFUNC_RECVR);

   if (do_sensi_sim) {
     wrk1 = cv_mem.cv_ftemp;
     wrk2 = cv_mem.cv_ftempS[0];
     retval = cvSensRhsWrapper(cv_mem, cv_mem.cv_tn, cv_mem.cv_zn[0],
                               cv_mem.cv_tempv, cv_mem.cv_znS[0],
                               cv_mem.cv_tempvS, wrk1, wrk2);
     if (retval < 0) return(CV_SRHSFUNC_FAIL);
     if (retval > 0) return(SRHSFUNC_RECVR);
   }

   /* Initialize correction to zero */

   N_VConst_Serial(ZERO, cv_mem.cv_acor);
   if (do_sensi_sim) {
     for (is=0; is<cv_mem.cv_Ns; is++)
       N_VConst_Serial(ZERO, cv_mem.cv_acorS[is]);
   }

   /* Loop until convergence; accumulate corrections in acor */

   for(;;) {

     cv_mem.cv_nni++;

     /* Correct y directly from the last f value */

     N_VLinearSum_Serial(cv_mem.cv_h, cv_mem.cv_tempv, -ONE,
                  cv_mem.cv_zn[1], cv_mem.cv_tempv);
     N_VScale_Serial(cv_mem.cv_rl1, cv_mem.cv_tempv, cv_mem.cv_tempv);
     N_VLinearSum_Serial(ONE, cv_mem.cv_zn[0], ONE, cv_mem.cv_tempv, cv_mem.cv_y);

     if (do_sensi_sim)
       for (is=0; is<cv_mem.cv_Ns; is++) {
         N_VLinearSum_Serial(cv_mem.cv_h, cv_mem.cv_tempvS[is], -ONE,
                      cv_mem.cv_znS[1][is], cv_mem.cv_tempvS[is]);
         N_VScale_Serial(cv_mem.cv_rl1, cv_mem.cv_tempvS[is], cv_mem.cv_tempvS[is]);
         N_VLinearSum_Serial(ONE, cv_mem.cv_znS[0][is], ONE,
                      cv_mem.cv_tempvS[is], cv_mem.cv_yS[is]);
       }
     
     /* Get WRMS norm of current correction to use in convergence test */

     N_VLinearSum_Serial(ONE, cv_mem.cv_tempv, -ONE, cv_mem.cv_acor, cv_mem.cv_acor);
     if (do_sensi_sim)
       for (is=0; is<cv_mem.cv_Ns; is++)
         N_VLinearSum_Serial(ONE, cv_mem.cv_tempvS[is], -ONE,
                      cv_mem.cv_acorS[is], cv_mem.cv_acorS[is]);

     del = N_VWrmsNorm_Serial(cv_mem.cv_acor, cv_mem.cv_ewt);
     if (do_sensi_sim)
       delS = cvSensUpdateNorm(cv_mem, del, cv_mem.cv_acorS, cv_mem.cv_ewtS);

     N_VScale_Serial(ONE, cv_mem.cv_tempv, cv_mem.cv_acor);
     if (do_sensi_sim) 
       for (is=0; is<cv_mem.cv_Ns; is++)
         N_VScale_Serial(ONE, cv_mem.cv_tempvS[is], cv_mem.cv_acorS[is]);
     
     /* Test for convergence.  If m > 0, an estimate of the convergence
        rate constant is stored in crate, and used in the test. 

        Recall that, even when errconS=SUNFALSE, all variables are used in the
        convergence test. Hence, we use Del (and not del). However, acnrm
        is used in the error test and thus it has different forms
        depending on errconS (and this explains why we have to carry around
        del and delS)
     */
     
     Del = (do_sensi_sim) ? delS : del;
     if (m > 0) cv_mem.cv_crate = Math.max(CRDOWN * cv_mem.cv_crate, Del / Delp);
     dcon = Del * Math.min(ONE, cv_mem.cv_crate) / cv_mem.cv_tq[4];

     if (dcon <= ONE) {
       if (m == 0)
         if (do_sensi_sim && cv_mem.cv_errconS)
           cv_mem.cv_acnrm = delS;
         else
           cv_mem.cv_acnrm = del;
       else {
         cv_mem.cv_acnrm = N_VWrmsNorm_Serial(cv_mem.cv_acor, cv_mem.cv_ewt);
         if (do_sensi_sim && cv_mem.cv_errconS)
           cv_mem.cv_acnrm = cvSensUpdateNorm(cv_mem, cv_mem.cv_acnrm,
                                               cv_mem.cv_acorS, cv_mem.cv_ewtS);
       }
       return(CV_SUCCESS);  /* Convergence achieved */
     }

     /* Stop at maxcor iterations or if iter. seems to be diverging */

     m++;
     if ((m==cv_mem.cv_maxcor) || ((m >= 2) && (Del > RDIV * Delp)))
       return(CONV_FAIL);

     /* Save norm of correction, evaluate f, and loop again */

     Delp = Del;

     retval = f(cv_mem.cv_tn, cv_mem.cv_y,
                           cv_mem.cv_tempv, cv_mem.cv_user_data);
     cv_mem.cv_nfe++;
     if (retval < 0) return(CV_RHSFUNC_FAIL);
     if (retval > 0) return(RHSFUNC_RECVR);

     if (do_sensi_sim) {
       wrk1 = cv_mem.cv_ftemp;
       wrk2 = cv_mem.cv_ftempS[0];
       retval = cvSensRhsWrapper(cv_mem, cv_mem.cv_tn, cv_mem.cv_y,
                                 cv_mem.cv_tempv, cv_mem.cv_yS,
                                 cv_mem.cv_tempvS, wrk1, wrk2);
       if (retval < 0) return(CV_SRHSFUNC_FAIL);
       if (retval > 0) return(SRHSFUNC_RECVR);
     }  

   } /* end loop */

 }

 /*
  * cvNlsNewton
  *
  * This routine handles the Newton iteration. It calls lsetup if 
  * indicated, calls cvNewtonIteration to perform the iteration, and 
  * retries a failed attempt at Newton iteration if that is indicated.
  * See return values at top of this file.
  *
  * This routine also handles the Newton iteration of the combined 
  * system when sensitivities are computed using the CV_SIMULTANEOUS
  * approach. Since in that case we use a quasi-Newton on the 
  * combined system (by approximating the Jacobian matrix by its
  * block diagonal) and thus only solve linear systems with 
  * multiple right hand sides (all sharing the same coefficient
  * matrix - whatever iteration matrix we decide on) we set-up
  * the linear solver to handle N equations at a time.
  *
  * Possible return values:
  *
  *   CV_SUCCESS       ---> continue with error test
  *
  *   CV_RHSFUNC_FAIL  -+  
  *   CV_LSETUP_FAIL    |
  *   CV_LSOLVE_FAIL    |-> halt the integration 
  *   CV_SRHSFUNC_FAIL -+
  *
  *   CONV_FAIL        -+
  *   RHSFUNC_RECVR     |-> predict again or stop if too many
  *   SRHSFUNC_RECVR   -+
  *
  */

 private int cvNlsNewton(CVodeMemRec cv_mem, int nflag)
 {
   NVector vtemp1, vtemp2, vtemp3, wrk1, wrk2;
   int convfail, ier;
   boolean callSetup, do_sensi_sim;
   int retval, is;
   
   /* Are we computing sensitivities with the CV_SIMULTANEOUS approach? */
   do_sensi_sim = (cv_mem.cv_sensi && (cv_mem.cv_ism==CV_SIMULTANEOUS));

   vtemp1 = cv_mem.cv_acor;  /* rename acor as vtemp1 for readability  */
   vtemp2 = cv_mem.cv_y;     /* rename y as vtemp2 for readability     */
   vtemp3 = cv_mem.cv_tempv; /* rename tempv as vtemp3 for readability */
   
   /* Set flag convfail, input to lsetup for its evaluation decision */
   convfail = ((nflag == FIRST_CALL) || (nflag == PREV_ERR_FAIL)) ?
     CV_NO_FAILURES : CV_FAIL_OTHER;

   /* Decide whether or not to call setup routine (if one exists) */
   if (cv_mem.cv_lsetup_select > 0) {      
     callSetup = (nflag == PREV_CONV_FAIL) || (nflag == PREV_ERR_FAIL) ||
       (cv_mem.cv_nst == 0) ||
       (cv_mem.cv_nst >= cv_mem.cv_nstlp + MSBP) ||
       (Math.abs(cv_mem.cv_gamrat-ONE) > DGMAX);

     /* Decide whether to force a call to setup */
    if (cv_mem.cv_forceSetup) {
       callSetup = true;
       convfail = CV_FAIL_OTHER;
     }

   } else {  
     cv_mem.cv_crate = ONE;
     cv_mem.cv_crateS = ONE; /* if NO lsetup all conv. rates are set to ONE */
     callSetup = false;
   }
   
   /* Looping point for the solution of the nonlinear system.
      Evaluate f at the predicted y, call lsetup if indicated, and
      call cvNewtonIteration for the Newton iteration itself.      */

   for(;;) {

     retval = f(cv_mem.cv_tn, cv_mem.cv_zn[0],
                           cv_mem.cv_ftemp, cv_mem.cv_user_data);
     cv_mem.cv_nfe++; 
     if (retval < 0) return(CV_RHSFUNC_FAIL);
     if (retval > 0) return(RHSFUNC_RECVR);

     if (do_sensi_sim) {
       wrk1 = cv_mem.cv_tempv;
       wrk2 = cv_mem.cv_tempvS[0];
       retval = cvSensRhsWrapper(cv_mem, cv_mem.cv_tn, cv_mem.cv_zn[0],
                                 cv_mem.cv_ftemp, cv_mem.cv_znS[0],
                                 cv_mem.cv_ftempS, wrk1, wrk2);
       if (retval < 0) return(CV_SRHSFUNC_FAIL);
       if (retval > 0) return(SRHSFUNC_RECVR);
     }

     if (callSetup) {
       ier = cv_lsetup(cv_mem, convfail, cv_mem.cv_zn[0],
                               cv_mem.cv_ftemp, cv_mem.cv_jcur,
                               vtemp1, vtemp2, vtemp3, cv_mem.cv_lsetup_select);
       cv_mem.cv_nsetups++;
       callSetup = false;
       cv_mem.cv_forceSetup = false;
       cv_mem.cv_gamrat = ONE; 
       cv_mem.cv_gammap = cv_mem.cv_gamma;
       cv_mem.cv_crate = ONE;
       cv_mem.cv_crateS = ONE; /* after lsetup all conv. rates are reset to ONE */
       cv_mem.cv_nstlp = cv_mem.cv_nst;
       /* Return if lsetup failed */
       if (ier < 0) return(CV_LSETUP_FAIL);
       if (ier > 0) return(CONV_FAIL);
     }

     /* Set acor to zero and load prediction into y vector */
     N_VConst_Serial(ZERO, cv_mem.cv_acor);
     N_VScale_Serial(ONE, cv_mem.cv_zn[0], cv_mem.cv_y);

     if (do_sensi_sim)
       for (is=0; is<cv_mem.cv_Ns; is++) {
         N_VConst_Serial(ZERO, cv_mem.cv_acorS[is]);
         N_VScale_Serial(ONE, cv_mem.cv_znS[0][is], cv_mem.cv_yS[is]);
       }

     /* Do the Newton iteration */
       ier = cvNewtonIteration(cv_mem);

     /* If there is a convergence failure and the Jacobian-related 
        data appears not to be current, loop again with a call to lsetup
        in which convfail=CV_FAIL_BAD_J.  Otherwise return.                 */
     if (ier != TRY_AGAIN) return(ier);
     
     callSetup = true;
     convfail = CV_FAIL_BAD_J;
   }
 }
     
     int cv_lsetup(CVodeMemRec cv_mem, int convfail, NVector y, 
             NVector fy, boolean jcurPtr[], 
             NVector tmp1, NVector tmp2, NVector tmp3, int select) {
    	 int flag = 0;
    	 switch(select) {
    	 case cvDlsSetup_select:
    		 flag = cvDlsSetup(cv_mem, convfail, y, fy, jcurPtr,
    				 tmp1, tmp2, tmp3);
    	 }
    	 return flag;
     }
     
     /*-----------------------------------------------------------------
     cvDlsSetup
     -----------------------------------------------------------------
     This routine determines whether to update a Jacobian matrix (or
     use a stored version), based on heuristics regarding previous 
     convergence issues, the number of time steps since it was last
     updated, etc.; it then creates the system matrix from this, the
     'gamma' factor and the identity matrix, 
       A = I-gamma*J.
     This routine then calls the LS 'setup' routine with A.
     -----------------------------------------------------------------*/
   int cvDlsSetup(CVodeMemRec cv_mem, int convfail, NVector y, 
                  NVector fy, boolean jcurPtr[], 
                  NVector tmp1, NVector tmp2, NVector tmp3)
   {
     boolean jbad, jok;
     double dgamma;
     CVDlsMemRec cvdls_mem;
     int retval;

     /* Return immediately if cv_mem or cv_mem->cv_lmem are NULL */
     if (cv_mem == null) {
       cvProcessError(null, CVDLS_MEM_NULL, "CVSDLS", 
                       "cvDlsSetup", MSGD_CVMEM_NULL);
       return(CVDLS_MEM_NULL);
     }
     if (cv_mem.cv_lmem == null) {
       cvProcessError(cv_mem, CVDLS_LMEM_NULL, "CVSDLS", 
                       "cvDlsSetup", MSGD_LMEM_NULL);
       return(CVDLS_LMEM_NULL);
     }
     cvdls_mem = cv_mem.cv_lmem;

     /* Use nst, gamma/gammap, and convfail to set J eval. flag jok */
     dgamma = Math.abs((cv_mem.cv_gamma/cv_mem.cv_gammap) - ONE);
     jbad = (cv_mem.cv_nst == 0) || 
       (cv_mem.cv_nst > cvdls_mem.nstlj + CVD_MSBJ) ||
       ((convfail == CV_FAIL_BAD_J) && (dgamma < CVD_DGMAX)) ||
       (convfail == CV_FAIL_OTHER);
     jok = !jbad;
    
     /* If jok = SUNTRUE, use saved copy of J */
     if (jok) {
       jcurPtr[0] = false;
       // Copy savedJ to A
       retval = SUNMatCopy(cvdls_mem.savedJ, cvdls_mem.A);
       if (retval != 0) {
         cvProcessError(cv_mem, CVDLS_SUNMAT_FAIL, "CVSDLS", 
                         "cvDlsSetup",  MSGD_MATCOPY_FAILED);
         cvdls_mem.last_flag = CVDLS_SUNMAT_FAIL;
         return(-1);
       }

     /* If jok = SUNFALSE, call jac routine for new J value */
     } else {
       cvdls_mem.nje++;
       cvdls_mem.nstlj = cv_mem.cv_nst;
       jcurPtr[0] = true;
       retval = SUNMatZero(cvdls_mem.A);
       if (retval != 0) {
         cvProcessError(cv_mem, CVDLS_SUNMAT_FAIL, "CVSDLS", 
                         "cvDlsSetup",  MSGD_MATZERO_FAILED);
         cvdls_mem.last_flag = CVDLS_SUNMAT_FAIL;
         return(-1);
       }

       retval = Jac(cv_mem.cv_tn, y, fy, cvdls_mem.A, 
                               cvdls_mem.J_data, tmp1, tmp2, tmp3);
       if (retval < 0) {
         cvProcessError(cv_mem, CVDLS_JACFUNC_UNRECVR, "CVSDLS", 
                         "cvDlsSetup",  MSGD_JACFUNC_FAILED);
         cvdls_mem.last_flag = CVDLS_JACFUNC_UNRECVR;
         return(-1);
       }
       if (retval > 0) {
         cvdls_mem.last_flag = CVDLS_JACFUNC_RECVR;
         return(1);
       }

       retval = SUNMatCopy(cvdls_mem.A, cvdls_mem.savedJ);
       if (retval != 0) {
         cvProcessError(cv_mem, CVDLS_SUNMAT_FAIL, "CVSDLS", 
                         "cvDlsSetup",  MSGD_MATCOPY_FAILED);
         cvdls_mem.last_flag = CVDLS_SUNMAT_FAIL;
         return(-1);
       }

     }
     
     /* Scale and add I to get A = I - gamma*J */
     retval = SUNMatScaleAddI(-cv_mem.cv_gamma, cvdls_mem.A);
     if (retval != 0) {
       cvProcessError(cv_mem, CVDLS_SUNMAT_FAIL, "CVSDLS", 
                      "cvDlsSetup",  MSGD_MATSCALEADDI_FAILED);
       cvdls_mem.last_flag = CVDLS_SUNMAT_FAIL;
       return(-1);
     }

     /* Call generic linear solver 'setup' with this system matrix, and
        return success/failure flag */
     cvdls_mem.last_flag = SUNLinSolSetup_Dense(cvdls_mem.LS, cvdls_mem.A);
     return(cvdls_mem.last_flag);
   }
   
   private int SUNLinSolSetup_Dense(SUNLinearSolver S, double A[][])
   {
     double A_cols[][];
     int pivots[];
     int i,j;
     
     /* check for valid inputs */
     if ( (A == null) || (S == null) ) 
       return(SUNLS_MEM_NULL);
     
     
     /* access data pointers (return with failure on NULL) */
     A_cols = null;
     pivots = null;
     //A_cols = SUNDenseMatrix_Cols(A);
     // return SM_COLS_D(A)
     // // #define SM_COLS_D(A)        ( SM_CONTENT_D(A)->cols )
     A_cols = new double[A[0].length][A.length];
     for (i = 0; i < A.length; i++) {
    	 for (j = 0; j < A[0].length; j++) {
    		 A_cols[j][i] = A[i][j];
    	 }
     }
     pivots = S.pivots;
     if ( (A_cols == null) || (pivots == null) ) {
       S.last_flag = SUNLS_MEM_FAIL;
       return(S.last_flag);
     }
     
     /* perform LU factorization of input matrix */
     S.last_flag = denseGETRF(A_cols, A.length,
                              A[0].length, pivots);
     for (i = 0; i < A.length; i++) {
    	 for (j = 0; j < A[0].length; j++) {
    		 A[i][j] = A_cols[j][i];
    	 }
     }

     /* store error flag (if nonzero, this row encountered zero-valued pivod) */
     if (S.last_flag > 0)
       return(SUNLS_LUFACT_FAIL);
     return(SUNLS_SUCCESS);
   }
   
   private int denseGETRF(double a[][], int m, int n, int p[])
   {
     int i, j, k, l, q;
     // a is [n][m]
     // n column each with m row numbers
     double col_j[] = new double[m];
     double col_k[] = new double[m];
     double temp, mult, a_kj;

     /* k-th elimination step number */
     for (k=0; k < n; k++) {

         for (q = 0; q < m; q++) {
    	     col_k[q]  = a[k][q];
         }

       /* find l = pivot row number */
       l=k;
       for (i=k+1; i < m; i++)
         if (Math.abs(col_k[i]) > Math.abs(col_k[l])) l=i;
       p[k] = l;

       /* check for zero pivot element */
       if (col_k[l] == ZERO) return(k+1);
       
       /* swap a(k,1:n) and a(l,1:n) if necessary */    
       if ( l!= k ) {
         for (i=0; i<n; i++) {
           temp = a[i][l];
           a[i][l] = a[i][k];
           a[i][k] = temp;
         }
       }

       /* Scale the elements below the diagonal in
        * column k by 1.0/a(k,k). After the above swap
        * a(k,k) holds the pivot element. This scaling
        * stores the pivot row multipliers a(i,k)/a(k,k)
        * in a(i,k), i=k+1, ..., m-1.                      
        */
       mult = ONE/col_k[k];
       for(i=k+1; i < m; i++) col_k[i] *= mult;

       /* row_i = row_i - [a(i,k)/a(k,k)] row_k, i=k+1, ..., m-1 */
       /* row k is the pivot row after swapping with row l.      */
       /* The computation is done one column at a time,          */
       /* column j=k+1, ..., n-1.                                */

       for (j=k+1; j < n; j++) {
         for (q = 0; q < m; q++) {
             col_j[q] = a[j][q];
         }
         a_kj = col_j[k];

         /* a(i,j) = a(i,j) - [a(i,k)/a(k,k)]*a(k,j)  */
         /* a_kj = a(k,j), col_k[i] = - a(i,k)/a(k,k) */

         if (a_kj != ZERO) {
   	for (i=k+1; i < m; i++)
   	  col_j[i] -= a_kj * col_k[i];
         }
       }
     }

     /* return 0 to indicate success */

     return(0);
   }

   
   private int SUNMatCopy(double A[][], double B[][]) {
	   int i,j;
	   if ((A == null) || (B == null) || (A.length != B.length) || (A[0].length != B[0].length)) {
		   return 1;
	   }
	   for (i = 0; i < A.length; i++) {
		   for (j = 0; j < A[0].length; j++) {
			   B[i][j] = A[i][j];
		   }
	   }
	   return 0;
   }
   
   private int SUNMatZero(double A[][]) {
	   int i, j;
	   if (A == null) {
		   return 1;
	   }
	   for (i = 0; i < A.length; i++) {
		   for (j = 0; j < A[0].length; j++) {
			   A[i][j] = 0.0;
		   }
	   }
	   return 0;
   }
   
   private int SUNMatScaleAddI(double c, double A[][]) {
	   int i, j;
	   if ((A == null) || (A.length != A[0].length)) {
		   return 1;
	   }
	   for (i = 0; i < A.length; i++) {
		   for (j = 0; j < A[0].length; j++) {
			   A[i][j] = c * A[i][j];
			   if (i == j) {
				   A[i][j] = A[i][j] + 1.0;
			   }
		   }
	   }
	   return 0;
   }

   /*
    * cvNewtonIteration
    *
    * This routine performs the Newton iteration. If the iteration succeeds,
    * it returns the value CV_SUCCESS. If not, it may signal the cvNlsNewton 
    * routine to call lsetup again and reattempt the iteration, by
    * returning the value TRY_AGAIN. (In this case, cvNlsNewton must set 
    * convfail to CV_FAIL_BAD_J before calling setup again). 
    * Otherwise, this routine returns one of the appropriate values 
    * CV_LSOLVE_FAIL, CV_RHSFUNC_FAIL, CV_SRHSFUNC_FAIL, CONV_FAIL,
    * RHSFUNC_RECVR, or SRHSFUNC_RECVR back to cvNlsNewton.
    *
    * If sensitivities are computed using the CV_SIMULTANEOUS approach, this
    * routine performs a quasi-Newton on the combined nonlinear system.
    * The iteration matrix of the combined system is block diagonal with
    * each block being the iteration matrix of the original system. Thus
    * we solve linear systems with the same matrix but different right
    * hand sides.
    */

   private int cvNewtonIteration(CVodeMemRec cv_mem)
   {
     int m;
     double del, delS, Del, Delp, dcon;
     NVector bS[] = null;
     NVector b, wrk1, wrk2;
     boolean do_sensi_sim;
     int retval, is;
     
     /* Are we computing sensitivities with the CV_SIMULTANEOUS approach? */
     do_sensi_sim = (cv_mem.cv_sensi && (cv_mem.cv_ism==CV_SIMULTANEOUS));

     cv_mem.cv_mnewt = m = 0;

     /* Initialize delS and Delp to avoid compiler warning message */
     delS = Delp = ZERO;

     /* At this point, ftemp  <- f(t_n, y_n(0))
        ftempS <- fS(t_n, y_n(0), s_n(0))
        acor   <- 0
        acorS  <- 0
        y      <- y_n(0)
        yS     <- yS_n(0)                 */

     /* Looping point for Newton iteration */
     for(;;) {
       
       /* Evaluate the residual of the nonlinear system */
       N_VLinearSum_Serial(cv_mem.cv_rl1, cv_mem.cv_zn[1], ONE,
                    cv_mem.cv_acor, cv_mem.cv_tempv);
       N_VLinearSum_Serial(cv_mem.cv_gamma, cv_mem.cv_ftemp, -ONE,
                    cv_mem.cv_tempv, cv_mem.cv_tempv);

       /* Call the lsolve function */
       b = cv_mem.cv_tempv;
       /*retval = cv_mem->cv_lsolve(cv_mem, b, cv_mem.cv_ewt,
                                  cv_mem.cv_y, cv_mem.cv_ftemp); 
       cv_mem->cv_nni++;

       if (retval < 0) return(CV_LSOLVE_FAIL);*/
       
       /* If lsolve had a recoverable failure and Jacobian data is
          not current, signal to try the solution again            */
      /* if (retval > 0) { 
         if ((!cv_mem->cv_jcur) && (cv_mem->cv_lsetup))
           return(TRY_AGAIN);
         else
           return(CONV_FAIL);
       }*/

       /* Solve the sensitivity linear systems and do the same 
          tests on the return value of lsolve. */
    
      /* if (do_sensi_sim) {

         for (is=0; is<cv_mem->cv_Ns; is++) {
           N_VLinearSum(cv_mem->cv_rl1, cv_mem->cv_znS[1][is], ONE,
                        cv_mem->cv_acorS[is], cv_mem->cv_tempvS[is]);
           N_VLinearSum(cv_mem->cv_gamma, cv_mem->cv_ftempS[is], -ONE,
                        cv_mem->cv_tempvS[is], cv_mem->cv_tempvS[is]);
         }
         bS = cv_mem->cv_tempvS;
         for (is=0; is<cv_mem->cv_Ns; is++) {
           retval = cv_mem->cv_lsolve(cv_mem, bS[is], cv_mem->cv_ewtS[is],
                                      cv_mem->cv_y, cv_mem->cv_ftemp);
           if (retval < 0) return(CV_LSOLVE_FAIL);
           if (retval > 0) { 
             if ((!cv_mem->cv_jcur) && (cv_mem->cv_lsetup))
               return(TRY_AGAIN);
             else
               return(CONV_FAIL);
           }
         }
       }*/
       
       /* Get WRMS norm of correction; add correction to acor and y */

       /*del = N_VWrmsNorm(b, cv_mem->cv_ewt);
       N_VLinearSum(ONE, cv_mem->cv_acor, ONE, b, cv_mem->cv_acor);
       N_VLinearSum(ONE, cv_mem->cv_zn[0], ONE, cv_mem->cv_acor, cv_mem->cv_y);

       if (do_sensi_sim) {
         delS = cvSensUpdateNorm(cv_mem, del, bS, cv_mem->cv_ewtS);
         for (is=0; is<cv_mem->cv_Ns; is++) {
           N_VLinearSum(ONE, cv_mem->cv_acorS[is], ONE,
                        bS[is], cv_mem->cv_acorS[is]);
           N_VLinearSum(ONE, cv_mem->cv_znS[0][is], ONE,
                        cv_mem->cv_acorS[is], cv_mem->cv_yS[is]);
         }
       }*/

       /* Test for convergence.  If m > 0, an estimate of the convergence
          rate constant is stored in crate, and used in the test.        */

       /*Del = (do_sensi_sim) ? delS : del;
       if (m > 0)
         cv_mem->cv_crate = SUNMAX(CRDOWN * cv_mem->cv_crate, Del/Delp);
       dcon = Del * SUNMIN(ONE, cv_mem->cv_crate) / cv_mem->cv_tq[4];
       
       if (dcon <= ONE) {
         if (m == 0)
           if (do_sensi_sim && cv_mem->cv_errconS)
             cv_mem->cv_acnrm = delS;
           else
             cv_mem->cv_acnrm = del;
         else {
           cv_mem->cv_acnrm = N_VWrmsNorm(cv_mem->cv_acor, cv_mem->cv_ewt);
           if (do_sensi_sim && cv_mem->cv_errconS)
             cv_mem->cv_acnrm = cvSensUpdateNorm(cv_mem, cv_mem->cv_acnrm,
                                                 cv_mem->cv_acorS, cv_mem->cv_ewtS);
         }
         cv_mem->cv_jcur = SUNFALSE;
         return(CV_SUCCESS); */ /* Convergence achieved */
       /*}

       cv_mem->cv_mnewt = ++m;*/
       
       /* Stop at maxcor iterations or if iter. seems to be diverging.
          If still not converged and Jacobian data is not current, 
          signal to try the solution again                            */
       /*if ((m == cv_mem->cv_maxcor) || ((m >= 2) && (Del > RDIV * Delp))) {
         if ((!cv_mem->cv_jcur) && (cv_mem->cv_lsetup))
           return(TRY_AGAIN);
         else
           return(CONV_FAIL);
       }*/
       
       /* Save norm of correction, evaluate f, and loop again */
      /* Delp = Del;
       retval = cv_mem->cv_f(cv_mem->cv_tn, cv_mem->cv_y,
                             cv_mem->cv_ftemp, cv_mem->cv_user_data);
       cv_mem->cv_nfe++;
       if (retval < 0) return(CV_RHSFUNC_FAIL);
       if (retval > 0) {
         if ((!cv_mem->cv_jcur) && (cv_mem->cv_lsetup))
           return(TRY_AGAIN);
         else
           return(RHSFUNC_RECVR);
       }

       if (do_sensi_sim) {
         wrk1 = cv_mem->cv_tempv;
         wrk2 = cv_mem->cv_tempvS[0];
         retval = cvSensRhsWrapper(cv_mem, cv_mem->cv_tn, cv_mem->cv_y,
                                   cv_mem->cv_ftemp, cv_mem->cv_yS,
                                   cv_mem->cv_ftempS, wrk1, wrk2);
         if (retval < 0) return(CV_SRHSFUNC_FAIL);
         if (retval > 0) {
           if ((!cv_mem->cv_jcur) && (cv_mem->cv_lsetup))
             return(TRY_AGAIN);
           else
             return(SRHSFUNC_RECVR);
         }
       }*/

     } /* end loop */

   }


}