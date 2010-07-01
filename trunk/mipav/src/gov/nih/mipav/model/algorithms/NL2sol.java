package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

public abstract class NL2sol {
	
	public NL2sol() {
	
	}
	
	// emin, the smallest exponent E for double precision, is I1MACH(15)
    // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022) = 2.225073858507201E-308
    // D1MACH(1) is the smallest normalized number, which preserves the
    // full precision of the mantissa.
    // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number = 4.9E-324,
    // which preserves only a portion of the fraction's precision.
	/** 2**-1022 = D1MACH(1). */
    private double tiny = Math.pow(2, -1022);
    
    // epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.224460e-16
    // epsilon is called the largest relative spacing
    private double epsilon = Math.pow(2, -52);
    private double huge = Double.MAX_VALUE;
	double sqteta_dotprd = 0.0;
	
	double dgxfac_gqtstp = 0.0;
	
	double rktol_qrfact = 0.0;
	double ufeta_qrfact = 0.0;
	
	double sqteta_v2norm = 0.0;
	
	private void assess(double d[], int iv[], int p, double step[], double stlstg[], 
			            double v[], double x[], double x0[]) {
		/***********************************************************************
		!
		!! ASSESS assesses a candidate step.
		!
		!  Discussion:
		!
		!    This subroutine is called by an unconstrained minimization
		!    routine to assess the next candidate step.  It may recommend one
		!    of several courses of action, such as accepting the step, 
		!    recomputing it using the same or a new quadratic model, or 
		!    halting due to convergence or false convergence.  See the return 
		!    code listing below.
		!
		!    This routine is called as part of the NL2SOL (nonlinear
		!    least-squares) package.  It may be used in any unconstrained
		!    minimization solver that uses dogleg, Goldfeld-Quandt-Trotter,
		!    or Levenberg-Marquardt steps.
		!
		!    See Dennis, Gay and Welsch for further discussion of the assessing 
		!    and model switching strategies.  While NL2SOL considers only two 
		!    models, ASSESS is designed to handle any number of models.
		!
		!    On the first call of an iteration, only the I/O variables
		!    step, X, IV(IRC), IV(MODEL), V(F), V(DSTNRM), V(GTSTEP), and
		!    V(PREDUC) need have been initialized.  Between calls, no I/O
		!    values execpt STEP, X, IV(MODEL), V(G) and the stopping tolerances 
		!    should be changed.
		!
		!    After a return for convergence or false convergence, one can
		!    change the stopping tolerances and call ASSESS again, in which
		!    case the stopping tests will be repeated.
		!
		!  Modified:
		!
		!    04 April 2006
		!
		!  Author:
		!
		!    David Gay
		!
		!  Reference:
		!
		!    John Dennis, David Gay, Roy Welsch,
		!    An Adaptive Nonlinear Least Squares Algorithm,
		!    ACM Transactions on Mathematical Software,
		!    Volume 7, Number 3, 1981.
		!
		!    M J D Powell,
		!    A FORTRAN Subroutine for Solving Systems of Nonlinear Algebraic Equations,
		!    in Numerical Methods for Nonlinear Algebraic Equations, 
		!    edited by Philip Rabinowitz, 
		!    Gordon and Breach, London, 1970.
		!
		!  Parameters:
		!
		!     iv (i/o) integer parameter and scratch vector -- see description
		!             below of iv values referenced.
		!
		!    Input, real D(P), a scale vector used in computing V(RELDX).
		!
		!    Input, integer P, the number of parameters being optimized.
		!
		!   step (i/o) on input, step is the step to be assessed.  it is un-
		!             changed on output unless a previous step achieved a
		!             better objective function reduction, in which case stlstg
		!             will have been copied to step.
		!
		! stlstg (i/o) when assess recommends recomputing step even though the
		!             current (or a previous) step yields an objective func-
		!             tion decrease, it saves in stlstg the step that gave the
		!             best function reduction seen so far (in the current itera-
		!             tion).  if the recomputed step yields a larger function
		!             value, then step is restored from stlstg and
		!             x = x0 + step is recomputed.
		!
		!      v (i/o) real parameter and scratch vector -- see description
		!             below of v values referenced.
		!
		!      x (i/o) on input, x = x0 + step is the point at which the objec-
		!             tive function has just been evaluated.  if an earlier
		!             step yielded a bigger function decrease, then x is
		!             restored to the corresponding earlier value.  otherwise,
		!             if the current step does not give any function decrease,
		!             then x is restored to x0.
		!
		!     x0 (in)  initial objective function parameter vector (at the
		!             start of the current iteration).
		!
		!  iv values referenced
		!
		!    iv(irc) (i/o) on input for the first step tried in a new iteration,
		!             iv(irc) should be set to 3 or 4 (the value to which it is
		!             set when step is definitely to be accepted).  on input
		!             after step has been recomputed, iv(irc) should be
		!             unchanged since the previous return of assess.
		!                on output, iv(irc) is a return code having one of the
		!             following values...
		!                  1 = switch models or try smaller step.
		!                  2 = switch models or accept step.
		!                  3 = accept step and determine v(radfac) by gradient
		!                       tests.
		!                  4 = accept step, v(radfac) has been determined.
		!                  5 = recompute step (using the same model).
		!                  6 = recompute step with radius = v(lmax0) but do not
		!                       evaulate the objective function.
		!                  7 = x-convergence (see v(xctol)).
		!                  8 = relative function convergence (see v(rfctol)).
		!                  9 = both x- and relative function convergence.
		!                 10 = absolute function convergence (see v(afctol)).
		!                 11 = singular convergence (see v(lmax0)).
		!                 12 = false convergence (see v(xftol)).
		!                 13 = iv(irc) was out of range on input.
		!             return code i has precdence over i+1 for i = 9, 10, 11.
		! iv(mlstgd) (i/o) saved value of iv(model).
		!  iv(model) (i/o) on input, iv(model) should be an integer identifying
		!             the current quadratic model of the objective function.
		!             if a previous step yielded a better function reduction,
		!             then iv(model) will be set to iv(mlstgd) on output.
		! iv(nfcall) (in)  invocation count for the objective function.
		! iv(nfgcal) (i/o) value of iv(nfcall) at step that gave the biggest
		!             function reduction this iteration.  iv(nfgcal) remains
		!             unchanged until a function reduction is obtained.
		! iv(radinc) (i/o) the number of radius increases (or minus the number
		!             of decreases) so far this iteration.
		! iv(restor) (out) set to 0 unless x and v(f) have been restored, in
		!             which case assess sets iv(restor) = 1.
		!  iv(stage) (i/o) count of the number of models tried so far in the
		!             current iteration.
		! iv(stglim) (in)  maximum number of models to consider.
		! iv(switch) (out) set to 0 unless a new model is being tried and it
		!             gives a smaller function value than the previous model,
		!             in which case assess sets iv(switch) = 1.
		! iv(toobig) (in)  is nonzero if step was too big (e.g. if it caused
		!             overflow).
		!   iv(xirc) (i/o) value that iv(irc) would have in the absence of
		!             convergence, false convergence, and oversized steps.
		!
		!  v values referenced
		!
		! v(afctol) (in)  absolute function convergence tolerance.  if the
		!             absolute value of the current function value v(f) is less
		!             than v(afctol), then assess returns with iv(irc) = 10.
		! v(decfac) (in)  factor by which to decrease radius when iv(toobig) is
		!             nonzero.
		! v(dstnrm) (in)  the 2-norm of d * step.
		! v(dstsav) (i/o) value of v(dstnrm) on saved step.
		!   v(dst0) (in)  the 2-norm of d times the Newton step (when defined,
		!             i.e., for v(nreduc) >= 0).
		!      v(f) (i/o) on both input and output, v(f) is the objective func-
		!             tion value at x.  if x is restored to a previous value,
		!             then v(f) is restored to the corresponding value.
		!   v(fdif) (out) the function reduction v(f0) - v(f) (for the output
		!             value of v(f) if an earlier step gave a bigger function
		!             decrease, and for the input value of v(f) otherwise).
		! v(flstgd) (i/o) saved value of v(f).
		!     v(f0) (in)  objective function value at start of iteration.
		! v(gtslst) (i/o) value of v(gtstep) on saved step.
		! v(gtstep) (in)  inner product between step and gradient.
		! v(incfac) (in)  minimum factor by which to increase radius.
		!  v(lmax0) (in)  maximum reasonable step size (and initial step bound).
		!             if the actual function decrease is no more than twice
		!             what was predicted, if a return with iv(irc) = 7, 8, 9,
		!             or 10 does not occur, if v(dstnrm) > v(lmax0), and if
		!             v(preduc) <= v(rfctol) * abs(v(f0)), then assess re-
		!             turns with iv(irc) = 11.  if so doing appears worthwhile,
		!             then assess repeats this test with v(preduc) computed for
		!             a step of length v(lmax0) (by a return with iv(irc) = 6).
		! v(nreduc) (i/o)  function reduction predicted by quadratic model for
		!             Newton step.  if assess is called with iv(irc) = 6, i.e.,
		!             if v(preduc) has been computed with radius = v(lmax0) for
		!             use in the singular convervence test, then v(nreduc) is
		!             set to -v(preduc) before the latter is restored.
		! v(plstgd) (i/o) value of v(preduc) on saved step.
		! v(preduc) (i/o) function reduction predicted by quadratic model for
		!             current step.
		! v(radfac) (out) factor to be used in determining the new radius,
		!             which should be v(radfac)*dst, where  dst  is either the
		!             output value of v(dstnrm) or the 2-norm of
		!             diag(newd) * step  for the output value of step and the
		!             updated version, newd, of the scale vector d.  for
		!             iv(irc) = 3, v(radfac) = 1.0 is returned.
		! v(rdfcmn) (in)  minimum value for v(radfac) in terms of the input
		!             value of v(dstnrm) -- suggested value = 0.1.
		! v(rdfcmx) (in)  maximum value for v(radfac) -- suggested value = 4.0.
		!  v(reldx) (out) scaled relative change in x caused by step, computed
		!             by function  reldst  as
		!                 max (d(i)*abs(x(i)-x0(i)), 1 <= i <= p) /
		!                    max (d(i)*(abs(x(i))+abs(x0(i))), 1 <= i <= p).
		!             if an acceptable step is returned, then v(reldx) is com-
		!             puted using the output (possibly restored) values of x
		!             and step.  otherwise it is computed using the input
		!             values.
		! v(rfctol) (in)  relative function convergence tolerance.  if the
		!             actual function reduction is at most twice what was pre-
		!             dicted and  v(nreduc) <= v(rfctol)*abs(v(f0)),  then
		!             assess returns with iv(irc) = 8 or 9.  see also v(lmax0).
		! v(STPPAR) (in)  Marquardt parameter -- 0 means full Newton step.
		! v(tuner1) (in)  tuning constant used to decide if the function
		!             reduction was much less than expected.  suggested
		!             value = 0.1.
		! v(tuner2) (in)  tuning constant used to decide if the function
		!             reduction was large enough to accept step.  suggested
		!             value = 10**-4.
		! v(tuner3) (in)  tuning constant used to decide if the radius
		!             should be increased.  suggested value = 0.75.
		!  v(xctol) (in)  x-convergence criterion.  if step is a Newton step
		!             (v(STPPAR) = 0) having v(reldx) <= v(xctol) and giving
		!             at most twice the predicted function decrease, then
		!             assess returns iv(irc) = 7 or 9.
		!  v(xftol) (in)  false convergence tolerance.  if step gave no or only
		!             a small function decrease and v(reldx) <= v(xftol),
		!             then assess returns with iv(irc) = 12.
		*/
		final int afctol = 31;
		final int decfac = 22;
		double emax;
		boolean goodx;
		double gts;
		int i;
		int j;
		final int irc = 3;
		final int lmax0 = 35;
		int nfc;
		final int nreduc = 6;
		final int plstgd = 15;
		final int preduc = 7;
		final int radfac = 16;
		final int rdfcmn = 24;
		final int rdfcmx = 25;
		final int reldx = 17;
		double reldx1;
		double rfac1;
		final int rfctol = 32;
		final int stppar = 5;
		final int tuner1 = 26;
		final int tuner2 = 27;
		final int tuner3 = 28;
		final int xctol = 33;
		final int xftol = 34;
		final int xirc = 13;
		double xmax;
		
		// subscripts for iv and v
		final int mlstgd = 4;
		final int model = 5;
		final int nfcall = 6;
		final int nfgcal = 7;
		final int radinc = 8;
		final int restor = 9;
		final int stage = 10;
		final int stglim = 11;
		final int switchConstant = 12;
		final int toobig = 2;
		final int dstnrm = 2;
		final int dst0 = 3;
		final int dstsav = 18;
		final int f = 10;
		final int fdif = 11;
		final int flstgd = 12;
		final int f0 = 13;
		final int gtslst = 14;
		final int gtstep = 4;
		final int incfac = 23;
		
		boolean do10 = false;
		boolean do20 = false;
		boolean do30 = false;
		boolean do40 = false;
		boolean do50 = false;
		boolean do70 = false;
		boolean do80 = false;
		boolean do90 = false;
		boolean do140 = false;
		boolean do160 = false;
		boolean do200 = false;
		boolean do230 = false;
		boolean do260 = false;
		boolean do290 = false;
		boolean do300 = false;
		boolean do310 = false;
		boolean do360 = false;
		
		
		nfc = iv[nfcall];
		iv[switchConstant] = 0;
		iv[restor] = 0;
		rfac1 = 1.0;
		goodx = true;
		i = iv[irc];
		
		if ((i < 1) || (i > 12)) {
			iv[irc] = 13;
			return;
		}
		
	    switch(i) {
		    case 1:
		    	do20 = true;
		    	break;
		    case 2:
		    	do30 = true;
		    	break;
		    case 3:
		    case 4:
		    	do10 = true;
		    	break;
		    case 5:
		    	do40 = true;
		    	break;
		    case 6:
		    	do360 = true;
		    	break;
		    case 7:
		    case 8:
		    case 9:
		    case 10:
		    case 11:
		    	do290 = true;
		    	break;
		    case 12:
		    	do140 = true; 
	    } // switch(i)
	    
	    while (true) {
	    	
	    	if (do10) {
	    		// Initialize for new iteration
	    		do10 = false;
	    	    iv[stage] = 1;
	    	    iv[radinc] = 0;
	    	    v[flstgd] = v[f0];
	    	    
	    	    if (iv[toobig] != 0) {
	    	    	iv[stage] = -1;
	    	    	iv[xirc] = i;
	    	    	v[radfac] = v[decfac];
	    	    	iv[radinc] = iv[radinc] - 1;
	    	    	iv[irc] = 5;
	    	    	return;
	    	    }
	    	    do90 = true;
	    	} // if (do10)
	    	
	    	if (do20) {
	    		do20 = false;
	    		// Step was recomputed with new model or smaller radius.
	    		// First decide which
	    		
	    		// Old model retained, smaller radius tried.
	    		// Do not consider any more new models this iteration.
	    		
	    		if (iv[model] == iv[mlstgd]) {
	    			iv[stage] = iv[stglim];
	    			iv[radinc] = -1;
	    			do90 = true;
	    		} // if (iv[model] == iv[mlstgd])
	    		do30 = true;
	    	} // if (do20)
	    	
	    	if (do30) {
	    		do30 = false;
	    		// A new model is being tried.  Decide whether to keep it.
	    		iv[stage] = iv[stage] + 1;
	    		do40 = true;
	    	} // if (do30)
	    	
	    	if (do40) {
	    		do40 = false;
	    		// Now we add the possibility that step was recomputed with
	    		// the same model, perhaps because of an oversized step.
	    		if (iv[stage] > 0) {
	    			do50 = true;
	    		}
	    		else if (iv[toobig] != 0) {
	    			v[radfac] = v[decfac];
	    			iv[radinc] = iv[radinc] - 1;
	    			iv[irc] = 5;
	    			return;
	    		} // else if (iv[toobig] != 0)
	    		else {
	    			// Restore iv[stage] adn pick up where we left off.
	    			iv[stage] = -iv[stage];
	    			i = iv[xirc];
	    			switch(i) {
		    			case 1:
		    				do20 = true;
		    				break;
		    			case 2:
		    				do30 = true;
		    				break;
		    			case 3:
		    			case 4:
		    				do90 = true;
		    				break;
		    			case 5:
		    				do70 = true;
		    				break;
	    			    default:
	    			    	do50 = true;
	    			} // switch(i)
	    		}// else
	    	} // if (do40)
	    	
	    	if (do50) {
	    		do50 = false;
	    		if (iv[toobig] == 0) {
	    			do70 = true;
	    		}
	    		else if (iv[radinc] <= 0) {
	    			// Handle oversize step.
	    			iv[stage] = -iv[stage];
	    			iv[xirc] = iv[irc];
	    			v[radfac] = v[decfac];
	    			iv[radinc] = iv[radinc] - 1;
	    			iv[irc] = 5;
	    			return;
	    		} // else if (iv[radinc] <= 0)
	    		else {
	    			do80 = true;
	    		}
	    	} // if (do50)
	    	
	    	if (do70) {
	    		do70 = false;
	    		if (v[f] < v[flstgd]) {
	    			do90 = true;
	    		}
	    		// The new step is a loser.  Restore old model.
	    		else {
	    			do80 = true;
	    			if (iv[model] != iv[mlstgd]) {
	    				iv[model] = iv[mlstgd];
	    				iv[switchConstant] = 1;
	    			}
	    		}
	    	} // if (do70)
	    	
	    	if (do80) {
	    		do80 = false;
	    		do90 = true;
	    		// Restore step, etc. only if a previous step decreased V(F).
	    		if (v[flstgd] < v[f0]) {
	                iv[restor] = 1;
	                v[f] = v[flstgd];
	                v[preduc] = v[plstgd];
	                v[gtstep] = v[gtslst];
	                if (iv[switchConstant] == 0) {
	                	rfac1 = v[dstnrm]/v[dstsav];
	                }
	                v[dstnrm] = v[dstsav];
	                nfc = iv[nfgcal];
	                goodx = false;
	    		} // if (v[flstgd] < v[f0])
	    	} // if (do80)
	    	
	    	if (do90) {
	    		do90 = false;
	    		// Compute relative change in X by current step.
	    		reldx1 = reldst(p, d, x, x0);
	    		
	    		// Restore X and STEP if necessary.
	    		if (!goodx) {
	    		    for (j = 1; j <= p; j++) {
	    		    	step[j] = stlstg[j];
	    		    	x[j] = x0[j] + stlstg[j];
	    		    }
	    		} // if (!goodx)
	    		
	    		v[fdif] = v[f0] - v[f];
	    		
	    		// No (or only a trivial) function decrease,
	    		// so try new model or smaller radius.
	    		if (v[fdif] <= v[tuner2] * v[preduc]) {
	    			v[reldx] = reldx1;
	    			
	    			if (v[f0] <= v[f]) {
	    				iv[mlstgd] = iv[model];
	    				v[flstgd] = v[f];
	    				v[f] = v[f0];
	    				for (j = 1; j <= p; j++) {
	    					x[j] = x0[j];
	    				}
	    				iv[restor] = 1;
	    			} // if (v[f0] <= v[f])
	    			else {
	    				iv[nfgcal] = nfc;
	    			}
	    			
	    			iv[irc] = 1;
	    			if (iv[stglim] <= iv[stage]) {
	    				iv[irc] = 5;
	    				iv[radinc] = iv[radinc] - 1;
	    			} // if (iv[stglim] <= iv[stage] 
	    		} // if (v[fdif] <= v[tuner2] * v[preduc])
	    		else {
	    			// Nontrivial function decrease achieved.
	    			iv[nfgcal] = nfc;
	    			rfac1 = 1.0;
	    			if (goodx) {
	    				v[reldx] = reldx1;
	    			}
	    			v[dstsav] = v[dstnrm];
	    			
	    			if (v[preduc] * v[tuner1] < v[fdif]) {
	    				do200 = true;
	    			}
	    			// Decrease was much less than predicted: either change models
	    			// or accept step with decreased radius.
	    			else if (iv[stage] < iv[stglim]) {
	    				iv[irc] = 2;
	    			}
	    			else {
	    				iv[irc] = 4;
	    			}
	    		} // else
	    		if (!do200) {
	    			do140 = true;
	    			// Set V[radfac] to Fletcher's decrease factor.
	    			iv[xirc] = iv[irc];
	    			emax = v[gtstep] + v[fdif];
	    			v[radfac] = 0.5 * rfac1;
	    			
	    			if (emax < v[gtstep]) {
	    				v[radfac] = rfac1 * Math.max(v[rdfcmn], 0.5 * v[gtstep] / emax);
	    			}
	    		} // if (!do200)
	    	} // if (do90)
	    	
	    	if (do140) {
	    		do140 = false;
	    		// Do a false convergence test
	    		if (v[reldx] < v[xftol]) {
	    			do160 = true;
	    		}
	    		else {
	    			iv[irc] = iv[xirc];
	    			if (v[f] < v[f0]) {
	    				do230 = true;
	    			}
	    			else {
	    				do300 = true;
	    			}
	    		} // else
	    	} // if (do140)
	    	
	    	if (do160) {
	    		do160 = false;
	    		iv[irc] = 12;
	    		do310 = true;
	    	} // if (do160)
	    	
	    	if (do200) {
	    		do200 = false;
	    		// Handle good function decrease,
	    		if (v[fdif] < (-v[tuner3] * v[gtstep])) {
	    			do260 = true;
	    		}
	    		// Increasing radius looks worthwhile.  See if we just
	    		// recomputed step with a decreased radius or restored step
	    		// after recomputing it with a larger radius.
	    		else if (iv[radinc] < 0) {
	    			do260 = true;
	    		}
	    		else if (iv[restor] == 1) {
	    			do260 = true;
	    		}
	    		else {
	    			// We did not.  Try a longer step unless this was a Newton step.
	    			v[radfac] = v[rdfcmx];
	    			gts = v[gtstep];
	    			if (v[fdif] < (0.5/v[radfac] - 1.0) * gts) {
	    				v[radfac] = Math.max(v[incfac], 0.5 * gts/(gts + v[fdif]));
	    			}
	    			iv[irc] = 4;
	    			
	    			if (v[stppar] == 0.0) {
	    				do300 = true;
	    			}
	    			else {
	    				do230 = true;
	    				// Step was not a Newton step.  Recompute it with a larger radius.
	    				iv[irc] = 5;
	    				iv[radinc] = iv[radinc] + 1;
	    			}
	    		} // else
	    	} // if (do200)
	    	
	    	if (do230) {
	    		do230 = false;
	    	    // Save values corresponding to good step.
	    		v[flstgd] = v[f];
	    		iv[mlstgd] = iv[model];
	    		for (j = 1; j <= p; j++) {
	    			stlstg[j] = step[j];
	    		}
	    		v[dstsav] = v[dstnrm];
	    		iv[nfgcal] = nfc;
	    		v[plstgd] = v[preduc];
	    		v[gtslst] = v[gtstep];
	    		do300 = true;
	    	} // if (do230)
	    	
	    	if (do260) {
	    		do260 = false;
	    		// Accept step with radius unchanged.
	    		v[radfac] = 1.0;
	    		iv[irc] = 3;
	    		do300 = true;
	    	} // if (do260)
	    	
	    	if (do290) {
	    		do290 = false;
	    		// Come here for a restart after convergence.
	    		iv[irc] = iv[xirc];
	    		if (v[dstsav] < 0.0) {
	    			iv[irc] = 12;
	    		}
	    		do310 = true;
	    	} // if (do290)
	    	
	    	// Perform convergence tests.
	    	
	    	if (do300) {
	    		do300 = false;
	    		iv[xirc] = iv[irc];
	    		do310 = true;
	    	} // if (do300)
	    	
	    	if (do310) {
	    		do310 = false;
	    		if (Math.abs(v[f]) < v[afctol]) {
	    			iv[irc] = 10;
	    		}
	    		
	    		if (0.5 * v[fdif] > v[preduc]) {
	    			return;
	    		}
	    		
	    		emax = v[rfctol] * Math.abs(v[f0]);
	    		
	    		if (v[dstnrm] > v[lmax0] && v[preduc] <= emax) {
	    			iv[irc] = 11;
	    		}
	    		
	    		if (0.0 <= v[dst0]) {
	    			if ((v[nreduc] > 0.0 && v[nreduc] <= emax) ||
	    			    (v[nreduc] == 0.0 && v[preduc] == 0.0)) {
	    			    i = 2;
	    			}
	    			else {
	    				i = 0;
	    			}
	    			
	    			if (v[stppar] == 0.0 && v[reldx] < v[xctol] && goodx) {
	    				i = i + 1;
	    			}
	    			
	    			if (i < 0) {
	    				iv[irc] = i + 6;
	    			}
	    		} // if (0.0 <= v[dst0])
	    		
	    		// Consider recomputing step of length V{LMAX0) for singular convergence tests.
	    		if (Math.abs(iv[irc]-3) > 2 && iv[irc] != 12) {
	    			return;
	    		}
	    		
	    		if (v[lmax0] < v[dstnrm]) {
	    		    if (0.5 * v[dstnrm] <= v[lmax0]) {
	    		    	return;
	    		    }
	    		    
	    		    xmax = v[lmax0]/v[dstnrm];
	    		    
	    		    if (emax <= xmax * (2.0 - xmax) * v[preduc]) {
	    		    	return;
	    		    }
	    		} // if (v[max0] < v[dstnrm])
	    		else {
	    			if (emax <= v[preduc]) {
	    				return;
	    			}
	    			
	    			if (0.0 < v[dst0]) {
	    				
	    				if (0.5 * v[dst0] <= v[lmax0]) {
	    					return;
	    				}
	    			} // if (0.0 < v[dst0])
	    		} // else
	    		
	    		if (v[nreduc] < 0.0) {
	    			if (-v[nreduc] <= v[rfctol] * Math.abs(v[f0])) {
	    				iv[irc] = 11;
	    			}
	    			return;
	    		} // if (v[nreduc] < 0.0)
	    		
	    		// Recompute V(PREDUC) for use in singular convergence test.
	    		v[gtslst] = v[gtstep];
	    		v[dstsav] = v[dstnrm];
	    		if (iv[irc] == 12) {
	    			v[dstsav] = -v[dstsav];
	    		}
	    		v[plstgd] = v[preduc];
	    		iv[irc] = 6;
	    		for (j = 1; j <= p; j++) {
	    			stlstg[j] = step[j];
	    		}
	    		return;
	    	} // if (do310)
	    	
	    	// Perform singular convergence test with recomputed V(PREDUC).
	    	if (do360) {
	    		do360 = false;
	    		v[gtstep] = v[gtslst];
	    		v[dstnrm] = Math.abs(v[dstsav]);
	    		for (j = 1; j <= p; j++) {
	    			step[j] = stlstg[j];
	    		}
	    		
	    		if (v[dstsav] <= 0.0) {
	    			iv[irc] = 12;
	    		}
	    		else {
	    			iv[irc] = iv[xirc];
	    		}
	    		
	    		v[nreduc] = -v[preduc];
	    		v[preduc] = v[plstgd];
	    		
	    		if (-v[nreduc] <= v[rfctol] * Math.abs(v[f0])) {
	    			iv[irc] = 11;
	    		}
	    		
	    		return;
	    	} // if (do360)
	    } // while (true)		
	} // assess
	
	private void covclc ( int covirc[], double d[], int iv[], double j[][], int n, int nn, int p, 
			              double r[], double v[], double x[] ) {

	/***********************************************************************
	!
	!! COVCLC computes the covariance matrix for NL2ITR.
	!
	!  Discussion:
	!
	!    Let K = abs ( IV(COVREQ) ).  
	!
	!    For K <= 2, a finite-difference hessian H is computed,
	!    * using function and gradient values if IV(COVREQ) is nonnegative,
	!    * using only function values if IV(COVREQ) is negative).  
	!
	!    Let
	!      SCALE = 2 * F(X) / max ( 1, N - P ),
	!    where 2 * F(X) is the residual sum of squares.
	!
	!    COVCLC computes:
	!      K = 0 or 1:  SCALE * inverse ( H ) * ( J' * J ) * inverse ( H ).
	!      K = 2:       SCALE * inverse ( H );
	!      K >= 3:      SCALE * inverse ( J' * J ).
	!
	!  Modified:
	!
	!    13 April 2006
	!
	!  Parameters:
	!
	!    ?, integer COVIRC, ?
	!
	!    Input, real D(P), the scaling vector.
	!
	!    Input/output, integer IV(*), the NL2SOL integer parameter vector.
	!
	!    Input, real J(NN,P), the N by P Jacobian matrix.
	!
	!    Input, integer N, the number of functions.
	!
	!    Input, integer NN, the leading dimension of J.
	!
	!    Input, integer P, the number of variables.
	!
	!    ?, real R(N), ?
	!
	!    Input, real V(*), the NL2SOL real parameter array.
	!
	!    ?, real X(P), ?
	*/
	  
	  int ii;
      int cov = 0;
	  final int covmat = 26;
	  final int covreq = 15;
	  double del;
	  final int delta = 50;
	  final int delta0 = 44;
	  final int dltfdc = 40;
	  final int f = 10;
	  final int fx = 46;
	  final int g = 28;
	  int g1;
	  int gp;
	  int gsave1;
	  final int h = 44;
	  boolean havej;
	  int hc;
	  int hmi;
	  int hpi;
	  int hpm;
	  int i;
	  final int ierr = 32;
	  final int ipiv0 = 60;
	  int ipivi;
	  int ipivk;
	  final int ipivot = 61;
	  int irc[] = new int[1];
	  int k;
	  final int kagqt = 35;
	  final int kalm = 36;
	  int kind;
	  int kl;
	  int l;
	  final int lmat = 58;
	  int m;
	  int mm1;
	  int mm1o2;
	  final int mode = 38;
	  final int nfgcal = 7;
	  int pp1o2;
	  final int qtr = 49;
	  int qtr1;
	  final int rd = 51;
	  int rd1;
	  final int rsave = 52;
	  final int savei = 54;
	  int stp0;
	  int stpi;
	  int stpm;
	  final int switchConstant = 12;
	  double t;
	  final int toobig = 2;
	  final int w = 59;
	  int w0;
	  int w1;
	  double wk;
	  int wl;
	  final int xmsave = 49;
	  double alpha[];
	  int ipivotArr[];
	  int ierrArr[];
	  double sum[];
	  double arr[];
	  double arr2[];
	  double arr3[];
	  boolean do350 = true;

	  covirc[0] = 4;
	  kind = iv[covreq];
	  m = iv[mode];

	  loop1: while (true) {
	  if ( m <= 0 ) { // #1

	    iv[kagqt] = -1;

	    if ( 0 < iv[kalm] ) {
	      iv[kalm] = 0;
	    }

	    if ( 3 <= Math.abs ( kind ) ) {

	      rd1 = iv[rd];

	      if ( iv[kalm] == -1 ) {
	        qtr1 = iv[qtr];
	        for (ii = 1; ii <= n; ii++) {
	        	v[qtr1 + ii - 1] = r[ii];
	        }
	        w1 = iv[w] + p;
	        alpha = new double[p+1];
	        ipivotArr = new int[p+1];
	        ierrArr = new int[1];
	        sum = new double[p+1];
	        qrfact ( nn, n, p, j, alpha, ipivotArr, ierrArr, 0, sum );
	        for (ii = 1; ii <= p; ii++) {
	        	v[rd1 + ii - 1] = alpha[ii];
	        	iv[ipivot + ii - 1] = ipivotArr[ii];
	        }
	        iv[ierr] = ierrArr[0];
	        iv[kalm] = -2;
	      } // if (iv[kalm] == -1)

	      iv[covmat] = -1;

	      if (iv[ierr] != 0) {
	        return;
	      }

	      cov = iv[lmat];
	      hc = Math.abs ( iv[h] );
	      iv[h] = -hc;
	//
	//  Set HC = R matrix from QRFACT.
	//
	      l = hc;
	      for ( i = 1; i <= p; i++) {
	        if ( 1 < i ) {
	          for (ii = 1; ii <= i-1; ii++) {
	        	  v[l+ii-1] = j[ii][i];
	          }
	        }
	        l = l + i - 1;
	        v[l] = v[rd1];
	        l = l + 1;
	        rd1 = rd1 + 1;
	      } // for (i = 1; i <= p; i++)

	      break loop1;

	    } // if ( 3 <= Math.abs ( kind ) )

	    v[fx] = v[f];
	    k = iv[rsave];
	    for (ii = 1; ii <= n; ii++) {
	        v[k+ii-1] = r[ii];
	    }

	  } // if (m <= 0) #1

	  if ( m <= p ) { // #1
     loop2: while (true) {		  
     loop3: while (true) {
	    if ( kind < 0 ) {
	    	break loop3;
	    }
	//
	//  Compute finite-difference hessian using both function and
	//  gradient values.
	//
	    gsave1 = iv[w] + p;
	    g1 = iv[g];
	//
	//  First call on COVCLC.  Set GSAVE = G, take first step.
	//
	    if ( m <= 0 ) { // #2
          for (ii = 0; ii <= p-1; ii++) {
	          v[gsave1+ii] = v[g1+ii];
          }
	      iv[switchConstant] = iv[nfgcal];
	    }
	    else {

	      del = v[delta];
	      x[m] = v[xmsave];
	//
	//  Handle oversize V(DELTA).
	//
	      if ( iv[toobig] != 0 ) {

	        if ( 0.0 < del * x[m] ) {
	          del = -0.5 * del;
	          x[m] = x[m] + del;
	          v[delta] = del;
	          covirc[0] = 2;
	          return;
	        } // if (0.0 < del * x[m])

	        iv[covmat] = -2;
	        break loop2;

	      } // if (iv[toobig] != 0)

	      cov = iv[lmat];
	      gp = g1 + p - 1;
	//
	//  Set G = ( G - GSAVE ) / DEL.
	//
	      for (i = g1; i <= gp; i++) {
	        v[i] = (v[i] - v[gsave1]) / del;
	        gsave1 = gsave1 + 1;
	      }
	//
	//  Add G as new column to finite-difference hessian matrix.
	//
	      k = cov + ( m * ( m - 1 ) ) / 2;
	      l = k + m - 2;
	//
	//  Set H(1:M-1,M) = 0.5 * (H(1:M-1,m) + G(1:M-1)).
	//
	      if ( m != 1 ) {

	        for ( i = k; i <= l; i++) {
	          v[i] = 0.5 * ( v[i] + v[g1] );
	          g1 = g1 + 1;
	        } 

	      } // if (m != 1)
	//
	//  Add H(M:P,M) = G(M:P).
	//
	      l = l + 1;
	      for (i = m; i <= p; i++) {
	        v[l] = v[g1];
	        l = l + i;
	        g1 = g1 + 1;
	      }

	    } // if (m <= 0) #2

	    m = m + 1;
	    iv[mode] = m;

	    if ( p < m ) {
	      break loop2;
	    }
	//
	//  Choose next finite-difference step, return to get G there.
	//
	    del = v[delta0] * Math.max ( 1.0 / d[m], Math.abs ( x[m] ) );
	    if ( x[m] < 0.0 ) {
	      del = -del;
	    }
	    v[xmsave] = x[m];
	    x[m] = x[m] + del;
	    v[delta] = del;
	    covirc[0] = 2;
	    return;
	//
	//  Compute finite-difference hessian using function values only.
	//
      } // loop3: while(true)

	    stp0 = iv[w] + p - 1;
	    mm1 = m - 1;
	    mm1o2 = m * mm1 / 2;
	//
	//  First call on COVCLC.
	//
	    if ( m <= 0 ) { // #3

	      iv[savei] = 0;
	    }
	    else {

	      i = iv[savei];
	 
	      if ( i <= 0 ) {
	//
	//  Handle oversize step.
	//
	        if ( iv[toobig] != 0 ) {

	          stpm = stp0 + m;
	          del = v[stpm];
	//
	//  We already tried shrinking the step, so quit.
	//
	          if ( del * v[xmsave] <= 0.0 ) {
	            iv[covmat] = -2;
	            return;
	          }
	//
	//  Try shrinking the step.
	//
	          del = -0.5 * del;
	          x[m] = v[xmsave] + del;
	          v[stpm] = del;
	          covirc[0] = 1;
	          return;

	        } // if (iv[toobig] != 0)
	//
	//  Save F(X + STP(M)*E(M)) in H(P,M).
	//
	        pp1o2 = ( p * ( p - 1 ) ) / 2;
	        cov = iv[lmat];
	        hpm = cov + pp1o2 + mm1;
	        v[hpm] = v[f];
	//
	//  Start computing row M of the finite-difference hessian H.
	//
	        hmi = cov + mm1o2;
	        hpi = cov + pp1o2;

	        for ( i = 1; i <= mm1; i++) {
	          v[hmi] = v[fx] - (v[f] + v[hpi]);
	          hmi = hmi + 1;
	          hpi = hpi + 1;
	        }

	        v[hmi] = v[f] - 2.0 * v[fx];
	//
	//  Compute function values needed to complete row M of H.
	//
	        i = 1;

	        iv[savei] = i;
	        stpi = stp0 + i;
	        v[delta] = x[i];
	        x[i] = x[i] + v[stpi];
	        if ( i == m ) {
	          x[i] = v[xmsave] - v[stpi];
	        }
	        covirc[0] = 1;
	        return;

	      } // if (i <= 0)

	      x[i] = v[delta];
	//
	//  Punt in the event of an oversize step.
	//
	      if ( iv[toobig] != 0 ) {
	        iv[covmat] = -2;
	        return;
	      }
	//
	//  Finish computing H(M,I).
	//
	      stpi = stp0 + i;
	      hmi = cov + mm1o2 + i - 1;
	      stpm = stp0 + m;
	      v[hmi] = ( v[hmi] + v[f] ) / ( v[stpi] * v[stpm] );
	      i = i + 1;

	      if ( i <= m ) {
	        iv[savei] = i;
	        stpi = stp0 + i;
	        v[delta] = x[i];
	        x[i] = x[i] + v[stpi];
	        if ( i == m ) {
	          x[i] = v[xmsave] - v[stpi];
	        }
	        covirc[0] = 1;
	        return;
	      } // if (i <= m)

	      iv[savei] = 0;
	      x[m] = v[xmsave];

	    } // if (m <= 0) #3

	    m = m + 1;
	    iv[mode] = m;
	//
	//  Prepare to compute row M of the finite-difference hessian H.
	//  Compute the M-th step size STP(M), then return to obtain
	//  F(X + STP(M)*E(M)), where E(M) = M-th standard unit vector.
	//
	    if ( m <= p ) {
	      del = v[dltfdc] * Math.max ( 1.0 / d[m], Math.abs(x[m]) );
	      if (x[m] < 0.0 ) {
	        del = -del;
	      }
	      v[xmsave] = x[m];
	      x[m] = x[m] + del;
	      stpm = stp0 + m;
	      v[stpm] = del;
	      covirc[0] = 1;
	      return;
	    } // if (m <= p)
     } // loop2: while(true)
	//
	//  Restore R, V(F), etc.
	//

	    k = iv[rsave];
	    for (ii = 1; ii <= n; ii++) {
	        r[ii] = v[k+ii-1];
	    }
	    v[f] = v[fx];

	    if ( 0 <= kind ) {

	      iv[nfgcal] = iv[switchConstant];
	      qtr1 = iv[qtr];
	      for (ii = 1; ii <= n; ii++) {
	          v[qtr1+ii-1] = r[ii];
	      }
	 
	      if ( 0 <= iv[covmat] ) {
	        covirc[0] = 3;
	      }
	 
	      return;
	    } // if (0 <= kind)

	  } // if (m <= p) #1

	  cov = iv[lmat];
	//
	//  The complete finite-difference hessian is now stored at V(COV).
	//  Use it to compute the requested covariance matrix.
	//
	//  Compute Cholesky factor C of H = C * C' and store it at V(HC).
	//
	  hc = cov;

	  if ( Math.abs ( kind ) != 2 ) {
	    hc = Math.abs ( iv[h] );
	    iv[h] = -hc;
	  }

	  arr = new double[p*(p+1)/2 + 1];
	  arr2 = new double[p*(p+1)/2 + 1];
	  for (ii = 1; ii <= p*(p+1)/2; ii++) {
		  arr2[ii] = v[cov + ii - 1];
	  }
	  lsqrt ( 1, p, arr, arr2, irc );
	  for (ii = 1; ii <= p*(p+1)/2; ii++) {
		  v[hc + ii - 1] = arr[ii];
	  }
	  iv[covmat] = -1;

	  if ( irc[0] != 0 ) {
	    return;
	  }

	  w1 = iv[w] + p;

	  if ( 1 < Math.abs ( kind ) ) {
	    break loop1;
	  }
	//
	//  Covariance = SCALE * inverse ( H ) * (J' * J) * inverse ( H ).
	//
	  for (ii = 0; ii <= p*(p+1)/2; ii++) {
		  v[cov+ii] = 0.0;
	  }
	  havej = iv[kalm] == (-1);
	//
	//  HAVEJ = .true. means J is in its original form, while
	// HAVEJ = .false. means QRFACT has been applied to J.
	//
	  if ( havej ) {
	    m = n;
	  }
	  else {
	    m = p;
	  }
	  w0 = w1 - 1;
	  rd1 = iv[rd];

	  for ( i = 1; i <= m; i++) {
	//
	//  Set W = IPIVOT * (row I of R matrix from QRFACT).
	//
	    if ( ! havej ) {

	      for (ii = 0; ii <= p-1; ii++) {
	          v[w1+ii] = 0.0;
	      }
	      ipivi = ipiv0 + i;
	      l = w0 + iv[ipivi];
	      v[l] = v[rd1];
	      rd1 = rd1 + 1;

	      for (k = i+1; k <= p; k++) {
	        ipivk = ipiv0 + k;
	        l = w0 + iv[ipivk];
	        v[l] = j[i][k];
	      } // for (k = i+1; k <= p; k++)
	   } // if (!havej)
	//
	//  Set W = (row I of J).
    //
	    else {

	      l = w0;
	      for (k = 1; k <= p; k++) {
	        l = l + 1;
	        v[l] = j[i][k];
	      } // for (k = 1; k <= p; k++)

	    } // else
	//
	//  Set W = inverse ( H ) * W.
	//
	    arr = new double[p+1];
	    arr2 = new double[p*(p+1)/2 + 1];
	    arr3 = new double[p+1];
	    for (ii = 1; ii <= p*(p+1)/2; ii++) {
	    	arr2[ii] = v[hc+ii-1];
	    }
	    for (ii = 1; ii <= p; ii++) {
	    	arr3[ii] = v[w1+ii-1];
	    }
	    livmul ( p, arr, arr2, arr3 );
	    for (ii = 1; ii <= p; ii++) {
	    	v[w1+ii-1] = arr[ii];
	    }
	    for (ii = 1; ii <= p*(p+1)/2; ii++) {
	    	arr2[ii] = v[hc+ii-1];
	    }
	    for (ii = 1; ii <= p; ii++) {
	    	arr3[ii] = v[w1+ii-1];
	    }
	    litvmu ( p, arr, arr2, arr3 );
	    for (ii = 1; ii <= p; ii++) {
	    	v[w1+ii-1] = arr[ii];
	    }
	//
	//  Add W * W' to covariance matrix.
	//
	    kl = cov;
	    for (k = 1; k <= p; k++) {
	      l = w0 + k;
	      wk = v[l];
	      for (l = 1; l <= k; l++) {
	        wl = w0 + l;
	        v[kl] = v[kl]  +  wk * v[wl];
	        kl = kl + 1;
	      } // for (l = 1; l <= k; l++)
	    } // for (k = 1; k <= p; k++)

	  } // for (i = 1; i <= m; i++)

	  
	  do350 = false;
	  break loop1;
	  } // loop1: while(true)
	//
	//  The Cholesky factor C of the unscaled inverse covariance matrix
	//  (or permutation thereof) is stored at V(HC).
	//
	//  Set C = inverse ( C ).
	//
	if (do350) {
      arr = new double[p*(p+1)/2 + 1];
      arr2 = new double[p*(p+1)/2 + 1];
      for (ii = 1; ii <= p*(p+1)/2; ii++) {
    	  arr2[ii] = v[hc+ii-1];
      }
	  linvrt ( p, arr, arr2 );
	  for (ii = 1; ii <= p*(p+1)/2; ii++) {
		  v[hc+ii-1] = arr[ii];
	  }
	//
	//  Set C = C' * C.
	//
	  for (ii = 1; ii <= p*(p+1)/2; ii++) {
    	  arr2[ii] = v[hc+ii-1];
      }
	  ltsqar ( p, arr, arr2 );
	  for (ii = 1; ii <= p*(p+1)/2; ii++) {
		  v[hc+ii-1] = arr[ii];
	  }
	//
    //  C = permuted, unscaled covariance.
	//  Set COV = IPIVOT * C * IPIVOT'.
	//
	  if ( hc != cov ) {

	    for (i = 1; i <= p; i++) {
	      m = ipiv0 + i;
	      ipivi = iv[m];
	      kl = cov-1 + ( ipivi * (ipivi-1) ) / 2;
	      for (k = 1; k <= i; k++) {
	        m = ipiv0 + k;
	        ipivk = iv[m];
	        l = kl + ipivk;
	        if ( ipivi < ipivk ) {
	          l = l + ( (ipivk-ipivi) * (ipivk+ipivi-3) ) / 2;
	        }
	        v[l] = v[hc];
	        hc = hc + 1;
	      } // for (k = 1; k <= i; k++)
	    } // for (i = 1; i <= p; i++)

	  } // if (hc != cov)
	} // if (do350)

	  iv[covmat] = cov;
	//
	//  Apply scale factor = (residual sum of squares) / max(1,n-p).
	//
	  t = v[f] / ( 0.5 * (double) ( Math.max ( 1, n-p ) ) );
	  k = cov - 1 + ( p * ( p + 1 ) ) / 2;

	  for (ii = cov; ii <= k; ii++) {
	      v[ii] = t * v[ii];
	  }

	  return;
	} // private void covclc
	
	private void dfault ( int iv[], double v[] ) {

	/***********************************************************************
	!
	!! DFAULT supplies default values to IV and V.
	!
	!  Discussion:
	!
	!    Only entries in the first 25 positions of IV and the first 45
	!    positions of V are reset.
	! 
	!  Modified:
	!
	!    05 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Output, integer IV(25), contains default values for specific entries.
	!
	!    Output, real V(45), contains default values for specific values.
	*/

	  int afctol = 31;
	  int cosmin = 43;
	  int covprt = 14;
	  int covreq = 15;
	  int d0init = 37;
	  int decfac = 22;
	  int delta0 = 44;
	  int dfac = 41;
	  int dinit = 38;
	  int dltfdc = 40;
	  int dltfdj = 36;
	  int dtype = 16;
	  int inits = 25;
	  int epslon = 19;
	  int fuzz = 45;
	  int incfac = 23;
	  int jtinit = 39;
	  int lmax0 = 35;
	  double machep;
	  double mepcrt;
	  int mxfcal = 17;
	  int mxiter = 18;
	  int outlev = 19;
	  int parprt = 20;
	  int phmnfc = 20;
	  int phmxfc = 21;
	  int prunit = 21;
	  int rdfcmn = 24;
	  int rdfcmx = 25;
	  int rfctol = 32;
	  int rlimit = 42;
	  int solprt = 22;
	  double sqteps;
	  int statpr = 23;
	  int tuner1 = 26;
	  int tuner2 = 27;
	  int tuner3 = 28;
	  int tuner4 = 29;
	  int tuner5 = 30;
	  int x0prt = 24;
	  int xctol = 33;
	  int xftol = 34;

	  iv[1] = 12;
	  iv[covprt] = 1;
	  iv[covreq] = 1;
	  iv[dtype] = 1;
	  iv[inits] = 0;
	  iv[mxfcal] = 200;
	  iv[mxiter] = 150;
	  iv[outlev] = -1;
	  iv[parprt] = 1;
	  iv[prunit] = 6;
	  iv[solprt] = 1;
	  iv[statpr] = 1;
	  iv[x0prt] = 1;

	  machep = epsilon;
	  v[afctol] = 1.0e-20;
	  if ( 1.0e-10 < machep ) { 
	    v[afctol] = machep*machep;
	  }
	  v[cosmin] = Math.max ( 1.0e-06, 1.0e+02 * machep );
	  v[decfac] = 0.5;
	  sqteps = Math.sqrt (epsilon);
	  v[delta0] = sqteps;
	  v[dfac] = 0.6;
	  v[dinit] = 0.0;
	  mepcrt = Math.pow(machep ,( 1.0 / 3.0) );
	  v[dltfdc] = mepcrt;
	  v[dltfdj] = sqteps;
	  v[d0init] = 1.0;
	  v[epslon] = 0.1;
	  v[fuzz] = 1.5;
	  v[incfac] = 2.0;
	  v[jtinit] = 1.0e-6;
	  v[lmax0] = 100.0;
	  v[phmnfc] = -0.1;
	  v[phmxfc] = 0.1;
	  v[rdfcmn] = 0.1;
	  v[rdfcmx] = 4.0;
	  v[rfctol] = Math.max ( 1.0E-10, mepcrt*mepcrt );
	  v[rlimit] = Math.sqrt ( 0.999 * huge);
	  v[tuner1] = 0.1;
	  v[tuner2] = 1.0e-4;
	  v[tuner3] = 0.75;
	  v[tuner4] = 0.5;
	  v[tuner5] = 0.75;
	  v[xctol] = sqteps;
	  v[xftol] = 1.0e+2 * machep;

	  return;
	} // private void dfault
	
	private double dotprd ( int p, double x[], double y[] ) {
	
	/***********************************************************************
	!
	!! DOTPRD returns the inner product of two vectors.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer P, the number of entries in the vectors.
	!
	!    Input, real X(P), Y(P), the vectors.
	!
	!    Output, real DOTPRD, the dot product of X and Y.
	*/

	  double result;
	  int i;
	  double t;

	  result = 0.0;

	  if ( p <= 0 ) {
	    return result;
	  }

	  if ( sqteta_dotprd == 0.0 ) {
	    sqteta_dotprd = Math.sqrt ( 1.001 * tiny);
	  }

	  for ( i = 1; i <= p; i++) {

	    t = Math.max ( Math.abs ( x[i] ), Math.abs ( y[i] ) );

	    if ( t < sqteta_dotprd ) {
	    	
	    }
	    else if ( 1.0 < t ) {

	      result = result + x[i] * y[i];

	    }
	    else {
	      t = ( x[i] / sqteta_dotprd ) * y[i];

	      if ( sqteta_dotprd <= Math.abs ( t ) ) {
	        result = result + x[i] * y[i];
	      }

	    } // else

	  } // for (i = 1; i <= p; i++)

	  return result;
	} // private double dotprd
	
	private void dupdat ( double d[], int iv[], double j[][], int n, int nn, int p, double v[] ) {

	/***********************************************************************
	!
	!! DUPDAT updates the scale vector for NL2ITR.
	!
	!  Modified:
	!
	!    05 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input/output, real D(P), the scale vector.
	!
	!    Input, integer IV(*), the NL2SOL integer array.
	!
	!    Input, real J(NN,P), the N by P Jacobian matrix.
	!
	!    Input, integer N, the number of functions.
	!
	!    Input, integer NN, the leading dimension of J.
	!
	!    Input, integer P, the number of variables.
	!
	!    Input, real V(*), the NL2SOL real array.
	*/
	  
	  int d0;
	  int dfac = 41; 
	  int dtype = 16;
	  int i;
	  int jtol0 = 86;
	  int jtoli;
	  int niter = 31;
	  int s = 53;
	  int s1;
	  double sii;
	  double t;
	  double vdfac;
	  double arr[] = new double[n];
	  int ii;

	  i = iv[dtype];

	  if ( i != 1 ) {

	    if ( 0 < iv[niter] ) {
	      return;
	    }

	  } // if (i != 1)

	  vdfac = v[dfac];
	  d0 = jtol0 + p;
	  s1 = iv[s] - 1;

	  for (i = 1; i <= p; i++) {

	    s1 = s1 + i;
	    sii = v[s1];
	    for (ii = 1; ii <= n; ii++) {
	    	arr[ii] = j[ii][i];
	    }
	    t = v2norm ( n, arr );

	    if ( 0.0 < sii ) {
	      t = Math.sqrt ( t * t + sii );
	    }

	    jtoli = jtol0 + i;
	    d0 = d0 + 1;

	    if ( t < v[jtoli] ) {
	      t = Math.max ( v[d0], v[jtoli] );
	    }

	    d[i] = Math.max ( vdfac * d[i], t );

	  } // for (i = 1; i <= p; i++)

	  return;
	} // private void dupdat
	
	private void gqtstp ( double d[], double dig[], double dihdi[], int ka[], double l[],
			              int p, double step[], double v[], double w[] ) {

	/***********************************************************************
	!
	!! GQTSTP computes the Goldfeld-Quandt-Trotter step by More-Hebden technique.
	!
	!  Discussion:
	!
	!    Given the compactly stored lower triangle of a scaled
	!    hessian approximation and a nonzero scaled gradient vector,
	!    this subroutine computes a Goldfeld-Quandt-Trotter step of
	!    approximate length V(RADIUS) by the More-Hebden technique.
	!
	!    In other words, STEP is computed to approximately minimize
	!      PSI(STEP) = G' * STEP + 0.5 * STEP' * H * STEP  
	!    such that the 2-norm of D * STEP is at most approximately V(RADIUS),
	!    where G is the gradient, H is the hessian, and D is a diagonal
	!    scale matrix whose diagonal is stored in the parameter D.
	!
	!    GQTSTP assumes:
	!
	!      DIG = inverse ( D ) * G,
	!      DIHDI = inverse ( D ) * H * inverse ( D ).
	!
	!    If G = 0, however, STEP = 0 is returned, even at a saddle point.
	!
	!    If it is desired to recompute STEP using a different value of
	!    V(RADIUS), then this routine may be restarted by calling it
	!    with all parameters unchanged except V(RADIUS).  This explains
	!    why STEP and W are listed as I/O.  On an initial call, with
	!    KA < 0, STEP and W need not be initialized and only components
	!    V(EPSLON), V(STPPAR), V(PHMNFC), V(PHMXFC), V(RADIUS), and
	!    V(RAD0) of V must be initialized.  To compute STEP from a saddle
	!    point, where the true gradient vanishes and H has a negative
	!    eigenvalue, a nonzero G with small components should be passed.
	!
	!    This routine is called as part of the NL2SOL package, but it could 
	!    be used in solving any unconstrained minimization problem.
	!
	!    The desired G-Q-T step (references 2, 3, 4) satisfies
	!    (H + ALPHA*D**2) * STEP = -G  for some nonnegative ALPHA such that
	!    H + ALPHA*D**2 is positive semidefinite.  ALPHA and STEP are
	!    computed by a scheme analogous to the one described in reference 5.
	!    Estimates of the smallest and largest eigenvalues of the hessian
	!    are obtained from the Gerschgorin circle theorem enhanced by a
	!    simple form of the scaling described in reference 6.  
	!
	!    Cases in which H + ALPHA*D**2 is nearly or exactly singular are 
	!    handled by the technique discussed in reference 2.  In these 
	!    cases, a step of exact length V(RADIUS) is returned for which 
	!    PSI(STEP) exceeds its optimal value by less than 
	!    -V(EPSLON)*PSI(STEP).
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Reference:
	!
	!    John Dennis, David Gay, Roy Welsch,
	!    An Adaptive Nonlinear Least Squares Algorithm,
	!    ACM Transactions on Mathematical Software,
	!    Volume 7, Number 3, 1981.
	!
	!    David Gay,
	!    Computing Optimal Locally Constrained Steps,
	!    SIAM Journal on Scientific and Statistical Computing, 
	!    Volume 2, Number 2, pages 186-197, 1981.
	!
	!    S M Goldfeld, R E Quandt, H F Trotter,
	!    Maximization by Quadratic Hill-climbing, 
	!    Econometrica,
	!    Volume 34, pages 541-551, 1966.
	!
	!    M D Hebden,
	!    An Algorithm for Minimization using Exact Second Derivatives, 
	!    Report TP515, 
	!    Theoretical Physics Division, 
	!    AERE, Harwell, Oxon., England, 1973.
	!
	!    Jorge More,
	!    The Levenberg-Marquardt Algorithm, Implementation and Theory, 
	!    in Springer Lecture Notes in Mathematics, Number 630, 
	!    edited by G A Watson,
	!    Springer Verlag, Berlin and New York, pages 105-116, 1978.
	!
	!    Richard Varga, 
	!    Minimal Gerschgorin Sets, 
	!    Pacific Journal of Mathematics, 
	!    Volume 15, pages 719-729, 1965.
	!
	!  Parameters:
	!
	!    Input, real D(P), the scale vector, that is, the diagonal of the scale
	!    matrix D mentioned above.
	!
	!    Input, real DIG(P), the scaled gradient vector, inverse ( D ) * G.  
	!    If G = 0, then STEP = 0 and V(STPPAR) = 0 are returned.
	!
	!    Input, real DIHDI((P*(P+1))/2), the lower triangle of the scaled 
	!    hessian approximation, that is, 
	!      inverse ( D ) * H * inverse ( D ),
	!    stored compactly by rows, in the order (1,1), (2,1), (2,2), (3,1), 
	!    (3,2), and so on.
	!
	!    Input/output, integer KA, the number of Hebden iterations taken so
	!    far to determine STEP.  KA < 0 on input means this is the first
	!    attempt to determine STEP for the present DIG and DIHDI.
	!    KA is initialized to 0 in this case.  Output with KA = 0  or 
	!    V(STPPAR) = 0 means STEP = -inverse(H)*G.
	!
	!     l (i/o) = workspace of length p*(p+1)/2 for cholesky factors.
	!
	!     p (in)  = number of parameters -- the hessian is a  p x p  matrix.
	!
	!  step (i/o) = the step computed.
	!
	!     v (i/o) contains various constants and variables described below.
	!
	!     w (i/o) = workspace of length 4*p + 6.
	!
	!  entries in v
	!
	! v(dgnorm) (i/o) = 2-norm of (d**-1)*g.
	! v(dstnrm) (output) = 2-norm of d * step.
	! v(dst0)   (i/o) = 2-norm of d*(h**-1)*g (for pos. def. h only), or
	!             overestimate of smallest eigenvalue of (d**-1)*h*(d**-1).
	! v(epslon) (in)  = max. relative error allowed for psi(step).  for the
	!             step returned, psi(step) will exceed its optimal value
	!             by less than -v(epslon)*psi(step).  suggested value = 0.1.
	! v(gtstep) (out) = inner product between g and step.
	! v(nreduc) (out) = psi(-(h**-1)*g) = psi(Newton step)  (for pos. def.
	!             h only -- v(nreduc) is set to zero otherwise).
	! v(phmnfc) (in)  = tol. (together with v(phmxfc)) for accepting step
	!             (More's sigma).  the error v(dstnrm) - v(radius) must lie
	!             between v(phmnfc)*v(radius) and v(phmxfc)*v(radius).
	! v(phmxfc) (in)  (see v(phmnfc).)
	!             suggested values -- v(phmnfc) = -0.25, v(phmxfc) = 0.5.
	! v(preduc) (out) = psi(step) = predicted obj. func. reduction for step.
	! v(radius) (in)  = radius of current (scaled) trust region.
	! v(rad0)   (i/o) = value of v(radius) from previous call.
	! v(STPPAR) (i/o) is normally the Marquardt parameter, i.e. the alpha
	!             described below under algorithm notes.  if h + alpha*d**2
	!             (see algorithm notes) is (nearly) singular, however,
	!             then v(STPPAR) = -alpha.
	*/
	  

	  double aki;
	  double akk;
	  double alphak;
	  double delta;
	  int dggdmx;
	  final int dgnorm = 1;
	  int diag;
	  int diag0;
	  double dst;
	  final int dst0 = 3;
	  final int dstnrm = 2;
	  int dstsav;
	  int emax;
	  int emin;
	  final double epsfac = 50.0;
	  final int epslon = 19;
	  double epso6;
	  final int gtstep = 4;
	  int i;
	  int inc;
	  int irc[] = new int[1];
	  int j;
	  int k;
	  int k1;
	  int kalim;
	  final double kappa = 2.0;
	  double lk;
	  int lk0;
	  double lsvmin;
	  final int nreduc = 6;
	  double oldphi;
	  double phi;
	  double phimax;
	  double phimin;
	  int phipin;
	  final int phmnfc = 20;
	  final int phmxfc = 21;
	  final int preduc = 7;
	  double psifac;
	  int q;
	  int q0;
	  double rad;
	  final int rad0 = 9;
	  final int radius = 8;
	  boolean restrt;
	  double root;
	  double si;
	  double sk;
	  final int stppar = 5;
	  double sw;
	  double t;
	  double t1;
	  double twopsi;
	  double uk;
	  int uk0;
	  double wi;
	  int x;
	  int x0;
	  int ii;
	  double arr[];
	  boolean do20 = true;
	  boolean do40 = false;
	  boolean do60 = false;
	  boolean do70 = false;
	  boolean do210 = false;
	  boolean do260 = false;
	//
	//  Store largest absolute entry in inverse(D)*H*inverse(D) at W(DGGDMX).
	//
	  dggdmx = p + 1;
	//
	//  Store Gerschgorin over- and underestimates of the largest
	//  and smallest eigenvalues of inverse(D)*H*inverse(D) at W(EMAX)
	//  and W(EMIN) respectively.
	//
	  emax = dggdmx + 1;
	  emin = emax + 1;
	//
	//  For use in recomputing step, the final values of LK, UK, DST,
	//  and the inverse derivative of More's PHI at 0, for positive definite
	//  H, are stored in W(LK0), W(UK0), W(DSTSAV), and W(PHIPIN)
	//  respectively.
	//
	  lk0 = emin + 1;
	  phipin = lk0 + 1;
	  uk0 = phipin + 1;
	  dstsav = uk0 + 1;
	//
	//  Store diagonal of inverse(D)*H*inverse(D) in W(DIAG:DIAG+P-1).
	//
	  diag0 = dstsav;
	  diag = diag0 + 1;
	//
	//  Store -D * STEP in W(Q:Q+P-1).
	//
	  q0 = diag0 + p;
	  q = q0 + 1;
	  rad = v[radius];
	//
	//  PHITOL = maximum error allowed in DST = V(DSTNRM) = 2-norm of
	//  D * STEP.
	//
	  phimax = v[phmxfc] * rad;
	  phimin = v[phmnfc] * rad;
	//
	//  EPSO6 and PSIFAC are used in checking for the special case
	//  of nearly singular H + ALPHA*D**2.  See reference 2.
	//
	  psifac = 2.0 * v[epslon] / ( 3.0 * ( 4.0 * ( v[phmnfc] + 1.0 ) * 
	    ( kappa + 1.0 )  +  kappa  +  2.0 ) * rad*rad );
	//
	//  OLDPHI is used to detect limits of numerical accuracy.  If
	//  we recompute step and it does not change, then we accept it.
	//
	  oldphi = 0.0;
	  epso6 = v[epslon] / 6.0;
	  irc[0] = 0;
	  restrt = false;
	  kalim = ka[0] + 50;
	//
	//  Start or restart, depending on KA.
	//
	  if ( 0 <= ka[0] ) {
		  do20 = false;
	  loop1: while (true) {
	//
	//  Restart with new radius.
	//
	//  Prepare to return Newton step.
	//
	    if ( 0.0 < v[dst0] && v[dst0] - rad <= phimax ) {

	      restrt = true;
	      ka[0] = ka[0] + 1;
	      k = 0;
	      for (i = 1; i <= p; i++) {
	        k = k + i;
	        j = diag0 + i;
	        dihdi[k] = w[j];
	      } // for (i = 1; i <= p; i++)
	      uk = -1.0;
	      do40 = true;
	      break loop1;

	    } // if ( 0.0 < v[dst0] && v[dst0] - rad <= phimax )

	    if ( ka[0] == 0 ) {
	      do60 = true;
	      break loop1;
	    }

	    dst = w[dstsav];
	    alphak = Math.abs ( v[stppar] );
	    phi = dst - rad;
	    t = v[dgnorm] / rad;
	//
	//  Smaller radius.
	//
	    if ( rad <= v[rad0] ) {

	      uk = t - w[emin];
	      lk = 0.0;
	      if ( 0.0 < alphak ) {
	        lk = w[lk0];
	      }
	      lk = Math.max ( lk, t - w[emax] );
	      if ( 0.0 < v[dst0] ) {
	        lk = Math.max ( lk, ( v[dst0] - rad ) * w[phipin] );
	      }
	    } // if (rad <= v[rad0])
	//
	//  Bigger radius.
	//
	    else {

	      uk = t - w[emin];
	      if ( 0.0 < alphak ) {
	        uk = Math.min ( uk, w[uk0] );
	      }
	      lk = Math.max ( 0.0, Math.max(-v[dst0], t - w[emax] ));
	      if ( 0.0 < v[dst0] ) {
	        lk = Math.max ( lk, (v[dst0]-rad)*w[phipin] );
	      }
	  
	    } // else for if (rad <= v[rad0])

	    do260 = true;
	    break loop1;
	  } // loop1: while(true)
	  } // if (0 <= ka[0])
	
	if (do20) {
	//
	//  Fresh start.
	//
	  k = 0;
	  uk = -1.0;
	  ka[0] = 0;
	  kalim = 50;
	//
	//  Store diagonal of DIHDI in W(DIAG0+1:DIAG0+P).
	//
	  j = 0;
	  for ( i = 1; i <= p; i++) {
	    j = j + i;
	    k1 = diag0 + i;
	    w[k1] = dihdi[j];
	  } // for (i = 1; i <= p; i++)
	//
	//  Determine W(DGGDMX), the largest element of DIHDI.
	//
	  t1 = 0.0;
	  j = p * (p + 1) / 2;
	  for (i = 1; i <= j; i++) {
	    t = Math.abs(dihdi[i]);
	    t1 = Math.max ( t1, t );
	  } // for (i = 1; i <= j; i++)
	  w[dggdmx] = t1;
	  do40 = true;
	} // if (do20)
	
	if (do40) {
	//
	//  Try ALPHA = 0.
	//

	  lsqrt ( 1, p, l, dihdi, irc );
	//
	//  Indefinite H.  Underestimate smallest eigenvalue, use this
	//  estimate to initialize lower bound LK on ALPHA.
    //
	  if ( irc[0] == 0 ) {
		  do60 = true;
	  }
	  else {
	      j = ( irc[0] * ( irc[0] + 1 ) ) / 2;
	      t = l[j];
	      l[j] = 1.0;
	      for (ii = 1; ii <= irc[0]-1; ii++) {
	          w[ii] = 0.0;
	      }
	      w[irc[0]] = 1.0;
	      litvmu(irc[0], w, l, w);
	      t1 = v2norm(irc[0], w);
	      lk = -t / t1 / t1;
	      v[dst0] = -lk;

	      if (restrt) {
	    	  do210 = true;
	      }
	      else {
	          v[nreduc] = 0.0;
	          do70 = true;
	      }
	  } // else
	} // if (do40)
	/*if (d060) {
	//
	//  Positive definite H.  Compute unmodified Newton step.
	//

	  lk = 0.0;
	  arr = new double[p+1];
	  livmul(p, arr, l, dig);
	  for (ii = 1; ii <= p; ii++) {
		  w[q+ii-1] = arr[ii];
	  }
	  for (ii = 1; ii <= p; ii++) {
		  arr[ii] = w[q+ii-1];
	  }
	  v[nreduc] = 0.5 * dotprd(p, arr, arr);
	  call litvmu(p, w(q), l, w(q))
	  dst = v2norm(p, w(q))
	  v(dst0) = dst
	  phi = dst - rad

	  if ( phi <= phimax ) then
	    alphak = 0.0E+00
	    go to 290
	  end if

	  if (restrt) go to 210
	} // if (do60)
	!
	!  Prepare to compute Gerschgorin estimates of largest and
	!  smallest eigenvalues.
	!
	70 continue

	  v(dgnorm) = v2norm ( p, dig )

	  if ( v(dgnorm) == 0.0E+00 ) then
	    v(stppar) = 0.0E+00
	    v(preduc) = 0.0E+00
	    v(dstnrm) = 0.0E+00
	    v(gtstep) = 0.0E+00
	    step(1:p) = 0.0E+00
	    return
	  end if

	  k = 0
	  do i = 1, p
	    wi = 0.0E+00
	    do j = 1, i - 1
	      k = k + 1
	      t = abs ( dihdi(k) )
	      wi = wi + t
	      w(j) = w(j) + t
	    end do
	    w(i) = wi
	    k = k + 1
	  end do
	!
	!  Underestimate smallest eigenvalue of inverse(D)*H*inverse(D).
	!
	  k = 1
	  t1 = w(diag) - w(1)

	  do i = 2, p
	    j = diag0 + i
	    t = w(j) - w(i)
	    if ( t < t1 ) then
	      t1 = t
	      k = i
	    end if
	  end do
	  
	  sk = w(k)
	  j = diag0 + k
	  akk = w(j)
	  k1 = ( k * ( k - 1 ) ) / 2 + 1
	  inc = 1
	  t = 0.0E+00

	  do i = 1, p

	    if ( i == k ) then
	      inc = i
	      k1 = k1 + inc
	    else
	      aki = abs(dihdi(k1))
	      si = w(i)
	      j = diag0 + i
	      t1 = 0.5E+00 * (akk - w(j) + si - aki)
	      t1 = t1 + sqrt(t1*t1 + sk*aki)
	      if (t < t1) t = t1
	      if ( k <= i ) then
	        inc = i
	      end if
	      k1 = k1 + inc
	    end if

	  end do

	  w(emin) = akk - t
	  uk = v(dgnorm) / rad - w(emin)
	!
	!  Compute Gerschgorin overestimate of largest eigenvalue.
	!
	  k = 1
	  t1 = w(diag) + w(1)

	  do i = 2, p
	    j = diag0 + i
	    t = w(j) + w(i)
	    if ( t1 < t ) then
	      t1 = t
	      k = i
	    end if
	  end do

	  sk = w(k)
	  j = diag0 + k
	  akk = w(j)
	  k1 = ( k * ( k - 1 ) ) / 2 + 1
	  inc = 1
	  t = 0.0E+00

	  do i = 1, p
	    if (i == k) then
	      inc = i
	      k1 = k1 + inc
	    else
	      aki = abs ( dihdi(k1) )
	      si = w(i)
	      j = diag0 + i
	      t1 = 0.5E+00 * ( w(j) + si - aki - akk )
	      t1 = t1 + sqrt ( t1 * t1 + sk * aki )
	      if (t < t1) t = t1
	      if ( k <= i ) then
	        inc = i
	      end if
	      k1 = k1 + inc
	    end if
	  end do

	  w(emax) = akk + t
	  lk = max ( lk, v(dgnorm) / rad - w(emax) )
	!
	!  ALPHAK = current value of ALPHA.  We
	!  use More's scheme for initializing it.
	!
	  alphak = abs ( v(stppar) ) * v(rad0) / rad
	!
	!  Compute L0 for positive definite H.
	!
	  if ( irc == 0 ) then

	    call livmul(p, w, l, w(q))
	    t = v2norm(p, w)
	    w(phipin) = dst / t / t
	    lk = max ( lk, phi * w(phipin) )

	  end if
	!
	!  Safeguard ALPHAK and add ALPHAK*IDENTITY to inverse(D)*H*inverse(D).
	!
	210 continue

	  ka = ka + 1

	  if ( -v(dst0) >= alphak .or. alphak < lk .or. alphak >= uk ) then
	    alphak = uk * max ( 0.001E+00, sqrt ( lk / uk ) )
	  end if

	  k = 0
	  do i = 1, p
	    k = k + i
	    j = diag0 + i
	    dihdi(k) = w(j) + alphak
	  end do
	!
	!  Try computing Cholesky decomposition.
	!
	  call lsqrt(1, p, l, dihdi, irc)
	!
	!  inverse(D)*H*inverse(D) + ALPHAK*IDENTITY  is indefinite.  Overestimate
	!  smallest eigenvalue for use in updating LK.
	!
	  if ( irc /= 0 ) then

	    j = ( irc * ( irc + 1 ) ) / 2
	    t = l(j)
	    l(j) = 1.0E+00
	    w(1:irc) = 0.0E+00
	    w(irc) = 1.0E+00
	    call litvmu ( irc, w, l, w )
	    t1 = v2norm ( irc, w )
	    lk = alphak - t / t1 / t1
	    v(dst0) = -lk
	    go to 210

	  end if
	!
	!  ALPHAK makes inverse(D)*H*inverse(D) positive definite.
	!  Compute Q = -D * STEP, check for convergence.
	!
	  call livmul(p, w(q), l, dig)
	  call litvmu(p, w(q), l, w(q))
	  dst = v2norm(p, w(q))
	  phi = dst - rad

	  if (phi <= phimax .and. phi >= phimin) go to 290

	  if (phi == oldphi) go to 290

	  oldphi = phi

	  if ( phi > 0.0E+00 ) go to 260
	!
	!  Check for the special case of H + ALPHA*D**2  (nearly)
	!  singular.  delta is >= the smallest eigenvalue of
	!  inverse(D)*H*inverse(D) + ALPHAK*IDENTITY.
	!
	  if ( v(dst0) > 0.0E+00 ) go to 260

	  delta = alphak + v(dst0)
	  twopsi = alphak * dst * dst + dotprd ( p, dig, w(q) )

	  if ( delta < psifac*twopsi ) then
	    go to 270
	  end if
	!
	!  Unacceptable ALPHAK.  Update LK, UK, ALPHAK.
	!
	260 continue

	  if (ka >= kalim) go to 290

	  call livmul(p, w, l, w(q))
	  t1 = v2norm(p, w)
	!
	!  The following min is necessary because of restarts.
	!
	  if ( phi < 0.0E+00 ) then
	    uk = min ( uk, alphak )
	  end if

	  alphak = alphak + ( phi / t1 ) * ( dst / t1 ) * ( dst / rad )
	  lk = max ( lk, alphak )
	  go to 210
	!
	!  Decide how to handle nearly singular H + ALPHA*D**2.
	!
	!  If not yet available, obtain machine dependent value dgxfac.
	!
	270 continue

	  if ( dgxfac == 0.0E+00 ) then
	    dgxfac = epsfac * epsilon ( dgxfac )
	  end if
	!
	!  Is DELTA so small we cannot handle the special case in
	!  the available arithmetic?  If so, accept STEP as it is.
	!
	  if ( dgxfac * w(dggdmx) < delta ) then
	!
	!  Handle nearly singular H + ALPHA*D**2.
	!  Negate ALPHAK to indicate special case.
	!
	    alphak = -alphak
	!
	!  Allocate storage for scratch vector X.
	!
	    x0 = q0 + p
	    x = x0 + 1
	!
	!  Use inverse power method with start from LSVMIN to obtain
	!  approximate eigenvector corresponding to smallest eigenvalue
	!  of inverse ( D ) * H * inverse ( D ).
	!
	    delta = kappa * delta
	    t = lsvmin(p, l, w(x), w)
	    k = 0
	!
	!  Normalize W.
	!
	    do

	      w(1:p) = t * w(1:p)
	!
	!  Complete current inverse power iteration.  
	!  Replace W by inverse ( L' ) * W.
	!
	      call litvmu ( p, w, l, w )
	      t1 = 1.0E+00 / v2norm(p, w)
	      t = t1 * t

	      if ( t <= delta ) then
	        exit
	      end if

	      if ( 30 < k ) then
	        go to 290
	      end if

	      k = k + 1
	!
	!  Start next inverse power iteration by storing normalized W in X.
	!
	      do i = 1, p
	        j = x0 + i
	        w(j) = t1 * w(i)
	      end do
	!
	!  Compute W = inverse ( L ) * X.
	!
	      call livmul(p, w, l, w(x))
	      t = 1.0E+00 / v2norm(p, w)

	    end do

	    w(1:p) = t1 * w(1:p)
	!
	!  Now W is the desired approximate unit eigenvector and
	!  T * X = ( inverse(D) * H * inverse(D) + ALPHAK * I ) * W.
	!
	    sw = dotprd ( p, w(q), w )
	    t1 = ( rad + dst ) * ( rad - dst )
	    root = sqrt ( sw * sw + t1 )
	    if ( sw < 0.0E+00 ) then
	      root = -root
	    end if
	    si = t1 / (sw + root)
	!
	!  Accept current step if adding SI * W would lead to a
	!  further relative reduction in PSI of less than V(EPSLON) / 3.
	!
	    v(preduc) = 0.5E+00 * twopsi
	    t1 = 0.0E+00 
	    t = si * ( alphak * sw &
	      - 0.5E+00 * si * ( alphak + t * dotprd ( p, w(x), w ) ) )

	    if ( epso6 * twopsi <= t ) then
	      v(preduc) = v(preduc) + t
	      dst = rad
	      t1 = -si
	    end if

	    do i = 1, p
	      j = q0 + i
	      w(j) = t1 * w(i) - w(j)
	      step(i) = w(j) / d(i)
	    end do

	    v(gtstep) = dotprd ( p, dig, w(q) )
	!
	!  Save values for use in a possible restart.
	!
	    v(dstnrm) = dst
	    v(stppar) = alphak
	    w(lk0) = lk
	    w(uk0) = uk
	    v(rad0) = rad
	    w(dstsav) = dst
	!
	!  Restore diagonal of DIHDI.
	!
	    j = 0
	    do i = 1, p
	      j = j + i
	      k = diag0 + i
	      dihdi(j) = w(k)
	    end do

	    return

	  end if
	!
	!  Successful step.  Compute STEP = - inverse ( D ) * Q.
	!
	290 continue

	  do i = 1, p
	    j = q0 + i
	    step(i) = -w(j) / d(i)
	  end do
	  v(gtstep) = -dotprd(p, dig, w(q))
	  v(preduc) = 0.5E+00 * ( abs ( alphak ) *dst*dst - v(gtstep))
	!
	!  Save values for use in a possible restart.
	!
	  v(dstnrm) = dst
	  v(stppar) = alphak
	  w(lk0) = lk
	  w(uk0) = uk
	  v(rad0) = rad
	  w(dstsav) = dst
	!
	!  Restore diagonal of DIHDI.
	!
	  j = 0
	  do i = 1, p
	    j = j + i
	    k = diag0 + i
	    dihdi(j) = w(k)
	  end do*/

	  return;
	} // private void gqtstp
	
	private void linvrt ( int n, double lin[], double l[] ) {

	/***********************************************************************
	!
	!! LINVRT computes the inverse of a lower triangular matrix.
	!
	!  Discussion:
	!
	!    LIN = inverse ( L ), both N by N lower triangular matrices stored
	!    compactly by rows.  LIN and L may share the same storage.
	!
	!  Modified:
	!
	!    05 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer N, the order of L and LIN.
	!
	!    Output, real LIN((N*(N+1))/2), the inverse of L, a lower triangular
	!    matrix stored by rows.
	!
	!    Input, real L((N*(N+1))/2), a lower triangular matrix stored by rows.
	*/
	  

	  int i;
	  int ii;
	  int j0;
	  int j1;
	  int jj;
	  int k;
	  int k0;
	  double t;

	  j0 = ( n * ( n + 1 ) ) / 2;

	  for (ii = 1; ii <= n; ii++) {

	    i = n + 1 - ii;
	    lin[j0] = 1.0 / l[j0];

	    if ( i <= 1 ) {
	      return;
	    }

	    j1 = j0;

	    for (jj = 1; jj <= i - 1; jj++) {

	      t = 0.0;
	      j0 = j1;
	      k0 = j1 - jj;

	      for (k = 1; k <= jj; k++) {
	        t = t - l[k0] * lin[j0];
	        j0 = j0 - 1;
	        k0 = k0 + k - i;
	      }

	      lin[j0] = t / l[k0];

	    } // for (jj = 1; jj <= i-1; jj++)

	    j0 = j0 - 1;

	  } // for (ii = 1; ii <= n; ii++)

	  return;
	} // private void linvrt
	
	private void litvmu ( int n, double x[], double l[], double y[] ) {

	/***********************************************************************
	!
	!! LITVMU solves L' * X = Y, where L is a lower triangular matrix.
	!
	!  Discussion:
	!
	!    This routine solves L' * X = Y, where L is an N by N lower
	!    triangular matrix stored compactly by rows.  X and Y may occupy 
	!    the same storage.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer N, the order of L.
	!
	!    Output, real X(N), the solution.
	!
	!    Input, real L((N*(N+1))/2), the lower triangular matrix, stored
	!    by rows.
	!
	!    Input, real Y(N), the right hand side.
	*/

	  int i;
	  int i0;
	  int ii;
	  int ij;
	  int j;
	  double xi;
	  int jj;

	  for (jj = 1; jj <= n; jj++) {
	      x[jj] = y[jj];
	  }
	  i0 = ( n * ( n + 1 ) ) / 2;

	  for (ii = 1; ii <= n; ii++) {

	    i = n + 1 - ii;
	    xi = x[i] / l[i0];
	    x[i] = xi;

	    if ( i <= 1 ) {
	      return;
	    }

	    i0 = i0 - i;

	    if ( xi != 0.0 ) {

	      for ( j = 1; j <= i - 1; j++) {
	        ij = i0 + j;
	        x[j] = x[j] - xi * l[ij];
	      }

	    } // if (xi != 0.0)

	  } // for (ii = 1; ii <= n; ii++)

	  return;
	} // private void litvmu
	
	private void livmul ( int n, double x[], double l[], double y[] ) {

	/***********************************************************************
	!
	!! LIVMUL solves L * X = Y, where L is a lower triangular matrix.
	!
	!  Discussion:
	!
	!    This routine solves L * X = Y, where L is an N by N lower 
	!    triangular matrix stored compactly by rows.  X and Y may occupy 
	!    the same storage.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer N, the order of L.
	!
	!    Output, real X(N), the solution.
	!
	!    Input, real L((N*(N+1))/2), the lower triangular matrix, stored
	!    by rows.
	!
	!    Input, real Y(N), the right hand side.
	*/

	  int i;
	  int j;
	  double t;
	  double arr[];
	  int ii;

	  x[1] = y[1] / l[1];

	  j = 1;

	  for ( i = 2; i <= n; i++) {
		arr = new double[i];
		for (ii = 1; ii <= i-1; ii++) {
			arr[ii] = l[j+ii];
		}
	    t = dotprd ( i-1, arr, x );
	    j = j + i;
	    x[i] = ( y[i] - t ) / l[j];
	  }

	  return;
	} // private void livmul
	
	private void lsqrt ( int n1, int n, double l[], double a[], int irc[] ) {

	/***********************************************************************
	!
	!! LSQRT computes the Cholesky factor of a lower triangular matrix.
	!
	!  Discussion:
	!
	!    Compute rows N1 through N of the Cholesky factor L of
	!    A = L * L', where L and the lower triangle of A are both
	!    stored compactly by rows, and may occupy the same storage.
	!
	!    IRC = 0 means all went well.  IRC = J means the leading
	!    principal J x J submatrix of A is not positive definite,
	!    and L(J*(J+1)/2) contains the nonpositive reduced J-th diagonal.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer N1, N, the first and last rows to be computed.
	!
	!    Output, real L((N*(N+1))/2), contains rows N1 through N of the
	!    Cholesky factorization of A, stored compactly by rows as a lower 
	!    triangular matrix.
	!
	!    Input, real A((N*(N+1))/2), the matrix whose Cholesky factorization
	!    is desired.
	!
	!    Output, integer IRC, an error flag.  If IRC = 0, then the factorization
	!    was carried out successfully.  Otherwise, the principal J x J subminor
	!    of A was not positive definite.
	*/
	  
	  int i;
	  int i0;
	  int ij;
	  int ik;
	  int j;
	  int j0;
	  int jk;
	  int k;
	  double t;
	  double td;

	  i0 = ( n1 * ( n1 - 1 ) ) / 2;

	  for (i = n1; i <= n; i++) {

	    td = 0.0;
	    j0 = 0;

	    for ( j = 1; j <=  i - 1; j++) {

	      t = 0.0;

	      for (k = 1; k <= j - 1; k++) {
	        ik = i0 + k;
	        jk = j0 + k;
	        t = t + l[ik] * l[jk];
	      } // for (k = 1; k <= j - 1; k++)

	      ij = i0 + j;
	      j0 = j0 + j;
	      t = ( a[ij] - t ) / l[j0];
	      l[ij] = t;
	      td = td + t * t;

	    } // for (j = 1; j <= i-1; j++)

	    i0 = i0 + i;
	    t = a[i0] - td;

	    if ( t <= 0.0 ) {
	      l[i0] = t;
	      irc[0] = i;
	      return;
	    } // if (t <= 0.0)

	    l[i0] = Math.sqrt ( t );

	  } // for (i = n1; i <= n; i++)

	  irc[0] = 0;

	  return;
	} // private void lsqrt
	
	private void ltsqar ( int n, double a[], double l[] ) {
	
	/***********************************************************************
	!
	!! LTSQAR sets A to the lower triangle of L' * L.
	!
	!  Discussion:
	!
	!    L is an N by N lower triangular matrix, stored by rows.
	!
	!    A is also stored by rows, and may share storage with L.
	!
	!  Modified:
	!
	!    03 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer N, the order of L and A.
	!
	!    Output, real A((N*(N+1))/2), the lower triangle of L' * L,
	!    stored by rows.
	!
	!    Input, real L((N*(N+1))/2), the lower triangular matrix,
	!    stored by rows.
	*/
	  
	  int i;
	  int i1;
	  int ii;
	  int j;
	  int k;
	  int m;

	  ii = 0;

	  for ( i = 1; i <= n; i++) {

	    i1 = ii + 1;
	    ii = ii + i;
	    m = 1;

	    for (j = i1; j <= ii - 1; j++) {
	      for (k = i1; k <= j; k++) {
	        a[m] = a[m] + l[j] * l[k];
	        m = m + 1;
	      }
	    }

	    for ( j = i1; j <= ii; j++) {
	      a[j] = l[ii] * l[j];
	    }

	  } // for (i = 1; i <= n; i++)

	  return;
	} // private void ltsqar
	
	private void qrfact ( int nm, int m, int n, double qr[][], double alpha[], int ipivot[],
			              int ierr[], int nopivk, double sum[] ) {

	/***********************************************************************
	!
	!! QRFACT computes the QR decomposition of a matrix.
	!
	!  Discussion:
	!
	!    This subroutine does a QR decomposition on the M x N matrix QR,
	!    with an optionally modified column pivoting, and returns the
	!    upper triangular R matrix, as well as the orthogonal vectors
	!    used in the transformations.
	!
	!    This may be used when solving linear least-squares problems.
	!    See subroutine QR1 of ROSEPACK.  It is called for this purpose
	!    in the NL2SOL package.
	!
	!    This version of QRFACT tries to eliminate the occurrence of
	!    underflows during the accumulation of inner products.  RKTOL1
	!    is chosen below so as to insure that discarded terms have no
	!    effect on the computed two-norms.
	!
	!    This routine was adapted from Businger and Golub's ALGOL 
	!    routine "SOLVE".
	!
	!    This routine is equivalent to the subroutine QR1 of ROSEPACK 
	!    with RKTOL1 used in place of RKTOL below, with V2NORM used 
	!    to initialize (and sometimes update) the sum array, and 
	!    with calls on DOTPRD in place of some loops.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Reference:
	!
	!    P Businger and Gene Golub,
	!    Linear Least Squares Solutions by Householder Transformations,
	!    in Handbook for Automatic Computation,
	!    Volume II, Linear Algebra,
	!    edited by James Wilkinson and C Reinsch,
	!    Springer Verlag, pages 111-118, 1971;
	!    prepublished in Numerische Mathematik,
	!    Volume 7, pages 269-276, 1965.
	!
	!  Parameters:
	!
	!    Input, integer NM, the row dimension of the two dimensional
	!    array parameters as declared in the calling program dimension statement.
	!
	!    Input, integer M, the number of rows in the matrix.
	!
	!    Input, integer N, the number of columns in the matrix.
	!
	!    Input/output, real QR(NM,N), on input, the M by N rectangular matrix
	!    to be decomposed.  On output, contains the non-diagonal elements of 
	!    the R matrix in the strict upper triangle.  The vectors U, which
	!    define the Householder transformations (Identity - U*U'), are in the 
	!    columns of the lower triangle.  These vectors U are scaled so that 
	!    the square of their 2-norm is 2.0.
	!
	!    Output, real ALPHA(N), the diagonal elements of R.
	!
	!    Output, integer IPIVOT(N), reflects the column pivoting performed 
	!    on the input matrix to accomplish the decomposition.  The J-th
	!    element of IPIVOT gives the column of the original matrix which was 
	!    pivoted into column J during the decomposition.
	!
	!    Output, integer IERR, error flag.
	!    0, for normal return,
	!    K, if no non-zero pivot could be found for the K-th transformation,
	!    -K, for an error exit on the K-th transformation.
	!    If an error exit was taken, the first (K - 1) transformations are correct.
	!
	!    Input, integer NOPIVK, controls pivoting.  Columns 1 through NOPIVK
	!    will remain fixed in position.
	!
	!    Workspace, real SUM(N).
	!
	!  Local Parameters:
	!
	!    Local, real UFETA, the smallest positive floating point number
	!    such that UFETA and -UFETA can both be represented.
	!
	!    Local, real RKTOL, the square root of the relative precision
	!    of floating point arithmetic (MACHEP).
	*/

	  double alphak;
	  double beta;
	  int i;
	  int j;
	  int jbar;
	  int k;
	  int minum;
	  int mk1;
	  double qrkk;
	  double qrkmax;
	  double rktol1;
	  double sigma;
	  double sumj;
	  double temp;
	  int ii;
	  double arr[];
	  double arr2[];

	  if ( ufeta_qrfact <= 0.0E+00 ) {
	    ufeta_qrfact = tiny;
	    rktol_qrfact = Math.sqrt ( 0.999E+00 * epsilon);
	  }

	  ierr[0] = 0;
	  rktol1 = 0.01E+00 * rktol_qrfact;

      arr = new double[m+1];	  
	  for (j = 1; j <= n; j++) {
		for (ii = 1; ii <= m; ii++) {
			arr[ii] = qr[ii][j];
		}
	    sum[j] = v2norm ( m, arr );
	    ipivot[j] = j;
	  }

	  minum = Math.min ( m, n );

	  for ( k = 1; k <= minum; k++) {

	    mk1 = m - k + 1;
	//
	//  K-th Householder transformation.
    //
	    sigma = 0.0;
	    jbar = 0;
	//
    //  Find largest column sum.
	//
	    if ( nopivk < k ) {

	      for (j = k; j <= n; j++) {
	        if ( sigma < sum[j] ) {
	          sigma = sum[j];
	          jbar = j;
	        }
	      } // for (j = k; j <= n; j++)

	      if ( jbar == 0 ) {
	        ierr[0] = k;
	        for (i = k; i <= n; i++) {
	          alpha[i] = 0.0;
	          if ( k < i ) {
	        	for (ii = k; ii <= i-1; ii++) {
	                qr[ii][i] = 0.0;
	        	}
	          } // if (k < i)
	        } // for (i = k; i <= n; i++)
	        return;
	      } // if (jbar == 0)
	//
	//  Column interchange.
	//
	      i = ipivot[k];
	      ipivot[k] = ipivot[jbar];
	      ipivot[jbar] = i;

	      sum[jbar] = sum[k];
	      sum[k] = sigma;

	      for (i = 1; i <= m; i++) {
	        sigma = qr[i][k];
	        qr[i][k] = qr[i][jbar];
	        qr[i][jbar] = sigma;
	      } // for (i = 1; i <= m; i++)

	    } // if (nopivk < k)
	//
	//  Second inner product.
	//
	    qrkmax = Math.abs(qr[k][k]);
	    for (ii = k+1; ii <= m; ii++) {
	    	qrkmax = Math.max(qrkmax, Math.abs(qr[ii][k]));
	    }

	    if ( qrkmax < ufeta_qrfact ) {
	      ierr[0] = -k;
	      for (i = k; i <= n; i++) {
	        alpha[i] = 0.0;
	        if ( k < i ) {
	          for (ii = k; ii <= i-1; ii++) {
	              qr[ii][i] = 0.0;
	          }
	        } // if (k < i)
	      } // for (i = k; i <= n; i++)
	      return;
	    } // if (qrkmax < ufeta_qrfact)

	    arr = new double[mk1+1];
	    for (ii = 1; ii <= mk1; ii++) {
	    	arr[ii] = qr[k+ii-1][k];
	    }
	    alphak = v2norm ( mk1, arr ) / qrkmax;
	    sigma = alphak*alphak;
	//
	//  End second inner product.
	//
	    qrkk = qr[k][k];
	    if ( 0.0 <= qrkk ) {
	      alphak = -alphak;
	    }

	    alpha[k] = alphak * qrkmax;
	    beta = qrkmax * Math.sqrt ( sigma - ( qrkk * alphak / qrkmax ) );
	    qr[k][k] = qrkk - alpha[k];
	    for (ii = k; ii <= m; ii++) {
	        qr[ii][k] =  qr[ii][k] / beta;
	    }

	    for (j = k + 1; j <= n; j++) {
          arr = new double[mk1+1];
          arr2 = new double[mk1+1];
          for (ii = 1; ii <= mk1; ii++) {
        	  arr[ii] = qr[k+ii-1][k];
        	  arr2[ii] = qr[k+ii-1][j];
          }
	      temp = -dotprd ( mk1, arr, arr2 );

	      for (ii = k; ii <= m; ii++) {
	          qr[ii][j] = qr[ii][j] + temp * qr[ii][k];
	      }

	      if ( k + 1 <= m ) {
	        sumj = sum[j];
	        if ( ufeta_qrfact <= sumj ) {
	          temp = Math.abs ( qr[k][j] / sumj );
	          if ( rktol1 <= temp ) {
	            if ( 0.99 <= temp ) {
	              arr = new double[m-k+1];
	              for (ii = 1; ii <= m-k; ii++) {
	            	  arr[ii] = qr[k+ii][j];
	              }
	              sum[j] = v2norm ( m-k, arr );
	            }
	            else {
	              sum[j] = sumj * Math.sqrt ( 1.0 - temp*temp );
	            }

	          } // if (rktol1 <= temp)
	        } // if (ufeta_qrfact <= sumj)
	      } // if (k + 1 <= m)

	    }// for (j = k + 1; j <= n; j++)

	  } // for ( k = 1; k <= minum; k++)

	  return;
	} // private void qrfact
	
	double reldst ( int p, double d[], double x[], double x0[] ) {

	/***********************************************************************
	!
	!! RELDST computes the relative difference between two real values.
	!
	!  Modified:
	!
	!    03 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer P, the length of the vectors.
	!
	!    Input, real D(P), a scaling vector.
	!
	!    Input, real X(P), X0(P), two vectors whose relative difference
	!    is desired.
	!
	!    Output, real reldstVal, the relative difference between X and X0.
	*/
	  
	  double emax;
	  int i;
	  double reldstVal;
	  double xmax;

	  emax = 0.0E+00;
	  xmax = 0.0E+00;
	  for ( i = 1; i <= p; i++) {
	    emax = Math.max ( emax, Math.abs ( d[i] * ( x[i] - x0[i] ) ) );
	    xmax = Math.max ( xmax, d[i] * ( Math.abs ( x[i] ) + Math.abs ( x0[i] ) ) );
	  }

	  if ( 0.0E+00 < xmax ) {
	    reldstVal = emax / xmax;
	  }
	  else {
	    reldstVal = 0.0E+00;
	  }

	  return reldstVal;
	} // double reldst ( int p, double d[], double x[], double x0[] )
	
	private double v2norm ( int p, double x[] ) {

	/***********************************************************************
	!
	!! V2NORM computes the L2 norm of a vector.
	!
	!  Modified:
	!
	!    04 April 2006
	!
	!  Author:
	!
	!    David Gay
	!
	!  Parameters:
	!
	!    Input, integer P, the length of the vector.
	!
	!    Input, real X(P), the vector.
	!
	!    Output, real V2NORM, the Euclidean norm of the vector.
	! 
	!  Local Parameters:
	!
	!    SQTETA is (slightly larger than) the square root of the
	!    smallest positive floating point number on the machine.
	!    The tests involving SQTETA are done to prevent underflows.
	*/

	  int i;
	  int j;
	  double r;
	  double scale;
	  
	  double t;
	  double result;
	  double xi;

	  if ( p <= 0 ) {
	    result = 0.0;
	    return result;
	  }

	  i = 0;

	  for ( j = 1; j <= p; j++) {
	    if ( x[j] != 0.0 ) {
	      i = j;
	      break;
	    } //if (x[j] != 0.0)
	  } // for (j = 1; j <= p; j++)

	  if ( i == 0 ) {
	    result = 0.0;
	    return result;
	  }

	  scale = Math.abs ( x[i] );

	  t = 1.0;

	  if ( sqteta_v2norm == 0.0 ) {
	    sqteta_v2norm = Math.sqrt ( 1.001 * tiny);
	  }

	  j = i + 1;

	  for (i = j; i <= p; i++) {

	    xi = Math.abs ( x[i] );

	    if ( xi <= scale ) {

	      r = xi / scale;

	      if ( sqteta_v2norm < r ) {
	        t = t + r * r;
	      }

	    } // if (xi <= scale)
	    else {

	      r = scale / xi;

	      if ( sqteta_v2norm < r ) {
	        t = 1.0E+00 + t * r * r;
	      }
	      else {
	        t = 1.0E+00;
	      }

	      scale = xi;

	    } // else

	  } // for (i = j; i <= p; i++)

	  result = scale * Math.sqrt ( t );

	  return result;
	} // private double v2norm
}