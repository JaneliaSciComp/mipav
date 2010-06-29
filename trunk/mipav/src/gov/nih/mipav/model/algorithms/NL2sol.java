package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

public abstract class NL2sol {
	
	public NL2sol() {
	
	}
	
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
		double reldst;
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
	    	} // if (do310)
	    } // while (true)		
	} // assess
	
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
}