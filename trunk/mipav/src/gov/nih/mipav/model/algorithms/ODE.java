package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public abstract class ODE {
	
	 // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ODE object.
     */
    public ODE() {}
    
    // This is a port of the FORTRAN code ode.f by L. F. Shampine and M. K. Gordon.
    
    //   double precision subroutine ode integrates a system of neqn
    //   first order ordinary differential equations of the form:
    //             dy(i)/dt = f(t,y(1),y(2),...,y(neqn))
    //             y(i) given at  t .
    //   the subroutine integrates from  t  to  tout .  on return the
    //   parameters in the call list are set for continuing the integration.
    //   the user has only to define a new value  tout  and call  ode  again.
    //
    //   the differential equations are actually solved by a suite of codes
    //   de ,  step , and  intrp .  ode  allocates virtual storage in the
    //   arrays  work  and  iwork  and calls  de .  de  is a supervisor which
    //   directs the solution.  it calls on the routines  step  and  intrp
    //   to advance the integration and to interpolate at output points.
    //   step  uses a modified divided difference form of the adams pece
    //   formulas and local extrapolation.  it adjusts the order and step
    //   size to control the local error per unit step in a generalized
    //   sense.  normally each call to  step  advances the solution one step
    //   in the direction of  tout .  for reasons of efficiency  de
    //   integrates beyond  tout  internally, though never beyond
    //   t+10*(tout-t), and calls  intrp  to interpolate the solution at
    //   tout .  an option is provided to stop the integration at  tout  but
    //   it should be used only if it is impossible to continue the
    //   integration beyond  tout .
    //
    //   this code is completely explained and documented in the text,
    //   computer solution of ordinary differential equations:  the initial
    //   value problem  by l. f. shampine and m. k. gordon.
    //
    //   the parameters represent:
    //      f -- double precision subroutine f(t,y,yp) to evaluate
    //                derivatives yp(i)=dy(i)/dt
    //      neqn -- number of equations to be integrated (integer*4)
    //      y(*) -- solution vector at t                 (real*8)
    //      t -- independent variable                    (real*8)
    //      tout -- point at which solution is desired   (real*8)
    //      relerr,abserr -- relative and absolute error tolerances for local
    //           error test (real*8).  at each step the code requires
    //             dabs(local error) .le. dabs(y)*relerr + abserr
    //           for each component of the local error and solution vectors
    //      iflag -- indicates status of integration     (integer*4)
    //      work(*)  (real*8)  -- arrays to hold information internal to
    //      iwork(*) (integer*4)    which is necessary for subsequent calls
    //
    //   first call to ode --
    //
    //   the user must provide storage in his calling program for the arrays
    //   in the call list,
    //      y(neqn), work(100+21*neqn), iwork(5),
    //   declare  f  in an external statement, supply the double precision
    //   subroutine f(t,y,yp)  to evaluate
    //      dy(i)/dt = yp(i) = f(t,y(1),y(2),...,y(neqn))
    //   and initialize the parameters:
    //      neqn -- number of equations to be integrated
    //      y(*) -- vector of initial conditions
    //      t -- starting point of integration
    //      tout -- point at which solution is desired
    //      relerr,abserr -- relative and absolute local error tolerances
    //      iflag -- +1,-1.  indicator to initialize the code.  normal input
    //           is +1.  the user should set iflag=-1 only if it is
    //           impossible to continue the integration beyond  tout .
    //   all parameters except  f ,  neqn  and  tout  may be altered by the
    //   code on output so must be variables in the calling program.
    //
    //   output from  ode  --
    //
    //      neqn -- unchanged
    //      y(*) -- solution at  t
    //      t -- last point reached in integration.  normal return has
    //           t = tout .
    //      tout -- unchanged
    //      relerr,abserr -- normal return has tolerances unchanged.  iflag=3
    //           signals tolerances increased
    //      iflag = 2 -- normal return.  integration reached  tout
    //            = 3 -- integration did not reach  tout  because error
    //                   tolerances too small.  relerr ,  abserr  increased
    //                   appropriately for continuing
    //            = 4 -- integration did not reach  tout  because more than
    //                   500 steps needed
    //            = 5 -- integration did not reach  tout  because equations
    //                   appear to be stiff
    //            = 6 -- invalid input parameters (fatal error)
    //           the value of  iflag  is returned negative when the input
    //           value is negative and the integration does not reach  tout ,
    //           i.e., -3, -4, -5.
    //      work(*),iwork(*) -- information generally of no interest to the
    //           user but necessary for subsequent calls.
    //
    //   subsequent calls to  ode --
    //
    //   subroutine  ode  returns with all information needed to continue
    //   the integration.  if the integration reached  tout , the user need
    //   only define a new  tout  and call again.  if the integration did not
    //   reach  tout  and the user wants to continue, he just calls again.
    //   the output value of  iflag  is the appropriate input value for
    //   subsequent calls.  the only situation in which it should be altered
    //   is to stop the integration internally at the new  tout , i.e.,
    //   change output  iflag=2  to input  iflag=-2 .  error tolerances may
    //   be changed by the user before continuing.  all other parameters must
    //   remain unchanged.
    //
    //***********************************************************************
    //*  subroutines  de  and  step  contain machine dependent constants. *
    //*  be sure they are set before using  ode .                          *
    //*****************************************************************
    
    private int neqn;
    private double y[];
    private double t[];
    private double tout;
    private double relerr[];
    private double abserr[];
    private int iflag[];
    private double work[];
    private int iwork[];
    
    // For work:
    private int ialpha = 0;
    private int ibeta = 12;
    private int isig = 24;
    private int iv = 37;
    private int iw = 49;
    private int ig = 61;
    private int iphase = 74;
    private int ipsi = 75;
    private int ix = 87;
    private int ih = 88;
    private int ihold = 89;
    private int istart = 90;
    private int itold = 91;
    private int idelsn = 92;
    private int iyy = 99;
    private int iwt = iyy + neqn;
    private int ip = iwt + neqn;
    private int iyp = ip + neqn;
    private int iypout = iyp + neqn;
    private int iphi = iypout + neqn;
    
    // For iwork
    private int ns = 0;
    private int k = 2;
    private int kold = 3;
    private int isnold = 4;
    
    private boolean start = false;
    private boolean phase1 = false;
    private boolean nornd = false;
    
    // input neqn = number of equations to be integrated
    // input/output double y[] = new double[neqn]
    // y input contains y values at starting time
    // y output contains y values for last point reached in integration, normally tout.
    // input/output double t[] = new double[1]
    // t input contains the starting point of integration
    // t output the last point reached in integration.  Normal return has t = tout.
    // input tout has point at which solution is desired
    // input/ouput double relerr[] = new double[1]
    // input has starting relative error tolerance
    // output with normal return has relerr unchanged, but relerr is increased for iflag = 3
    // input/ouput double abserr[] = new double[1]
    // input has starting absolute error tolerance
    // output with normal return has abserr unchanged, but abserr is increased for iflag = 3
    // input/output int iflag[] = new int[1] iflag[0] = 1
    // Input is +1 or -1.  Indicator to initialize the code.  Normal input is +1.
    // The user should set iflag[0] = -1 only if it is impossible to continue
    // integration beyond tout.
    // iflag output as described above.
    // internal double work[] = new double[100 + 21*neqn]
    // internal int iwork[] = new int[5]
    public ODE(int neqn, double y[], double t[], double tout, double relerr[],
    		double abserr[], int iflag[], double work[], int iwork[]) {
    	        this.neqn = neqn;
        this.y = y;
        this.t = t;
        this.tout = tout;
        this.relerr = relerr;
        this.abserr = abserr;
        this.iflag = iflag;
        this.work = work;
        this.iwork = iwork;
    }
    
    public double[] getY() {
    	return y;
    }
    public double getT() {
    	return t[0];
    }
    
    public int getIflag() {
        return iflag[0];	
    }
    
    public double getRelerr() {
        return relerr[0];	
    }
    
    public String getErrorMessage() {
    	String message = null;
    	if (iflag[0] == 2) {
    		message = new String("In ODE normal return.  Integration reached tout");
    	}
    	else if (iflag[0] == 3) {
    		message = new String("In ODE integration did not reach tout because error\n" +
    			    "tolerances too small.  relerr increased to " + relerr[0] + "\n" +
    				"abserr increased to " + abserr[0] + "\n");
    	}
    	else if (iflag[0] == 4) {
    		message = new String("In ODE integration did not reach tout because more than\n" + 
                       "500 steps needed\n");
    	}
    	else if (iflag[0] == 5) {
    		message = new String("In ODE integration did not reach tout because equations\n" + 
                       "appear to be stiff\n");
    	}
    	else if (iflag[0] == 6) {
    		message = new String("In ODE invalid input parameters (fatal error)\n");
    	}
    	return message;
    }
    
    public double getAbserr() {
    	return abserr[0];
    }
     public void driver() {   
        int i;
    	int j;

        if (Math.abs(iflag[0]) != 1) {
            start = (work[istart] > 0.0);
            phase1 = (work[iphase] > 0.0);
            nornd = iwork[1] != -1;
        }
        double phi[][] = new double[neqn][16];
        // While Java is row major order, FORTRAN is column major order
        for (j =  0; j < 16; j++) {
        	for (i = 0; i < neqn; i++) {
        		phi[i][j] = work[iphi + i + j*neqn];
        	}
        }
        de(phi);
        for (j =  0; j < 16; j++) {
        	for (i = 0; i < neqn; i++) {
        		work[iphi + i + j*neqn] = phi[i][j];
        	}
        }
        work[istart] = -1.0;
        if (start) {
        	work[istart] = 1.0;
        }
        work[iphase] = -1.0;
        if (phase1) {
        	work[iphase] = 1.0;
        }
        iwork[1] = -1;
        if (nornd) {
        	iwork[1] = 1;
        }
        return;
    }
    
    private void de(double phi[][]) {
    	//
    	//   ode  merely allocates storage for  de  to relieve the user of the
    	//   inconvenience of a long call list.  consequently  de  is used as
    	//   described in the comments for  ode .
    	//
    	//   this code is completely explained and documented in the text,
    	//   computer solution of ordinary differential equations:  the initial
    	//   value problem  by l. f. shampine and m. k. gordon.
    	//
    	boolean stiff;
    	boolean crash[] = new boolean[1];
    	double eps[] = new double[1];
    	int i;
    	int l;
    	//      logical stiff,crash,start,phase1,nornd
    	//      dimension y(neqn),yy(neqn),wt(neqn),phi(neqn,16),p(neqn),yp(neqn),
    	//     1  ypout(neqn),psi(12),alpha(12),beta(12),sig(13),v(12),w(12),g(13)
    	//      external f
    	//
    	//***********************************************************************
    	//*  the only machine dependent constant is based on the machine unit   *
    	//*  roundoff error  u  which is the smallest positive number such that *
    	//*  1.0+u .gt. 1.0 .  u  must be calculated and  fouru=4.0*u  inserted *
    	//*  in the following data statement before using  de .  the routine    *
    	//*  machin  calculates  u .  fouru  and  twou=2.0*u  must also be      *
    	//*  inserted in subroutine  step  before calling  de .                 *
    	//     data fouru/.888d-15/                                              ***
    	//***********************************************************************
    	//
    	//   the constant  maxnum  is the maximum number of steps allowed in one
    	//   call to  de .  the user may change this limit by altering the
    	//   following statement
    	int maxnum = 500;
    	//
    	//            ***            ***            ***
    	//   test for improper parameters
    	//
    	// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        double epsilon = 1.0;
        double neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)
        
        double fouru = 4.0 * epsilon;
        if (neqn < 1) {
        	iflag[0] = 6;
        	return;
        }
        if (t[0] == tout) {
        	iflag[0] = 6;
        	return;
        }
    	if ((relerr[0] < 0.0) || (abserr[0] < 0.0)) {
    		iflag[0] = 6;
    		return;
    	}
    	eps[0] = Math.max(relerr[0], abserr[0]);
    	if (eps[0] <= 0.0) {
    		iflag[0] = 6;
    		return;
    	}
    	if (iflag[0] == 0) {
    		iflag[0] = 6;
    		return;
    	}
    	int isn;
    	if (iflag[0] >= 0) {
    	    isn = 1;	
    	}
    	else {
    		isn = -1;
    	}
    	iflag[0] = Math.abs(iflag[0]);
    	if (iflag[0] != 1) {
    		if (t[0] != work[itold]) {
    			iflag[0] = 6;
    			return;
    		}
    		if ((iflag[0] < 2) || (iflag[0] > 5)) {
    			iflag[0] = 6;
    			return;
    		}
    	} // if (iflag[0] != 1)
    	//
    	//   on each call set interval of integration and counter for number of
    	//   steps.  adjust input error tolerances to define weight vector for
    	//   subroutine  step
    	//
    	
    	double del = tout - t[0];
    	double absdel = Math.abs(del);
    	double tend = t[0] + 10.0*del;
    	if (isn < 0.0) {
    		tend = tout;
    	}
    	int nostep = 0;
    	int kle4 = 0;
    	stiff = false;
    	double releps = relerr[0]/eps[0];
    	double abseps = abserr[0]/eps[0];
    	if ((iflag[0] == 1) || (iwork[isnold] < 0) || (work[idelsn]*del <= 0.0)) {
    		//
    		//   on start and restart also set work variables x and yy(*), store the
    		//   direction of integration and initialize the step size
    		//
    		start = true;
    		work[ix] = t[0];
    		for (l = 0; l < neqn; l++) {
    			work[iyy+l] = y[l];
    		}
    		if (del >= 0.0) {
    		    work[idelsn] = 1.0;	
    		}
    		else {
    			work[idelsn] = -1.0;
    		}
    		double maxVal = Math.max(Math.abs(tout-work[ix]), fouru*Math.abs(work[ix]));
    		if ((tout - work[ix])>= 0.0) {
    			work[ih] = maxVal;
    		}
    		else {
    			work[ih] = -maxVal;
    		}
    	} // if ((iflag[0] == 1) || (iwork[isnold] < 0) || (work[idelsn]*del <= 0.0))
    	//
    	// if already past output point, interpolate and return
    	//
    	while (true) {
	    	if (Math.abs(work[ix] - t[0]) >= absdel) {
	    	    intrp(phi);
	    	    iflag[0] = 2;
	    	    t[0] = tout;
	    	    work[itold] = t[0];
	    	    iwork[isnold] = isn;
	    	    return;
	    	} // if (Math.abs(work[ix] - t[0]) >= absdel)
	    	//
	    	// if cannot go past output point and sufficiently close,
	    	// extrapolate and return
	    	//
	    	if ((isn <= 0) && (Math.abs(tout - work[ix]) < fouru*Math.abs(work[ix]))) {
	    	    work[ih] = tout - work[ix];	
	    	    double yy[] = new double[neqn];
	    	    for (i = 0; i < neqn; i++) {
	    	    	yy[i] = work[iyy + i];
	    	    }
	    	    double yp[] = new double[neqn];
	    	    f(work[ix], yy, yp);
	    	    for (i = 0; i < neqn; i++) {
	    	    	work[iyp + i] = yp[i];
	    	    }
	    	    for (l = 0; l < neqn; l++) {
	    	        y[l] = work[iyy+l]+ work[ih]*work[iyp+l];
	    	    }
	    	    iflag[0] = 2;
	    	    t[0] = tout;
	    	    work[itold] = t[0];
	    	    iwork[isnold] = isn;
	    	    return;
	    	} // if ((isn <= 0) && (Math.abs(tout - work[ix]) < fouru*Math.abs(work[ix])))
	    	//
	    	// test for too many steps
	    	//
	    	if (nostep >= maxnum) {
	    		iflag[0] = isn * 4;
	    		if (stiff) {
	    			iflag[0] = isn * 5;
	    		}
	    		for (l = 0; l < neqn; l++) {
	    			y[l] = work[iyy +l];
	    		}
	    		t[0] = work[ix];
	    		work[itold] = t[0];
	    		iwork[isnold] = 1;
	    		return;
	    	} // if (nostep >= maxnum)
	    	//
	    	// limit step size, set weight vector and take a step
	    	//
	    	double minVal = Math.min(Math.abs(work[ih]), Math.abs(tend-work[ix]));
	    	if (work[ih] >= 0) {
	    		work[ih] = minVal;
	    	}
	    	else {
	    		work[ih] = -minVal;
	    	}
	    	for (l = 0; l < neqn; l++) {
	    		work[iwt+l] = releps * Math.abs(work[iyy+l]) + abseps;
	    	}
	    	step(eps, crash, phi);
	    	//
	    	// test for tolerances not too small
	    	//
	    	if (crash[0]) {
	    		iflag[0] = isn * 3;
	    		relerr[0] = eps[0]*releps;
	    		abserr[0] = eps[0]*abseps;
	    		for (l = 0; l < neqn; l++) {
	    		    y[l] = work[iyy + l];	
	    		}
	    		t[0] = work[ix];
	    		work[itold] = t[0];
	    		iwork[isnold] = 1;
	    		return;
	    	} // if (crash[0])
	    	
	    	//
	    	// augment counter on number of steps and test for stiffness
	    	//
	    	nostep = nostep + 1;
	    	kle4 = kle4 + 1;
	    	if (iwork[kold] > 4) {
	    		kle4 = 0;
	    	}
	    	if (kle4 >= 50) {
	    		stiff = true;
	    	}
    	} // while (true)
    }
    
    public void step(double eps[], boolean crash[], double phi[][]) {
    	//
    	//   double precision subroutine  step
    	//   integrates a system of first order ordinary
    	//   differential equations one step, normally from x to x+h, using a
    	//   modified divided difference form of the adams pece formulas.  local
    	//   extrapolation is used to improve absolute stability and accuracy.
    	//   the code adjusts its order and step size to control the local error
    	//   per unit step in a generalized sense.  special devices are included
    	//   to control roundoff error and to detect when the user is requesting
    	//   too much accuracy.
    	//
    	//   this code is completely explained and documented in the text,
    	//   computer solution of ordinary differential equations:  the initial
    	//   value problem  by l. f. shampine and m. k. gordon.
    	//
    	//
    	//   the parameters represent:
    	//      x -- independent variable             (real*8)
    	//      y(*) -- solution vector at x          (real*8)
    	//      yp(*) -- derivative of solution vector at  x  after successful
    	//           step                             (real*8)
    	//      neqn -- number of equations to be integrated (integer*4)
    	//      h -- appropriate step size for next step.  normally determined by
    	//           code                             (real*8)
    	//      eps -- local error tolerance.  must be variable  (real*8)
    	//      wt(*) -- vector of weights for error criterion   (real*8)
    	//      start -- logical variable set .true. for first step,  .false.
    	//           otherwise                        (logical*4)
    	//      hold -- step size used for last successful step  (real*8)
    	//      k -- appropriate order for next step (determined by code)
    	//      kold -- order used for last successful step
    	//      crash -- logical variable set .true. when no step can be taken,
    	//           .false. otherwise.
    	//   the arrays  phi, psi  are required for the interpolation subroutine
    	//   intrp.  the array p is internal to the code.  all are real*8
    	//
    	//   input to  step
    	//
    	//      first call --
    	//
    	//   the user must provide storage in his driver program for all arrays
    	//   in the call list, namely
    	//
    	//     dimension y(neqn),wt(neqn),phi(neqn,16),p(neqn),yp(neqn),psi(12)
    	//
    	//   the user must also declare  start  and  crash  logical variables
    	//   and  f  an external subroutine, supply the subroutine  f(x,y,yp)
    	//   to evaluate
    	//      dy(i)/dx = yp(i) = f(x,y(1),y(2),...,y(neqn))
    	//   and initialize only the following parameters:
    	//      x -- initial value of the independent variable
    	//      y(*) -- vector of initial values of dependent variables
    	//      neqn -- number of equations to be integrated
    	//      h -- nominal step size indicating direction of integration
    	//           and maximum size of step.  must be variable
    	//      eps -- local error tolerance per step.  must be variable
    	//      wt(*) -- vector of non-zero weights for error criterion
    	//      start -- .true.
    	//
    	//   step  requires the l2 norm of the vector with components
    	//   local error(l)/wt(l)  be less than  eps  for a successful step.  the
    	//   array  wt  allows the user to specify an error test appropriate
    	//   for his problem.  for example,
    	//      wt(l) = 1.0  specifies absolute error,
    	//            = dabs(y(l))  error relative to the most recent value of
    	//                 the l-th component of the solution,
    	//            = dabs(yp(l))  error relative to the most recent value of
    	//                 the l-th component of the derivative,
    	//            = dmax1(wt(l),dabs(y(l)))  error relative to the largest
    	//                 magnitude of l-th component obtained so far,
    	//            = dabs(y(l))*relerr/eps + abserr/eps  specifies a mixed
    	//                 relative-absolute test where  relerr  is relative
    	//                 error,  abserr  is absolute error and  eps =
    	//                 dmax1(relerr,abserr) .
    	//
    	//      subsequent calls --
    	//
    	//   subroutine  step  is designed so that all information needed to
    	//   continue the integration, including the step size  h  and the order
    	//   k , is returned with each step.  with the exception of the step
    	//   size, the error tolerance, and the weights, none of the parameters
    	//   should be altered.  the array  wt  must be updated after each step
    	//   to maintain relative error tests like those above.  normally the
    	//   integration is continued just beyond the desired endpoint and the
    	//   solution interpolated there with subroutine  intrp .  if it is
    	//   impossible to integrate beyond the endpoint, the step size may be
    	//   reduced to hit the endpoint since the code will not take a step
    	//   larger than the  h  input.  changing the direction of integration,
    	//   i.e., the sign of  h , requires the user set  start = .true. before
    	//   calling  step  again.  this is the only situation in which  start
    	//   should be altered.
    	//
    	//   output from  step
    	//
    	//      successful step --
    	//
    	//   the subroutine returns after each successful step with  start  and
    	//   crash  set .false. .  x  represents the independent variable
    	//   advanced one step of length  hold  from its value on input and  y
    	//   the solution vector at the new value of  x .  all other parameters
    	//   represent information corresponding to the new  x  needed to
    	//   continue the integration.
    	//
    	//      unsuccessful step --
    	//
    	//   when the error tolerance is too small for the machine precision,
    	//   the subroutine returns without taking a step and  crash = .true. .
    	//   an appropriate step size and error tolerance for continuing are
    	//   estimated and all other information is restored as upon input
    	//   before returning.  to continue with the larger tolerance, the user
    	//   just calls the code again.  a restart is neither required nor
    	//   desirable.
    	//
    	//      logical start,crash,phase1,nornd
    	//      dimension y(neqn),wt(neqn),phi(neqn,16),p(neqn),yp(neqn),psi(12)
    	//      dimension alpha(12),beta(12),sig(13),w(12),v(12),g(13),
    	//     1  gstr(13),two(13)
    	//      external f
    	double gstr[] = new double[13];
    	double two[] = new double[13];
    	//***********************************************************************
    	//*  the only machine dependent constants are based on the machine unit *
    	//*  roundoff error  u  which is the smallest positive number such that *
    	//*  1.0+u .gt. 1.0  .  the user must calculate  u  and insert          *
    	//*  twou=2.0*u  and  fouru=4.0*u  in the data statement before calling *
    	//*  the code.  the routine  machin  calculates  u .                    *
    	//     data twou,fouru/.444d-15,.888d-15/                                ***
    	//***********************************************************************	
    }
    
    public abstract void f(double t, double y[], double yp[]);
    
    private void intrp(double phi[][]) {
    	//
    	//   the methods in subroutine  step  approximate the solution near  x (work[ix])
    	//   by a polynomial.  subroutine  intrp  approximates the solution at
    	//   xout (tout) by evaluating the polynomial there.  information defining this
    	//   polynomial is passed from  step  so  intrp  cannot be used alone.
    	//
    	//   this code is completely explained and documented in the text,
    	//   computer solution of ordinary differential equations:  the initial
    	//   value problem  by l. f. shampine and m. k. gordon.
    	//
    	//   input to intrp --
    	//
    	//   all floating point variables are double precision
    	//   the user provides storage in the calling program for the arrays in
    	//   the call list
    	//       dimension y(neqn),yout(neqn),ypout(neqn),phi(neqn,16),psi(12)
    	// y = work[iyy]
    	// yout = y
    	// ypout = work[iypout]
    	// psi = work[ipsi]
    	//   and defines
    	//      xout (tout) -- point at which solution is desired.
    	//   the remaining parameters are defined in  step  and passed to  intrp
    	//   from that subroutine
    	//
    	//   output from  intrp --
    	//
    	//      yout(*) (y)-- solution at  xout (tout)
    	//       ypout(*) (work[iypout]) -- derivative of solution at  xout (tout)
    	//   the remaining parameters are returned unaltered from their input
    	//   values.  integration with  step  may be continued.
    	//
    	int i;
    	int j;
    	int l;
    	int jm1;
    	double psijm1;
    	double gamma;
    	double eta;
    	int limit1;
    	double temp2;
    	double temp3;
    	double g[] = new double[13];
    	double w[] = new double[13];
    	double rho[] = new double[13];
    	g[0] = 1.0;
    	rho[0] = 1.0;
    	
    	double hi = tout - work[ix];
    	int ki = iwork[kold] + 1;
    	int kip1 = ki + 1;
    	//
    	// initialize w[] for computing g[]
    	//
    	for (i = 1; i <= ki; i++) {
    		w[i-1] = 1.0/(double)i;
    	}
    	double term = 0.0;
    	//
    	// compute g[]
    	//
    	for (j = 2; j <= ki; j++) {
    	    jm1 = j - 1;
    	    psijm1 = work[ipsi + jm1];
    	    gamma = (hi + term)/psijm1;
    	    eta = hi/psijm1;
    	    limit1 = kip1 - j;
    	    for (i = 0; i < limit1; i++) {
    	    	w[i] = gamma*w[i] - eta*w[i+1];
    	    }
    	    g[j-1] = w[0];
    	    rho[j-1] = gamma*rho[jm1-1];
    	    term = psijm1;
    	} // for (j = 2; j <= ki; j++)
    	//
    	// interpolate
    	//
    	for (l = 0; l < neqn; l++) {
    		work[iypout+l] = 0.0;
    		y[l] = 0.0;
    	} // for (l = 0; l < neqn; l++)
    	for (j = 1; j <= ki; j++) {
    	    i = kip1 - j;
    	    temp2 = g[i-1];
    	    temp3 = rho[i-1];
    	    for (l = 0; l < neqn; l++) {
    	        y[l] = y[l] + temp2*phi[l][i-1];
    	        work[iypout+l] = work[iypout+l] + temp3*phi[l][i-1];
    	    } // for (l = 0; l < neqn; l++)
    	} // for (j = 1; j <= ki; j++)
    	for (l = 0; l < neqn; l++) {
    	    y[l] = work[iyy+l] + hi*y[l];	
    	} // for (l = 0; l < neqn; l++)
    	return;
    }
}