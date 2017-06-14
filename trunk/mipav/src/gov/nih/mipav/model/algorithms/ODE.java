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
    // internal int iwork[][] = new int[5][1]
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
        
        
        if (Math.abs(iflag[0]) != 1) {
            start = (work[istart] > 0.0);
            phase1 = (work[iphase] > 0.0);
            nornd = iwork[1] != -1;
        }
        de();
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
    
    private void de() {
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
    	boolean crash;
    	double x;
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
    	double eps = Math.max(relerr[0], abserr[0]);
    	if (eps <= 0.0) {
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
    	double releps = relerr[0]/eps;
    	double abseps = abserr[0]/eps;
    	if ((iflag[0] == 1) || (iwork[isnold] < 0) || (work[idelsn]*del <= 0.0)) {
    		//
    		//   on start and restart also set work variables x and yy(*), store the
    		//   direction of integration and initialize the step size
    		//
    		start = true;
    		x = t[0];
    		for (l = 0; l < neqn; l++) {
    			work[iyy+l] = y[l];
    		}
    	}
    }
}