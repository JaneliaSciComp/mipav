package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.AlgorithmCircleToRectangle.ODEtest;
import gov.nih.mipav.view.Preferences;

public abstract class ODE {
	
	 // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
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
    
    // For work:
    private double alpha[] = new double[12];
    private double beta[] = new double[12];
    private double sig[] = new double[13];
    private double v[] = new double[12];
    private double w[] = new double[12];
    private double g[] = new double[13];
    private double phase;
    private double psi[] = new double[12];
    private double x;
    private double h;
    private double hold;
    private double istart;
    private double told;
    private double delsn;
    private double yy[];
    private double wt[];
    private double p[];
    private double yp[];
    private double ypout[];
    private double phi[][];
    
    // For iwork
    private int ns;
    private int iwork1;
    private int k;
    private int kold;
    private int isnold;
    
    private boolean start;
    private boolean phase1;
    private boolean nornd;
    
    private double twou;
    private double fouru;
    //   the constant  maxnum  is the maximum number of steps allowed in one
   	//   call to  de .  the user may change this limit by altering the
   	//   following statement
    private int maxnum = 2000;
    
    private boolean testMode = false;
    
    private int testCase;
    
    private final int ENRIGHT_AND_PRYCE_A1 = 0;
    private final int ENRIGHT_AND_PRYCE_A2 = 1;
    private final int ENRIGHT_AND_PRYCE_A3 = 2;
    private final int ENRIGHT_AND_PRYCE_A4 = 3;
    private final int ENRIGHT_AND_PRYCE_A5 = 4;
    private final int ENRIGHT_AND_PRYCE_B1 = 5;
    
    /**
     * Creates a new ODE object.
     * Test with:
     * new ODEtest();
     * class ODEtest extends ODE {
    	  public ODEtest() {
    		  super();
    	  }
    	
    	  public void f(double x, double yy[], double yp[]) {
    		
    	  }
      }
     */
    public ODE() {
        int i;
    	testMode = true;
    	testCase = ENRIGHT_AND_PRYCE_A1;
    	Preferences.debug("Enright andPryce #A1 neqn = 1 y' = -y Exponential decay\n");
    	Preferences.debug("y[0] = 1\n");
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	for (i = 0; i < 10; i++) {
    		y[0] = 1;
    		t[0] = 0;
    		tout = 2.0*(i+1);
    		relerr[0] = 4.441E-16;
    		abserr[0] = 4.441E-16;
    		//relerr[0] = 1.0E-16;
    		//abserr[0] = 1.0E-16;
    		// If set error tolerances to 1.0E-16, receive message:
    		//In ODE integration did not reach tout because error
    		//tolerances too small.  relerr increased to 4.44089209850063E-16
    		//abserr increased to 4.44089209850063E-16
    		iflag[0] = 1;
    		clearArrays();
    		driver();
    		Preferences.debug(getErrorMessage());
    		Preferences.debug("Actual value = " + Math.exp(-tout) + 
    				" Calculated value = " + y[0] + "\n");
    		Preferences.debug("Final time = " + t[0] + "\n");
    		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
    	} // for (i = 0; i < 10; i++)
    	
    	testCase = ENRIGHT_AND_PRYCE_A2;
    	Preferences.debug("Enright and Pryce #A2 neqn = 1 y' = -(y^3)/2\n");
    	Preferences.debug("y[0] = 1\n");
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 0.218218"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_A3;
    	Preferences.debug("Enright and Pryce #A3 neqn = 1 y' = cost(t)*y\n");
    	Preferences.debug("y[0] = 1\n");
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 2.49165"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_A4;
    	Preferences.debug("Enright and Pryce #A4 neqn = 1 y' = y*(20-y)/80\n");
    	Preferences.debug("y[0] = 1\n");
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 17.7302"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_A5;
    	Preferences.debug("Enright and Pryce #A5 neqn = 1 y' = (y-t)/(y+t)\n");
    	Preferences.debug("y[0] = 4.0\n");
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 4.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -0.788783"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B1;
    	Preferences.debug("Enright and Pryce #B1 neqn = 2 y1' 2*y1*(1-y2)\n");
    	Preferences.debug("y2' = -y2*(1-y1)\n");
    	Preferences.debug("y[0] = 1.0  y[1] = 3.0\n");
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1.0;
    	y[1] = 3.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 0.676188"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 0.186082"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
    	
    }
    
    private void allocateArrays() {
    	 yy = new double[neqn];
         wt = new double[neqn];
         p = new double[neqn];
         yp = new double[neqn];
         ypout = new double[neqn];
         phi = new double[neqn][16];	
    }
    
    private void clearArrays() {
    	int i;
    	int j;
    	for (i = 0; i < 12; i++) {
    		alpha[i] = 0.0;
    		beta[i] = 0.0;
    		v[i] = 0.0;
    		w[i] = 0.0;
    		psi[i] = 0.0;
    	}
    	for (i = 0; i < 13; i++) {
    		sig[i] = 0.0;
    		g[i] = 0.0;
    	}
    	for (i = 0; i < neqn; i++) {
    		yy[i] = 0.0;
    		wt[i] = 0.0;
    		p[i] = 0.0;
    		yp[i] = 0.0;
    		ypout[i] = 0.0;
    		for (j = 0; j < 16; j++) {
    			phi[i][j] = 0.0;
    		}
    	}
    }
    
    private void fTestMode(double x, double yy[], double yp[]) {
        switch(testCase) {
        case ENRIGHT_AND_PRYCE_A1:
        	yp[0] = -yy[0];
        	break;
        case ENRIGHT_AND_PRYCE_A2:
        	yp[0] = -Math.pow(yy[0], 3.0)/2.0;
        	break;
        case ENRIGHT_AND_PRYCE_A3:
        	yp[0] = Math.cos(x)*yy[0];
        	break;
        case ENRIGHT_AND_PRYCE_A4:
        	yp[0] = yy[0]*(20.0-yy[0])/80.0;
        	break;
        case ENRIGHT_AND_PRYCE_A5:
        	yp[0] = (yy[0] - x)/(yy[0] + x);
        	break;
        case ENRIGHT_AND_PRYCE_B1:
        	yp[0] = 2*yy[0]*(1 - yy[1]);
        	yp[1] = -yy[1]*(1 - yy[0]);
        	break;
        }
    }
    
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
    		double abserr[], int iflag[]) {
    	        this.neqn = neqn;
        this.y = y;
        this.t = t;
        this.tout = tout;
        this.relerr = relerr;
        this.abserr = abserr;
        this.iflag = iflag;
        yy = new double[neqn];
        wt = new double[neqn];
        p = new double[neqn];
        yp = new double[neqn];
        ypout = new double[neqn];
        phi = new double[neqn][16];
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
    		message = new String("In ODE normal return.  Integration reached tout\n");
    	}
    	else if (iflag[0] == 3) {
    		message = new String("In ODE integration did not reach tout because error\n" +
    			    "tolerances too small.  relerr increased to " + relerr[0] + "\n" +
    				"abserr increased to " + abserr[0] + "\n");
    	}
    	else if (iflag[0] == 4) {
    		message = new String("In ODE integration did not reach tout because more than\n" + 
                       maxnum + " steps needed\n");
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
        
        twou = 2.0 * epsilon;
        fouru = 4.0 * epsilon;

        if (Math.abs(iflag[0]) != 1) {
            start = (istart > 0.0);
            phase1 = (phase > 0.0);
            nornd = iwork1 != -1;
        }
       
        de();
        
        istart = -1.0;
        if (start) {
        	istart = 1.0;
        }
        phase = -1.0;
        if (phase1) {
        	phase = 1.0;
        }
        iwork1 = -1;
        if (nornd) {
        	iwork1 = 1;
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
    	boolean crash[] = new boolean[1];
    	double eps[] = new double[1];
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
    	//
    	//            ***            ***            ***
    	//   test for improper parameters
    	//
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
    		if (t[0] != told) {
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
    	if ((iflag[0] == 1) || (isnold < 0) || (delsn*del <= 0.0)) {
    		//
    		//   on start and restart also set work variables x and yy(*), store the
    		//   direction of integration and initialize the step size
    		//
    		start = true;
    		x = t[0];
    		for (l = 0; l < neqn; l++) {
    			yy[l] = y[l];
    		}
    		if (del >= 0.0) {
    		   delsn = 1.0;	
    		}
    		else {
    			delsn = -1.0;
    		}
    		double maxVal = Math.max(Math.abs(tout-x), fouru*Math.abs(x));
    		if ((tout - x)>= 0.0) {
    			h = maxVal;
    		}
    		else {
    			h = -maxVal;
    		}
    	} // if ((iflag[0] == 1) || (isnold < 0) || (delsn*del <= 0.0))
    	//
    	// if already past output point, interpolate and return
    	//
    	while (true) {
	    	if (Math.abs(x - t[0]) >= absdel) {
	    	    intrp();
	    	    iflag[0] = 2;
	    	    t[0] = tout;
	    	    told = t[0];
	    	    isnold = isn;
	    	    return;
	    	} // if (Math.abs(x - t[0]) >= absdel)
	    	//
	    	// if cannot go past output point and sufficiently close,
	    	// extrapolate and return
	    	//
	    	if ((isn <= 0) && (Math.abs(tout - x) < fouru*Math.abs(x))) {
	    	    h = tout - x;	
	    	    if (testMode) {
	    	    	fTestMode(x, yy, yp);
	    	    }
	    	    else {
	    	        f(x, yy, yp);
	    	    }
	    	    for (l = 0; l < neqn; l++) {
	    	        y[l] = yy[l]+ h*yp[l];
	    	    }
	    	    iflag[0] = 2;
	    	    t[0] = tout;
	    	    told = t[0];
	    	    isnold = isn;
	    	    return;
	    	} // if ((isn <= 0) && (Math.abs(tout - x) < fouru*Math.abs(x)))
	    	//
	    	// test for too many steps
	    	//
	    	if (nostep >= maxnum) {
	    		iflag[0] = isn * 4;
	    		if (stiff) {
	    			iflag[0] = isn * 5;
	    		}
	    		for (l = 0; l < neqn; l++) {
	    			y[l] = yy[l];
	    		}
	    		t[0] = x;
	    		told = t[0];
	    		isnold = 1;
	    		return;
	    	} // if (nostep >= maxnum)
	    	//
	    	// limit step size, set weight vector and take a step
	    	//
	    	double minVal = Math.min(Math.abs(h), Math.abs(tend-x));
	    	if (h >= 0) {
	    		h = minVal;
	    	}
	    	else {
	    		h = -minVal;
	    	}
	    	for (l = 0; l < neqn; l++) {
	    		wt[l] = releps * Math.abs(yy[l]) + abseps;
	    	}
	    	step(eps, crash);
	    	//
	    	// test for tolerances not too small
	    	//
	    	if (crash[0]) {
	    		iflag[0] = isn * 3;
	    		relerr[0] = eps[0]*releps;
	    		abserr[0] = eps[0]*abseps;
	    		for (l = 0; l < neqn; l++) {
	    		    y[l] = yy[l];	
	    		}
	    		t[0] = x;
	    		told = t[0];
	    		isnold = 1;
	    		return;
	    	} // if (crash[0])
	    	
	    	//
	    	// augment counter on number of steps and test for stiffness
	    	//
	    	nostep = nostep + 1;
	    	kle4 = kle4 + 1;
	    	if (kold > 4) {
	    		kle4 = 0;
	    	}
	    	if (kle4 >= 50) {
	    		stiff = true;
	    	}
    	} // while (true)
    }
    
    public void step(double eps[], boolean crash[]) {
    	//
    	//   double precision subroutine  step
    	//   integrates a system of first order ordinary
    	//   differential equations one step, normally from x to x+h work[ih], using a
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
    	//      x (work[ix]) -- independent variable             (real*8)
    	//      y(*)(work[iyy]) -- solution vector at x          (real*8)
    	//      yp(*) (work[iyp])-- derivative of solution vector at  x (work[ix]) after successful
    	//           step                             (real*8)
    	//      neqn -- number of equations to be integrated (integer*4)
    	//      h (work[ih])-- appropriate step size for next step.  normally determined by
    	//           code                             (real*8)
    	//      eps -- local error tolerance.  must be variable  (real*8)
    	//      wt(*)(work[iwt]) -- vector of weights for error criterion   (real*8)
    	//      start -- logical variable set .true. for first step,  .false.
    	//           otherwise                        (logical*4)
    	//      hold work[ihold])-- step size used for last successful step  (real*8)
    	//      k (iwork[k])-- appropriate order for next step (determined by code)
    	//      kold (iwork[kold])-- order used for last successful step
    	//      crash -- logical variable set .true. when no step can be taken,
    	//           .false. otherwise.
    	//   the arrays  phi, psi (work[ipsi])  are required for the interpolation subroutine
    	//   intrp.  the array p (work[ip]) is internal to the code.  all are real*8
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
    	boolean do450;
    	boolean do455;
    	int i;
    	int ifail;
    	int ip1;
    	int iq;
    	int j;
    	int km1;
    	int km2;
    	int knew;
    	int kp1;
    	int kp2;
    	int l;
    	int limit2;
    	double absh;
    	double erk;
    	double erkm1;
    	double erkm2;
    	double erkp1;
    	double err;
    	double hnew;
    	double r;
    	double rho;
    	double tau;
    	double temp1;
    	double temp2;
    	double temp3;
    	double temp4;
    	double temp5;
    	double temp6;
    	double var;
    	double xold;
    	double gstr[] = new double[]{0.500,0.0833,0.0417,0.0264,0.0188,0.0143,
    			0.0114,0.00936,0.00789,0.00679,0.00592,0.00524,0.00468};
    	double two[] = new double[]{2.0,4.0,8.0,16.0,32.0,64.0,128.0,256.0,
    			512.0,1024.0,2048.0,4096.0,8192.0};
  
    	//***********************************************************************
    	//*  the only machine dependent constants are based on the machine unit *
    	//*  roundoff error  u  which is the smallest positive number such that *
    	//*  1.0+u .gt. 1.0  .  the user must calculate  u  and insert          *
    	//*  twou=2.0*u  and  fouru=4.0*u  in the data statement before calling *
    	//*  the code.  the routine  machin  calculates  u .                    *
    	//     data twou,fouru/.444d-15,.888d-15/                                ***
    	//***********************************************************************
    	//       ***     begin block 0     ***
    	//   check if step size or error tolerance is too small for machine
    	//   precision.  if first step, initialize phi array and estimate a
    	//   starting step size.
    	//                   ***
    	//
    	//   if step size is too small, determine an acceptable one
    	//
    	crash[0] = true;
    	if (Math.abs(h) < fouru*Math.abs(x)) {
    		if (h >= 0) {
    			h = fouru*Math.abs(x);
    		}
    		else {
    			h = -fouru*Math.abs(x);
    		}
    		return;
    	} // if (Math.abs(h]) < fouru*Math.abs(x))
    	double p5eps = 0.5 * eps[0];
    	//
    	// if step size is too small, determine an acceptable one
    	//
    	double round = 0.0;
    	for (l = 0; l < neqn; l++) {
    		var = (yy[l]/wt[l]);
    	    round = round + var*var;	
    	} // for (l = 0; l < neqn; l++)
    	round = twou * Math.sqrt(round);
    	if (p5eps < round) {
    		eps[0] = 2.0*round*(1.0 + fouru);
    		return;
    	} // if (p5eps < round)
    	crash[0] = false;
    	g[0] = 1.0;
    	g[1] = 0.5;
    	sig[0] = 1.0;
    	if (start) {
    	    //
    		// initialize.  Compute appropriate step size for first step
    		//
    	    if (testMode) {
    	    	fTestMode(x, yy, yp);
    	    }
    	    else {
    	        f(x, yy, yp);
    	    }
    	    double sum = 0.0;
    	    for (l = 0; l < neqn; l++) {
    	    	phi[l][0] = yp[l];
    	    	phi[l][1] = 0.0;
    	    	var = (yp[l]/wt[l]);
    	    	sum = sum + var*var;
    	    } // for (l = 0; l < neqn; l++)
    	    sum = Math.sqrt(sum);
    	    absh = Math.abs(h);
    	    if (eps[0] < 16.0*sum*h*h) {
    	    	absh = 0.25*Math.sqrt(eps[0]/sum);
    	    }
    	    if (h >= 0) {
    	    	h = Math.max(absh, fouru*Math.abs(x));
    	    }
    	    else {
    	    	h = -Math.max(absh, fouru*Math.abs(x));	
    	    }
    	    hold = 0.0;
    	    k = 1;
    	    kold = 0;
    	    start = false;
    	    phase1 = true;
    	    nornd = true;
    	    if (p5eps <= 100.0*round) {
    	        nornd = false;
    	        for (l = 0; l < neqn; l++) {
    	        	phi[l][14] = 0.0;
    	        }
    	    } // if (p5eps <= 100.0*round)
    	} // if (start)
    	ifail = 0;
    	//       ***     end block 0     ***
    	//
    	//       ***     begin block 1     ***
    	//   compute coefficients of formulas for this step.  avoid computing
    	//   those quantities not changed when step size is not changed.
    	//                   ***
    	//
    	while (true) {
	    	kp1 = k+1;
	    	kp2 = k+2;
	    	km1 = k-1;
	    	km2 = k-2;
	    	//
	    	//   ns is the number of steps taken with size h, including the current
	    	//   one.  when k.lt.ns, no coefficients change
	    	//
	    	if (h != hold) {
	    		ns = 0;
	    	}
	    	if (ns <= kold) {
	    		ns = ns + 1;
	    	}
	    	int nsp1 = ns + 1;
	    	if (k >= ns) {
	    		//
	    		//   compute those components of alpha(*),beta(*),psi(*),sig(*) which
	    		//   are changed
	    		//
	    		beta[ns - 1] = 1.0;
	    		double realns = (double)ns;
	    		alpha[ns - 1] = 1.0/realns;
	    		temp1 = h * realns;
	    		sig[nsp1 - 1] = 1.0;
	    		if (k >= nsp1) {
	    		    for (i = nsp1; i <= k; i++) {
	    		        int im1 = i - 1;
	    		        temp2 = psi[im1 - 1];
	    		        psi[im1 -1] = temp1;
	    		        beta[i - 1] = beta[im1-1]*psi[im1-1]/temp2;
	    		        temp1 = temp2 + h;
	    		        alpha[i - 1] = h/temp1;
	    		        double reali = (double)i;
	    		        sig[i] = reali*alpha[i - 1]*sig[i - 1];
	    		    } // for (i = nsp1; i <= k; i++)
	    		} // if (k >= nsp1)
	    		psi[k - 1] = temp1;
	    		//
	    		//   compute coefficients g(*)
	    		//
	    		//   initialize v(*) and set w(*).  g(2) is set in data statement
	    		//
	    		if (ns <= 1) {
	    		    for (iq = 1; iq <= k; iq++) {
	    		        temp3 = iq*(iq+1);
	    		        v[iq - 1] = 1.0/temp3;
	    		        w[iq - 1] = v[iq - 1];
	    		    } // for (iq = 1; iq <= k; iq++)
	    		} // if (ns <= 1)
	    		else {
	    			//
	    			// if order was raised, update diagonal part of v(*)
	    			//
		    		if (k > kold) {
		    		    temp4 = k*kp1;
		    		    v[k - 1] = 1.0/temp4;
		    		    int nsm2 = ns - 2;
		    		    if (nsm2 >= 1) {
		    		    	for (j = 1; j <= nsm2; j++) {
		    		    	    i = k - j;
		    		    	    v[i - 1] = v[i - 1] - alpha[j]*v[i];
		    		    	} // for (j = 1; j <= nsm2; j++)
		    		    } // if (nsm2 >= 1)
		    		} // if (k > kold)
		    		//
		    		// update v(8) and set w(*)
		    		//
		    		int limit1 = kp1 - ns;
		    		temp5 = alpha[ns - 1];
		    		for (iq = 1; iq <= limit1; iq++) {
		    			v[iq - 1] = v[iq - 1] - temp5 *v[iq];
		    			w[iq - 1] = v[iq - 1];
		    		} // for (iq = 1; iq <= limit1; iq++)
		    		g[nsp1 - 1] = w[0];
	    		} // else
	    		//
	    		// compute the g(*) in the work vector w(*)
	    		//
	    		int nsp2 = ns + 2;
	    		if (kp1 >= nsp2) {
	    		    for (i = nsp2; i <= kp1; i++) {
	    		    	limit2 = kp2 - i;
	    		    	temp6 = alpha[i - 2];
	    		    	for (iq = 1; iq <= limit2; iq++) {
	    		    	    w[iq - 1] = w[iq - 1] - temp6 * w[iq];	
	    		    	} // for (iq = 1; iq <= limit2; iq++)
	    		    	g[i - 1] = w[0];
	    		    } // for (i = nsp2; i <= kp1; i++)
	    		} // if (kp1 >= nsp2)
	    	} // if (k >= ns)
	    	//       ***     end block 1     ***
	    	//
	    	//       ***     begin block 2     ***
	    	//   predict a solution p(*), evaluate derivatives using predicted
	    	//   solution, estimate local error at order k and errors at orders k,
	    	//   k-1, k-2 as if constant step size were used.
	    	//                   ***
	    	//
	    	//   change phi to phi star
	    	//
	    	if (k >= nsp1) {
	    		for (i = nsp1; i <= k; i++) {
	    		    temp1 = beta[i - 1];
	    		    for (l = 1; l <= neqn; l++) {
	    		        phi[l-1][i-1] = temp1*phi[l-1][i-1];	
	    		    } // for (l = 1; l <= neqn; l++)
	    		} // for (i = nsp1; i <= k; i++)
	    	} // if (k >= nsp1)
	    	//
	    	// predict solution and differences
	    	//
	    	for (l = 1; l <= neqn; l++) {
	    	    phi[l-1][kp2-1]	 = phi[l-1][kp1-1];
	    	    phi[l-1][kp1-1] = 0.0;
	    	    p[l - 1] = 0.0;
	    	} // for (l = 1; l <= neqn; l++)
	    	for (j = 1; j <= k; j++) {
	    	    i = kp1 - j;
	    	    ip1 = i+1;
	    	    temp2 = g[i - 1];
	    	    for (l = 1; l <= neqn; l++) {
	    	        p[l - 1] = p[l - 1] + temp2*phi[l-1][i-1];
	    	        phi[l-1][i-1] = phi[l-1][i-1] + phi[l-1][ip1-1];
	    	    } // for (l = 1; l <= neqn; l++)
	    	} // for (j = 1; j <= k; j++)
	    	if (!nornd) {
	    		for (l = 1; l <= neqn; l++) {
	    		    tau = h*p[l - 1] - phi[l-1][14];
	    		    p[l - 1] = yy[l - 1] + tau;
	    		    phi[l-1][15] = (p[l - 1] - yy[l - 1]) - tau;
	    		} // for (l = 1; l <= neqn; l++)
	    	} // if (!nornd)
	    	else {
	    		for (l = 1; l <= neqn; l++) {
	    			p[l - 1] = yy[l - 1] + h*p[l - 1];
	    		} // for (l = 1; l <= neqn; l++)
	    	} // else
	    	xold = x;
	    	x = x + h;
	    	absh = Math.abs(h);
		    if (testMode) {
		    	fTestMode(x, p, yp);
		    }
		    else {
		        f(x, p, yp);
		    }
		    //
		    // estimate errors at k,k-1,k-2
		    //
		    erkm2 = 0.0;
		    erkm1 = 0.0;
		    erk = 0.0;
		    for (l = 1; l <= neqn; l++) {
		    	temp3 = 1.0/wt[l - 1];
		    	temp4 = yp[l - 1] - phi[l-1][0];
		    	if (km2 > 0) {
		    		erkm2 = erkm2 + Math.pow(((phi[l-1][km1-1]+temp4)*temp3),2);
		    	}
		    	if (km2 >= 0) {
		    		erkm1 = erkm1 + Math.pow(((phi[l-1][k-1]+temp4)*temp3), 2);
		    	}
		        erk = erk + Math.pow((temp4*temp3), 2);
		    } // for (l = 1; l <= neqn; l++)
		    if (km2 > 0) {
		    	erkm2 = absh * sig[km1 -1]*gstr[km2-1]*Math.sqrt(erkm2);
		    }
		    if (km2 >= 0) {
		    	erkm1 = absh * sig[k - 1]*gstr[km1-1]*Math.sqrt(erkm1);
		    }
		    temp5 = absh*Math.sqrt(erk);
		    err = temp5*(g[k - 1] - g[kp1 - 1]);
		    erk = temp5 * sig[kp1 -1]*gstr[k - 1];
		    knew = k;
		    //
		    // test if order should be lowered
		    //
		    if (km2 > 0) {
		    	if (Math.max(erkm1, erkm2) <= erk) {
		    		knew = km1;
		    	}
		    } // if (km2 > 0)
		    else if (km2 == 0) {
		    	if (erkm1 <= 0.5*erk) {
		    		knew = km1;
		    	}
		    } // else if (km2 == 0)
		    //
		    // test if successful
		    //
		    if (err <= eps[0]) {
		    	break;
		    }
	    	//       ***     end block 2     ***
	    	//
	    	//       ***     begin block 3     ***
	    	//   the step is unsuccessful.  restore  x, phi(*,*), psi(*) .
	    	//   if third consecutive failure, set order to one.  if step fails more
	    	//   than three times, consider an optimal step size.  double error
	    	//   tolerance and return if estimated step size is too small for machine
	    	//   precision.
	    	//                   ***
	    	//
	    	//   restore x, phi(*,*) and psi(*)
	    	//
	    	phase1 = false;
	    	x = xold;
	    	for (i = 1; i <= k; i++) {
	    	    temp1 = 1.0/beta[i - 1];
	    	    ip1 = i+1;
	    	    for (l = 1; l <= neqn; l++) {
	    	        phi[l-1][i-1] = temp1*(phi[l-1][i-1] - phi[l-1][ip1-1]);	
	    	    } // for (l = 1; l <= neqn; l++) 
	    	} // for (i = 1; i <= k; i++)
	    	if (k >= 2) {
	    		for (i = 2; i <= k; i++) {
	    		    psi[i - 2] = psi[i - 1] - h;	
	    		} // for (i = 2; i <= k; i++)
	    	} // if (k >= 2)
	    	//
	    	// on third failure, set order to one.  thereafter, use optimal step size
	    	//
	    	ifail = ifail + 1;
	    	temp2 = 0.5;
	    	if ((ifail -3) > 0) {
	    		if (p5eps < 0.25*erk) {
	    			temp2 = Math.sqrt(p5eps/erk);
	    		}
	    	} // if ((ifail -3) > 0)
	    	if ((ifail - 3) >= 0) {
	    		knew = 1;
	    	}
	    	h = temp2*h;
	    	k = knew;
		    if (Math.abs(h) >= fouru*Math.abs(x)) {
		    	continue;
		    }
		    crash[0] = true;
		    if (h >= 0) {
		    	h = fouru*Math.abs(x);
		    }
		    else {
		    	h = -fouru*Math.abs(x);	
		    }
		    eps[0] = eps[0] + eps[0];
		    return;
    	} // while(true)
    	//       ***     end block 3     ***
    	//
    	//       ***     begin block 4     ***
    	//   the step is successful.  correct the predicted solution, evaluate
    	//   the derivatives using the corrected solution and update the
    	//   differences.  determine best order and step size for next step.
    	//                   ***
	    kold = k;
	    hold = h;
	    //
	    // correct and evaluate
	    //
	    temp1 = h*g[kp1 - 1];
	    if (!nornd) {
	    	for (l = 1; l <= neqn; l++) {
	    	    rho = temp1 * (yp[l-1] - phi[l-1][0]) - phi[l-1][15]; 
	    	    yy[l-1] = p[l-1] + rho;
	    	    phi[l-1][14] = (yy[l-1] - p[l-1]) - rho;
	    	} // for (l = 1; l <= neqn; l++)
	    } // if (!nornd)
	    else {
	    	for (l = 1; l <= neqn; l++) {
	    	    yy[l-1] = p[l-1] + temp1*(yp[l-1] - phi[l-1][0]);	
	    	} // for (l = 1; l <= neqn; l++)
	    } // else
	    if (testMode) {
	    	fTestMode(x, yy, yp);
	    }
	    else {
	        f(x, yy, yp);
	    }
	    // 
	    // update differences for next step
	    //
	    for (l = 1; l <= neqn; l++) {
	        phi[l-1][kp1-1] = yp[l-1] - phi[l-1][0];
	        phi[l-1][kp2-1] = phi[l-1][kp1-1] - phi[l-1][kp2-1];
	    } // for (l = 1; l <= neqn; l++)
	    for (i = 1; i <= k; i++) {
	        for (l = 1; l <= neqn; l++) { 
	        	phi[l-1][i-1] = phi[l-1][i-1] + phi[l-1][kp1-1];
	        } // for (l = 1; l <= neqn; l++) 
	    } // for (i = 1; i <= k; i++)
	    //
	    //   estimate error at order k+1 unless:
	    //     in first phase when always raise order,
	    //     already decided to lower order,
	    //     step size not constant so estimate unreliable
	    //
	    erkp1 = 0.0;
	    if ((knew == km1) || (k == 12)) {
	    	phase1 = false;
	    }
	    if (phase1) {
	        do450 = true;
	        do455 = true;
	    } // if (phase1)
	    else { // !phase1
	        if (knew == km1) {
	        	do450 = false;
	        	do455 = true;
	        } // if (knew == km1)
	        else { // knew != km1
	            if (kp1 > ns) {
	            	do450 = false;
	            	do455 = false;
	            } // if (kp1 > ns)
	            else { // kp1 <= ns
	            	for (l = 1; l <= neqn; l++) {
	            	    erkp1 = erkp1 + Math.pow((phi[l-1][kp2-1]/wt[l-1]), 2);	
	            	} // for (l = 1; l <= neqn; l++)
	            	erkp1 = absh*gstr[kp1-1]*Math.sqrt(erkp1);
	            	//
	            	// using estimated error at order k+1, determine approximate order
	            	// for next step
	            	//
	            	if (k <= 1) {
	            	    if (erkp1 >= 0.5*erk) {
	            	    	do450 = false;
	            	    	do455 = false;
	            	    }
	            	    else {
	            	    	do450 = true;
	            	    	do455 = true;
	            	    }
	            	} // if (k <= 1)
	            	else { // k > 1
	            	    if (erkm1 <= Math.min(erk, erkp1)) {
	            	    	do450 = false;
	            	    	do455 = true;
	            	    }
	            	    else {
	            	    	if ((erkp1 >= erk) || (k == 12)) {
	            	    		do450 = false;
	            	    		do455 = false;
	            	    	}
	            	    	else {
	            	    		do450 = true;
	            	    		do455 = true;
	            	    	}
	            	    }
	            	} // else k > 1
	            } // else kp1 <= ns
	        } // else knew != km1
	    } // else !phase1
	    //
	    // here erkp1 .lt. erk .lt. dmax1(erkm1,erkm2) else ordere would have
	    // been lowered in block 2.  thus order is to be raised
	    //
	    // raise order
	    // 
	    if (do450) {
	    	k = kp1;
	    	erk = erkp1;
	    	do455 = false;
	    } // if (do450)
	    //
	    // lower order
	    //
	    if (do455) {
	    	k = km1;
	    	erk = erkm1;
	    } // if (do455)
	    //
	    // with new order determine appropriate step size for next step
	    //
	    hnew = h + h;
	    if (phase1) {
	    	h = hnew;
	    	return;
	    }
	    if (p5eps >= erk*two[k]) {
	    	h = hnew;
	    	return;
	    }
	    hnew = h;
	    if (p5eps >= erk) {
	    	h = hnew;
	    	return;
	    }
	    temp2 = k + 1;
	    r = Math.pow((p5eps/erk), (1.0/temp2));
	    hnew = absh*Math.max(0.5, Math.min(0.9,r));
	    if (h >= 0) {
	    	hnew = Math.abs(Math.max(hnew, fouru*Math.abs(x)));
	    }
	    else {
	    	hnew = -Math.abs(Math.max(hnew, fouru*Math.abs(x)));	
	    }
	    h = hnew;
	    return;
	    // ***  end block 4 ***
    }
    
    public abstract void f(double t, double y[], double yp[]);
    
    private void intrp() {
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
    	
    	double hi = tout - x;
    	int ki = kold + 1;
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
    	    psijm1 = psi[jm1 - 1];
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
    	    ypout[l] = 0.0;
    		y[l] = 0.0;
    	} // for (l = 0; l < neqn; l++)
    	for (j = 1; j <= ki; j++) {
    	    i = kip1 - j;
    	    temp2 = g[i-1];
    	    temp3 = rho[i-1];
    	    for (l = 0; l < neqn; l++) {
    	        y[l] = y[l] + temp2*phi[l][i-1];
    	        ypout[l] = ypout[l] + temp3*phi[l][i-1];
    	    } // for (l = 0; l < neqn; l++)
    	} // for (j = 1; j <= ki; j++)
    	for (l = 0; l < neqn; l++) {
    	    y[l] = yy[l] + hi*y[l];	
    	} // for (l = 0; l < neqn; l++)
    	return;
    }
}